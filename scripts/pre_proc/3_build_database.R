# Purpose: Build the normalised extraction database from the cleaned analysis
# files. Run from the project root:
#
#   source("scripts/pre_proc/3_build_database.R")
#
# Reads   data/processed/for_analysis/*.csv    written by 2_clean_google_sheet.R
#         data/processed/paper_metadata.csv    written by 0_clean_metadata.R
# Writes  data/raw/bmp_meta.duckdb
#
# The cleaned files are the basis for everything downstream: practice strings
# and species names are already canonical there, each row carries exactly one
# of each, and each row already carries its species' analysis class. This
# script therefore does no cleaning of its own. It normalises the screening
# metadata into tables, classifies what each response variable measures, and
# records the values that fail a range check.
#
# The build is idempotent: the output is deleted and recreated from scratch on
# every run, so re-running after correcting a spreadsheet is safe.

# libraries ----------------------------------------------------------------

library(DBI)
library(tidyverse)

source("scripts/pre_proc/db_vocab.R")

# configuration ------------------------------------------------------------

config_build <-
  list(

    # The cleaned screening metadata, one row per study x practice x place.

    metadata = "data/processed/paper_metadata.csv",

    # The cleaned extraction tables.

    extraction_directory = "data/processed/for_analysis",

    # The database, the only output written into data/raw.

    database = "data/raw/bmp_meta.duckdb",

    # Table definitions and convenience views, executed in that order.

    schema = "scripts/pre_proc/schema.sql",

    views = "scripts/pre_proc/views.sql",

    # A 16 KiB block keeps the file small; the 256 KiB default pre-allocates
    # far more space than a database of this size needs.

    block_size = "16384",

    # The cleaned tables, in the order that fixes effect_id.

    sheets =
      tribble(
        ~ source_sheet, ~ extraction_type_code, ~ design,
        "mean_diff", "mean_difference", "categorical",
        "beta_categorical", "beta_categorical", "categorical",
        "beta_continuous", "beta_continuous", "continuous",
        "other_categorical", "other_categorical", "categorical",
        "other_continuous", "other_continuous", "continuous"
      ),

    # Per-arm descriptive statistics.

    group_summary_columns =
      c(
        "xbar_e",
        "n_e",
        "sd_e",
        "se_e",
        "lcl_e",
        "ucl_e",
        "xbar_c",
        "n_c",
        "sd_c",
        "se_c",
        "lcl_c",
        "ucl_c"
      ),

    # A model coefficient and its dispersion.

    coefficient_columns =
      c(
        "beta",
        "n",
        "sd",
        "se",
        "lcl",
        "ucl"
      ),

    # The test-statistic block. The global and per-arm sd and se columns are
    # empty in every table, so they are not carried.

    test_columns =
      c(
        "test_stat_value",
        "df",
        "global_n",
        "global_lcl",
        "global_ucl"
      ),

    test_arm_columns =
      c(
        "treatment_n",
        "treatment_df",
        "control_n",
        "control_df"
      )
  )

# cell helpers -------------------------------------------------------------

# Trim, collapse internal whitespace, and map the source null sentinels onto
# a real missing value.

squash <-
  function(.text) {
    trimmed <-
      .text %>%
      as.character() %>%
      str_squish()

    if_else(
      str_to_lower(trimmed) %in% db_vocab$null_tokens,
      NA_character_,
      trimmed
    )
  }

# Split a semicolon-delimited cell into cleaned atoms, dropping repeats that
# differ only in case and keeping the spelling seen first. Only the screening
# ledger and the error class still carry delimited cells; the practice and
# species columns were split upstream.

atoms <-
  function(.cell) {
    if (is.na(.cell)) {
      return(character())
    }

    parts <-
      .cell %>%
      str_split_1(";") %>%
      squash() %>%
      discard(
        \(.part) {
          is.na(.part)
        }
      )

    repeated <-
      parts %>%
      str_to_lower() %>%
      duplicated()

    parts[!repeated]
  }

# yes / no, with everything else (including a screened "unknown") missing.

tri_bool <-
  function(.text) {
    squash(.text) %>%
      str_to_lower() %>%
      case_match(
        "yes" ~ TRUE,
        "no" ~ FALSE,
        .default = NA
      )
  }

# A column as cleaned text, or all-missing where the table lacks it. The five
# tables differ in which statistic and review columns they carry.

text_column <-
  function(.data, .column) {
    if (!.column %in% names(.data)) {
      return(rep(NA_character_, nrow(.data)))
    }

    squash(.data[[.column]])
  }

numeric_column <-
  function(.data, .column) {
    if (!.column %in% names(.data)) {
      return(rep(NA_real_, nrow(.data)))
    }

    as.numeric(.data[[.column]])
  }

logical_column <-
  function(.data, .column) {
    if (!.column %in% names(.data)) {
      return(rep(NA, nrow(.data)))
    }

    as.logical(.data[[.column]])
  }

# flag formatting ----------------------------------------------------------

# A double rendered losslessly and stably: the fewest digits that round-trip,
# always a decimal point, and an exponent only outside 1e-4 .. 1e16. This is
# what a data-quality flag quotes the offending value with, so the flag text
# is reproducible rather than dependent on options(digits).

format_one_number <-
  function(.value) {
    if (is.na(.value)) {
      return(NA_character_)
    }

    if (.value == 0) {
      return(
        if_else(
          sign(1 / .value) < 0,
          "-0.0",
          "0.0"
        )
      )
    }

    digits <-
      detect(
        1:17,
        \(.digits) {
          rounded <-
            formatC(
              .value,
              format = "e",
              digits = .digits - 1
            )
          as.numeric(rounded) == .value
        }
      )

    scientific <-
      formatC(
        .value,
        format = "e",
        digits = digits - 1
      )

    exponent <-
      scientific %>%
      str_extract("[-+][0-9]+$") %>%
      as.integer()

    if (exponent < -4 || exponent >= 16) {
      return(scientific)
    }

    formatC(
      .value,
      format = "f",
      digits = max(digits - 1 - exponent, 1)
    )
  }

format_number <-
  function(.value) {
    .value %>%
      map_chr(
        \(.one) {
          format_one_number(.one)
        }
      )
  }

# Four significant digits, for a survival probability in flag text.

format_signif_4 <-
  function(.value) {
    formatC(
      .value,
      format = "g",
      digits = 4
    )
  }

# A label quoted for inclusion in flag text: single quotes, switching to
# double where the label carries an apostrophe.

quote_label <-
  function(.text) {
    has_apostrophe <- str_detect(.text, "'")
    has_quotation <- str_detect(.text, '"')

    escaped <-
      str_replace_all(
        .text,
        "'",
        "\\\\'"
      )

    single_quoted <-
      str_c(
        "'",
        escaped,
        "'"
      )

    double_quoted <-
      str_c(
        '"',
        .text,
        '"'
      )

    if_else(
      has_apostrophe & !has_quotation,
      double_quoted,
      single_quoted
    )
  }

# registries ---------------------------------------------------------------

# A lookup table of the distinct values of `.values`, each given a surrogate
# key: 1 for the first value seen, 2 for the next, and so on. The id is always
# the first column and the value the second, which is what registry_id() reads.

registry <-
  function(
    .values,
    .code,
    .id) {
    codes <-
      .values %>%
      discard(
        \(.value) {
          is.na(.value)
        }
      ) %>%
      unique()

    tibble(
      !!.id := seq_along(codes),
      !!.code := codes
    )
  }

registry_id <-
  function(.registry, .values) {
    .registry[[1]][match(.values, .registry[[2]])]
  }

# canonicalisers -----------------------------------------------------------

# The screening ledger is cleaned by nothing, so it still spells two
# practices the way the extraction used to. Anything that is neither
# canonical nor renamed is vocabulary drift and stops the build rather than
# entering the database unnoticed.

canonical_bmp <-
  function(.recorded) {
    renames <- db_vocab$bmp_renames

    code <-
      coalesce(
        renames$bmp_code[match(.recorded, renames$recorded_code)],
        .recorded
      )

    unknown <-
      code %>%
      discard(
        \(.code) {
          is.na(.code)
        }
      ) %>%
      setdiff(db_vocab$bmp_canonical$bmp_code)

    if (!is_empty(unknown)) {
      cli::cli_abort(
        c(
          "Practice strings that are not in the vocabulary.",
          i = "Add to db_vocab$bmp_canonical: {.val {unknown}}"
        )
      )
    }

    code
  }

species_group <-
  function(.species_name) {
    members <-
      db_vocab$species_groups %>%
      enframe(
        name = "group_code",
        value = "species_name"
      ) %>%
      unnest(species_name)

    members$group_code[match(.species_name, members$species_name)] %>%
      replace_na("species")
  }

canonical_error_class <-
  function(.raw) {
    code <-
      db_vocab$error_class_aliases[str_to_lower(.raw)] %>%
      unname()

    if (any(is.na(code))) {
      cli::cli_abort(
        c(
          "Unmapped error class.",
          i = "Add to db_vocab$error_class_aliases: {.val {unique(.raw)}}"
        )
      )
    }

    code
  }

# What each (response variable, extracted class) pair actually measures.
# Rules are tried in vocabulary order within a class and the first match wins;
# an override beats every rule. A pair matching neither keeps a missing scale
# and is left on the 'other' scale.

classify_response_variable <-
  function(.pairs) {
    rules <-
      db_vocab$response_rules %>%
      list_rbind(names_to = "response_class")

    overrides <-
      db_vocab$response_variable_overrides %>%
      rename_with(
        \(.name) {
          str_c("override_", .name)
        },
        c(response_scale, direction, analysis_class)
      )

    by_rule <-
      .pairs %>%
      left_join(
        rules,
        by = join_by(response_class),
        relationship = "many-to-many"
      ) %>%
      filter(
        str_detect(match_key, pattern)
      ) %>%
      slice_head(
        n = 1,
        by = c(response_variable_id, response_class_id)
      ) %>%
      select(
        response_variable_id,
        response_class_id,
        response_scale,
        direction,
        analysis_class
      )

    # Every override names a scale, so a non-missing override scale is a
    # sufficient test for "this pair was overridden" -- which matters because
    # one override deliberately sets analysis_class to missing.

    .pairs %>%
      left_join(
        by_rule,
        by = join_by(response_variable_id, response_class_id)
      ) %>%
      left_join(
        overrides,
        by = join_by(response_class, match_key == response_variable)
      ) %>%
      mutate(
        override_applied = !is.na(override_response_scale),
        response_scale =
          if_else(
            override_applied,
            override_response_scale,
            response_scale
          ),
        direction =
          if_else(
            override_applied,
            override_direction,
            direction
          ),
        analysis_class =
          if_else(
            override_applied,
            override_analysis_class,
            analysis_class
          ),
        response_scale = replace_na(response_scale, "other"),
        direction = replace_na(direction, "success")
      ) %>%
      select(
        !starts_with("override_")
      )
  }

# sql ----------------------------------------------------------------------

# duckdb prepares one statement at a time, so a .sql file has to be split
# before it can be executed. Line comments are stripped first, because they
# contain both apostrophes and semicolons; the split then relies on there
# being no escaped quotes in either file, which there are not.

sql_statements <-
  function(.path) {
    .path %>%
      read_lines() %>%
      str_replace("(^(?:[^']*'[^']*')*[^']*?)--.*$", "\\1") %>%
      str_flatten(collapse = "\n") %>%
      str_split_1(";(?=(?:[^']*'[^']*')*[^']*$)") %>%
      str_trim() %>%
      discard(
        \(.statement) {
          .statement == ""
        }
      )
  }

# screening metadata -------------------------------------------------------

paper_metadata <-
  config_build$metadata %>%
  read_csv(
    col_types =
      cols(.default = col_character())
  ) %>%
  arrange(key) %>%
  mutate(
    across(
      everything(),
      \(.column) {
        squash(.column)
      }
    )
  )

# One row per bibliographic item. The metadata carries one row per place, so
# the study-level facts are taken from the first row of each key.

study_from_metadata <-
  paper_metadata %>%
  summarise(
    .by = key,
    paper = first(paper),
    title = first(title),
    article_type_code = first(article_type),
    reviewed =
      reviewed %>%
      first() %>%
      tri_bool()
  )

# cleaned extraction -------------------------------------------------------

extraction <-
  config_build$sheets$source_sheet %>%
  set_names() %>%
  map(
    \(.sheet) {
      config_build$extraction_directory %>%
        fs::path(
          .sheet,
          ext = "csv"
        ) %>%
        read_csv(
          show_col_types = FALSE,
          guess_max = Inf
        )
    }
  )

extraction_type <-
  config_build$sheets %>%
  mutate(extraction_type_id = row_number())

effect_size_source <-
  config_build$sheets %>%
  pmap(
    \(source_sheet, extraction_type_code, design) {
      sheet_data <- extraction[[source_sheet]]

      statistics <-
        c(
          config_build$group_summary_columns,
          config_build$coefficient_columns,
          config_build$test_columns,
          config_build$test_arm_columns
        ) %>%
        set_names() %>%
        map(
          \(.column) {
            numeric_column(sheet_data, .column)
          }
        ) %>%
        as_tibble()

      tibble(
        study_key = text_column(sheet_data, "key"),
        paper = text_column(sheet_data, "paper"),
        extraction_type_code = extraction_type_code,
        bmp_code = text_column(sheet_data, "bmp"),
        species_name = text_column(sheet_data, "species"),
        analysis_class = text_column(sheet_data, "analysis_class"),
        species_include = logical_column(sheet_data, "species_include"),
        response_class_code = text_column(sheet_data, "response_class"),
        response_variable_label = text_column(sheet_data, "response_var"),
        notes = text_column(sheet_data, "notes"),
        source_sheet = source_sheet,
        source_row = seq_len(nrow(sheet_data)),
        error_atoms =
          sheet_data %>%
          text_column("error_class") %>%
          map(
            \(.cell) {
              atoms(.cell)
            }
          ),
        treatment_description = text_column(sheet_data, "treatment"),
        control_description = text_column(sheet_data, "control"),
        predictor_variable = text_column(sheet_data, "predictor_var"),
        model_type_label = text_column(sheet_data, "model"),
        test_statistic_code = text_column(sheet_data, "test_statistic"),
        has_test_block = "test_stat_value" %in% names(sheet_data),
        effect_sign = numeric_column(sheet_data, "sign"),
        contrast_flag = text_column(sheet_data, "treatment_control_flag"),
        response_flag = numeric_column(sheet_data, "response_flag"),
        response_direction = numeric_column(sheet_data, "response_dir")
      ) %>%
        bind_cols(statistics)
    }
  ) %>%
  list_rbind() %>%
  mutate(
    effect_id = row_number(),
    extraction_type_id =
      extraction_type$extraction_type_id[
        match(extraction_type_code, extraction_type$extraction_type_code)
      ],
    .before = 1
  )

# practices ----------------------------------------------------------------

bmp_table <-
  db_vocab$bmp_canonical %>%
  mutate(bmp_id = row_number()) %>%
  left_join(
    db_vocab$bmp_analysis_groups,
    by = join_by(bmp_code)
  ) %>%
  mutate(
    analysis_bmp_code = coalesce(analysis_bmp_code, bmp_code),
    analysis_bmp_label = coalesce(analysis_bmp_label, bmp_label)
  ) %>%
  select(
    bmp_id,
    bmp_code,
    bmp_label,
    is_practice,
    analysis_bmp_code,
    analysis_bmp_label
  )

# species ------------------------------------------------------------------

# The analysis class travels with the species on every extracted row, so it
# is a species-level fact. A species carrying two of them would mean the
# classification frame changed part-way through a build.

species_table <-
  effect_size_source %>%
  distinct(
    species_name,
    analysis_class,
    species_include
  ) %>%
  mutate(
    species_id = row_number(),
    species_group = species_group(species_name)
  ) %>%
  select(
    species_id,
    species_name,
    species_group,
    analysis_class,
    include_in_analysis = species_include
  )

if (n_distinct(species_table$species_name) != nrow(species_table)) {
  cli::cli_abort(
    "A species carries more than one analysis class in the cleaned files."
  )
}

# studies ------------------------------------------------------------------

# A study present in the extraction but missing from the screening ledger is
# inserted from the extraction, and flagged.

extra_studies <-
  effect_size_source %>%
  filter(
    !is.na(study_key),
    !study_key %in% study_from_metadata$key
  ) %>%
  slice_head(
    n = 1,
    by = study_key
  ) %>%
  arrange(study_key) %>%
  select(
    study_key,
    paper
  )

lookup_article_type <-
  registry(
    study_from_metadata$article_type_code,
    "article_type_code",
    "article_type_id"
  )

study <-
  bind_rows(
    study_from_metadata %>%
      mutate(
        study_key = key,
        article_type_id =
          registry_id(lookup_article_type, article_type_code),
        in_citation_index = TRUE
      ),
    extra_studies %>%
      mutate(in_citation_index = FALSE)
  ) %>%
  select(
    study_key,
    paper,
    title,
    article_type_id,
    reviewed,
    in_citation_index
  )

# Place names and their types are settled by 0_clean_metadata.R, which types
# them from ISO 3166. A place that reference data could not name keeps the
# spelling the sheet used and carries no type.

lookup_region <-
  paper_metadata$geography %>%
  registry("region_code", "region_id") %>%
  left_join(
    paper_metadata %>%
      distinct(
        geography,
        geography_type
      ),
    by = join_by(region_code == geography)
  ) %>%
  mutate(
    region_label =
      region_code %>%
      str_replace_all("_", " ") %>%
      str_to_title(),
    region_type = geography_type
  ) %>%
  select(
    region_id,
    region_code,
    region_label,
    region_type
  )

study_region <-
  paper_metadata %>%
  distinct(
    study_key = key,
    geography
  ) %>%
  filter(
    !is.na(geography)
  ) %>%
  mutate(
    region_id = registry_id(lookup_region, geography)
  ) %>%
  select(
    study_key,
    region_id
  )

# screening decisions ------------------------------------------------------

# One row per study x practice. The ledger repeats a pairing where a study
# was screened more than once; the first row is authoritative.

study_bmp_source <-
  paper_metadata %>%
  mutate(
    bmp_code = canonical_bmp(bmp),
    bmp_id = registry_id(bmp_table, bmp_code)
  ) %>%
  distinct(
    key,
    bmp_id,
    .keep_all = TRUE
  )

lookup_availability <-
  registry(
    str_to_lower(study_bmp_source$effect_size),
    "availability_code",
    "availability_id"
  )

lookup_problem <-
  registry(
    str_to_lower(study_bmp_source$problem),
    "problem_code",
    "problem_id"
  )

study_bmp <-
  study_bmp_source %>%
  mutate(
    study_key = key,
    availability_id =
      registry_id(
        lookup_availability,
        str_to_lower(effect_size)
      ),
    is_useful = tri_bool(useful),
    problem_id =
      registry_id(
        lookup_problem,
        str_to_lower(problem)
      ),
    notes = additional_notes
  ) %>%
  select(
    study_key,
    bmp_id,
    availability_id,
    is_useful,
    problem_id,
    notes
  )

study_bmp_response_source <-
  study_bmp_source %>%
  mutate(
    response_atoms =
      response %>%
      map(
        \(.cell) {
          atoms(.cell) %>%
            str_to_lower()
        }
      )
  ) %>%
  select(
    study_key = key,
    bmp_id,
    response_atoms
  ) %>%
  unnest(response_atoms)

lookup_response_type <-
  registry(
    study_bmp_response_source$response_atoms,
    "response_type_code",
    "response_type_id"
  )

study_bmp_response <-
  study_bmp_response_source %>%
  mutate(
    response_type_id = registry_id(lookup_response_type, response_atoms)
  ) %>%
  select(
    study_key,
    bmp_id,
    response_type_id
  )

# `multiple_bmps` is redundant with the long structure, so it is verified
# rather than stored. Where the two disagree the long-format `bmp` value is
# taken as authoritative, and the row is flagged.

bmp_not_declared <-
  paper_metadata %>%
  distinct(
    key,
    bmp,
    multiple_bmps
  ) %>%
  mutate(
    declared =
      multiple_bmps %>%
      map(
        \(.cell) {
          atoms(.cell)
        }
      ) %>%
      map(
        \(.atoms) {
          if (is_empty(.atoms)) {
            return(.atoms)
          }
          .atoms %>%
            canonical_bmp() %>%
            unique() %>%
            str_sort(locale = "C")
        }
      ),
    actual = canonical_bmp(bmp)
  ) %>%
  filter(
    lengths(declared) > 0,
    !map2_lgl(
      actual,
      declared,
      \(.actual, .declared) {
        .actual %in% .declared
      }
    )
  ) %>%
  mutate(
    declared_codes =
      declared %>%
      map_chr(
        \(.codes) {
          str_flatten_comma(.codes)
        }
      )
  )

# effect-size subtypes -----------------------------------------------------

effect_error_source <-
  effect_size_source %>%
  select(
    effect_id,
    error_atoms
  ) %>%
  unnest(error_atoms) %>%
  mutate(
    error_class_code = canonical_error_class(error_atoms)
  )

lookup_error_class <-
  registry(
    effect_error_source$error_class_code,
    "error_class_code",
    "error_class_id"
  )

effect_error_class <-
  effect_error_source %>%
  mutate(
    error_class_id = registry_id(lookup_error_class, error_class_code)
  ) %>%
  select(
    effect_id,
    error_class_id
  )

effect_contrast <-
  effect_size_source %>%
  filter(
    !is.na(treatment_description) | !is.na(control_description)
  ) %>%
  select(
    effect_id,
    treatment_description,
    control_description
  )

effect_predictor <-
  effect_size_source %>%
  filter(
    !is.na(predictor_variable)
  ) %>%
  select(
    effect_id,
    predictor_variable
  )

# The hand review recorded on the three categorical tables: which direction a
# higher value points, whether the contrast tests the practice at all, and
# whether the response still needs a second look.

effect_review <-
  effect_size_source %>%
  filter(
    !is.na(effect_sign) |
      !is.na(contrast_flag) |
      !is.na(response_flag) |
      !is.na(response_direction)
  ) %>%
  select(
    effect_id,
    effect_sign,
    contrast_flag,
    response_flag,
    response_direction
  )

effect_group_summary <-
  effect_size_source %>%
  filter(
    if_any(
      all_of(config_build$group_summary_columns),
      \(.value) {
        !is.na(.value)
      }
    )
  ) %>%
  select(
    effect_id,
    all_of(config_build$group_summary_columns)
  )

effect_coefficient <-
  effect_size_source %>%
  filter(
    if_any(
      all_of(config_build$coefficient_columns),
      \(.value) {
        !is.na(.value)
      }
    )
  ) %>%
  select(
    effect_id,
    all_of(config_build$coefficient_columns)
  )

effect_test_source <-
  effect_size_source %>%
  filter(
    has_test_block,
    !is.na(model_type_label) |
      !is.na(test_statistic_code) |
      if_any(
        all_of(config_build$test_columns),
        \(.value) {
          !is.na(.value)
        }
      )
  )

lookup_model_type <-
  registry(
    effect_test_source$model_type_label,
    "model_type_label",
    "model_type_id"
  )

lookup_test_statistic <-
  registry(
    str_to_lower(effect_test_source$test_statistic_code),
    "test_statistic_code",
    "test_statistic_id"
  )

effect_test <-
  effect_test_source %>%
  mutate(
    model_type_id = registry_id(lookup_model_type, model_type_label),
    test_statistic_id =
      registry_id(
        lookup_test_statistic,
        str_to_lower(test_statistic_code)
      )
  ) %>%
  select(
    effect_id,
    model_type_id,
    test_statistic_id,
    all_of(config_build$test_columns)
  )

effect_test_arm <-
  effect_test_source %>%
  select(
    effect_id,
    all_of(config_build$test_arm_columns)
  ) %>%
  pivot_longer(
    !effect_id,
    names_to = c("arm", ".value"),
    names_pattern = "(treatment|control)_(n|df)"
  ) %>%
  filter(
    !is.na(n) | !is.na(df)
  )

# effect sizes -------------------------------------------------------------

lookup_response_class <-
  registry(
    str_to_lower(effect_size_source$response_class_code),
    "response_class_code",
    "response_class_id"
  )

lookup_response_variable <-
  registry(
    effect_size_source$response_variable_label,
    "response_variable",
    "response_variable_id"
  )

effect_size <-
  effect_size_source %>%
  mutate(
    bmp_id = registry_id(bmp_table, canonical_bmp(bmp_code)),
    species_id = registry_id(species_table, species_name),
    response_class_id =
      registry_id(
        lookup_response_class,
        str_to_lower(response_class_code)
      ),
    response_variable_id =
      registry_id(lookup_response_variable, response_variable_label)
  ) %>%
  select(
    effect_id,
    study_key,
    extraction_type_id,
    bmp_id,
    species_id,
    response_class_id,
    response_variable_id,
    notes,
    source_sheet,
    source_row
  )

# Classification is keyed on the (variable, extracted class) pair actually
# observed, so a label used under two classes gets two classifications.

response_variable_class <-
  effect_size %>%
  filter(
    !is.na(response_variable_id),
    !is.na(response_class_id)
  ) %>%
  distinct(
    response_variable_id,
    response_class_id
  ) %>%
  arrange(response_variable_id, response_class_id) %>%
  mutate(
    response_variable =
      lookup_response_variable$response_variable[response_variable_id],
    response_class =
      lookup_response_class$response_class_code[response_class_id],
    match_key =
      response_variable %>%
      str_to_lower() %>%
      str_trim()
  ) %>%
  classify_response_variable() %>%
  left_join(
    db_vocab$response_review_notes %>%
      rename(match_key = response_variable),
    by = join_by(match_key)
  ) %>%
  mutate(
    analysis_response_class = analysis_class,
    is_poolable = !is.na(analysis_class)
  )

# data quality -------------------------------------------------------------

# Every range check an effect size is put through. The row order here is the
# order the checks run in, and issue_id follows it, so a rebuilt database
# numbers its flags the same way. A source row naming two practices is two
# effect sizes now, so a bad value there is flagged once per practice.

effect_checks <-
  bind_rows(
    tribble(
      ~ label, ~ lower, ~ upper, ~ point,
      "treatment arm", "lcl_e", "ucl_e", "xbar_e",
      "control arm", "lcl_c", "ucl_c", "xbar_c"
    ),
    tibble(column = config_build$group_summary_columns),
    tribble(
      ~ label, ~ lower, ~ upper, ~ point,
      "coefficient", "lcl", "ucl", "beta"
    ),
    tibble(column = config_build$coefficient_columns),
    tribble(
      ~ label, ~ lower, ~ upper, ~ point, ~ test_only,
      "global", "global_lcl", "global_ucl", NA_character_, TRUE
    )
  ) %>%
  mutate(
    check_order = row_number(),
    test_only = replace_na(test_only, FALSE)
  )

interval_flags <-
  effect_checks %>%
  filter(
    !is.na(label)
  ) %>%
  select(
    check_order,
    label,
    lower,
    upper,
    point,
    test_only
  ) %>%
  pmap(
    \(check_order, label, lower, upper, point, test_only) {
      effect_size_source %>%
        filter(
          !test_only | has_test_block
        ) %>%
        mutate(
          check_order = check_order,
          check_label = label,
          bound_lower = .data[[lower]],
          bound_upper = .data[[upper]],
          estimate =
            if (is.na(point)) {
              NA_real_
            } else {
              .data[[point]]
            }
        ) %>%
        filter(
          !is.na(bound_lower),
          !is.na(bound_upper)
        ) %>%
        mutate(
          issue_code =
            case_when(
              bound_lower > bound_upper ~ "interval_bounds_transposed",
              !is.na(estimate) &
                (estimate < bound_lower | estimate > bound_upper) ~
                "estimate_outside_interval"
            )
        ) %>%
        filter(
          !is.na(issue_code)
        ) %>%
        mutate(
          transposed_detail =
            glue::glue(
              "{check_label}: lcl {format_number(bound_lower)} > ",
              "ucl {format_number(bound_upper)} ",
              "(sheet {source_sheet}, row {source_row})"
            ),
          outside_detail =
            glue::glue(
              "{check_label}: {format_number(estimate)} not within ",
              "[{format_number(bound_lower)}, ",
              "{format_number(bound_upper)}] ",
              "(sheet {source_sheet}, row {source_row})"
            ),
          detail =
            if_else(
              issue_code == "interval_bounds_transposed",
              as.character(transposed_detail),
              as.character(outside_detail)
            )
        ) %>%
        select(
          effect_id,
          study_key,
          check_order,
          issue_code,
          detail
        )
    }
  ) %>%
  list_rbind()

positive_flags <-
  effect_checks %>%
  filter(
    !is.na(column)
  ) %>%
  select(
    check_order,
    column
  ) %>%
  pmap(
    \(check_order, column) {
      effect_size_source %>%
        mutate(
          check_order = check_order,
          check_column = column,
          check_value = .data[[column]]
        ) %>%
        filter(
          !is.na(check_value)
        ) %>%
        mutate(
          issue_code =
            case_when(
              str_starts(check_column, "n") & check_value <= 0 ~
                "non_positive_sample_size",
              str_starts(check_column, "s[de]") & check_value < 0 ~
                "negative_dispersion"
            )
        ) %>%
        filter(
          !is.na(issue_code)
        ) %>%
        mutate(
          detail =
            glue::glue(
              "{check_column} = {format_number(check_value)} ",
              "(sheet {source_sheet}, row {source_row})"
            ) %>%
            as.character()
        ) %>%
        select(
          effect_id,
          study_key,
          check_order,
          issue_code,
          detail
        )
    }
  ) %>%
  list_rbind()

range_flags <-
  bind_rows(interval_flags, positive_flags) %>%
  arrange(effect_id, check_order) %>%
  select(
    effect_id,
    study_key,
    issue_code,
    detail
  )

structural_flags <-
  bind_rows(
    extra_studies %>%
      mutate(
        issue_code = "study_not_in_citation_index",
        detail =
          str_c(
            "study key appears in the extraction but not in the ",
            "screening metadata"
          )
      ) %>%
      select(
        study_key,
        issue_code,
        detail
      ),
    bmp_not_declared %>%
      mutate(
        study_key = key,
        issue_code = "bmp_not_in_multiple_bmps",
        detail =
          glue::glue(
            "{actual} is not listed in multiple_bmps ({declared_codes})"
          ) %>%
          as.character()
      ) %>%
      select(
        study_key,
        issue_code,
        detail
      )
  ) %>%
  mutate(effect_id = NA_integer_) %>%
  select(
    effect_id,
    study_key,
    issue_code,
    detail
  )

# Rows extracted under nest_success whose response variable turns out not to
# measure nest survival are flagged rather than quietly reclassified.

classified_effects <-
  effect_size %>%
  filter(
    !is.na(response_variable_id),
    !is.na(response_class_id)
  ) %>%
  left_join(
    response_variable_class %>%
      select(
        response_variable_id,
        response_class_id,
        response_variable,
        response_class,
        response_scale,
        direction
      ),
    by = join_by(response_variable_id, response_class_id)
  ) %>%
  mutate(
    quoted = quote_label(response_variable)
  )

classification_flags <-
  classified_effects %>%
  mutate(
    issue_code =
      case_when(
        response_scale == "evenness" ~ "evenness_not_poolable",
        response_class != "nest_success" ~ NA_character_,
        response_scale %in%
          c(
            "density",
            "duration",
            "other"
          ) ~
          "misfiled_response_class",
        response_scale == "count" ~ "reclassified_as_productivity"
      )
  ) %>%
  filter(
    !is.na(issue_code)
  ) %>%
  mutate(
    evenness_detail =
      glue::glue(
        "{quoted} is an evenness ratio, constructed to be independent ",
        "of richness; excluded from the pool ",
        "(sheet {source_sheet}, row {source_row})"
      ),
    misfiled_detail =
      glue::glue(
        "extracted as nest_success but {quoted} is on a ",
        "{response_scale} scale ",
        "(sheet {source_sheet}, row {source_row})"
      ),
    productivity_detail =
      glue::glue(
        "extracted as nest_success but {quoted} is a count per nest; ",
        "modelled as productivity ",
        "(sheet {source_sheet}, row {source_row})"
      ),
    detail =
      case_match(
        issue_code,
        "evenness_not_poolable" ~ as.character(evenness_detail),
        "misfiled_response_class" ~ as.character(misfiled_detail),
        "reclassified_as_productivity" ~ as.character(productivity_detail)
      )
  ) %>%
  select(
    effect_id,
    study_key,
    issue_code,
    detail
  )

# Survival values that cannot be right, checked after unit and direction
# handling so that the conversion view never emits a spurious hazard ratio.

survival_flags <-
  classified_effects %>%
  filter(
    response_scale %in% c("daily_survival", "period_survival")
  ) %>%
  left_join(
    effect_group_summary %>%
      select(
        effect_id,
        xbar_e,
        xbar_c
      ),
    by = join_by(effect_id)
  ) %>%
  filter(
    !is.na(xbar_e),
    !is.na(xbar_c)
  ) %>%
  mutate(
    divisor =
      if_else(
        pmax(xbar_e, xbar_c) > 1,
        100,
        1
      ),
    treatment_survival =
      if_else(
        direction == "failure",
        1 - xbar_e / divisor,
        xbar_e / divisor
      ),
    control_survival =
      if_else(
        direction == "failure",
        1 - xbar_c / divisor,
        xbar_c / divisor
      ),
    lowest = pmin(treatment_survival, control_survival),
    highest = pmax(treatment_survival, control_survival),
    issue_code =
      case_when(
        lowest <= 0 | highest >= 1 ~ "implausible_survival_value",
        response_scale == "daily_survival" & lowest < 0.5 ~
          "implausible_daily_survival"
      )
  ) %>%
  filter(
    !is.na(issue_code)
  ) %>%
  mutate(
    outside_detail =
      glue::glue(
        "{quoted} gives survival ",
        "{format_signif_4(treatment_survival)} vs ",
        "{format_signif_4(control_survival)}, ",
        "outside (0, 1) (sheet {source_sheet}, row {source_row})"
      ),
    daily_detail =
      glue::glue(
        "{quoted} gives a daily survival rate of ",
        "{format_signif_4(lowest)}; at that rate almost no nest fledges ",
        "(sheet {source_sheet}, row {source_row})"
      ),
    detail =
      if_else(
        issue_code == "implausible_survival_value",
        as.character(outside_detail),
        as.character(daily_detail)
      )
  ) %>%
  select(
    effect_id,
    study_key,
    issue_code,
    detail
  )

data_quality_issue <-
  bind_rows(
    structural_flags,
    range_flags,
    classification_flags,
    survival_flags
  ) %>%
  mutate(issue_id = row_number()) %>%
  select(
    issue_id,
    effect_id,
    study_key,
    issue_code,
    detail
  )

# write --------------------------------------------------------------------

# Tables are loaded in dependency order, so every foreign key already has its
# parent row by the time it is inserted.

database_tables <-
  list(
    bmp = bmp_table,
    species = species_table,
    region = lookup_region,
    response_type = lookup_response_type,
    response_class = lookup_response_class,
    response_variable = lookup_response_variable,
    error_class = lookup_error_class,
    response_variable_class =
      response_variable_class %>%
      select(
        response_variable_id,
        response_class_id,
        response_scale,
        direction,
        analysis_response_class,
        is_poolable,
        review_note
      ),
    article_type = lookup_article_type,
    effect_size_availability = lookup_availability,
    screening_problem = lookup_problem,
    extraction_type =
      extraction_type %>%
      select(
        extraction_type_id,
        extraction_type_code,
        source_sheet,
        design
      ),
    model_type = lookup_model_type,
    test_statistic = lookup_test_statistic,
    study = study,
    study_region = study_region,
    study_bmp = study_bmp,
    study_bmp_response = study_bmp_response,
    effect_size = effect_size,
    effect_error_class = effect_error_class,
    effect_contrast = effect_contrast,
    effect_predictor = effect_predictor,
    effect_review = effect_review,
    effect_group_summary = effect_group_summary,
    effect_coefficient = effect_coefficient,
    effect_test = effect_test,
    effect_test_arm = effect_test_arm,
    data_quality_issue = data_quality_issue
  )

c(
  config_build$database,
  str_c(config_build$database, ".wal"),
  str_c(config_build$database, ".tmp")
) %>%
  keep(
    \(.path) {
      fs::file_exists(.path)
    }
  ) %>%
  fs::file_delete()

connection <-
  dbConnect(
    duckdb::duckdb(),
    dbdir = config_build$database,
    config =
      list(default_block_size = config_build$block_size)
  )

sql_statements(config_build$schema) %>%
  walk(
    \(.statement) {
      dbExecute(connection, .statement)
    }
  )

database_tables %>%
  iwalk(
    \(.rows, .table) {
      dbAppendTable(
        connection,
        .table,
        .rows
      )
    }
  )

sql_statements(config_build$views) %>%
  walk(
    \(.statement) {
      dbExecute(connection, .statement)
    }
  )

dbDisconnect(connection, shutdown = TRUE)

cli::cli_inform(
  "Wrote {config_build$database}: {nrow(effect_size)} effect sizes, \\
   {nrow(study)} studies, {nrow(data_quality_issue)} data-quality flags."
)
