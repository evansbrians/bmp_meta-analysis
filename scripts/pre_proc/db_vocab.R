# Purpose: The judgement calls the extraction database rests on, gathered in
# one place. Sourced by 3_build_database.R and by the gradient synthesis,
# so the database and the analysis share one vocabulary.

# The cleaning scripts canonicalise what they can: 2_clean_google_sheet.R the
# practice strings and species names, 0_clean_metadata.R the study locations,
# which it types from ISO 3166 rather than from a list kept here. What remains
# is what neither script decides: the practice labels and their analysis
# grouping, the spellings the screening ledger still uses, which species
# labels are not single species, and the response-variable classification.

library(tidyverse)

db_vocab <-
  list(

    # best management practices ----

    # The canonical practice list. `is_practice` is FALSE for the one code
    # that records the absence of a practice. Row order fixes bmp_id.

    bmp_canonical =
      tribble(
        ~ bmp_code, ~ bmp_label, ~ is_practice,
        "delay_hay", "Delay first hay cutting", TRUE,
        "edge_and_shrub_habitat", "Edge and shrub habitat", TRUE,
        "eliminate_pesticides", "Eliminate pesticide use", TRUE,
        "grazing_intensity", "Grazing intensity", TRUE,
        "install_nest_boxes", "Install nest boxes", TRUE,
        "keep_cats_indoors", "Keep cats indoors", TRUE,
        "manage_in_patches", "Manage fields in patches", TRUE,
        "mow_towards_refugia", "Mow towards refugia", TRUE,
        "plant_nwsg",
        "Plant native warm-season grasses and wildflowers", TRUE,
        "provide_overwintering_habitat", "Provide overwintering habitat", TRUE,
        "remove_non_native_shrubs", "Remove non-native shrubs", TRUE,
        "set_aside_adjacent_unmowed",
        "Set aside unmowed areas adjacent to mowed areas", TRUE,
        "stream_exclusion_and_buffers",
        "Stream exclusion and buffer plantings", TRUE,
        "rotational_grazing", "Rotational grazing", TRUE,
        "upgrade_to_darksky", "Upgrade to dark-sky lighting", TRUE,
        "no_bmp", "No best management practice", FALSE,
        "prescribed_fire", "Prescribed fire", TRUE
      ),

    # The cleaning step rewrites this one, so it is mapped back here.
    # Anything neither canonical nor renamed stops 3_build_database.R.

    bmp_renames =
      tribble(
        ~ recorded_code, ~ bmp_code,
        "reduce_grazing_intensity", "grazing_intensity"
      ),

    # No practice is grouped for the models at present. Add a row to pool
    # one practice into another; extraction stays canonical either way.

    bmp_analysis_groups =
      tibble(
        bmp_code = character(),
        analysis_bmp_code = character(),
        analysis_bmp_label = character()
      ),

    # species ----

    # Species labels that are not a single bird species. Everything else in
    # the extraction is taken to be a species. The names are the cleaned ones
    # 2_clean_google_sheet.R writes, so they join without further work.

    species_groups =
      list(
        artificial_nest =
          c(
            "artificial_nests",
            "artificial_nests_chestnut_sided_warbler",
            "artificial_nests_northern_bobwhite",
            "artificial_nests_ovenbird"
          ),
        non_bird = "carnivores",
        guild =
          c(
            "breeding_grassland_species",
            "breeding_shrub_scrub_species",
            "edge_species",
            "facultative_grassland_species",
            "farmland_bird_indicator_species",
            "farmland_specialists",
            "frugivores",
            "generalists",
            "granivores",
            "grassland_facultative_species",
            "grassland_obligates",
            "grassland_specialists",
            "grassland_species",
            "ground_nesters",
            "insectivores",
            "meadowlark_spp",
            "non_grassland_species",
            "non_insectivores",
            "obligate_grassland_species",
            "omnivores",
            "passerines",
            "resident_species",
            "residents",
            "shrub_species",
            "sparrows",
            "specialists",
            "tits",
            "waders",
            "wintering_shrub_scrub_species",
            "woodland_species"
          ),
        aggregate =
          c(
            "all_species",
            "bird_and_mammal_species",
            "breeding_species",
            "wintering_species"
          )
      ),

    # error classes ----

    # Atoms left after splitting the error_class cell on ';'.

    error_class_aliases =
      c(
        "se" = "standard_error",
        "standard error" = "standard_error",
        "standard_error" = "standard_error",
        "standard_deviation" = "standard_deviation",
        "confidence_intervals" = "confidence_intervals",
        "confident_intervals" = "confidence_intervals"
      ),

    # Sentinels that mean "missing" in the source workbooks. "unknown" is
    # deliberately absent: in the screening columns it is a real category (the
    # reviewer looked and could not tell) and is preserved as such.

    null_tokens =
      c(
        "",
        "-",
        "na",
        "n/a",
        "none",
        "nan"
      ),

    # response classification ----

    # The `nest_success` response class as extracted pools five incompatible
    # measurement scales -- daily rates, period probabilities, counts per
    # nest, densities and durations -- and mixes success-direction with
    # failure-direction responses. Nothing downstream can pool them safely
    # until each response variable is told what it actually measures.

    # Classification is keyed on the PAIR (response variable, extracted
    # response class), not on the label alone, because two labels are used
    # under two different classes: "nest density (nests/ha)" is a perfectly
    # good abundance measure under `abundance` but a misfiling under
    # `nest_success`, and "abundance" appears under `species_richness`.

    # Within a class, rules are tried in order and the first match wins; the
    # class's final entry is a catch-all. `response_variable_overrides`, keyed
    # on (lower-cased label, class), beats every rule.

    #   response_scale     what the number is on
    #   direction          'success' = higher is better, 'failure' = worse
    #   analysis_class     the class to model under, or NA if not poolable

    # The permitted response scales are not repeated here: schema.sql already
    # constrains response_variable_class.response_scale to that set.

    # 'daily_survival' and 'period_survival' are made comparable by converting
    # both to a log hazard ratio; see v_nest_survival_effect in views.sql.

    response_rules =
      list(
        nest_success =
          tribble(
            ~ pattern, ~ response_scale, ~ direction, ~ analysis_class,

            # Daily scale, success direction.

            str_c(
              "^(dsr|daily survival rate|daily nest survival|",
              "mayfield daily nest survival rate)$"
            ),
            "daily_survival", "success", "nest_survival",

            # Daily scale, failure direction.

            "^daily (mortality rate|nest failure|nest predation( rate)?)$",
            "daily_survival", "failure", "nest_survival",

            "^daily percent predation rate",
            "daily_survival", "failure", "nest_survival",

            # Not nest survival at all: misfiled under this class.

            str_flatten(
              c(
                "per 100 ha",
                "nests?/ha",
                "nest density"
              ),
              collapse = "|"
            ),
            "density", "success", NA_character_,

            "\\(days\\)$",
            "duration", "success", NA_character_,

            # Period scale, failure direction.

            str_flatten(
              c(
                "predat", "depredat", "parasit", "abandonment",
                "brood reduction", "partial_brood_loss", "chicks killed",
                "nest failure"
              ),
              collapse = "|"
            ),
            "period_survival", "failure", "nest_survival",

            # Rates that merely happen to mention fledging.

            str_flatten(
              c(
                "fledg\\w*\\s*(success|rate)",
                "fledging_rate",
                "fledge_rate"
              ),
              collapse = "|"
            ),
            "period_survival", "success", "nest_survival",

            # Productivity: counts per nest, per female, per pair.

            str_flatten(
              c(
                "per (successful )?nest", "per female", "per year",
                "per breeding pair",
                "number of (fledglings|young|nestlings|offspring)",
                "clutch size", "eggs per nest", "productivity",
                "chicks per female", "nestlings surviving"
              ),
              collapse = "|"
            ),
            "count", "success", "productivity",

            # Everything else in this class is a period probability.

            ".", "period_survival", "success", "nest_survival"
          ),

        abundance =
          tribble(
            ~ pattern, ~ response_scale, ~ direction, ~ analysis_class,
            ".", "abundance", "success", "abundance"
          ),

        species_richness =
          tribble(
            ~ pattern, ~ response_scale, ~ direction, ~ analysis_class,

            # Evenness is J' = H' / ln(S), constructed to be independent of
            # richness, so it is not poolable with richness or diversity.

            "evenness", "evenness", "success", NA_character_,

            # Shannon and Simpson are Hill numbers of order q > 0: the same
            # community summarised with more weight on common species. Pooled
            # with richness at Brian's direction, with response_scale kept as
            # the moderator that distinguishes them.

            "diversity|shannon|simpson",
            "diversity", "success", "species_richness",

            ".", "richness", "success", "species_richness"
          ),

        occupancy =
          tribble(
            ~ pattern, ~ response_scale, ~ direction, ~ analysis_class,
            ".", "occupancy", "success", "occupancy"
          ),

        survival =
          tribble(
            ~ pattern, ~ response_scale, ~ direction, ~ analysis_class,
            "mortality", "survival", "failure", "survival",
            ".", "survival", "success", "survival"
          ),

        population_trend =
          tribble(
            ~ pattern, ~ response_scale, ~ direction, ~ analysis_class,
            ".", "trend", "success", "population_trend"
          )
      ),

    # Keyed on the lower-cased label and the extracted class; beats every
    # rule above.

    response_variable_overrides =
      tribble(
        ~ response_variable, ~ response_class,
        ~ response_scale, ~ direction, ~ analysis_class,

        # A per-egg hatching probability, not a count, despite the label
        # naming clutch size.

        "proportion of eggs hatched for a given clutch size", "nest_success",
        "period_survival", "success", "nest_survival",

        # Composition of predation events among predator types, not a
        # survival probability -- a hazard ratio would not mean anything.

        "proportion of nest predation events by domestic cats", "nest_success",
        "other", "neutral", NA_character_,

        # Labelled "abundance" but extracted under species richness.

        "abundance", "species_richness",
        "richness", "success", "species_richness"
      ),

    # Labels whose classification is defensible but debatable. Carried into
    # response_variable_class.review_note, so the calls stay visible rather
    # than buried in a regex.

    response_review_notes =
      tribble(
        ~ response_variable, ~ review_note,

        "proportion of eggs hatched for a given clutch size",
        str_c(
          "Hatching success is a per-egg probability, not a per-nest one. ",
          "Treated as period survival; the unit of analysis differs."
        ),

        "breeding success",
        str_c(
          "Ambiguous label. Treated as period survival; could be ",
          "productivity."
        ),

        str_c(
          "brood reduction of nests in which at least one chick survived ",
          "to fledging"
        ),
        "Brood-level failure, not nest-level. Direction set to failure.",

        "brood reduction of nests in which brood reduction was observed",
        "Brood-level failure, not nest-level. Direction set to failure.",

        "proportion of nest predation events by domestic cats",
        str_c(
          "A composition of predation events among predator types, not a ",
          "survival probability. Excluded from the hazard-ratio scale."
        ),

        str_c(
          "nest predation rate (number of nests depredated after 14 ",
          "days/total nests in edge)"
        ),
        "Fixed 14-day denominator, so already a period quantity.",

        "brood parasitism",
        str_c(
          "Parasitism is not mortality; pooling it with predation into one ",
          "nest-survival hazard is a substantive choice."
        ),

        "brood parasitism probability",
        "Parasitism is not mortality; see brood parasitism.",

        "brood parasitism rate",
        "Parasitism is not mortality; see brood parasitism.",

        "proportion of brood parasitized nests",
        "Parasitism is not mortality; see brood parasitism.",

        "abundance",
        str_c(
          "Label says abundance but the row was extracted under species ",
          "richness. Modelled as richness; verify against the paper."
        ),

        "species evenness",
        str_c(
          "J' = H' / ln(S) is constructed to be independent of richness ",
          "and can move opposite to it. Not poolable with richness or ",
          "diversity; excluded from the analysed pool."
        ),

        "shannon diversity",
        str_c(
          "A Hill number of order q = 1, not a species count. Pooled with ",
          "richness by decision; condition on response_scale when ",
          "modelling."
        ),

        "shannon diversity index",
        str_c(
          "A Hill number of order q = 1, not a species count. Pooled with ",
          "richness by decision; condition on response_scale when ",
          "modelling."
        ),

        "shannon index",
        str_c(
          "A Hill number of order q = 1, not a species count. Pooled with ",
          "richness by decision; condition on response_scale when ",
          "modelling."
        ),

        "species diversity",
        str_c(
          "Index unspecified in the source. Pooled with richness by ",
          "decision; condition on response_scale when modelling."
        ),

        "nest density (nests/ha)",
        str_c(
          "Under `abundance` this is a valid density measure. The single ",
          "row extracted under `nest_success` is a misfiling and is not ",
          "poolable."
        )
      )
  )
