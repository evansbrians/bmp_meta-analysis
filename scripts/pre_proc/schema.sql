-- =====================================================================
-- bmp_meta-analysis :: relational schema (DuckDB)
-- =====================================================================
-- Source files
--   data/processed/paper_metadata.csv        (screening ledger, grain = study x BMP)
--   data/raw/bmp_review_analysis_subset.xlsx (5 extraction sheets of effect sizes)
--
-- Normal-form notes
--   1NF  every semicolon-delimited cell in the sources (bmp, geography,
--        response, error_class, species) is decomposed into a junction table;
--        no repeating groups, no multi-valued attributes remain.
--   2NF  all base tables use a single-column primary key, so no non-key
--        attribute can depend on part of a key. Junction tables carry no
--        non-key attributes other than ones dependent on the whole key.
--   3NF  descriptive attributes of a study (title, article type) live only in
--        `study`; per-(study,BMP) screening judgements live only in
--        `study_bmp`; per-effect-size statistics live only in the effect
--        subtype tables. Labels for controlled vocabularies live once, in
--        their lookup table, and are referenced by surrogate key.
--   BCNF every determinant listed above is a candidate key of its table.
--   4NF  the independent multi-valued facts about a study (its regions vs its
--        responses vs its BMPs) are held in separate junction tables rather
--        than one combined table, so no multi-valued dependency is violated.
--
-- Design of the effect-size hierarchy
--   `effect_size` is the supertype: one row per extracted effect size, holding
--   only what every extraction has (study, extraction type, response class,
--   response variable, notes, provenance).
--   Subtypes hang off it 1:0..1 on the same primary key, split by *statistic
--   family* rather than by source sheet, so that a beta_categorical row can
--   legitimately carry both a coefficient and a group summary without either
--   table containing structurally-empty columns:
--     effect_contrast        treatment/control arm descriptions (categorical designs)
--     effect_predictor       continuous predictor description   (continuous designs)
--     effect_group_summary   per-arm descriptive statistics
--     effect_coefficient     model coefficient and its dispersion
--     effect_test            test statistic / model output
--     effect_test_arm        per-arm n and df attached to a test (1NF split)
-- =====================================================================

-- ---------------------------------------------------------------------
-- 1. Controlled vocabularies
-- ---------------------------------------------------------------------

CREATE TABLE bmp (
    bmp_id      INTEGER      PRIMARY KEY,
    bmp_code    VARCHAR      NOT NULL UNIQUE,
    bmp_label   VARCHAR      NOT NULL,
    is_practice BOOLEAN      NOT NULL,  -- FALSE for the sentinel 'no_bmp'
    analysis_bmp_code  VARCHAR NOT NULL,
    analysis_bmp_label VARCHAR NOT NULL
);
COMMENT ON TABLE bmp IS 'Canonical best-management practices, 17 rows. analysis_bmp_code is the coarser grouping the models use, and equals bmp_code wherever no practice is pooled into another. Curated in scripts/pre_proc/db_vocab.R, bmp_analysis_groups.';


CREATE TABLE region (
    region_id    INTEGER PRIMARY KEY,
    region_code  VARCHAR NOT NULL UNIQUE,
    region_label VARCHAR NOT NULL,
    region_type  VARCHAR                            -- NULL where no reference data names the place
);
COMMENT ON TABLE region IS 'Distinct study locations, one row per place. region_type is the ISO 3166-2 subdivision category (state, province, district, ...), or country, continent or global, as 0_clean_metadata.R assigns it.';

CREATE TABLE response_type (
    response_type_id   INTEGER PRIMARY KEY,
    response_type_code VARCHAR NOT NULL UNIQUE
);
COMMENT ON TABLE response_type IS 'Response variables recorded during screening, decomposed from the semicolon-delimited response column.';

CREATE TABLE response_class (
    response_class_id   INTEGER PRIMARY KEY,
    response_class_code VARCHAR NOT NULL UNIQUE
);
COMMENT ON TABLE response_class IS 'The analytic response class assigned to an effect size (abundance, nest_success, ...).';

CREATE TABLE response_variable (
    response_variable_id INTEGER PRIMARY KEY,
    response_variable    VARCHAR NOT NULL UNIQUE
);
COMMENT ON TABLE response_variable IS 'The verbatim response measured, as reported by the source paper.';

CREATE TABLE response_variable_class (
    response_variable_id   INTEGER NOT NULL
        REFERENCES response_variable (response_variable_id),
    response_class_id      INTEGER NOT NULL
        REFERENCES response_class (response_class_id),
    response_scale         VARCHAR NOT NULL
        CHECK (response_scale IN ('daily_survival','period_survival','count',
                                  'density','duration','abundance','richness',
                                  'diversity','evenness','occupancy','survival',
                                  'trend','other')),
    direction              VARCHAR NOT NULL
        CHECK (direction IN ('success','failure','neutral')),
    analysis_response_class VARCHAR,
    is_poolable            BOOLEAN NOT NULL,
    review_note            VARCHAR,
    PRIMARY KEY (response_variable_id, response_class_id)
);
COMMENT ON TABLE response_variable_class IS 'What each response variable actually measures. The extracted response_class pools incompatible scales -- notably daily survival rate with period nest success -- so this table records the measurement scale, whether higher values are good or bad, and the class the row should be modelled under. Keyed on the PAIR (variable, extracted class), because two labels are used under two different classes: "nest density (nests/ha)" is a valid abundance measure under `abundance` but a misfiling under `nest_success`. Curated in scripts/pre_proc/db_vocab.R, response_rules; is_poolable is FALSE where the label does not belong to any poolable analysis class.';

CREATE TABLE error_class (
    error_class_id   INTEGER PRIMARY KEY,
    error_class_code VARCHAR NOT NULL UNIQUE
);
COMMENT ON TABLE error_class IS 'Kinds of uncertainty reported. An effect size may report more than one, hence effect_error_class.';

CREATE TABLE species (
    species_id          INTEGER PRIMARY KEY,
    species_name        VARCHAR NOT NULL UNIQUE,
    species_group       VARCHAR NOT NULL
        CHECK (species_group IN ('species','guild','aggregate','artificial_nest','non_bird')),
    analysis_class      VARCHAR,     -- obligate / facultative / shrub / other
    include_in_analysis BOOLEAN
);
COMMENT ON TABLE species IS 'One row per species label used in the extraction. Names arrive canonical from 2_clean_google_sheet.R, which also attaches analysis_class and include_in_analysis from data/processed/species_classification. species_group distinguishes a single species from a guild ("grassland obligates"), an undifferentiated aggregate ("all species"), artificial nest studies, and non-bird subjects.';

CREATE TABLE article_type (
    article_type_id   INTEGER PRIMARY KEY,
    article_type_code VARCHAR NOT NULL UNIQUE
);

CREATE TABLE effect_size_availability (
    availability_id   INTEGER PRIMARY KEY,
    availability_code VARCHAR NOT NULL UNIQUE
);
COMMENT ON TABLE effect_size_availability IS 'Screening judgement on whether an effect size could be obtained: yes / no / calculated / maybe / unknown.';

CREATE TABLE screening_problem (
    problem_id   INTEGER PRIMARY KEY,
    problem_code VARCHAR NOT NULL UNIQUE
);
COMMENT ON TABLE screening_problem IS 'Reason a study x BMP pairing was not usable.';

CREATE TABLE extraction_type (
    extraction_type_id   INTEGER PRIMARY KEY,
    extraction_type_code VARCHAR NOT NULL UNIQUE,
    source_sheet         VARCHAR NOT NULL UNIQUE,
    design               VARCHAR NOT NULL CHECK (design IN ('categorical','continuous'))
);
COMMENT ON TABLE extraction_type IS 'Which cleaned extraction file an effect size came from, and whether its design contrasts groups or regresses on a continuous predictor.';

CREATE TABLE model_type (
    model_type_id    INTEGER PRIMARY KEY,
    model_type_label VARCHAR NOT NULL UNIQUE
);

CREATE TABLE test_statistic (
    test_statistic_id   INTEGER PRIMARY KEY,
    test_statistic_code VARCHAR NOT NULL UNIQUE
);

-- ---------------------------------------------------------------------
-- 2. Studies
-- ---------------------------------------------------------------------

CREATE TABLE study (
    study_key         VARCHAR PRIMARY KEY,           -- 8-character Zotero item key
    paper             VARCHAR NOT NULL,
    title             VARCHAR,
    article_type_id   INTEGER REFERENCES article_type (article_type_id),
    reviewed          BOOLEAN,
    in_citation_index BOOLEAN NOT NULL               -- FALSE = appears only in the extraction workbook
);
COMMENT ON TABLE study IS 'One row per bibliographic item. study_key is the natural key shared by the screening ledger and the extraction.';

CREATE TABLE study_region (
    study_key VARCHAR NOT NULL REFERENCES study (study_key),
    region_id INTEGER NOT NULL REFERENCES region (region_id),
    PRIMARY KEY (study_key, region_id)
);

-- ---------------------------------------------------------------------
-- 3. Screening ledger  (grain of paper_metadata: study x BMP)
-- ---------------------------------------------------------------------

CREATE TABLE study_bmp (
    study_key       VARCHAR NOT NULL REFERENCES study (study_key),
    bmp_id          INTEGER NOT NULL REFERENCES bmp (bmp_id),
    availability_id INTEGER REFERENCES effect_size_availability (availability_id),
    is_useful       BOOLEAN,                          -- NULL where screening recorded 'unknown'
    problem_id      INTEGER REFERENCES screening_problem (problem_id),
    notes           VARCHAR,
    PRIMARY KEY (study_key, bmp_id)
);
COMMENT ON TABLE study_bmp IS 'Screening decision for one study against one BMP. This is the grain of the screening metadata.';

CREATE TABLE study_bmp_response (
    study_key        VARCHAR NOT NULL,
    bmp_id           INTEGER NOT NULL,
    response_type_id INTEGER NOT NULL REFERENCES response_type (response_type_id),
    PRIMARY KEY (study_key, bmp_id, response_type_id),
    FOREIGN KEY (study_key, bmp_id) REFERENCES study_bmp (study_key, bmp_id)
);
COMMENT ON TABLE study_bmp_response IS '1NF decomposition of the semicolon-delimited response column.';

-- ---------------------------------------------------------------------
-- 4. Effect sizes -- supertype
-- ---------------------------------------------------------------------

CREATE TABLE effect_size (
    effect_id            INTEGER PRIMARY KEY,
    study_key            VARCHAR NOT NULL REFERENCES study (study_key),
    extraction_type_id   INTEGER NOT NULL REFERENCES extraction_type (extraction_type_id),
    bmp_id               INTEGER REFERENCES bmp (bmp_id),
    species_id           INTEGER REFERENCES species (species_id),
    response_class_id    INTEGER REFERENCES response_class (response_class_id),
    response_variable_id INTEGER REFERENCES response_variable (response_variable_id),
    notes                VARCHAR,
    source_sheet         VARCHAR NOT NULL,
    source_row           INTEGER NOT NULL,            -- 1-based data row in the cleaned file
    UNIQUE (source_sheet, source_row)
);
COMMENT ON TABLE effect_size IS 'One row per row of a cleaned extraction file, which is one effect size for one practice and one species: 2_clean_google_sheet.R has already split the semicolon-delimited practice column, so a source row naming two practices arrives here as two effect sizes. source_sheet/source_row point back into the cleaned file and are the only surviving natural key, because the extraction contains rows identical on every substantive column.';

CREATE TABLE effect_error_class (
    effect_id      INTEGER NOT NULL REFERENCES effect_size (effect_id),
    error_class_id INTEGER NOT NULL REFERENCES error_class (error_class_id),
    PRIMARY KEY (effect_id, error_class_id)
);

-- ---------------------------------------------------------------------
-- 5. Effect sizes -- subtypes (1:0..1 on effect_id)
-- ---------------------------------------------------------------------

CREATE TABLE effect_contrast (
    effect_id             INTEGER PRIMARY KEY REFERENCES effect_size (effect_id),
    treatment_description VARCHAR,
    control_description   VARCHAR
);
COMMENT ON TABLE effect_contrast IS 'Arm descriptions for effect sizes from a categorical design.';

CREATE TABLE effect_predictor (
    effect_id            INTEGER PRIMARY KEY REFERENCES effect_size (effect_id),
    predictor_variable   VARCHAR NOT NULL
);
COMMENT ON TABLE effect_predictor IS 'Predictor description for effect sizes from a continuous design.';

CREATE TABLE effect_review (
    effect_id          INTEGER PRIMARY KEY REFERENCES effect_size (effect_id),
    effect_sign        DOUBLE,      -- +1 / -1: direction a higher value points
    contrast_flag      VARCHAR,     -- apples / oranges / grapes
    response_flag      DOUBLE,      -- 1 = response needs a second look
    response_direction DOUBLE       -- other_categorical only
);
COMMENT ON TABLE effect_review IS 'The hand review recorded on the three categorical extraction tables. contrast_flag says whether the two arms are comparable at all: "oranges" marks a contrast that does not test the practice.';

CREATE TABLE effect_group_summary (
    effect_id INTEGER PRIMARY KEY REFERENCES effect_size (effect_id),
    xbar_e    DOUBLE,
    n_e       DOUBLE,        -- DOUBLE, not INTEGER: some sources report a mean sample size
    sd_e      DOUBLE,
    se_e      DOUBLE,
    lcl_e     DOUBLE,
    ucl_e     DOUBLE,
    xbar_c    DOUBLE,
    n_c       DOUBLE,
    sd_c      DOUBLE,
    se_c      DOUBLE,
    lcl_c     DOUBLE,
    ucl_c     DOUBLE
    -- NB: lcl <= ucl is NOT enforced as a constraint. The source workbooks
    -- contain a handful of transposed interval bounds; those values are
    -- loaded verbatim and reported in data_quality_issue instead, so that the
    -- database stays a faithful mirror of the extraction sheets.
    -- Non-negativity of sd/se and positivity of n are likewise reported
    -- rather than enforced, for the same reason.
);
COMMENT ON TABLE effect_group_summary IS 'Descriptive statistics for the treatment (_e) and control (_c) arms. Populated for every mean-difference extraction and for the beta_categorical rows that also reported arm means.';

CREATE TABLE effect_coefficient (
    effect_id INTEGER PRIMARY KEY REFERENCES effect_size (effect_id),
    beta      DOUBLE,
    n         DOUBLE,
    sd        DOUBLE,
    se        DOUBLE,
    lcl       DOUBLE,
    ucl       DOUBLE
    -- see the note on effect_group_summary regarding lcl <= ucl,
    -- sd/se non-negativity and n positivity
);
COMMENT ON TABLE effect_coefficient IS 'A model coefficient reported directly by the source paper, with its dispersion.';

CREATE TABLE effect_test (
    effect_id         INTEGER PRIMARY KEY REFERENCES effect_size (effect_id),
    model_type_id     INTEGER REFERENCES model_type (model_type_id),
    test_statistic_id INTEGER REFERENCES test_statistic (test_statistic_id),
    test_stat_value   DOUBLE,
    df                DOUBLE,
    global_n          DOUBLE,
    global_lcl        DOUBLE,
    global_ucl        DOUBLE
    -- see the note on effect_group_summary regarding lcl <= ucl
    --  and global_n positivity
);
COMMENT ON TABLE effect_test IS 'Effect sizes reported only as a test statistic. The all-NULL global_sd/global_se columns of the source sheets are not carried over.';

CREATE TABLE effect_test_arm (
    effect_id INTEGER NOT NULL REFERENCES effect_test (effect_id),
    arm       VARCHAR NOT NULL CHECK (arm IN ('treatment','control')),
    n         DOUBLE,
    df        DOUBLE,
    PRIMARY KEY (effect_id, arm)
);
COMMENT ON TABLE effect_test_arm IS '1NF decomposition of the repeating treatment_n/treatment_df and control_n/control_df group in the "other" sheets. The parallel _sd and _se columns are empty in the sources and are not carried over.';

-- ---------------------------------------------------------------------
-- 6. Data-quality register
-- ---------------------------------------------------------------------

CREATE TABLE data_quality_issue (
    issue_id    INTEGER PRIMARY KEY,
    effect_id   INTEGER REFERENCES effect_size (effect_id),
    study_key   VARCHAR REFERENCES study (study_key),
    issue_code  VARCHAR NOT NULL,
    detail      VARCHAR NOT NULL
);
COMMENT ON TABLE data_quality_issue IS 'Values that were loaded verbatim but violate an expectation (transposed interval bounds, point estimates outside their own interval, orphan study keys). Populated by the ETL; nothing here is silently corrected.';
