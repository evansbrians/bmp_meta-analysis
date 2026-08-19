-- ==========================================================================
-- bmp_meta-analysis :: relational schema (DuckDB)
-- ==========================================================================
-- Ten tables, loaded by 3_build_database.R: one per level of observation,
-- from data/processed, plus the practice vocabulary.
--
-- Keys are natural. study_key is the bibliographic key the sheets record.
-- effect_id is <sheet code>_<index>, assigned over the rows of a cleaned sheet
-- that are one observation, and is the key the analysis shares.
--
-- There are no surrogate-key registries: a controlled vocabulary is stored as
-- its label. The one lookup table, bmp, adds no key -- it maps each practice
-- label to the formal name the manuscript prints, and is loaded from
-- bmp_vocabulary in scripts/functions.R. Two source columns are not carried
-- because they restate data already held here -- multiple_bmps restates
-- study_bmp, and error_class restates which dispersion columns are populated.
-- Both are checked in the prep script, not here.
-- ==========================================================================


CREATE TABLE bmp (
    bmp       TEXT PRIMARY KEY,
    bmp_name  TEXT NOT NULL
);


CREATE TABLE study (
    study_key     TEXT PRIMARY KEY,
    reviewed      TEXT,
    paper         TEXT,
    title         TEXT,
    article_type  TEXT,
    in_metadata   BOOLEAN NOT NULL
);


CREATE TABLE study_place (
    study_key       TEXT NOT NULL REFERENCES study (study_key),
    geography       TEXT NOT NULL,
    geography_type  TEXT,
    continent       TEXT,
    PRIMARY KEY (study_key, geography)
);


CREATE TABLE study_bmp (
    study_key         TEXT NOT NULL REFERENCES study (study_key),
    bmp               TEXT NOT NULL,
    effect_size       TEXT,
    useful            TEXT,
    problem           TEXT,
    additional_notes  TEXT,
    PRIMARY KEY (study_key, bmp)
);


CREATE TABLE study_bmp_response (
    study_key  TEXT NOT NULL,
    bmp        TEXT NOT NULL,
    response   TEXT NOT NULL,
    PRIMARY KEY (study_key, bmp, response),
    FOREIGN KEY (study_key, bmp) REFERENCES study_bmp (study_key, bmp)
);


CREATE TABLE species (
    species         TEXT PRIMARY KEY,
    species_group   TEXT,
    analysis_class  TEXT,
    include         BOOLEAN
);


CREATE TABLE effect (
    effect_id               TEXT PRIMARY KEY,
    study_key               TEXT NOT NULL REFERENCES study (study_key),
    source_sheet            TEXT NOT NULL,
    source_row              INTEGER NOT NULL,
    extraction_type         TEXT NOT NULL,
    design                  TEXT NOT NULL,
    response_class          TEXT,
    response_var            TEXT,
    survival_scale          TEXT,
    link                    TEXT,
    baseline_survival       DOUBLE,
    beta_is_derived         BOOLEAN,
    treatment               TEXT,
    control                 TEXT,
    species                 TEXT REFERENCES species (species),
    sign                    INTEGER,
    treatment_control_flag  TEXT,
    response_flag           INTEGER,
    response_dir            INTEGER,
    notes                   TEXT,
    CHECK (design IN ('categorical', 'continuous'))
);


CREATE TABLE effect_bmp (
    effect_id  TEXT NOT NULL REFERENCES effect (effect_id),
    bmp        TEXT NOT NULL,
    PRIMARY KEY (effect_id, bmp)
);


CREATE TABLE effect_arm (
    effect_id  TEXT NOT NULL REFERENCES effect (effect_id),
    arm        TEXT NOT NULL,
    xbar       DOUBLE,
    n          DOUBLE,
    sd         DOUBLE,
    se         DOUBLE,
    lcl        DOUBLE,
    ucl        DOUBLE,
    df         DOUBLE,
    PRIMARY KEY (effect_id, arm),
    CHECK (arm IN ('treatment', 'control'))
);


CREATE TABLE effect_estimate (
    effect_id        TEXT PRIMARY KEY REFERENCES effect (effect_id),
    statistic_type   TEXT,
    statistic_value  DOUBLE,
    model            TEXT,
    n                DOUBLE,
    sd               DOUBLE,
    se               DOUBLE,
    lcl              DOUBLE,
    ucl              DOUBLE,
    df               DOUBLE
);
