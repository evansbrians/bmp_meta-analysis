-- ==========================================================================
-- Convenience views. The base tables are normalised; these flatten them back
-- into the shapes an analysis will actually want. Executed by
-- scripts/pre_proc/3_build_database.R after schema.sql.
-- ==========================================================================


-- ==========================================================================
-- v_nest_survival_effect -- daily and period nest success on one scale
-- ==========================================================================
-- Period survival S = DSR^d, so the two are not comparable as a standardised
-- mean difference: the same biology yields a different Hedges' g depending on
-- which the paper reported. The complementary log-log transform removes the
-- exposure period. Writing cll(x) = log(-log(x)),
--
--     cll(S) = cll(DSR^d) = log(d) + cll(DSR)
--
-- so log(d) is shared by both arms and cancels in the difference:
--
--     log_hazard_ratio = cll(S_treatment) - cll(S_control)
--
-- That is the log ratio of cumulative hazards, positive when the treatment
-- has HIGHER mortality. Its standard error follows by the delta method, since
-- d/dS cll(S) = -1 / (S log S):
--
--     SE(cll(S)) = SE(S) / |S * log(S)|
--
-- Before transforming: percentages are detected (any arm > 1) and divided by
-- 100, and failure-direction responses are converted to survival as 1 - x so
-- that every row points the same way. Rows that still fall outside (0, 1)
-- are returned with a null estimate and a stated reason.
CREATE OR REPLACE VIEW v_nest_survival_effect AS
WITH base AS (
    SELECT
        e.effect_id,
        e.study_key,
        s.paper,
        rv.response_variable,
        rvc.response_scale,
        rvc.direction,
        gs.n_e,
        gs.n_c,
        CASE WHEN greatest(gs.xbar_e, gs.xbar_c) > 1
             THEN 100.0 ELSE 1.0 END              AS unit_divisor,
        gs.xbar_e                                 AS raw_e,
        gs.xbar_c                                 AS raw_c,
        coalesce(gs.se_e, gs.sd_e / sqrt(gs.n_e)) AS raw_se_e,
        coalesce(gs.se_c, gs.sd_c / sqrt(gs.n_c)) AS raw_se_c,
        e.source_sheet,
        e.source_row
    FROM effect_size e
    JOIN study s
      ON s.study_key = e.study_key
    JOIN response_variable rv
      ON rv.response_variable_id = e.response_variable_id
    JOIN response_variable_class rvc
      ON rvc.response_variable_id = e.response_variable_id
     AND rvc.response_class_id = e.response_class_id
    JOIN effect_group_summary gs
      ON gs.effect_id = e.effect_id
    WHERE rvc.response_scale IN ('daily_survival', 'period_survival')
      AND gs.xbar_e IS NOT NULL
      AND gs.xbar_c IS NOT NULL
), scaled AS (
    SELECT *,
        CASE WHEN direction = 'failure'
             THEN 1 - raw_e / unit_divisor
             ELSE raw_e / unit_divisor END AS s_e,
        CASE WHEN direction = 'failure'
             THEN 1 - raw_c / unit_divisor
             ELSE raw_c / unit_divisor END AS s_c,
        raw_se_e / unit_divisor            AS se_s_e,
        raw_se_c / unit_divisor            AS se_s_c
    FROM base
), guarded AS (
    -- A daily rate below 0.5 implies essentially no nest ever fledges, so it
    -- is an extraction error rather than a real value. The percentage
    -- heuristic above cannot rescue such a row and converting it would emit a
    -- large spurious hazard ratio. Guard rather than convert.
    SELECT *,
        (s_e > 0 AND s_e < 1 AND s_c > 0 AND s_c < 1
         AND NOT (response_scale = 'daily_survival'
                  AND least(s_e, s_c) < 0.5)) AS is_plausible
    FROM scaled
)
SELECT
    effect_id,
    study_key,
    paper,
    response_variable,
    response_scale,
    direction,
    CASE WHEN unit_divisor = 100
         THEN 'percent' ELSE 'proportion' END AS input_units,
    s_e    AS survival_e,
    s_c    AS survival_c,
    se_s_e AS se_survival_e,
    se_s_c AS se_survival_c,
    n_e,
    n_c,
    is_plausible,
    CASE WHEN is_plausible
         THEN ln(-ln(s_e)) - ln(-ln(s_c)) END AS log_hazard_ratio,
    CASE WHEN is_plausible
          AND se_s_e IS NOT NULL
          AND se_s_c IS NOT NULL
         THEN sqrt(power(se_s_e / abs(s_e * ln(s_e)), 2) +
                   power(se_s_c / abs(s_c * ln(s_c)), 2))
         END AS se_log_hazard_ratio,
    CASE
        WHEN s_e IS NULL OR s_c IS NULL
            THEN 'missing arm value'
        WHEN s_e <= 0 OR s_e >= 1 OR s_c <= 0 OR s_c >= 1
            THEN 'survival outside (0, 1) after unit and direction handling'
        WHEN response_scale = 'daily_survival' AND least(s_e, s_c) < 0.5
            THEN 'implausible daily survival rate (< 0.5); check the extraction'
        WHEN se_s_e IS NULL OR se_s_c IS NULL
            THEN 'point estimate only; no arm standard error'
    END AS conversion_problem,
    source_sheet,
    source_row
FROM guarded;


-- ==========================================================================
-- v_nest_survival_unconvertible -- and why each one cannot be converted
-- ==========================================================================
-- Coefficient rows dominate: a beta for DSR is a slope on a link the
-- extraction sheets never recorded, so there is nothing to transform.
CREATE OR REPLACE VIEW v_nest_survival_unconvertible AS
SELECT
    e.effect_id,
    e.study_key,
    s.paper,
    rv.response_variable,
    rvc.response_scale,
    rvc.direction,
    xt.extraction_type_code AS extraction_type,
    CASE
        WHEN gs.effect_id IS NULL
            THEN 'no arm probabilities: reported as a coefficient or test statistic'
        WHEN gs.xbar_e IS NULL OR gs.xbar_c IS NULL
            THEN 'only one arm reported'
    END AS reason,
    e.source_sheet,
    e.source_row
FROM effect_size e
JOIN study s
  ON s.study_key = e.study_key
JOIN response_variable rv
  ON rv.response_variable_id = e.response_variable_id
JOIN response_variable_class rvc
  ON rvc.response_variable_id = e.response_variable_id
 AND rvc.response_class_id = e.response_class_id
JOIN extraction_type xt
  ON xt.extraction_type_id = e.extraction_type_id
LEFT JOIN effect_group_summary gs
  ON gs.effect_id = e.effect_id
WHERE rvc.response_scale IN ('daily_survival', 'period_survival')
  AND (gs.effect_id IS NULL OR gs.xbar_e IS NULL OR gs.xbar_c IS NULL);


-- ==========================================================================
-- v_effect_size_wide -- one row per effect size, the shape of the sheets
-- ==========================================================================
-- BMPs, species and error classes are re-aggregated into semicolon-delimited
-- lists, and every statistic column is carried. This is the closest analogue
-- of the original spreadsheet rows.
CREATE OR REPLACE VIEW v_effect_size_wide AS
SELECT
    e.effect_id,
    e.study_key,
    s.paper,
    s.title,
    art.article_type_code   AS article_type,
    xt.extraction_type_code AS extraction_type,
    xt.design,
    rc.response_class_code  AS response_class,
    rv.response_variable,
    rvc.analysis_response_class,
    rvc.response_scale,
    rvc.direction,
    CASE WHEN rvc.direction = 'failure'
         THEN -1 ELSE 1 END AS response_direction,
    rvc.is_poolable,
    nsv.log_hazard_ratio,
    nsv.se_log_hazard_ratio,
    b.bmp_code,
    b.bmp_label,
    b.analysis_bmp_code,
    sp.species_name,
    sp.species_group,
    sp.analysis_class AS species_analysis_class,
    sp.include_in_analysis AS species_included,
    ec.error_classes,
    rev.effect_sign,
    rev.contrast_flag,
    rev.response_flag,
    ctr.treatment_description,
    ctr.control_description,
    prd.predictor_variable,
    gs.xbar_e, gs.n_e, gs.sd_e, gs.se_e, gs.lcl_e, gs.ucl_e,
    gs.xbar_c, gs.n_c, gs.sd_c, gs.se_c, gs.lcl_c, gs.ucl_c,
    cf.beta, cf.n, cf.sd, cf.se, cf.lcl, cf.ucl,
    mt.model_type_label     AS model,
    tsl.test_statistic_code AS test_statistic,
    tst.test_stat_value,
    tst.df,
    tst.global_n, tst.global_lcl, tst.global_ucl,
    e.notes,
    e.source_sheet,
    e.source_row
FROM effect_size e
JOIN study s
  ON s.study_key = e.study_key
JOIN extraction_type xt
  ON xt.extraction_type_id = e.extraction_type_id
LEFT JOIN article_type art
  ON art.article_type_id = s.article_type_id
LEFT JOIN response_class rc
  ON rc.response_class_id = e.response_class_id
LEFT JOIN response_variable rv
  ON rv.response_variable_id = e.response_variable_id
LEFT JOIN response_variable_class rvc
  ON rvc.response_variable_id = e.response_variable_id
 AND rvc.response_class_id = e.response_class_id
LEFT JOIN v_nest_survival_effect nsv
  ON nsv.effect_id = e.effect_id
LEFT JOIN bmp b
  ON b.bmp_id = e.bmp_id
LEFT JOIN species sp
  ON sp.species_id = e.species_id
LEFT JOIN effect_review rev
  ON rev.effect_id = e.effect_id
LEFT JOIN (
    SELECT ee.effect_id,
           string_agg(x.error_class_code, '; ' ORDER BY x.error_class_code)
               AS error_classes
    FROM effect_error_class ee
    JOIN error_class x ON x.error_class_id = ee.error_class_id
    GROUP BY ee.effect_id
) ec ON ec.effect_id = e.effect_id
LEFT JOIN effect_contrast      ctr ON ctr.effect_id = e.effect_id
LEFT JOIN effect_predictor     prd ON prd.effect_id = e.effect_id
LEFT JOIN effect_group_summary gs  ON gs.effect_id = e.effect_id
LEFT JOIN effect_coefficient   cf  ON cf.effect_id = e.effect_id
LEFT JOIN effect_test          tst ON tst.effect_id = e.effect_id
LEFT JOIN model_type           mt  ON mt.model_type_id = tst.model_type_id
LEFT JOIN test_statistic       tsl
  ON tsl.test_statistic_id = tst.test_statistic_id;


-- ==========================================================================
-- v_effect_size_long -- the modelling grain, named for what it carries
-- ==========================================================================
-- One row per effect size, practice and species. That is already the grain of
-- effect_size, because the cleaning step split the practice column, so this
-- view only resolves the surrogate keys to labels.
CREATE OR REPLACE VIEW v_effect_size_long AS
SELECT
    e.effect_id,
    e.study_key,
    s.paper,
    b.bmp_code,
    b.bmp_label,
    b.analysis_bmp_code,
    b.analysis_bmp_label,
    sp.species_name         AS species,
    sp.species_group,
    sp.analysis_class       AS species_analysis_class,
    sp.include_in_analysis  AS species_included,
    rc.response_class_code  AS response_class,
    rv.response_variable,
    rvc.analysis_response_class,
    rvc.response_scale,
    rvc.is_poolable,
    xt.extraction_type_code AS extraction_type,
    xt.design
FROM effect_size e
JOIN study s
  ON s.study_key = e.study_key
JOIN extraction_type xt
  ON xt.extraction_type_id = e.extraction_type_id
LEFT JOIN bmp b
  ON b.bmp_id = e.bmp_id
LEFT JOIN species sp
  ON sp.species_id = e.species_id
LEFT JOIN response_class rc
  ON rc.response_class_id = e.response_class_id
LEFT JOIN response_variable rv
  ON rv.response_variable_id = e.response_variable_id
LEFT JOIN response_variable_class rvc
  ON rvc.response_variable_id = e.response_variable_id
 AND rvc.response_class_id = e.response_class_id;


-- ==========================================================================
-- v_screening -- the ledger, flattened back to one row per study x BMP
-- ==========================================================================
CREATE OR REPLACE VIEW v_screening AS
SELECT
    sb.study_key,
    s.paper,
    s.title,
    s.reviewed,
    art.article_type_code AS article_type,
    b.bmp_code,
    b.bmp_label,
    geo.regions           AS geography,
    resp.responses        AS response,
    av.availability_code  AS effect_size_available,
    sb.is_useful,
    pr.problem_code       AS problem,
    sb.notes
FROM study_bmp sb
JOIN study s
  ON s.study_key = sb.study_key
JOIN bmp b
  ON b.bmp_id = sb.bmp_id
LEFT JOIN article_type art
  ON art.article_type_id = s.article_type_id
LEFT JOIN effect_size_availability av
  ON av.availability_id = sb.availability_id
LEFT JOIN screening_problem pr
  ON pr.problem_id = sb.problem_id
LEFT JOIN (
    SELECT sr.study_key,
           string_agg(r.region_code, '; ' ORDER BY r.region_code) AS regions
    FROM study_region sr
    JOIN region r ON r.region_id = sr.region_id
    GROUP BY sr.study_key
) geo ON geo.study_key = sb.study_key
LEFT JOIN (
    SELECT sbr.study_key,
           sbr.bmp_id,
           string_agg(rt.response_type_code, '; '
                      ORDER BY rt.response_type_code) AS responses
    FROM study_bmp_response sbr
    JOIN response_type rt ON rt.response_type_id = sbr.response_type_id
    GROUP BY sbr.study_key, sbr.bmp_id
) resp ON resp.study_key = sb.study_key AND resp.bmp_id = sb.bmp_id;


-- ==========================================================================
-- v_bmp_coverage -- how many usable effect sizes each practice has
-- ==========================================================================
CREATE OR REPLACE VIEW v_bmp_coverage AS
SELECT
    b.bmp_code,
    b.bmp_label,
    count(DISTINCT sb.study_key)
        AS studies_screened,
    count(DISTINCT sb.study_key) FILTER (WHERE sb.is_useful)
        AS studies_useful,
    count(DISTINCT e.effect_id)
        AS effect_sizes,
    count(DISTINCT e.study_key)
        AS studies_extracted
FROM bmp b
LEFT JOIN study_bmp sb  ON sb.bmp_id = b.bmp_id
LEFT JOIN effect_size e ON e.bmp_id = b.bmp_id
GROUP BY b.bmp_id, b.bmp_code, b.bmp_label
ORDER BY effect_sizes DESC, b.bmp_code;


-- ==========================================================================
-- v_species_usage -- every species label, its classification and its weight
-- ==========================================================================
CREATE OR REPLACE VIEW v_species_usage AS
SELECT
    sp.species_name,
    sp.species_group,
    sp.analysis_class,
    sp.include_in_analysis,
    coalesce(use.n_effects, 0) AS n_effects,
    coalesce(use.n_studies, 0) AS n_studies
FROM species sp
LEFT JOIN (
    SELECT species_id,
           count(*)                    AS n_effects,
           count(DISTINCT study_key)   AS n_studies
    FROM effect_size
    GROUP BY species_id
) use ON use.species_id = sp.species_id
ORDER BY sp.species_group, sp.species_name;


-- ==========================================================================
-- v_response_variable_class -- the classification, with usage counts
-- ==========================================================================
-- The whole curation in one table, for review.
CREATE OR REPLACE VIEW v_response_variable_class AS
SELECT
    rv.response_variable,
    rc.response_class_code      AS extracted_class,
    rvc.response_scale,
    rvc.direction,
    rvc.analysis_response_class,
    rvc.is_poolable,
    rvc.review_note,
    count(*)                    AS n_effects,
    count(DISTINCT e.study_key) AS n_studies
FROM response_variable rv
JOIN response_variable_class rvc
  ON rvc.response_variable_id = rv.response_variable_id
JOIN effect_size e
  ON e.response_variable_id = rv.response_variable_id
 AND e.response_class_id = rvc.response_class_id
LEFT JOIN response_class rc
  ON rc.response_class_id = rvc.response_class_id
GROUP BY rv.response_variable,
         rc.response_class_code,
         rvc.response_scale,
         rvc.direction,
         rvc.analysis_response_class,
         rvc.is_poolable,
         rvc.review_note
ORDER BY rvc.response_scale, rvc.direction, lower(rv.response_variable);


-- ==========================================================================
-- v_nest_survival_disagreement -- survival and failure measures that conflict
-- ==========================================================================
-- Within one study, contrast and species, an all-cause survival measure and a
-- cause-specific failure measure (depredation, parasitism) should point the
-- same way even though they are not the same quantity. Where they disagree in
-- sign, one of the two extractions is usually wrong.
CREATE OR REPLACE VIEW v_nest_survival_disagreement AS
SELECT
    v.paper,
    v.study_key,
    c.treatment_description,
    c.control_description,
    sp.species_name AS species,
    max(v.response_variable) FILTER (WHERE v.direction = 'success')
        AS survival_measure,
    max(v.log_hazard_ratio) FILTER (WHERE v.direction = 'success')
        AS log_hazard_ratio_survival,
    max(v.response_variable) FILTER (WHERE v.direction = 'failure')
        AS failure_measure,
    max(v.log_hazard_ratio) FILTER (WHERE v.direction = 'failure')
        AS log_hazard_ratio_failure
FROM v_nest_survival_effect v
JOIN effect_contrast c
  ON c.effect_id = v.effect_id
JOIN effect_size e
  ON e.effect_id = v.effect_id
JOIN species sp
  ON sp.species_id = e.species_id
WHERE v.log_hazard_ratio IS NOT NULL
GROUP BY 1, 2, 3, 4, 5
HAVING count(DISTINCT v.direction) = 2
   AND sign(max(v.log_hazard_ratio) FILTER (WHERE v.direction = 'success'))
    <> sign(max(v.log_hazard_ratio) FILTER (WHERE v.direction = 'failure'));
