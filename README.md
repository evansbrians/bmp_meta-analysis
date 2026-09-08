# Meta-analysis of best management practices for grassland bird conservation on working lands

This repository contains scripts, data files, and output used in a meta-analysis of the efficacy of best management practices on facultative and obligate birds in working landscapes.

## About this study

Grassland bird populations are declining in response to multiple ecological pressures associated with anthropogenic land-use change. Working lands represent the last line of defense for many species across geographies, though current management activities can be beneficial or detrimental. Best Management Practices (BMPs) are defined here as operational, vegetative, or structural interventions recommended to promote wildlife conservation on working landscapes. Despite the widespread application of these BMPs, their relative efficacy across obligate and facultative grassland bird species and among response metrics (e.g., nest success, abundance) is largely unexplored. We conducted a systematic review and meta-analysis to evaluate which BMPs were most strongly associated with avian responses, explore potential trade-offs among response metrics and avian guilds, and identify BMPs with insufficient empirical support. Although several BMPs could not be included in our analysis due to a lack of empirical research, at least one BMP was supported to have a positive effect on each response metric and guild. Eliminate Pesticides was positively associated with species richness, whereas Install Nest-Boxes and Plant Native Grasses and Forbs were positively associated with overall abundance. Avoid Haying During the Nesting Season was the only practice positively associated with improved nest success in grassland obligate birds. Some BMPs showed contrasting effects on abundance when analyzed by guild. Notably, Implement Prescribed Fire was positively associated with obligate abundance but negatively associated with facultative abundance. Future research on BMPs should evaluate metrics of habitat quality, recommendations with insufficient empirical support, disaggregated effects across application strategies, and interactions among practices. Our results indicate that effective conservation on working lands requires interventions that target specific ecological contexts, taxa, and outcomes.

## Repository structure

The pipeline runs in the order the numbered folders are named. Searching comes
first, then `scripts`, which writes to `data` and then to `output`. `src`
contains the functions and lookup tables that every script sources.

### Search files

`searches` contains the search strategy and the code used to build it.
`searches/bmps` and `searches/response_metrics` contain a text file for each
practice and each response metric. The scripts in `searches/search_scripts`
write `response_search.txt` and `species_search.txt`, and combine those with a
practice term into the strings pasted into Web of Science.

### Script organization

Data processing and analysis scripts are located in the `scripts` folder.
Scripts include:

* `1_pre_processing`
    * `compare_metadata_with_study_results.R`: Compares the papers in the metadata sheet against those in the extraction sheets, and records which of them appear in the analysis table.
* `2_process_data`
    * `0_clean_metadata.R`: Reads the paper metadata workbook, cleans the screening flags and the notes column, repairs the geography, and writes the result to `data/processed` with a row for each place.
    * `1_classify_species.R`: Combines habitat classifications from several sources and defines the obligate and facultative grassland species.
    * `2_clean_extraction.R`: Reformats the extraction workbook, cleans the grouping variables, flags whether a nest-success response is a daily or a period rate, and writes each tab as a csv.
    * `3_build_database.R`: Normalizes the cleaned inputs into a table for each level of observation and writes `data/raw/bmp_meta.duckdb`.
    * `schema.sql`: The database schema used by the build script.
* `2_process_data/species_classification`
    * `eubirds.R`: Habitat classifications from Storchova and Horak (2018), with common names taken from the IUCN and BirdLife listing.
    * `iucn_bli_classification.R`: Downloads the IUCN and BirdLife species list and habitat classification.
* `3_analysis`
    * `0_prep_data.R`: Reads the database, restores the extraction shapes, and attaches the study and species lookups required by each shape. Nothing is screened at this stage.
    * `1_effect_sizes.R`: Converts abundance and richness records to Hedges' *g*, and nest-survival records to log hazard ratios (via a pathway defined by by each record's columns).
    * `2_screen_effects.R`: Applies the exclusion screen in a single pass, derives the guild, fire and pool columns used for grouping, and excludes the cells supported by fewer than three papers.
    * `3_models.R`: Fits the Bayesian multilevel meta-analysis models with four chains each, then writes the fits, their pools, and the cell and convergence tables.
    * `4_sensitivity.R`: Refits every model family under each alternative specification, prior and inclusion threshold, tests for publication bias, and flags influential effect sizes.
    * `5_verification.R`: Refits every reported cell with REML as an independent check and verifies that the pools, thresholds, response scales and reported tables agree.
* `4_reporting_manuscript`
    * `1_screening_roses_flow.R`: Counts the records and papers retained and excluded at each screening stage, through to the three-paper cutoff.
    * `2_screening_draw_roses.R`: Draws the review flow diagram as an svg for editing and as a png.
    * `3_output_tables.R`: Converts the fits and their pools into the results tables.
    * `4_figures.R`: Builds the manuscript figures from the results tables and the posterior draws.
* `5_reporting_supplemental`
    * `1_report_geographies.R`: Writes the paper and record counts by region, and by practice and region.
    * `2_supplemental_tables.R`: Assembles the supplemental tables into a .docx and writes the sensitivity specification table.
    * `3_supplemental_figures.R`: Builds the supplemental figures.

### Source files

`src` contains `functions.R`, which defines every function used in this
analysis, together with a csv for each lookup table read by the functions and
the scripts. The lookup tables provide the practice vocabulary and the labels
printed by the figures and tables, the inclusion thresholds, the screen
reasons, the geography and species classifications assigned by hand, the
register of extraction sheets, and the description of each sensitivity
specification.

### Data files

`data` contains the inputs and the intermediate tables used in the analysis.
`data/raw` contains the two extraction workbooks the pipeline reads,
`citations_by_bmp_long.xlsx` for the paper metadata and
`bmp_review_analysis_subset.xlsx` for the extracted study findings, together
with the database written by `3_build_database.R` and the species
classification sources used to build it. `data/processed` contains the
cleaned extraction sheets and the shapes derived from them for analysis.
`data/db_mirror` contains the converted and screened effect-size tables used to
fit the models. `data/flagged_effects.csv` is the data-quality register read by
the sensitivity analysis.

### Output files

`output/draft_output` contains everything produced by the analysis on the way to
the reported results: the fitted models and their pools, the audit trail of the
screen, the convergence and verification diagnostics, and the results and
sensitivity tables. These files are tracked so that the figures, tables and
results page can be rebuilt without refitting the models. The fitted models
are too large to track in GitHub and can be regenerated by running
`3_analysis/3_models.R`.

The reported output is provided in `output/manuscript` and
`output/supplementals`:

* `output/manuscript`
    * `figure_1_roses_diagram`: The review flow diagram as a png and as an svg for editing, with the stage and reconciliation tables behind it.
    * `figure_2_species_richness.png`: Species richness by practice.
    * `figure_3_abundance_pooled.png`: Abundance by practice, guilds pooled.
    * `figure_4_abundance_by_guild.png`: Abundance by practice, within guild.
* `output/supplementals`
    * `supplemental_tables.docx`: The supplemental tables.
    * `sensitivity_specifications.csv`: Each sensitivity specification, the rule applied, and the decision tested.
* `output/supplementals/supplemental_figures`
    * `figure_S1_nest_success_by_guild.png`: Nest success by practice, within guild.
    * `figure_S2_nest_success_by_guild_intervals.png`: The same estimates as intervals.
    * `figure_S3_abundance_guild_contrasts.png`: Obligate minus facultative abundance, by practice.
    * `figure_S4_heterogeneity.png`: The variance components of each model.
    * `figure_S5_species_abundance.png`: Species-level abundance estimates.
