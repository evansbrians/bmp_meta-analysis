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

* `1_pre-process_data`
    * `1.1_compare_metadata_with_study_results.R`: Compares the papers in the metadata sheet against those in the extraction sheets, and records which of them appear in the analysis table.
    * `1.2_species_classification`: Build classification systems from online sources
        * `1.2.1_iucn_bli_classification.R`: Downloads the IUCN and BirdLife species list and habitat classification.
        * `1.2.2_eubirds.R`: Habitat classifications from Storchova and Horak (2018), with common names taken from the IUCN and BirdLife listing.

* `2_process_data`
    * `2.1_clean_metadata.R`: Reads the paper metadata workbook, cleans the screening flags and the notes column, repairs the geography, and writes the result to `data/processed` with a row for each place.
    * `2.2_classify_species.R`: Combines habitat classifications from several sources and defines the obligate and facultative grassland species.
    * `2.3_clean_extraction.R`: Reformats the extraction workbook, cleans the grouping variables, flags whether a nest-success response is a daily or a period rate, and writes each tab as a csv.
    * `2.4_build_database.R`: Normalizes the cleaned inputs into a table for each level of observation and writes `data/raw/bmp_meta.duckdb`.
    * `2.5_prep_data.R`: Reads the database, restores the extraction shapes, and attaches the study and species lookups required by each shape. Nothing is screened at this stage.
    * `schema.sql`: The database schema used by the build script.

* `3_analysis`
    * `3.1_effect_sizes.R`: Converts abundance and richness records to Hedges' *g*, and nest-survival records to log hazard ratios (via a pathway defined by by each record's columns).
    * `3.2_screen_effects.R`: Applies the exclusion screen in a single pass, derives the guild, fire and pool columns used for grouping, and excludes the cells supported by fewer than three papers.
    * `3.3_models.R`: Fits the Bayesian multilevel meta-analysis models with four chains each, then writes the fits, their pools, and the cell and convergence tables. Because the fitted model output is too large for GitHub, this must be run prior to scripts that are dependent on this output.
    * `3.4_sensitivity.R`: Refits every model family under each alternative specification, prior and inclusion threshold, tests for publication bias, and flags influential effect sizes. This script requires first running `3.3_models.R` on your local machine.
    * `3.5_verification.R`: Refits every reported cell with REML as an independent check and verifies that the pools, thresholds, response scales and reported tables agree.
* `4_reporting_manuscript`
    * `4.1_screening_roses_flow.R`: Counts the records and papers retained and excluded at each screening stage, through to the three-paper cutoff.
    * `4.2_screening_draw_roses.R`: Draws the review flow diagram as an svg for editing and as a png.
    * `4.3_output_tables.R`: Converts the fits and their pools into the results tables. This script requires first running `3.3_models.R` on your local machine.
    * `4.4_figures.R`: Builds the manuscript figures from the results tables and the posterior draws.
    * `4.5_report_geographies.R`: Calculates records and papers by region to inform paragraph 2 of the methods.
* `5_reporting_supplemental`
    * `5.1_supplemental_tables.R`: Assembles the supplemental tables into a .docx and writes the sensitivity specification table.
    * `5.2_supplemental_figures.R`: Builds the supplemental figures.

### Source files

`src` contains `functions.R`, which defines every custom function used in this
analysis, and csv files of each lookup table read by the functions and
the scripts. The lookup tables provide:

* BMP names and labels printed by the figures and tables, the inclusion thresholds
* Screen reasons (i.e., inclusion and exclusion criteria for papers and records)
* The geography and species classifications assigned by hand
* The register of extraction sheets
* Description of each sensitivity specification

### Data files

* `data`: the inputs and the intermediate tables used in the analysis.
* `data/raw`: the two extraction workbooks the pipeline reads
    * `citations_by_bmp_long.xlsx`: paper-level metadata
    * `bmp_review_analysis_subset.xlsx` extracted study findings
    * `bmp_meta.duckdb`: The database written by `2.4_build_database.R` 
    *  `for_species_clasification` (folder): species classification sources
* `data/processed`:
    * `cleaned_data` (folder): Cleaned extraction sheets written in `2.3_clean_extraction.R`
    * `for_analysis` (folder): Analysis-ready data written in `2.5_prep_data.R`
* `data/db_mirror`: The converted and screened effect-size tables used to
fit the models.
* `data/flagged_effects.csv`: The data-quality register read by the sensitivity analysis in `3.4_sensitivity.R`

### Output files

`output/draft_output` contains everything produced by the analysis on the way to
the reported results: 

* `audits` (folder): The audit trail of the screen
* `diagnostics` (folder): Convergence and verification diagnostics
* `models` (folder): The fitted models and their pools
* `tables` (folder): Model results and sensitivity tables

These files are tracked so that the figures, tables and results page can be rebuilt without refitting the models. The fitted models are too large to track in GitHub and can be regenerated by running `3.3_models.R`.

The reported output is provided in `output/manuscript` and
`output/supplementals`:

* `output/manuscript`
    * `figure_1_roses_diagram`: The review flow diagram as a png and as an svg for editing, with the stage and reconciliation tables behind it
    * `figure_2_species_richness.png`: Species richness by practice
    * `figure_3_abundance_pooled.png`: Abundance by practice, guilds pooled
    * `figure_4_abundance_by_guild.png`: Abundance by practice, within guild
* `output/supplementals`
    * `snedgen_et_al_2026_supporting_information.docx`: The supplemental tables 
    * `sensitivity_specifications.csv`: Each sensitivity specification, the rule applied, and the decision tested
* `output/supplementals/supplemental_figures`
    * `figure_S1_nest_success_by_guild.png`: Nest success by practice, within guild
    * `figure_S2_nest_success_by_guild_intervals.png`: The same estimates as intervals
    * `figure_S3_abundance_guild_contrasts.png`: Obligate minus facultative abundance, by practice
    * `figure_S4_heterogeneity.png`: The variance components of each model
    * `figure_S5_species_abundance.png`: Species-level abundance estimates
