# Meta-analysis of best management practices for grassland bird conservation on working lands

This repository contains scripts, data files, and output used in a meta-analysis of the efficacy of best management practices on facultative and obligate birds in working landscapes.

## About this study

Grassland bird populations are declining in response to multiple ecological pressures associated with anthropogenic land-use change. Working lands represent the last line of defense for many species across geographies, though current management activities can be beneficial or detrimental. Best Management Practices (BMPs) are defined here as operational, vegetative, or structural interventions recommended to promote wildlife conservation on working landscapes. Despite the widespread application of these BMPs, their relative efficacy across obligate and facultative grassland bird species and among response metrics (e.g., nest success, abundance) is largely unexplored. We conducted a systematic review and meta-analysis to evaluate which BMPs were most strongly associated with avian responses, explore potential trade-offs among response metrics and avian guilds, and identify BMPs with insufficient empirical support. Although several BMPs could not be included in our analysis due to a lack of empirical research, at least one BMP was supported to have a positive effect on each response metric and guild. Eliminate Pesticides was positively associated with species richness, whereas Install Nest-Boxes and Plant Native Grasses and Forbs were positively associated with overall abundance. Avoid Haying During the Nesting Season was the only practice positively associated with improved nest success in grassland obligate birds. Some BMPs showed contrasting effects on abundance when analyzed by guild. Notably, Implement Prescribed Fire was positively associated with obligate abundance but negatively associated with facultative abundance. Future research on BMPs should evaluate metrics of habitat quality, recommendations with insufficient empirical support, disaggregated effects across application strategies, and interactions among practices. Our results indicate that effective conservation on working lands requires interventions that target specific ecological contexts, taxa, and outcomes.

## Repository structure

### Files to conduct searching

Lorem ipsum

* `searches`: Lorem ipsum
    * `response_search.txt`: Lorem ipsum
    * `species_search.txt`: Lorem ipsum
* `searches/bmps`: Lorem ipsum
    * `delay_hay.txt`: Lorem ipsum
    * `edge_and_shrub.txt`: Lorem ipsum
    * `eliminate_pesticides.txt`: Lorem ipsum
    * `flushing_bar.txt`: Lorem ipsum
    * `grazing_intensity.txt`: Lorem ipsum
    * `manage_in_patches.txt`: Lorem ipsum
    * `mow_in_day.txt`: Lorem ipsum
    * `mow_toward_refugia.txt`: Lorem ipsum
    * `nest_boxes.txt`: Lorem ipsum
    * `plant_nwsg.txt`: Lorem ipsum
    * `prescribed_fire.txt`: Lorem ipsum
    * `raise_your_blades.txt`: Lorem ipsum
    * `remove_non-native.txt`: Lorem ipsum
    * `rotational_grazing.txt`: Lorem ipsum
    * `stream_exclusion_and_buffers.txt`: Lorem ipsum
* `searches/response_metrics`: Lorem ipsum
    * `abundance.txt`: Lorem ipsum
    * `nest_success.txt`: Lorem ipsum
    * `species_richness.txt`: Lorem ipsum
    * `survival.txt`: Lorem ipsum
* `searches/search_scripts`: Lorem ipsum
    * `combine_search_strings.r`: Lorem ipsum
    * `generate_response_search.R`: Lorem ipsum
    * `generate_species_search.R`: Lorem ipsum
    * `make_paper_counts_table.R`: Lorem ipsum

### Script organization

Data processing and analysis scripts are located in the `scripts` folder. Scripts include:

* `1_pre_processing`: Lorem ipsum
    * `compare_metadata_with_study_results.R`: Lorem ipsum
* `2_process_data`: Lorem ipsum
    * `0_clean_metadata_gsheet.R`: Lorem ipsum
    * `1_classify_species.R`: Lorem ipsum
    * `2_clean_extraction_gsheet.R`: Lorem ipsum
    * `3_build_database.R`: Lorem ipsum
    * `schema.sql`: Lorem ipsum
* `2_process_data/species_classification`: Lorem ipsum
    * `eubirds.R`: Lorem ipsum
    * `iucn_bli_classification.R`: Lorem ipsum
* `3_analysis`: Lorem ipsum
    * `0_prep_data.R`: Lorem ipsum
    * `1_effect_sizes.R`: Lorem ipsum
    * `2_screen_effects.R`: Lorem ipsum
    * `3_models.R`: Lorem ipsum
    * `4_sensitivity.R`: Lorem ipsum
* `4_reporting_manuscript`: Lorem ipsum
    * `1_screening_roses_flow.R`: Lorem ipsum
    * `2_screening_draw_roses.R`: Lorem ipsum
    * `3_contrasts_tables.R`: Lorem ipsum
    * `4_figures.R`: Lorem ipsum
* `5_reporting_supplemental`: Lorem ipsum
    * `report_geographies.R`: Lorem ipsum
    
### Source files

The folder `src` contains custom functions and files that are used to define variables. Files include:

* `functions.R`: Lorem ipsum
* `bmp_vocabulary.csv`: Lorem ipsum
* `citation_problems.csv`: Lorem ipsum
* `continent_reference.csv`: Lorem ipsum
* `extraction_sheets.csv`: Lorem ipsum
* `geography_by_hand.csv`: Lorem ipsum
* `inclusion_thresholds.csv`: Lorem ipsum
* `phase_units.csv`: Lorem ipsum
* `pool_labels.csv`: Lorem ipsum
* `practice_labels.csv`: Lorem ipsum
* `response_expression_preference.csv`: Lorem ipsum
* `screen_reasons.csv`: Lorem ipsum
* `species_classes_by_hand.csv`: Lorem ipsum
* `test_statistic_degrees_of_freedom.csv`: Lorem ipsum

### Data files

Lorem ipsum

* `data`: Lorem ipsum
    * `flagged_effects.csv`: Lorem ipsum
* `data/raw`: Lorem ipsum
    * `bmp_meta.duckdb`: Lorem ipsum
* `data/raw/for_species_classification`: Lorem ipsum
    * `birdlife_international_all_species.csv`: Lorem ipsum
    * `iucn_bli_classification.rds`: Lorem ipsum
* `data/raw/for_species_classification/species_classified_by_source`: Lorem ipsum
    * `species_classification_aab.csv`: Lorem ipsum
    * `species_classification_birdbase.csv`: Lorem ipsum
    * `species_classification_eubirds.csv`: Lorem ipsum
    * `species_classification_pif.csv`: Lorem ipsum
    * `species_classification_vgbi.csv`: Lorem ipsum
    * `species_classification_vickery_1999.csv`: Lorem ipsum
* `data/db_mirror`: Lorem ipsum
    * `converted_effects.csv`: Lorem ipsum
    * `effect_sizes.csv`: Lorem ipsum
* `data/processed`: Lorem ipsum
    * `paper_metadata.csv`: Lorem ipsum
    * `species_classified_analysis_frame.csv`: Lorem ipsum
* `data/processed/cleaned_data`: Lorem ipsum
    * `beta_categorical.csv`: Lorem ipsum
    * `mean_diff.csv`: Lorem ipsum
    * `other_categorical.csv`: Lorem ipsum
* `data/processed/for_analysis`: Lorem ipsum
    * `beta_categorical.csv`: Lorem ipsum
    * `mean_diff.csv`: Lorem ipsum
    * `other_categorical.csv`: Lorem ipsum

### Output files

Lorem ipsum

* `output`: Lorem ipsum
    * `sensitivity_specifications.csv`: Lorem ipsum
* `output/audits`: Lorem ipsum
    * `excluded_effects.csv`: Lorem ipsum
* `output/models`: Lorem ipsum
    * `posterior_cell_draws.rds`: Lorem ipsum
* `output/figures`: Lorem ipsum
    * `figure_2_species_richness.png`: Lorem ipsum
    * `figure_3_abundance_pooled.png`: Lorem ipsum
    * `figure_4_abundance_by_guild.png`: Lorem ipsum
    * `figure_S1_species_richness_intervals.png`: Lorem ipsum
    * `figure_S2_abundance_by_guild_intervals.png`: Lorem ipsum
    * `figure_S3_nest_success_by_guild.png`: Lorem ipsum
    * `figure_S4_nest_success_by_guild_intervals.png`: Lorem ipsum
    * `figure_S5_abundance_guild_contrasts.png`: Lorem ipsum
    * `figure_S6_heterogeneity.png`: Lorem ipsum
    * `figure_S7_species_abundance.png`: Lorem ipsum
* `output/roses_diagram`: Lorem ipsum
    * `README.md`: Lorem ipsum
    * `roses_diagram.png`: Lorem ipsum
    * `roses_diagram.svg`: Lorem ipsum
    * `roses_diagram_manual.svg`: Lorem ipsum
    * `roses_flow_reconciliation.csv`: Lorem ipsum
    * `roses_flow_stages.csv`: Lorem ipsum
* `output/tables`: Lorem ipsum
    * `reanalysis_tables.docx`: Lorem ipsum
    * `sensitivity_conclusion_changes.csv`: Lorem ipsum
    * `sensitivity_digest.csv`: Lorem ipsum
    * `sensitivity_estimates.csv`: Lorem ipsum
    * `sensitivity_flagged_effects.csv`: Lorem ipsum
    * `sensitivity_influence.csv`: Lorem ipsum
    * `sensitivity_outliers_per_cell.csv`: Lorem ipsum
    * `sensitivity_outliers_per_effect.csv`: Lorem ipsum
    * `sensitivity_publication_bias.csv`: Lorem ipsum
    * `sensitivity_summary.csv`: Lorem ipsum
    * `table_guild_bmp.csv`: Lorem ipsum
    * `table_guild_contrasts_by_bmp.csv`: Lorem ipsum
    * `table_heterogeneity.csv`: Lorem ipsum
    * `table_pooled_bmp.csv`: Lorem ipsum
    * `table_species_richness_by_bmp.csv`: Lorem ipsum
    * `table_species_abundance.csv`: Lorem ipsum
