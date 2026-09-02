# This script:
# - Reads the stage table 1_screening_roses_flow.R writes
# - Draws it: what was retained down one column, what left beside it
# - Writes roses_diagram.svg, for editing in Inkscape, and a png

# setup --------------------------------------------------------------------

library(fs)
library(glue)
library(tidyverse)

# Project functions:

source("src/functions.R")

# Diagram directory:

flow_directory <- path("output/roses_diagram")

# Counting unit per phase:

phase_units <-
  read_csv(
    "src/phase_units.csv",
    show_col_types = FALSE
  )

# Stage counts, with their unit:

flow_stages <-
  read_csv(
    path(
      flow_directory,
      "roses_flow_stages",
      ext = "csv"
    ),
    show_col_types = FALSE
  ) %>%
  left_join(
    phase_units,
    by = join_by(phase)
  )

# Reconciliation counts:

flow_reconciliation <-
  read_csv(
    path(
      flow_directory,
      "roses_flow_reconciliation",
      ext = "csv"
    ),
    show_col_types = FALSE
  )

# labels -------------------------------------------------------------------

# Survivors per phase:

survivors <-
  flow_stages %>%
  summarize(
    records = last(records),
    papers = last(papers),
    unit = last(unit),
    .by = phase
  )

# Lines for the not-extracted box:

not_extracted_lines <-
  tibble(
    reason =
      c(
        "eligible, never extracted",
        "extracted despite a recorded problem",
        "extracted with no screening record"
      ),
    records =
      c(
        flow_reconciliation$not_in_analysis_records,
        flow_reconciliation$extracted_despite_problem_records,
        flow_reconciliation$extracted_outside_metadata_records
      ),
    papers =
      c(
        flow_reconciliation$not_in_analysis_table,
        flow_reconciliation$extracted_despite_problem,
        flow_reconciliation$extracted_outside_metadata
      )
  ) %>%
  filter(
    records > 0 |
      papers > 0
  )

# The box, dropped when empty:

not_extracted_body <-
  if (nrow(not_extracted_lines) == 0) {
    body_line(character())
  } else {
    bind_rows(
      body_line(
        glue_data(
          flow_reconciliation,
          "{not_in_analysis_records} practice records   |   ",
          "{not_in_analysis_table} papers"
        )
      ),
      body_line(""),
      reason_lines(
        .reason = not_extracted_lines$reason,
        .records = not_extracted_lines$records,
        .papers = not_extracted_lines$papers
      )
    )
  }

# Pool names, in reporting order:

pool_labels <-
  read_csv(
    "src/pool_labels.csv",
    show_col_types = FALSE
  )

# Each pool's counts, in that order:

pool_counts <-
  pool_labels %>%
  left_join(
    flow_stages,
    by = join_by(stage)
  ) %>%

  # Keep the pools that were fitted:

  drop_na(records)

# The total, then one line per pool:

models_body <-
  bind_rows(
    retained_body(survivors, "cutoff"),
    body_line(""),
    reason_lines(
      .reason = pool_counts$pool_label,
      .records = pool_counts$records,
      .papers = pool_counts$papers
    )
  )

# nodes and edges ----------------------------------------------------------

# Box rows and columns:

flow_nodes <-
  bind_rows(

    # Retained boxes:

    tibble(
      name =
        c(
          "identified",
          "screened",
          "eligible",
          "extracted",
          "retained",
          "primary",
          "cutoff",
          "models"
        ),
      row = 1:8,
      group = "Retained",
      band =
        c(
          "Identification",
          "Screening",
          "Eligibility",
          "Extraction",
          "Effect-size screen",
          "Primary pool",
          "Paper cutoff",
          "Models"
        ),
      title =
        c(
          "Records in the screening record",
          "Passing article screening",
          "Meeting the inclusion criteria",
          "Effect sizes extracted",
          "Effect sizes retained",
          "Primary analysis pool",
          "Cells of three or more papers",
          "Modeling pools"
        ),
      body =
        list(
          retained_body(survivors, "identification"),
          retained_body(survivors, "screening"),
          retained_body(survivors, "eligibility"),
          retained_body(survivors, "extraction"),
          retained_body(survivors, "screen"),
          retained_body(survivors, "analysis"),
          retained_body(survivors, "cutoff"),
          models_body
        )
    ),

    # Excluded boxes:

    tibble(
      name =
        c(
          "screening_out",
          "eligibility_out",
          "not_extracted",
          "screen_out",
          "vegetation_out",
          "cutoff_out"
        ),
      row = 2:7,
      group = "Excluded",
      band = NA_character_,
      title =
        c(
          "Excluded at screening",
          "Excluded at eligibility",
          "Eligible but not in the analysis table",
          "Held out by the effect-size screen",
          "Held out of the primary pool",
          "Held out by the three-paper cutoff"
        ),
      body =
        list(
          excluded_body(flow_stages, "screening"),
          excluded_body(flow_stages, "eligibility"),
          not_extracted_body,
          excluded_body(flow_stages, "screen"),
          kept_phase_body(flow_stages, "analysis"),
          kept_phase_body(flow_stages, "cutoff")
        )
    )
  )

# Drop the boxes that lost nothing:

flow_nodes <-
  flow_nodes %>%

  # Body length:

  mutate(
    body_lines =
      map_int(
        body,
        \(.lines) {
          nrow(.lines)
        }
      )
  ) %>%

  # Drop them and close the rows:

  filter(body_lines > 0) %>%
  mutate(
    row = dense_rank(row)
  )

# place the boxes ----------------------------------------------------------

# Box heights, in lines of text:

box_padding <- 0.45
title_gap <- 0.5
row_gap <- 1.3

# Column edges, in inches:

chart_width <- 11.5
band_edges <- c(0.09, 1.83)
spine_edges <- c(1.97, 6.62)
excluded_edges <- c(6.92, 11.41)

# Height and column of each box:

box_heights <-
  flow_nodes %>%
  mutate(
    height =
      1 + title_gap + body_lines + 2 * box_padding,
    xmin =
      if_else(
        group == "Retained",
        spine_edges[1],
        excluded_edges[1]
      ),
    xmax =
      if_else(
        group == "Retained",
        spine_edges[2],
        excluded_edges[2]
      )
  )

# Row positions:

row_positions <-
  box_heights %>%
  summarize(
    row_height = max(height),
    .by = row
  ) %>%
  arrange(row) %>%
  mutate(
    ymax =
      -lag(
        cumsum(row_height + row_gap),
        default = 0
      ),
    ymin = ymax - row_height
  )

# Place the boxes and their text:

placed_nodes <-
  box_heights %>%
  left_join(
    row_positions %>%
      select(
        row,
        ymax
      ),
    by = join_by(row)
  ) %>%
  mutate(
    ymin = ymax - height,
    x_text = xmin + 0.14,
    y_title = ymax - box_padding - 0.5,
    y_body = ymax - box_padding - 1 - title_gap
  )

# Body text columns:

count_column <- 0.62
count_gap <- 0.16
divider_column <- 0.37
divider_gap <- 0.05

# One row per line of body text:

body_text <-
  placed_nodes %>%
  select(
    name,
    x_text,
    y_body,
    body
  ) %>%
  unnest(body) %>%
  mutate(
    y_line = y_body - (row_number() - 1),
    x_line =
      if_else(
        indent,
        x_text + count_column + count_gap,
        x_text
      ),
    .by = name
  )

# Band boxes, one per row:

band_boxes <-
  placed_nodes %>%
  filter(group == "Retained") %>%
  select(
    name,
    row,
    band
  ) %>%
  left_join(
    row_positions %>%
      select(
        row,
        ymax,
        ymin
      ),
    by = join_by(row)
  ) %>%
  mutate(
    name = str_c(name, "_band"),
    xmin = band_edges[1],
    xmax = band_edges[2],
    x_text = mean(band_edges),
    y_text = (ymax + ymin) / 2
  )

# rounded boxes ------------------------------------------------------------

# Every box to outline:

box_frames <-
  bind_rows(
    placed_nodes %>%
      select(
        name,
        xmin,
        xmax,
        ymin,
        ymax,
        group
      ),
    band_boxes %>%
      select(
        name,
        xmin,
        xmax,
        ymin,
        ymax
      ) %>%
      mutate(group = "Band")
  )

# Corner radii:

corner_x <- 0.071
corner_y <- 0.34

# Corner arc:

arc <-
  seq(
    0,
    pi / 2,
    length.out = 8
  )

# Outline each box:

box_shapes <-
  box_frames %>%
  pmap(
    \(name, xmin, xmax, ymin, ymax, ...) {
      tibble(
        name = name,
        x =
          c(
            xmax - corner_x + corner_x * cos(arc),
            xmin + corner_x - corner_x * sin(arc),
            xmin + corner_x - corner_x * cos(arc),
            xmax - corner_x + corner_x * sin(arc)
          ),
        y =
          c(
            ymax - corner_y + corner_y * sin(arc),
            ymax - corner_y + corner_y * cos(arc),
            ymin + corner_y - corner_y * sin(arc),
            ymin + corner_y - corner_y * cos(arc)
          )
      )
    }
  ) %>%
  list_rbind() %>%
  left_join(
    box_frames %>%
      select(
        name,
        group
      ),
    by = join_by(name)
  )

# arrows -------------------------------------------------------------------

# Spine arrows, box to box:

spine_arrows <-
  placed_nodes %>%
  filter(group == "Retained") %>%
  arrange(row) %>%
  mutate(
    yend = lead(ymax)
  ) %>%
  filter(
    !is.na(yend)
  )

# Exclusion arrows, spine to box:

exclusion_arrows <-
  placed_nodes %>%
  filter(group == "Excluded") %>%
  left_join(
    placed_nodes %>%
      filter(group == "Retained") %>%
      select(
        row,
        spine_height = height
      ),
    by = join_by(row)
  ) %>%
  mutate(
    y_arrow = ymax - pmin(height, spine_height) / 2
  )

# draw ---------------------------------------------------------------------

# Text and page sizing:

body_points <- 12.6
line_inches <- body_points * 1.2 / 72
heading_space <- 2.4
bottom_margin <- 0.6

# X limits:

x_limits <- c(0, chart_width)

# Y limits:

y_limits <-
  c(
    min(row_positions$ymin) - bottom_margin,
    heading_space
  )

# Every layer brings its own data:

roses_chart <-

  # Initialize the plot:

  ggplot() +

  # Add geometries:

  geom_polygon(
    data = box_shapes,
    aes(
      x = x,
      y = y,
      group = name,
      fill = group,
      color = group
    ),
    linewidth = 0.3
  ) +
  geom_text(
    data = placed_nodes,
    aes(
      x = x_text,
      y = y_title,
      label = title
    ),
    hjust = 0,
    vjust = 0.5,
    fontface = "bold",
    size = 13.7 / .pt,
    color = "#12222f"
  ) +
  geom_text(
    data = body_text,
    aes(
      x = x_line,
      y = y_line,
      label = text
    ),
    hjust = 0,
    vjust = 1,
    color = "#33454f"
  ) +
  geom_text(
    data =
      body_text %>%
      filter(count_records != ""),
    aes(
      x = x_text + divider_column - divider_gap,
      y = y_line,
      label = count_records
    ),
    hjust = 1,
    vjust = 1,
    color = "#33454f"
  ) +
  geom_text(
    data =
      body_text %>%
      filter(count_records != ""),
    aes(
      x = x_text + divider_column,
      y = y_line,
      label = count_papers
    ),
    hjust = 0,
    vjust = 1,
    color = "#33454f"
  ) +
  geom_text(
    data = band_boxes,
    aes(
      x = x_text,
      y = y_text,
      label = band
    ),
    hjust = 0.5,
    size = 12.8 / .pt,
    fontface = "bold",
    color = "#3d5160"
  ) +
  geom_segment(
    data = spine_arrows,
    aes(
      x = mean(spine_edges),
      xend = mean(spine_edges),
      y = ymin,
      yend = yend
    ),
    color = "#2c3f4d",
    linewidth = 0.35,
    arrow =
      arrow(
        length = unit(0.3, "cm"),
        type = "closed"
      )
  ) +
  geom_segment(
    data = exclusion_arrows,
    aes(
      x = spine_edges[2],
      xend = xmin,
      y = y_arrow,
      yend = y_arrow
    ),
    color = "#7b8d99",
    linewidth = 0.3,
    linetype = "dashed",
    arrow =
      arrow(
        length = unit(0.24, "cm"),
        type = "closed"
      )
  ) +

  # Add the title:

  annotate(
    "text",
    x = band_edges[1],
    y = heading_space - 0.9,
    label =
      str_c(
        "Grassland bird BMP meta-analysis: Records, papers and effect ",
        "sizes at each stage"
      ),
    hjust = 0,
    vjust = 0.5,
    size = 16.6 / .pt,
    fontface = "bold",
    color = "#12222f"
  ) +

  # Define scale elements:

  scale_fill_manual(
    values =
      c(
        Retained = "#dbe6f0",
        Excluded = "#fbf3ee",
        Band = "#f6f8fa"
      )
  ) +
  scale_color_manual(
    values =
      c(
        Retained = "#2c3f4d",
        Excluded = "#2c3f4d",
        Band = "#c3ced6"
      )
  ) +
  scale_x_continuous(
    limits = x_limits,
    expand = expansion(0, 0)
  ) +
  scale_y_continuous(
    limits = y_limits,
    expand = expansion(0, 0)
  ) +

  # Modify the theme:

  theme_void() +
  theme(
    geom =
      element_geom(fontsize = body_points),
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0)
  )

# write --------------------------------------------------------------------

# Page height:

chart_height <-
  line_inches * (y_limits[2] - y_limits[1])

# Write the svg and png:

c("svg", "png") %>%
  walk(
    \(.extension) {
      ggsave(
        path(
          flow_directory,
          "roses_diagram",
          ext = .extension
        ),
        roses_chart,
        width = chart_width,
        height = chart_height,
        dpi = 300
      )
    }
  )

# clear the environment ----------------------------------------------------

rm(
  list = ls()
)
