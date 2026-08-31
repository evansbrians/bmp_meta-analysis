# This script:
# - Reads the stage table 1_screening_roses_flow.R writes
# - Draws it with ggplot2: a spine of what was retained down one column,
#   what left beside it, and the stage names down the left
# - Drops every step that lost nothing, so the published figure carries only
#   the stages that moved a record or a paper
# - Writes roses_diagram.svg for editing in Inkscape, and a png

# setup --------------------------------------------------------------------

library(fs)
library(glue)
library(tidyverse)

flow_directory <- path("output/roses_diagram")

flow_stages <-
  read_csv(
    path(flow_directory, "roses_flow_stages", ext = "csv"),
    show_col_types = FALSE
  )

flow_reconciliation <-
  read_csv(
    path(flow_directory, "roses_flow_reconciliation", ext = "csv"),
    show_col_types = FALSE
  )

# labels -------------------------------------------------------------------

# Every box is a bold heading over a body, so the two are carried apart and
# drawn apart.

# A body is one row per line. `count` is the records-and-papers pair, drawn in
# its own right-aligned column and empty on a line that carries none; `text`
# is what sits beside it; `indent` says whether the line belongs in the text
# column or runs the full width of the box. Two columns rather than one padded
# string, because a space is narrower than a digit in this font, so padding
# never lines the reasons up and a wrapped line lands wherever it lands.

thousands <-
  function(.count) {
    format(
      .count,
      big.mark = ",",
      trim = TRUE
    )
  }

body_line <-
  function(
    .text,
    .count = "",
    .indent = FALSE) {
    tibble(
      count = .count,
      text = .text,
      indent = .indent
    )
  }

# The reason column is narrower than the box, so a reason wraps sooner than
# the heading above it.

reason_width <- 42

# One row per wrapped line, with the count against the first of them.

reason_lines <-
  function(
    .reason,
    .count) {
    tibble(
      id = seq_along(.reason),
      count = .count,
      text =
        str_wrap(
          .reason,
          width = reason_width
        )
    ) %>%
      separate_longer_delim(text, delim = "\n") %>%
      mutate(
        count =
          if_else(row_number() == 1, count, ""),
        indent = TRUE,
        .by = id
      ) %>%
      select(count, text, indent)
  }

survivors <-
  flow_stages %>%
  summarise(
    records = last(records),
    papers = last(papers),
    .by = phase
  )

retained_body <-
  function(.phase, .unit) {
    counts <-
      survivors %>%
      filter(phase == .phase)
    body_line(
      glue(
        "{thousands(counts$records)} {.unit}",
        "   |   {thousands(counts$papers)} papers"
      )
    )
  }

# A step is drawn only if it moved something. Everything below reads its
# lines through this, so an empty step never reaches the page.

lost_stages <-
  function(.phase) {
    flow_stages %>%
      filter(
        phase == .phase,
        records_lost > 0 |
          papers_lost > 0
      )
  }

# An exclusion box leads with what the phase lost in total, then one line
# per reason with its own records and papers against it.

excluded_body <-
  function(.phase, .unit) {
    lost <-
      lost_stages(.phase)
    if (nrow(lost) == 0) {
      return(body_line(character()))
    }
    bind_rows(
      body_line(
        glue_data(
          lost,
          "{thousands(sum(records_lost))} {.unit}",
          "   |   {thousands(sum(papers_lost))} papers"
        ) %>%
          first()
      ),
      body_line(""),
      reason_lines(
        .reason = lost$stage,
        .count =
          str_c(
            lost$records_lost,
            " | ",
            lost$papers_lost
          )
      )
    )
  }

# The one box whose counts come from the reconciliation rather than a stage,
# so its three lines are filtered here rather than by `lost_stages()`.

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
        .count =
          str_c(
            not_extracted_lines$records,
            " | ",
            not_extracted_lines$papers
          )
      )
    )
  }

# The analysis and cutoff phases name what they kept, so their exclusions
# read off `reason`, as a paragraph rather than a counted list.

kept_phase_body <-
  function(.phase) {
    kept <-
      lost_stages(.phase)
    if (nrow(kept) == 0) {
      return(body_line(character()))
    }
    bind_rows(
      body_line(
        glue_data(
          kept,
          "{records_lost} effect sizes   |   {papers_lost} papers"
        )
      ),
      body_line(""),
      body_line(
        kept$reason %>%
          str_wrap(width = reason_width + 4) %>%
          str_split("\n") %>%
          list_c()
      )
    )
  }

# The pools the models were fitted to, named as the results name them:
# response first, and the guild model before the pooled one. The guild-only
# abundance model carries no reported cell means, so it is not listed, and
# `_bmp` is dropped because every pool here is by practice.

models_body <-
  flow_stages %>%
  filter(
    phase == "models",
    stage != "abundance_guild",
    records > 0 |
      papers > 0
  ) %>%
  mutate(
    pool =
      stage %>%
      str_remove("_bmp$") %>%
      fct_relevel(
        "richness",
        "abundance_guild",
        "abundance_pooled",
        "nest_success_guild",
        "nest_success_pooled"
      ),
    label =
      pool %>%
      as.character() %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  ) %>%
  arrange(pool) %>%
  mutate(
    entry =
      glue(
        "{label}: {thousands(records)} effect sizes, {papers} studies"
      )
  ) %>%
  pull(entry) %>%
  body_line()


# nodes and edges ----------------------------------------------------------

# `row` places a stage down the page; `group` puts it in the spine of
# retained counts or in the column of what left at that stage.

flow_nodes <-
  bind_rows(
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
          retained_body("identification", "practice records"),
          retained_body("screening", "practice records"),
          retained_body("eligibility", "practice records"),
          retained_body("extraction", "effect sizes"),
          retained_body("screen", "effect sizes"),
          retained_body("analysis", "effect sizes"),
          retained_body("cutoff", "effect sizes"),
          models_body
        )
    ),
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
          excluded_body("screening", "practice records"),
          excluded_body("eligibility", "practice records"),
          not_extracted_body,
          excluded_body("screen", "effect sizes"),
          kept_phase_body("analysis"),
          kept_phase_body("cutoff")
        )
    )
  )

# A box whose body came back empty lost nothing, so it is dropped and the
# rows close up behind it. Only exclusion boxes can empty; a spine box always
# carries what it retained.

flow_nodes <-
  flow_nodes %>%
  mutate(
    body_lines =
      map_int(
        body,
        \(.lines) {
          nrow(.lines)
        }
      )
  ) %>%
  filter(body_lines > 0) %>%
  mutate(
    row = dense_rank(row)
  )

# place the boxes ----------------------------------------------------------

# One y unit is one line of text, so a box's height is its title, the gap
# below it, its body and its padding. Rows stack by a cumulative sum.

box_padding <- 0.45

title_gap <- 0.5

row_gap <- 1.3

# The x axis is inches on the page, so a column is exactly as wide as the
# text it has to hold, whatever the font size.

chart_width <- 11.5

band_edges <- c(0.09, 1.83)

spine_edges <- c(1.97, 6.62)

excluded_edges <- c(6.92, 11.41)

box_heights <-
  flow_nodes %>%
  mutate(
    height =
      1 + title_gap + body_lines + 2 * box_padding,
    xmin =
      if_else(group == "Retained", spine_edges[1], excluded_edges[1]),
    xmax =
      if_else(group == "Retained", spine_edges[2], excluded_edges[2])
  )

row_positions <-
  box_heights %>%
  summarise(
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

# The body is drawn a line at a time, because a counted line puts its records
# and papers in a right-aligned column of its own and its reason in a second
# column beside it. One y unit is one line, so a line sits at its index below
# the top of the body.

count_column <- 0.62

count_gap <- 0.16

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

# The band boxes are as tall as the row they name, so they read as a column.

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

# geom_rect has square corners, so the outline is built as four arcs and
# drawn as a polygon. The radii differ because a y unit is not an x unit.

corner_x <- 0.071

corner_y <- 0.34

box_outline <-
  function(name, xmin, xmax, ymin, ymax, ...) {
    arc <- seq(0, pi / 2, length.out = 8)
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

box_shapes <-
  box_frames %>%
  pmap(
    \(name, xmin, xmax, ymin, ymax, ...) {
      box_outline(name, xmin, xmax, ymin, ymax)
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

# The spine arrow drops from one retained box to the next; the exclusion
# arrow runs across from the spine at the shallower of the two boxes.

spine_arrows <-
  placed_nodes %>%
  filter(group == "Retained") %>%
  arrange(row) %>%
  mutate(
    yend = lead(ymax)
  ) %>%
  filter(!is.na(yend))

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

# A device sets a line of text at 1.2 times the font size, so that product
# is one y unit and the body is drawn at a line height of 1 to match.

# That holds only if a y unit is the same length on the page as in the data:
# scales and margins expand by nothing, and labs() would steal panel height.

# Sizes are points, as the theme takes them; a geom's `size` is millimetres,
# so the layers that do not read the theme divide by `.pt`.

body_points <- 12.6

line_inches <- body_points * 1.2 / 72

heading_space <- 2.4

# The footnote that used to sit here is the figure caption on the results
# page, so the bottom of the chart is a margin and nothing else.

bottom_margin <- 0.6

x_limits <- c(0, chart_width)

y_limits <-
  c(
    min(row_positions$ymin) - bottom_margin,
    heading_space
  )

# Every layer carries its own data -- boxes, node text, band labels and the
# two arrow families are separate frames -- so the plot is initialized empty
# and each geometry is mapped where it is added.

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
      filter(count != ""),
    aes(
      x = x_text + count_column,
      y = y_line,
      label = count
    ),
    hjust = 1,
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
      grid::arrow(
        length = grid::unit(0.3, "cm"),
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
      grid::arrow(
        length = grid::unit(0.24, "cm"),
        type = "closed"
      )
  ) +

  # Add the title and the footnote:

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

chart_height <-
  line_inches * (y_limits[2] - y_limits[1])

c("svg", "png") %>%
  walk(
    \(.extension) {
      ggsave(
        path(flow_directory, "roses_diagram", ext = .extension),
        roses_chart,
        width = chart_width,
        height = chart_height,
        dpi = 300
      )
    }
  )

# clean the environment ----------------------------------------------------

# Everything this script produces is written above; the next script
# reads it back from disk, so nothing is handed on in memory.

rm(list = ls())
