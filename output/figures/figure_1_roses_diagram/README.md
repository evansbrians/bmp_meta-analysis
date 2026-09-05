# Review flow: papers and effect sizes at each stage

| file | does |
|---|---|
| `roses_flow.py` | counts the stages, writes the two CSVs |
| `roses_flow.R` | the same counts, tidyverse idiom |
| `draw_roses.R` | draws `roses_diagram.svg` and `.png` with **ggplot2** |
| `draw_roses.py` | hand-laid fallback, writes `roses_diagram_manual.svg` |

Run from the repository root, not from this directory:

```r
source("brian_sandbox/roses_diagram/roses_flow.R")
source("brian_sandbox/roses_diagram/draw_roses.R")
```

`draw_roses.R` needs `svglite`; `ggsave()` reads the device off the
file extension and writes the SVG through it.

The png is what `bmp_results.qmd` embeds, under *Sampling overview*.

## Inputs

`roses_flow.R` reads:

- `data/raw/bmp_meta.duckdb` — `study_bmp` is the screening record, keyed on
  study_key x bmp, so it is already one row per paper x practice; `study`
  supplies `in_metadata`, which names the studies the extraction holds and
  the screening record never had.
- `output/audits/screened_effects.csv` — every converted effect size, with
  `excluded_by` and `in_primary_pool`. One frame covers three stages:
  extracted, retained, and the primary pool.
- `output/audits/analysis_pool_summary.csv` — the modelling pools.

**The flow therefore reflects the last database build.** Edit the metadata
and you must re-run `3_build_database.R` before the counts move.

`roses_flow.py` reads `data/processed/paper_metadata.csv` instead of the
database — the file the database is built from — and de-duplicates it to
distinct paper x practice pairs. The two should agree, which is what makes
the pair worth keeping.

`data/excluded_effects.csv` is deliberately not a stage: it is the
data-quality register, and it only bites under the `flagged_effects_removed`
sensitivity specification.

## Two counts, not one

Down to extraction the unit is a **practice record** — a paper x practice
row. A paper studying three practices contributes three, and it leaves the
flow only when its last record does, so the paper count falls more slowly
than the record count. After extraction the unit is an **effect size**.

## The reconciliation file

`roses_flow_reconciliation.csv` records where the screening record and the
extraction disagree, which a flow figure has to state rather than smooth
over. The largest entry is papers marked eligible that never reached the
analysis table.

The database does not carry `in_analysis_table`, so the flow can no longer
split those papers into a backlog and the ones extracted only to the
archived continuous sheets. It reports the total instead. Adding
`in_analysis_table` to `study_bmp` would restore the split.

## The layout

`draw_roses.R` places the boxes itself and draws them with `geom_rect()`,
`geom_text()` and `geom_segment()`. **One y unit is one line of text**, so a
box's height is the lines it holds plus its padding, and the rows stack by a
cumulative sum over the tallest box in each row. That keeps the arithmetic
readable and lets every box be the size of its own contents.

It was written against ggflowchart first. Two things sent it back to
ggplot2: the package draws every node at one fixed size, which suits a
flowchart of short labels and not a column of eight-line exclusion lists;
and the `layout = "custom"` argument that would have allowed placement is
newer than the released version.

For the text to sit inside its box, a y unit has to be the same length on
the page as it is in the data. That needs three things together: both scales
expand by nothing, `plot.margin` is nothing, and the heading and note are
drawn as annotations in the data rather than by `labs()`, which would take
their height out of the panel. Get any of them wrong and the boxes are drawn
at one scale while the type is set at another, which shows up as the last
line of the tallest boxes spilling out of the bottom.

The knobs are `box_padding` and `title_gap` (lines of space inside a box),
`row_gap`, `band_edges` / `spine_edges` / `excluded_edges` (the three
columns' x extents), and `body_size`, which sets the type and through
`line_inches` the height of every row.

`ggsave()` writes through svglite, so every label stays live text rather
than paths, and the nodes are separable in Inkscape.
