library(readxl)
library(tidyverse)

# read in and pre-process citations ---------------------------------------

citations_all <-
  read_csv(
    "data/bmp_zotero_exports/z-grasslands_meta_analysis_full_zotero.csv"
  ) %>% 
  janitor::clean_names()

citations_short <-
  citations_all %>% 
  
  # Generate short author and date naming for the paper:
  
  mutate(
    n_authors = str_count(author, ";") + 1,
    author1 = 
      author %>% 
      str_extract("[A-Za-záíöüóé'\\- ]*"),
    author2 =
      author %>% 
      str_extract("; [A-Za-záíöüóé'\\- ]*") %>% 
      str_remove("; "),
    paper = 
      case_when(
        n_authors == 1 ~ author1,
        n_authors == 2 ~ 
          str_c(
            author1,
            author2,
            sep = " and "
          ),
        n_authors > 2 ~ str_c(author1, " et al.")
      ) %>% 
      str_c(publication_year, sep = " "),
    title = 
      str_sub(
        title,
        start = 1, 
        end = 40) %>% 
      str_c("...") %>% 
      str_to_title()
  ) %>% 
  select(
    key,
    paper,
    title
  )

# citations by bmp --------------------------------------------------------

citations_by_bmp <- 
  list.files(
    "data/bmp_zotero_exports/") %>% 
  keep(
    ~ !str_detect(.x, "z-")
  ) %>% 
  set_names(
    str_remove(., ".csv$")
  ) %>% 
  map(
    \(x) {
      file.path("data/bmp_zotero_exports", x) %>% 
        read_csv() %>% 
        janitor::clean_names() %>% 
        select(key, notes) %>% 
        mutate(
          notes = 
            notes %>% 
            str_remove("<div data-schema-version=\"9\">") %>% 
            str_remove("<p>Application notes</p> ") %>% 
            str_remove("</div>")
        )
    }
  )

# flatten bmps ------------------------------------------------------------

citations_by_bmp_flattened <-
  citations_by_bmp %>% 
  map(
    ~ .x %>% 
      mutate(
        bmp = str_extract_all(notes, "bmp: [A-Za-z_]*</p>"),
        article_type = str_extract(notes, "article_type: [A-Za-z_]*</p>"),
        geography = str_extract_all(notes, "geography: [A-Za-z_]*</p>"),
        response = str_extract_all(notes, "response: [A-Za-z_]*</p>"),
        effect_size = str_extract_all(notes, "effect_size: [A-Za-z_]*</p>"),
        useful = str_extract_all(notes, "useful: [A-Za-z_]*</p>"),
        additional_notes = str_extract(notes, "[Aa]dditional .*$")
      ) %>% 
      select(key, bmp:additional_notes) %>%
      rowwise() %>% 
      mutate(
        across(
          c(bmp:useful),
          ~ .x %>% 
            unlist() %>% 
            str_c(collapse = "; ") %>% 
            str_remove_all("[a-zA-z]*: ") %>% 
            str_remove_all("</p>")
        )
      ) %>% 
      ungroup() %>% 
      mutate(
        additional_notes = 
          additional_notes %>% 
          str_remove_all("[Aa]dditional (comments|notes):") %>% 
          str_remove_all("<p>|</p>|</span>") %>% 
          str_trim() %>% 
          str_to_sentence(),
        across(
          everything(),
          ~ if_else(
            nchar(.x) == 0, 
            NA,
            .x
          )
        )
      )
  )

# combine to get paper info and write to file -----------------------------

citations_by_bmp_flattened %>% 
  map(
    ~ citations_short %>% 
      inner_join(
        .x,
        by = "key"
      )
  ) %>%
  writexl::write_xlsx("data/processed/citations_by_bmp_worksheets.xlsx")


citations_by_bmp_flattened %>% 
  map(
    ~ citations_short %>% 
      inner_join(
        .x,
        by = "key"
      )
  ) %>% 
  bind_rows() %>% 
  distinct() %>% 
  writexl::write_xlsx("data/processed/citations_by_bmp_simple_combine.xlsx")

citations_by_bmp_single_frame <- 
  citations_by_bmp_flattened %>% 
  names() %>% 
  map(
    ~ citations_short %>% 
      inner_join(
        citations_by_bmp_flattened %>% 
          pluck(.x) %>% 
          mutate(
            multiple_bmps = bmp,
            bmp = .x),
        by = "key"
      ) %>% 
      select(
        key:bmp, 
        multiple_bmps,
        everything()
      )
  ) %>% 
  bind_rows()

# perhaps the best version? -----------------------------------------------

citations_by_bmp_single_frame %>% 
  bind_rows(
    citations_short %>% 
      anti_join(
        citations_by_bmp_single_frame,
        by = "key"
      ) %>% 
      mutate(bmp = "z-unfiled")
  ) %>% 
  arrange(paper, title, bmp) %>% 
  writexl::write_xlsx("data/processed/citations_by_bmp_long.xlsx")
  
