
unfiled_items <- 
  read_csv("data/bmp_zotero_exports/zotero_unfiled.csv") %>% 
  janitor::clean_names() %>% 
  select(
    key,
    author, 
    year = publication_year,
    abstract = abstract_note
  ) %>% 
  mutate(
    n_authors = str_count(author, ";") + 1,
    author_1 =
      author %>% 
      str_extract("^[^,]+"),
    author_2 = 
      author %>% 
      str_extract("; [^,]+") %>% 
      str_remove("; "),
    paper = 
      case_when(
        n_authors == 1 ~ author_1,
        n_authors == 2 ~ str_c(author_1, author_2, sep = " and "),
        n_authors > 2 ~ str_c(author_1, " et. al")
      ) %>% 
      str_c(year, sep = " ")
  ) %>% 
  select(key, paper, abstract) %>% 
  arrange(paper)

unfiled_items[[2, 3]]
