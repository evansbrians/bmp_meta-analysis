
# Plot for the VWL annual meeting:

bmp_response_classified %>% 
  filter(response_class == "abundance") %>% 
  summarize(
    n = 
      key %>% 
      unique() %>% 
      length(),
    .by = bmp
  ) %>% 
  mutate(
    bmp = 
      bmp %>% 
      str_replace_all("_", " ") %>% 
      str_to_title() %>% 
      str_replace("Nwsg", "NWSG") %>% 
      str_replace("And", "and") %>% 
      str_replace("In ", "in ") %>% 
      str_replace("To", "to") %>% 
      str_replace("Native", "native") %>% 
      fct_reorder(n)
  ) %>% 
  ggplot() +
  aes(
    x = bmp,
    y = n,
  ) +
  geom_bar(
    stat = "identity",
    color = "#3d5013",
    fill = "#8cac4c",
    size = .7
  ) +
  coord_flip() +
  scale_y_continuous(
    limits =  c(0, 25),
    expand = c(0, 0)
  ) +
  labs(
    title = "Determining the influence of Best Management Practices on bird abundance",
    x = "Best Management Practices",
    y = "Number of papers"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "Times"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 4),
    plot.title = 
      element_text(
        size = 20, 
        hjust = 0,
        margin = margin(0, 0, 10, 19),
        face = "bold"
      ),
    plot.margin = 
      unit(
        c(10, 10, 12, 18), 
        "pt"
      ),
    axis.text = 
      element_text(
        size = 12, 
        color = "black"
      ),
    plot.title.position = "plot",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#cccccc"),
    panel.grid.minor.x = element_line(color = "#cccccc", linetype = "dashed"),
    panel.background = element_rect(fill = "#eefeff")
  )

ggsave(
  "reports/outputs/abundance_paper_count.png",
  width = 10.75,
  height = 6.837,
  units = "in",
  dpi = 300
)


