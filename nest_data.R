# Run 1_nest_data.R before running this script.

# library(tidyverse)
# library(lubridate)
library(RColorBrewer)

# Create a plot showing distribution of nest heights

# first, create a summary of mean nest height
nest_sum <- nest_raw |> 
  group_by(banding_code) |> 
  summarize(n = mean(nest_height_cm, na.rm = TRUE)) |> 
  replace_na(list(n = 0)) |> 
  filter(banding_code != "INBU")



height_distribution_plot <- nest_raw |> 
  filter(banding_code != "INBU") |> 
  group_by(banding_code) |> 
  ggplot() +
  geom_point(
    aes(x = banding_code,
        y = nest_height_cm,
        color = banding_code,
        fill = banding_code,
        shape = banding_code)
  ) +
  geom_point(
    data = nest_sum,
    aes(x = banding_code,
        y = n,
        color = banding_code,
        fill = banding_code,
        shape = banding_code),
    show.legend = FALSE,
    size = 3,
    alpha = 0.5
  ) +
  scale_color_brewer(palette = "Dark2", name = NULL) +
  scale_fill_brewer(palette = "Dark2", name = NULL) +
  scale_shape_manual(values = c(21:25), name = NULL) +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Nest height (cm)"
  )

ggsave(
  "height_distribution_plot.png",
  height_distribution_plot,
  width = 1600,
  height = 900,
  units = "px",
  dpi = 300,
  bg = "white"
)


# Tables ------------------------------------------------------------------

# Make table of Species, Plant, Mean Nest Height, Number of Nests
nest_tbl <- nest_raw |> 
  group_by(common_bird, sci_plant) |> 
  summarize(num_nests = n(),
            mean_nest_height = mean(nest_height_cm, na.rm = TRUE),
            .groups = "keep") |> 
  select(common_bird, num_nests, mean_nest_height, sci_plant) |> 
  ungroup() |> 
  group_by(common_bird) |> 
  arrange(desc(num_nests), .by_group = TRUE) |> 
  write_csv("results/nest_tbl.csv")
  


# Make table of Plant, Mean Nest Height, Number of nests
plant_tbl <- nest_raw |> 
  group_by(common_plant) |> 
  summarize(mean_nest_height = mean(nest_height_cm, na.rm = TRUE),
            num_nests = n()) |> 
  arrange(desc(num_nests)) |> 
  filter(common_plant != "-") |> 
  print(n = 30) |> 
  write_csv("results/plant_heights.csv")




library(vegan)

# Try removing unknown nests

nest_mds_data <- 
  nest_raw |> 
  filter(banding_code != "UNK") |>  # Try removing unkown
  filter(!is.na(nest_height_cm))



nest_mds <- metaMDS(nest_mds_data[c(10,16,17)], k = 2)

nest_vars <- as_tibble(scores(nest_mds)$species)
nest_vars$vars <- row.names(scores(nest_mds)$species)

nest_species <- as_tibble(scores(nest_mds)$sites)

temp_var <- nest_raw |> 
  filter(!is.na(nest_height_cm)) |> 
  filter(banding_code != "UNK")

nest_species$banding_code <- temp_var$banding_code

nest_vars |> 
  ggplot() +
  geom_segment(
    aes(
      x = 0,
      y = 0,
      xend = NMDS1,
      yend = NMDS2
    ),
    color = "gray80"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  geom_text(
    aes(x = NMDS1,
        y = NMDS2,
        label = vars),
    label = c("Nest height", "Over water", "Plant spp"),
    hjust = "outward",
    vjust = "outward"
  ) +
  geom_point(
    data = nest_species,
    aes(
      x = NMDS1,
      y = NMDS2,
      color = banding_code,
      fill = banding_code,
      shape = banding_code
    )
  ) +
  scale_x_continuous(
    limits = c(-0.75, 1.4),
    breaks = c(-0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
  ) +
  scale_y_continuous(
    limits = c(-0.5, 0.75),
    breaks = c(-0.5, -0.25, 0, 0.25, 0.5)
  ) +
  scale_color_brewer(palette = "Dark2", name = NULL) +
  scale_fill_brewer(palette = "Dark2", name = NULL) +
  scale_shape_manual(values = c(21:25), name = NULL) +
  labs(
    x = "NMDS1",
    y = "NMDS2"
  ) 

ggsave(
  "nmds_plot.png",
  width = 1600,
  height = 900,
  units = "px",
  dpi = 300,
  bg = "white"
)


