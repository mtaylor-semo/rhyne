# library(tidyverse)
# library(lubridate)
# 
# # Nest data is from the modified field data file.
# # MST split the species and common names for birds and plants
# # to separate columns.
# 
# nest_raw <- read_csv(
#   file = "data/nest_data.csv",
#   skip = 1,
#   col_names = c(
#     "nest_id",
#     "date_recorded",
#     "common_bird",
#     "sci_bird",
#     "id_notes",
#     "common_plant",
#     "sci_plant",
#     "over_water",
#     "nest_height_in",
#     "nest_height_cm",
#     "eggs_hatchlings",
#     "notes",
#     "latitude",
#     "longitude"
#   )
# ) |> 
#   mutate(common_plant = str_replace_all(common_plant, pattern = "Wool Grass", replacement = "Woolgrass"))
# 

nest_raw |> 
  ggplot() +
  geom_point(aes(x = longitude, y = latitude))


nest_tbl <- nest_raw |> 
  group_by(common_bird, common_plant) |> 
  summarize(mean_nest_height = mean(nest_height_cm, na.rm = TRUE),
            num_nests = n(),
            .groups = "keep") |> 
  arrange(desc(num_nests), .by_group = TRUE) |> 
  print(n = 30)

nest_tbl




nest_raw |> 
  group_by(common_plant) |> 
  summarize(mean_nest_height = mean(nest_height_cm, na.rm = TRUE),
            num_nests = n()) |> 
  arrange(desc(num_nests)) |> 
  filter(common_plant != "-") |> 
  print(n = 30)



