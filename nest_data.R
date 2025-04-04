library(tidyverse)
library(lubridate)

# Nest data is from the modified field data file.
# MST split the species and common names for birds and plants
# to separate columns.

nest_raw <- read_csv(
  file = "data/nest_data.csv",
  skip = 1,
  col_names = c(
    "nest_id",
    "date_recorded",
    "common_bird",
    "sci_bird",
    "id_notes",
    "common_plant",
    "sci_plant",
    "over_water",
    "nest_height_in",
    "nest_height_cm",
    "eggs_hatchlings",
    "notes",
    "latitude",
    "longitude"
  )
)


nest_raw |> 
  ggplot() +
  geom_point(aes(x = longitude, y = latitude))
