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
) |> 
  mutate(
    common_plant = str_replace_all(common_plant, pattern = "Wool Grass", replacement = "Woolgrass"),
    common_plant = str_replace_all(common_plant, pattern = "Not specified", replacement = "-"),
    common_plant = replace_na(common_plant, "-"),
    sci_plant = replace_na(sci_plant, "-"),
    )

# Add banding codes

nest_raw <- nest_raw |> 
  mutate(banding_code = case_when(
    common_bird == "Red-winged Blackbird" ~ "RWBL",
    common_bird == "Blue Grosbeak" ~ "BLGR",
    common_bird == "Indigo Bunting" ~ "INBU",
    common_bird == "Mourning Dove" ~ "MODO",
    common_bird == "Unknown" ~ "UNK"
  ))

nest_raw <- nest_raw |> 
  mutate(ow_code =
           if_else(over_water == "Yes", 1, 0))

# All species of plant. Works in alphabetical order
nest_raw <- nest_raw |>
  mutate(plant_code = case_when(
    common_plant == "-" ~ 0,
    common_plant == "American Sweetgum" ~ 1,
    common_plant == "Blackberry" ~ 2,
    common_plant == "Box Elder" ~ 3,
    common_plant == "Buttonbush" ~ 4,
    common_plant == "Cattails" ~ 5,
    common_plant == "Common Pear" ~ 6,
    common_plant == "Eastern Redcedar" ~ 7,
    common_plant == "Elm" ~ 8,
    common_plant == "Goldenrod" ~ 9,
    common_plant == "Green Hawthorn" ~ 10,
    common_plant == "Hackberry" ~ 11,
    common_plant == "Hole in the ground" ~ 12,
    common_plant == "Juniper" ~ 13,
    common_plant == "Overcup Oak" ~ 14,
    common_plant == "Prairie Cordgrass" ~ 15,
    common_plant == "Sugar Hackberry" ~ 16,
    common_plant == "Willow" ~ 17,
    common_plant == "Woolgrass" ~ 18
  )
  )

# Group plants by category
# stress plot is stair step
# 1 Monocots = Typha and Woolgrass, Prairie Cordgrass
# 2 Conifer = "Redcedar and Juniper"
# 3 Forb/shrub = Goldenrod, blackberry, buttonbush
# 4 Hardwood = all others

nest_raw <- nest_raw |> 
  mutate(plant_group = case_when(
        common_plant == "-" ~ 0,
        common_plant == "American Sweetgum" ~ 4,
        common_plant == "Blackberry" ~ 3,
        common_plant == "Box Elder" ~ 4,
        common_plant == "Buttonbush" ~ 3,
        common_plant == "Cattails" ~ 1,
        common_plant == "Common Pear" ~ 4,
        common_plant == "Eastern Redcedar" ~ 2,
        common_plant == "Elm" ~ 4,
        common_plant == "Goldenrod" ~ 3,
        common_plant == "Green Hawthorn" ~ 4,
        common_plant == "Hackberry" ~ 4,
        common_plant == "Hole in the ground" ~ 0,
        common_plant == "Juniper" ~ 2,
        common_plant == "Overcup Oak" ~ 4,
        common_plant == "Prairie Cordgrass" ~ 1,
        common_plant == "Sugar Hackberry" ~ 4,
        common_plant == "Willow" ~ 4,
        common_plant == "Woolgrass" ~ 1
  ))




# These are all that end up in analysis. 
# Code all others as 0
# [1] "Elm"               "American Sweetgum" "Willow"           
# [4] "Juniper"           "Hackberry"         "Blackberry"       
# [7] "Cattails"          "Prairie Cordgrass" "Goldenrod"        
# [10] "Woolgrass"         "Buttonbush"        "Common Pear"      
# [13] "Overcup Oak" 

# nest_raw <- nest_raw |>
#   mutate(plant_code = case_when(
#     common_plant == "-" ~ 0,
#     common_plant == "Box Elder" ~ 0,
#     common_plant == "Eastern Redcedar" ~ 0,
#     common_plant == "Green Hawthorn" ~ 0,
#     common_plant == "Hole in the ground" ~ 0,
#     common_plant == "Sugar Hackberry" ~ 0,
#     common_plant == "Prairie Cordgrass" ~ 1,
#     common_plant == "Woolgrass" ~ 2,
#     common_plant == "Goldenrod" ~ 3,
#     common_plant == "Blackberry" ~ 4,
#     common_plant == "Buttonbush" ~ 5,
#     common_plant == "Cattails" ~ 6,
#     common_plant == "American Sweetgum" ~ 7,
#     common_plant == "Common Pear" ~ 8,
#     common_plant == "Elm" ~ 9,
#     common_plant == "Hackberry" ~ 10,
#     common_plant == "Juniper" ~ 11,
#     common_plant == "Overcup Oak" ~ 12,
#     common_plant == "Willow" ~ 13
#     )
#   )
# 

# Tried grouping plants but now insufficient data to separate
# stress plot is stair step
# 1 Monocots = Typha and Woolgrass, Prairie Cordgrass
# 2 Conifer = "Redcedar and Juniper"
# 3 Forb/shrub = Goldenrod, blackberry, buttonbush
# 4 Hardwood = all others

# nest_raw <- nest_raw |> 
#   mutate(plant_code = case_when(
#     common_plant == "-" ~ 0,
#     common_plant == "American Sweetgum" ~ 4,
#     common_plant == "Blackberry" ~ 3,
#     common_plant == "Box Elder" ~ 4,
#     common_plant == "Buttonbush" ~ 3,
#     common_plant == "Cattails" ~ 1,
#     common_plant == "Common Pear" ~ 4,
#     common_plant == "Eastern Redcedar" ~ 2,
#     common_plant == "Elm" ~ 4,
#     common_plant == "Goldenrod" ~ 3,
#     common_plant == "Green Hawthorn" ~ 4,
#     common_plant == "Hackberry" ~ 4,
#     common_plant == "Hole in the ground" ~ 0,
#     common_plant == "Juniper" ~ 2,
#     common_plant == "Overcup Oak" ~ 4,
#     common_plant == "Prairie Cordgrass" ~ 1,
#     common_plant == "Sugar Hackberry" ~ 4,
#     common_plant == "Willow" ~ 4,
#     common_plant == "Woolgrass" ~ 1
#   )
#   )
