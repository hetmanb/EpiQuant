library(googlesheets)
library(dplyr)

isolate_source_names <- c("animal_farm_duck",
                  "animal_farm_chicken",
                  "environmental_recreation_water",
                  "human_urban_foodworker",
                  "human_rural_farmer")

isolate_epi_cols <- c("Source","Animal","Avian","Ruminant","Porcine","OtherAnimal",
                      "Food.Farm.Level","Food.Processing.Level","Food.Retail.Level","NonFood.Wild","NonFood.Domestic.Companion","Human",
                      "Urban","Rural","Food.Consumption","Recreational.Water","Recreational.AnimalContact","Recreational.Travel",
                      "Occupational.FarmWork","Occupational.FoodHandling","Environmental","Water","Soil.Sand","Farm.Impact",
                      "Human.Impact","Non.Impacted") 
  

isolate_source_df <- setNames(data.frame(matrix(ncol = 26, nrow = length(isolate_source_names))), isolate_epi_cols)
isolate_source_df$Source <- isolate_source_names


new_ss <- gs_new(title = "isolate_source_scores", input = isolate_source_df)
new_ss %>% gs_read()  
