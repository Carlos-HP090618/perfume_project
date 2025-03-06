#Load libraries
library(tidyverse)
library(recipes)
library(parsnip)
library(rsample)

#Transform type into columns
types <- strsplit(perfume_df$Type, ", ")
types_unlisted <- unlist(types)
types_unique <- unique(types_unlisted)

for (type in types_unique){
  perfume_df[[type]] <- sapply(perfume_df$Type, function(x) as.integer(grepl(type, x)))
}

# repeat the procedure for Notes
notes <- strsplit(perfume_df$Notes, ", ")
notes_unlisted <- unlist(notes)
unique_notes <- unique(notes_unlisted)

for (note in unique_notes){
  perfume_df[[note]] <- sapply(perfume_df$Notes, function(x) as.integer(grepl(note, x)))
}

# clean the df and remove some columns
perfume_df <- perfume_df %>% remove_rownames() %>% column_to_rownames(var = "Name")
perfume_df <- perfume_df %>% select(-Type, -Notes, -Group)

#bulding the model
tree_model <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

# create the recipe and feature engineering 
perfume_recipe <- recipe(Purchased ~., data = perfume_df) %>%
  step_normalize(Longevity, Sillage)

# Recipe training
perfume_recipe_prep <- perfume_recipe %>% prep(training(perfume_df)) 

