#Load libraries
library(tidyverse)
library(recipes)
library(parsnip)
library(rsample)
library(rpart)
library(rpart.plot)


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
 

# normalize Longevity and Sillage
perfume_df_alternative <- perfume_df %>% 
  mutate(Longevity = (Longevity - mean(Longevity))/sd(Longevity), Sillage = (Sillage - mean(Sillage))/sd(Sillage))

# tree model

model <- rpart(Purchased ~., data = perfume_df_alternative, method = "class")
rpart.plot(model)
real <- perfume_df_alternative$Purchased
tree_pred <- (predict(model, perfume_df_alternative, type = "prob"))
tree_pred_correction <- ifelse(tree_pred[, 2] >0.5, 1, 0)