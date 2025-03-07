#Load libraries
#most I never used, I had an idea but it didn't work
library(tidyverse)
library(recipes)
library(parsnip)
library(rsample)
library(rpart)
library(rpart.plot)
library(broom)
library(ggmosaic)

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

tree_model <- rpart(Purchased ~., data = perfume_df_alternative, method = "class")
rpart.plot(tree_model)
summary(tree_model)

#create a DF with results
#     setting type 2 class orks, but when I create a DF combining the real results with prediction
#      it changes the results to 1 and 2, instead of 1 and 0 like  in the original DF.
#     to overcome this I got the probability of results and used a ifelse function

real_value <- perfume_df_alternative$Purchased
tree_pred <- predict(tree_model, perfume_df_alternative, type = "prob")
tree_pred_correction <- ifelse(tree_pred[, 2] >0.5, 1, 0)

# changing my columns to factors so I can use conf_mat from yardstick package
df_results <- as.data.frame(cbind(real_value, tree_pred_correction))
df_results$real_value <- as.factor(df_results$real_value)
df_results$tree_pred_correction <- as.factor(df_results$tree_pred_correction)
# RESULTS
conf_mat_rpart <- conf_mat(df_results, truth = real_value, estimate = tree_pred_correction)
conf_mat_rpart
accuracy(df_results, truth = real_value, estimate = tree_pred_correction)
sensitivity(df_results, truth = real_value, estimate = tree_pred_correction)
specificity(df_results, truth = real_value, estimate = tree_pred_correction)

#Visualizing
#there's a lot of reeated code here, I know
predictions_ROC <- predict(tree_model, perfume_df_alternative, type = "prob")[, "TRUE"]
roc_obj_rpart <- roc(real_value, predictions_prob_rpart)
auc_rpart <- auc(roc_obj_rpart)
plot(roc_obj_rpart, main = paste("ROC Curve (rpart), AUC =", round(auc_rpart, 3))) #the ugliest ROC plot ever

autoplot(conf_mat_rpart, type = "mosaic") + ggtitle("Confusion Matrix Mosaic Plot (rpart)")