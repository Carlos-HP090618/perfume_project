install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# normalize Longevity and Sillage
perfume_df_alternative <- perfume_df %>% 
  mutate(Longevity = (Longevity - mean(Longevity))/sd(Longevity), Sillage = (Sillage - mean(Sillage))/sd(Sillage))

# tree model

model <- rpart(Purchased ~., data = perfume_df_alternative, method = "class")
rpart.plot(model)
real <- perfume_df_alternative$Purchased
tree_pred <- (predict(model, perfume_df_alternative, type = "prob"))
tree_pred_correction <- ifelse(tree_pred[, 2] >0.5, 1, 0)
result_df <- as.data.frame(cbind(real, tree_pred_correction))

# logistic regression

lg_model <- glm(Purchased ~., data = perfume_df_alternative, family = "binomial")
probs <- predict(lg_model, perfume_df_alternative, type = "response")
prediction <- ifelse(probs >0.5, 1, 0)
prediction


summary_df <- cbind(prediction, real)
