setwd("C:/Users/#/Downloads") 
#Working Directory will need to be wherever Data Sets have been loaded to

library(readxl)
library(readr)
library(tidyverse)
library(caret)

Bachelor_Historical <- read_excel("C:/Users/#/Downloads/Bachelor Data Set_final.xlsx")
Bachelor_Current <- read_excel("C:/Users/#/Downloads/Bachelor Data Set_Current.xlsx")

Bachelor_Historical_x <- 
  Bachelor_Historical %>%
  select(FirstImpression, AgeDiff, One_on_One_Roses, DateRoses, Group_Date_Roses, DateRoses, Callout_Deviation, Avg_Callout_Position)

Bachelor_Historical_y <- 
  Bachelor_Historical %>%
  select(Place)

Bachelor_Historical <- Bachelor_Historical %>%
  select(Place, FirstImpression, AgeDiff, One_on_One_Roses, DateRoses, Group_Date_Roses, DateRoses, Callout_Deviation, Avg_Callout_Position)

myControl <- trainControl(
  method = "cv",
  number = 100,
  summaryFunction = defaultSummary,
  verboseIter = TRUE
)

#Build Random Forest Model
model_rf <- train(
  x = Bachelor_Historical_x,
  y = as.numeric(Bachelor_Historical_y$Place),
  metric = "AUC",
  method = "ranger",
  trControl = myControl
)

#glmnet_model
model_glm <- train(
  Place ~ .,
  data = Bachelor_Historical,
  overfit,
  method = "glmnet",
  trControl = myControl
)

#gbm_model
model_gbm <- train(
  Place ~ .,
  data = Bachelor_Historical,
  method = "gbm",
  trControl = myControl
)

model_list <- list(item1 = model_glmnet, item2 = model_rf, item3 = model_gbm)

resamples <- resamples(model_list)

summary(resamples)

Bachelor_Current_contestant <- 
  Bachelor_Current %>%
  select(Full_Name, Age, City, State, Occupation)

Bachelor_Current_predict <-
  Bachelor_Current %>%
  select(FirstImpression, AgeDiff, One_on_One_Roses, DateRoses, Group_Date_Roses, DateRoses, Callout_Deviation, Avg_Callout_Position)

Bachelor_Current_expected_finish_rf <- predict(model_rf, Bachelor_Current_predict)

Bachelor_Current_expected_finish_gbm <- predict(model_gbm, Bachelor_Current_predict)

Bachelor_Current_Prediction <- cbind(Bachelor_Current_contestant, Bachelor_Current_expected_finish_rf, Bachelor_Current_expected_finish_gbm)

view(Bachelor_Current_Prediction)


