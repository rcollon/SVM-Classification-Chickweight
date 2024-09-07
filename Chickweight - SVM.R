
#Simple classificaiton using ChickenWeight dataset. The data 

.libPaths()
# install missing packages 
#install.packages ("tidyverse")
#install.packages("LiblineaR")

library (tidymodels)
library (tidyverse)
library (datasets)


# Dataframe is labelled inconsistently so make it all lowercase 
chickWeight <- ChickWeight %>% rename_all(tolower)

# 578 rows
glimpse (ChickWeight)


#Data exploration 

# Bar chart of counts of chickens
chickWeight %>% group_by(diet) %>%  count() %>% ggplot (aes(x=diet, y=n)) + geom_bar(stat="identity") + ggtitle ("Counts by diet type")

# Box plot of weight by diet
chickWeight %>% ggplot (aes(x=diet,y=weight)) + geom_boxplot() + ggtitle("Distribution of weight by diet")

# Scatter plot of weight over time for different diets
chickWeight %>% ggplot (aes(x=time,y=weight, color=diet)) + geom_point() + ggtitle("Weight over time by diet")

# Average weight over time
chickWeight %>% group_by (time,diet) %>% summarize (mean_weight=mean(weight)) %>% ggplot (aes (x=time, y=mean_weight, color=diet)) + geom_line() + ggtitle("Mean weight by diet over time")


# SVM Classifier

# Use a hyperplane to classify objects that are linearly seperable. If the objects are not linearly separable then 
# Predict type of diet by chicken weight

# Set seed for reproducible samples
set.seed(28)
# Initial split with 80% in training
# nesting by chick so that individual chicks are not seperated. Creating a strata of 
groupedChickWeight <- chickWeight %>% nest_by (chick, diet)
dataSplit <- groupedChickWeight %>% initial_split(prop=.8, strata=diet)
train <- training(dataSplit) %>% unnest(data)
test <- testing(dataSplit) %>% unnest(data)

# Remove target variable
testDiet <- test %>% select (diet)
test <- test %>% select (-diet)

# Recipe 
svmRecipe <- recipe(diet ~ ., data = train) %>%  
  step_dummy(chick, one_hot = TRUE)
  #step_mutate(chick = as.character(chick))
  #step_normalize(all_predictors())

# Model
svmModel <- svm_linear () %>%
  set_mode("classification") %>%
  set_engine("LiblineaR")

# Workflow
svmWorkflow <- workflow() %>%
  add_recipe(svmRecipe) %>%
  add_model(svmModel) 

# Train model 
svmFit <- fit(svmWorkflow, data = train)

# Make predictions on the test set
svmPredict <- predict(svmFit, test) %>%
  bind_cols(testDiet)

# Evaluate the model
svmMetrics <- svmPredict %>%
  metrics(truth = diet, estimate = .pred_class)

svmMetrics

svmMetricsTime <- svmPredict %>% bind_cols(test) %>% group_by (time) %>% metrics(truth = diet, estimate = .pred_class)

# Visualise accuracy. Accuracy improves over time 
svmMetricsTime %>% filter(.metric=="accuracy") %>% ggplot (aes(x=time,  y=.estimate)) + geom_bar(stat="identity") + ggtitle ("Accuracy over time")
svmMetricsTime
