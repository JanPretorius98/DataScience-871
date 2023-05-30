title: "Data-Science-871-JHPretorius-Transform"
author: "JH Pretorius"
date: "2023-05-24"

Goal: "Transform and Prepare data for exploratory analysis and modelling"

# Housekeeping
rm(list = ls()) #Clear environment
pacman::p_load(dplyr, ggplot2, tidyverse, rsample, caret, glmnet, vip, pdp)

# Load data
path <- "/Users/janhendrikpretorius/Library/CloudStorage/OneDrive-StellenboschUniversity/Masters-2023/Modules/Data Science/DataScience-871-repo/JHPretorius-Project/Candidate Data Sets/Dating/"
file <- "lovoo_v3_users_api-results.csv"

df <- read_csv(paste0(path, file))

# Define full and null model
full.model <- lm(counts_kisses ~ genderLooking + age + counts_details + counts_pictures + counts_profileVisits + counts_fans + counts_g +
                 flirtInterests_chat + flirtInterests_friends + flirtInterests_date + isFlirtstar + isHighlighted + isInfluencer + isMobile +
                   lang_count + lang_fr + lang_en + lang_de + verified + shareProfileEnabled, data = df)
null.model <- lm(counts_kisses ~ 1, data = df)

# Perform stepwise regression
stepwise.model <- step(null.model, scope = list(lower = null.model, upper = full.model), direction = "both")

# Display the final model
summary(stepwise.model)

# Compile machine learning model:

set.seed(123)
# split df into training and testing sets
working_df <- df %>% 
  select(c(counts_kisses, counts_profileVisits, counts_g, lang_de, age, counts_pictures, lang_fr, counts_details))

split  <- initial_split(working_df, prop = 0.7, strata = "counts_kisses")
df_train  <- training(split)
df_test   <- testing(split)

# MLR model
set.seed(123)  
(cv_model <- train(form = counts_kisses ~ ., 
                   data = df_train, 
                   method = "lm",
                   trControl = trainControl(method = "cv", number = 10)
))

# Create training  feature matrices
X <- model.matrix(counts_kisses ~ ., df_train)[, -1]
Y <- df_train$counts_kisses

# Apply CV ridge regression
ridge <- cv.glmnet(
  x = X,
  y = Y,
  alpha = 0)

# Apply CV lasso regression
lasso <- cv.glmnet(
  x = X,
  y = Y,
  alpha = 1)

# You can use summary() to get a summary of your models and compare them:
summary(cv_model)
summary(ridge)
summary(lasso)

