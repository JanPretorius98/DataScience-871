title: "Data-Science-871-JHPretorius-Transform"
author: "JH Pretorius"
date: "2023-05-24"

Goal: "Transform and Prepare data for exploratory analysis and modelling"

# Housekeeping
rm(list = ls()) #Clear environment
pacman::p_load(dplyr, ggplot2, tidyverse)

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
