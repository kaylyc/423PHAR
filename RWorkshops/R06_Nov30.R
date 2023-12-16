library(tidyverse)
library(janitor)

gusto <- read_csv("data/gusto.csv") %>% 
  clean_names()

# fit a logistic regression
lgs <- glm(day30~killip,family='binomial',data=gusto)
predicted_probs <- predict(lgs,type='response')

# Draw ROC by hands

# Step 1: Given a threshold, compute the predicted outcome
threshold <- 0.50
predicted_outcome <- as.numeric(predicted_probs>threshold)
actual_predicted_table <- table(gusto$day30,predicted_outcome)

# Step 2: write a function that takes the 2x2 table as input
# and return a vector of accuracy, specificity and sensitivity
compute_metrics <- function(tmp_table){
  
}

compute_metrics(actual_predicted_table)
