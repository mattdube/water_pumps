library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# load training data
waterpoints_train <- read_csv(here("00_Data/raw", "train_data.csv"))
waterpoints_train_labels <- read_csv(here("00_Data/raw", "train_labels.csv"))

# join training data and labels
water_train_full <- 
    waterpoints_train %>%
    left_join(waterpoints_train_labels, by = c(id = "id"))

