library(here)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(ggmap)
library(purrr)
library(broom)
library(corrplot)
library(RColorBrewer)
library(Hmisc)
library(vcd)


# load data using read_csv from readr library
train_values <- read.csv(here("00_Data/", "temp/train.set.csv"))
train_labels <- read.csv(here("00_Data/", "temp/train.label.csv"))
test_values <- read.csv(here("00_Data/", "temp/test.set.csv"))

# merge train values & labels
train <- merge(train_values, train_labels)

# look at small sample of full train set
train %>%
    head(20) %>%
    View()

# save sample as new dataframe
train_sample <- train %>%
    head(200)

# target variable is status_group - examine count and proportions for each group
table(train$status_group)
prop.table(table(train$status_group))

# compare quantity with functioning pumps
table(train$quantity, train$status_group)

# As row-wise proportions, quantity vs status_group
prop.table(table(train$quantity, train$status_group), margin = 1)

# structure of data
str(train)

glimpse(train)
tidy(table(train$waterpoint_type))
tidy(table(train$waterpoint_type_group))

tidy(table(train$extraction_type))
tidy(table(train$extraction_type_group))
tidy(table(train$extraction_type_class))

tidy(chisq.test(train$extraction_type, train$extraction_type_class))
?assocstats

tab <- xtabs(~extraction_type_class + region, data=train)
summary(assocstats(tab))


train %>%
    select(basin, subvillage, region, region_code, district_code, lga) %>%
    head(500) %>% View()

# count missing values by column
train %>%
    select(everything()) %>%
    summarise_all(funs(sum(is.na(.))))

map(train, ~sum(is.na(.)))

# small function to count empty values by column
# small function to count empty values by column
count_empty <- function(col_name) {
    return(sum(col_name == "", na.rm=TRUE))
}

train %>%
    select(everything()) %>%
    summarise_all(funs(count_empty(.))) %>%
    tidy() %>%
    View()

train %>%
    filter(funder == "") %>%
    View()


######################################
#
#  Data Visualization
#
######################################

# bar plot of quantity - using qplot & ggplot
qplot(quantity, data = train, geom = "bar", fill = status_group) +
    theme(legend.position = "top")

train %>%
    ggplot(aes(quantity)) +
    geom_bar(aes(fill = status_group)) +
    theme(legend.position = "top") %>%

# bar plot for quality_group
qplot(quality_group, data = train, geom = "bar", fill = status_group) +
    theme(legend.position = "top")

train %>%
    ggplot(aes(quality_group)) +
    geom_bar(aes(fill = status_group)) +
    theme(legend.position = "top")

# bar plot for waterpoint_type
qplot(waterpoint_type, data = train, geom = "bar", fill = status_group) +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = -20, hjust = 0))

train %>%
    ggplot(aes(waterpoint_type)) +
    geom_bar(aes(fill = status_group)) +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = -25, hjust = 0))

# bar plot for payment
train %>%
    ggplot(aes(payment)) +
    geom_bar(aes(fill = status_group)) +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = -25, hjust = 0))

# histogram: construction_year by status_group
train %>%
    ggplot(aes(construction_year)) +
    geom_histogram(bins = 20) +
    facet_grid(~ status_group)

# Now subsetting when construction_year is larger than 0
ggplot(subset(train, construction_year > 0), aes(x = construction_year)) +
    geom_histogram(bins = 20) + 
    facet_grid( ~ status_group)

