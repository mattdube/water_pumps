library(dplyr)
library(ggplot2)
#library(lubridate)
#library(mltools)

# there are several redundant features that can be removed
train_processed <- 
  train %>% select(-c(extraction_type, extraction_type_group, payment_type, quantity_group,
                      source, waterpoint_type, region_code))

# drop not-needed features
train_processed <- 
  train_processed %>%
  select(-c(funder, installer, recorded_by, subvillage, wpt_name, num_private, 
            scheme_name, ward, amount_tsh, id, date_recorded))

# transform latitude and longitude into x, y, z coordinates, drop latitude/longitude
train_processed <- 
  train_processed %>%
  mutate(x = cos(latitude) * cos(longitude), 
         y = cos(latitude) * sin(longitude),
         z = sin(latitude)) %>%
  select(-c(latitude, longitude))

# put construction year into bins by decade
train_processed <- train_processed %>%
  mutate(construction_year = case_when
          (between(construction_year, 1960, 1969) ~ "1960's",
           between(construction_year, 1970, 1979) ~ "1970's",
           between(construction_year, 1980, 1989) ~ "1980's",
           between(construction_year, 1990, 1999) ~ "1990's",
           between(construction_year, 2000, 2009) ~ "2000's",
           between(construction_year, 2010, 2019) ~ "2010's",
           construction_year == 0 ~ "missing")) 



# public meeting, scheme_management, permit having 3000-4000 missing values
# add missing value indicator, and input "missing" for value
train_processed <- 
  train_processed %>%
  mutate(public_meeting_missing = ifelse(is.na(public_meeting), 1, 0),
         scheme_management_missing = ifelse(is.na(scheme_management), 1, 0),
         permit_missing = ifelse(is.na(permit), 1, 0))


any_column_NA <- function(x){
  any(is.na(x))
}

replace_NA_missing <- function(x){
  if_else(is.na(x), "missing", x)
}

train_processed <- 
  train_processed %>%
  mutate_if(any_column_NA, replace_NA_missing)

# small function to count empty values by column
# small function to count empty values by column
count_empty <- function(col_name) {
  return(sum(col_name == "", na.rm=TRUE))
}

train_processed <- 
    train_processed %>%
        mutate(district_code = as.factor(district_code)) %>%
    mutate_if(is.character, as.factor)



train_clean <- train_processed %>% select(-lga)
train_clean <- train_clean %>% select(-status_group, everything())

