## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(dbplyr)
library(purrr)
library(rlang)
library(readr)
library(nycflights13)
library(DBI)
library(modeldb)
library(dbplot)

## ------------------------------------------------------------------------
# Open a database connection
con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
RSQLite::initExtension(con)

library(dplyr)
# Copy data to the database
db_flights <- copy_to(con, nycflights13::flights, "flights")
# Create a simple sample
db_sample <- db_flights %>%
  filter(!is.na(arr_time)) %>%
  head(20000) 

## ------------------------------------------------------------------------
db_sample %>%
  select(arr_delay, dep_delay, distance) %>%
  linear_regression_db(arr_delay)

## ------------------------------------------------------------------------
db_sample %>%
  select(arr_delay, origin) %>%
  add_dummy_variables(origin, values = c("EWR", "JFK", "LGA"))

## ------------------------------------------------------------------------
origins <- db_flights %>%
  group_by(origin) %>%
  summarise() %>%
  pull()

origins

## ------------------------------------------------------------------------
db_sample %>%
  select(arr_delay, origin) %>%
  add_dummy_variables(origin, values = origins) %>%
  linear_regression_db(arr_delay)

## ------------------------------------------------------------------------
db_sample %>%
  select(arr_delay, arr_time, dep_delay, dep_time) %>%
  linear_regression_db(arr_delay, sample_size = 20000)

## ------------------------------------------------------------------------
db_sample %>%
  mutate(distanceXarr_time = distance * arr_time) %>%
  select(arr_delay, distanceXarr_time) %>% 
  linear_regression_db(arr_delay, sample_size = 20000)

## ------------------------------------------------------------------------
db_sample %>%
  mutate(distanceXarr_time = distance * arr_time) %>%
  select(arr_delay, distance, arr_time, distanceXarr_time) %>% 
  linear_regression_db(arr_delay, sample_size = 20000)

## ------------------------------------------------------------------------
db_flights %>%
  mutate(distanceXarr_time = distance * arr_time) %>%
  select(month, arr_delay, distance, arr_time, distanceXarr_time) %>% 
  group_by(month) %>%
  linear_regression_db(arr_delay, auto_count = TRUE)

## ------------------------------------------------------------------------
remote_model <- db_sample %>%
  mutate(distanceXarr_time = distance * arr_time) %>%
  select(arr_delay, dep_time, distanceXarr_time, origin) %>% 
  add_dummy_variables(origin, values = origins) %>%
  linear_regression_db(y_var = arr_delay, sample_size = 20000)

remote_model

## ---- include = FALSE----------------------------------------------------
DBI::dbDisconnect(con)

