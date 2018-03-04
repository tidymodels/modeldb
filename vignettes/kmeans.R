## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(purrr)
library(rlang)
library(nycflights13)

## ------------------------------------------------------------------------
library(tidyverse)

con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
RSQLite::initExtension(con)

db_flights <- copy_to(con, nycflights13::flights, "flights")

## ------------------------------------------------------------------------
library(modeldb)

km <- db_flights %>%
  simple_kmeans(dep_time, distance)

## ------------------------------------------------------------------------
km$centers

## ------------------------------------------------------------------------
head(km$tbl, 10)

## ------------------------------------------------------------------------
dbplyr::remote_query(km$tbl)

## ------------------------------------------------------------------------
km <- db_flights %>%
  simple_kmeans(dep_time, distance, max_repeats = 10)

## ------------------------------------------------------------------------
km <- db_flights %>%
  simple_kmeans(dep_time, distance, initial_kmeans = read_csv("kmeans.csv"))

## ------------------------------------------------------------------------
km$tbl <- collect(km$tbl) # ONLY USE THIS STEP IF WORKING WITH SQLITE

## ---- fig.width=10, fig.height=10----------------------------------------
km$tbl %>%
  plot_kmeans(dep_time, distance)

## ---- fig.width=10, fig.height=10----------------------------------------
km$tbl %>%
  plot_kmeans(dep_time, distance, resolution = 30)

