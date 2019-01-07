## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
library(purrr)
library(rlang)
library(readr)
library(nycflights13)

## ------------------------------------------------------------------------
library(dplyr)

con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
RSQLite::initExtension(con)

db_flights <- copy_to(con, nycflights13::flights, "flights")

## ------------------------------------------------------------------------
library(modeldb)

km <- db_flights %>%
  simple_kmeans_db(dep_time, distance)

## ------------------------------------------------------------------------
km$centers

## ------------------------------------------------------------------------
head(km$tbl, 10)

## ------------------------------------------------------------------------
dbplyr::remote_query(km$tbl)

## ---- include = FALSE----------------------------------------------------
DBI::dbDisconnect(con)

