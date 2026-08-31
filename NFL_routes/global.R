library(shiny)
library(bslib)
library(tidyverse)

source("helpers.R")

files <- list.files(path = './data', pattern = '.rds')

# read existing RDS files and create table of players we have data for
# create vector of player names to feed "selectInput$player"

ck2018app <- readRDS("./data/ck2018app.rds")
ck2018app_results <- readRDS("./data/ck2018app_results.rds")