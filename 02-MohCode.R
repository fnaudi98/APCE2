# -------- Script start stuff -----------
# clear all data
remove(list=ls())

# load libraries
library(tidyverse)
library(lme4)
library(ggplot2)

# upload data
budburst_db   <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSRpZm3ve3ymZ-Cg3oDo0cw_-s3Ii_YZoCF7mmOa9e27cpwaGwqcTKcbgnK_ZFb54KEoj-1lBeKBOkG/pub?gid=552268252&single=true&output=csv"
flycatcher_db <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSRpZm3ve3ymZ-Cg3oDo0cw_-s3Ii_YZoCF7mmOa9e27cpwaGwqcTKcbgnK_ZFb54KEoj-1lBeKBOkG/pub?gid=1908224333&single=true&output=csv"

treedat <- read_csv(budburst_db)
birddat <- read_csv(flycatcher_db)
