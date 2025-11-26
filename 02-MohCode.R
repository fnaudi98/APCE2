#q1.1 Do pied flycatchers adjust their arrival and laying dates to match annual variation in caterpillar peak date? and are these dates repeatable? 
#first model between and within individuals for timing and caterpillar peak dates 
#repeatability plus plastic response - whether they adjust between years
#laying date 0 simplify by using females only

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

# -------- Clean up data -----------
treedat <- treedat |>
  filter(Year <= 2023) |>
  mutate(peak_burst_diff = BudburstDate - PeakDate)

treedat |>
  ggplot(mapping= aes(x=peak_burst_diff, y = PeakBiomass)) +
  geom_point()

treesummary <- treedat |>
  group_by(Year) |>
  summarise(avgBurstDate = mean(BudburstDate, na.rm=TRUE) |> round())

# add avgBurstDate to birddat
birddat <- treesummary |>
  select(c(Year, avgBurstDate)) |>
  left_join(birddat, by="Year") 

birddat <- birddat |>
  filter(!is.na(Arrival)) |>
  mutate(burst_arr = avgBurstDate - Arrival)

mean(birddat$burst_arr)


# -------- Repeatability of Arrival Date -----------

# ID as random effect
rep_m1 <- birddat |>
  lmer(Arrival ~ (1|IndividualID),
       data=_)
summary(rep_m1)  

# ID and Year as random effect
rep_m2 <- birddat |>
  lmer(Arrival ~ (1|IndividualID) + (1|Year),
       data=_)
summary(rep_m2) 

29.85/(29.85+8.42+31.75) # repeatability = 0.4263068

# show group arrival trend
birddat |>
  group_by(Year) |>
  summarise(avgArrival = mean(Arrival, na.rm=TRUE)) |>
  ggplot(mapping = aes(x = Year, y = avgArrival)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE)

# -------- Repeatability of burst-arr -----------

# ID and Year as random effect
rep_m3 <- birddat |>
  lmer(Arrival~ avgBurstDate+Sex+(1|IndividualID) + (1|Year),
       data=_)
summary(rep_m3) 

28.96/(28.96+45.02+31.11) # repeatability = 0.2755733


# ---- TODO ----
# [] redo what I did for arrival for lay date
# [] within individual centering
# [] between individual centering
# [] instead of difference between dates use burstdate as a fixed effect
# [] dont forget to use sex as fixed effect because males arrive earlier on average


  









