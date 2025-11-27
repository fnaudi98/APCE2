# -------- Script start stuff -----------
# clear all data
remove(list=ls())
#---- laod packages ----

suppressPackageStartupMessages({
  library(googlesheets4)
  library(dplyr)
  library(purrr)
  library(tidyverse)
  library(patchwork)
  library(httpuv)
})

library(tidyverse)
library(lme4)
library(ggplot2)

#---- load and prepare data ----
caterpillar <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSRpZm3ve3ymZ-Cg3oDo0cw_-s3Ii_YZoCF7mmOa9e27cpwaGwqcTKcbgnK_ZFb54KEoj-1lBeKBOkG/pub?gid=552268252&single=true&output=csv") |>
  as.tibble()

pied <-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSRpZm3ve3ymZ-Cg3oDo0cw_-s3Ii_YZoCF7mmOa9e27cpwaGwqcTKcbgnK_ZFb54KEoj-1lBeKBOkG/pub?gid=1908224333&single=true&output=csv") |>
  as.tibble()

caterpillar_year <- caterpillar |>
  group_by(Year) |>
  summarise(
    PeakDate = mean(PeakDate, na.rm = TRUE),
    PeakBiomass = mean(PeakBiomass, na.rm = TRUE),
    BudburstDate = mean(BudburstDate, na.rm = TRUE)
  )

combined <- pied |>
  left_join(caterpillar_year, by = "Year")

combined$mismatch <- combined$LayingDate - combined$PeakDate
combined$abs_mismatch <- abs(combined$mismatch)

combined <- combined |>
  group_by(IndividualID) |>
  mutate(
    peak_mean = mean(PeakDate, na.rm = TRUE),
    peak_within = PeakDate - peak_mean
  ) |>
  ungroup()

combined_fem <- combined |>
  filter(Sex == 1)


#---- begin models----
#model for clutch size
m_clutch <- lmer(ClutchSize ~ mismatch  + (1|IndividualID) + (1|Year),
                 data = combined_fem)
summary(m_clutch) #if std error is as least half of the estumate then it is significant, negative = higher mismatch, smaller clutch
plot(m_clutch)
confint(m_clutch)
#tried quadratic but there was no support for that so it was removed


#calculate repeatability for clutch size: 
m_clutch_rep <- lmer(ClutchSize ~ 1 + (1|IndividualID) + (1|Year),
                   data = combined_fem)
summary(m_clutch_rep)
0.19809 / (0.19809 + 0.03048 + 0.35030) #= 0.19809
confint(m_clutch_rep)
#95% CI
(0.4788353^2)/((0.4788353^2) + (0.2476464^2 + 0.6146856^2)) #= 0.3430071
#2.5% CI
(0.4096222^2) /((0.4096222^2) + (0.1225611^2) + (0.5701928^2)) #= 0.3303406


#within and between for clutch size centred on mismatch

# centre mismatch per individual (between effect)
ind_avg_mismatch <- aggregate(cbind(mismatch) ~ IndividualID, combined_fem, mean)
#Attach the mean peak date to the main dataset
combined_fem$mismatch_between <- lookup(combined_fem$IndividualID,ind_avg_mismatch[, c("IndividualID", "mismatch")])

#Calculate within individual effect: how each year's peak date differs from individual mean peak date
combined_fem$mismatch_within <- combined_fem$mismatch - combined_fem$mismatch_between
#Plasticity = how much the year differs from her usual peak timing

#Model with peak_cen (within individual effect) and peak_mean(between individual effect) 
m_clutch_mismatch <- lmer(ClutchSize ~ mismatch_between + mismatch_within + (1|IndividualID) + (1|Year),
                          data = combined_fem) #add year 
summary(m_clutch_mismatch) #random effects can never be continous. when you have a direction in mind on how a variable affects your response variable mean, put it as fixed effect
confint(m_clutch_mismatch) #more between individual effect as opposed to plasticity. maybe the females that always arrive early have better clutch sizes 
plot(m_clutch_mismatch)
#effects mainly due to plasticity 

#model for Recruits 

m_rec <- glmer(Recruits ~ mismatch + (1|IndividualID) + (1|Year),
                data = combined_fem,
                family = poisson)
summary(m_rec)

#calculate repeatability for Recruits
m_recruits_rep <- lmer(Recruits ~ 1 + (1|IndividualID) + (1|Year),
                     data = combined_fem)
summary(m_recruits_rep)
confint(m_recruits_rep)

#repeatability
0.013850 / (0.013850 + 0.001417 + 0.443682) #= 0.03017765
confint(m_clutch_rep)
#95% CI
(0.18919561^2)/((0.18919561^2) + (0.07041244^2 + 0.68818278^2)) #= 0.06959286
#2.5% CI
(0.0000000^2) /((0.0000000^2) + (0.0000000^2) + (0.6446184^2)) #= 0


#within and between for Recruits centred on mismatch

# centre mismatch per individual (between effect)
ind_avg_mismatch <- aggregate(cbind(mismatch) ~ IndividualID, combined_fem, mean)
#Attach the mean peak date to the main dataset
combined_fem$mismatch_between <- lookup(combined_fem$IndividualID,ind_avg_mismatch[, c("IndividualID", "mismatch")])

#Calculate within individual effect: how each year's peak date differs from individual mean peak date
combined_fem$mismatch_within <- combined_fem$mismatch - combined_fem$mismatch_between
#Plasticity = how much the year differs from her usual peak timing

#Model with peak_cen (within individual effect) and peak_mean(between individual effect) 
m_recruits_mismatch <- lmer(Recruits ~ mismatch_between + mismatch_within + (1|IndividualID) + (1|Year),
                          data = combined_fem) #add year 
summary(m_recruits_mismatch) 
confint(m_recruits_mismatch) 
plot(m_recruits_mismatch)
