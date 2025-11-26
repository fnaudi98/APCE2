#How does individual variation in timing relative to caterpillar food peaks affect reproductive performance and its repeatability in pied flycatchers

#q1.1 Do pied flycatchers adjust their arrival and laying dates to match annual variation in caterpillar peak date? and are these dates repeatable? 
#firs model between and within individuals for timing and caterpillar peak dates 
#repeatability plus plastic response - whether they adjust between years
#laying date 0 simplify by using females only

#q1.2 looking at mismatch from peak date and seeing if there is a trend among individuals and looking at what success outcomes we find (clutch size & recruits, maybe mass too? ) 
#mismatch becomes predictor variable and can look at within birds whether in years they are more matched they have a higher fitness. (clutch size and recruits will have same data for males and females  since they breed together, so maybe best to select females in second step.)
#either look at data separately or put data together and make sex a fixed factor).

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

#start models
m_clutch <- lmer(ClutchSize ~ abs_mismatch + (1|IndividualID) + (1|Year), 
                 data = combined_fem)
summary(m_clutch)
plot(m_clutch)

m_rec <- glmer(Recruits ~ abs_mismatch + 
                 (1|IndividualID) + (1|Year),
               data = combined_fem,
               family = poisson)
summary(m_rec)
plot(m_rec)

m_mass <- lmer(Mass ~ abs_mismatch + 
                 (1|IndividualID) + (1|Year),
               data = combined_fem)
summary(m_mass)
plot(m_mass)

#visualise
ggplot(combined_fem, aes(x = abs_mismatch, y = ClutchSize)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(combined_fem, aes(x = abs_mismatch, y = Recruits)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(combined_fem, aes(x = abs_mismatch, y = Mass)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#this seemed weird, probably because I used absolute mismatch?
#trying with quadratic:

combined_fem$mismatch2 <- combined_fem$mismatch^2

m_clutch2 <- lmer(ClutchSize ~ mismatch + mismatch2 + (1|IndividualID) + (1|Year),
                 data = combined_fem)
summary(m_clutch2)
#Clutch size decreased significantly with increasing mismatch (β = –0.047 ± 0.003, p < 0.001).
#Birds that laid further away from the caterpillar peak produced smaller clutches.
#The quadratic mismatch effect was not significant (p = 0.14), indicating that the decline in clutch size was approximately linear rather than U-shaped.

#Birds laying too early and too late are not behaving differently — any mismatch reduces clutch size.

#Birds that laid further away from the caterpillar peak produced smaller clutches.

m_rec2 <- glmer(Recruits ~ mismatch + mismatch2 + 
                  (1|IndividualID) + (1|Year),
                data = combined_fem,
                family = poisson)
summary(m_rec2)

m_mass2 <- lmer(Mass ~ mismatch + mismatch2 + 
                  (1|IndividualID) + (1|Year),
                data = combined_fem)
summary(m_mass2)


ggplot(combined_fem, aes(x = mismatch, y = ClutchSize)) +
  geom_point() +
  geom_smooth(method="lm", formula=y~x + I(x^2), se=FALSE) 



