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

m_clutch2 <- lmer(ClutchSize ~ mismatch  + (1|IndividualID) + (1|Year),
                 data = combined_fem)
summary(m_clutch2)
plot(m_clutch2)
confint(m_clutch2)
#tried quadraticbut there was no support for that so it was removed

m_clutch2b <- lmer(ClutchSize ~ mismatch + mismatch2 + PeakBiomass + (1|IndividualID) + (1|Year),
                  data = combined_fem)
summary(m_clutch2b)
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
  geom_jitter(alpha=.3, width = .5) +
  geom_smooth(method="lm", formula=y~x + I(x^2), se=FALSE, aes(colour=as.factor(Year)))

ggplot(combined_fem, aes(x = mismatch, y = ClutchSize)) +
  geom_jitter(alpha=.3, width = .5) +
  geom_vline(xintercept = -15, colour = "grey40", linetype = "dashed", size = 1) +
  geom_smooth(method="lm", formula=y~x + I(x^2), se=FALSE) +
  facet_wrap(~Year)


ggplot(combined_fem, aes(x = mismatch, y = Recruits)) +
  geom_jitter(alpha=.3, width = .5) +
  geom_vline(xintercept = -15, colour = "grey40", linetype = "dashed", size = 1) +
  geom_smooth(method="lm", formula=y~x + I(x^2), se=FALSE) +
  facet_wrap(~Year)

ggplot(combined_fem, aes(x = mismatch, y = Mass)) +
  geom_jitter(alpha=.3, width = .5) +
  geom_vline(xintercept = -15, colour = "grey40", linetype = "dashed", size = 1) +
  geom_smooth(method="lm", formula=y~x + I(x^2), se=FALSE) +
  facet_wrap(~Year)


#calculate repeatability of mismatch
m_mismatch <- lmer(mismatch ~ 1 + (1|IndividualID) + (1|Year),
                   data = combined)
summary(m_mismatch)
6.978 / (6.978 + 49.064 + 20.660) = 0.09097546
confint(m_mismatch)
#95% CI
(2.852365^2)/((2.852365^2) + (9.924270^2 + 4.669786^2)) = 0.06334759
#2.5% CI
(2.419190^2) /((2.419190^2) + (5.019311^2) + (4.424978^2)) = 0.1156014

#centre averages 
#Load package qdapTools to be able to use the lookup function
install.packages("qdapTools")
library(qdapTools)

# centre Mean peak date per individual (between effect)
ind_avg_peak <- aggregate(cbind(PeakDate) ~ IndividualID, combined_fem, mean)
#Attach the mean peak date to the main dataset
combined_fem$peak_mean <- lookup(combined_fem$IndividualID,ind_avg_peak[, c("IndividualID", "PeakDate")])

#Calculate within individual effect: how each year's peak date differs from individual mean peak date
combined_fem$peak_cen <- combined_fem$PeakDate - combined_fem$peak_mean
#Plasticity = how much the year differs from her usual peak timing

#Model with peak_cen (within individual effect) and peak_mean(between individual effect) 
m_clutch_peak <- lmer(ClutchSize ~ peak_mean + peak_cen + (1|IndividualID),
  data = combined_fem)

summary(m_clutch_peak)

confint(m_clutch_peak)
#confidence intervals overlap (-0.01169989 -0.004407312 = peal_mean, -0.01512442 -0.007094657 = peak_cen) so no strong evidence that within individual effect is different from between individual effect
#this means we attribute it to the within individual effect
#The confidence intervals overlap - So the slopes are statistically indistinguishable - Therefore, between and within effects do not differ
#Interpretation:Because the between- and within-individual effects overlap, there is no evidence females differ consistently in how peak date affects clutch size. Variation in clutch size is therefore attributed primarily to within-individual plasticity.


plot(m_clutch_peak)

#peakmean = between individual effect
#peak_cen = within individual effect


#try centring on lay date and using peak date as a fixed effect
# centre lay per individual (between effect)
ind_avg_laydate <- aggregate(cbind(LayingDate) ~ IndividualID, combined_fem, mean)
#Attach the mean peak date to the main dataset
combined_fem$laydate_between <- lookup(combined_fem$IndividualID,ind_avg_laydate[, c("IndividualID", "LayingDate")])

#Calculate within individual effect: how each year's peak date differs from individual mean peak date
combined_fem$laydate_within <- combined_fem$LayingDate - combined_fem$laydate_between
#Plasticity = how much the year differs from her usual peak timing

#Model with peak_cen (within individual effect) and peak_mean(between individual effect) 
m_clutch_lay <- lmer(ClutchSize ~ laydate_between + laydate_within + PeakDate + (1|IndividualID),
                      data = combined_fem)

summary(m_clutch_lay)

confint(m_clutch_lay)

#try centring on mismatch 
# centre mismatch per individual (between effect)
ind_avg_mismatch <- aggregate(cbind(mismatch) ~ IndividualID, combined_fem, mean)
#Attach the mean peak date to the main dataset
combined_fem$mismatch_between <- lookup(combined_fem$IndividualID,ind_avg_mismatch[, c("IndividualID", "mismatch")])

#Calculate within individual effect: how each year's peak date differs from individual mean peak date
combined_fem$mismatch_within <- combined_fem$mismatch - combined_fem$mismatch_between
#Plasticity = how much the year differs from her usual peak timing

#Model with peak_cen (within individual effect) and peak_mean(between individual effect) 
m_clutch_mismatch <- lmer(ClutchSize ~ mismatch_between + mismatch_within + (1|PeakBiomass) + (1|IndividualID),
                     data = combined_fem)
summary(m_clutch_mismatch)
confint(m_clutch_mismatch)
plot(m_clutch_mismatch)
#effects mainly due to plasticity 