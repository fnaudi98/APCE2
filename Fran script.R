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
  filter(Sex == 1) |>
  filter( Year <=2023)


#---- begin models----
#1 model for clutch size

#1.1 calculate repeatability for clutch size: 
m_clutch_rep <- lmer(ClutchSize ~ 1 + (1|IndividualID) + (1|Year),
                     data = combined_fem)
summary(m_clutch_rep)
#repeatability
0.20771 / (0.20771 + 0.02476 + 0.34143) #= 0.3619272
confint(m_clutch_rep)
#95% CI
(0.4901923^2)/((0.4901923^2) + (0.2284743^2 + 0.6081420^2)) #= 0.3627951
#2.5% CI
(0.4196112^2) /((0.4196112^2) + (0.1075839^2) + (0.5617665^2)) #= 0.3498872

#1.2 clutch size explained by mismatch
#tried to do this quadratic, but it provided no added value.
m_clutch <- lmer(ClutchSize ~ mismatch  + (1|IndividualID) + (1|Year),
                 data = combined_fem)
summary(m_clutch) #if std error is as least half of the estumate then it is significant, negative = higher mismatch, smaller clutch
plot(m_clutch)
confint(m_clutch)
#tried quadratic but there was no support for that so it was removed



#1.3 within and between for clutch size centred on mismatch

# centre mismatch per individual (between effect)
ind_avg_mismatch <- aggregate(cbind(mismatch) ~ IndividualID, combined_fem, mean)
#Attach the mean peak date to the main dataset
combined_fem$mismatch_between <- lookup(combined_fem$IndividualID,ind_avg_mismatch[, c("IndividualID", "mismatch")])

#Calculate within individual effect: how each year's peak date differs from individual mean peak date
combined_fem$mismatch_within <- combined_fem$mismatch - combined_fem$mismatch_between
#Plasticity = how much the year differs from her usual peak timing

#Model 
m_clutch_mismatch <- lmer(ClutchSize ~ mismatch_between + mismatch_within + (1|IndividualID),
                          data = combined_fem) 
summary(m_clutch_mismatch) #random effects can never be continuous. when you have a direction in mind on how a variable affects your response variable mean, put it as fixed effect
confint(m_clutch_mismatch) #more between individual effect as opposed to plasticity. maybe the females that always arrive early have better clutch sizes 
plot(m_clutch_mismatch)
#UpperCI
(0.45323063^2)/((0.45323063^2) + (0.56208476^2 + 0.54953784^2)) #= 0.3435922
#effects mainly due to plasticity 

ggplot()

#2 model for Recruits 

#2.1 calculate repeatability for Recruits
m_recruits_rep <- glmer(Recruits ~ 1 + (1|IndividualID) + (1|Year),
                        data = combined_fem, family = poisson)
summary(m_recruits_rep)
confint(m_recruits_rep)

#repeatability
#since it is a poisson model you cannot calculate repeatability - no residual 

#2.2 Recruits explained by mismatch
m_rec <- glmer(Recruits ~ mismatch + (1|IndividualID) + (1|Year),
                data = combined_fem,
                family = poisson)
summary(m_rec)
confint(m_rec)

#2.3 within and between for Recruits centred on mismatch

# centre mismatch per individual (between effect)
ind_avg_mismatch <- aggregate(cbind(mismatch) ~ IndividualID, combined_fem, mean)
#Attach the mean peak date to the main dataset
combined_fem$mismatch_between <- lookup(combined_fem$IndividualID,ind_avg_mismatch[, c("IndividualID", "mismatch")])

#Calculate within individual effect: how each year's peak date differs from individual mean peak date
combined_fem$mismatch_within <- combined_fem$mismatch - combined_fem$mismatch_between
#Plasticity = how much the year differs from her usual peak timing

#Model with(within individual effect) (between individual effect) 
m_recruits_mismatch <- lmer(Recruits ~ mismatch_between + mismatch_within + (1|IndividualID),
                          data = combined_fem)
summary(m_recruits_mismatch) 
confint(m_recruits_mismatch) 
plot(m_recruits_mismatch)


#plot
#test plot 1
plot_between <- ggpredict(m_clutch_mismatch, terms = "mismatch_between") %>%
  mutate(effect = "Between-individual")

plot_within <- ggpredict(m_clutch_mismatch, terms = "mismatch_within") %>%
  mutate(effect = "Within-individual")

plot_data <- bind_rows(plot_between, plot_within)

ggplot(plot_data, aes(x = x, y = predicted, colour = effect, linetype = effect)) +
  geom_line(size = 1.2) +
  scale_colour_manual(values = c("Between-individual" = "blue",
                                 "Within-individual" = "red")) +
  scale_linetype_manual(values = c("Between-individual" = "solid",
                                   "Within-individual" = "dashed")) +
  labs(
    x = "Mismatch (days)",
    y = "Predicted Clutch Size",
    colour = "",
    linetype = "",
    title = "Within vs Between Individual effects of Mismatch on Clutch Size"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "top")

#plot 3
# Extract the data actually used by the model:
femdat_model <- combined_fem[rownames(model.frame(m_clutch_mismatch)), ] %>%
  mutate(pred = predict(m_clutch_mismatch))

set.seed(123)  # ensures reproducible sampling

sample_ids <- femdat_model %>%
  group_by(IndividualID) %>%
  filter(n() >= 3) %>%
  summarise() %>%
  slice_sample(n = 10) %>%
  pull(IndividualID)

plotdat <- femdat_model %>%
  filter(IndividualID %in% sample_ids)

ggplot(combined_fem, aes(x = mismatch_within, y = ClutchSize)) +
  geom_smooth(data=plotdat, aes(x=mismatch_within,
                                y=pred,group = IndividualID),
              method="lm",
              formula = y~x, se=FALSE,
              alpha = 0.8, linewidth = .5, color = "steelblue") +
  geom_smooth(data=femdat_model,aes(x=mismatch_between, y=pred),
              method = "lm", se = FALSE, color = "black") +
  labs(x="Mismatch between lay date and caterpillar peak",
       y="Clutch size")


#plot 3 - final plot
# Extract the data actually used by the model:
femdat_model <- combined_fem[rownames(model.frame(m_clutch_mismatch)), ] |>
  mutate(pred = predict(m_clutch_mismatch),
         mismatch = mismatch_within + mismatch_between    # uses ONLY your variables
  )

set.seed(123)  # ensures reproducible sampling

sample_ids <- femdat_model %>%
  group_by(IndividualID) %>%
  filter(n() >= 3) %>%
  summarise() %>%
  slice_sample(n = 10) %>%
  pull(IndividualID)

plotdat <- femdat_model %>%
  filter(IndividualID %in% sample_ids)

ggplot() +
  # Reaction norm lines (model predictions)
  geom_line(
    data = plotdat,
    aes(
      x = mismatch,
      y = pred,
      group = IndividualID,
      color = "Individual reaction norms"
    ),
    linewidth = 0.8,
    alpha = 0.8
  ) +
  
  # Population-level fixed-effect line
  geom_smooth(
    data = femdat_model,
    aes(
      x = mismatch,
      y = ClutchSize,
      color = "Population-level effect"
    ),
    method = "lm",
    se = FALSE,
    linewidth = 1.1
  ) +
  
  scale_color_manual(
    name = "",
    values = c(
      "Individual reaction norms" = "steelblue",
      "Population-level effect" = "black"
    )
  ) +
  
  labs(
    title = "Within vs Between Individual effects of Mismatch on Clutch Size",
    x = "Mismatch between lay date and caterpillar peak",
    y = "Clutch size"
  ) +
  #theme_classic(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(8, 8, 8, 8),
    legend.position = "inside",
    legend.position.inside = c(.85,.9)
  )

#plot3 for recruits
# Extract the data actually used by the model:
femdat_model2 <- combined_fem[rownames(model.frame(m_recruits_mismatch)), ] |>
  mutate(pred = predict(m_recruits_mismatch),
         mismatch = mismatch_within + mismatch_between    # uses ONLY your variables
  )

set.seed(123)  # ensures reproducible sampling

sample_ids <- femdat_model2 %>%
  group_by(IndividualID) %>%
  filter(n() >= 3) %>%
  summarise() %>%
  slice_sample(n = 10) %>%
  pull(IndividualID)

plotdat <- femdat_model2 %>%
  filter(IndividualID %in% sample_ids)

ggplot() +
  # Reaction norm lines (model predictions)
  geom_line(
    data = plotdat,
    aes(
      x = mismatch,
      y = pred,
      group = IndividualID,
      color = "Individual reaction norms"
    ),
    linewidth = 0.8,
    alpha = 0.8
  ) +
  
  # Population-level fixed-effect line
  geom_smooth(
    data = femdat_model,
    aes(
      x = mismatch,
      y = Recruits,
      color = "Population-level effect"
    ),
    method = "lm",
    se = FALSE,
    linewidth = 1.1
  ) +
  
  scale_color_manual(
    name = "",
    values = c(
      "Individual reaction norms" = "steelblue",
      "Population-level effect" = "black"
    )
  ) +
  
  labs(
    title = "Within vs Between Individual effects of Mismatch on Recruits",
    x = "Mismatch between lay date and caterpillar peak",
    y = "Number of Recruits"
  ) +
  #theme_classic(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(8, 8, 8, 8),
    legend.position = "inside",
    legend.position.inside = c(.85,.9)
  )
