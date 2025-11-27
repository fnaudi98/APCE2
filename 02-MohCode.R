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
  summarise(avgBurstDate = mean(BudburstDate, na.rm=TRUE) |> round(),
            avgCatPeakDate = mean(PeakDate, na.rm=TRUE) |> round())

# add avgBurstDate to birddat
birddat <- treesummary |>
  select(c(Year, avgBurstDate, avgCatPeakDate)) |>
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

# ID and Year as random effect
# burstdate and sex as fixed effect
rep_m3 <- birddat |>
  lmer(Arrival~ Sex + (1|IndividualID) + (1|Year),
       data=_)
summary(rep_m3) 

13.112/(13.112+5.647+32.242) # repeatability = 0.257093 

# -------- Repeatability of Lay Date -----------
# ID as random effect
rep_m4 <- birddat |>
  filter(Sex == 1) |>
  lmer(LayingDate ~ (1|IndividualID),
       data=_)
summary(rep_m4)  

# ID and Year as random effect
rep_m5 <- birddat |>
  filter(Sex == 1) |>
  lmer(LayingDate ~ (1|IndividualID) + (1|Year),
       data=_)
summary(rep_m5) 

9.911/(9.911+6.630+15.280) # repeatability = 0.311461 !!!

# ID and Year as random effect
# burstdate as fixed effect
rep_m6 <- birddat |>
  filter(Sex == 1) |>
  lmer(LayingDate ~ avgBurstDate + (1|IndividualID) + (1|Year),
       data=_)
summary(rep_m6) 
confint(rep_m6)

9.928/(9.928+2.602+15.268) # repeatability = 0.357148

# ID and Year as random effect
# burstdate and sex as fixed effect
rep_m6.1 <- birddat |>
  filter(Sex == 1) |>
  lmer(LayingDate ~ burst_arr + (1|IndividualID) + (1|Year),
       data=_)
summary(rep_m6.1) 

1.140/(1.140+37.865+5.362) # repeatability = 0.02569477

# -------- Centering Within/Between Individuals -----------
#Load package qdapTools to be able to use the lookup function
#install.packages("qdapTools")
library(qdapTools)

# make female dataframe
femdat <- birddat |>
  filter(Sex == 1)

#Center Annual Density per individual
ind_avg<-aggregate(cbind(avgBurstDate)~IndividualID,femdat,mean) # Calc avg density per fem
## Between individual effect: mean density for each female! This is how individuals differ
femdat$Btw_Ind<-lookup(femdat$IndividualID,ind_avg[,c("IndividualID","avgBurstDate")])
## Within individual effect: how each value differs from individual mean.
femdat$Wthin_Ind<-femdat$avgBurstDate-femdat$Btw_Ind
#Model with annual_density_cen (within individual effect) and avgAnDens (between individual effect
m7<-lmer(LayingDate ~ Wthin_Ind + Btw_Ind + (1|IndividualID), data= femdat)
summary(m7)
confint(m7) #  !!!


#Center Annual Density per individual
ind_avg<-aggregate(cbind(avgCatPeakDate)~IndividualID,femdat,mean) # Calc avg density per fem
## Between individual effect: mean density for each female! This is how individuals differ
femdat$Btw_Ind<-lookup(femdat$IndividualID,ind_avg[,c("IndividualID","avgCatPeakDate")])
## Within individual effect: how each value differs from individual mean.
femdat$Wthin_Ind<-femdat$avgCatPeakDate-femdat$Btw_Ind
#Model with annual_density_cen (within individual effect) and avgAnDens (between individual effect
m7.1<-lmer(LayingDate ~ Wthin_Ind + Btw_Ind + Arrival + (1|IndividualID), data= femdat)
summary(m7.1)
confint(m7.1) 


### again but for Arrival

#Center Annual Density per individual
ind_avg<-aggregate(cbind(avgBurstDate)~IndividualID,birddat,mean) # Calc avg density per fem
## Between individual effect: mean density for each female! This is how individuals differ
birddat$Btw_Ind<-lookup(birddat$IndividualID,ind_avg[,c("IndividualID","avgBurstDate")])
## Within individual effect: how each value differs from individual mean.
birddat$Wthin_Ind<-birddat$avgBurstDate-birddat$Btw_Ind
#Model with annual_density_cen (within individual effect) and avgAnDens (between individual effect
m8<-lmer(Arrival ~ Wthin_Ind + Btw_Ind + Sex + (1|IndividualID), data= birddat)
summary(m8)
confint(m8)


### again but for burst-arrival
#Center Annual Density per individual
ind_avg<-aggregate(cbind(burst_arr)~IndividualID,femdat,mean) # Calc avg density per fem
## Between individual effect: mean density for each female! This is how individuals differ
femdat$Btw_Ind<-lookup(femdat$IndividualID,ind_avg[,c("IndividualID","burst_arr")])
## Within individual effect: how each value differs from individual mean.
femdat$Wthin_Ind<-femdat$burst_arr-femdat$Btw_Ind
#Model with annual_density_cen (within individual effect) and avgAnDens (between individual effect
m9<-lmer(LayingDate ~ Wthin_Ind + Btw_Ind + (1|IndividualID), data= femdat)
summary(m9)
confint(m9) # 

ggplot(femdat, aes(x = Wthin_Ind, y = LayingDate)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~Year)

#
# Extract the data actually used by the model:
femdat_model <- femdat[rownames(model.frame(m7)), ] %>%
  mutate(pred = predict(m7),
         budburst = Wthin_Ind + Btw_Ind    # uses ONLY your variables
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
      x = budburst,
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
      x = budburst,
      y = LayingDate,
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
    title = "Within vs Between Individual Lay Dates Centered on Budburst Date",
    x = "Budburst date (days since April 1)",
    y = "Laying date (days since April 1)"
  ) +
  #theme_classic(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(8, 8, 8, 8),
    legend.position = "top"
  )


# ---- TODO ----
# [x] redo what I did for arrival for lay date
# [x] within individual centering
# [x] between individual centering
# [x] instead of difference between dates use burstdate as a fixed effect
# [x] dont forget to use sex as fixed effect because males arrive earlier on average

library(modelsummary)

modelsummary(
  list(
    "Laying Date Repeatability"  = rep_m5,
    "Laying Date Individual Centering"  = m7
  )) 





  









