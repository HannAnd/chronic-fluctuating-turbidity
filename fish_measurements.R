setwd("~/R/fluctuating-turbidity")
library(tidyverse)

#two week chronic clay exposure experiment
claylong_wide <- read.csv("chronic_flux_turb_fish_measurements.csv")
#two day acute clay exposure experiment
clayshort_wide <- read.csv("acute_flux_turb_fish_measurements_clay.csv")
#two day dye exposure experiment with stirring
dyestir_wide <- read.csv("acute_turb_flux_dye_stir_fish_measurements.csv")
#two day dye exposure experiment without stirring
dyestirless_wide <- read.csv("acute_turb_flux_dye_nostir_fish_measurements.csv")


#converting data from wide to long
claylong <- claylong_wide %>%
  #selecting the columns to manipulate
  pivot_longer(cols = fish1_weight.g:fish3_SL.mm,
               #dividing the fishID from the measurement values
               names_to = c("fish#", "measurement"),
               #identifying the character in the column name that separates the fishID from the measurement type
               names_sep = "_") %>%
  #widening the measurement types into individual columns
  pivot_wider(names_from = "measurement",
              values_from = "value")

clayshort <- clayshort_wide %>%
  pivot_longer(cols = m_fish1_weight.g:f_fish2_SL.mm,
               names_to = c("sex", "fish#", "measurement"),
               names_sep = "_") %>%
  pivot_wider(names_from = "measurement",
              values_from = "value")

dyestir <- dyestir_wide %>%
  pivot_longer(cols = m_fish1_weight.g:f_fish2_SL.mm,
               names_to = c("sex", "fish#", "measurement"),
               names_sep = "_") %>%
  pivot_wider(names_from = "measurement",
              values_from = "value")

dyestirless <- dyestirless_wide %>%
  pivot_longer(cols = m_fish1_weight.g:f_fish2_SL.mm,
               names_to = c("sex", "fish#", "measurement"),
               names_sep = "_") %>%
  pivot_wider(names_from = "measurement",
              values_from = "value")
  
#just a few basic data calculations

#means and standard deviations
claylong %>%
  group_by(sex, time) %>%
  summarise(mean(SL.mm), sd(SL.mm), mean(weight.g), sd(weight.g))
clayshort %>%
  group_by(sex) %>%
  summarise(mean(SL.mm, na.rm = TRUE), sd(SL.mm, na.rm = TRUE), mean(weight.g), sd(weight.g))
dyestir %>%
  group_by(sex) %>%
  summarise(mean(SL.mm), sd(SL.mm), mean(weight.g), sd(weight.g))
dyestirless %>%
  group_by(sex) %>%
  summarise(mean(SL.mm, na.rm = TRUE), sd(SL.mm, na.rm = TRUE), mean(weight.g), sd(weight.g))

#checking for differences in size between treatments groups at the start of experiments
  #there are none
##Chronic two week clay experiment
summary(aov(weight.g ~ treatment, data = claylong))
summary(aov(SL.mm ~ treatment, data = claylong))
##Acute two day clay experiment
summary(aov(weight.g ~ treatment, data = clayshort))
summary(aov(SL.mm ~ treatment, data = clayshort))
##Acute two day dye experiment with stirring
summary(aov(weight.g ~ treatment, data = dyestir))
summary(aov(SL.mm ~ treatment, data = dyestir))
##Acute two day dye experiment without stirring
summary(aov(weight.g ~ treatment, data = dyestirless))
summary(aov(SL.mm ~ treatment, data = dyestirless))
