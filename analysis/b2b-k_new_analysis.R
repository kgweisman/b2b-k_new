########################################################### preliminaries #####

# --- PACKAGES & FUNCTIONS ----------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(psych)
library(stats)

# clear environment
rm(list=ls())

# clear graphics
dev.off()

# --- IMPORTING DATA ----------------------------------------------------------

# read in data
d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/B2B-K_new/b2b-k_new/data/anonymized/TEMP_b2b-k_study2_us-children_mean.csv")[-1] # get rid of column of obs numbers

glimpse(d)

########################################################### summary stats #####

# --- DEMOGRAPHICS ------------------------------------------------------------

# study 1: us adults, "does that mean?" framing
demo = dd %>% distinct(subid)

# total n
demo %>% summarise(n = length(subid))

# condition assignment
dd %>% group_by(sequence) %>% distinct(subid) %>% summarise(n = length(subid))

# gender
demo %>% count(gender)

# ethnicity
demo %>% count(ethnicity)

# age
demo %>% summarise(mean_age = mean(ageCalc, na.rm = T), sd_age = sd(ageCalc, na.rm = T))
qplot(demo$ageCalc)

# demo %>% filter(age < 100) %>% summarise(mean_age = mean(age, na.rm = T), sd_age = sd(age, na.rm = T))
# qplot(demo$age[demo$age < 100])

