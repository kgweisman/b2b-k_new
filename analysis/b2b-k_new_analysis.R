# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(stats)
library(stringr)
library(lubridate)

# clear environment
rm(list=ls())

# set working directory
setwd("/Users/kweisman/Documents/Research (Stanford)/Projects/B2B-K_new/b2b-k_new/")

# --- STUDY KEY ---------------------------------------------------------------

# study 1 (2014-04-10)
# - EXPERIMENTAL SETTING: mturk
# - COUNTRY: us
# - AGE GROUP: adults
# - FRAMING: "does that mean...?" 
# - ITEM SET: cb1 (affect: positive/negative valence)

# study 1' (2014-05-23) - SUPPLEMENTAL
# - EXPERIMENTAL SETTING: mturk
# - COUNTRY: us
# - AGE GROUP: adults
# - FRAMING: "does that mean...?" 
# - ITEM SET: cb2 (affect: positive/negative valence & high/low arousal)

# study 2 (spring 2014 - fall 2014)
# - EXPERIMENTAL SETTING: university preschool
# - COUNTRY: us
# - AGE GROUP: children
# - FRAMING: "does that mean...?" 
# - ITEM SET: cb1 (affect: positive/negative valence)

# study 3 (2014-06-17)
# - EXPERIMENTAL SETTING: mturk
# - COUNTRY: india
# - AGE GROUP: adults
# - FRAMING: "does that mean...?" 
# - ITEM SET: cb1 (affect: positive/negative valence)

# study 4a (2014-06-25)
# - EXPERIMENTAL SETTING: mturk
# - COUNTRY: us
# - AGE GROUP: adults
# - FRAMING: "do you think...?" 
# - ITEM SET: cb1 (affect: positive/negative valence)

# study 4b (2014-06-25)
# - EXPERIMENTAL SETTING: mturk
# - COUNTRY: india
# - AGE GROUP: adults
# - FRAMING: "do you think...?" 
# - ITEM SET: cb1 (affect: positive/negative valence)

# --- IMPORTING DATA ----------------------------------------------------------

# read in data
d = read.csv("./data/anonymized/b2b-k_adults-data_anonymized-and-randomized.csv")[-1] # delete observation numbers

# glimpse(d)

# --- DEMOGRAPHICS ------------------------------------------------------------

# sample size by study
sampleSize <- d %>% distinct(subid) %>% group_by(study, country) %>% summarise(n = length(subid))
# print(sampleSize)

# sequence assignment by study
sequenceAssign <- d %>% distinct(subid) %>% group_by(study, country, sequence) %>% summarise(n = length(subid))
# print(sequenceAssign)

# duration by study (minutes; adults only)
duration <- d %>% distinct(subid) %>% group_by(study, country) %>% summarise(m = mean(durationOfTest), sd = sd(durationOfTest))
# print(duration)

# gender by study
gender <- d %>% distinct(subid) %>% group_by(study, country, gender) %>% summarise(n = length(gender))
# print(gender)

# age by study (years; children only)
age <- d %>% distinct(subid) %>% group_by(study, country) %>% summarise(m = mean(age), sd = sd(age), min = min(age), max = max(age))
# print(age)

# race/ethnicity by study
# ... finest grain
raceEthn4 <- d %>% distinct(subid) %>% group_by(study, country, raceEthn4) %>% summarise(n = length(raceEthn4))
# print(raceEthn4)
# ... white-only/some-eastern/other
raceEthn3 <- d %>% distinct(subid) %>% group_by(study, country, raceEthn3) %>% summarise(n = length(raceEthn3))
# print(raceEthn3)
# ... coarsest grain
raceEthn2 <- d %>% distinct(subid) %>% group_by(study, country, raceEthn2) %>% summarise(n = length(raceEthn2))
# print(raceEthn2)

# religion by study (study 4 only)
religion <- d %>% distinct(subid) %>% group_by(study, country, religion) %>% summarise(n = length(religion))
# print(religion)

# --- STUDY 1 -----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# comparison to neutral

# planned contrasts

# ------ exploratory analyses -------------------------------------------------

# planned contrasts on absolute values

# --- STUDY 1' ----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# comparison to neutral

# planned contrasts

# --- STUDY 2 -----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# comparison to neutral

# planned contrasts

# ------ exploratory analyses -------------------------------------------------

# planned contrasts on absolute values

# race/ethnicity comparison: chi-squared tests

# race/ethnicity comparison: comparison to neutral

# race/ethnicity comparison: planned contrasts

# adult/child comparison: planned contrasts

# --- STUDY 3 -----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# comparison to neutral

# planned contrasts

# ------ exploratory analyses -------------------------------------------------

# planned contrasts on absolute values

# us/india comparison: planned contrasts

# --- STUDY 4 -----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# us/india comparison: comparison to neutral

# us/india comparison: planned contrasts

# us/india comparison: planned contrasts on absolute values

# ------ exploratory analyses -------------------------------------------------

# us/india comparison, framing comparison: planned contrasts

# us/india comparison, framing comparison: planned contrasts on absolute values
