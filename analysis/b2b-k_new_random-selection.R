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

# --- RANDOM SELECTION: EVENING OUT CONDITION ASSIGMENT -----------------------

# read in data
d0 = read.csv("./data/anonymized/b2b-k_adults-data_anonymized.csv")[-1] # delete observation numbers

# set number of participants to select per sequence
psPerSequence = data.frame(study = levels(d0$study),
                      n = c(10, 9, 10, 10))

# randomly choose n participants from each sequence for each study
subidList <- data.frame()
for (i in levels(d0$study)) {
  n <- psPerSequence$n[psPerSequence$study == i]
  temp <- d0 %>% 
    subset(study == i) %>%
    select(study, country, sequence, subid) %>%
    distinct() %>%
    group_by(study, country, sequence) %>%
    sample_n(n, replace = F)
  subidList <- bind_rows(subidList, temp)
  rm(n, i)
}

# filter dataset to include only randomly selected participants
d <- d0 %>%
  filter(is.element(subid, subidList$subid))

# check
checkTable <- d %>% group_by(study, country, sequence) %>% select(subid) %>% unique() %>% summarise(count = length(subid))
# View(checkTable)

# --- WRITING FINAL DATAFILES TO CSV ------------------------------------------

# write subidList to csv file
write.csv(subidList, "./data/anonymized/b2b-k_adults-randomized_subidList.csv")

# write data to de-identified csv file
write.csv(d, "./data/anonymized/b2b-k_adults-data_anonymized-and-randomized.csv")

d = read.csv("./data/anonymized/b2b-k_adults-data_anonymized-and-randomized.csv")[-1] # delete observation numbers