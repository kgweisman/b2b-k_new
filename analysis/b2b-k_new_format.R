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
# - AFFECT ITEMS: standard set (positive/negative valence)

# study 1' (2014-05-23) - SUPPLEMENTAL
# - EXPERIMENTAL SETTING: mturk
# - COUNTRY: us
# - AGE GROUP: adults
# - FRAMING: "does that mean...?" 
# - AFFECT ITEMS: alternative set (positive/negative valence, high/low arousal)

# study 2 (spring 2014 - fall 2014)
# - EXPERIMENTAL SETTING: university preschool
# - COUNTRY: us
# - AGE GROUP: children
# - FRAMING: "does that mean...?" 
# - AFFECT ITEMS: standard set (positive/negative valence)

# study 3 (2014-06-17)
# - EXPERIMENTAL SETTING: mturk
# - COUNTRY: india
# - AGE GROUP: adults
# - FRAMING: "does that mean...?" 
# - AFFECT ITEMS: standard set (positive/negative valence)

# study 4a (2014-06-25)
# - EXPERIMENTAL SETTING: mturk
# - COUNTRY: us
# - AGE GROUP: adults
# - FRAMING: "do you think...?" 
# - AFFECT ITEMS: standard set (positive/negative valence)

# study 4b (2014-06-25)
# - EXPERIMENTAL SETTING: mturk
# - COUNTRY: india
# - AGE GROUP: adults
# - FRAMING: "do you think...?" 
# - AFFECT ITEMS: standard set (positive/negative valence)

# --- READING IN RAW DATA -----------------------------------------------------

# ------ study 1 --------------------------------------------------------------

# read in raw data from qualtrics
d1.raw <- read.csv("./data/raw/b2b-k_study1_us-adults_mean.csv", 
                   header = FALSE,
                   colClasses = "character")

# use column names provided by qualtrics
d1.0 <- d1.raw
names(d1.0)[1:12] <- d1.raw[2,1:12]
names(d1.0)[13:155] <- d1.raw[1,13:155]
d1.1 <- d1.0[-c(1,2),] # remove rows containing column names
row.names(d1.1) = NULL

# rename columns with more informative labels
d1.2 <- d1.1
names(d1.2) <- ifelse(grepl("Q1.", names(d1.2), fixed = T) == T |
                        grepl("Q2.", names(d1.2), fixed = T) == T, 
                      "intro",
                      gsub("Q3.", "practice.", names(d1.2), fixed = T))
names(d1.2) <- gsub("_1", "_response", names(d1.2))
names(d1.2) <- gsub("Q20.2", "comments", names(d1.2))
names(d1.2)[13:152] <- ifelse(
  grepl("_response", names(d1.2[13:152]), fixed = T) == F & 
    grepl("comments", names(d1.2[13:152]), fixed = T) == F,
  paste0(names(d1.2[13:152]), "_text"), 
  names(d1.2[13:152]))

# eliminate superfluous columns (no data)
d1.3 <- d1.2 %>% 
  select(-ResponseSet, -Name, -ExternalDataReference, -EmailAddress,
         -IPAddress, -Status, -mTurkCODE, -LocationLatitude, 
         -LocationLongitude, -LocationAccuracy, -ends_with("text"))

# separate by sequence
d1.4 <- data.frame()
for (i in 1:8) {
  temp = subset(d1.3, Sequence == i)
  temp[temp == ""] <- NA
  temp <- Filter(function(x)!all(is.na(x)), temp)
  names(temp) = c(names(d1.3[1:5]),
                  "p1", "p2", "p3", # practice trials
                  "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", # test trials
                  "comments")
  d1.4 <- bind_rows(d1.4, temp)
  rm(temp, i)
}

# recode variables
d1.5 <- d1.4 %>%
  mutate(subid = factor(ResponseID), 
         dateOfTest = parse_date_time(StartDate, orders = "mdyHM"),
         durationOfTest = as.numeric(
           parse_date_time(EndDate, orders = "mdyHM") - 
             parse_date_time(StartDate, orders = "mdyHM"))/60,
         status = factor(ifelse(is.na(t8) == T, "partial", "complete")),
         sequence = factor(Sequence),
         # center 4-point response scale around 0
         p1 = as.numeric(p1) - 2.5, 
         p2 = as.numeric(p2) - 2.5,
         p3 = as.numeric(p3) - 2.5,
         t1 = as.numeric(t1) - 2.5,
         t2 = as.numeric(t2) - 2.5,
         t3 = as.numeric(t3) - 2.5,
         t4 = as.numeric(t4) - 2.5,
         t5 = as.numeric(t5) - 2.5,
         t6 = as.numeric(t6) - 2.5,
         t7 = as.numeric(t7) - 2.5,
         t8 = as.numeric(t8) - 2.5) %>%
  select(subid, dateOfTest, durationOfTest, status, sequence, p1:t8)

# remove participants who did not complete session
d1.6 <- subset(d1.5, status == "complete") %>% select(-status)

# randomly select 10 participants per sequence
d1.7 <- d1.6 %>%
  group_by(sequence) %>%
  sample_n(10, replace = F)

d1.processed <- d1.7
rm(d1.raw, d1.0, d1.1, d1.2, d1.3, d1.4, d1.5, d1.6, d1.7)

