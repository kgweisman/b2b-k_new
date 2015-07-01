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

# make function for formatting qualtrics data
tidyFormat <- function(rawDataFilename, psPerSequence,
                       study, country, ageGroup, framing) {

  # set study specifications
  study <- study
  country <- country
  ageGroup <- ageGroup
  framing <- framing
  
  # read in raw data from qualtrics
  d.raw <- read.csv(paste0("./data/raw/", rawDataFilename), 
                     header = FALSE,
                     colClasses = "character")
  
  # use column names provided by qualtrics
  d.0 <- d.raw
  names(d.0)[1:12] <- d.raw[2,1:12]
  names(d.0)[13:length(d.0)] <- d.raw[1,13:length(d.raw)]
  d.1 <- d.0[-c(1,2),] # remove rows containing column names
  row.names(d.1) = NULL
  
  # rename columns with more informative labels
  d.2 <- d.1
  names(d.2) <- ifelse(grepl("Q1.", names(d.2), fixed = T) == T |
                          grepl("Q2.", names(d.2), fixed = T) == T, 
                        "intro",
                        gsub("Q3.", "practice.", names(d.2), fixed = T))
  names(d.2) <- gsub("_1", "_response", names(d.2))
  names(d.2) <- gsub("Q20.2", "comments", names(d.2))
  names(d.2) <- gsub("Q144", "demographics", names(d.2))
  names(d.2) <- gsub("Q142", "gender_response", names(d.2))
  names(d.2) <- gsub("Q143", "religion_response", names(d.2))
  names(d.2) <- gsub("Q141", "repeat_response", names(d.2))
  names(d.2)[13:length(names(d.2))] <- ifelse(
    grepl("_response", names(d.2[13:length(names(d.2))]), fixed = T) == F & 
      grepl("comments", names(d.2[13:length(names(d.2))]), fixed = T) == F,
    paste0(names(d.2[13:length(names(d.2))]), "_text"), 
    names(d.2[13:length(names(d.2))]))
  
  # eliminate superfluous columns (no data)
  d.3 <- d.2 %>% 
    select(ResponseID, StartDate, EndDate, Finished, Sequence, intro_text:comments) %>%
    select(-ends_with("text"))
  
  # separate by sequence
  d.4 <- data.frame()
  for (i in 1:8) {
    temp = subset(d.3, Sequence == i)
    temp[temp == ""] <- NA
    temp <- Filter(function(x)!all(is.na(x)), temp)
    names(temp) = c(names(d.3[1:5]),
                    "p1", "p2", "p3", # practice trials
                    "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", # test trials
                    "comments")
    d.4 <- bind_rows(d.4, temp)
    rm(temp, i)
  }
  
  # recode variables
  d.5 <- d.4 %>%
    mutate(study = factor(study),
           country = factor(country),
           ageGroup = factor(ageGroup),
           framing = factor(framing),
           subid = factor(ResponseID), 
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
    select(study, country, ageGroup, framing, 
           subid, dateOfTest, durationOfTest, status, sequence, p1:t8)
  
  # remove participants who did not complete session
  d.6 <- subset(d.5, status == "complete") %>% select(-status)
  
  # randomly select n participants per sequence
  d.7 <- d.6 %>%
    group_by(sequence) %>%
    sample_n(psPerSequence, replace = F)
  
  # make into tidy data
  d.8 <- d.7 %>%
    gather(trialNum, response, p1:t8) %>%
    arrange(study, sequence, subid, trialNum)
  
  # return final dataframe
  return(d.8)
  
  # remove superfluous objects
  rm(d.4, i)
}

# ready in data for qualtrics studies
# ... study 1
d1 = tidyFormat(rawDataFilename = "b2b-k_study1_us-adults_mean.csv", 
                psPerSequence = 10, 
                study = "1", 
                country = "us", 
                ageGroup = "adults",
                framing = "does that mean...?")

# ... study 1'
d1p = tidyFormat(rawDataFilename = "b2b-k_study1'_us-adults_mean_arousal.csv", 
                psPerSequence = 9, 
                study = "1prime", 
                country = "us", 
                ageGroup = "adults",
                framing = "does that mean...?")

# ... study 3
d3 = tidyFormat(rawDataFilename = "b2b-k_study3_indian-adults_mean.csv", 
                psPerSequence = 10, 
                study = "1", 
                country = "india", 
                ageGroup = "adults",
                framing = "does that mean...?")

# ... study 4a
d4a = tidyFormat(rawDataFilename = "b2b-k_study4a_us-adults_think.csv", 
                psPerSequence = 10, 
                study = "4", 
                country = "us", 
                ageGroup = "adults",
                framing = "do you think...?")

# ... study 4b
d4b = tidyFormat(rawDataFilename = "b2b-k_study4b_indian-adults_think.csv", 
                 psPerSequence = 10, 
                 study = "4", 
                 country = "india", 
                 ageGroup = "adults",
                 framing = "do you think...?")
