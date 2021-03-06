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
# - EXPERIMENTAL SETTING: mturk/qualtrics
# - COUNTRY: us
# - AGE GROUP: adults
# - FRAMING: "does that mean...?" 
# - ITEM SET: cb1 (affect: positive/negative valence)

# study 1' (2014-05-23) - SUPPLEMENTAL
# - EXPERIMENTAL SETTING: mturk/qualtrics
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
# - EXPERIMENTAL SETTING: mturk/qualtrics
# - COUNTRY: india
# - AGE GROUP: adults
# - FRAMING: "does that mean...?" 
# - ITEM SET: cb1 (affect: positive/negative valence)

# study 4a (2014-06-25)
# - EXPERIMENTAL SETTING: mturk/qualtrics
# - COUNTRY: us
# - AGE GROUP: adults
# - FRAMING: "do you think...?" 
# - ITEM SET: cb1 (affect: positive/negative valence)

# study 4b (2014-06-25)
# - EXPERIMENTAL SETTING: mturk/qualtrics
# - COUNTRY: india
# - AGE GROUP: adults
# - FRAMING: "do you think...?" 
# - ITEM SET: cb1 (affect: positive/negative valence)

# --- READING IN RAW DATA -----------------------------------------------------

# make function for formatting qualtrics data
# NOTE: requires packages to be installed and loaded (see PRELIMINARIES, above)
tidyFormat <- function(rawDataFilename, study, itemSet, 
                       country, ageGroup, framing) {

  # set study specifications
  study <- study
  itemSet <- itemSet
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
    select(-ResponseSet, -Name, -ExternalDataReference, -EmailAddress, -IPAddress,
           -Status, -mTurkCODE, -ends_with("text"))
  
  # separate by sequence
  d.4 <- data.frame()
  for (i in 1:8) {
    temp <- subset(d.3, Sequence == i)
    temp[temp == ""] <- NA
    temp <- Filter(function(x)!all(is.na(x)), temp)
    ifelse("gender_response" %in% names(temp),
           temp <- temp %>% mutate(gender = gender_response),
           temp <- temp %>% mutate(gender = NA))
    ifelse("religion_response" %in% names(temp),
           temp <- temp %>% mutate(religion = religion_response),
           temp <- temp %>% mutate(religion = NA))
    ifelse("repeat_response_response" %in% names(temp),
           temp <- temp %>% mutate(repeated = repeat_response_response),
           temp <- temp %>% mutate(repeated = NA))
    ifelse("comments" %in% names(temp),
           temp <- temp %>% mutate(comments = comments),
           temp <- temp %>% mutate(comments = NA))
    temp <- temp %>% 
      select(ResponseID, StartDate, EndDate, Finished, Sequence, 
             starts_with("practice"), starts_with("Q"),
             comments, gender, religion, repeated)
    names(temp) <- c(names(d.3[1:5]),
                    "p1", "p2", "p3", # practice trials
                    "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", # test trials
                    "comments", "gender", "religion", "repeated") # self-reports
    d.4 <- bind_rows(d.4, temp)
    rm(temp, i)
  }
  
  # recode variables
  d.5 <- d.4 %>%
    mutate(study = factor(study),
           itemSet = factor(itemSet),
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
           gender = factor(gender),
           religion = factor(religion),
           repeated = factor(repeated),
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
    select(study, itemSet, country, ageGroup, framing, 
           subid, dateOfTest, durationOfTest, status,
           gender, religion, repeated, comments,
           sequence, p1:t8)
  
  # remove participants who did not complete session
  d.6 <- subset(d.5, status == "complete") %>% select(-status)
    
  # make into tidy data
  d.7 <- d.6 %>%
    gather(trialNum, response, p1:t8) %>%
    mutate(phase = factor(
      ifelse(grepl("p", trialNum) == T, "practice",
             ifelse(grepl("t", trialNum) == T, "test",
                          NA)))) %>%
    select(study:sequence, phase, trialNum:response) %>%
    arrange(study, sequence, subid, trialNum)
  
  # return final dataframe
  return(d.7)
  
  # remove superfluous objects
  rm(d.4, i)
}

# ready in data for qualtrics studies
# ... study 1
d1 <- tidyFormat(rawDataFilename = "b2b-k_study1_us-adults_mean.csv", 
                study = "1", 
                itemSet = "cb1",
                country = "us", 
                ageGroup = "adults",
                framing = "does that mean...?")

# ... study 1'
d1p <- tidyFormat(rawDataFilename = "b2b-k_study1'_us-adults_mean_arousal.csv", 
                study = "1prime", 
                itemSet = "cb2",
                country = "us", 
                ageGroup = "adults",
                framing = "does that mean...?")

# ... study 3
d3 <- tidyFormat(rawDataFilename = "b2b-k_study3_indian-adults_mean.csv", 
                study = "3", 
                itemSet = "cb1",
                country = "india", 
                ageGroup = "adults",
                framing = "does that mean...?")

# ... study 4a
d4a <- tidyFormat(rawDataFilename = "b2b-k_study4a_us-adults_think.csv", 
                study = "4", 
                itemSet = "cb1",
                country = "us", 
                ageGroup = "adults",
                framing = "do you think...?")

# ... study 4b
d4b <- tidyFormat(rawDataFilename = "b2b-k_study4b_indian-adults_think.csv", 
                 study = "4", 
                 itemSet = "cb1",
                 country = "india", 
                 ageGroup = "adults",
                 framing = "do you think...?")

# read in child data
d2.raw <- read.csv("./data/raw/b2b-k_study2_us-children_mean.csv")

d2.0 <- d2.raw %>% # exclude participants by hand: see justification below
  filter(Subj != "S11" # did not speak english
         & Subj != "S13" # did not finish
         & Subj != "S31" # too young (4.2 years old)
         & Subj != "C16" # did not finish
         & Subj != "C19" # too young (4.2 years old)
  )
  
d2 <- d2.0 %>%
  mutate(study = factor("2"),
         itemSet = factor("cb1"),
         country = factor("us"),
         ageGroup = factor("children"),
         age = as.numeric(Age),
         framing = factor("does that mean...?"),
         experimenter = ifelse(grepl("C", Subj) == T, "exp2", # C Tran
                               ifelse(grepl("S", Subj) == T, "exp1", # K Weisman
                                      NA)),
         subid = factor(Subj),
#          dateOfTest = NA, # to prevent deduction of DOB
         gender = factor(Gen, levels = c("M", "F"), 
                         labels = c(1:2)),
         raceEthn = factor(Race),
         sequence = factor(gsub("s", "", Sequence)),
         phase = factor(Phase),
         trialNum = tolower(Trial),
         response = as.numeric(X4ptResponse - 2.5), # center around 0
         ynResponse = factor(tolower(YNResponse)),
         trialComments = as.character(Scomments)) %>% 
  select(study, itemSet, country, ageGroup, framing, experimenter,
         subid, age, gender, raceEthn, sequence, 
         phase, trialNum, ynResponse, response, trialComments) %>%
  arrange(sequence, trialNum, subid)

# join all together!
d0 <- full_join(d1, d1p) %>% 
  full_join(d2) %>%
  full_join(d3) %>% 
  full_join(d4a) %>% 
  full_join(d4b) %>%
  mutate(study = factor(study),
         itemSet = factor(itemSet),
         country = factor(country),
         framing = factor(framing),
         subid = factor(subid),
         gender = factor(gender, levels = c(1:3),
                         labels = c("male", "female", "other/skip")),
         religion = factor(religion, levels = c(1:9),
                           labels = c("buddhism", "christianity", "hinduism", 
                                      "islam", "jainism", "judaism", "sikhism",
                                      "other", "non-religious")),
         repeated = factor(repeated, levels = c(1:3),
                           labels = c("no", "yes", "unsure")), # self-reported
         raceEthn = factor(raceEthn),         
         raceEthn2 = factor(
           ifelse(raceEthn == "C" | raceEthn == "S", "white",
                  ifelse(raceEthn == "unknown" | raceEthn == "NA", NA, 
                         "of-color"))),
         raceEthn3 = factor(
           ifelse(raceEthn == "A" | raceEthn == "AAME" | raceEthn == "AH" |
                    raceEthn == "AI" | raceEthn == "CA" | raceEthn == "CCA" |
                    raceEthn == "CHA" | raceEthn == "CHI" | raceEthn == "CME" |
                    raceEthn == "I" | raceEthn == "IH" | raceEthn == "ME" |
                    raceEthn == "MEI", "some-asian",
                  ifelse(raceEthn == "C" | raceEthn == "S", "white-only",
                         ifelse(raceEthn == "unknown" | raceEthn == "NA", NA,
                                "other-nonwhite")))),
         raceEthn4 = factor(
           ifelse(
             raceEthn == "A", 
             "east-asian",
             ifelse(raceEthn == "AA", 
                    "african-american",
                    ifelse(raceEthn == "C" | raceEthn == "S", 
                           "white", # S for "Swedish"
                           ifelse(raceEthn == "H", 
                                  "hispanic-latino",
                                  ifelse(raceEthn == "I", 
                                         "indian",
                                         ifelse(raceEthn == "ME", 
                                                "middle-eastern",
                                                ifelse(raceEthn == "unknown", 
                                                       "unknown",
                                                       ifelse(raceEthn == "NA", 
                                                              NA,
                                                              "multiracial"))))))))),
         sequence = factor(sequence),
         experimenter = factor(experimenter),
         trialNum = factor(trialNum),
         ynResponse = factor(
           ifelse(is.na(ynResponse) == T,
                  ifelse(response < 0, "n", 
                         ifelse(response > 0, "y", NA)),
                  ynResponse))) %>%
  arrange(study, country, sequence, subid, trialNum)

# --- ADDING STIMULUS INFO (BY SEQUENCE) --------------------------------------

# load counterbalancing tables
# ... studies 1, 2, 3, 4
cb1 <- read.csv("./design/cb1.csv", colClasses = "factor") %>%
  mutate(itemSet = "cb1")

# ... study 1'
cb2 <- read.csv("./design/cb2.csv", colClasses = "factor") %>%
  mutate(itemSet = "cb2")

# join counterbalancing tables together, separate out category and subcategory
cb <- full_join(cb1, cb2) %>% 
  mutate(factSub = factor(factSub),
         factText = factor(factText),
         questionSub = factor(questionSub),
         questionText = factor(questionText),
         itemSet = factor(itemSet),
         phase = factor(phase),
         trialNum = factor(trialNum)) %>% # recode trials as numeric
  select(sequence, itemSet, phase, trialNum, 
         factCat, factSub, factText, 
         questionCat, questionSub, questionText)

# add counterbalancing info to dataset
d <- full_join(d0, cb) %>% 
  mutate(itemSet = factor(itemSet),
         sequence = factor(sequence),
         trial = factor(trialNum),
         trialNum = as.numeric(trialNum)) %>%
  select(study, country, ageGroup, framing, itemSet, # study info
         subid, dateOfTest, durationOfTest, # session info
         age, gender, raceEthn, raceEthn2, raceEthn3, raceEthn4, # p info
         religion, repeated, comments, # p info, continued
         experimenter, sequence, phase, trialNum, # trial info
         factCat, factSub, factText, # trial info, continued
         questionCat, questionSub, questionText, # trial info, continued
         ynResponse, response, trialComments) %>% # p responses
  arrange(study, country, sequence, subid, trialNum)

# check sequence assignment by study
checkTable <- d %>% 
  group_by(study, country, sequence) %>% 
  select(subid) %>% unique() %>% 
  summarise(count = length(subid))
# View(checkTable)

# --- WRITING ANONYMIZED CSV --------------------------------------------------

# write data to de-identified csv file
write.csv(d, "./data/anonymized/b2b-k_adults-data_anonymized.csv")

d = read.csv("./data/anonymized/b2b-k_adults-data_anonymized.csv")[-1] # delete observation numbers

