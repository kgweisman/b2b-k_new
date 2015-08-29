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

# make funtion to print out min and max for beta and t by trial type
minMaxSumReg <- function(regSummary, trialType = "all") {
  if (trialType == "sentient-only") {
    range <- c(1:3, 5:7, 9:11)
  } else if (trialType == "inanimate") {
    range <- c(4, 8, 12, 13:16)
  } else if (trialType == "sentient-to-inanimate") {
    range <- c(4, 8, 12)
  } else if (trialType == "inanimate-to-sentient") {
    range <- c(13:15)
  } else {
    range <- c(1:16)
  }
  
  # sentient-only trials
  betaMin <- min(summary(regSummary)$coefficients[range, 1])
  betaMax <- max(summary(regSummary)$coefficients[range, 1])
  tMin <- min(summary(regSummary)$coefficients[range, 3])
  tMax <- max(summary(regSummary)$coefficients[range, 3])
  
  # print out table
  table <- cbind("beta" = c(betaMin, betaMax),
                 "t" = c(tMin, tMax))
  row.names(table) = c("min", "max")
  return(round(table, 2))
}

# make function to print out means by trial type
meansPrint <- function(studyNum, countryName = "us", contrast) {
  temp <- d %>% filter(study == studyNum &
                         country == countryName &
                         phase == "test")
  sent <- temp %>% filter(grepl("phy", pair) == F)
  inan <- temp %>% filter(grepl("phy", pair) == T)
  within <- sent %>% filter(factCat == questionCat)
  between <- sent %>% filter(factCat != questionCat)
  snt_f_aff <- sent %>% filter(factCat == "aff")
  snt_f_others <- sent %>% filter(factCat != "aff")
  snt_f_aut <- sent %>% filter(factCat == "aut")
  snt_f_per <- sent %>% filter(factCat == "per")
  snt_q_aff <- sent %>% filter(questionCat == "aff")
  snt_q_others <- sent %>% filter(questionCat != "aff")
  snt_q_aut <- sent %>% filter(questionCat == "aut")
  snt_q_per <- sent %>% filter(questionCat == "per")
  inan_f_aff <- inan %>% filter(factCat == "aff")
  inan_f_others <- inan %>% filter(factCat != "aff")
  inan_f_aut <- inan %>% filter(factCat == "aut")
  inan_f_per <- inan %>% filter(factCat == "per")
  inan_q_aff <- inan %>% filter(questionCat == "aff")
  inan_q_others <- inan %>% filter(questionCat != "aff")
  inan_q_aut <- inan %>% filter(questionCat == "aut")
  inan_q_per <- inan %>% filter(questionCat == "per")
  inan_phyphy <- inan %>% filter(factCat == questionCat)
  inan_others <- inan %>% filter(factCat != questionCat)
  
  if (contrast == "sent.inanim") {
    c1 <- sent
    c2 <- inan
  } else if (contrast == "snt_within.between") {
    c1 <- within
    c2 <- between
  } else if (contrast == "snt_f_aff.othrs") {
    c1 <- snt_f_aff
    c2 <- snt_f_others
  } else if (contrast == "snt_f_aut.per") {
    c1 <- snt_f_aut
    c2 <- snt_f_per
  } else if (contrast == "snt_q_aff.othrs") {
    c1 <- snt_q_aut
    c2 <- snt_q_others
  } else if (contrast == "snt_q_auth.per") {
    c1 <- snt_q_aut
    c2 <- snt_q_per
  } else if (contrast == "inan_f_aff.othrs") {
    c1 <- inan_f_aff
    c2 <- inan_f_others
  } else if (contrast == "inan_f_aut.per") {
    c1 <- inan_f_aut
    c2 <- inan_f_per
  } else if (contrast == "inan_q_aff.othrs") {
    c1 <- inan_q_aff
    c2 <- inan_q_others
  } else if (contrast == "inan_q_aut.per") {
    c1 <- inan_q_aut
    c2 <- inan_q_per
  } else if (contrast == "inan_phyphy.othrs") {
    c1 <- inan_phyphy
    c2 <- inan_others
  }
  
  c1m <- with(c1, mean(response, na.rm = T))
  c1sd <- with(c1, sd(response, na.rm = T))
  c2m <- with(c2, mean(response, na.rm = T))
  c2sd <- with(c2, sd(response, na.rm = T))
  
  # print out table
  table <- rbind("c1" = c(c1m, c1sd),
                 "c2" = c(c2m, c2sd))
  colnames(table) = c("mean", "sd")
  return(list("contrast" = contrast, "summaryStats" = round(table, 2)))
}

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

# set up table of all fact-question pairings (16)
pairsTable <- data_frame(
  factCat = sort(rep(levels(d$factCat), 4)),
  questionCat = rep(levels(d$questionCat), 4)) %>%
  mutate(factCat = factor(factCat),
         questionCat = factor(questionCat),
         pair = factor(paste(factCat, questionCat, sep = "_")))

# add pair variable to dataset
# add pair and sentient vs. inanimate variables to dataset
d <- d %>%
  full_join(pairsTable) %>%
  # make us the base country and "does that mean...?" the base framing
  mutate(country = factor(country, levels = c("us", "india")),
         framing = factor(framing, levels = c("does that mean...?",
                                              "do you think...?")),
         sentInan = factor(ifelse(phase == "test",
                                  ifelse(grepl("phy", pair),
                                         "inanimate",
                                         "sentient-only"),
                                  NA)))

# glimpse(d)

# --- DEMOGRAPHICS ------------------------------------------------------------

# sample size by study
sum_sampleSize <- d %>% distinct(subid) %>% group_by(study, country) %>% summarise(n = length(subid))
# print(sum_sampleSize)

# sequence assignment by study
sum_sequenceAssign <- d %>% distinct(subid) %>% group_by(study, country, sequence) %>% summarise(n = length(subid))
# print(sum_sequenceAssign)

# duration by study (minutes; adults only)
# BROKEN FOR STUDY 4 US ADULTS! probably need to multiply by 60...
dDuration <- d %>%
  filter(durationOfTest != "NA") %>%
  mutate(durationOfTest = ifelse(study == "4" & country == "us",
                                 durationOfTest * 60,
                                 durationOfTest))

# get 95% CIs on duration
# library(langcog)
# # ... by country
# multi_boot.data.frame(dDuration, summary_function = "mean", column = "durationOfTest", summary_groups = c("study"), statistics_functions = c("ci_lower", "mean", "ci_upper"))
# # ... overall
# multi_boot.data.frame(dDuration, summary_function = "mean", column = "durationOfTest", statistics_functions = c("ci_lower", "mean", "ci_upper"))

# gender by study
sum_gender <- d %>% distinct(subid) %>% group_by(study, country, gender) %>% summarise(n = length(gender))
# print(sum_gender)

# age by study (years; children only)
sum_age <- d %>% distinct(subid) %>% group_by(study, country) %>% summarise(m = mean(age), sd = sd(age), min = min(age), max = max(age))
# print(sum_age)

# race/ethnicity by study
# ... finest grain
sum_raceEthn4 <- d %>% distinct(subid) %>% group_by(study, country, raceEthn4) %>% summarise(n = length(raceEthn4))
# print(sum_raceEthn4)
# ... white-only/some-eastern/other
sum_raceEthn3 <- d %>% distinct(subid) %>% group_by(study, country, raceEthn3) %>% summarise(n = length(raceEthn3))
# print(sum_raceEthn3)
# ... coarsest grain
sum_raceEthn2 <- d %>% distinct(subid) %>% group_by(study, country, raceEthn2) %>% summarise(n = length(raceEthn2))
# print(sum_raceEthn2)

# religion by study (study 4 only)
sum_religion <- d %>% distinct(subid) %>% group_by(study, country, religion) %>% summarise(n = length(religion))
# print(sum_religion)

# --- CONTRASTS ---------------------------------------------------------------

# set up comparisons to neutral (default)
contrastNeutral <- contrasts(pairsTable$pair)

# set up planned orthogonal contrasts: [affect] vs. [perception + autonomy]
contrastOrthogonal <- pairsTable %>%
  mutate(sent.inanim  = ifelse(factCat != "phy" & questionCat != "phy", 7, -9),
         snt_within = ifelse(sent.inanim > 0 & factCat == questionCat, 2,
                             ifelse(sent.inanim > 0, -1, 0)),
         snt_f_aff.othrs = ifelse(sent.inanim > 0 & factCat == "aff", 2,
                                  ifelse(sent.inanim > 0, -1, 0)),
         snt_f_aut.per = ifelse(sent.inanim > 0 & factCat == "aut", 1,
                                ifelse(sent.inanim > 0 & factCat == "per", 
                                       -1, 0)),
         snt_q_aff.othrs = ifelse(sent.inanim > 0 & questionCat == "aff", 2,
                                  ifelse(sent.inanim > 0, -1, 0)),
         snt_q_aut.per = ifelse(sent.inanim > 0 & questionCat == "aut", 1,
                                ifelse(sent.inanim > 0 & questionCat == "per", 
                                       -1, 0)),
         inan_phyphy.othrs = ifelse(sent.inanim < 0 &
                                      factCat == questionCat, 6,
                                    ifelse(sent.inanim < 0, -1, 0)),
         inan_f_aff.othrs = ifelse(sent.inanim < 0 & factCat == "aff", 2,
                                   ifelse(sent.inanim < 0 & factCat != "phy", 
                                          -1, 0)),
         inan_f_aut.per = ifelse(sent.inanim < 0 & factCat == "aut", 1,
                                 ifelse(sent.inanim < 0 & factCat == "per", 
                                        -1, 0)),
         inan_q_aff.othrs = ifelse(sent.inanim < 0 & 
                                     factCat == "phy" & questionCat == "aff", 2,
                                   ifelse(sent.inanim < 0 & 
                                            factCat == "phy" & questionCat != "phy", 
                                          -1, 0)),
         inan_q_aut.per = ifelse(sent.inanim < 0 & 
                                   factCat == "phy" & questionCat == "aut", 1,
                                 ifelse(sent.inanim < 0 & 
                                          factCat == "phy" & questionCat == "per", 
                                        -1, 0))) %>%
  select(-factCat, -questionCat)

contrastOrthogonal <- contrastOrthogonal[-1]
row.names(contrastOrthogonal) = pairsTable$pair

# # test orthogonality
# # ... for each contrast, do weights sum to 0?
# colSums(contrastOrthogonal) == 0
# 
# # ... are all dot products 0?
# numdots = sum(1:(length(contrastOrthogonal)))
# dotprods = NULL
# for(k in 1:numdots){
#   for(i in 1:(length(contrastOrthogonal)-1)){
#     for(j in (i+1):length(contrastOrthogonal)){
#       dotprods[k] = sum(contrastOrthogonal[i] * contrastOrthogonal[j])    
#     }
#   }
# }
# dotprods == 0

contrastOrthogonal <- as.matrix(contrastOrthogonal)

# set up planned orthogonal contrasts: [affect + perception] vs. [autonomy]
contrastOrthogonal2 <- pairsTable %>%
  mutate(sent.inanim  = ifelse(factCat != "phy" & questionCat != "phy", 7, -9),
         snt_within = ifelse(sent.inanim > 0 & factCat == questionCat, 2,
                             ifelse(sent.inanim > 0, -1, 0)),
         snt_f_othrs.aut = ifelse(sent.inanim > 0 & factCat == "aut", -2,
                                  ifelse(sent.inanim > 0, 1, 0)),
         snt_f_aff.per = ifelse(sent.inanim > 0 & factCat == "aff", 1,
                                ifelse(sent.inanim > 0 & factCat == "per", 
                                       -1, 0)),
         snt_q_othrs.aut = ifelse(sent.inanim > 0 & questionCat == "aut", -2,
                                  ifelse(sent.inanim > 0, 1, 0)),
         snt_q_aff.per = ifelse(sent.inanim > 0 & questionCat == "aff", 1,
                                ifelse(sent.inanim > 0 & questionCat == "per", 
                                       -1, 0)),
         inan_phyphy.othrs = ifelse(sent.inanim < 0 &
                                      factCat == questionCat, 6,
                                    ifelse(sent.inanim < 0, -1, 0)),
         inan_f_othrs.aut = ifelse(sent.inanim < 0 & factCat == "aut", -2,
                                   ifelse(sent.inanim < 0 & factCat != "phy", 
                                          1, 0)),
         inan_f_aff.per = ifelse(sent.inanim < 0 & factCat == "aff", 1,
                                 ifelse(sent.inanim < 0 & factCat == "per", 
                                        -1, 0)),
         inan_q_othrs.aut = ifelse(sent.inanim < 0 & 
                                     factCat == "phy" & questionCat == "aut", -2,
                                   ifelse(sent.inanim < 0 & 
                                            factCat == "phy" & questionCat != "phy", 
                                          1, 0)),
         inan_q_aff.per = ifelse(sent.inanim < 0 & 
                                   factCat == "phy" & questionCat == "aff", 1,
                                 ifelse(sent.inanim < 0 & 
                                          factCat == "phy" & questionCat == "per", 
                                        -1, 0))) %>%
  select(-factCat, -questionCat)

contrastOrthogonal2 <- contrastOrthogonal2[-1]
row.names(contrastOrthogonal2) = pairsTable$pair

# # test orthogonality
# # ... for each contrast, do weights sum to 0?
# colSums(contrastOrthogonal2) == 0
# 
# # ... are all dot products 0?
# numdots = sum(1:(length(contrastOrthogonal2)))
# dotprods = NULL
# for(k in 1:numdots){
#   for(i in 1:(length(contrastOrthogonal2)-1)){
#     for(j in (i+1):length(contrastOrthogonal2)){
#       dotprods[k] = sum(contrastOrthogonal2[i] * contrastOrthogonal2[j])    
#     }
#   }
# }
# dotprods == 0

contrastOrthogonal2 <- as.matrix(contrastOrthogonal2)

# effect-coding of demographic variables (UGM = unweighted grand mean)
contrasts(d$ageGroup) <- cbind("children_UGM" = 
                                 c(-1, 1)) # adults = -1, children = 1
contrasts(d$gender) <- cbind("male_UGM" = 
                               c(-1, 1)) # female = -1, male = 1
contrasts(d$raceEthn2) <- cbind("ofColor_UGM" = 
                                  c(1, -1)) # white = -1, of-color = 1
contrasts(d$country) <- cbind("india_UGM" = 
                                c(-1, 1)) # us = -1, india = 1
contrasts(d$framing) <- cbind("opinion_UGM" = 
                                c(-1, 1)) # logical = -1, opinion = 1

# --- HISTOGRAMS AND COUNTS ---------------------------------------------------

# study 1: us adults
d1 <- d %>% filter(study == "1" & phase == "test")
g1a <- qplot(data = d1, response) + 
  facet_grid(factCat ~ questionCat) +
  ggtitle("Study 1: US Adults")
g1b <- qplot(data = d1, response) + 
  facet_grid(sentInan ~ .) +
  ggtitle("Study 1: US Adults")

d1 %>%
  group_by(pair) %>%
  summarise(mode = mfv(response))

# study 2: us children
d2 <- d %>% filter(study == "2" & phase == "test")
g2a <- qplot(data = d2, response) + 
  facet_grid(factCat ~ questionCat) +
  ggtitle("Study 2: Children")
g2b <- qplot(data = d2, response) + 
  facet_grid(sentInan ~ .) +
  ggtitle("Study 2: Children")

d2 %>%
  group_by(pair) %>%
  summarise(mode = mfv(response))

# study 2: white children
d2w <- d %>% filter(study == "2" & phase == "test" & raceEthn2 == "white")
g2wa <- qplot(data = d2w, response) + 
  facet_grid(factCat ~ questionCat) +
  ggtitle("Study 2: White children")
g2wb <- qplot(data = d2w, response) + 
  facet_grid(sentInan ~ .) +
  ggtitle("Study 2: White children")

d2w %>%
  group_by(pair) %>%
  summarise(mode = mfv(response))

# study 2: children of color
d2cc <- d %>% filter(study == "2" & phase == "test" & raceEthn2 == "of-color")
g2cca <- qplot(data = d2cc, response) + 
  facet_grid(factCat ~ questionCat) +
  ggtitle("Study 2: Children of color")
g2ccb <- qplot(data = d2cc, response) + 
  facet_grid(sentInan ~ .) +
  ggtitle("Study 2: Children of color")

d2cc %>%
  group_by(pair) %>%
  summarise(mode = mfv(response))

# study 3: indian adults
d3 <- d %>% filter(study == "3" & phase == "test")
g3a <- qplot(data = d3, response) + 
  facet_grid(factCat ~ questionCat) +
  ggtitle("Study 3: Indian Adults")
g3b <- qplot(data = d3, response) + 
  facet_grid(sentInan ~ .) +
  ggtitle("Study 3: Indian Adults")

d3 %>%
  group_by(pair) %>%
  summarise(mode = mfv(response))

# study 4: us adults
d4us <- d %>% filter(study == "4" & phase == "test" & country == "us")
g4usa <- qplot(data = d4us, response) + 
  facet_grid(factCat ~ questionCat) +
  ggtitle("Study 4: US adults")
g4usb <- qplot(data = d4us, response) + 
  facet_grid(sentInan ~ .) +
  ggtitle("Study 4: US adults")

d4us %>%
  group_by(pair) %>%
  summarise(mode = mfv(response))

# study 4: indian adults
d4india <- d %>% filter(study == "4" & phase == "test" & country == "india")
g4indiaa <- qplot(data = d4india, response) + 
  facet_grid(factCat ~ questionCat) +
  ggtitle("Study 4: Indian adults")
g4indiab <- qplot(data = d4india, response) + 
  facet_grid(sentInan ~ .) +
  ggtitle("Study 4: Indian adults")

d4india %>%
  group_by(pair) %>%
  summarise(mode = mfv(response))

grid.arrange(g1a, g2a, g3a, ncol = 1)
grid.arrange(g2wa, g2cca, ncol = 1)
grid.arrange(g4usa, g4indiaa, ncol = 1)

grid.arrange(g1b, g2b, g3b, ncol = 1)
grid.arrange(g2wb, g2ccb, ncol = 1)
grid.arrange(g4usb, g4indiab, ncol = 1)