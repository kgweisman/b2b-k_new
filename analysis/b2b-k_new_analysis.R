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
# ... by country
multi_boot.data.frame(dDuration, summary_function = "mean", column = "durationOfTest", summary_groups = c("study", "country"), statistics_functions = c("ci_lower", "mean", "ci_upper"))
# ... overall
multi_boot.data.frame(dDuration, summary_function = "mean", column = "durationOfTest", summary_groups = c("study"), statistics_functions = c("ci_lower", "mean", "ci_upper"))

# gender by study
sum_gender <- d %>% distinct(subid) %>% group_by(study, country, gender) %>% summarise(n = length(gender))
# print(sum_gender)

# age by study (years; children only)
sum_age <- d %>% distinct(subid) %>% group_by(study, country) %>% summarise(m = mean(age, na.rm = T), sd = sd(age, na.rm = T), min = min(age), max = max(age))
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

# --- STUDY 1 -----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# comparison to neutral
contrasts(d$pair) <- contrastNeutral
r1.neut <- lmer(response ~ -1 + pair + (1 | subid), 
                subset(d, phase == "test" & study == "1"))
summary(r1.neut)

minMaxSumReg(r1.neut, "sentient-only")
minMaxSumReg(r1.neut, "sentient-to-inanimate")
minMaxSumReg(r1.neut, "inanimate-to-sentient")

# orthogonal contrasts: [affect] vs. [autonomy + perception]
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r1.orth <- lmer(response ~ pair + (1 | subid), 
                subset(d, phase == "test" & study == "1"))
summary(r1.orth)

meansPrint("1", contrast = "sent.inanim")
meansPrint("1", contrast = "snt_within.between")
meansPrint("1", contrast = "snt_f_aff.othrs")
meansPrint("1", contrast = "snt_f_aut.per")
meansPrint("1", contrast = "snt_q_aff.othrs")
meansPrint("1", contrast = "snt_q_auth.per")
meansPrint("1", contrast = "inan_f_aff.othrs")
meansPrint("1", contrast = "inan_f_aut.per")
meansPrint("1", contrast = "inan_q_aff.othrs")
meansPrint("1", contrast = "inan_q_aut.per")
meansPrint("1", contrast = "inan_phyphy.othrs")

round(summary(r1.orth)$coefficients,2)

# STANDARDIZED orthogonal contrasts: [affect] vs. [autonomy + perception]
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r1.orthSTD <- lmer(scale(response) ~ pair + (1 | subid), 
                subset(d, phase == "test" & study == "1"))
summary(r1.orthSTD)
round(summary(r1.orthSTD)$coefficients,2)

# affect-affect
with(d %>% filter(study == "1" & country == "us" & phase == "test") %>%
       filter(factCat == "aff" & questionCat == "aff"), 
     mean(response, na.rm = T))
with(d %>% filter(study == "1" & country == "us" & phase == "test") %>%
       filter(factCat == "aff" & questionCat == "aff"), 
     sd(response, na.rm = T))

# ------ exploratory analyses -------------------------------------------------

# # orthogonal contrasts: on binary values
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# r1.orthBin <- glmer(ynResponse ~ pair + (1 | subid),
#                    subset(d, phase == "test" & study == "1"), 
#                    family = "binomial")
# summary(r1.orthBin)

# --- STUDY 1' ----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# comparison to neutral
contrasts(d$pair) <- contrastNeutral
r1prime.neut <- lmer(response ~ -1 + pair + (1 | subid), 
                     subset(d, phase == "test" & study == "1prime"))
summary(r1prime.neut)

minMaxSumReg(r1prime.neut, "sentient-only")
minMaxSumReg(r1prime.neut, "sentient-to-inanimate")
minMaxSumReg(r1prime.neut, "inanimate-to-sentient")

# orthogonal contrasts: [affect] vs. [autonomy + perception]
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r1prime.orth <- lmer(response ~ pair + (1 | subid), 
                     subset(d, phase == "test" & study == "1prime"))
summary(r1prime.orth)

meansPrint("1prime", contrast = "sent.inanim")
meansPrint("1prime", contrast = "snt_within.between")
meansPrint("1prime", contrast = "snt_f_aff.othrs")
meansPrint("1prime", contrast = "snt_f_aut.per")
meansPrint("1prime", contrast = "snt_q_aff.othrs")
meansPrint("1prime", contrast = "snt_q_auth.per")
meansPrint("1prime", contrast = "inan_f_aff.othrs")
meansPrint("1prime", contrast = "inan_f_aut.per")
meansPrint("1prime", contrast = "inan_q_aff.othrs")
meansPrint("1prime", contrast = "inan_q_aut.per")

# affect-affect
with(d %>% filter(study == "1prime" & country == "us" & phase == "test") %>%
       filter(factCat == "aff" & questionCat == "aff"), 
     mean(response, na.rm = T))
with(d %>% filter(study == "1prime" & country == "us" & phase == "test") %>%
       filter(factCat == "aff" & questionCat == "aff"), 
     sd(response, na.rm = T))

# --- STUDY 2 -----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# comparison to neutral
contrasts(d$pair) <- contrastNeutral
r2.neut <- lmer(response ~ -1 + pair + gender + scale(age, scale = F) + (1 | subid), 
                subset(d, phase == "test" & study == "2"))
summary(r2.neut)

minMaxSumReg(r2.neut, "sentient-only")
minMaxSumReg(r2.neut, "inanimate")

# orthogonal contrasts: [affect] vs. [autonomy + perception]
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r2.orth <- lmer(response ~ pair + gender + scale(age, scale = F) + (1 | subid), 
                subset(d, phase == "test" & study == "2"))
summary(r2.orth)

meansPrint("2", contrast = "sent.inanim")
meansPrint("2", contrast = "snt_within.between")
meansPrint("2", contrast = "snt_f_aff.othrs")
meansPrint("2", contrast = "snt_f_aut.per")
meansPrint("2", contrast = "snt_q_aff.othrs")
meansPrint("2", contrast = "snt_q_auth.per")
meansPrint("2", contrast = "inan_f_aff.othrs")
meansPrint("2", contrast = "inan_f_aut.per")
meansPrint("2", contrast = "inan_q_aff.othrs")
meansPrint("2", contrast = "inan_q_aut.per")

round(summary(r2.orth)$coefficients,2)

# STANDARDIZED orthogonal contrasts: [affect] vs. [autonomy + perception]
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r2.orthSTD <- lmer(scale(response) ~ pair + gender + scale(age) + (1 | subid), 
                   subset(d, phase == "test" & study == "2"))
summary(r2.orthSTD)
round(summary(r2.orthSTD)$coefficients,2)

# affect-affect
with(d %>% filter(study == "2" & country == "us" & phase == "test") %>%
       filter(factCat == "aff" & questionCat == "aff"), 
     mean(response, na.rm = T))
with(d %>% filter(study == "2" & country == "us" & phase == "test") %>%
       filter(factCat == "aff" & questionCat == "aff"), 
     sd(response, na.rm = T))

# ------ exploratory analyses -------------------------------------------------

# # orthogonal contrasts: on binary values
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# r2.orthBin <- glmer(ynResponse ~ pair + gender + scale(age, scale = F) + (1 | subid),
#                    subset(d, phase == "test" & study == "2"),
#                    family = "binomial")
# summary(r2.orthBin)

# race/ethnicity comparison: chi-squared & t-tests tests
# ... for age
d.age <- d %>% 
  filter(study == "2") %>%
  select(subid, age, raceEthn2) %>%
  distinct()

r2.tAge <- t.test(age ~ raceEthn2, var.equal = T, d.age); r2.tAge

# ... for gender
r2.tableGender <- with(d %>% 
                         filter(phase == "test" & study == "2") %>% 
                         select(subid, gender, raceEthn2) %>% 
                         distinct(subid), 
                       table(gender, raceEthn2)); r2.tableGender
r2.chisqGender <- summary(r2.tableGender); r2.chisqGender

# ... for sequence assignment
r2.tableSequence <- with(d %>% 
                           filter(phase == "test" & study == "2") %>% 
                           select(subid, sequence, raceEthn2) %>% 
                           distinct(subid), 
                         table(sequence, raceEthn2)); r2.tableSequence
r2.chisqSequence <- summary(r2.tableSequence); r2.chisqSequence

# race/ethnicity comparison: comparison to neutral
contrasts(d$pair) <- contrastNeutral
r2.neutREsimp <- lmer(response ~ pair + gender + scale(age, scale = F) + (1 | subid), 
                      data = subset(d, phase == "test" & 
                                      study == "2" & 
                                      raceEthn2 != "NA"))
r2.neutREadd <- lmer(response ~ pair + raceEthn2 + gender + scale(age, scale = F) + (1 | subid), 
                     data = subset(d, phase == "test" & 
                                     study == "2" & 
                                     raceEthn2 != "NA"))
r2.neutREint <- lmer(response ~ pair * raceEthn2 + gender + scale(age, scale = F) + (1 | subid), 
                     data = subset(d, phase == "test" & 
                                     study == "2" & 
                                     raceEthn2 != "NA"))
anova(r2.neutREsimp, r2.neutREadd, r2.neutREint)
anova(r2.neutREsimp, r2.neutREint)
summary(r2.neutREint)

# race/ethnicity comparison: orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r2.orthREsimp <- lmer(response ~ pair + gender + scale(age, scale = F) + (1 | subid), 
                      data = subset(d, phase == "test" & 
                                      study == "2" & 
                                      raceEthn2 != "NA"))
r2.orthREadd <- lmer(response ~ pair + raceEthn2 + gender + scale(age, scale = F) + (1 | subid), 
                     data = subset(d, phase == "test" & 
                                     study == "2" & 
                                     raceEthn2 != "NA"))
r2.orthREint <- lmer(response ~ pair * raceEthn2 + gender + scale(age, scale = F) + (1 | subid), 
                     data = subset(d, phase == "test" & 
                                     study == "2" & 
                                     raceEthn2 != "NA"))
anova(r2.orthREsimp, r2.orthREadd, r2.orthREint)
anova(r2.orthREsimp, r2.orthREint)
summary(r2.orthREint)

round(summary(r2.orthREint)$coefficients,2)

# STANDARDIZED orthogonal contrasts: [affect] vs. [autonomy + perception]
r2.orthREintSTD <- lmer(scale(response) ~ pair * raceEthn2 + gender + scale(age) + (1 | subid), 
                        data = subset(d, phase == "test" & 
                                        study == "2" & 
                                        raceEthn2 != "NA"))
summary(r2.orthREintSTD)
round(summary(r2.orthREintSTD)$coefficients,2)

# # race/ethnicity comparison: orthogonal contrasts on binary values
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# r2.orthBinREadd <- glmer(ynResponse ~ pair + raceEthn2 + gender + scale(age, scale = F) + (1 | subid), 
#                      subset(d, phase == "test" & study == "2"),
#                      family = "binomial")
# r2.orthBinREint <- glmer(ynResponse ~ pair * raceEthn2 + gender + scale(age, scale = F) + (1 | subid), 
#                      subset(d, phase == "test" & study == "2"),
#                      family = "binomial")
# anova(r2.orth, r2.orthBinREadd, r2.orthBinREint)
# anova(r2.orth, r2.orthBinREint)
# summary(r2.orthBinREint)

# more interactions!
# three-way interactions with gender and age
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r2.orth2way <- lmer(response ~ pair * gender * scale(age, scale = F) 
                    + (1 | subid), 
                    subset(d, phase == "test" & study == "2"))
summary(r2.orth2way)
round(summary(r2.orth2way)$coefficients, 2)

# STANDARDIZED three-way interactions with gender and age
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r2.orth2waySTD <- lmer(scale(response) ~ pair * gender * scale(age) 
                       + (1 | subid), 
                       subset(d, phase == "test" & study == "2"))
summary(r2.orth2waySTD)
round(summary(r2.orth2waySTD)$coefficients, 2)

# four-way interactions with gender, age, and raceEthn2
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r2.orth3way <- lmer(response ~ pair * gender * scale(age, scale = F) 
                    * raceEthn2 
                    + (1 | subid), 
                    subset(d, phase == "test" & study == "2"))
summary(r2.orth3way)
round(summary(r2.orth3way)$coefficients, 2)


# adult/child comparison: orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal

devo <- d %>%
  filter(study %in% c("1", "2")) %>%
  mutate(group = factor(ifelse(study == "1", "adults", as.character(raceEthn2)))) %>%
  filter(!is.na(group))
contrasts(devo$pair, how.many = 11) <- contrastOrthogonal
# contrasts(devo$pair, how.many = 11) <- contrastNeutral
contrasts(devo$group) <- cbind("COC_adults" = c(0, 1, 0), "W_adults" = c(0, 0, 1))

devo.orthSimp <- lmer(response ~ pair + (1 | subid),
                      subset(devo, phase == "test" & (study == "1" | study == "2")))
devo.orthAddAge <- lmer(response ~ pair + group + (1 | subid),
                        subset(devo, phase == "test" & (study == "1" | study == "2")))
devo.orthIntAge <- lmer(response ~ pair * group + (1 | subid),
                        subset(devo, phase == "test" & (study == "1" | study == "2")))

anova(devo.orthSimp, devo.orthAddAge, devo.orthIntAge)
summary(devo.orthIntAge)

round(summary(devo.orthIntAge)$coefficients,2)


# # adult/child comparison: orthogonal contrasts
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# r2.orthAgeGrpSimp <- lmer(response ~ pair + (1 | subid), 
#                           subset(d, phase == "test" & 
#                                    (study == "1" | study == "2")))
# r2.orthAgeGrpAdd <- lmer(response ~ pair + ageGroup + (1 | subid), 
#                          subset(d, phase == "test" & 
#                                   (study == "1" | study == "2")))
# r2.orthAgeGrpInt <- lmer(response ~ pair * ageGroup + (1 | subid), 
#                          subset(d, phase == "test" & 
#                                   (study == "1" | study == "2")))
# anova(r2.orthAgeGrpSimp, r2.orthAgeGrpAdd, r2.orthAgeGrpInt)
# anova(r2.orthAgeGrpSimp, r2.orthAgeGrpInt)
# summary(r2.orthAgeGrpInt)
# 
# round(summary(r2.orthAgeGrpInt)$coefficients,2)
# 
# # STANDARDIZED orthogonal contrasts: [affect] vs. [autonomy + perception]
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# r2.orthAgeGrpIntSTD <- lmer(scale(response) ~ pair * ageGroup + (1 | subid), 
#                             subset(d, phase == "test" & 
#                                      (study == "1" | study == "2")))
# summary(r2.orthAgeGrpIntSTD)
# round(summary(r2.orthAgeGrpIntSTD)$coefficients,2)
# 
# # # adult/child comparison: orthogonal contrasts on binary values
# # contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# # r2.orthBinAgeGrpSimp <- glmer(ynResponse ~ pair + (1 | subid), 
# #                           subset(d, phase == "test" & 
# #                                    (study == "1" | study == "2")),
# #                           family = "binomial")
# # r2.orthBinAgeGrpAdd <- glmer(ynResponse ~ pair + ageGroup + (1 | subid), 
# #                              subset(d, phase == "test" & 
# #                                       (study == "1" | study == "2")),
# #                              family = "binomial")
# # r2.orthBinAgeGrpInt <- glmer(ynResponse ~ pair * ageGroup + (1 | subid),
# #                              subset(d, phase == "test" & 
# #                                       (study == "1" | study == "2")),
# #                              family = "binomial")
# # anova(r2.orthBinAgeGrpSimp, r2.orthBinAgeGrpAdd, r2.orthBinAgeGrpInt)
# # anova(r2.orthBinAgeGrpSimp, r2.orthBinAgeGrpInt)
# # summary(r2.orthBinAgeGrpInt)


# --- STUDY 3 -----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# comparison to neutral
contrasts(d$pair) <- contrastNeutral
r3.neut <- lmer(response ~ -1 + pair + (1 | subid), 
                subset(d, phase == "test" & study == "3"))
summary(r3.neut)

minMaxSumReg(r3.neut, "sentient-only")
minMaxSumReg(r3.neut, "inanimate")

# orthogonal contrasts: [affect] vs. [autonomy + perception]
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r3.orth <- lmer(response ~ pair + (1 | subid), 
                subset(d, phase == "test" & study == "3"))
summary(r3.orth)

meansPrint("3", countryName = "india", contrast = "sent.inanim")
meansPrint("3", countryName = "india", contrast = "snt_within.between")
meansPrint("3", countryName = "india", contrast = "snt_f_aff.othrs")
meansPrint("3", countryName = "india", contrast = "snt_f_aut.per")
meansPrint("3", countryName = "india", contrast = "snt_q_aff.othrs")
meansPrint("3", countryName = "india", contrast = "snt_q_auth.per")
meansPrint("3", countryName = "india", contrast = "inan_f_aff.othrs")
meansPrint("3", countryName = "india", contrast = "inan_f_aut.per")
meansPrint("3", countryName = "india", contrast = "inan_q_aff.othrs")
meansPrint("3", countryName = "india", contrast = "inan_q_aut.per")

round(summary(r3.orth)$coefficients,2)


# STANDARDIZED orthogonal contrasts: [affect] vs. [autonomy + perception]
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r3.orthSTD <- lmer(scale(response) ~ pair + (1 | subid), 
                   subset(d, phase == "test" & study == "3"))
summary(r3.orthSTD)
round(summary(r3.orthSTD)$coefficients,2)

# affect-affect
with(d %>% filter(study == "3" & country == "india" & phase == "test") %>%
       filter(factCat == "aff" & questionCat == "aff"), 
     mean(response, na.rm = T))
with(d %>% filter(study == "3" & country == "india" & phase == "test") %>%
       filter(factCat == "aff" & questionCat == "aff"), 
     sd(response, na.rm = T))

# ------ exploratory analyses -------------------------------------------------

# # orthogonal contrasts: on binary values
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# r3.orthBin <- glmer(ynResponse ~ pair + (1 | subid), 
#                 subset(d, phase == "test" & study == "3"),
#                 family = "binomial")
# summary(r3.orthBin)

# us/india comparison: orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r3.orthCountrySimp <- lmer(response ~ pair + (1 | subid), 
                          subset(d, (phase == "test") & 
                                   (study == "1" | study == "3")))
r3.orthCountryAdd <- lmer(response ~ pair + country + (1 | subid), 
                          subset(d, (phase == "test") & 
                                   (study == "1" | study == "3")))
r3.orthCountryInt <- lmer(response ~ pair * country + (1 | subid), 
                          subset(d, (phase == "test") & 
                                   (study == "1" | study == "3")))
anova(r3.orthCountrySimp, r3.orthCountryAdd, r3.orthCountryInt)
anova(r3.orthCountrySimp, r3.orthCountryInt)
summary(r3.orthCountryInt)

round(summary(r3.orthCountryInt)$coefficients,2)

# STANDARDIZED orthogonal contrasts: [affect] vs. [autonomy + perception]
r3.orthCountryInt <- lmer(scale(response) ~ pair * country + (1 | subid), 
                        data = subset(d, phase == "test" & 
                                        study == "1" | study == "3"))
summary(r3.orthCountryInt)
round(summary(r3.orthCountryInt)$coefficients,2)

# # us/india comparison: orthogonal contrasts on binary values
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# r3.orthBinCountrySimp <- glmer(ynResponse ~ pair + (1 | subid), 
#                                subset(d, (phase == "test") & 
#                                         (study == "1" | study == "3")),
#                                family = "binomial")
# r3.orthBinCountryAdd <- glmer(ynResponse ~ pair + country + (1 | subid),
#                               subset(d, (phase == "test") &
#                                        (study == "1" | study == "3")),
#                               family = "binomial")
# r3.orthBinCountryInt <- glmer(ynResponse ~ pair * country + (1 | subid),
#                               subset(d, (phase == "test") &
#                                        (study == "1" | study == "3")),
#                               family = "binomial")
# anova(r3.orthBinCountrySimp, r3.orthBinCountryAdd, r3.orthBinCountryInt)
# anova(r3.orthBinCountrySimp, r3.orthBinCountryInt)
# summary(r3.orthBinCountryInt)

# --- STUDY 4 -----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# us/india comparison: comparison to neutral
contrasts(d$pair) <- contrastNeutral
r4.neutCountrySimp <- lmer(response ~ -1 + pair + (1 | subid), 
                           subset(d, phase == "test" & study == "4"))
r4.neutCountryAdd <- lmer(response ~ -1 + pair + country + (1 | subid), 
                          subset(d, phase == "test" & study == "4"))
r4.neutCountryInt <- lmer(response ~ -1 + pair * country + (1 | subid), 
                          subset(d, phase == "test" & study == "4"))
anova(r4.neutCountrySimp, r4.neutCountryAdd, r4.neutCountryInt)
anova(r4.neutCountrySimp, r4.neutCountryInt)
summary(r4.neutCountryInt)

minMaxSumReg(r4.neutCountryInt, "sentient-only")
minMaxSumReg(r4.neutCountryInt, "inanimate")

# hand-make minMax for interactions
interactions <- summary(r4.neutCountryInt)$coefficients[17:32,c(1,3)] # include countryindia_UGM for aff-aff comparison
betaMin <- c("sent" = min(interactions[c(1:3, 5:7, 9:11), 1]),
             "inan" = min(interactions[c(4, 8, 12:16), 1]))
betaMax <- c("sent" = max(interactions[c(1:3, 5:7, 9:11), 1]),
             "inan" = max(interactions[c(4, 8, 12:16), 1]))
tMin <- c("sent" = min(interactions[c(1:3, 5:7, 9:11), 2]),
          "inan" = min(interactions[c(4, 8, 12:16), 2]))
tMax <- c("sent" = max(interactions[c(1:3, 5:7, 9:11), 2]),
          "inan" = max(interactions[c(4, 8, 12:16), 2]))
minMaxTable <- cbind("beta" = c(betaMin, betaMax),
               "t" = c(tMin, tMax))
row.names(minMaxTable) = c("sent_min", "inan_min",
                           "sent_max", "inan_max")
round(minMaxTable, 2)

# us/india comparison: orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r4.orthCountrySimp <- lmer(response ~ pair + (1 | subid), 
                           subset(d, phase == "test" & study == "4"))
r4.orthCountryAdd <- lmer(response ~ pair + country + (1 | subid), 
                          subset(d, phase == "test" & study == "4"))
r4.orthCountryInt <- lmer(response ~ pair * country + (1 | subid), 
                          subset(d, phase == "test" & study == "4"))
anova(r4.orthCountrySimp, r4.orthCountryAdd, r4.orthCountryInt)
anova(r4.orthCountrySimp, r4.orthCountryInt)
summary(r4.orthCountryInt)

round(summary(r4.orthCountryInt)$coefficients,2)

# STANDARDIZED orthogonal contrasts: [affect] vs. [autonomy + perception]
r4.orthCountryIntSTD <- lmer(scale(response) ~ pair * country + (1 | subid), 
                          data = subset(d, phase == "test" & 
                                          study == "4"))
summary(r4.orthCountryIntSTD)
round(summary(r4.orthCountryIntSTD)$coefficients,2)

meansPrint("4", contrast = "sent.inanim")
meansPrint("4", contrast = "snt_within.between")
# meansPrint("4", contrast = "snt_f_aff.othrs")
# meansPrint("4", contrast = "snt_f_aut.per")
# meansPrint("4", contrast = "snt_q_aff.othrs")
# meansPrint("4", contrast = "snt_q_auth.per")
# meansPrint("4", contrast = "inan_f_aff.othrs")
# meansPrint("4", contrast = "inan_f_aut.per")
# meansPrint("4", contrast = "inan_q_aff.othrs")
# meansPrint("4", contrast = "inan_q_aut.per")
# 
# # affect-affect
# with(d %>% filter(study == "4" & country == "us" & phase == "test") %>%
#        filter(factCat == "aff" & questionCat == "aff"), 
#      mean(response, na.rm = T))
# with(d %>% filter(study == "4" & country == "us" & phase == "test") %>%
#        filter(factCat == "aff" & questionCat == "aff"), 
#      sd(response, na.rm = T))

# ------ exploratory analyses -------------------------------------------------

# us/india comparison, framing comparison: orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
r4.orthCntryFrmSimp <- lmer(response ~ pair * country + (1 | subid), 
                           subset(d, phase == "test" & 
                                    study != "2" & study != "1prime"))
r4.orthCntryFrmAdd <- lmer(response ~ pair * country + framing + (1 | subid), 
                           subset(d, phase == "test" & 
                                    study != "2" & study != "1prime"))
r4.orthCntryFrmInt1 <- lmer(response ~ pair * country + 
                              framing + country:framing + (1 | subid), 
                            subset(d, phase == "test" & 
                                     study != "2" & study != "1prime"))
r4.orthCntryFrmInt2 <- lmer(response ~ pair * country * framing + (1 | subid), 
                            subset(d, phase == "test" & 
                                     study != "2" & study != "1prime"))
anova(r4.orthCntryFrmSimp, r4.orthCntryFrmAdd, 
      r4.orthCntryFrmInt1, r4.orthCntryFrmInt2)
anova(r4.orthCntryFrmSimp, r4.orthCntryFrmInt2)
summary(r4.orthCntryFrmInt2)

round(summary(r4.orthCntryFrmInt2)$coefficients, 2)

# STANDARDIZED us/india comparison, framing comparison: orthogonal contrasts
r4.orthCntryFrmInt2STD <- lmer(scale(response) ~ pair * country * framing + (1 | subid), 
                            subset(d, phase == "test" & 
                                     study != "2" & study != "1prime"))
summary(r4.orthCntryFrmInt2STD)
round(summary(r4.orthCntryFrmInt2STD)$coefficients, 2)


# # write regression results table to csv
# temp <- round(summary(r4.orthCntryFrmInt2)$coefficients, 2)
# write.csv(temp, "./study4_exploratory-regression.csv")