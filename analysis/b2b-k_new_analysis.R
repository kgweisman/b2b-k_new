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
sum_sampleSize <- d %>% distinct(subid) %>% group_by(study, country) %>% summarise(n = length(subid))
# print(sum_sampleSize)

# sequence assignment by study
sum_sequenceAssign <- d %>% distinct(subid) %>% group_by(study, country, sequence) %>% summarise(n = length(subid))
# print(sum_sequenceAssign)

# duration by study (minutes; adults only)
sum_duration <- d %>% distinct(subid) %>% group_by(study, country) %>% summarise(m = mean(durationOfTest), sd = sd(durationOfTest))
# print(sum_duration)

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

# set up table of all fact-question pairings (16)
pairsTable <- data_frame(
  factCat = sort(rep(levels(d$factCat), 4)),
  questionCat = rep(levels(d$questionCat), 4)) %>%
  mutate(factCat = factor(factCat),
         questionCat = factor(questionCat),
         pair = factor(paste(factCat, questionCat, sep = "_")))

# set up comparisons to neutral (default)
contrastNeutral <- contrasts(pairsTable$pair)

# set up planned orthogonal contrasts
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

# add pair variable to dataset
d <- d %>%
  full_join(pairsTable)

# --- STUDY 1 -----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# comparison to neutral
contrasts(d$pair) <- contrastNeutral
r1.neut <- lmer(response ~ -1 + pair + (1 | subid), 
                subset(d, phase == "test" & study == "1"))
summary(r1.neut)

# orthogonal contrasts
contrasts(d$pair, how.many = 10) <- contrastOrthogonal
r1.orth <- lmer(response ~ pair + (1 | subid), 
                subset(d, phase == "test" & study == "1"))
summary(r1.orth)

# ------ exploratory analyses -------------------------------------------------

# orthogonal contrasts on absolute values
contrasts(d$pair, how.many = 10) <- contrastOrthogonal
r1.orthAbs <- lmer(abs(response) ~ pair + (1 | subid), 
                   subset(d, phase == "test" & study == "1"))
summary(r1.orthAbs)

# --- STUDY 1' ----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# comparison to neutral
contrasts(d$pair) <- contrastNeutral
r1prime.neut <- lmer(response ~ -1 + pair + (1 | subid), 
                     subset(d, phase == "test" & study == "1prime"))
summary(r1prime.neut)

# orthogonal contrasts
contrasts(d$pair, how.many = 10) <- contrastOrthogonal
r1prime.orth <- lmer(response ~ pair + (1 | subid), 
                     subset(d, phase == "test" & study == "1prime"))
summary(r1prime.orth)

# --- STUDY 2 -----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# comparison to neutral
contrasts(d$pair) <- contrastNeutral
r2.neut <- lmer(response ~ -1 + pair + (1 | subid), 
                subset(d, phase == "test" & study == "2"))
summary(r2.neut)

# orthogonal contrasts
contrasts(d$pair, how.many = 10) <- contrastOrthogonal
r2.orth <- lmer(response ~ pair + (1 | subid), 
                subset(d, phase == "test" & study == "2"))
summary(r2.orth)

# ------ exploratory analyses -------------------------------------------------

# orthogonal contrasts on absolute values
contrasts(d$pair, how.many = 10) <- contrastOrthogonal
r2.orthAbs <- lmer(abs(response) ~ pair + (1 | subid), 
                   subset(d, phase == "test" & study == "2"))
summary(r2.orthAbs)

# adult/child comparison: orthogonal contrasts
contrasts(d$pair, how.many = 10) <- contrastOrthogonal
r2.orthAgeGrpSimp <- lmer(response ~ pair + (1 | subid), 
                          subset(d, (phase == "test" & study == "1") | 
                                   (phase == "test" & study == "2")))
r2.orthAgeGrpAdd <- lmer(response ~ pair + ageGroup + (1 | subid), 
                         subset(d, (phase == "test" & study == "1") | 
                                  (phase == "test" & study == "2")))
r2.orthAgeGrpInt <- lmer(response ~ pair * ageGroup + (1 | subid), 
                         subset(d, (phase == "test" & study == "1") | 
                                  (phase == "test" & study == "2")))
anova(r2.orthAgeGrpSimp, r2.orthAgeGrpAdd, r2.orthAgeGrpInt)
anova(r2.orthAgeGrpSimp, r2.orthAgeGrpInt)
summary(r2.orthAgeGrpInt)

# race/ethnicity comparison: chi-squared & t-tests tests
# ... for age
r2.tAge <- t.test(age ~ raceEthn2, var.equal = T, 
                  subset(d, phase == "test" & study == "2")); r2.tAge

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
r2.neutREadd <- lmer(response ~ pair + raceEthn2 + (1 | subid), 
                     subset(d, phase == "test" & study == "2"))
r2.neutREint <- lmer(response ~ pair * raceEthn2 + (1 | subid), 
                     subset(d, phase == "test" & study == "2"))
anova(r2.neut, r2.neutREadd, r2.neutREint)
anova(r2.neut, r2.neutREint)
summary(r2.neutREint)

# race/ethnicity comparison: orthogonal contrasts
contrasts(d$pair, how.many = 10) <- contrastOrthogonal
r2.orthREadd <- lmer(response ~ pair + raceEthn2 + (1 | subid), 
                     subset(d, phase == "test" & study == "2"))
r2.orthREint <- lmer(response ~ pair * raceEthn2 + (1 | subid), 
                     subset(d, phase == "test" & study == "2"))
anova(r2.orth, r2.orthREadd, r2.orthREint)
anova(r2.orth, r2.orthREint)
summary(r2.orthREint)

# --- STUDY 3 -----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# comparison to neutral
contrasts(d$pair) <- contrastNeutral
r3.neut <- lmer(response ~ -1 + pair + (1 | subid), 
                subset(d, phase == "test" & study == "3"))
summary(r3.neut)

# orthogonal contrasts
contrasts(d$pair, how.many = 10) <- contrastOrthogonal
r3.orth <- lmer(response ~ pair + (1 | subid), 
                subset(d, phase == "test" & study == "3"))
summary(r3.orth)

# ------ exploratory analyses -------------------------------------------------

# orthogonal contrasts on absolute values
contrasts(d$pair, how.many = 10) <- contrastOrthogonal
r3.orthAbs <- lmer(abs(response) ~ pair + (1 | subid), 
                   subset(d, phase == "test" & study == "3"))
summary(r3.orthAbs)

# us/india comparison: orthogonal contrasts
contrasts(d$pair, how.many = 10) <- contrastOrthogonal
r3.orthCountrySimp <- lmer(response ~ pair + (1 | subid), 
                          subset(d, (phase == "test" & study == "1") | 
                                   (phase == "test" & study == "3")))
r3.orthCountryAdd <- lmer(response ~ pair + country + (1 | subid), 
                         subset(d, (phase == "test" & study == "1") | 
                                  (phase == "test" & study == "3")))
r3.orthCountryInt <- lmer(response ~ pair * country + (1 | subid), 
                         subset(d, (phase == "test" & study == "1") | 
                                  (phase == "test" & study == "3")))
anova(r3.orthCountrySimp, r3.orthCountryAdd, r3.orthCountryInt)
anova(r3.orthCountrySimp, r3.orthCountryInt)
summary(r3.orthCountryInt)

# --- STUDY 4 -----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# us/india comparison: comparison to neutral
contrasts(d$pair) <- contrastNeutral
r4.neutCountrySimp <- lmer(response ~ pair + (1 | subid), 
                           subset(d, phase == "test" & study == "4"))
r4.neutCountryAdd <- lmer(response ~ pair + country + (1 | subid), 
                          subset(d, phase == "test" & study == "4"))
r4.neutCountryInt <- lmer(response ~ pair * country + (1 | subid), 
                          subset(d, phase == "test" & study == "4"))
anova(r4.neutCountrySimp, r4.neutCountryAdd, r4.neutCountryInt)
anova(r4.neutCountrySimp, r4.neutCountryInt)
summary(r4.neutCountryInt)

# us/india comparison: orthogonal contrasts
contrasts(d$pair, how.many = 10) <- contrastOrthogonal
r4.orthCountrySimp <- lmer(response ~ pair + (1 | subid), 
                           subset(d, phase == "test" & study == "4"))
r4.orthCountryAdd <- lmer(response ~ pair + country + (1 | subid), 
                          subset(d, phase == "test" & study == "4"))
r4.orthCountryInt <- lmer(response ~ pair * country + (1 | subid), 
                          subset(d, phase == "test" & study == "4"))
anova(r4.orthCountrySimp, r4.orthCountryAdd, r4.orthCountryInt)
anova(r4.orthCountrySimp, r4.orthCountryInt)
summary(r4.orthCountryInt)

# us/india comparison: orthogonal contrasts on absolute values
contrasts(d$pair, how.many = 10) <- contrastOrthogonal
r4.orthAbsCountrySimp <- lmer(abs(response) ~ pair + (1 | subid), 
                           subset(d, phase == "test" & study == "4"))
r4.orthAbsCountryAdd <- lmer(abs(response) ~ pair + country + (1 | subid), 
                          subset(d, phase == "test" & study == "4"))
r4.orthAbsCountryInt <- lmer(abs(response) ~ pair * country + (1 | subid), 
                          subset(d, phase == "test" & study == "4"))
anova(r4.orthAbsCountrySimp, r4.orthAbsCountryAdd, r4.orthAbsCountryInt)
anova(r4.orthAbsCountrySimp, r4.orthAbsCountryInt)
summary(r4.orthAbsCountryInt)

# ------ exploratory analyses -------------------------------------------------

# us/india comparison, framing comparison: orthogonal contrasts
contrasts(d$pair, how.many = 10) <- contrastOrthogonal
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

# us/india comparison, framing comparison: orthogonal contrasts on absolute values
contrasts(d$pair, how.many = 10) <- contrastOrthogonal
r4.orthAbsCntryFrmSimp <- lmer(abs(response) ~ pair * country 
                               + (1 | subid), 
                            subset(d, phase == "test" & 
                                     study != "2" & study != "1prime"))
r4.orthAbsCntryFrmAdd <- lmer(abs(response) ~ pair * country + framing 
                              + (1 | subid), 
                           subset(d, phase == "test" 
                                  & study != "2" & study != "1prime"))
r4.orthAbsCntryFrmInt1 <- lmer(abs(response) ~ pair * country + 
                              framing + country:framing 
                              + (1 | subid), 
                            subset(d, phase == "test" & 
                                     study != "2" & study != "1prime"))
r4.orthAbsCntryFrmInt2 <- lmer(abs(response) ~ pair * country * framing 
                               + (1 | subid), 
                            subset(d, phase == "test" & 
                                     study != "2" & study != "1prime"))
anova(r4.orthAbsCntryFrmSimp, r4.orthAbsCntryFrmAdd, 
      r4.orthAbsCntryFrmInt1, r4.orthAbsCntryFrmInt2)
anova(r4.orthAbsCntryFrmSimp, r4.orthAbsCntryFrmInt2)
summary(r4.orthAbsCntryFrmSimp)