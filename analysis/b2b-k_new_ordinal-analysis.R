# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(stats)
library(stringr)
library(lubridate)
library(ordinal)

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
  
  c1m <- with(c1, mean(responseF, na.rm = T))
  c1sd <- with(c1, sd(responseF, na.rm = T))
  c2m <- with(c2, mean(responseF, na.rm = T))
  c2sd <- with(c2, sd(responseF, na.rm = T))
  
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

# make responseF a factor
d <- d %>%
  mutate(responseF = factor(response, 
                            labels = c("really no", "maybe no", 
                                       "maybe yes", "really yes")))

# glimpse(d)

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

# add pair variable to dataset
d <- d %>%
  full_join(pairsTable)

# --- STUDY 1 -----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# comparison to neutral
contrasts(d$pair) <- contrastNeutral
or1.neut <- clmm(responseF ~ -1 + pair + (1 | subid), 
                subset(d, phase == "test" & study == "1"))
summary(or1.neut)

minMaxSumReg(or1.neut, "sentient-only")
minMaxSumReg(or1.neut, "sentient-to-inanimate")
minMaxSumReg(or1.neut, "inanimate-to-sentient")

# orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
or1.orth <- clmm(responseF ~ pair + (1 | subid), 
                subset(d, phase == "test" & study == "1"))
summary(or1.orth)

# meansPrint("1", contrast = "sent.inanim")
# meansPrint("1", contrast = "snt_within.between")
# meansPrint("1", contrast = "snt_f_aff.othrs")
# meansPrint("1", contrast = "snt_f_aut.per")
# meansPrint("1", contrast = "snt_q_aff.othrs")
# meansPrint("1", contrast = "snt_q_auth.per")
# meansPrint("1", contrast = "inan_f_aff.othrs")
# meansPrint("1", contrast = "inan_f_aut.per")
# meansPrint("1", contrast = "inan_q_aff.othrs")
# meansPrint("1", contrast = "inan_q_aut.per")
# meansPrint("1", contrast = "inan_phyphy.othrs")

round(summary(or1.orth)$coefficients,2)

# # affect-affect
# with(d %>% filter(study == "1" & country == "us" & phase == "test") %>%
#        filter(factCat == "aff" & questionCat == "aff"), 
#      mean(responseF, na.rm = T))
# with(d %>% filter(study == "1" & country == "us" & phase == "test") %>%
#        filter(factCat == "aff" & questionCat == "aff"), 
#      sd(responseF, na.rm = T))

# ------ exploratory analyses -------------------------------------------------

# # orthogonal contrasts on absolute values
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# or1.orthAbs <- clmm(abs(responseF) ~ pair + (1 | subid), 
#                    subset(d, phase == "test" & study == "1"))
# summary(or1.orthAbs)

# # orthogonal contrasts on binary values
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# or1.orthBin <- gclmm(ynresponseF ~ pair + (1 | subid),
#                    subset(d, phase == "test" & study == "1"), 
#                    family = "binomial")
# summary(or1.orthBin)

# --- STUDY 1' ----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# comparison to neutral
contrasts(d$pair) <- contrastNeutral
or1prime.neut <- clmm(responseF ~ -1 + pair + (1 | subid), 
                     subset(d, phase == "test" & study == "1prime"))
summary(or1prime.neut)

minMaxSumReg(or1prime.neut, "sentient-only")
minMaxSumReg(or1prime.neut, "sentient-to-inanimate")
minMaxSumReg(or1prime.neut, "inanimate-to-sentient")

# orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
or1prime.orth <- clmm(responseF ~ pair + (1 | subid), 
                     subset(d, phase == "test" & study == "1prime"))
summary(or1prime.orth)

# meansPrint("1prime", contrast = "sent.inanim")
# meansPrint("1prime", contrast = "snt_within.between")
# meansPrint("1prime", contrast = "snt_f_aff.othrs")
# meansPrint("1prime", contrast = "snt_f_aut.per")
# meansPrint("1prime", contrast = "snt_q_aff.othrs")
# meansPrint("1prime", contrast = "snt_q_auth.per")
# meansPrint("1prime", contrast = "inan_f_aff.othrs")
# meansPrint("1prime", contrast = "inan_f_aut.per")
# meansPrint("1prime", contrast = "inan_q_aff.othrs")
# meansPrint("1prime", contrast = "inan_q_aut.per")
# 
# # affect-affect
# with(d %>% filter(study == "1prime" & country == "us" & phase == "test") %>%
#        filter(factCat == "aff" & questionCat == "aff"), 
#      mean(responseF, na.rm = T))
# with(d %>% filter(study == "1prime" & country == "us" & phase == "test") %>%
#        filter(factCat == "aff" & questionCat == "aff"), 
#      sd(responseF, na.rm = T))

# --- STUDY 2 -----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# comparison to neutral
contrasts(d$pair) <- contrastNeutral
or2.neut <- clmm(responseF ~ -1 + pair + (1 | subid), 
                subset(d, phase == "test" & study == "2"))
summary(or2.neut)

minMaxSumReg(or2.neut, "sentient-only")
minMaxSumReg(or2.neut, "inanimate")

# orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
or2.orth <- clmm(responseF ~ pair + (1 | subid), 
                subset(d, phase == "test" & study == "2"))
summary(or2.orth)

# meansPrint("2", contrast = "sent.inanim")
# meansPrint("2", contrast = "snt_within.between")
# meansPrint("2", contrast = "snt_f_aff.othrs")
# meansPrint("2", contrast = "snt_f_aut.per")
# meansPrint("2", contrast = "snt_q_aff.othrs")
# meansPrint("2", contrast = "snt_q_auth.per")
# meansPrint("2", contrast = "inan_f_aff.othrs")
# meansPrint("2", contrast = "inan_f_aut.per")
# meansPrint("2", contrast = "inan_q_aff.othrs")
# meansPrint("2", contrast = "inan_q_aut.per")

round(summary(or2.orth)$coefficients,2)

# # affect-affect
# with(d %>% filter(study == "2" & country == "us" & phase == "test") %>%
#        filter(factCat == "aff" & questionCat == "aff"), 
#      mean(responseF, na.rm = T))
# with(d %>% filter(study == "2" & country == "us" & phase == "test") %>%
#        filter(factCat == "aff" & questionCat == "aff"), 
#      sd(responseF, na.rm = T))

# ------ exploratory analyses -------------------------------------------------

# # orthogonal contrasts on absolute values
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# or2.orthAbs <- clmm(abs(responseF) ~ pair + (1 | subid), 
#                    subset(d, phase == "test" & study == "2"))
# summary(or2.orthAbs)

# # orthogonal contrasts on binary values
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# or2.orthBin <- gclmm(ynresponseF ~ pair + (1 | subid),
#                    subset(d, phase == "test" & study == "2"),
#                    family = "binomial")
# summary(or2.orthBin)

# adult/child comparison: orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
or2.orthAgeGrpSimp <- clmm(responseF ~ pair + (1 | subid), 
                          subset(d, phase == "test" & 
                                   (study == "1" | study == "2")))
or2.orthAgeGrpAdd <- clmm(responseF ~ pair + ageGroup + (1 | subid), 
                         subset(d, phase == "test" & 
                                  (study == "1" | study == "2")))
or2.orthAgeGrpInt <- clmm(responseF ~ pair * ageGroup + (1 | subid), 
                         subset(d, phase == "test" & 
                                  (study == "1" | study == "2")))
anova(or2.orthAgeGrpSimp, or2.orthAgeGrpAdd, or2.orthAgeGrpInt)
anova(or2.orthAgeGrpSimp, or2.orthAgeGrpInt)
summary(or2.orthAgeGrpInt)

round(summary(or2.orthAgeGrpInt)$coefficients,2)

# # adult/child comparison: orthogonal contrasts on binary values
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# or2.orthBinAgeGrpSimp <- gclmm(ynresponseF ~ pair + (1 | subid), 
#                           subset(d, phase == "test" & 
#                                    (study == "1" | study == "2")),
#                           family = "binomial")
# or2.orthBinAgeGrpAdd <- gclmm(ynresponseF ~ pair + ageGroup + (1 | subid), 
#                              subset(d, phase == "test" & 
#                                       (study == "1" | study == "2")),
#                              family = "binomial")
# or2.orthBinAgeGrpInt <- gclmm(ynresponseF ~ pair * ageGroup + (1 | subid),
#                              subset(d, phase == "test" & 
#                                       (study == "1" | study == "2")),
#                              family = "binomial")
# anova(or2.orthBinAgeGrpSimp, or2.orthBinAgeGrpAdd, or2.orthBinAgeGrpInt)
# anova(or2.orthBinAgeGrpSimp, or2.orthBinAgeGrpInt)
# summary(or2.orthBinAgeGrpInt)

# race/ethnicity comparison: chi-squared & t-tests tests
# ... for age
d.age <- d %>% 
  filter(study == "2") %>%
  select(subid, age, raceEthn2) %>%
  distinct()

or2.tAge <- t.test(age ~ raceEthn2, var.equal = T, d.age); or2.tAge

# ... for gender
or2.tableGender <- with(d %>% 
                         filter(phase == "test" & study == "2") %>% 
                         select(subid, gender, raceEthn2) %>% 
                         distinct(subid), 
                       table(gender, raceEthn2)); or2.tableGender
or2.chisqGender <- summary(or2.tableGender); or2.chisqGender

# ... for sequence assignment
or2.tableSequence <- with(d %>% 
                           filter(phase == "test" & study == "2") %>% 
                           select(subid, sequence, raceEthn2) %>% 
                           distinct(subid), 
                         table(sequence, raceEthn2)); or2.tableSequence
or2.chisqSequence <- summary(or2.tableSequence); or2.chisqSequence

# race/ethnicity comparison: comparison to neutral
contrasts(d$pair) <- contrastNeutral
or2.neutREsimp <- clmm(responseF ~ pair + (1 | subid), 
                      data = subset(d, phase == "test" & 
                                      study == "2" & 
                                      raceEthn2 != "NA"))
or2.neutREadd <- clmm(responseF ~ pair + raceEthn2 + (1 | subid), 
                     data = subset(d, phase == "test" & 
                                     study == "2" & 
                                     raceEthn2 != "NA"))
or2.neutREint <- clmm(responseF ~ pair * raceEthn2 + (1 | subid), 
                     data = subset(d, phase == "test" & 
                                     study == "2" & 
                                     raceEthn2 != "NA"))
anova(or2.neutREsimp, or2.neutREadd, or2.neutREint)
anova(or2.neutREsimp, or2.neutREint)
summary(or2.neutREint)

# race/ethnicity comparison: orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
or2.orthREsimp <- clmm(responseF ~ pair + (1 | subid), 
                      data = subset(d, phase == "test" & 
                                      study == "2" & 
                                      raceEthn2 != "NA"))
or2.orthREadd <- clmm(responseF ~ pair + raceEthn2 + (1 | subid), 
                     data = subset(d, phase == "test" & 
                                     study == "2" & 
                                     raceEthn2 != "NA"))
or2.orthREint <- clmm(responseF ~ pair * raceEthn2 + (1 | subid), 
                     data = subset(d, phase == "test" & 
                                     study == "2" & 
                                     raceEthn2 != "NA"))
anova(or2.orthREsimp, or2.orthREadd, or2.orthREint)
anova(or2.orthREsimp, or2.orthREint)
summary(or2.orthREint)

round(summary(or2.orthREint)$coefficients,2)

# # race/ethnicity comparison: orthogonal contrasts on binary values
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# or2.orthBinREadd <- gclmm(ynresponseF ~ pair + raceEthn2 + (1 | subid), 
#                      subset(d, phase == "test" & study == "2"),
#                      family = "binomial")
# or2.orthBinREint <- gclmm(ynresponseF ~ pair * raceEthn2 + (1 | subid), 
#                      subset(d, phase == "test" & study == "2"),
#                      family = "binomial")
# anova(or2.orth, or2.orthBinREadd, or2.orthBinREint)
# anova(or2.orth, or2.orthBinREint)
# summary(or2.orthBinREint)

# --- STUDY 3 -----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# comparison to neutral
contrasts(d$pair) <- contrastNeutral
or3.neut <- clmm(responseF ~ -1 + pair + (1 | subid), 
                subset(d, phase == "test" & study == "3"))
summary(or3.neut)

minMaxSumReg(or3.neut, "sentient-only")
minMaxSumReg(or3.neut, "inanimate")

# orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
or3.orth <- clmm(responseF ~ pair + (1 | subid), 
                subset(d, phase == "test" & study == "3"))
summary(or3.orth)

# meansPrint("3", countryName = "india", contrast = "sent.inanim")
# meansPrint("3", countryName = "india", contrast = "snt_within.between")
# meansPrint("3", countryName = "india", contrast = "snt_f_aff.othrs")
# meansPrint("3", countryName = "india", contrast = "snt_f_aut.per")
# meansPrint("3", countryName = "india", contrast = "snt_q_aff.othrs")
# meansPrint("3", countryName = "india", contrast = "snt_q_auth.per")
# meansPrint("3", countryName = "india", contrast = "inan_f_aff.othrs")
# meansPrint("3", countryName = "india", contrast = "inan_f_aut.per")
# meansPrint("3", countryName = "india", contrast = "inan_q_aff.othrs")
# meansPrint("3", countryName = "india", contrast = "inan_q_aut.per")

round(summary(or3.orth)$coefficients,2)

# # affect-affect
# with(d %>% filter(study == "3" & country == "india" & phase == "test") %>%
#        filter(factCat == "aff" & questionCat == "aff"), 
#      mean(responseF, na.rm = T))
# with(d %>% filter(study == "3" & country == "india" & phase == "test") %>%
#        filter(factCat == "aff" & questionCat == "aff"), 
#      sd(responseF, na.rm = T))

# ------ exploratory analyses -------------------------------------------------

# # orthogonal contrasts on absolute values
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# or3.orthAbs <- clmm(abs(responseF) ~ pair + (1 | subid), 
#                    subset(d, phase == "test" & study == "3"))
# summary(or3.orthAbs)

# # orthogonal contrasts on binary values
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# or3.orthBin <- gclmm(ynresponseF ~ pair + (1 | subid), 
#                 subset(d, phase == "test" & study == "3"),
#                 family = "binomial")
# summary(or3.orthBin)

# us/india comparison: neutral contrasts
contrasts(d$pair) <- contrastNeutral
or3.neutCountrySimp <- clmm(responseF ~ pair + (1 | subid), 
                           subset(d, (phase == "test") & 
                                    (study == "1" | study == "3")))
or3.neutCountryAdd <- clmm(responseF ~ pair + country + (1 | subid), 
                          subset(d, (phase == "test") & 
                                   (study == "1" | study == "3")))
or3.neutCountryInt <- clmm(responseF ~ pair * country + (1 | subid), 
                          subset(d, (phase == "test") & 
                                   (study == "1" | study == "3")))
anova(or3.neutCountrySimp, or3.neutCountryAdd, or3.neutCountryInt)
anova(or3.neutCountrySimp, or3.neutCountryInt)
summary(or3.neutCountryInt)

# us/india comparison: orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
or3.orthCountrySimp <- clmm(responseF ~ pair + (1 | subid), 
                           subset(d, (phase == "test") & 
                                    (study == "1" | study == "3")))
or3.orthCountryAdd <- clmm(responseF ~ pair + country + (1 | subid), 
                          subset(d, (phase == "test") & 
                                   (study == "1" | study == "3")))
or3.orthCountryInt <- clmm(responseF ~ pair * country + (1 | subid), 
                          subset(d, (phase == "test") & 
                                   (study == "1" | study == "3")))
anova(or3.orthCountrySimp, or3.orthCountryAdd, or3.orthCountryInt)
anova(or3.orthCountrySimp, or3.orthCountryInt)
summary(or3.orthCountryInt)

round(summary(or3.orthCountryInt)$coefficients,2)

# # us/india comparison: orthogonal contrasts on binary values
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# or3.orthBinCountrySimp <- gclmm(ynresponseF ~ pair + (1 | subid), 
#                                subset(d, (phase == "test") & 
#                                         (study == "1" | study == "3")),
#                                family = "binomial")
# or3.orthBinCountryAdd <- gclmm(ynresponseF ~ pair + country + (1 | subid),
#                               subset(d, (phase == "test") &
#                                        (study == "1" | study == "3")),
#                               family = "binomial")
# or3.orthBinCountryInt <- gclmm(ynresponseF ~ pair * country + (1 | subid),
#                               subset(d, (phase == "test") &
#                                        (study == "1" | study == "3")),
#                               family = "binomial")
# anova(or3.orthBinCountrySimp, or3.orthBinCountryAdd, or3.orthBinCountryInt)
# anova(or3.orthBinCountrySimp, or3.orthBinCountryInt)
# summary(or3.orthBinCountryInt)

# --- STUDY 4 -----------------------------------------------------------------

# ------ planned analyses -----------------------------------------------------

# us/india comparison: comparison to neutral
contrasts(d$pair) <- contrastNeutral
or4.neutCountrySimp <- clmm(responseF ~ -1 + pair + (1 | subid), 
                           subset(d, phase == "test" & study == "4"))
or4.neutCountryAdd <- clmm(responseF ~ -1 + pair + country + (1 | subid), 
                          subset(d, phase == "test" & study == "4"))
or4.neutCountryInt <- clmm(responseF ~ -1 + pair * country + (1 | subid), 
                          subset(d, phase == "test" & study == "4"))
anova(or4.neutCountrySimp, or4.neutCountryAdd, or4.neutCountryInt)
anova(or4.neutCountrySimp, or4.neutCountryInt)
summary(or4.neutCountryInt)

# minMaxSumReg(or4.neutCountryInt, "sentient-only")
# minMaxSumReg(or4.neutCountryInt, "inanimate")
# 
# # hand-make minMax for interactions
# interactions <- summary(or4.neutCountryInt)$coefficients[18:32,c(1,3)]
# betaMin <- c(min(interactions[c(1:2, 4:6, 8:10), 1]),
#              min(interactions[c(3, 7, 11:15), 1]))
# betaMax <- c(max(interactions[c(1:2, 4:6, 8:10), 1]),
#              max(interactions[c(3, 7, 11:15), 1]))
# tMin <- c(min(interactions[c(1:2, 4:6, 8:10), 2]),
#           min(interactions[c(3, 7, 11:15), 2]))
# tMax <- c(max(interactions[c(1:2, 4:6, 8:10), 2]),
#           max(interactions[c(3, 7, 11:15), 2]))
# minMaxTable <- cbind("beta" = c(betaMin, betaMax),
#                      "t" = c(tMin, tMax))
# row.names(minMaxTable) = c("sent_min", "inan_min",
#                            "sent_max", "inan_max")
# round(minMaxTable, 2)

# us/india comparison: orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
or4.orthCountrySimp <- clmm(responseF ~ pair + (1 | subid), 
                           subset(d, phase == "test" & study == "4"))
or4.orthCountryAdd <- clmm(responseF ~ pair + country + (1 | subid), 
                          subset(d, phase == "test" & study == "4"))
or4.orthCountryInt <- clmm(responseF ~ pair * country + (1 | subid), 
                          subset(d, phase == "test" & study == "4"))
anova(or4.orthCountrySimp, or4.orthCountryAdd, or4.orthCountryInt)
anova(or4.orthCountrySimp, or4.orthCountryInt)
summary(or4.orthCountryInt)

round(summary(or4.orthCountryInt)$coefficients,2)

# meansPrint("4", contrast = "sent.inanim")
# meansPrint("4", contrast = "snt_within.between")
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
#      mean(responseF, na.rm = T))
# with(d %>% filter(study == "4" & country == "us" & phase == "test") %>%
#        filter(factCat == "aff" & questionCat == "aff"), 
#      sd(responseF, na.rm = T))

# # us/india comparison: orthogonal contrasts on absolute values
# contrasts(d$pair, how.many = 11) <- contrastOrthogonal
# or4.orthAbsCountrySimp <- clmm(abs(responseF) ~ pair + (1 | subid), 
#                               subset(d, phase == "test" & study == "4"))
# or4.orthAbsCountryAdd <- clmm(abs(responseF) ~ pair + country + (1 | subid), 
#                              subset(d, phase == "test" & study == "4"))
# or4.orthAbsCountryInt <- clmm(abs(responseF) ~ pair * country + (1 | subid), 
#                              subset(d, phase == "test" & study == "4"))
# anova(or4.orthAbsCountrySimp, or4.orthAbsCountryAdd, or4.orthAbsCountryInt)
# anova(or4.orthAbsCountrySimp, or4.orthAbsCountryInt)
# summary(or4.orthAbsCountryInt)

# ------ exploratory analyses -------------------------------------------------

# us/india comparison, framing comparison: orthogonal contrasts
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
or4.orthCntryFrmSimp <- clmm(responseF ~ pair * country + (1 | subid), 
                            subset(d, phase == "test" & 
                                     study != "2" & study != "1prime"))
or4.orthCntryFrmAdd <- clmm(responseF ~ pair * country + framing + (1 | subid), 
                           subset(d, phase == "test" & 
                                    study != "2" & study != "1prime"))
or4.orthCntryFrmInt1 <- clmm(responseF ~ pair * country + 
                              framing + country:framing + (1 | subid), 
                            subset(d, phase == "test" & 
                                     study != "2" & study != "1prime"))
or4.orthCntryFrmInt2 <- clmm(responseF ~ pair * country * framing + (1 | subid), 
                            subset(d, phase == "test" & 
                                     study != "2" & study != "1prime"))
anova(or4.orthCntryFrmSimp, or4.orthCntryFrmAdd, 
      or4.orthCntryFrmInt1, or4.orthCntryFrmInt2)
anova(or4.orthCntryFrmSimp, or4.orthCntryFrmInt2)
summary(or4.orthCntryFrmInt2)

round(summary(or4.orthCntryFrmInt2)$coefficients, 2)

# us/india comparison, framing comparison: orthogonal contrasts on absolute values
contrasts(d$pair, how.many = 11) <- contrastOrthogonal
or4.orthAbsCntryFrmSimp <- clmm(abs(responseF) ~ pair * country 
                               + (1 | subid), 
                               subset(d, phase == "test" & 
                                        study != "2" & study != "1prime"))
or4.orthAbsCntryFrmAdd <- clmm(abs(responseF) ~ pair * country + framing 
                              + (1 | subid), 
                              subset(d, phase == "test" 
                                     & study != "2" & study != "1prime"))
or4.orthAbsCntryFrmInt1 <- clmm(abs(responseF) ~ pair * country + 
                                 framing + country:framing 
                               + (1 | subid), 
                               subset(d, phase == "test" & 
                                        study != "2" & study != "1prime"))
or4.orthAbsCntryFrmInt2 <- clmm(abs(responseF) ~ pair * country * framing 
                               + (1 | subid), 
                               subset(d, phase == "test" & 
                                        study != "2" & study != "1prime"))
anova(or4.orthAbsCntryFrmSimp, or4.orthAbsCntryFrmAdd, 
      or4.orthAbsCntryFrmInt1, or4.orthAbsCntryFrmInt2)
anova(or4.orthAbsCntryFrmSimp, or4.orthAbsCntryFrmInt2)
summary(or4.orthAbsCntryFrmSimp)