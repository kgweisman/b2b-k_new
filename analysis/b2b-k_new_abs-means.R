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

# --- [chosen alternative] WITHIN-Ss T-TESTS OF ABS VALUES OF MEANS -----------

# make summary dataframe for mean (and absolute mean) responses for 
# sentient-only vs. inanimate trials, by subject (and demographic grouping vars)
withinSubj <- d %>%
  filter(phase == "test" & study != "1prime") %>%
  mutate(sentInan = factor(sentInan, labels = c("inanimate", "sentientOnly"))) %>%
  group_by(study, country, ageGroup, raceEthn2, subid, sentInan) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  spread(sentInan, mean) %>%
  mutate(diff = inanimate - sentientOnly,
         inanimate_abs = abs(inanimate),
         sentientOnly_abs = abs(sentientOnly),
         diff_abs = inanimate_abs - sentientOnly_abs)
# View(withinSubj)

# study 1: us adults
# ... true values
t1 <- with(subset(withinSubj, study == 1), 
           t.test(sentientOnly, inanimate, paired = T))
# ... absolute values
t1a <- with(subset(withinSubj, study == 1),
            t.test(sentientOnly_abs, inanimate_abs, paired = T))

# study 2: us children
# ... true values
t2 <- with(subset(withinSubj, study == 2), 
           t.test(sentientOnly, inanimate, paired = T))
# ... absolute values
t2a <- with(subset(withinSubj, study == 2),
            t.test(sentientOnly_abs, inanimate_abs, paired = T))

# studies 1 & 2: compare us adults & us children
t2a_ageGroup <- with(subset(withinSubj, study == 1 | study == 2), 
                     t.test(diff_abs ~ ageGroup))

# study 2: compare white children & children of color
t2a_raceEthn2 <- with(subset(withinSubj, study == 1 | study == 2), 
                      t.test(diff_abs ~ raceEthn2))

# study 3: indian adults
# ... true values
t3 <- with(subset(withinSubj, study == 3), 
           t.test(sentientOnly, inanimate, paired = T))
# ... absolute values
t3a <- with(subset(withinSubj, study == 3),
            t.test(sentientOnly_abs, inanimate_abs, paired = T))

# studies 1 & 3: compare us adults & indian adults
t3a_country <- with(subset(withinSubj, study == 1 | study == 3), 
                    t.test(diff_abs ~ country))

# study 4: us adults
# ... true values
t4us <- with(subset(withinSubj, study == 4 & country == "us"), 
           t.test(sentientOnly, inanimate, paired = T))
# ... absolute values
t4usa <- with(subset(withinSubj, study == 4 & country == "us"),
            t.test(sentientOnly_abs, inanimate_abs, paired = T))

# study 4: indian adults
# ... true values
t4india <- with(subset(withinSubj, study == 4 & country == "india"), 
             t.test(sentientOnly, inanimate, paired = T))
# ... absolute values
t4indiaa <- with(subset(withinSubj, study == 4 & country == "india"),
              t.test(sentientOnly_abs, inanimate_abs, paired = T))

# study 4: compare us adults & indian adults
# ... true values
t4_country <- with(subset(withinSubj, study == 4), 
                   t.test(diff ~ country))

# ... true values
t4a_country <- with(subset(withinSubj, study == 4), 
                    t.test(diff_abs ~ country))

# --- [additional option #1] T-TESTS OF ABS VALUES OF MEANS -------------------

# write function to test whether the mean response to inanimate trials (overall) 
# is further from the midpoint (0) the mean response to sentient-only trials
# (overall); NOTE: m1 = inanimate, m2 = sentient-only
compAbsMean <- function(studyNum, var.equal = "test") {
  # make summary table for this study
  tab <- d %>%
    filter(phase == "test" & study == studyNum) %>%
    group_by(sentInan) %>%
    summarise(mean = mean(response, na.rm = T),
              absMean = abs(mean),
              sd = sd(response, na.rm = T),
              n = length(response))
  
  # get means, sds, and ns for sentient-only vs. inanimate trials
  m1 <- tab$absMean[1]; m2 <- tab$absMean[2]
  s1 <- tab$sd[1]; s2 <- tab$sd[2]; 
  n1 <- tab$n[1]; n2 <- tab$n[2]
  
  # test for equal variances
  varTest <- with(d %>% filter(phase == "test" & study == studyNum), 
                  var.test(response ~ sentInan))
  if(var.equal == "test") {
    var.equal <- ifelse(varTest$p.value < 0.10, F, T)
  }
  
  # calculate test statistics
  if(var.equal == F) {
    s.pool <- sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2)/(n1 + n2 - 2))
    t <- (m1 - m2) / sqrt(s1^2/n1 + s2^2/n2)
    df <- (s1^2/n1 + s2^2/n2)^2 / ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
    p <- pt(t, df, lower.tail = ifelse(t < 0, T, F))
    d <- (m1 - m2)/s.pool
    results <- c("Welch's t" = round(t, 2), df = round(df, 2), 
                 p = round(p, 4), "Cohen's d" = round(d, 2))
  } else if (var.equal == T) {
    s.pool <- sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2)/(n1 + n2 - 2))
    t <- (m1 - m2) / (s.pool * sqrt((1/n1) + (1/n2)))
    df <- n1 + n2 - 2
    p <- pt(t, df, lower.tail = ifelse(t < 0, T, F))
    d <- (m1 - m2)/s.pool
    results <- c(t = round(t, 2), df = round(df, 2), 
                 p = round(p, 4), "Cohen's d" = round(d, 2))
  }
  
  return(list("Summary Table" = tab, "Test of Equal Variances" = varTest, 
              "Test of Absolute Values of Means" = results))
}

compAbsMean(studyNum = 1)
compAbsMean(studyNum = 1, var.equal = F)
compAbsMean(studyNum = 2)
compAbsMean(studyNum = 3)
compAbsMean(studyNum = 4)

# compare effect sizes by country
compAbsMean_byCountry <- function(studyNum, var.equal = "test") {
  # make summary table for this study
  tab <- d %>%
    filter(phase == "test" & study %in% studyNum) %>%
    group_by(country, sentInan) %>%
    summarise(mean = mean(response, na.rm = T),
              absMean = abs(mean),
              sd = sd(response, na.rm = T),
              n = length(response))
  
  # get means, sds, and ns for sentient-only vs. inanimate trials
  m1.india <- tab$absMean[1]; m2.india <- tab$absMean[2]
  s1.india <- tab$sd[1]; s2.india <- tab$sd[2]; 
  n1.india <- tab$n[1]; n2.india <- tab$n[2]
  m1.us <- tab$absMean[3]; m2.us <- tab$absMean[4]
  s1.us <- tab$sd[3]; s2.us <- tab$sd[4]; 
  n1.us <- tab$n[3]; n2.us <- tab$n[4]
  
  # test for equal variances
  varTest <- with(d %>% filter(phase == "test" & study == studyNum), 
                  bartlett.test(response ~ interaction(country, sentInan)))
  if(var.equal == "test") {
    var.equal <- ifelse(varTest$p.value < 0.10, F, T)
  }
  
  # calculate test statistics
  if(var.equal == F) {
    
    # india
    s.pool.india <- sqrt(((n1.india - 1)*s1.india^2 + (n2.india - 1)*s2.india^2)/(n1.india + n2.india - 2))
    t.india <- (m1.india - m2.india) / sqrt(s1.india^2/n1.india + s2.india^2/n2.india)
    df.india <- (s1.india^2/n1.india + s2.india^2/n2.india)^2 / ((s1.india^2/n1.india)^2/(n1.india-1) + (s2.india^2/n2.india)^2/(n2.india-1))
    p.india <- pt(t.india, df.india, lower.tail = ifelse(t.india < 0, T, F))
    d.india <- (m1.india - m2.india)/s.pool.india
    results.india <- c("Welch's t" = round(t.india, 2), df = round(df.india, 2), 
                       p = round(p.india, 4), "Cohen's d" = round(d.india, 2))
    
    # us
    s.pool.us <- sqrt(((n1.us - 1)*s1.us^2 + (n2.us - 1)*s2.us^2)/(n1.us + n2.us - 2))
    t.us <- (m1.us - m2.us) / sqrt(s1.us^2/n1.us + s2.us^2/n2.us)
    df.us <- (s1.us^2/n1.us + s2.us^2/n2.us)^2 / ((s1.us^2/n1.us)^2/(n1.us-1) + (s2.us^2/n2.us)^2/(n2.us-1))
    p.us <- pt(t.us, df.us, lower.tail = ifelse(t.us < 0, T, F))
    d.us <- (m1.us - m2.us)/s.pool.us
    results.us <- c("Welch's t" = round(t.us, 2), df = round(df.us, 2), 
                    p = round(p.us, 4), "Cohen's d" = round(d.us, 2))
    
  } else if (var.equal == T) {
    
    # india
    s.pool.india <- sqrt(((n1.india - 1)*s1^2 + (n2.india - 1)*s2^2)/(n1.india + n2.india - 2))
    t.india <- (m1.india - m2.india) / (s.pool * sqrt((1/n1.india) + (1/n2.india)))
    df.india <- n1.india + n2.india - 2
    p.india <- pt(t.india, df.india, lower.tail = ifelse(t.india < 0, t.india, F))
    d.india <- (m1.india - m2.india)/s.pool.india
    results.india <- c(t = round(t.india, 2), df = round(df.india, 2), 
                       p = round(p.india, 4), "Cohen's d" = round(d.india, 2))
    
    # us
    s.pool.us <- sqrt(((n1.us - 1)*s1^2 + (n2.us - 1)*s2^2)/(n1.us + n2.us - 2))
    t.us <- (m1.us - m2.us) / (s.pool * sqrt((1/n1.us) + (1/n2.us)))
    df.us <- n1.us + n2.us - 2
    p.us <- pt(t.us, df.us, lower.tail = ifelse(t.us < 0, t.us, F))
    d.us <- (m1.us - m2.us)/s.pool.us
    results.us <- c(t = round(t.us, 2), df = round(df.us, 2), 
                    p = round(p.us, 4), "Cohen's d" = round(d.us, 2))
  }
  
  # compare effect sizes: d1 = us
  v.india <- (1/n1.india) + (1/n2.india) + d.india^2/(2*(n1.india + n2.india))
  v.us <- (1/n1.us) + (1/n2.us) + d.us^2/(2*(n1.us + n2.us))
  z.comp <- (d.us - d.india) / sqrt(v.us + v.india)
  p.comp <- pnorm(z.comp, lower.tail = ifelse(z.comp < 1, T, F))
  results.compare <- c(z = round(z.comp, 2), p = round(p.comp, 4))
  
  return(list("Summary Table" = tab, "Test of Equal Variances" = varTest, 
              "Test of Absolute Values of Means: India" = results.india,
              "Test of Absolute Values of Means: US" = results.us,
              "Comparison of Effect Sizes (US - India)" = results.compare))
}
compAbsMean_byCountry(studyNum = c(1,3))
compAbsMean_byCountry(studyNum = 4)

# --- [additional option #3] REGRESSION ANALYSES ------------------------------

# r1a <- lmer(response ~ -1 + sentInan + (1 | subid),
#             data = subset(d, phase == "test" & study == "1"))
# summary(r1a)

r1b <- lmer(response ~ -1 + sentInan + (sentInan | subid),
            data = subset(d, phase == "test" & study == "1"))
summary(r1b)

r2b <- lmer(response ~ -1 + sentInan + (sentInan | subid),
            data = subset(d, phase == "test" & study == "2"))
summary(r2b)

r3b <- lmer(response ~ -1 + sentInan + (sentInan | subid),
            data = subset(d, phase == "test" & study == "3"))
summary(r3b)

contrasts(d$sentInan)

r4b <- lmer(response ~ sentInan * country + (sentInan | subid),
            data = subset(d, phase == "test" & study == "4"))
summary(r4b)

# r1 <- lm(response ~ -1 + sentInan,
#          data = subset(d, phase == "test" & study == "1"))
# summary(r1)
# cohensD_inan <- (summary(r1)$coefficients[1,3]*2)/sqrt(summary(r1)$df[2])
# cohensD_sent <- (summary(r1)$coefficients[2,3]*2)/sqrt(summary(r1)$df[2])