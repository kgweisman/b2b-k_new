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

# --- PLOTTING FUNCTIONS ------------------------------------------------------

# make plotting function for plotting all possible fact-question pairings
plotFQP <- function(studyNum, countryName, ageGroup) {
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(sumTable, 
                            study == studyNum & country == countryName)) +
    geom_bar(aes(fill = questionCat), 
             colour = "black", position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing fact-question pairings across age groups
plotFcompAge <- function(studyNum) {
  levels(sumTable$ageGroup) = c("children", "adults")
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(sumTable, 
                            study == studyNum[1] | study == studyNum[2])) +
    facet_grid(ageGroup ~ .,
               labeller = labeller(ageGroup = c("adults" = "US Adults", 
                                               "children" = "US Children"))) +
    geom_bar(aes(fill = questionCat), 
             colour = "black", position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing fact-question pairings across race/ethnicity
plotFcompRE <- function(studyNum) {
  levels(sumTableRE$ageGroup) = c("of-color", "white")
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(sumTableRE, 
                            study == studyNum[1] | study == studyNum[2])) +
    facet_grid(raceEthn2 ~ .,
               labeller = labeller(raceEthn2 = c("of-color" = "Children of Color", 
                                               "white" = "White Children"))) +
    geom_bar(aes(fill = questionCat), 
             colour = "black", position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing fact-question pairings across countries
plotFcompCountry <- function(studyNum) {
  levels(sumTable$country) = c("india", "us")
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(sumTable, 
                            study == studyNum[1] | study == studyNum[2])) +
    facet_grid(country ~ .,
               labeller = labeller(country = c("india" = "Indian Adults", 
                                               "us" = "US Adults"))) +
    geom_bar(aes(fill = questionCat), 
             colour = "black", position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing fact-question pairings across countries
plotFcompFraming <- function(studyNum) {
  levels(sumTable$country) = c("india", "us")
  levels(sumTable$framing) = c("does that mean...?", "do you think...?")
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(sumTable, study == studyNum[1] | 
                              study == studyNum[2] | study == studyNum[3])) +
    facet_grid(country ~ framing,
               labeller = labeller(
                 country = c("india" = "Indian Adults", "us" = "US Adults"),
                 framing = c("do you think...?" = "Do you think...?",
                             "does that mean...?" = "Does that mean...?"))) +
    geom_bar(aes(fill = questionCat), 
             colour = "black", position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plot-formatting function
plotFformat <- function(plot) {
  studyNum <- levels(factor(plot$data$study))
  countryName <- levels(factor(plot$data$country))
  ageGroup <- levels(factor(plot$data$ageGroup))
  if (is.null(plot$data$raceEthn2)) {
    raceEthn2 <- "NA"
  } else {
    raceEthn2 <- levels(factor(plot$data$raceEthn2)); 
  }
  g <- plot +
        coord_cartesian(ylim = c(-1.5, 1.5)) +
        theme_bw() +
        theme(text = element_text(size = 20),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 20),
              axis.text.x = element_text(size = 20),
              legend.position = "top",
              legend.text = element_text(size = 15),
              legend.title = element_text(size = 15),
              plot.title = element_text(size = 20),
              strip.text = element_text(size = 20)) +
        labs(x = "Fact Category", 
             y = "Mean Rating (-1.5 = Really No, 1.5 = Really Yes)\n") +
      scale_x_discrete(labels = c("Affect", "Autonomy", 
                                  "Perception", "Inanimate\nMaterial")) +
      scale_fill_grey(name = "Question Category", 
                      labels = c(" Affect ", " Autonomy ", 
                                   " Perception ", " Inanimate Material "))
  if (length(studyNum) == 1) {
    if (length(countryName) == 1 & length(ageGroup) == 1 & 
          length(raceEthn2) < 2) {
      g <- g + 
        labs(title = paste0("Inferences by Fact and Question Category:\nStudy ",
                            studyNum, " (",
                            ifelse(countryName == "us", "US ", "Indian "),
                            str_to_title(ageGroup), ")"))
            
    } else {
      g <- g + 
        labs(title = paste0("Inferences by Fact and Question Category: Study ",
                            studyNum))    
    }
  } else {
    # make string of study labels
    studies <- NULL
    for(i in 1:(length(studyNum) - 1)) {
      studies <- paste(studies, studyNum[i], sep = ", ")
    }
    studies <- paste(studies, studyNum[length(studyNum)], sep = ", & ")
    studies <- substring(studies, first = 3)
    g <- g + 
      labs(title = paste0("Inferences by Fact and Question Category: Studies ",
                          studies))
  }
  return(g)
}

# --- SUMMARY TABLES BY FACT-QUESTION PAIR ------------------------------------

# make summary table (collapse across race/ethncity)
sumTable <- d %>%
  filter(study != "1prime" & phase == "test") %>%
  group_by(study, ageGroup, country, framing, factCat, questionCat) %>%
  summarise(mean = mean(response, na.rm = T),
            sd = sd(response, na.rm = T),
            n = length(response[is.na(response) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError)
sumTable

# make summary table (separate by race/ethncity)
sumTableRE <- d %>%
  filter(study != "1prime" & phase == "test" & raceEthn2 != "NA") %>%
  group_by(study, ageGroup, country, raceEthn2, framing, factCat, questionCat) %>%
  summarise(mean = mean(response, na.rm = T),
            sd = sd(response, na.rm = T),
            n = length(response[is.na(response) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError)
sumTableRE

# ------ PLOTS ----------------------------------------------------------------

# plot all possible fact-question pairings, by individual study
g1 <- plotFformat(plotFQP("1", "us", "adults")); g1
# g1prime <- plotFformat(plotFQP("1prime", "us", "adults")); g1prime
g2 <- plotFformat(plotFQP("2", "us", "children")); g2
g3 <- plotFformat(plotFQP("3", "india", "adults")); g3
g4us <- plotFformat(plotFQP("4", "us", "adults")); g4us
g4india <- plotFformat(plotFQP("4", "india", "adults")); g4india

# plot 2-way comparisons of all possible fact-quesiton pairings
g12 <- plotFformat(plotFcompAge(studyNum = c("1", "2"))); g12
g2re <- plotFformat(plotFcompRE(studyNum = c("2", "2"))); g2re
g13 <- plotFformat(plotFcompCountry(studyNum = c("1", "3"))); g13
g4all <- plotFformat(plotFcompCountry(studyNum = c("4", "4"))); g4all

# plot 4-way comparison of framing by country (adults)
g134 <- plotFformat(plotFcompFraming(studyNum = c("1", "3", "4"))); g134
