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

# ------ all possible fact-question pairings ----------------------------------

# make plotting function for plotting all possible fact-question pairings
plotQP <- function(studyNum, countryName, ageGroup, scoreType) {
  
  # select which table to use
  if (scoreType == "raw") {
    table <- sumTableQP
  } else if (scoreType == "abs") {
    table <- sumTableQP_abs
  }
  
  # plot means
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(table, 
                            study == studyNum & country == countryName)) +
    geom_bar(aes(fill = questionCat), 
             colour = "black", position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing fact-question pairings across age groups
plotQPCompAge <- function(studyNum, scoreType) {
  
  # select which table to use
  if (scoreType == "raw") {
    table <- sumTableQP
  } else if (scoreType == "abs") {
    table <- sumTableQP_abs
  }
  
  # plot means
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(table, 
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
plotQPCompRE <- function(studyNum, scoreType) {
  
  # select which table to use
  if (scoreType == "raw") {
    table <- sumTableQPRE
  } else if (scoreType == "abs") {
    table <- sumTableQPRE_abs
  }
  
  # plot means
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(table, 
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
plotQPCompCountry <- function(studyNum, scoreType) {
  
  # select which table to use
  if (scoreType == "raw") {
    table <- sumTableQP
  } else if (scoreType == "abs") {
    table <- sumTableQP_abs
  }
  
  # plot means
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(table, 
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
plotQPCompFraming <- function(studyNum, scoreType) {
  
  # select which table to use
  if (scoreType == "raw") {
    table <- sumTableQP
  } else if (scoreType == "abs") {
    table <- sumTableQP_abs
  }
  
  # plot means
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(table, study == studyNum[1] | 
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
plotQPFormat <- function(plot, scoreType) {
  
  # set parameters of study
  studyNum <- levels(factor(plot$data$study))
  countryName <- levels(factor(plot$data$country))
  ageGroup <- levels(factor(plot$data$ageGroup))
  if (is.null(plot$data$raceEthn2)) {
    raceEthn2 <- "NA"
  } else {
    raceEthn2 <- levels(factor(plot$data$raceEthn2)); 
  }
  if (scoreType == "raw") {
    ylim = c(-1.5, 1.5)
  } else if (scoreType == "abs") {
    ylim = c(0, 1.5)
  }
  
  # plot
  g <- plot +
        coord_cartesian(ylim = ylim) +
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
             y = "Mean Rating (-1.5 = Really No, 1.5 = Really Yes)") +
      scale_x_discrete(labels = c("Affect", "Autonomy", 
                                  "Perception", "Inanimate\nMaterial")) +
#     scale_fill_grey(name = "Question Category", 
#                     labels = c(" Affect ", " Autonomy ", 
#                                " Perception ", " Inanimate Material "))
#   
    scale_fill_manual(name = "Question Category", 
                      labels = c(" Affect ", " Autonomy ", 
                                   " Perception ", " Inanimate Material "),
                      values = c("darkblue", "mediumblue", "lightblue", "red")) 

  # provide study numbers in title
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
    studies <- paste(studies, studyNum[length(studyNum)], sep = " & ")
    studies <- substring(studies, first = 3)
    g <- g + 
      labs(title = paste0("Inferences by Fact and Question Category: Studies ",
                          studies))
  }
  return(g)
}

# ------ sentient only vs. inanimate trials -----------------------------------

# make plotting function for plotting trial types
plotSent <- function(studyNum, countryName, ageGroup, scoreType) {
  
  # select which table to use
  if (scoreType == "raw") {
    table <- sumTableSent
  } else if (scoreType == "abs") {
    table <- sumTableSent_abs
  }
  
  # plot means
  g <- ggplot(aes(x = sent, y = mean),
              data = subset(table,
                            study == studyNum & country == countryName)) +
    geom_bar(fill = "gray", colour = "black", 
             position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing trial types across age groups
plotSentCompAge <- function(studyNum, scoreType) {
  
  # select which table to use
  if (scoreType == "raw") {
    table <- sumTableSent
  } else if (scoreType == "abs") {
    table <- sumTableSent_abs
  }
  
  # plot means
  g <- ggplot(aes(x = sent, y = mean), 
              data = subset(table, 
                            study == studyNum[1] | study == studyNum[2])) +
    facet_grid(ageGroup ~ .,
               labeller = labeller(ageGroup = c("adults" = "US Adults", 
                                                "children" = "US Children"))) +
    geom_bar(fill = "gray", colour = "black", 
             position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing trial types across race/ethnicity
plotSentCompRE <- function(studyNum, scoreType) {
  
  # select which table to use
  if (scoreType == "raw") {
    table <- sumTableSentRE
  } else if (scoreType == "abs") {
    table <- sumTableSentRE_abs
  }
  
  # plot means
  g <- ggplot(aes(x = sent, y = mean), 
              data = subset(table, 
                            study == studyNum[1] | study == studyNum[2])) +
    facet_grid(raceEthn2 ~ .,
               labeller = labeller(raceEthn2 = c("of-color" = "Children of Color", 
                                                 "white" = "White Children"))) +
    geom_bar(fill = "gray", colour = "black", 
             position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing trial types across countries
plotSentCompCountry <- function(studyNum, scoreType) {
  
  # select which table to use
  if (scoreType == "raw") {
    table <- sumTableSent
  } else if (scoreType == "abs") {
    table <- sumTableSent_abs
  }
  
  # plot means
  g <- ggplot(aes(x = sent, y = mean), 
              data = subset(table, 
                            study == studyNum[1] | study == studyNum[2])) +
    facet_grid(country ~ .,
               labeller = labeller(country = c("india" = "Indian Adults", 
                                               "us" = "US Adults"))) +
    geom_bar(fill = "gray", colour = "black", 
             position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing trial types across countries
plotSentCompFraming <- function(studyNum, scoreType) {
  
  # select which table to use
  if (scoreType == "raw") {
    table <- sumTableSent
  } else if (scoreType == "abs") {
    table <- sumTableSent_abs
  }
  
  # plot means
  g <- ggplot(aes(x = sent, y = mean), 
              data = subset(table, study == studyNum[1] | 
                              study == studyNum[2] | study == studyNum[3])) +
    facet_grid(country ~ framing,
               labeller = labeller(
                 country = c("india" = "Indian Adults", "us" = "US Adults"),
                 framing = c("do you think...?" = "Do you think...?",
                             "does that mean...?" = "Does that mean...?"))) +
    geom_bar(fill = "gray", colour = "black", 
             position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plot-formatting function
plotSentFormat <- function(plot, scoreType) {
  
  # set parameters of study
  studyNum <- levels(factor(plot$data$study))
  countryName <- levels(factor(plot$data$country))
  ageGroup <- levels(factor(plot$data$ageGroup))
  if (is.null(plot$data$raceEthn2)) {
    raceEthn2 <- "NA"
  } else {
    raceEthn2 <- levels(factor(plot$data$raceEthn2)); 
  }
  if (scoreType == "raw") {
    ylim = c(-1.5, 1.5)
  } else if (scoreType == "abs") {
    ylim = c(0, 1.5)
  }
  
  # plot
  g <- plot +
    coord_cartesian(ylim = ylim) +
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
    labs(x = "Trial Type", 
         y = "Mean Rating (-1.5 = Really No, 1.5 = Really Yes)\n") +
    scale_x_discrete(labels = c("Inanimate", "Sentient-Only")) 
  
  # provide study numbers in title
  if (length(studyNum) == 1) {
    if (length(countryName) == 1 & length(ageGroup) == 1 & 
          length(raceEthn2) < 2) {
      g <- g + 
        labs(title = paste0("Inferences by Trial Type:\nStudy ",
                            studyNum, " (",
                            ifelse(countryName == "us", "US ", "Indian "),
                            str_to_title(ageGroup), ")\n"))
      
    } else {
      g <- g + 
        labs(title = paste0("Inferences by Trial Type: Study ",
                            studyNum, "\n"))    
    }
  } else {
    # make string of study labels
    studies <- NULL
    for(i in 1:(length(studyNum) - 1)) {
      studies <- paste(studies, studyNum[i], sep = ", ")
    }
    studies <- paste(studies, studyNum[length(studyNum)], sep = " & ")
    studies <- substring(studies, first = 3)
    g <- g + 
      labs(title = paste0("Inferences by Trial Type: Studies ",
                          studies, "\n"))
  }
  return(g)
}

# --- SUMMARY TABLES ----------------------------------------------------------

# make refactoring function to reo, scoreTyperder levels
refactor <- function(table) {
  table$ageGroup = factor(table$ageGroup, levels = c("adults", "children"))
  table$country = factor(table$country, levels = c("us", "india"))
  table$framing = factor(table$framing, levels = c("does that mean...?", 
                                                   "do you think...?"))
  if (grepl("raceEthn2", paste(names(table), collapse = "_"), fixed = T)) {
    table$raceEthn2 = factor(table$raceEthn2, levels = c("white", "of-color"))
  }
  return(table)
}

# ------ all possible fact-question pairings: RAW SCORES ----------------------

# make summary table by fact-question pair (collapse across race/ethncity)
sumTableQP <- d %>%
  filter(study != "1prime" & phase == "test") %>%
  group_by(study, ageGroup, country, framing, factCat, questionCat) %>%
  summarise(mean = mean(response, na.rm = T),
            sd = sd(response, na.rm = T),
            n = length(response[is.na(response) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError,
         sent = ifelse(factCat == "phy" | questionCat == "phy",
                              "inanimate", "sentient-only")) %>%
  refactor()
sumTableQP

# make summary table by fact-question pair (separate by race/ethncity)
sumTableQPRE <- d %>%
  filter(study != "1prime" & phase == "test" & raceEthn2 != "NA") %>%
  group_by(study, ageGroup, country, raceEthn2, framing, factCat, questionCat) %>%
  summarise(mean = mean(response, na.rm = T),
            sd = sd(response, na.rm = T),
            n = length(response[is.na(response) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError,
         sent = ifelse(factCat == "phy" | questionCat == "phy",
                       "inanimate", "sentient-only")) %>%
  refactor()
sumTableQPRE

# ------ all possible fact-question pairings: ABSOLUTE VALUES OF SCORES -------

# make summary table by fact-question pair (collapse across race/ethncity)
sumTableQP_abs <- d %>%
  filter(study != "1prime" & phase == "test") %>%
  mutate(response_abs = abs(response)) %>%
  group_by(study, ageGroup, country, framing, factCat, questionCat) %>%
  summarise(mean = mean(response_abs, na.rm = T),
            sd = sd(response_abs, na.rm = T),
            n = length(response_abs[is.na(response_abs) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError,
         sent = ifelse(factCat == "phy" | questionCat == "phy",
                       "inanimate", "sentient-only")) %>%
  refactor()
sumTableQP_abs

# make summary table by fact-question pair (separate by race/ethncity)
sumTableQPRE_abs <- d %>%
  filter(study != "1prime" & phase == "test" & raceEthn2 != "NA") %>%
  mutate(response_abs = abs(response)) %>%
  group_by(study, ageGroup, country, raceEthn2, framing, factCat, questionCat) %>%
  summarise(mean = mean(response_abs, na.rm = T),
            sd = sd(response_abs, na.rm = T),
            n = length(response_abs[is.na(response_abs) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError,
         sent = ifelse(factCat == "phy" | questionCat == "phy",
                       "inanimate", "sentient-only")) %>%
  refactor()
sumTableQPRE_abs

# ------ sentient-only vs. inanimate trials: RAW SCORES -----------------------

# make summary table by sentient-only vs. inanimate trials (collapse across race/ethncity)
sumTableSent <- d %>%
  filter(study != "1prime" & phase == "test") %>%
  mutate(sent = ifelse(factCat == "phy" | questionCat == "phy",
         "inanimate", "sentient-only")) %>%
  group_by(study, ageGroup, country, framing, sent) %>%
  summarise(mean = mean(response, na.rm = T),
            sd = sd(response, na.rm = T),
            n = length(response[is.na(response) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError) %>%
  refactor()
sumTableSent

# make summary table by fact-question pair (separate by race/ethncity)
sumTableSentRE <- d %>%
  filter(study != "1prime" & phase == "test" & raceEthn2 != "NA") %>%
  mutate(sent = ifelse(factCat == "phy" | questionCat == "phy",
                       "inanimate", "sentient-only")) %>%
  group_by(study, ageGroup, country, raceEthn2, framing, sent) %>%
  summarise(mean = mean(response, na.rm = T),
            sd = sd(response, na.rm = T),
            n = length(response[is.na(response) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError) %>%
  refactor()
sumTableSentRE

# ------ sentient-only vs. inanimate trials: ABSOLUTE VALUES OF SCORES --------

# make summary table by sentient-only vs. inanimate trials (collapse across race/ethncity)
sumTableSent_abs <- d %>%
  filter(study != "1prime" & phase == "test") %>%
  mutate(sent = ifelse(factCat == "phy" | questionCat == "phy",
                       "inanimate", "sentient-only"),
         response_abs = abs(response)) %>%
  group_by(study, ageGroup, country, framing, sent) %>%
  summarise(mean = mean(response_abs, na.rm = T),
            sd = sd(response_abs, na.rm = T),
            n = length(response_abs[is.na(response_abs) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError) %>%
  refactor()
sumTableSent_abs

# make summary table by fact-question pair (separate by race/ethncity)
sumTableSentRE_abs <- d %>%
  filter(study != "1prime" & phase == "test" & raceEthn2 != "NA") %>%
  mutate(sent = ifelse(factCat == "phy" | questionCat == "phy",
                       "inanimate", "sentient-only"),
         response_abs = abs(response)) %>%
  group_by(study, ageGroup, country, raceEthn2, framing, sent) %>%
  summarise(mean = mean(response_abs, na.rm = T),
            sd = sd(response_abs, na.rm = T),
            n = length(response_abs[is.na(response_abs) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError) %>%
  refactor()
sumTableSentRE_abs

# --- PLOTS -------------------------------------------------------------------

# ------ all possible fact-question pairings: RAW SCORES ----------------------

# plot all possible fact-question pairings, by individual study
g1 <- plotQPFormat(plotQP("1", "us", "adults", "raw"), "raw"); g1
# g1prime <- plotQPFormat(plotQP("1prime", "us", "adults", "raw"), "raw"); g1prime
g2 <- plotQPFormat(plotQP("2", "us", "children", "raw"), "raw"); g2
g3 <- plotQPFormat(plotQP("3", "india", "adults", "raw"), "raw"); g3
g4us <- plotQPFormat(plotQP("4", "us", "adults", "raw"), "raw"); g4us
g4india <- plotQPFormat(plotQP("4", "india", "adults", "raw"), "raw"); g4india

# plot 2-way comparisons of all possible fact-quesiton pairings
g12 <- plotQPFormat(plotQPCompAge(c("1", "2"), "raw"), "raw"); g12
g2re <- plotQPFormat(plotQPCompRE(c("2", "2"), "raw"), "raw"); g2re
g13 <- plotQPFormat(plotQPCompCountry(c("1", "3"), "raw"), "raw"); g13
g4all <- plotQPFormat(plotQPCompCountry(c("4", "4"), "raw"), "raw"); g4all

# plot 4-way comparison of framing by country (adults)
g134 <- plotQPFormat(plotQPCompFraming(c("1", "3", "4"), "raw"), "raw"); g134

# ------ all possible fact-question pairings: ABSOLUTE VALUES OF SCORES -------

# plot all possible fact-question pairings, by individual study
g1_abs <- plotQPFormat(plotQP("1", "us", "adults", "abs"), "abs"); g1_abs
# g1prime_abs <- plotQPFormat(plotQP("1prime", "us", "adults", "abs"), "abs"); g1prime_abs
g2_abs <- plotQPFormat(plotQP("2", "us", "children", "abs"), "abs"); g2_abs
g3_abs <- plotQPFormat(plotQP("3", "india", "adults", "abs"), "abs"); g3_abs
g4us_abs <- plotQPFormat(plotQP("4", "us", "adults", "abs"), "abs"); g4us_abs
g4india_abs <- plotQPFormat(plotQP("4", "india", "adults", "abs"), "abs"); g4india_abs

# plot 2-way comparisons of all possible fact-quesiton pairings
g12_abs <- plotQPFormat(plotQPCompAge(c("1", "2"), "abs"), "abs"); g12_abs
g2re_abs <- plotQPFormat(plotQPCompRE(c("2", "2"), "abs"), "abs"); g2re_abs
g13_abs <- plotQPFormat(plotQPCompCountry(c("1", "3"), "abs"), "abs"); g13_abs
g4all_abs <- plotQPFormat(plotQPCompCountry(c("4", "4"), "abs"), "abs"); g4all_abs

# plot 4-way comparison of framing by country (adults)
g134_abs <- plotQPFormat(plotQPCompFraming(c("1", "3", "4"), "abs"), "abs"); g134_abs

# ------ sentient-only vs. inanimate trials: RAW SCORES -----------------------

# plot sentient vs. inanimate, by individual study
p1 <- plotSentFormat(plotSent("1", "us", "adults", "raw"), "raw"); p1
# p1prime <- plotSentFormat(plotSent("1prime", "us", "adults", "raw"), "raw"); p1prime
p2 <- plotSentFormat(plotSent("2", "us", "children", "raw"), "raw"); p2
p3 <- plotSentFormat(plotSent("3", "india", "adults", "raw"), "raw"); p3
p4us <- plotSentFormat(plotSent("4", "us", "adults", "raw"), "raw"); p4us
p4india <- plotSentFormat(plotSent("4", "india", "adults", "raw"), "raw"); p4india

# plot 2-way comparisons of all possible fact-quesiton pairings
p12 <- plotSentFormat(plotSentCompAge(c("1", "2"), "raw"), "raw"); p12
p2re <- plotSentFormat(plotSentCompRE(c("2", "2"), "raw"), "raw"); p2re
p13 <- plotSentFormat(plotSentCompCountry(c("1", "3"), "raw"), "raw"); p13
p4all <- plotSentFormat(plotSentCompCountry(c("4", "4"), "raw"), "raw"); p4all

# plot 4-way comparison of framing by country (adults)
p134 <- plotSentFormat(plotSentCompFraming(c("1", "3", "4"), "raw"), "raw"); p134

# ------ sentient-only vs. inanimate trials: ABSOLUTE VALUES OF SCORES --------

# plot sentient vs. inanimate, by individual study
p1_abs <- plotSentFormat(plotSent("1", "us", "adults", "abs"), "abs"); p1_abs
# p1prime_abs <- plotSentFormat(plotSent("1prime", "us", "adults", "abs"), "abs"); p1prime_abs
p2_abs <- plotSentFormat(plotSent("2", "us", "children", "abs"), "abs"); p2_abs
p3_abs <- plotSentFormat(plotSent("3", "india", "adults", "abs"), "abs"); p3_abs
p4us_abs <- plotSentFormat(plotSent("4", "us", "adults", "abs"), "abs"); p4us_abs
p4india_abs <- plotSentFormat(plotSent("4", "india", "adults", "abs"), "abs"); p4india_abs

# plot 2-way comparisons of all possible fact-quesiton pairings
p12_abs <- plotSentFormat(plotSentCompAge(c("1", "2"), "abs"), "abs"); p12_abs
p2re_abs <- plotSentFormat(plotSentCompRE(c("2", "2"), "abs"), "abs"); p2re_abs
p13_abs <- plotSentFormat(plotSentCompCountry(c("1", "3"), "abs"), "abs"); p13_abs
p4all_abs <- plotSentFormat(plotSentCompCountry(c("4", "4"), "abs"), "abs"); p4all_abs

# plot 4-way comparison of framing by country (adults)
p134_abs <- plotSentFormat(plotSentCompFraming(c("1", "3", "4"), "abs"), "abs"); p134_abs