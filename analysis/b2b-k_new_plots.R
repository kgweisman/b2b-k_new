# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(stats)
library(stringr)
library(lubridate)
library(RColorBrewer)

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
plotQP <- function(studyNum, countryName, ageGroup, scoreType, blank = F) {
  
  # select which table to use
  if (blank == T) {
    table <- sumTableQP_blank
  } else if (scoreType == "raw") {
    table <- sumTableQP
  } else if (scoreType == "abs") {
    table <- sumTableQP_abs
  }
    
  # plot means
  # to order by within vs. between: group = interaction(questionCat, within)
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(table, 
                            study == studyNum & country == countryName)) +
    geom_bar(aes(fill = questionCat), colour = "black",
             position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing fact-question pairings across age groups
plotQPCompAge <- function(studyNum, scoreType, blank = F) {
  # select which table to use
  if (blank == T) {
    table <- sumTableQP_blank
  } else if (scoreType == "raw") {
    table <- sumTableQP
  } else if (scoreType == "abs") {
    table <- sumTableQP_abs
  }
    
  # plot means
  # to order by within vs. between: group = interaction(questionCat, within)
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(table, 
                            study == studyNum[1] | study == studyNum[2])) +
    facet_grid(ageGroup ~ .,
               labeller = labeller(ageGroup = c("adults" = "US Adults", 
                                               "children" = "US Children"))) +
    geom_bar(aes(fill = questionCat), colour = "black", 
             position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing fact-question pairings across race/ethnicity
plotQPCompRE <- function(studyNum, scoreType, blank = F) {
  
  # select which table to use
  if (blank == T) {
    table <- sumTableQPRE_blank
  } else if (scoreType == "raw") {
    table <- sumTableQPRE
  } else if (scoreType == "abs") {
    table <- sumTableQPRE_abs
  }
    
  # plot means
  # to order by within vs. between: group = interaction(questionCat, within)
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(table, 
                            study == studyNum[1] | study == studyNum[2])) +
    facet_grid(raceEthn2 ~ .,
               labeller = labeller(raceEthn2 = c("of-color" = "Children of Color", 
                                               "white" = "White Children"))) +
    geom_bar(aes(fill = questionCat), colour = "black", 
             position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing fact-question pairings across countries
plotQPCompCountry <- function(studyNum, scoreType, blank = F) {
  
  # select which table to use
  if (blank == T) {
    table <- sumTableQP_blank
  } else if (scoreType == "raw") {
    table <- sumTableQP
  } else if (scoreType == "abs") {
    table <- sumTableQP_abs
  }
    
  # plot means
  # to order by within vs. between: group = interaction(questionCat, within)
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(table, 
                            study == studyNum[1] | study == studyNum[2])) +
    facet_grid(country ~ .,
               labeller = labeller(country = c("india" = "Indian Adults", 
                                               "us" = "US Adults"))) +
    geom_bar(aes(fill = questionCat), colour = "black", 
             position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing fact-question pairings across countries
plotQPCompFraming <- function(studyNum, scoreType, blank = F) {
  
  # select which table to use
  if (blank == T) {
    table <- sumTableQP_blank
  } else if (scoreType == "raw") {
    table <- sumTableQP
  } else if (scoreType == "abs") {
    table <- sumTableQP_abs
  }
    
  # plot means
  # to order by within vs. between: group = interaction(questionCat, within)
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(table, study == studyNum[1] | 
                              study == studyNum[2] | study == studyNum[3])) +
    facet_grid(country ~ framing,
               labeller = labeller(
                 country = c("india" = "Indian Adults", "us" = "US Adults"),
                 framing = c("do you think...?" = "Do you think...?",
                             "does that mean...?" = "Does that mean...?"))) +
    geom_bar(aes(fill = questionCat), colour = "black",
             position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing fact-question pairings across countries
plotQPCompStudies123 <- function(studyNum, scoreType, blank = F) {
  
  # select which table to use
  if (blank == T) {
    table <- sumTableQP_blank
  } else if (scoreType == "raw") {
    table <- sumTableQP
  } else if (scoreType == "abs") {
    table <- sumTableQP_abs
  }
  
  # plot means
  # to order by within vs. between: group = interaction(questionCat, within)
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(table, study == studyNum[1] | 
                              study == studyNum[2] | study == studyNum[3])) +
    facet_grid(. ~ study,
               labeller = labeller(
                 study = c("1" = "US Adults", "2" = "US Children",
                             "3" = "Indian Adults"))) +
    geom_bar(aes(fill = questionCat), colour = "black",
             position = "dodge", stat = "identity") + 
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
        theme(text = element_text(size = 28),
              axis.text = element_text(size = 21),
              axis.title = element_text(size = 28),
              axis.text.x = element_text(size = 28),
              legend.position = "top",
              legend.text = element_text(size = 21),
              legend.title = element_text(size = 21),
              plot.title = element_text(size = 28),
              strip.text = element_text(size = 28)) +
        labs(x = "FACT Category", 
             y = "Mean Rating (-1.5 = Really No, 1.5 = Really Yes)\n") +
      scale_x_discrete(labels = c("\nAffect", "\nAutonomy", 
                                  "\nPerception", "\nInanimate\nMaterial")) +
#     scale_fill_grey(name = "QUESTION Category", 
#                     labels = c(" Affect ", " Autonomy ", 
#                                " Perception ", " Inanimate Material "))
#   
#     scale_fill_manual(name = "QUESTION Category", 
#                       labels = c(" Affect ", " Autonomy ", 
#                                    " Perception ", " Inanimate Material "),
#                       values = c("darkred", "darkgreen", "darkblue", "gray")) 

      scale_fill_manual(name = "QUESTION Category", 
                        labels = c(" Affect ", " Autonomy ", 
                                   " Perception ", " Inanimate Material "),
                        values = c(brewer.pal(3, "Set2"), "grey")) 

  # provide study numbers in title
  if (length(studyNum) == 1) {
    if (length(countryName) == 1 & length(ageGroup) == 1 & 
          length(raceEthn2) < 2) {
      g <- g + 
        labs(title = paste0("Study ",
                            studyNum, " (",
                            ifelse(countryName == "us", "US ", "Indian "),
                            str_to_title(ageGroup), ")"))
            
    } else {
      g <- g + 
        labs(title = paste0("Study ",
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
      labs(title = paste0("Studies ",
                          studies))
  }
  return(g)
}

# ------ sentient only vs. inanimate trials -----------------------------------

# make plotting function for plotting trial types
plotSent <- function(studyNum, countryName, ageGroup, scoreType, blank = F) {
  
  # select which table to use
  if (blank == T) {
    table <- sumTableSent_blank
  } else if (scoreType == "raw") {
    table <- sumTableSent
  } else if (scoreType == "abs") {
    table <- sumTableSent_abs
  }
  
  # plot means
  g <- ggplot(aes(x = sent, y = mean),
              data = subset(table,
                            study == studyNum & country == countryName)) +
    geom_bar(colour = "black", fill = "gray", 
             position = "dodge", stat = "identity") +
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing trial types across age groups
plotSentCompAge <- function(studyNum, scoreType, blank = F) {
  
  # select which table to use
  if (blank == T) {
    table <- sumTableSent_blank
  } else if (scoreType == "raw") {
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
    geom_bar(colour = "black", fill = "gray", 
             position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing trial types across race/ethnicity
plotSentCompRE <- function(studyNum, scoreType, blank = F) {
  
  # select which table to use
  if (blank == T) {
    table <- sumTableSentRE_blank
  } else if (scoreType == "raw") {
    table <- sumTableSentRE
  } else if (scoreType == "abs") {
    table <- sumTableSentRE_abs
  }
  
  # plot means
  g <- ggplot(aes(x = sent, y = mean), 
              data = subset(table, 
                            study == studyNum[1] | study == studyNum[2])) +
    facet_grid(raceEthn2 ~ .,
               labeller = labeller(raceEthn2 = c("of-color" = 
                                                   "Children of Color", 
                                                 "white" = "White Children"))) +
    geom_bar(colour = "black", fill = "gray", 
             position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing trial types across countries
plotSentCompCountry <- function(studyNum, scoreType, blank = F) {
  
  # select which table to use
  if (blank == T) {
    table <- sumTableSent_blank
  } else if (scoreType == "raw") {
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
    geom_bar(colour = "black", fill = "gray", 
             position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position = position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2)
  return(g)
}

# make plotting function comparing trial types across countries
plotSentCompFraming <- function(studyNum, scoreType, blank = F) {
  
  # select which table to use
  if (blank == T) {
    table <- sumTableSent_blank
  } else if (scoreType == "raw") {
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
    geom_bar(colour = "black", fill = "gray", 
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
    theme(text = element_text(size = 28),
          axis.text = element_text(size = 21),
          axis.title = element_text(size = 28),
          axis.text.x = element_text(size = 28),
          legend.position = "top",
          legend.text = element_text(size = 21),
          legend.title = element_text(size = 21),
          plot.title = element_text(size = 28),
          strip.text = element_text(size = 28)) +
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
  mutate(sent = ifelse(factCat == "phy" | questionCat == "phy",
                       "inanimate", "sentient-only"),
         within = ifelse(factCat == questionCat,
                         1, 2)) %>% # used for ordering bars in graphs (1 = within)
  group_by(study, ageGroup, country, framing, 
           sent, within, factCat, questionCat) %>%
  summarise(mean = mean(response, na.rm = T),
            sd = sd(response, na.rm = T),
            n = length(response[is.na(response) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError) %>%
  refactor()
sumTableQP

# make summary table by fact-question pair (separate by race/ethncity)
sumTableQPRE <- d %>%
  filter(study != "1prime" & phase == "test" & raceEthn2 != "NA") %>%
  mutate(sent = ifelse(factCat == "phy" | questionCat == "phy",
                       "inanimate", "sentient-only"),
         within = ifelse(factCat == questionCat,
                         1, 2)) %>%
  group_by(study, ageGroup, country, raceEthn2, framing, 
           sent, within, factCat, questionCat) %>%
  summarise(mean = mean(response, na.rm = T),
            sd = sd(response, na.rm = T),
            n = length(response[is.na(response) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError) %>%
  refactor()
sumTableQPRE

# ------ all possible fact-question pairings: ABSOLUTE VALUES OF SCORES -------

# make summary table by fact-question pair (collapse across race/ethncity)
sumTableQP_abs <- d %>%
  filter(study != "1prime" & phase == "test") %>%
  mutate(sent = ifelse(factCat == "phy" | questionCat == "phy",
                       "inanimate", "sentient-only"),
         within = ifelse(factCat == questionCat,
                         1, 2),
         response_abs = abs(response)) %>%
  group_by(study, ageGroup, country, framing, 
           sent, within, factCat, questionCat) %>%
  summarise(mean = mean(response_abs, na.rm = T),
            sd = sd(response_abs, na.rm = T),
            n = length(response_abs[is.na(response_abs) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError) %>%
  refactor()
sumTableQP_abs

# make summary table by fact-question pair (separate by race/ethncity)
sumTableQPRE_abs <- d %>%
  filter(study != "1prime" & phase == "test" & raceEthn2 != "NA") %>%
  mutate(sent = ifelse(factCat == "phy" | questionCat == "phy",
                       "inanimate", "sentient-only"),
         within = ifelse(factCat == questionCat,
                         1, 2),
         response_abs = abs(response)) %>%
  group_by(study, ageGroup, country, raceEthn2, framing, 
           sent, within, factCat, questionCat) %>%
  summarise(mean = mean(response_abs, na.rm = T),
            sd = sd(response_abs, na.rm = T),
            n = length(response_abs[is.na(response_abs) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError) %>%
  refactor()
sumTableQPRE_abs

# ------ all possible fact-question pairings: BLANK ---------------------------

# make BLANK summary table by fact-question pair (collapse across race/ethncity)
sumTableQP_blank <- d %>%
  filter(study != "1prime" & phase == "test") %>%
  mutate(sent = ifelse(factCat == "phy" | questionCat == "phy",
                       "inanimate", "sentient-only"),
         within = ifelse(factCat == questionCat,
                         1, 2)) %>%
  group_by(study, ageGroup, country, framing, 
           sent, within, factCat, questionCat) %>%
  summarise(mean = 0,
            sd = 0,
            n = length(response[is.na(response) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError) %>%
  refactor()
sumTableQP_blank

# make BLANK summary table by fact-question pair (separate by race/ethncity)
sumTableQPRE_blank <- d %>%
  filter(study != "1prime" & phase == "test" & raceEthn2 != "NA") %>%
  mutate(sent = ifelse(factCat == "phy" | questionCat == "phy",
                       "inanimate", "sentient-only"),
         within = ifelse(factCat == questionCat,
                         1, 2)) %>%
  group_by(study, ageGroup, country, raceEthn2, framing, 
           sent, within, factCat, questionCat) %>%
  summarise(mean = 0,
            sd = 0,
            n = length(response[is.na(response) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError) %>%
  refactor()
sumTableQPRE_blank

# ------ sentient-only vs. inanimate trials: RAW SCORES -----------------------

# make summary table by sentient-only vs. inanimate trials (collapse across race/ethncity)
sumTableSent <- d %>%
  filter(study != "1prime" & phase == "test") %>%
  mutate(sent = ifelse(factCat == "phy" | questionCat == "phy",
         "inanimate", "sentient-only"),
         within = ifelse(factCat == questionCat,
                         1, 2)) %>%
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
  filter(study != "1prime" & phase == "test") %>%
  mutate(sent = ifelse(factCat == "phy" | questionCat == "phy",
                       "inanimate", "sentient-only"), 
         within = ifelse(factCat == questionCat,
                         1, 2)) %>%
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
         within = ifelse(factCat == questionCat,
                         1, 2),
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
         within = ifelse(factCat == questionCat,
                         1, 2),
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

# ------ sentient-only vs. inanimate trials: RAW SCORES -----------------------

# make summary table by sentient-only vs. inanimate trials (collapse across race/ethncity)
sumTableSent_blank <- d %>%
  filter(study != "1prime" & phase == "test") %>%
  mutate(sent = ifelse(factCat == "phy" | questionCat == "phy",
                       "inanimate", "sentient-only"),
         within = ifelse(factCat == questionCat,
                         1, 2)) %>%
  group_by(study, ageGroup, country, framing, sent) %>%
  summarise(mean = 0,
            sd = 0,
            n = length(response[is.na(response) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError) %>%
  refactor()
sumTableSent_blank

# make summary table by fact-question pair (separate by race/ethncity)
sumTableSentRE_blank <- d %>%
  filter(study != "1prime" & phase == "test") %>%
  mutate(sent = ifelse(factCat == "phy" | questionCat == "phy",
                       "inanimate", "sentient-only"),
         within = ifelse(factCat == questionCat,
                         1, 2)) %>%
  group_by(study, ageGroup, country, raceEthn2, framing, sent) %>%
  summarise(mean = 0,
            sd = 0,
            n = length(response[is.na(response) == F])) %>%
  mutate(se = sd/sqrt(n),
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError) %>%
  refactor()
sumTableSentRE_blank

# --- PLOTS: BLANK ------------------------------------------------------------

# ------ all possible fact-question pairings: BLANK ---------------------------

# plot all possible fact-question pairings, by individual study
rb1 <- plotQPFormat(plotQP("1", "us", "adults", "raw", blank = T), "raw")
# rb1prime <- plotQPFormat(plotQP("1prime", "us", "adults", "raw", blank = T), "raw")
rb2 <- plotQPFormat(plotQP("2", "us", "children", "raw", blank = T), "raw")
rb3 <- plotQPFormat(plotQP("3", "india", "adults", "raw", blank = T), "raw")
rb4us <- plotQPFormat(plotQP("4", "us", "adults", "raw", blank = T), "raw")
rb4india <- plotQPFormat(plotQP("4", "india", "adults", "raw", blank = T), "raw")

# plot 2-way comparisons of all possible fact-quesiton pairings
rb12 <- plotQPFormat(plotQPCompAge(c("1", "2"), "raw", blank = T), "raw")
rb2re <- plotQPFormat(plotQPCompRE(c("2", "2"), "raw", blank = T), "raw")
rb13 <- plotQPFormat(plotQPCompCountry(c("1", "3"), "raw", blank = T), "raw")
rb4all <- plotQPFormat(plotQPCompCountry(c("4", "4"), "raw", blank = T), "raw")

# plot 4-way comparison of framing by country (adults)
rb134 <- plotQPFormat(plotQPCompFraming(c("1", "3", "4"), "raw", blank = T), "raw")

# ------ sentient-only vs. inanimate trials: BLANK ----------------------------

# plot sentient vs. inanimate, by individual study
ab1 <- plotSentFormat(plotSent("1", "us", "adults", "raw", blank = T), "raw")
# ab1prime <- plotSentFormat(plotSent("1prime", "us", "adults", "raw", blank = T), "raw")
ab2 <- plotSentFormat(plotSent("2", "us", "children", "raw", blank = T), "raw")
ab3 <- plotSentFormat(plotSent("3", "india", "adults", "raw", blank = T), "raw")
ab4us <- plotSentFormat(plotSent("4", "us", "adults", "raw", blank = T), "raw")
ab4india <- plotSentFormat(plotSent("4", "india", "adults", "raw", blank = T), "raw")

# plot 2-way comparisons of all possible fact-quesiton pairings
ab12 <- plotSentFormat(plotSentCompAge(c("1", "2"), "raw", blank = T), "raw")
ab2re <- plotSentFormat(plotSentCompRE(c("2", "2"), "raw", blank = T), "raw")
ab13 <- plotSentFormat(plotSentCompCountry(c("1", "3"), "raw", blank = T), "raw")
ab4all <- plotSentFormat(plotSentCompCountry(c("4", "4"), "raw", blank = T), "raw")

# plot 4-way comparison of framing by country (adults)
ab134 <- plotSentFormat(plotSentCompFraming(c("1", "3", "4"), "raw", blank = T), "raw")

# --- PLOTS: DATA -------------------------------------------------------------

# ------ all possible fact-question pairings: RAW SCORES ----------------------

# plot all possible fact-question pairings, by individual study
r1 <- plotQPFormat(plotQP("1", "us", "adults", "raw"), "raw")
# r1prime <- plotQPFormat(plotQP("1prime", "us", "adults", "raw"), "raw")
r2 <- plotQPFormat(plotQP("2", "us", "children", "raw"), "raw")
r3 <- plotQPFormat(plotQP("3", "india", "adults", "raw"), "raw")
r4us <- plotQPFormat(plotQP("4", "us", "adults", "raw"), "raw")
r4india <- plotQPFormat(plotQP("4", "india", "adults", "raw"), "raw")

# plot 2-way comparisons of all possible fact-quesiton pairings
r12 <- plotQPFormat(plotQPCompAge(c("1", "2"), "raw"), "raw")
r2re <- plotQPFormat(plotQPCompRE(c("2", "2"), "raw"), "raw")
r13 <- plotQPFormat(plotQPCompCountry(c("1", "3"), "raw"), "raw")
r4all <- plotQPFormat(plotQPCompCountry(c("4", "4"), "raw"), "raw")

# plot 4-way comparison of framing by country (adults)
r134 <- plotQPFormat(plotQPCompFraming(c("1", "3", "4"), "raw"), "raw")

# plot studies 1-3 in a row
r123 <- plotQPFormat(plotQPCompStudies123(c("1", "2", "3"), "raw"), "raw")

# ------ all possible fact-question pairings: ABSOLUTE VALUES OF SCORES -------

# plot all possible fact-question pairings, by individual study
r1_abs <- plotQPFormat(plotQP("1", "us", "adults", "abs"), "abs")
# r1prime_abs <- plotQPFormat(plotQP("1prime", "us", "adults", "abs"), "abs")
r2_abs <- plotQPFormat(plotQP("2", "us", "children", "abs"), "abs")
r3_abs <- plotQPFormat(plotQP("3", "india", "adults", "abs"), "abs")
r4us_abs <- plotQPFormat(plotQP("4", "us", "adults", "abs"), "abs")
r4india_abs <- plotQPFormat(plotQP("4", "india", "adults", "abs"), "abs")

# plot 2-way comparisons of all possible fact-quesiton pairings
r12_abs <- plotQPFormat(plotQPCompAge(c("1", "2"), "abs"), "abs")
r2re_abs <- plotQPFormat(plotQPCompRE(c("2", "2"), "abs"), "abs")
r13_abs <- plotQPFormat(plotQPCompCountry(c("1", "3"), "abs"), "abs")
r4all_abs <- plotQPFormat(plotQPCompCountry(c("4", "4"), "abs"), "abs")

# plot 4-way comparison of framing by country (adults)
r134_abs <- plotQPFormat(plotQPCompFraming(c("1", "3", "4"), "abs"), "abs")

# ------ sentient-only vs. inanimate trials: RAW SCORES -----------------------

# plot sentient vs. inanimate, by individual study
a1 <- plotSentFormat(plotSent("1", "us", "adults", "raw"), "raw")
# a1prime <- plotSentFormat(plotSent("1prime", "us", "adults", "raw"), "raw")
a2 <- plotSentFormat(plotSent("2", "us", "children", "raw"), "raw")
a3 <- plotSentFormat(plotSent("3", "india", "adults", "raw"), "raw")
a4us <- plotSentFormat(plotSent("4", "us", "adults", "raw"), "raw")
a4india <- plotSentFormat(plotSent("4", "india", "adults", "raw"), "raw")

# plot 2-way comparisons of all possible fact-quesiton pairings
a12 <- plotSentFormat(plotSentCompAge(c("1", "2"), "raw"), "raw")
a2re <- plotSentFormat(plotSentCompRE(c("2", "2"), "raw"), "raw")
a13 <- plotSentFormat(plotSentCompCountry(c("1", "3"), "raw"), "raw")
a4all <- plotSentFormat(plotSentCompCountry(c("4", "4"), "raw"), "raw")

# plot 4-way comparison of framing by country (adults)
a134 <- plotSentFormat(plotSentCompFraming(c("1", "3", "4"), "raw"), "raw")

# ------ sentient-only vs. inanimate trials: ABSOLUTE VALUES OF SCORES --------

# plot sentient vs. inanimate, by individual study
a1_abs <- plotSentFormat(plotSent("1", "us", "adults", "abs"), "abs")
# a1prime_abs <- plotSentFormat(plotSent("1prime", "us", "adults", "abs"), "abs")
a2_abs <- plotSentFormat(plotSent("2", "us", "children", "abs"), "abs")
a3_abs <- plotSentFormat(plotSent("3", "india", "adults", "abs"), "abs")
a4us_abs <- plotSentFormat(plotSent("4", "us", "adults", "abs"), "abs")
a4india_abs <- plotSentFormat(plotSent("4", "india", "adults", "abs"), "abs")

# plot 2-way comparisons of all possible fact-quesiton pairings
a12_abs <- plotSentFormat(plotSentCompAge(c("1", "2"), "abs"), "abs")
a2re_abs <- plotSentFormat(plotSentCompRE(c("2", "2"), "abs"), "abs")
a13_abs <- plotSentFormat(plotSentCompCountry(c("1", "3"), "abs"), "abs")
a4all_abs <- plotSentFormat(plotSentCompCountry(c("4", "4"), "abs"), "abs")

# plot 4-way comparison of framing by country (adults)
a134_abs <- plotSentFormat(plotSentCompFraming(c("1", "3", "4"), "abs"), "abs")

# --- PLOTS FOR MANUSCRIPT ----------------------------------------------------

# figure 1
png(file="figure_01.png",width=2000,height=650)
r123 <- plotQPFormat(plotQPCompStudies123(c("1", "2", "3"), "raw"), "raw")
r123
dev.off()
