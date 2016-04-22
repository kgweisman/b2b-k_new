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
library(langcog)

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
# - RESPONSE WORDING: "maybe yes/no"
# - ITEM SET: cb1 (affect: positive\negative valence)

# study 1' (2014-05-23) - SUPPLEMENTAL
# - EXPERIMENTAL SETTING: mturk
# - COUNTRY: us
# - AGE GROUP: adults
# - FRAMING: "does that mean...?" 
# - RESPONSE WORDING: "maybe yes/no"
# - ITEM SET: cb2 (affect: positive\negative valence & high/low arousal)

# study 2 (spring 2014 - fall 2014)
# - EXPERIMENTAL SETTING: university preschool
# - COUNTRY: us
# - AGE GROUP: children
# - FRAMING: "does that mean...?" 
# - RESPONSE WORDING: "sort of yes/no"
# - ITEM SET: cb1 (affect: positive\negative valence)

# study 3 (2014-06-17)
# - EXPERIMENTAL SETTING: mturk
# - COUNTRY: india
# - AGE GROUP: adults
# - FRAMING: "does that mean...?" 
# - RESPONSE WORDING: "maybe yes/no"
# - ITEM SET: cb1 (affect: positive\negative valence)

# study 4a (2014-06-25)
# - EXPERIMENTAL SETTING: mturk
# - COUNTRY: us
# - AGE GROUP: adults
# - FRAMING: "do you think...?" 
# - ITEM SET: cb1 (affect: positive\negative valence)

# study 4b (2014-06-25)
# - EXPERIMENTAL SETTING: mturk
# - COUNTRY: india
# - AGE GROUP: adults
# - FRAMING: "do you think...?" 
# - RESPONSE WORDING: "maybe yes/no"
# - ITEM SET: cb1 (affect: positive\negative valence)

# --- IMPORTING DATA ----------------------------------------------------------

# read in data
d = read.csv("./data/anonymized/b2b-k_adults-data_anonymized-and-randomized.csv")[-1] # delete observation numbers

# add variables
d <- d %>%
  filter(study != "1prime" & # remove alternative itemSet
         phase == "test" & # include only test trials
         response != "NA") %>% # remove NAs on 4-point responses
  # create variables for trialType and wording of responses
  mutate(sent = factor(ifelse(factCat == "phy" | questionCat == "phy",
                              "inanimate", "sentient-only")), 
         within = factor(ifelse(factCat == questionCat, 1, 2)), # (1 = within)
         respWording = factor(ifelse(ageGroup == "children", 
                                     "sort of", "maybe")),
         response_abs = abs(response)) %>%
  # reorder levels of faceting variables
  mutate(framing = factor(framing, levels = c("does that mean...?",
                                              "do you think...?")),
         country = factor(country, levels = c("us", "india")),
         ageGroup = factor(ageGroup, levels = c("adults", " children")),
         raceEthn2 = factor(raceEthn2, levels = c("white", "of-color"))) %>%
  # reorder levels of fact and question categories
  mutate(factCat = factor(factCat, levels = c("aff", "per", "aut", "phy")),
         questionCat = factor(questionCat, levels = c("aff", "per", "aut", "phy")))

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
             position = position_dodge(0.7), width = 0.7, stat = "identity") + 
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), # 95% CI
                  position = position_dodge(0.7), width = .2, size = .3) +
    scale_y_continuous(breaks = seq(-1.5, 1.5, 1)
#                        ,
#                        labels = c('"Really No"\n(-1.5)', '"Maybe No"\n(-0.5)',
#                                   '"Maybe Yes"\n(0.5)', '"Really Yes"\n(1.5)')
                       )
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
             position = position_dodge(0.7), width = 0.7, stat = "identity") + 
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), # 95% CI
                  position = position_dodge(0.7), width = .2, size = .3) +
    scale_y_continuous(breaks = seq(-1.5, 1.5, 1)
#                        ,
#                        labels = c('"Really No"\n(-1.5)', '"Maybe No"\n(-0.5)',
#                                   '"Maybe Yes"\n(0.5)', '"Really Yes"\n(1.5)')
                       )
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
             position = position_dodge(0.7), width = 0.7, stat = "identity") + 
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), # 95% CI
                  position = position_dodge(0.7), width = .2, size = .3) +
    scale_y_continuous(breaks = seq(-1.5, 1.5, 1)
#                        ,
#                        labels = c('"Really No"\n(-1.5)', '"Maybe No"\n(-0.5)',
#                                   '"Maybe Yes"\n(0.5)', '"Really Yes"\n(1.5)')
                       )
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
             position = position_dodge(0.7), width = 0.7, stat = "identity") + 
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), # 95% CI
                  position = position_dodge(0.7), width = .2, size = .3) +
    scale_y_continuous(breaks = seq(-1.5, 1.5, 1)
#                        ,
#                        labels = c('"Really No"\n(-1.5)', '"Maybe No"\n(-0.5)',
#                                   '"Maybe Yes"\n(0.5)', '"Really Yes"\n(1.5)')
                       )
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
                 framing = c("do you think...?" = '"Do you think...?"',
                             "does that mean...?" = '"Does that mean...?"'))) +
    geom_bar(aes(fill = questionCat), colour = "black",
             position = position_dodge(0.7), width = 0.7, stat = "identity") + 
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), # 95% CI
                  position = position_dodge(0.7), width = .2, size = .3) +
    scale_y_continuous(breaks = seq(-1.5, 1.5, 1)
#                        ,
#                        labels = c('"Really No"\n(-1.5)', '"Maybe No"\n(-0.5)',
#                                   '"Maybe Yes"\n(0.5)', '"Really Yes"\n(1.5)')
                       )
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
    facet_grid(study ~ .,
               labeller = labeller(
                 study = c("1" = "US Adults", "2" = "US Children",
                             "3" = "Indian Adults"))) +
    geom_bar(aes(fill = questionCat), colour = "black",
             position = position_dodge(0.7), width = 0.7, stat = "identity") + 
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), # 95% CI
                  position = position_dodge(0.7), width = .2, size = .3) +
    scale_y_continuous(breaks = seq(-1.5, 1.5, 1)
#                        ,
#                        labels = c('"Really No"\n(-1.5)', '"Maybe No"\n(-0.5)',
#                                   '"Maybe Yes"\n(0.5)', '"Really Yes"\n(1.5)')
                       )
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
    ylim = c(-1.6, 1.6)
  } else if (scoreType == "abs") {
    ylim = c(0, 1.6)
  }
  
  # plot
  g <- plot +
        coord_cartesian(ylim = ylim) +
        theme_bw() +
        theme(text = element_text(size = 48),
              axis.title = element_text(size = 48),
              axis.text = element_text(size = 36),
              axis.text.x = element_text(size = 48),
              axis.text.y = element_text(angle = 0, hjust = .5),
              legend.position = "top",
              legend.text = element_text(size = 36),
              legend.title = element_text(size = 36),
              plot.title = element_text(size = 48),
              strip.text = element_text(size = 48)) +
        labs(x = "FACT Category", 
             y = "Mean Rating\n") +
      scale_x_discrete(labels = c("\nAffect", "\nPerception", "\nAutonomy", 
                                  "\nInanimate\nMaterial")) +
      scale_fill_manual(name = "QUESTION Category", 
                        labels = c(" Affect ", " Perception ", " Autonomy ", 
                                   " Inanimate Material "),
                        values = c(brewer.pal(3, "Set2")[c(2, 3, 1)], "grey")) 

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
             position = position_dodge(0.7), width = 0.5, stat = "identity") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), # 95% CI
                  position = position_dodge(0.7), width = .2, size = .3) +
    scale_y_continuous(breaks = seq(-1.5, 1.5, 1)
#                        ,
#                        labels = c('"Really No"\n(-1.5)', '"Maybe No"\n(-0.5)',
#                                   '"Maybe Yes"\n(0.5)', '"Really Yes"\n(1.5)')
                       )
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
             position = position_dodge(0.7), width = 0.5, stat = "identity") + 
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), # 95% CI
                  position = position_dodge(0.7), width = .2, size = .3) +
    scale_y_continuous(breaks = seq(-1.5, 1.5, 1)
#                        ,
#                        labels = c('"Really No"\n(-1.5)', '"Maybe No"\n(-0.5)',
#                                   '"Maybe Yes"\n(0.5)', '"Really Yes"\n(1.5)')
                       )
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
             position = position_dodge(0.7), width = 0.5, stat = "identity") + 
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), # 95% CI
                  position = position_dodge(0.7), width = .2, size = .3) +
    scale_y_continuous(breaks = seq(-1.5, 1.5, 1)
#                        ,
#                        labels = c('"Really No"\n(-1.5)', '"Maybe No"\n(-0.5)',
#                                   '"Maybe Yes"\n(0.5)', '"Really Yes"\n(1.5)')
                       )
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
             position = position_dodge(0.7), width = 0.5, stat = "identity") + 
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), # 95% CI
                  position = position_dodge(0.7), width = .2, size = .3) +
    scale_y_continuous(breaks = seq(-1.5, 1.5, 1)
#                        ,
#                        labels = c('"Really No"\n(-1.5)', '"Maybe No"\n(-0.5)',
#                                   '"Maybe Yes"\n(0.5)', '"Really Yes"\n(1.5)')
                       )
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
                 framing = c("do you think...?" = '"Do you think...?"',
                             "does that mean...?" = '"Does that mean...?"'))) +
    geom_bar(colour = "black", fill = "gray", 
             position = position_dodge(0.7), width = 0.5, stat = "identity") + 
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), # 95% CI
                  position = position_dodge(0.7), width = .2, size = .3) +
    scale_y_continuous(breaks = seq(-1.5, 1.5, 1)
#                        ,
#                        labels = c('"Really No"\n(-1.5)', '"Maybe No"\n(-0.5)',
#                                   '"Maybe Yes"\n(0.5)', '"Really Yes"\n(1.5)')
                       )
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
    ylim = c(-1.6, 1.6)
  } else if (scoreType == "abs") {
    ylim = c(0, 1.6)
  }
  
  # plot
  g <- plot +
    coord_cartesian(ylim = ylim) +
    theme_bw() +
    theme(text = element_text(size = 48),
          axis.text = element_text(size = 36),
          axis.title = element_text(size = 48),
          axis.text.x = element_text(size = 48),
          axis.text.y = element_text(angle = 10),
          legend.position = "top",
          legend.text = element_text(size = 36),
          legend.title = element_text(size = 36),
          plot.title = element_text(size = 48),
          strip.text = element_text(size = 48)) +
    labs(x = "Trial Type", 
         y = "Mean Rating\n") +
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

sumGrpsQP <- c("study", "country", "ageGroup", "framing", "respWording", 
               "factCat", "questionCat")

sumGrpsQPRE <- c(sumGrpsQP, "raceEthn2")

sumGrpsSent <- c("study", "country", "ageGroup", "framing", "respWording", "sent")

sumGrpsSentRE <- c(sumGrpsSent, "sent", "raceEthn2")

# ------ all possible fact-question pairings: RAW SCORES ----------------------

# make summary table by fact-question pair 
# (collapse across race/ethnicity)
sumTableQP <- multi_boot.data.frame(data = d,
                                    summary_function = "mean",
                                    column = "response",
                                    summary_groups = sumGrpsQP,
                                    statistics_functions = c("ci_lower", 
                                                             "mean", 
                                                             "ci_upper"))

# make summary table by fact-question pair 
# (separate by race/ethncity)
sumTableQPRE <- multi_boot.data.frame(data = d %>% 
                                        filter(raceEthn2 != "NA"),
                                      summary_function = "mean",
                                      column = "response",
                                      summary_groups = sumGrpsQPRE,
                                      statistics_functions = c("ci_lower",
                                                               "mean",
                                                               "ci_upper"))

# ------ all possible fact-question pairings: ABSOLUTE VALUES OF SCORES -------

# make summary table by fact-question pair 
# (collapse across race/ethnicity)
sumTableQP_abs <- multi_boot.data.frame(data = d,
                                    summary_function = "mean",
                                    column = "response_abs",
                                    summary_groups = sumGrpsQP,
                                    statistics_functions = c("ci_lower", 
                                                             "mean", 
                                                             "ci_upper"))

# make summary table by fact-question pair 
# (separate by race/ethncity)
sumTableQPRE_abs <- multi_boot.data.frame(data = d %>% 
                                            filter(raceEthn2 != "NA"),
                                      summary_function = "mean",
                                      column = "response_abs",
                                      summary_groups = sumGrpsQPRE,
                                      statistics_functions = c("ci_lower",
                                                               "mean",
                                                               "ci_upper"))

# ------ all possible fact-question pairings: BLANK ---------------------------

# make BLANK summary table by fact-question pair 
# (collapse across race/ethncity)
sumTableQP_blank <- sumTableQP %>%
  mutate(ci_lower = 0,
         mean = 0,
         ci_upper = 0)

# make BLANK summary table by fact-question pair 
# (separate by race/ethncity)
sumTableQPRE_blank <- sumTableQPRE %>%
  mutate(ci_lower = 0,
         mean = 0,
         ci_upper = 0)

# ------ sentient-only vs. inanimate trials: RAW SCORES -----------------------

# make summary table by sentient-only vs. inanimate trials 
# (collapse across race/ethncity)
sumTableSent <- multi_boot.data.frame(data = d,
                                        summary_function = "mean",
                                        column = "response",
                                        summary_groups = sumGrpsSent,
                                        statistics_functions = c("ci_lower", 
                                                                 "mean", 
                                                                 "ci_upper"))

# make summary table by fact-question pair 
# (separate by race/ethncity)
sumTableSentRE <- multi_boot.data.frame(data = d,
                                      summary_function = "mean",
                                      column = "response",
                                      summary_groups = sumGrpsSentRE,
                                      statistics_functions = c("ci_lower", 
                                                               "mean", 
                                                               "ci_upper"))

# ------ sentient-only vs. inanimate trials: ABSOLUTE VALUES OF SCORES --------

# make summary table by sentient-only vs. inanimate trials 
# (collapse across race/ethncity)
sumTableSent_abs <- multi_boot.data.frame(data = d,
                                      summary_function = "mean",
                                      column = "response_abs",
                                      summary_groups = sumGrpsSent,
                                      statistics_functions = c("ci_lower", 
                                                               "mean", 
                                                               "ci_upper"))

# make summary table by fact-question pair 
# (separate by race/ethncity)
sumTableSentRE_abs <- multi_boot.data.frame(data = d,
                                            summary_function = "mean",
                                            column = "response_abs",
                                            summary_groups = sumGrpsSentRE,
                                            statistics_functions = 
                                              c("ci_lower", 
                                                "mean", 
                                                "ci_upper"))

# ------ sentient-only vs. inanimate trials: BLANK ----------------------------

# make summary table by sentient-only vs. inanimate trials 
# (collapse across race/ethncity)
sumTableSent_blank <- sumTableSent %>%
  mutate(ci_lower = 0,
         mean = 0,
         ci_upper = 0)

# make summary table by fact-question pair 
# (separate by race/ethncity)
sumTableSentRE_blank <- sumTableSentRE %>%
  mutate(ci_lower = 0,
         mean = 0,
         ci_upper = 0)

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

# make "soup-up" function for annotating plots
plotSoup <- function(g, studies) {
  dLabels <- d %>%
    select(study, country, ageGroup, framing, itemSet, respWording, 
           factCat, questionCat) %>%
    filter(factCat == "aff" & questionCat == "aff") %>%
    distinct() %>%
    filter(study %in% studies)

  g <- g +
    theme(panel.margin.y = grid::unit(5, "lines"),
          panel.margin.x = grid::unit(2, "lines")) +
#     geom_text(data = dLabels,
#              aes(x = -Inf, y = -1.5,
#                  label = '"Really No"'),
#              hjust = -0.25, vjust = -0.5, size = 10) +
#     geom_text(data = dLabels,
#               aes(x = -Inf, y = -.5,
#                   label = paste0('"', 
#                                  R.utils::capitalize(respWording), 
#                                  ' no"')),
#               hjust = -0.25, vjust = -0.5, size = 10) +
#     geom_text(data = dLabels,
#               aes(x = Inf, y = .5,
#                   label = paste0('"', 
#                                  R.utils::capitalize(respWording), 
#                                  ' yes"')),
#               hjust = 1.25, vjust = 1.5, size = 10) +
#     geom_text(data = dLabels,
#               aes(x = Inf, y = 1.5,
#                   label = '"Really Yes"'),
#               hjust = 1.25, vjust = 1.5, size = 10) +
    geom_hline(yintercept = 0, linetype = 1) +
    geom_hline(yintercept = -1.5, linetype = 3) +
    geom_hline(yintercept = -0.5, linetype = 3) +
    geom_hline(yintercept = 0.5, linetype = 3) +
    geom_hline(yintercept = 1.5, linetype = 3)
  return(g)
}

# figure 1
png(file="figure_01.png",width=1800,height=2000)
plotSoup(r123, studies = c(1:3))
dev.off()

# figure 2
png(file="figure_02.png",width=1800,height=1400)
plotSoup(r2re, studies = 2)
dev.off()

# figure 3
png(file="figure_03.png",width=1800,height=1400)
plotSoup(r4all, studies = 4)
dev.off()

# # figure 4
# png(file="figure_04.png",width=2540,height=1400)
# plotSoup(r134, studies = c(1,3:4))
# dev.off()
