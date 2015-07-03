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

# --- SETUP -------------------------------------------------------------------

# make summary table
sumTable <- d %>%
  filter(study != "1prime" & phase == "test") %>%
  group_by(study, ageGroup, country, framing, factCat, questionCat) %>%
  summarise(mean = mean(response, na.rm = T),
            sd = sd(response, na.rm = T),
            n = length(response[is.na(response) == F])) %>%
  mutate(se = sd/n,
         marginError = (qt(1 - (.05/2), df = n - 1)) * se,
         lowerB = mean - marginError,
         upperB = mean + marginError)
sumTable

# make plotting function for plotting all possible fact-question pairings
plotFQP <- function(studyNum, countryName, ageGroup) {
  g <- ggplot(aes(x = factCat, y = mean, group = questionCat), 
              data = subset(sumTable, 
                            study == studyNum & country == countryName)) +
    coord_cartesian(ylim = c(-1.5, 1.5)) +
    theme_bw() +
    theme(text = element_text(size = 20),
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          legend.position = "top",
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 15),
          plot.title = element_text(size = 20)) +
    geom_bar(aes(fill = questionCat), position = "dodge", stat = "identity") + 
    geom_errorbar(aes(ymin = lowerB, ymax = upperB), # 95% CI
                  position=position_dodge(0.9), width = .2, size = .3) +
    geom_hline(yintercept = 0, linetype = 2) +
    labs(title = paste0("Inferences by Fact and Question Category: Study ", studyNum, " (", ifelse(countryName == "us", "US ", "Indian "), str_to_title(ageGroup), ")"), 
         x = "Fact Category", 
         y = "Mean Rating (-1.5 = Really No, 1.5 = Really Yes)\n") +
    scale_x_discrete(labels = c("Affect", "Autonomy", "Perception", "Inanimate\nMaterial")) +
    scale_fill_grey(name = "Question Category", 
                    labels = c(" Affect ", " Autonomy ", 
                                 " Perception ", " Inanimate Material "))
  return(g)
}

# ------ PLOTS ----------------------------------------------------------------

# all possible fact-question pairings
g1 <- plotFQP("1", "us", "adults"); g1
# g1prime <- plotFQP("1prime", "us", "adults"); g1prime
g2 <- plotFQP("2", "us", "children"); g2
g3 <- plotFQP("3", "india", "adults"); g3
g4us <- plotFQP("4", "us", "adults"); g4us
g4india <- plotFQP("4", "india", "adults"); g4india
