# --- PRELIMINARIES -----------------------------------------------------------

# NOTE: MUST RUN ANALYSIS FILE FIRST!

# set studies
studies <- c(2)

# set groups to group by
# format: groups <- list(~country, ~framing)
groups <- list(~gender, ~ageCat)

# -- SET UP DATASET with contrasts --------------------------------------------
dSummaries <- d %>%
  filter(study %in% studies & phase == "test") %>%
  mutate(sent = ifelse(factCat == "phy" | questionCat == "phy",
                              "inanimate",
                              "sentient_only"),
         within = ifelse(sent == "sentient_only" & factCat == questionCat,
                         "s_within",
                         ifelse(sent == "sentient_only",
                                "s_between",
                                NA)),
         f_aff = ifelse(factCat == "aff",
                        "f_aff",
                        "f_othrs"),
         f_aut = ifelse(factCat == "aut",
                        "f_aut",
                        ifelse(factCat == "per",
                               "f_per",
                               NA)),
         q_aff = ifelse(questionCat == "aff",
                        "q_aff",
                        "q_othrs"),
         q_aut = ifelse(questionCat == "aut",
                        "q_aut",
                        ifelse(questionCat == "per",
                               "q_per",
                               NA)),
         phyphy = ifelse(factCat == "phy" & questionCat == "phy",
                         "phyphy",
                         ifelse(sent == "inanimate",
                                "othrs",
                                NA))) %>%
  mutate(sent = factor(sent, levels = c("sentient_only", "inanimate")),
         framing = factor(ifelse(framing == "does that mean...?", "logical",
                                 ifelse(framing == "do you think...?", "opinion",
                                        NA))),
         framing = factor(framing, levels = c("logical", "opinion")),
         raceEthn2 = factor(ifelse(raceEthn2 == "of-color", "of_color",
                                   ifelse(raceEthn2 == "white", "white",
                                          NA))),
         raceEthn2 = factor(raceEthn2, levels = c("white", "of_color")))

if (TRUE %in% grepl("raceEthn2", groups)) {
  dSummaries <- dSummaries %>%
    filter(raceEthn2 != "NA")
}

if (TRUE %in% grepl("ageCat", groups)) {
  dSummaries <- dSummaries %>%
    mutate(ageCat = ifelse(age == "NA", NA,
                           ifelse(age < median(age, na.rm = T), "young",
                                  "old")))
}

# -- GET MEANS AND DIFFERENCES by grouping variables --------------------------

# pairsent.inanim
pairsent.inanim <- dSummaries %>%
  group_by_(.dots = c(groups, ~sent)) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  spread(sent, mean) %>%
  mutate(diff = sentient_only - inanimate)

# pairsnt_within
pairsnt_within <- dSummaries %>% 
  filter(sent == "sentient_only") %>%
  group_by_(.dots = c(groups, ~within)) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  spread(within, mean) %>%
  mutate(diff = s_within - s_between)

# pairsnt_f_aff.othrs
pairsnt_f_aff.othrs <- dSummaries %>% 
  filter(sent == "sentient_only") %>%
  group_by_(.dots = c(groups, ~f_aff)) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  spread(f_aff, mean) %>%
  mutate(diff = f_aff - f_othrs)

# pairsnt_f_aut.per
pairsnt_f_aut.per <- dSummaries %>% 
  filter(sent == "sentient_only" & factCat != "aff") %>%
  group_by_(.dots = c(groups, ~f_aut)) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  spread(f_aut, mean) %>%
  mutate(diff = f_aut - f_per)

# pairsnt_q_aff.othrs
pairsnt_q_aff.othrs <- dSummaries %>% 
  filter(sent == "sentient_only") %>%
  group_by_(.dots = c(groups, ~q_aff)) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  spread(q_aff, mean) %>%
  mutate(diff = q_aff - q_othrs)

# pairsnt_q_aut.per
pairsnt_q_aut.per <- dSummaries %>% 
  filter(sent == "sentient_only" & questionCat != "aff") %>%
  group_by_(.dots = c(groups, ~q_aut)) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  spread(q_aut, mean) %>%
  mutate(diff = q_aut - q_per)

# pairinan_phyphy.othrs
pairinan_phyphy.othrs <- dSummaries %>% 
  filter(sent == "inanimate") %>%
  group_by_(.dots = c(groups, ~phyphy)) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  spread(phyphy, mean) %>%
  mutate(diff = phyphy - othrs)

# pairinan_f_aff.othrs
pairinan_f_aff.othrs <- dSummaries %>% 
  filter(sent == "inanimate" & factCat != "phy") %>%
  group_by_(.dots = c(groups, ~f_aff)) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  spread(f_aff, mean) %>%
  mutate(diff = f_aff - f_othrs)

# pairinan_f_aut.per
pairinan_f_aut.per <- dSummaries %>% 
  filter(sent == "inanimate" & factCat != "phy" & factCat != "aff") %>%
  group_by_(.dots = c(groups, ~f_aut)) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  spread(f_aut, mean) %>%
  mutate(diff = f_aut - f_per)

# pairinan_q_aff.othrs
pairinan_q_aff.othrs <- dSummaries %>% 
  filter(sent == "inanimate" & questionCat != "phy") %>%
  group_by_(.dots = c(groups, ~q_aff)) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  spread(q_aff, mean) %>%
  mutate(diff = q_aff - q_othrs)

# pairinan_q_aut.per
pairinan_q_aut.per <- dSummaries %>% 
  filter(sent == "inanimate" & questionCat != "phy" & questionCat != "aff") %>%
  group_by_(.dots = c(groups, ~q_aut)) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  spread(q_aut, mean) %>%
  mutate(diff = q_aut - q_per)

# grouping variable 1
grouping1 <- dSummaries %>%
  group_by_(.dots = c(~phase, groups[[1]])) %>% # phase is a dummy
  summarise(mean = mean(response, na.rm = T)) %>%
  spread_(substring(groups[[1]], 1)[2], "mean")
temp1 <- as.name(names(grouping1)[2])
if (temp1 == "us") {
  grouping1 <- grouping1 %>%
    mutate(diff = us - india)
} else if (temp1 == "adults") {
  grouping1 <- grouping1 %>%
    mutate(diff = adults - children)
} else if (temp1 == "does that mean...?") {
  grouping1 <- grouping1 %>%
    mutate(diff = "does that mean...?" - "do you think...?")
} else if (temp1 == "female") {
  grouping1 <- grouping1 %>%
    mutate(diff = female - male)
} else if (temp1 == "white") {
  grouping1 <- grouping1 %>%
    mutate(diff = white - of_color)
} else if (temp1 == "old") {
  group1 <- grouping1 %>%
    mutate(diff = old - young)
}

# grouping variable 2
if (length(groups) > 1) {
  grouping2 <- dSummaries %>%
    group_by_(.dots = c(~phase, groups[[2]])) %>% # phase is a dummy
    summarise(mean = mean(response, na.rm = T)) %>%
    spread_(substring(groups[[2]], 1)[2], "mean")
  temp2 <- as.name(names(grouping2)[2])
  if (temp2 == "us") {
    grouping2 <- grouping2 %>%
      mutate(diff = us - india)
  } else if (temp2 == "adults") {
    grouping2 <- grouping2 %>%
      mutate(diff = adults - children)
  } else if (temp2 == "logical") {
    grouping2 <- grouping2 %>%
      mutate(diff = logical - opinion)
  } else if (temp2 == "female") {
    grouping2 <- grouping2 %>%
      mutate(diff = female - male)
  } else if (temp2 == "white") {
    grouping2 <- grouping2 %>%
      mutate(diff = white - of_color)
  } else if (temp2 == "old") {
    grouping2 <- grouping2 %>%
      mutate(diff = old - young)
  }
} else {
  rm(temp2, grouping2)
}

# -- PRINT ALL ----------------------------------------------------------------
pairsent.inanim
pairsnt_within

pairsnt_f_aff.othrs
pairsnt_f_aut.per
pairsnt_q_aff.othrs
pairsnt_q_aut.per

pairinan_phyphy.othrs
pairinan_f_aff.othrs
pairinan_f_aut.per
pairinan_q_aff.othrs
pairinan_q_aut.per

grouping1
grouping2
