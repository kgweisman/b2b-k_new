frisemTable <- sumTableSentRE %>%
  filter(study %in% 1:3) %>%
  mutate(population = ifelse(study == "2" & raceEthn2 == "white", 
                             "White Children (n = 22)",
                             ifelse(study == "2" & raceEthn2 == "of-color", 
                                    "Children of Color (n = 36)",
                                    ifelse(study == "1",
                                           "US Adults (n = 80)",
                                           ifelse(study == "3",
                                                  "Indian Adults (n = 80)",
                                                  NA))))) %>%
  filter(is.na(population) == F) %>%
  ungroup() %>%
  mutate(population = factor(population, 
                             levels = c("US Adults (n = 80)",
                                        "Indian Adults (n = 80)",
                                        "White Children (n = 22)",
                                        "Children of Color (n = 36)")),
         sent = factor(sent, 
                       levels = c("sentient-only", "inanimate")))

frisemTableBlank <- frisemTable %>%
  mutate(mean = 0,
         ci_lower = 0,
         ci_upper = 0)

frisemTableHalf <- frisemTable %>%
  mutate(mean = ifelse(sent == "sentient-only", mean, 0),
         ci_lower = ifelse(sent == "sentient-only", ci_lower, 0),
         ci_upper = ifelse(sent == "sentient-only", ci_upper, 0))
         
ggplot(data = frisemTable, aes(x = sent, y = mean, fill = sent)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(0.7), width = .2, size = .3) +
  facet_wrap(~population, nrow = 2) +
  theme_bw() +
  theme(text = element_text(size = 30),
        legend.position = "none") +
  labs(x = "\nTrial Type", 
       y = "Mean Rating\n") +
  scale_fill_brewer(type = "qual", palette = 2) +
  scale_x_discrete(labels = c("Sentient-Only", "Inanimate")) +
  scale_y_continuous(limits = c(-1.55, 1.55),
                     breaks = seq(-1.5, 1.5, 1)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -1.5, lty = 2) +
  geom_hline(yintercept = -0.5, lty = 2) +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_hline(yintercept = 1.5, lty = 2)

ggplot(data = frisemTableBlank, aes(x = sent, y = mean, fill = sent)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(0.7), width = .2, size = .3) +
  facet_wrap(~population, nrow = 2) +
  theme_bw() +
  theme(text = element_text(size = 30),
        legend.position = "none") +
  labs(x = "\nTrial Type", 
       y = "Mean Rating\n") +
  scale_fill_brewer(type = "qual", palette = 2) +
  scale_x_discrete(labels = c("Sentient-Only", "Inanimate")) +
  scale_y_continuous(limits = c(-1.55, 1.55),
                     breaks = seq(-1.5, 1.5, 1)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -1.5, lty = 2) +
  geom_hline(yintercept = -0.5, lty = 2) +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_hline(yintercept = 1.5, lty = 2)

ggplot(data = frisemTableHalf, aes(x = sent, y = mean, fill = sent)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(0.7), width = .2, size = .3) +
  facet_wrap(~population, nrow = 2) +
  theme_bw() +
  theme(text = element_text(size = 30),
        legend.position = "none") +
  labs(x = "\nTrial Type", 
       y = "Mean Rating\n") +
  scale_fill_brewer(type = "qual", palette = 2) +
  scale_x_discrete(labels = c("Sentient-Only", "Inanimate")) +
  scale_y_continuous(limits = c(-1.55, 1.55),
                     breaks = seq(-1.5, 1.5, 1)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -1.5, lty = 2) +
  geom_hline(yintercept = -0.5, lty = 2) +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_hline(yintercept = 1.5, lty = 2)

  