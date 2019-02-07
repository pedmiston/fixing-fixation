# ---- exp2
library(tidyverse)
library(magrittr)

library(totems)

# Subjects ----
data("Sessions")
data("SubjInfoSheets")

DiachronicSubjs <- SubjInfoSheets$`tot-fall-18` %>%
  drop_na(InheritedID) %>%
  mutate(
    PlayerID = paste0("P", SubjID),
    SessionID = paste0("S", SubjID),
    InheritedID = paste0("S", InheritedID),
    Strategy = "Diachronic"
  ) %>%
  select(PlayerID, SessionID, Strategy, InheritedID)

DiachronicSubjGenerations <- Sessions %>%
  filter(
    SessionID %in% DiachronicSubjs$InheritedID
  ) %>%
  mutate(
    # Diachronic subj generations are the generation they inherited + 1
    Generation = Generation + 1
  ) %>%
  select(InheritedID = SessionID, Generation)

DiachronicSubjs %<>% left_join(DiachronicSubjGenerations)

IsolatedPlayerIDs <- Sessions %>%
  filter(
    SessionID %in% DiachronicSubjs$InheritedID
  ) %>%
  .$PlayerID %>%
  unique()
IsolatedSubjs <- Sessions %>%
  filter(
    PlayerID %in% IsolatedPlayerIDs,
    (Generation == 4 | SessionID %in% DiachronicSubjs$InheritedID)
  ) %>%
  mutate(
    InheritedID = NA
  ) %>%
  select(PlayerID, SessionID, Strategy, Generation, InheritedID)

YokedSessions <- bind_rows(DiachronicSubjs, IsolatedSubjs)

filter_yoked <- function(frame) {
  frame %>%
    filter(SessionID %in% YokedSessions$SessionID)
}

# Innovations ----
data("Guesses")

Innovations <- Guesses %>%
  filter_yoked() %>%
  recode_guess_type(unique_guess = "UniqueSessionGuess", unique_result = "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(NumInnovations = sum(GuessType == "unique_item")) %>%
  left_join(YokedSessions)

yoked_by_generation_plot <- ggplot(Innovations) +
  aes(Generation, NumInnovations) +
  geom_bar(aes(fill = Strategy),
           position = "dodge",
           stat = "summary",
           fun.y = "mean")

# Fixation ----
data("Guesses")
data("Sessions")
data("AdjacentItems")

IndividualPlayers <- Sessions %>%
  filter_yoked()

IndividualGuesses <- Guesses %>%
  filter_yoked() %>%
  left_join(AdjacentItems, by = c("PrevSessionInventoryID" = "ID")) %>%
  label_stage_ix()

FirstDiscovery <- IndividualGuesses %>%
  label_inheritance() %>%
  filter(StageIX == 0, Inheritance != "no_inheritance") %>%
  group_by(SessionID) %>%
  summarize(
    NumGuesses = n()
  ) %>%
  ungroup() %>%
  left_join(IndividualPlayers) %>%
  highlight_inheritance_100() %>%
  # Should not be necessary!
  filter(!is.na(Inheritance)) %>%
  recode_inheritance() %>%
  recode_strategy()

first_discovery_mod <- lm(NumGuesses ~ Diachronic_v_Individual,
                          data = FirstDiscovery)
#exp1$first_discovery_mean <- round(mean(FirstDiscovery$NumGuesses), 0)
#exp1$first_discovery_beta <- report_beta(first_discovery_mod, "Diachronic_v_Individual")
#exp1$first_discovery_stats <- report_lm_mod(first_discovery_mod, "Diachronic_v_Individual")

first_discovery_preds <- recode_inheritance() %>%
  filter(Inheritance != "no_inheritance") %>%
  mutate(Strategy = c("Diachronic", "Isolated")) %>%
  cbind(., predict(first_discovery_mod, newdata = ., se = TRUE)) %>%
  rename(NumGuesses = fit, SE = se.fit) %>%
  recode_strategy()

first_discovery_plot <- ggplot(FirstDiscovery) +
  aes(StrategyLabel, NumGuesses) +
  geom_bar(aes(fill = StrategyLabel),
           stat = "summary", fun.y = "mean",
           alpha = 0.6) +
  geom_errorbar(aes(ymin = NumGuesses - SE, ymax = NumGuesses + SE),
                width = 0.2, data = first_discovery_preds) +
  scale_y_continuous("Number of guesses") +
  t_$scale_color_strategy +
  t_$scale_fill_strategy +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())

first_discovery_by_generation_mod <- lmer(NumGuesses ~ Diachronic_v_Individual * Generation +
                                            (Generation|TeamID),
                                          data = FirstDiscovery)
exp1$first_discovery_by_gen_stats <- report_lmer_mod(first_discovery_by_generation_mod,
                                                     "Diachronic_v_Individual:Generation")

first_discovery_by_generation_preds <- expand.grid(
  Generation = 2:4, Inheritance = c("diachronic_inheritance", "individual_inheritance"),
  stringsAsFactors = FALSE
) %>%
  recode_inheritance() %>%
  mutate(Strategy = rep(c("Diachronic", "Isolated"), each = 3)) %>%
  recode_strategy() %>%
  cbind(., predictSE(first_discovery_by_generation_mod, newdata = ., se = TRUE)) %>%
  rename(NumGuesses = fit, SE = se.fit)

first_discovery_by_generation_plot <- ggplot(FirstDiscovery) +
  aes(Generation, NumGuesses) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.1, height = 0)) +
  geom_bar(aes(fill = StrategyLabel, group = factor(Generation)),
           data = first_discovery_by_generation_preds,
           stat = "identity",
           alpha = 0.6, width = 0.8) +
  geom_errorbar(aes(ymin = NumGuesses-SE, ymax = NumGuesses+SE),
                data = first_discovery_by_generation_preds,
                width = 0.2) +
  facet_wrap("Strategy") +
  scale_x_continuous(breaks = 2:4) +
  scale_y_continuous("Number of guesses") +
  scale_fill_manual(values = t_$color_picker(c("orange", "blue"))) +
  scale_color_manual(values = t_$color_picker(c("orange", "blue"))) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
