# ---- fixing-fixation
library(magrittr)
library(broom)
library(lme4)
library(AICcmodavg)
library(gridExtra)
library(crotchet)
library(totems)
library(tidyverse) # Load tidyverse after totems to prevent dplyr::filter from being masked
t_ <- load_totems_theme()
# t_$annotation_size <- 2.5  # for manuscript
t_$annotation_size <- 5      # for slides
theme_set(t_$base_theme)

exp1 <- list()

# Methods ----
data("Sessions")

Exp1Participants <- Sessions %>%
  filter_selfother() %>%
  select(Strategy, PlayerID) %>%
  unique() %>%
  count(Strategy) %>%
  rename(N = n)

# Innovations by generation ----
data("Guesses")
data("Sessions")

Innovations <- Guesses %>%
  filter_selfother() %>%
  recode_guess_type(unique_guess = "UniqueSessionGuess", unique_result = "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(NumInnovations = sum(GuessType == "unique_item")) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  jitter_team_generation() %>%
  recode_generation_base0() %>%
  recode_generation_quad() %>%
  recode_strategy()

innovations_gen1_mod <- lm(NumInnovations ~ Diachronic_v_Isolated,
                           data = filter(Innovations, Generation == 1))
exp1$gen1_innovations <- report_lm_mod(innovations_gen1_mod, "Diachronic_v_Isolated")

diachronic_gen_mod <- lmer(NumInnovations ~ Generation + (Generation|TeamID),
                           data = filter(Innovations, Strategy == "Diachronic"))
exp1$tools_per_diachronic_gen <- report_beta(diachronic_gen_mod, "Generation")
exp1$tools_per_diachronic_gen_stats <- report_lmer_mod(diachronic_gen_mod, "Generation")

isolated_gen_mod <- lmer(NumInnovations ~ Generation + (Generation|TeamID),
                         data = filter(Innovations, Strategy == "Isolated"))
exp1$tools_per_isolated_gen <- report_beta(isolated_gen_mod, "Generation")
exp1$tools_per_isolated_gen_stats <- report_lmer_mod(isolated_gen_mod, "Generation")

innovations_gen4_mod <- lm(NumInnovations ~ Diachronic_v_Isolated,
                           data = filter(Innovations, Generation == 4))
exp1$gen4_innovations <- report_lm_mod(innovations_gen4_mod, "Diachronic_v_Isolated")

innovations_by_generation_mod <- lmer(
  NumInnovations ~ Generation * Diachronic_v_Isolated  + (Generation|TeamID),
  data = Innovations
)

exp1$innovations_by_inheritance_slope <- report_lmer_mod(innovations_by_generation_mod, "Generation:Diachronic_v_Isolated")

innovations_by_generation_quad_mod <- lmer(
  NumInnovations ~ (Generation0 + Generation0Sqr) * Diachronic_v_Isolated + (Generation0 + Generation0Sqr|TeamID),
  data = Innovations
)
innovations_by_generation_preds <- expand.grid(
    Generation = 1:4, Strategy = c("Diachronic", "Isolated"),
    stringsAsFactors = FALSE
  ) %>%
  recode_generation_base0() %>%
  recode_generation_quad() %>%
  recode_strategy() %>%
  cbind(., predictSE(innovations_by_generation_quad_mod, newdata = ., SE = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit)

innovations_by_generation_plot <- ggplot(Innovations) +
  aes(Generation, NumInnovations, color = StrategyLabel) +
  geom_line(aes(GenerationJittered, group = TeamID, color = Strategy),
            size = 0.8, alpha = 0.6) +
  geom_ribbon(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE, fill = Strategy),
                data = innovations_by_generation_preds, alpha = 0.4,
              size = 0) +
  facet_wrap("Strategy") +
  scale_color_manual(values = t_$color_picker(c("blue", "orange"))) +
  scale_fill_manual(values = t_$color_picker(c("blue", "orange"))) +
  scale_y_continuous("Number of innovations", breaks = seq(0, 40, by = 5)) +
  theme(
    legend.position = "none",
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# Guess types ----
GuessTypesSelfOther <- Guesses %>%
  filter_selfother() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(
    NumGuesses = n(),
    NumRedundantGuesses = sum(GuessType == "redundant"),
    NumRepeatedItems = sum(GuessType == "repeat_item"),
    NumUniqueGuesses = sum(GuessType == "unique_guess"),
    NumUniqueItems = sum(GuessType == "unique_item")
  ) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  recode_strategy() %>%
  mutate(
    PropRedundantGuesses = NumRedundantGuesses/NumGuesses,
    PropRepeatedItems = NumRepeatedItems/NumGuesses,
    PropUniqueGuesses = NumUniqueGuesses/NumGuesses,
    PropUniqueItems = NumUniqueItems/NumGuesses
  ) %>%
  recode_strategy()

prop_redundant_strategy_mod <- lmer(PropRedundantGuesses ~ Diachronic_v_Isolated + (1|TeamID),
                                    data = GuessTypesSelfOther)
exp1$prop_redundant_strategy <- report_lmer_mod(prop_redundant_strategy_mod, "Diachronic_v_Isolated")

prop_redundant_gen_mod <- lmer(PropRedundantGuesses ~ Generation + (1|TeamID),
                               data = GuessTypesSelfOther)
exp1$prop_redundant_gen <- report_lmer_mod(prop_redundant_gen_mod, "Generation", formats = c(se=3))

prop_redundant_inter_mod <- lmer(PropRedundantGuesses ~ Generation * Diachronic_v_Isolated + (Generation|TeamID),
                               data = GuessTypesSelfOther)
exp1$prop_redundant_inter <- report_lmer_mod(prop_redundant_gen_mod, "Generation:Diachronic_v_Isolated", formats = c(b = 3))


prop_unique_strategy_mod <- lmer(PropUniqueGuesses ~ Diachronic_v_Isolated + (1|TeamID),
                              data = GuessTypesSelfOther)
exp1$prop_unique_strategy <- report_lmer_mod(prop_unique_strategy_mod, "Diachronic_v_Isolated")

prop_unique_gen_mod <- lmer(PropUniqueGuesses ~ Generation + (Generation|TeamID),
                              data = GuessTypesSelfOther)
exp1$prop_unique_gen <- report_lmer_mod(prop_unique_gen_mod, "Generation", formats = c(b=3))

prop_unique_inter_mod <- lmer(PropUniqueGuesses ~ Generation * Diachronic_v_Isolated + (Generation|TeamID),
                            data = GuessTypesSelfOther)
exp1$prop_unique_inter <- report_lmer_mod(prop_unique_inter_mod, "Generation:Diachronic_v_Isolated")


GuessTypesSelfOtherSummary <- Guesses %>%
  filter_selfother() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(Strategy, Generation) %>%
  summarize(
    NumGuesses = n(),
    NumRedundantGuesses = sum(GuessType == "redundant"),
    NumRepeatedItems = sum(GuessType == "repeat_item"),
    NumUniqueGuesses = sum(GuessType == "unique_guess"),
    NumUniqueItems = sum(GuessType == "unique_item")
  ) %>%
  ungroup() %>%
  mutate(
    PropRedundantGuesses = NumRedundantGuesses/NumGuesses,
    PropRepeatedItems = NumRepeatedItems/NumGuesses,
    PropUniqueGuesses = NumUniqueGuesses/NumGuesses,
    PropUniqueItems = NumUniqueItems/NumGuesses
  ) %>%
  select(Strategy, Generation, PropRedundantGuesses, PropRepeatedItems, PropUniqueGuesses, PropUniqueItems) %>%
  gather(PropGuessType, PropGuesses, -c(Strategy, Generation)) %>%
  recode_prop_guess_type_total()

prop_guess_types_selfother_plot <- ggplot(GuessTypesSelfOtherSummary) +
  aes(Generation, PropGuesses, fill = PropGuessTypeLabel) +
  geom_bar(stat = "identity") +
  facet_wrap("Strategy") +
  xlab("") +
  scale_y_continuous("Proportion of guesses", labels = scales::percent) +
  scale_fill_manual("Guess types",
                    values = t_$color_picker(c("green", "blue", "orange", "pink"))) +
  theme(panel.grid.major.x = element_blank())

# Learning times ----
StageTimesSelfOther <- Guesses %>%
  filter_selfother() %>%
  filter(Generation > 1) %>%
  group_by(SessionID) %>%
  summarize(LearningTime = max(SessionTime[Stage == "learning"])) %>%
  mutate(PlayingTime = 25 - LearningTime) %>%
  left_join(Sessions) %>%
  recode_strategy()

exp1$mean_learning_time <- round(mean(StageTimesSelfOther$LearningTime), 1)
exp1$prop_learning_time <- round((mean(StageTimesSelfOther$LearningTime)/25) * 100, 1)

learning_times_mod <- lmer(LearningTime ~ Generation * Diachronic_v_Isolated + (Generation|TeamID),
                           data = StageTimesSelfOther)
exp1$learning_times_inter <- report_lmer_mod(learning_times_mod, "Generation:Diachronic_v_Isolated")

learning_times_preds <- expand.grid(
  Generation = 2:4,
  Strategy = c("Diachronic", "Isolated"),
  stringsAsFactors = FALSE
) %>%
  recode_strategy() %>%
  cbind(., predictSE(learning_times_mod, newdata = ., se = TRUE)) %>%
  rename(LearningTime = fit, SE = se.fit)

learning_times_plot <- ggplot(StageTimesSelfOther) +
  aes(Generation, LearningTime) +
  geom_bar(aes(fill = StrategyLabel), stat = "identity", data = learning_times_preds, alpha = 0.6) +
  geom_point(aes(color = StrategyLabel), position = position_jitter(width=0.1, height=0)) +
  geom_errorbar(aes(ymin = LearningTime-SE, ymax = LearningTime+SE),
                data = learning_times_preds, width = 0.2) +
  facet_wrap("Strategy") +
  scale_y_continuous("Learning time (min)") +
  scale_fill_manual(values = t_$color_picker(c("orange", "blue"))) +
  scale_color_manual(values = t_$color_picker(c("orange", "blue"))) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Guesses per item ----

# Fixation ----
data("Guesses")
data("Sessions")
data("AdjacentItems")

IndividualPlayers <- Sessions %>%
  filter_selfother()

IndividualGuesses <- Guesses %>%
  filter_selfother() %>%
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
exp1$first_discovery_mean <- round(mean(FirstDiscovery$NumGuesses), 0)
exp1$first_discovery_beta <- report_beta(first_discovery_mod, "Diachronic_v_Individual")
exp1$first_discovery_stats <- report_lm_mod(first_discovery_mod, "Diachronic_v_Individual")

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