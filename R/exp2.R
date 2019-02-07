# ---- exp2
library(tidyverse)
library(magrittr)

library(totems)

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

IsolatedSubjs <- Sessions %>%
  # filter_selfother() %>%
  # Filtering by selfother excludes the following Isolated participants,
  # who were subsequently inherited by Diachronic participants:
  # c("S536", "S370", "S545", "S381", "S551", "S353")
  filter(Strategy == "Isolated") %>%
  select(PlayerID, SessionID, Strategy, Generation)

# Assert that all InheritedIDs are present in the Isolated sessions data
sum(!(DiachronicSubjs$InheritedID %in% IsolatedSubjs$SessionID)) == 0
DiachronicSubjs %>%
  filter(!(InheritedID %in% IsolatedSubjs$SessionID))

