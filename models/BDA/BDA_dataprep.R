setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(jsonlite)
library(rwebppl)


## preprocessing of the existing behavioral results
behavioral_results <- read.csv("../../data/2_listenerSpeakerAH/main/2_listenerSpeakerAH-trials.csv") %>% 
  filter(trigger_class %in% c("Critical", "Control")) %>% 
  select(workerid, ah_response, item, predicate, prior_condition, prior_rating, speaker_response, trigger)

bda_data <- behavioral_results %>% 
  mutate(item = paste0(str_to_title(item), "_", ifelse(prior_condition == "low_prob", "L", "H")),
         polarity = case_when(str_detect(trigger, "neg") ~ "neg",
                              str_detect(trigger, "pos") ~ "pos",
                              TRUE ~ "Polar"),
         utterance = case_when(polarity == "pos" ~ paste0(predicate, "-dances-?"),
                               polarity == "neg" ~ paste0(predicate, "-doesnt_dance-?"),
                               TRUE ~ "BARE-dances-?")) %>% 
  select(workerid, predicate, polarity, utterance, item, speaker_response, ah_response, prior_rating)
write.csv(bda_data, "../../data/2_listenerSpeakerAH/main/bda_data.csv", row.names = FALSE)

belief_data <- behavioral_results %>% 
  mutate(itemType = case_when(str_detect(trigger, "neg") & prior_condition == "low_prob" ~ "H",
                              str_detect(trigger, "neg") & prior_condition == "high_prob" ~ "L",
                              str_detect(trigger, "pos") & prior_condition == "low_prob" ~ "L",
                              str_detect(trigger, "pos") & prior_condition == "high_prob" ~ "H",
                              trigger=="Polar" & prior_condition == "high_prob" ~ "H",
                              trigger=="Polar" & prior_condition == "low_prob" ~ "L"),
         # itemType = ifelse(prior_condition == "low_prob", "L", "H"),
         polarity = case_when(str_detect(trigger, "neg") ~ "neg",
                              str_detect(trigger, "pos") ~ "pos",
                              TRUE ~ "Polar")) %>% 
  select(workerid, predicate, polarity, item, itemType, speaker_response, ah_response)
write.csv(belief_data, "../../data/2_listenerSpeakerAH/main/belief_data.csv", row.names = FALSE)
