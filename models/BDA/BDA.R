setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(jsonlite)
library(rwebppl)

source("./inferenceHelpers.R")
source("./BDA_vizhelpers.R")

print(Sys.setenv(NODE_OPTIONS = "--max-old-space-size=16384"))
Sys.getenv("NODE_OPTIONS")


# DATA

# Read the pre-processed empirical data
# empirical_df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv")
# df <- empirical_df %>% 
#   mutate(speaker_response = trunc(trunc(speaker_response * 100)/10),
#          ah_response = trunc(trunc(ah_response * 100)/10)) %>%
#   filter(predicate %in% c("think", "know", "BARE"))
 
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>% 
  mutate(speaker_response_bin = ifelse(speaker_response == 1, 9,
                                   trunc(trunc(speaker_response*100) / 10 ))) %>%
  filter(predicate %in% c("think", "know", "Polar")) %>% 
  # testing: one item, one utterance
  # filter(uttearnce=="know-dances-?" & item=="Frank_H") 
  # testing: one item, all utterances
  # filter(item=="Frank_H")
  # testing: multiple items, all utterances
  filter(item %in% c( "Charley_H", "Charley_L","Frank_H", "Frank_L","Danny_H", "Danny_L", "Emily_H", "Emily_L","Emma_H", "Emma_L", "Grace_H", "Grace_L", "Isabella_H","Isabella_L","Jackson_H","Jackson_L", "Jayden_H","Jayden_L","Jon_H","Jon_L","Josh_H","Josh_L","Josie_H","Josie_L","Julian_H", "Julian_L","Mary_H","Mary_L","Mia_H","Mia_L"))

# Create a script that can be used in the WebPPL interpeter (for testing)
# dataHeader = sprintf("var df = %s", toJSON(head(df)))
# write_file(paste(dataHeader, thresholdMixScript, sep = "\n"), file = "testScript.txt")

df_collapsed <- df %>% 
  group_by(predicate, item, polarity) %>%
  summarize(prob = mean(speaker_response))

# MODEL

# 1. threshold_mix
# INFERENCE
model <- makeModel("threshold_mix.txt")

thresholdMixScript <- wrapInference(model,"threshold_mix", 1000, 10, 500) 

thresholdMixPosteriors <- webppl(thresholdMixScript, data = df, data_var = "df", random_seed = 1024)

saveRDS(thresholdMixPosteriors, "results/thresholdMixPosteriors_negNegCost.RDS")

# plot the posterior
graphPosteriors(thresholdMixPosteriors) + ggtitle("Posteriors")

ggplot(thresholdMixPosteriors %>%
         filter(!Parameter %in% c("alpha","bareCost")) %>%
         pivot_wider(names_from = Parameter, values_from = value) %>% 
         group_by(negCost, embedCost) %>% 
         summarize(count = n()), 
  aes(x = negCost, y = embedCost)) +
  theme_bw() +
  geom_tile(aes(fill = count))

# graphJointPosteriors(thresholdMixPosteriors)


# PREDICTIVES
# read the posteriors instead of rerunning
# thresholdMixPosteriors <- readRDS("results/thresholdMixPosteriors_seed3333.RDS")

thresholdMixEstimates <- getEstimates(thresholdMixPosteriors) 

thresholdMixPredictionScript <- wrapPrediction(model, thresholdMixEstimates)

thresholdMixPredictives <- webppl(thresholdMixPredictionScript, data = unique(df %>%  select(utterance, item)), data_var = "df")

thresholdMixPredictives <- thresholdMixPredictives %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              str_detect(utterance, "BARE") ~ "Polar",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         prob = case_when(str_detect(utterance, "doesnt") ~ 1 - prob,
                          TRUE ~ prob),
         polarity = fct_relevel(polarity, "Polar", "pos", "neg" )) %>%
  select(-utterance)

graphPredictives(thresholdMixPredictives, df_collapsed)

ggsave("results/thresholdMixPredictive.png", width = 4, height = 3, units = "in")

