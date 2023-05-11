# script reads in webppl models and plots the literal listener, the speaker, and the pragmatic listener

# load required libraries
library(jsonlite)
library(tidyverse)
# rwebppl not available in this R version, install from github
# library(devtools)
# install_github("mhtess/rwebppl")
library(rwebppl)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load helper functions
source("../helpers.R")
source("../../BDA/BDA_vizhelpers.R") # to load graphPredictives()

theme_set(theme_bw())

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# read in models
#cognitives <- read_file("../models/cognitives.wppl")
# cognitives_test <- read_file("../models/cognitives-?only.wppl")
chemo <- read_file("../simple_chemo.wppl")

# evaluate the models
eval_webppl <- function(command) {
  webppl(paste(chemo,command,sep="\n"))
}
# view(eval_webppl)
u = "know-dances-?"
q = "CC"
it = "Charley_H"
LL_tmp = eval_webppl(paste("literalListener('",u,"','",q,"','",i,"')",sep=""))

# prepare the plots ----

# define the utterances
utterances = c("know-dances-?", 
               "know-doesnt_dance-?",
               "think-dances-?", 
               "think-doesnt_dance-?",
               "BARE-dances-?")

# define the QUD 
quds = c("MC", "CC")

# define beliefs
speaker_beliefs = c("dances","doesnt_dance")
ah_beliefs = c("dances","doesnt_dance","null")

# define priors 
item_priors <- read.csv("../data/binary-priors.csv")

# define items
items <- unique(item_priors$item)

# literal listener ----

LL = data.frame(utterance = character(), qud = character(), item = character(), speaker_belief = character(), ah_belief = character(), prob = numeric())

for (u in utterances) {
  for (q in quds) {
    for (it in items) {
      LL_tmp = eval_webppl(paste("literalListener('",u,"','",q,"','",it,"')",sep=""))
      if (q == "CC") {
        # print("inside speaker belief qud")
        for (i in 1:nrow(LL_tmp)) {
          LL = LL %>% 
            add_row(utterance = u, qud = q, item = it, speaker_belief = LL_tmp$speaker_belief[i], ah_belief = NA, prob = LL_tmp$prob[i])
        }
      } else if (q == "MC") {
          # print("inside ah belief qud")
          for (i in 1:nrow(LL_tmp)) {
            LL = LL %>% 
              add_row(utterance = u, qud = q, item = it, speaker_belief = NA, ah_belief = LL_tmp$ah_belief[i], prob = LL_tmp$prob[i])
          }
        }
      else {
          print("error: no valid qud")
        }
      }
    }
  }

LL_summary = LL %>% 
  mutate(belief_tuple = paste("sp: ",speaker_belief,", ah: ",ah_belief, sep="")) %>% 
  group_by(utterance, qud, belief_tuple) %>% 
  summarize(mean_prob = mean(prob))

ggplot(LL_summary, aes(x=belief_tuple, y=mean_prob)) +
  geom_bar(stat="identity") +
  facet_grid(qud~utterance, scales = "free_y") +
  coord_flip()
# scale_fill_manual(values=cbPalette)
ggsave("../graphs/simple_chemo/LL.pdf",width=15,height=5)

# LL_long = LL %>% 
#   pivot_longer(speaker_belief:ah_belief, names_to = c("belief_holder"), values_to = c("content")) %>% 
#   drop_na()
# 
# ggplot(LL_long, aes(x=content, y=prob, fill=belief_holder)) +
#   geom_bar(stat="identity",position="dodge") +
#   facet_grid(qud~utterance) +
#   scale_fill_manual(values=cbPalette)

#  pragmatic speaker -----

sp_belief = "dances"
ah_belief = "doesnt_dance"
q = "MC"
it = "Charley_H"
PS_tmp = eval_webppl(paste("speaker({speaker_belief:'",sp_belief,"', ah_belief:'",ah_belief,"'},'",q,"','",it,"')",sep=""))

PS = data.frame(qud = character(), speaker_belief = character(), ah_belief = character(), utterance = character(), item = character(), prob = numeric())

for (sp_belief in speaker_beliefs) {
  for (ah_belief in ah_beliefs) {
    for (q in quds) {
      for (it in items) {
        PS_tmp = eval_webppl(paste("speaker({speaker_belief:'",sp_belief,"', ah_belief:'",ah_belief,"'},'",q,"','",it,"')",sep=""))
        for (i in 1:nrow(PS_tmp)) {
          PS = PS %>% 
            add_row(qud = q, speaker_belief = sp_belief, ah_belief = ah_belief, item = it, utterance = PS_tmp$support[i], prob = PS_tmp$prob[i])
        }
      }
    }
  }
}

PS_summary = PS %>% 
  mutate(belief_tuple = paste("sp: ",speaker_belief,", ah: ",ah_belief, sep="")) %>% 
  group_by(utterance, qud, belief_tuple) %>% 
  summarize(mean_prob = mean(prob))

ggplot(PS_summary, aes(x=utterance, y=mean_prob)) +
  geom_bar(stat="identity") +
  facet_grid(qud~belief_tuple) +
  coord_flip()
ggsave("../graphs/simple_chemo/PS-prior.pdf",width=14,height=6) 

# pragmatic listener ----

u <- "know-dances-?"
it <- "Charley_H"
PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",it,"')",sep=""))

# ignore joint inference for now
PL = data.frame(utterance = character(), item = character(), speaker_belief = character(), ah_belief = character(), prob = numeric())

for (u in utterances) {
  for (it in items) {
    PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",it,"')",sep=""))
    for (i in 1:nrow(PL_tmp)) {
      # PL = PL %>% 
      #   add_row(utterance = u, item = it, speaker_belief = PL_tmp$belief_tuple.speaker_belief[i], ah_belief = PL_tmp$belief_tuple.ah_belief[i], prob = PL_tmp$prob[i], qud=PL_tmp$qud[i])
      PL = PL %>% 
        add_row(utterance = u, item = it, speaker_belief = PL_tmp$speaker_belief[i], ah_belief = PL_tmp$ah_belief[i], prob = PL_tmp$prob[i])
    }
  }
}

PL_summary = PL %>% 
  mutate(belief_tuple = paste("sp: ",speaker_belief,", ah: ",ah_belief, sep="")) %>%
  group_by(utterance, belief_tuple) %>% 
  summarize(mean_prob = mean(prob))
  
ggplot(PL_summary, aes(x=belief_tuple, y=mean_prob)) +
  geom_bar(stat="identity") +
  facet_grid(~utterance) +
  coord_flip()
# scale_fill_manual(values=cbPalette)
# ggsave("../graphs/simple_chemo/PL.pdf",width=14,height=4)


# plot the predictions against observations ----
# read in the empirical results
df <- read.csv("../../../data/2_listenerSpeakerAH/main/bda_data.csv") %>% 
  filter(predicate %in% c("think", "know", "Polar"))

df_speaker <- df %>% 
  group_by(predicate, item, polarity) %>%
  summarize(prob = mean(speaker_response)) %>% 
  ungroup() %>% 
  mutate(polarity = case_when(polarity=="pos" ~ "Embedded: p",
                              polarity=="neg" ~ "Embedded: not p",
                              TRUE ~ "Polar"))

df_ah <- df %>% 
  filter(polarity != "Polar") %>% 
  group_by(predicate, item, polarity) %>%
  summarize(prob = mean(ah_response)) %>% 
  ungroup() %>% 
  mutate(polarity = case_when(polarity=="pos" ~ "Embedded: p",
                              polarity=="neg" ~ "Embedded: not p"))

# plot speaker belief
PL_speaker = PL %>% 
  group_by(utterance, item, speaker_belief) %>% 
  summarize(speaker_prob = sum(prob)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = speaker_belief, values_from = speaker_prob) %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              str_detect(utterance, "BARE") ~ "polar",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "think") ~ "think",
                               str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "BARE") ~ "BARE",
                               TRUE ~ "ERROR")) %>% 
  mutate(speaker_prob = ifelse(polarity == "neg", doesnt_dance, dances)) %>% 
  mutate(predicate = ifelse(predicate == "BARE", "Polar", predicate)) %>% 
  mutate(predicate = fct_relevel(predicate, "Polar", "think", "know"),
         polarity = case_when(polarity=="pos" ~ "Embedded: p",
                              polarity=="neg" ~ "Embedded: not p",
                              TRUE ~ "Polar")) %>% 
  mutate(polarity = fct_relevel(polarity, rev)) %>% 
  select(item, predicate, polarity, speaker_prob)

predictives_speaker <- PL_speaker %>% 
  rename(prob = speaker_prob) %>% 
  select(c('predicate', 'polarity', 'prob', 'item')) %>% 
  filter(!item %in% c("Sophia_H", "Sophia_L", "Mia_H", "Mia_L"))

graphPredictives(predictives_speaker, df_speaker)
# ggsave("../graphs/simple_chemo/simpleChemo_predictives-empircal-sp.pdf",width=14,height=4)


# plot ah belief
PL_ah = PL %>% 
  filter(utterance != "BARE-dances-?") %>% 
  group_by(utterance, item, ah_belief) %>% 
  summarize(ah_prob = sum(prob)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = ah_belief, values_from = ah_prob) %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "think") ~ "think",
                               str_detect(utterance, "know") ~ "know",
                               TRUE ~ "ERROR")) %>% 
  mutate(ah_prob = ifelse(polarity == "neg", doesnt_dance, dances)) %>% 
  mutate(predicate = fct_relevel(predicate, "think", "know"),
         polarity = case_when(polarity=="pos" ~ "Embedded: p",
                              TRUE ~ "Embedded: not p")) %>% 
  mutate(polarity = fct_relevel(polarity, rev)) %>% 
  select(item, predicate, polarity, ah_prob)


predictives_ah <- PL_ah %>% 
  rename(prob = ah_prob) %>% 
  select(c('predicate', 'polarity', 'prob', 'item')) %>% 
  filter(!item %in% c("Sophia_H", "Sophia_L", "Mia_H", "Mia_L"))

graphPredictives_ah(predictives_ah, df_ah)
# ggsave("../graphs/simple_chemo/simpleChemo_predictives-empircal-ah.pdf",width=14,height=4)
