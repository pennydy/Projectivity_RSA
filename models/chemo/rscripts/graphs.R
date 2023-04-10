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

theme_set(theme_bw())

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# read in models
#cognitives <- read_file("../models/cognitives.wppl")
# cognitives_test <- read_file("../models/cognitives-?only.wppl")
chemo <- read_file("../chemo.wppl")

# evaluate the models
eval_webppl <- function(command) {
  webppl(paste(chemo,command,sep="\n"))
}
# view(eval_webppl)
u = "know-dances-?"
q = "ah_belief"
LL_tmp = eval_webppl(paste("literalListener('",u,"','",q,"')",sep=""))

# prepare the plots ----

# define the utterances
# utterances = c("know-pos-dances-?", 
#                 "know-pos-dances-.",
#                "know-neg-dances-?", 
#                "know-neg-dances-.",
#                   "be_right-pos-dances-?", 
#                   "be_right-pos-dances-.",  
#                "be_right-neg-dances-?", 
#                "be_right-neg-dances-.",  
#                   "think-pos-dances-?", 
#                   "think-pos-dances-.",  
#                "think-neg-dances-?", 
#                "think-neg-dances-.", 
#                "thinkNAI-pos-dances-?", 
#                "thinkNAI-pos-dances-.",  
#                "thinkNAI-neg-dances-?", 
#                "thinkNAI-neg-dances-.", 
#                   "BARE-pos-dances-?",
#                   "BARE-pos-dances-.",
#                   "BARE-neg-dances-."
#                )
utterances = c("know-dances-?", 
               "know-dances-.", 
               "be_right-dances-?", 
               "be_right-dances-.",                   
               "think-dances-?", 
               "think-dances-.",                         
               "BARE-dances-?",
               "BARE-dances-.")

utterances

# define the QUD 
quds = c("speaker_belief", "ah_belief", "full_belief")

# define the beliefs
beliefs = c("dances","not dances")

# literal listener ----

LL = data.frame(utterance = character(), qud = character(), speaker_belief = character(), ah_belief = character(), prob = numeric())

for (u in utterances) {
  for (q in quds) {
    LL_tmp = eval_webppl(paste("literalListener('",u,"','",q,"')",sep=""))
    if (q == "full_belief") {
      # print("inside full belief qud")
      for (i in 1:nrow(LL_tmp)) {
        LL = LL %>% 
          add_row(utterance = u, qud = q, speaker_belief = LL_tmp$speaker_belief[i], ah_belief = LL_tmp$ah_belief[i], prob = LL_tmp$prob[i])
      }
    } else {
    if (q == "speaker_belief") {
      # print("inside speaker belief qud")
      for (i in 1:nrow(LL_tmp)) {
        LL = LL %>% 
          add_row(utterance = u, qud = q, speaker_belief = LL_tmp$speaker_belief[i], ah_belief = NA, prob = LL_tmp$prob[i])
      }
    } else {
        if (q == "ah_belief") {
          # print("inside ah belief qud")
          for (i in 1:nrow(LL_tmp)) {
            LL = LL %>% 
              add_row(utterance = u, qud = q, speaker_belief = NA, ah_belief = LL_tmp$ah_belief[i], prob = LL_tmp$prob[i])
          }
        } else {
        print("error: no valid qud")
      }
    }
  }
}
}

LL = LL %>% 
  mutate(belief_tuple = paste("sp: ",speaker_belief,", ah: ",ah_belief, sep=""))

ggplot(LL, aes(x=belief_tuple, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(qud~utterance, scales = "free_y") +
  coord_flip() 
# scale_fill_manual(values=cbPalette)
ggsave("../graphs/chemo/chemo-LL.pdf",width=15,height=5)

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
ah_belief = "not dances"
q = "speaker_belief"
PS_tmp = eval_webppl(paste("speaker({speaker_belief:'",sp_belief,"', ah_belief:'",ah_belief,"'},'",q,"')",sep=""))

PS = data.frame(qud = character(), speaker_belief = character(), ah_belief = character(), utterance = character(), prob = numeric())

for (sp_belief in beliefs) {
  for (ah_belief in beliefs) {
    for (q in quds) {
      PS_tmp = eval_webppl(paste("speaker({speaker_belief:'",sp_belief,"', ah_belief:'",ah_belief,"'},'",q,"')",sep=""))
      for (i in 1:nrow(PS_tmp)) {
        PS = PS %>% 
          add_row(qud = q, speaker_belief = sp_belief, ah_belief = ah_belief, utterance = PS_tmp$support[i], prob = PS_tmp$prob[i])
      }
    }
  }
}

PS = PS %>% 
  mutate(belief_tuple = paste("sp: ",speaker_belief,", ah: ",ah_belief, sep=""))

ggplot(PS, aes(x=utterance, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(qud~belief_tuple) +
  coord_flip()
ggsave("../graphs/chemo/chemo-PS-prior.pdf",width=10,height=6) 

# pragmatic listener ----

# returning joint inference

PL = data.frame(utterance = character(), speaker_belief = character(), ah_belief = character(), prob = numeric(), qud=character())


for (u in utterances) {
  PL_tmp = eval_webppl(paste("pragmaticListener('",u,"')",sep=""))
  for (i in 1:nrow(PL_tmp)) {
    PL = PL %>% 
      add_row(utterance = u, speaker_belief = PL_tmp$speaker_belief[i], ah_belief = PL_tmp$ah_belief[i], prob = PL_tmp$prob[i], qud=PL_tmp$qud[i])
  }
}

PL = PL %>% 
  mutate(belief_tuple = paste("sp: ",speaker_belief,", ah: ",ah_belief, sep=""))

ggplot(PL, aes(x=belief_tuple, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(~utterance) +
  coord_flip()
  # scale_fill_manual(values=cbPalette)
ggsave("../graphs/chemo/chemo-prior.pdf",width=25,height=2)

# plot speaker belief in p
#view(PL)
tmp = PL %>%
  group_by(utterance, speaker_belief) %>%
  summarise(Mean=mean(prob)) %>%
  ungroup() %>%
  pivot_wider(names_from = speaker_belief, values_from = Mean) %>%
  rename_with(make.names) %>%
  replace(is.na(.), 0) %>%
  mutate(absolute_speaker_belief = dances - not.dances) %>%
  mutate(utterance = fct_reorder(as.factor(utterance),absolute_speaker_belief))
tmp
  
ggplot(tmp, aes(x=utterance, y=absolute_speaker_belief, label=round(absolute_speaker_belief, digits=3))) + 
  geom_point(stroke=.5,size=3,color="black") +
  geom_hline(yintercept=0, linetype="dashed", color = "blue") +
  geom_text(size=3, vjust=-1)+
  ylim(-1, 1.1) +
  ylab("Absolute speaker belief") +
  xlab("Utterance") 
ggsave(f="../graphs/chemo/absolute_speaker_beliefs-prior.pdf",height=5,width=20)


