library(tidyverse)
library(lme4)
library(rwebppl)
#library(ggpattern)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../helpers.R")

# Helper functions for the graph
theme_set(theme_bw())
# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 

# read in model
threshold_mix <- read_file("../threshold_mix.wppl")

# evaluate the models
eval_webppl <- function(command) {
  webppl(paste(threshold_mix,command,sep="\n"))
}

# read in by-item priors
item_priors <- read.csv("../data/binned-priors.csv")
item_priors$state <- item_priors$state / 10
# ggsave("../prior.pdf",width=14,height=8)

priors <- unique(item_priors$item)

# view(eval_webppl)

# prepare the plots ----

# define the utterances
utterances = c("know-dances-?", 
               "know-doesnt_dance-?",
               "think-dances-?", 
               "think-doesnt_dance-?",
               # "inform-dances-?",
               # "inform-doesnt_dance",
               # "say-dances-?",
               # "say-doesnt_dance-?",
               "BARE-dances-?"
)

utterances

# define the QUD 
quds = c("MC", "CC")

# define beliefs
speaker_beliefs = seq(0,9,by=1)
states <- speaker_beliefs + 1


# priors
# directly importing the by-condition (Hight/Low) prior ratings
# obtained from Degen & Tonhauser 2021
priors_by_condition <- read.csv("../data/condition-priors.csv") %>% 
  rename(prior_mean = prior_rating,
         prior = item_condition) %>% 
  mutate(prior_neg_mean = 1 - prior_mean)

combined_prior <- mean(priors_by_condition$prior_mean)

# read in by-predicate qud priors
qud_priors <- read.csv("../data/predicate-nai.csv")

# literal listener ----
# # testing literal listener
# u = "know-dances-?"
# LL_tmp = eval_webppl(paste("literalListener('",u,"')",sep=""))
# 
# LL_test <- LL_tmp %>%
#   mutate(odds = exp(score),
#          prob = odds / (1+odds)) %>% # convert log odds to probability
#   group_by(value) %>% 
#   summarize(mean_prob=mean(prob)) %>%
#   mutate(sum_means = sum(mean_prob)) %>%
#   mutate(prob=mean_prob/sum_means) 

LL = data.frame(utterance = character(), speaker_belief = numeric(), prob = numeric())

for (u in utterances) {
  LL_tmp = eval_webppl(paste("literalListener('",u,"')",sep="")) %>% 
    mutate(odds = exp(score),
           prob = odds / (1+odds)) %>% # convert log odds to probability
    group_by(value) %>% 
    summarize(mean_prob=mean(prob)) %>%
    mutate(sum_means = sum(mean_prob)) %>%
    mutate(prob=mean_prob/sum_means) 
    # group_by(value) %>%
    # summarize(mean_score=mean(score)) %>%
    # mutate(sum_means = sum(mean_score)) %>%
    # mutate(prob=mean_score/sum_means) %>%
    # select(value,prob)
  for (i in 1:nrow(LL_tmp)) {
    LL = LL %>%
      add_row(utterance = u, speaker_belief = LL_tmp$value[i], prob = LL_tmp$prob[i])
    }
}

# make bins go from .1 - 1
# .1 means "belief in p is < .1"
# .2 means "belief in p is >= .1 and < .1"
# ...
# 1 means "belief in p is >= .9 and <= 1"
LL$speaker_belief = (LL$speaker_belief + 1)/10

ggplot(LL, aes(x=speaker_belief, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(~utterance, scales = "free_y") +
  # scale_fill_manual(values=cbPalette)
  scale_x_continuous(breaks=seq(0,1,by=.1))
ggsave("../graphs/threshold_mix/threshold_mix-LL.pdf",width=12,height=5)


#  pragmatic speaker -----
# # testing pragmatic speaker
# sp_belief = 9
# PS_tmp = eval_webppl(paste("speaker(",sp_belief,")",sep=""))

PS = data.frame(utterance = character(), speaker_belief = numeric(), prob = numeric())

for (sp_belief in speaker_beliefs) {
      PS_tmp = eval_webppl(paste("speaker(",sp_belief,")",sep=""))
      for (i in 1:nrow(PS_tmp)) {
        PS = PS %>% 
          add_row(speaker_belief = sp_belief, utterance = PS_tmp$support[i], prob = PS_tmp$prob[i])
      }
}

# make bins go from .1 - 1
# .1 means "belief in p is < .1"
# .2 means "belief in p is >= .1 and < .1"
# ...
# 1 means "belief in p is >= .9 and <= 1"
PS$speaker_belief = (PS$speaker_belief + 1)/10


# plot SP, given speaker belief 
ggplot(PS, aes(x=speaker_belief, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(.~utterance) +
  scale_x_continuous(breaks=seq(0,1,by=.1)) + 
  xlab("Production probability") +
  ylab("Speaker belief") +
  coord_flip()
ggsave("../graphs/threshold_mix/threshold_mix-PS-byUtterance.pdf",width=12,height=5)


ggplot(PS, aes(x=utterance, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(.~speaker_belief) +
  xlab("Utterance") +
  ylab("Production probability") +
  coord_flip()
ggsave("../graphs/threshold_mix/threshold_chemo-PS-byBelief.pdf",width=12,height=5)


# pragmatic listener ----
# only infer speaker belief. TODO: jointly infer qud
# # testing pragmatic listener
# u = "know-dances-?"
# pr = "Charley_H"
# PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",pr,"')",sep=""))
#   
# PL_test_know_H <- PL_tmp %>% 
#   mutate(odds = exp(score),
#          prob = odds / (1+odds)) %>% 
#   group_by(value) %>% 
#   summarize(mean_prob=mean(prob)) %>%
#   mutate(sum_means = sum(mean_prob)) %>%
#   mutate(prob=mean_prob/sum_means) %>%
#   select(value, prob)

PL = data.frame(utterance = character(), prior = character(), speaker_belief = numeric(), speaker_prob = numeric())

for (u in utterances) {
  for (pr in priors) {
    PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",pr,"')",sep="")) %>% 
      # PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",q,"','",pr,"')",sep="")) %>% 
      mutate(odds = exp(score),
             prob = odds / (1+odds)) %>% 
      group_by(value) %>% 
      summarize(mean_prob=mean(prob)) %>%
      mutate(sum_means = sum(mean_prob)) %>%
      mutate(prob=mean_prob/sum_means)  %>% 
      select(value, prob)
    
    for (i in 1:nrow(PL_tmp)) {
      PL = PL %>%
        add_row(utterance = u, prior = pr, speaker_belief = PL_tmp$value[i], speaker_prob = PL_tmp$prob[i])
    }
  }
}
# save the dataframe
write.csv(PL, "../results/threshold_mix/PL.csv", row.names=FALSE)

PL_summary = PL %>% 
  left_join(priors_by_condition, by=c("prior")) %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              str_detect(utterance, "BARE") ~ "polar",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "think") ~ "think",
                               str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "BARE") ~ "BARE",
                               TRUE ~ "ERROR"))

# make bins go from .1 - 1
# .1 means "belief in p is < .1"
# .2 means "belief in p is >= .1 and < .1"
# ...
# 1 means "belief in p is >= .9 and <= 1"
PL_summary$speaker_belief = (PL_summary$speaker_belief + 1)/10

# plot by-item prior speaker belief results, marginalize over qud
for (item in priors){
  file_path <- paste0("../graphs/threshold_mix/PL_item/sp-",item,".pdf")
  ggplot(PL_summary %>% 
           filter(prior==item) %>% 
           distinct(speaker_belief, utterance, .keep_all = TRUE),
         aes(x=speaker_belief, y=speaker_prob)) +
    geom_bar(stat="identity") +
    facet_grid(.~utterance) +
    # scale_fill_manual(values=cbPalette)
    scale_x_continuous(breaks=seq(0,1,by=.1))
  ggsave(file_path,width=12,height=5)
}


# plot speaker_belief ~ prior belief
agr = PL_summary %>%
  mutate(predicate = fct_relevel(predicate, "BARE", "think", "know")) %>% # reorder for the graph
  distinct(speaker_belief, utterance, prior, .keep_all = TRUE) %>%
  group_by(utterance,prior_mean,polarity,predicate) %>%
  summarize(posterior_belief_p = sum(speaker_prob*speaker_belief)) %>%
  mutate(posterior_belief_emb = case_when(polarity == "neg" ~ 1 - posterior_belief_p,
                                          TRUE ~ posterior_belief_p),
         prior_mean = ifelse(polarity=="neg", 1-prior_mean, prior_mean),
         polarity = case_when(polarity=="pos" ~ "Embedded: p",
                              polarity=="neg" ~ "Embedded: not p",
                              TRUE ~ "Polar")) %>% 
  mutate(polarity = fct_relevel(polarity, rev))

combine_summary <- agr %>% 
  group_by(predicate, polarity) %>%  # collapse prior condition
  summarize(mean_posterior_belief = mean(posterior_belief_emb),
            speaker_ci_low = ci.low(posterior_belief_emb),
            speaker_ci_high = ci.high(posterior_belief_emb)) %>%  
  ungroup() %>% 
  mutate(speaker_YMin = mean_posterior_belief - speaker_ci_low,
         speaker_YMax = mean_posterior_belief + speaker_ci_high)

ggplot(agr,aes(x=prior_mean,y=posterior_belief_emb, color=predicate)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm", fullrange=TRUE) +
  geom_point(data=combine_summary, 
             aes(x=combined_prior,
                 y=mean_posterior_belief,
                 fill=predicate),
             shape=21,size=2,color="black",stroke=1) +
  geom_errorbar(data=combine_summary,
                aes(x=combined_prior,y=mean_posterior_belief,
                    ymin=speaker_YMin, ymax=speaker_YMax),
                width=0.05,
                color="black") +
  facet_grid(.~polarity) +
  scale_color_manual(values=cbPalette[2:4]) +
  scale_fill_manual(values=cbPalette[2:4]) +
  scale_y_continuous(breaks=seq(0,1,by=.25),
                     name = "Posterior speaker belief\nin the embedded content") +
  scale_x_continuous(breaks=seq(0,1,by=0.2),
                     name = "Rating of prior belief in the embedded content")
ggsave("../graphs/threshold_mix/threshold_mix-PL-prior.pdf",width=7,height=3)

