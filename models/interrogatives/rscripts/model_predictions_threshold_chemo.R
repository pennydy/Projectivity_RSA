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
threshold_chemo <- read_file("../threshold_chemo.wppl")

# evaluate the models
eval_webppl <- function(command) {
  webppl(paste(threshold_chemo,command,sep="\n"))
}

# read in by-item priors
item_priors <- read_csv("../data/binned-priors.csv")
item_priors$state <- item_priors$state / 10
ggplot(item_priors, aes(x=state, y=prop)) +
  geom_bar(stat="identity") +
  facet_wrap(~item,ncol = 6) +
  scale_x_continuous(breaks=seq(from = 0, to = 1, by = .1),
                     name = "Speaker belief states") +
  scale_y_continuous(name = "Probability of prior speaker belief state")
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
speaker_beliefs = as.character(speaker_beliefs)
ah_beliefs = c(speaker_beliefs, "null")
beliefs <- c()
for (sp in speaker_beliefs) {
  for (ah in ah_beliefs) {
    beliefs <- c(beliefs, paste0("<",sp,",",ah,">"))
  }
}


get_mean_prior <- function(item) {
  item_mean <- item/sum(item)
  item_mean <- mean(item_mean*states)
  return(item_mean)
}

prior_means <- item_priors %>% 
  select(c("item", "state", "prop")) %>% 
  pivot_wider(names_from = item, values_from = prop) %>%
  arrange(state)

item_prior_means <- prior_means %>% 
  select(-state) %>%
  sapply(., get_mean_prior)

item_prior <- data.frame(row.names = NULL,
                         prior=priors, prior_mean=item_prior_means) %>% 
  mutate(prior_neg_mean = 1 - prior_mean)

# read in by-predicate qud priors
qud_priors <- read_csv("../data/predicate-nai.csv")

# literal listener ----

LL = data.frame(utterance = character(), qud = character(), speaker_belief = character(), ah_belief = character(), prob = numeric())

# # testing literal listener
# u = "know-dances-?"
# q = "CC"
# LL_tmp = eval_webppl(paste("literalListener('",u,"','",q,"')",sep="")) 
# 
# LL_test <- LL_tmp %>%
#   # pivot_wider(names_from = Parameter, values_from = value) %>% 
#   mutate(total_count = n()) %>% 
#   group_by(value) %>% 
#   mutate(belief_count = n(),
#          prob = belief_count / total_count) %>% 
#   distinct(value, Parameter, .keep_all = TRUE) %>% 
#   select(value,prob)


for (u in utterances) {
  for (q in quds) {
    LL_tmp = eval_webppl(paste("literalListener('",u,"','",q,"')",sep="")) %>% 
      mutate(total_count = n()) %>% 
      group_by(value) %>% 
      mutate(belief_count = n(),
             prob = belief_count / total_count) %>% 
      distinct(value, Parameter, .keep_all = TRUE) %>% 
      select(value,prob)

    # assume there are only two quds, CC -> return only speaker belief, MC -> return only ah belief
    if (q == "CC") {
      for (i in 1:nrow(LL_tmp)) {
        LL = LL %>% 
          add_row(utterance = u, qud = q, speaker_belief = LL_tmp$value[i], ah_belief = NA, prob = LL_tmp$prob[i])
      }
    } else {
      if (q == "MC") {
        for (i in 1:nrow(LL_tmp)) {
          LL = LL %>% 
            add_row(utterance = u, qud = q, speaker_belief = NA, ah_belief = LL_tmp$value[i], prob = LL_tmp$prob[i])
        }
      }
    }
  }
}

# make bins go from .1 - 1
# .1 means "belief in p is < .1"
# .2 means "belief in p is >= .1 and < .1"
# ...
# 1 means "belief in p is >= .9 and <= 1"
LL$speaker_belief = (as.numeric(LL$speaker_belief) + 1)/10
LL$ah_belief = na_if(LL$ah_belief, "null")
LL$ah_belief = (as.numeric(LL$ah_belief) + 1)/10

ggplot(LL, aes(x=speaker_belief, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(~utterance, scales = "free_y") +
  # scale_fill_manual(values=cbPalette)
  scale_x_continuous(breaks=seq(0,1,by=.1))
ggsave("../graphs/threshold_chemo/threshold_chemo-LL-CC.pdf",width=12,height=5)

ggplot(LL %>% filter(utterance != "BARE-dances-?"), aes(x=ah_belief, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(~utterance, scales = "free_y") +
  # scale_fill_manual(values=cbPalette)
  scale_x_continuous(breaks=seq(0,1,by=.1))
ggsave("../graphs/threshold_chemo/threshold_chemo-LL-MC.pdf",width=12,height=5)

#  pragmatic speaker -----

PS = data.frame(utterance = character(), qud = character(), speaker_belief = character(), ah_belief = character(), prob = numeric())

# # testing pragmatic speaker
# sp_belief = "9"
# ah_belief = "2"
# q = "CC"
# PS_tmp = eval_webppl(paste("speaker({speaker_belief:'",sp_belief,"', ah_belief:'",ah_belief,"'},'",q,"')",sep=""))

for (sp_belief in speaker_beliefs) {
  for (ah_belief in ah_beliefs){
    for (q in quds) {
      PS_tmp = eval_webppl(paste("speaker({speaker_belief:'",sp_belief,"', ah_belief:'",ah_belief,"'},'",q,"')",sep=""))
      for (i in 1:nrow(PS_tmp)) {
        PS = PS %>% 
          add_row(qud = q, speaker_belief = sp_belief, ah_belief = ah_belief, utterance = PS_tmp$support[i], prob = PS_tmp$prob[i])
      }
    }
  }
}

# make bins go from .1 - 1
# .1 means "belief in p is < .1"
# .2 means "belief in p is >= .1 and < .1"
# ...
# 1 means "belief in p is >= .9 and <= 1"
PS_summary <- PS %>% 
  mutate(speaker_belief = as.numeric(speaker_belief),
         ah_belief = ifelse(ah_belief == "null", NA, as.numeric(ah_belief)))
typeof(PS_summary$speaker_belief)
PS_summary$speaker_belief_numeric = (PS_summary$speaker_belief + 1)/10
PS_summary$ah_belief_numeric = (PS_summary$ah_belief + 1)/10
PS_summary$beliefs = paste0("<",PS_summary$speaker_belief,",",PS_summary$ah_belief,">")

PS_speaker_belief <- PS_summary %>% 
  group_by(speaker_belief, utterance, qud) %>% 
  summarize(speaker_prob = mean(prob))
PS_speaker_belief$speaker_belief_numeric = (PS_speaker_belief$speaker_belief + 1)/10

# production distribution of the pragmatic speaker, marginalize one belief --- ignore for now
# # plot SP, given speaker belief and qud (marginalize over speaker belief)
# ggplot(PS_speaker_belief, aes(x=speaker_belief_numeric, y=speaker_prob)) +
#   geom_bar(stat="identity") +
#   facet_grid(qud~utterance) +
#   scale_x_continuous(breaks=seq(0,1,by=.1))
# # coord_flip()
# ggsave("../graphs/threshold_chemo/threshold_chemo-PS-sp_belief-byutterance.pdf",width=12,height=5)
# 
# # plot SP, given ah belief and qud (marginalize over speaker belief)
# PS_ah_belief <- PS_summary %>% 
#   group_by(ah_belief, utterance, qud) %>% 
#   summarize(ah_prob = mean(prob))
# PS_ah_belief$ah_belief_numeric = (PS_ah_belief$ah_belief + 1)/10
# 
# ggplot(PS_ah_belief, aes(x=ah_belief_numeric, y=ah_prob)) +
#   geom_bar(stat="identity") +
#   facet_grid(qud~utterance) +
#   scale_x_continuous(breaks=seq(0,1,by=.1))
# # coord_flip()
# ggsave("../graphs/threshold_chemo/threshold_chemo-PS-ah_belief-byutterance.pdf",width=12,height=5)


for (sp_belief in speaker_beliefs) {
  file_path <- paste0("../graphs/threshold_chemo/PS_beliefs/SP-",sp_belief,".pdf")
  ggplot(PS_summary %>% 
           filter(speaker_belief == sp_belief), 
         aes(x=utterance, y=prob)) +
    geom_bar(stat="identity") +
    facet_grid(qud~beliefs) +
    scale_y_continuous(breaks=seq(0,1,by=.25), guide = guide_axis(angle = 45)) +
    coord_flip()
  ggsave(file_path,width=12,height=5)
}


for (ah in ah_beliefs) {
  file_path <- paste0("../graphs/threshold_chemo/PS_beliefs/AH-",ah,".pdf")
  ggplot(PS_summary %>% 
           mutate(ah_belief = ifelse(is.na(ah_belief), "null",as.character(ah_belief))) %>% 
           filter(ah_belief == ah), 
         aes(x=utterance, y=prob)) +
    geom_bar(stat="identity") +
    facet_grid(qud~beliefs) +
    scale_y_continuous(breaks=seq(0,1,by=.25),guide = guide_axis(angle = 45)) +
    coord_flip()
  ggsave(file_path,width=12,height=5)
}

# summary plot is too big
# ggplot(PS_summary,
#        aes(x=utterance, y=prob)) +
#   geom_bar(stat="identity") +
#   facet_grid(beliefs~qud) +
#   scale_y_continuous(breaks=seq(0,1,by=.25),guide = guide_axis(angle = 45)) +
#   coord_flip()
# ggsave("../graphs/threshold_chemo/PS_beliefs/SP_all.pdf",width=12,height=48)


# pragmatic listener ----

# jointly infer speaker belief, ah belief, and qud
# speaker_belief/ah_belief: speaker belief/ah belief, binned 0-9 (before normalizing)
# state: <speaker_belief, ah_belief, qud>
# speaker_prob/ah_prob/qud_prob: marginal distribution of speaker belief/ah belief/qud
# speaker_qud_prob/ah_qud_prob: conditional distribution of speaker belief/ah belief given the qud
# state_prob: the probability of each state (i.e., each combination of <speaker_belief, ah_belief, qud>)

# very slow, run 10 items at a time
PL = data.frame(utterance = character(), qud = character(), prior = character(), speaker_belief = character(), ah_belief = character(), speaker_prob = numeric(), ah_prob = numeric(), qud_prob = numeric(), speaker_qud_prob = numeric(), ah_qud_prob = numeric())
PL_10 = data.frame(utterance = character(), qud = character(), prior = character(), speaker_belief = character(), ah_belief = character(), speaker_prob = numeric(), ah_prob = numeric(), qud_prob = numeric(), speaker_qud_prob = numeric(), ah_qud_prob = numeric())
PL_20 = data.frame(utterance = character(), qud = character(), prior = character(), speaker_belief = character(), ah_belief = character(), speaker_prob = numeric(), ah_prob = numeric(), qud_prob = numeric(), speaker_qud_prob = numeric(), ah_qud_prob = numeric())
PL_30 = data.frame(utterance = character(), qud = character(), prior = character(), speaker_belief = character(), ah_belief = character(), speaker_prob = numeric(), ah_prob = numeric(), qud_prob = numeric(), speaker_qud_prob = numeric(), ah_qud_prob = numeric())
PL_35 = data.frame(utterance = character(), qud = character(), prior = character(), speaker_belief = character(), ah_belief = character(), speaker_prob = numeric(), ah_prob = numeric(), qud_prob = numeric(), speaker_qud_prob = numeric(), ah_qud_prob = numeric())

# # testing pragmatic listener
# u = "think-dances-?"
# pr = "Charley_L"
# PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",pr,"')",sep="")) 
# 
# 
# PL_test <- PL_tmp %>% 
#   group_by(Iteration) %>% 
#   mutate(name=factor(Parameter, unique(Parameter))) %>% 
#   pivot_wider(values_from = value) %>% 
#   summarize(across(speaker_belief:qud, ~toString(unique(na.omit(.x)))),.groups = 'drop') %>% 
#   mutate(state=paste(speaker_belief, ah_belief, qud, sep = ","),
#          state=paste0("<",state,">")) 
# 
# PL_test <- PL_test %>%
#   mutate(total_count=n()) %>% 
#   group_by(state) %>% 
#   mutate(state_count = n(),
#          state_prob=state_count/total_count) %>% 
#   ungroup() %>% 
#   group_by(speaker_belief) %>% 
#   mutate(speaker_count = n(),
#          speaker_prob = speaker_count/total_count) %>% 
#   ungroup() %>% 
#   group_by(ah_belief) %>% 
#   mutate(ah_count = n(),
#          ah_prob = ah_count/total_count) %>% 
#   ungroup() %>% 
#   group_by(qud) %>% 
#   mutate(qud_count = n(),
#          qud_prob = qud_count/total_count) %>%
#   ungroup() %>% 
#   group_by(qud, speaker_belief) %>% 
#   mutate(speaker_qud_count = n(),
#          speaker_qud_prob = speaker_qud_count/total_count) %>%
#   ungroup() %>% 
#   group_by(qud, ah_belief) %>% 
#   mutate(ah_qud_count = n(),
#          ah_qud_prob = ah_qud_count/total_count) %>%
#   select(speaker_belief, ah_belief,qud,state,
#          state_prob, speaker_prob, ah_prob, qud_prob,
#          speaker_qud_prob,ah_qud_prob)

for (u in utterances) {
  # for (q in quds) {
    for (pr in priors[36:40]) {
      PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",pr,"')",sep="")) %>% 
      # PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",q,"','",pr,"')",sep="")) %>% 
        group_by(Iteration) %>% 
        mutate(name=factor(Parameter, unique(Parameter))) %>% 
        pivot_wider(values_from = value) %>% 
        summarize(across(speaker_belief:qud, ~toString(unique(na.omit(.x)))),.groups = 'drop') %>% 
        mutate(state=paste(speaker_belief, ah_belief, qud, sep = ","),
               state=paste0("<",state,">")) %>%
        mutate(total_count=n()) %>% 
        group_by(speaker_belief) %>% 
        mutate(speaker_count = n(),
               speaker_prob = speaker_count/total_count) %>% 
        ungroup() %>% 
        group_by(ah_belief) %>% 
        mutate(ah_count = n(),
               ah_prob = ah_count/total_count) %>% 
        ungroup() %>% 
        group_by(qud) %>% 
        mutate(qud_count = n(),
               qud_prob = qud_count/total_count) %>% 
        ungroup() %>% 
        group_by(qud, speaker_belief) %>% 
        mutate(speaker_qud_count = n(),
               speaker_qud_prob = speaker_qud_count/total_count) %>%
        ungroup() %>% 
        group_by(qud, ah_belief) %>% 
        mutate(ah_qud_count = n(),
               ah_qud_prob = ah_qud_count/total_count) %>%
        select(speaker_belief, ah_belief,qud,speaker_prob, ah_prob, qud_prob,speaker_qud_prob,ah_qud_prob)
        
      
      for (i in 1:nrow(PL_tmp)) {
        PL_35 = PL_35 %>%
          add_row(utterance = u, qud = PL_tmp$qud[i], prior = pr, speaker_belief = PL_tmp$speaker_belief[i], ah_belief = PL_tmp$ah_belief[i], speaker_prob = PL_tmp$speaker_prob[i], ah_prob = PL_tmp$ah_prob[i], qud_prob = PL_tmp$qud_prob[i], speaker_qud_prob = PL_tmp$speaker_qud_prob[i], ah_qud_prob = PL_tmp$ah_qud_prob[i])
      }
    }
  # }
}
write.csv(PL, "../results/PL.csv", row.names=FALSE)
write.csv(PL_10,"../results/PL10.csv", row.names=FALSE)
write.csv(PL_20,"../results/PL20.csv", row.names=FALSE)
write.csv(PL_30,"../results/PL30.csv", row.names=FALSE)
write.csv(PL_35,"../results/PL35.csv", row.names=FALSE)

PL_summary = PL %>% 
  left_join(item_prior, by=c("prior")) %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "think") ~ "think",
                               str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "BARE") ~ "BARE",
                               TRUE ~ "ERROR"))

PL_summary_10 = PL_10 %>% 
  left_join(item_prior, by=c("prior")) %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "think") ~ "think",
                               str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "BARE") ~ "BARE",
                               TRUE ~ "ERROR"))

PL_summary_20 = PL_20 %>% 
  left_join(item_prior, by=c("prior")) %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "think") ~ "think",
                               str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "BARE") ~ "BARE",
                               TRUE ~ "ERROR"))

PL_summary_30 = PL_30 %>% 
  left_join(item_prior, by=c("prior")) %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "think") ~ "think",
                               str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "BARE") ~ "BARE",
                               TRUE ~ "ERROR"))

PL_summary_35 = PL_35 %>% 
  left_join(item_prior, by=c("prior")) %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
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
PL_summary$speaker_belief = (as.numeric(PL_summary$speaker_belief) + 1)/10
PL_summary$ah_belief = (as.numeric(PL_summary$ah_belief) + 1)/10

PL_summary_10$speaker_belief = (as.numeric(PL_summary_10$speaker_belief) + 1)/10
PL_summary_10$ah_belief = (as.numeric(PL_summary_10$ah_belief) + 1)/10

PL_summary_20$speaker_belief = (as.numeric(PL_summary_20$speaker_belief) + 1)/10
PL_summary_20$ah_belief = (as.numeric(PL_summary_20$ah_belief) + 1)/10

PL_summary_30$speaker_belief = (as.numeric(PL_summary_25$speaker_belief) + 1)/10
PL_summary_30$ah_belief = (as.numeric(PL_summary_25$ah_belief) + 1)/10

PL_summary_35$speaker_belief = (as.numeric(PL_summary_35$speaker_belief) + 1)/10
PL_summary_35$ah_belief = (as.numeric(PL_summary_35$ah_belief) + 1)/10

# plot by-item prior speaker belief results, marginalize over qud
for (item in priors[36:40]){
  file_path <- paste0("../graphs/threshold_chemo/PL_item/marginal/marginal-sp-",item,".pdf")
  ggplot(PL_summary_35 %>% 
           filter(prior==item) %>% 
           distinct(speaker_belief, utterance, .keep_all = TRUE),
         aes(x=speaker_belief, y=speaker_prob)) +
    geom_bar(stat="identity") +
    facet_grid(.~utterance) +
    # scale_fill_manual(values=cbPalette)
    scale_x_continuous(breaks=seq(0,1,by=.1))
  ggsave(file_path,width=12,height=5)
}

# plot by-item prior speaker belief, given different quds
for (item in priors[36:40]){
  file_path <- paste0("../graphs/threshold_chemo/PL_item/sp/sp-",item,".pdf")
  ggplot(PL_summary_35 %>% 
           filter(prior==item) %>% 
           distinct(speaker_belief, utterance, qud, .keep_all = TRUE),
         aes(x=speaker_belief, y=speaker_qud_prob)) +
    geom_bar(stat="identity") +
    facet_grid(qud~utterance) +
    # scale_fill_manual(values=cbPalette)
    scale_x_continuous(breaks=seq(0,1,by=.1))
  ggsave(file_path,width=12,height=5)
}

# not needed: by-item prior ~ qud (no correlation between nai and prior)
# for (item in priors[0:10]){
#   file_path <- paste0("../graphs/threshold_chemo/PL_item/qud-",item,".pdf")
#   ggplot(PL_summary %>%
#            filter(prior==item) %>%
#            distinct(utterance, qud, .keep_all = TRUE),
#          aes(x=qud, y=qud_prob)) +
#     geom_bar(stat="identity") +
#     facet_grid(.~utterance)
#   ggsave(file_path,width=12,height=5)
# }

PL <- read.csv("../results/PL.csv")
PL_10 <- read.csv("../results/PL10.csv")
PL_20 <- read.csv("../results/PL20.csv")
PL_30 <- read.csv("../results/PL30.csv")
PL_35 <- read.csv("../results/PL35.csv")

PL_all <- rbind(PL, PL_10, PL_20, PL_30, PL_35) %>% 
  left_join(item_prior, by=c("prior")) %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "think") ~ "think",
                               str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "BARE") ~ "BARE",
                               TRUE ~ "ERROR"),
         speaker_belief = (as.numeric(speaker_belief) + 1)/10,
         ah_belief = (as.numeric(ah_belief) + 1)/10)

# plot speaker_belief ~ prior belief, marginalize over qud
agr = PL_all %>%
  mutate(predicate = fct_relevel(predicate, "BARE", "think", "know")) %>% # reorder for the graph
  distinct(speaker_belief, utterance, prior, .keep_all = TRUE) %>%
  group_by(utterance,prior_mean,polarity,predicate) %>%
  summarize(posterior_belief_p = sum(speaker_prob*speaker_belief)) %>%
  mutate(posterior_belief_emb = case_when(polarity == "neg" ~ 1 - posterior_belief_p,
                                          TRUE ~ posterior_belief_p),
         prior_mean = ifelse(polarity=="neg", 1-prior_mean, prior_mean))

ggplot(agr, aes(x=prior_mean,y=posterior_belief_emb,color=predicate)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm", fullrange=TRUE) +
  facet_grid(.~polarity) +
  scale_color_manual(values=cbPalette[2:4]) +
  scale_y_continuous(name = "Posterior speaker belief\nin the embedded content") +
  scale_x_continuous(name = "Rating of prior belief in the embedded content")
ggsave("../graphs/threshold_chemo/threshold_chemo-PL-sp-prior.pdf",width=12,height=5)


agr_ah = PL_all %>%
  filter(predicate!="BARE") %>% 
  mutate(predicate = fct_relevel(predicate, "think", "know")) %>% # reorder for the graph
  distinct(ah_belief, utterance, prior, .keep_all = TRUE) %>%
  group_by(utterance,prior_mean,polarity,predicate) %>%
  summarize(posterior_belief_p = sum(ah_prob*ah_belief)) %>%
  mutate(posterior_belief_emb = case_when(polarity == "neg" ~ 1 - posterior_belief_p,
                                          TRUE ~ posterior_belief_p),
         prior_mean = ifelse(polarity=="neg", 1-prior_mean, prior_mean))

ggplot(agr_ah, aes(x=prior_mean,y=posterior_belief_emb,color=predicate)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm", fullrange=TRUE) +
  facet_grid(.~polarity) +
  scale_color_manual(values=cbPalette[3:4]) +
  scale_y_continuous(name = "Posterior ah belief\nin the embedded content") +
  scale_x_continuous(name = "Rating of prior belief in the embedded content")
ggsave("../graphs/threshold_chemo/threshold_chemo-PL-ah-prior.pdf",width=12,height=5)
