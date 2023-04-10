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
m_qud <- read_file("../threshold_qud.wppl")

# evaluate the models
eval_webppl <- function(command) {
  webppl(paste(m_qud,command,sep="\n"))
}

# view(eval_webppl)

# read in by-item priors
item_priors <- read_csv("../data/binned-priors.csv")
ggplot(item_priors, aes(x=state, y=prop)) +
  geom_bar(stat="identity") +
  facet_wrap(.~item) +
  scale_x_continuous(breaks=seq(from = 0, to = 10, by = 1))

priors <- unique(item_priors$item)

# prepare the plots ----

# define the utterances
utterances = c("know-dances-?", 
               "know-doesnt_dance-?",
               # "know-dances-.", 
               # "be_right-dances-?", 
               # "be_right-dances-.",                   
               "think-dances-?", 
               "think-doesnt_dance-?",
               # "think-dances-.",                         
               "BARE-dances-?"
               # "BARE-dances-."
)

utterances

# define the QUD 
quds = c("MC", "CC")

# define the beliefs
#beliefs = c("dances","not dances")

# define beliefs
speaker_beliefs = seq(0,9,by=1)

states <- speaker_beliefs + 1

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

LL = data.frame(utterance = character(), qud = character(), speaker_belief = numeric(), prob = numeric())


for (u in utterances) {
  for (q in quds) {
    LL_tmp = eval_webppl(paste("literalListener('",u,"','",q,"')",sep="")) %>% 
      group_by(value) %>%
      summarize(mean_score=mean(score)) %>%
      mutate(sum_means = sum(mean_score)) %>%
      mutate(prob=mean_score/sum_means) %>%
      select(value,prob)

    for (i in 1:nrow(LL_tmp)) {
      LL = LL %>%
        add_row(utterance = u, qud = q, speaker_belief = LL_tmp$value[i], prob = LL_tmp$prob[i])
    }
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
  facet_grid(qud~utterance, scales = "free_y") +
  # scale_fill_manual(values=cbPalette)
  scale_x_continuous(breaks=seq(0,1,by=.1))
ggsave("../graphs/threshold_qud/threshold_qud-LL.pdf",width=12,height=5)

#  pragmatic speaker -----

PS = data.frame(qud = character(), speaker_belief = numeric(), utterance = character(), prob = numeric())

for (sp_belief in speaker_beliefs) {
  for (q in quds) {
    PS_tmp = eval_webppl(paste("speaker(",sp_belief,",'",q,"')",sep=""))
    for (i in 1:nrow(PS_tmp)) {
      PS = PS %>% 
        add_row(qud = q, speaker_belief = sp_belief, utterance = PS_tmp$support[i], prob = PS_tmp$prob[i])
    }
  }  
}

# make bins go from .1 - 1
# .1 means "belief in p is < .1"
# .2 means "belief in p is >= .1 and < .1"
# ...
# 1 means "belief in p is >= .9 and <= 1"
PS$speaker_belief = (PS$speaker_belief + 1)/10

ggplot(PS, aes(x=speaker_belief, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(qud~utterance) +
  scale_x_continuous(breaks=seq(0,1,by=.1))
# coord_flip()
ggsave("../graphs/threshold_qud/threshold_qud-PS-byutterance.pdf",width=12,height=5)

ggplot(PS, aes(x=utterance, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(qud~speaker_belief) +
  coord_flip()
# scale_x_continuous(breaks=seq(0,1,by=.1))
# coord_flip()
ggsave("../graphs/threshold_qud/threshold_qud-PS-bybelief.pdf",width=12,height=3)



# pragmatic listener ----

# currently implemented as just returning beliefs as a function of utterance and qud, but ideally would be a joint inference model
# also inferring qud. not implemented for purely technical boring reasons: 
# rwebppl can't parse joint inference webppl output) -> cannot do joint inference of two different types of variables

PL = data.frame(utterance = character(), qud = character(), prior = character(), speaker_belief = character(), speaker_prob = numeric(), qud_prob=numeric())

u = "know-dances-?"
pr = "Charley_H"
PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",pr,"')",sep=""))
# q = "MC"
# PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",q,"','",pr,"')",sep=""))

PL_test <- PL_tmp %>% 
  group_by(Iteration) %>% 
  pivot_wider(names_from = Parameter, values_from = value) %>%
  ungroup() %>% 
  mutate(total_count=n()) %>% 
  group_by(belief) %>% 
  mutate(speaker_count = n(),
         speaker_prob = speaker_count/total_count) %>% 
  ungroup() %>% 
  group_by(qud) %>% 
  mutate(qud_count = n(),
         qud_prob = qud_count/total_count) %>% 
  select(belief, qud, speaker_prob, qud_prob)


for (u in utterances) {
  # for (q in quds) {
    for (pr in priors) {
      # PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",q,"','",pr,"')",sep=""))
      PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",pr,"')",sep="")) %>% 
        group_by(Iteration) %>% 
        pivot_wider(names_from = Parameter, values_from = value) %>%
        ungroup() %>% 
        mutate(total_count=n()) %>% 
        group_by(belief) %>% 
        mutate(speaker_count = n(),
               speaker_prob = speaker_count/total_count) %>% 
        ungroup() %>% 
        group_by(qud) %>% 
        mutate(qud_count = n(),
               qud_prob = qud_count/total_count) %>% 
        select(belief, qud, speaker_prob, qud_prob)
      
      for (i in 1:nrow(PL_tmp)) {
        PL = PL %>% 
          add_row(utterance = u, qud = PL_tmp$qud[i], prior = pr, speaker_belief = PL_tmp$belief[i], speaker_prob = PL_tmp$speaker_prob[i], qud_prob = PL_tmp$qud_prob[i])
      }
    }
  # }
}

PL_prior = PL %>% 
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
PL_prior$speaker_belief = (as.numeric(PL_prior$speaker_belief) + 1)/10
# joint probability qud x speaker belief
PL_prior$belief_qud_prob = PL_prior$speaker_prob * PL_prior$qud_prob

PL_prior %>% 
  distinct(utterance, prior, speaker_belief, qud, .keep_all = TRUE) %>% 
  group_by(utterance,prior) %>% 
  summarize(s = sum(belief_qud_prob))


# plot by-item prior results
for (item in priors){
  file_path <- paste("../graphs/threshold_qud/PL_item/",item,".pdf")
  ggplot(PL_prior[PL_prior$prior == item,], aes(x=speaker_belief, y=prob)) +
    geom_bar(stat="identity") +
    facet_grid(qud~utterance) +
    # scale_fill_manual(values=cbPalette)
    scale_x_continuous(breaks=seq(0,1,by=.1))
  ggsave(file_path,width=12,height=5)
}

agr = PL_prior %>% 
  mutate(predicate = fct_relevel(predicate, "BARE", "think", "know")) %>% # reorder for the graph
  distinct(utterance, prior, qud, speaker_belief, .keep_all = TRUE) %>%
  group_by(utterance,prior,prior_mean,polarity,predicate, qud) %>%
  summarize(posterior_belief_p = sum(belief_qud_prob*speaker_belief)) %>%
  mutate(posterior_belief_emb = case_when(polarity == "neg" ~ 1 - posterior_belief_p,
                                          TRUE ~ posterior_belief_p),
         prior_mean = ifelse(polarity=="neg", 1-prior_mean, prior_mean))
  
agr_belief = PL_prior %>%
  mutate(predicate = fct_relevel(predicate, "BARE", "think", "know")) %>% # reorder for the graph
  distinct(utterance, prior, speaker_belief, .keep_all = TRUE) %>% 
  group_by(utterance,prior,prior_mean,polarity,predicate) %>%
  summarize(posterior_belief_p = sum(speaker_prob*speaker_belief)) %>%
  mutate(posterior_belief_emb = case_when(polarity == "neg" ~ 1 - posterior_belief_p,
                                          TRUE ~ posterior_belief_p),
         prior_mean = ifelse(polarity=="neg", 1-prior_mean, prior_mean))

agr_qud = PL_prior %>% 
  mutate(predicate = fct_relevel(predicate, "BARE", "think", "know")) %>% # reorder for the graph
  distinct(utterance, prior, qud, .keep_all = TRUE) %>% 
  mutate(prior_mean = ifelse(polarity=="neg", 1-prior_mean, prior_mean)) %>% 
  select(utterance, prior_mean, qud, qud_prob, polarity, predicate)

# agr = PL_prior %>% 
#   group_by(utterance,qud,prior_mean) %>% 
#   summarize(posterior_belief_p = sum(prob*speaker_belief)) %>% 
#   mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
#                               TRUE ~ "pos"),
#          predicate = case_when(str_detect(utterance, "think") ~ "think",
#                                str_detect(utterance, "know") ~ "know",
#                                str_detect(utterance, "BARE") ~ "BARE",
#                                TRUE ~ "ERROR")) %>% 
#   mutate(posterior_belief_emb = case_when(polarity == "neg" ~ 1 - posterior_belief_p,
#                                           TRUE ~ posterior_belief_p),
#          # prior_mean = prior_mean)
#          prior_mean = ifelse(polarity=="neg", 1-prior_mean, prior_mean))

# qud
ggplot(agr_qud %>% 
         distinct(utterance, qud, .keep_all = TRUE), aes(x=utterance,y=qud_prob,fill=qud)) +
  geom_bar(stat="identity",position = position_dodge2(preserve = "single"))+
  scale_fill_grey() +
  scale_y_continuous(name = "Posterior speaker belief\nin the embedded content") +
  scale_x_discrete(name = "Utterance")
ggsave("../graphs/threshold_qud/threshold_qud-PL-qud.pdf",width=12,height=5)

# # not needed: prior and inferred qud prob (no correlation between nai and prior)
# ggplot(arg_qud, aes(x=prior_mean,y=qud_prob,color=predicate)) +
#   geom_point(alpha=0.6) +
#   geom_smooth(method="lm",fullrange=TRUE)+
#   facet_grid(qud~utterance) +
#   scale_color_manual(values=cbPalette[2:4]) +
#   scale_y_continuous(name = "Posterior QUD Likelihood") +
#   scale_x_continuous(name = "Rating of prior belief in the embedded content")
# ggsave("../graphs/threshold_qud/threshold_qud-PL-qud.pdf",width=12,height=5)


ggplot(agr_belief, aes(x=prior_mean,y=posterior_belief_emb,color=predicate)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm") +
  facet_grid(.~polarity) +
  scale_color_manual(values=cbPalette[2:4]) +
  scale_y_continuous(name = "Posterior speaker belief\nin the embedded content") +
  scale_x_continuous(name = "Rating of prior belief in the embedded content")
ggsave("../graphs/threshold_qud/threshold_qud-PL-prior.pdf",width=12,height=5)

ggplot(agr, aes(x=prior_mean,y=posterior_belief_emb,color=predicate)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm") +
  facet_grid(qud~polarity) +
  scale_color_manual(values=cbPalette[2:4]) +
  scale_y_continuous(name = "Posterior speaker belief\nin the embedded content") +
  scale_x_continuous(name = "Rating of prior belief in the embedded content")
ggsave("../graphs/threshold_qud/threshold_qud-PL-prior.pdf",width=12,height=5)
