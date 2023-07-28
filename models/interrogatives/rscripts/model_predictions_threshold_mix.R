library(tidyverse)
library(lme4)
library(rwebppl)
#library(ggpattern)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../helpers.R")
source("../../BDA/BDA_vizhelpers.R") # to load graphPredictives()

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
               "BARE-dances-?"
)


# define the QUD 
quds = c("MC", "CC")

# define predicates
predicates = c("think", "know", "BARE")

# define beliefs
speaker_beliefs = seq(0,9,by=1)+ 1


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

# predicate threshold ----
# p <- "think"
# threshold_tmp = eval_webppl(paste("viz_predicate_thresholds('",p,"')",sep=""))

thresholds = data.frame(predicate = character(), value = numeric(), score = numeric())

for (p in predicates) {
  threshold_tmp = eval_webppl(paste("viz_predicate_thresholds('",p,"')",sep=""))
  for (i in 1:nrow(threshold_tmp)) {
    thresholds = thresholds %>%
      add_row(predicate = p, value = threshold_tmp$value[i], score = threshold_tmp$score[i])
  }
}

ggplot(thresholds %>% 
         filter(predicate != "BARE"), aes(x=value)) +
  theme_bw() +
  facet_wrap(~predicate, scales="free") +
  geom_density(alpha=0.05) +
  scale_x_continuous(limits=c(0,1), breaks=seq(0,1,by=0.2), "Threshold value")
# ggsave("../graphs/threshold_mix_threshold.pdf",width=6,height=2)

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
# beliefs in the range (1,10)
LL$speaker_belief = (LL$speaker_belief)/10

ggplot(LL, aes(x=speaker_belief, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(~utterance, scales = "free_y") +
  # scale_fill_manual(values=cbPalette)
  scale_x_continuous(breaks=seq(0,1,by=.1), name = "speaker belief") + 
  scale_y_continuous(name = "probabiltiy")
ggsave("../graphs/threshold_mix/threshold_mix-LL.pdf",width=10,height=3)


#  pragmatic speaker -----
### BDA ----
# load the BDA posterior
all_predictives <- data.frame(seed = character(), utterance = character(), speaker_belief = character(), prob = numeric())

production_path <- "../results/threshold_mix/bda/production/"
for (file in list.files(production_path)) {
  name <- gsub("^([[:alnum:]]+).csv", "\\1", file)
  predictives <- read.csv(paste0(production_path,file))
  predictives$seed <- name
  all_predictives <- rbind(all_predictives, predictives)
}


all_predictives = all_predictives %>% 
  mutate(utterance_type = ifelse(str_detect(utterance, "doesnt"), "not p","p"),
         polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              str_detect(utterance, "Polar") ~ "Polar",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         utterance = case_when(utterance == "BARE-dances-?" ~ "BARE p",
                               utterance == "know-dances-?" ~ '"know p"',
                               utterance == "think-dances-?" ~ '"think p"',
                               utterance == "know-doesnt_dance-?" ~ '"know not p"',
                               TRUE ~ '"think not p"'))

predictives_summary <- all_predictives %>% 
  group_by(utterance, speaker_belief, predicate, utterance_type) %>%  # collapse prior condition
  summarize(mean_prob = mean(prob),
            prob_ci_low = ci.low(prob),
            prob_ci_high = ci.high(prob)) %>%  
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)

predictives_summary <- predictives_summary %>% 
  mutate(utterance_type = ifelse(utterance_type == "p", "Embedded content:p", "Embedded content:not p")) %>%
  mutate(utterance = fct_relevel(utterance, 'BARE p', '"know p"', '"know not p"', '"think p"', '"think not p"'),
         predicate = fct_relevel(predicate, 'Polar', 'think', 'know'),
         utterance_type = fct_relevel(utterance_type, "Embedded content:p", "Embedded content:not p"))

production <- ggplot(predictives_summary,
                     aes(x=utterance, y=mean_prob, fill = predicate)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=prob_YMin,
                    ymax=prob_YMax),
                position=position_dodge(width=0.8),
                width=.2) + 
  facet_grid(.~speaker_belief) +
  scale_fill_manual(values=c("#56B4E9", "#009E73", "#F0E442"),
                    labels=c('Polar', 'think', 'know'),
                    name="Predicate") +
  scale_x_discrete(name="Utterance",
                   guide=guide_axis(angle = 60)) +
  theme(legend.position="top",
        axis.text.x = element_text(size = 10, vjust=1, hjust=1),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14)) +
  ylab("Production probability")
production
ggsave(production, file="../graphs/threshold_mix/model_eval/production.pdf", width=8, height=4)


### simulation ---- 
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

# not needed, just 1-10 is fine
# make bins go from .1 - 1
# .1 means "belief in p is < .1"
# .2 means "belief in p is >= .1 and < .1"
# ...
# 1 means "belief in p is >= .9 and <= 1"
PS$speaker_belief = (PS$speaker_belief)/10


# plot SP, given speaker belief 
ggplot(PS, aes(x=speaker_belief, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(.~utterance) +
  scale_x_continuous(breaks=seq(1,10,by=1)) + 
  ylab("Production probability") +
  xlab("Speaker belief") +
  coord_flip()
ggsave("../graphs/threshold_mix/threshold_chemo-PS-byBelief.pdf",width=12,height=5)

ggplot(PS %>% 
         mutate(predicate = case_when(str_detect(utterance, "know") ~ "know",
                                      str_detect(utterance, "think") ~ "think",
                                      TRUE ~ "Polar"),
                predicate=fct_relevel(predicate,"Polar", "think","know"),
                utterance = case_when(utterance == "BARE-dances-?" ~ "BARE p",
                                      utterance == "know-dances-?" ~ '"know p"',
                                      utterance == "think-dances-?" ~ '"think p"',
                                      utterance == "know-doesnt_dance-?" ~ '"know not p"',
                                      TRUE ~ '"think not p"'),
                utterance = fct_relevel(utterance, 'BARE p', '"know p"', '"know not p"', '"think p"', '"think not p"')), 
       aes(x=utterance, y=prob, color=predicate, fill=predicate)) +
  geom_bar(stat="identity",width = 0.8, position = position_dodge(width = 0.9))+
  scale_fill_manual(values=c("#56B4E9", "#009E73", "#F0E442"),
                    # labels=c('Polar', 'think', 'know'),
                    name="Predicate") +
  scale_color_manual(values=c("#56B4E9", "#009E73", "#F0E442"),
                    # labels=c('Polar', 'think', 'know'),
                    name="Predicate") +
  facet_grid(.~speaker_belief)+
  # theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1)) + 
  theme(legend.position="top",
        axis.text.x = element_text(angle = 60, size = 10, vjust=1, hjust=1),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14)) +
  ylab("Production probability") +
  scale_x_discrete("Utterance")
ggsave("../graphs/threshold_mix/threshold_mix-PS-byUtterance.pdf",width=8,height=4)


# ggplot(PS, aes(x=utterance, y=prob)) +
#   geom_bar(stat="identity") +
#   facet_grid(.~speaker_belief) +
#   xlab("Utterance") +
#   ylab("Production probability") +
#   coord_flip()
ggsave("../graphs/threshold_mix/threshold_chemo-PS-byBelief.pdf",width=8,height=4)


# pragmatic listener ----
### simulation ----
# only infer speaker belief.
# # testing pragmatic listener
# u = "think-doesnt_dance-?"
# pr = "Owen_H"
# PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",pr,"')",sep=""))
# sum((PL_tmp$support +1 )/10 * PL_tmp$prob)

# using enumerate, so don't need to calculate the prob.
# PL_test_bare_H <- PL_tmp %>%
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
    PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",pr,"')",sep="")) # %>% 
      # PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",q,"','",pr,"')",sep="")) %>% 
      # mutate(odds = exp(score),
      #        prob = odds / (1+odds)) %>% 
      # group_by(value) %>% 
      # summarize(mean_prob=mean(prob)) %>%
      # mutate(sum_means = sum(mean_prob)) %>%
      # mutate(prob=mean_prob/sum_means)  %>% 
      # select(value, prob)
    
    for (i in 1:nrow(PL_tmp)) {
      PL = PL %>%
        add_row(utterance = u, prior = pr, speaker_belief = PL_tmp$support[i], speaker_prob = PL_tmp$prob[i])
    }
  }
}
# save the dataframe
# write.csv(PL, "../results/threshold_mix/PL_bda.csv", row.names=FALSE)

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
PL_summary$speaker_belief = (PL_summary$speaker_belief)/10

# # plot by-item prior speaker belief results, marginalize over qud
# for (item in priors){
#   file_path <- paste0("../graphs/threshold_mix/PL_item/sp-",item,".pdf")
#   ggplot(PL_summary %>% 
#            filter(prior==item) %>% 
#            distinct(speaker_belief, utterance, .keep_all = TRUE),
#          aes(x=speaker_belief, y=speaker_prob)) +
#     geom_bar(stat="identity") +
#     facet_grid(.~utterance) +
#     # scale_fill_manual(values=cbPalette)
#     scale_x_continuous(breaks=seq(0,1,by=.1))
#   ggsave(file_path,width=12,height=5)
# }


# plot speaker_belief ~ prior belief
agr = PL_summary %>%
  mutate(predicate = ifelse(predicate == "BARE", "Polar", predicate)) %>% 
  mutate(predicate = fct_relevel(predicate, "Polar", "think", "know")) %>% # reorder for the graph
  distinct(speaker_belief, utterance, prior, .keep_all = TRUE) %>%
  group_by(utterance,prior_mean,polarity,predicate, prior) %>%
  summarize(posterior_belief_p = sum(speaker_prob*speaker_belief)) %>%
  mutate(posterior_belief_emb = case_when(polarity == "neg" ~ 1 - posterior_belief_p,
                                          TRUE ~ posterior_belief_p),
         prior_mean = ifelse(polarity=="neg", 1-prior_mean, prior_mean),
         polarity = case_when(polarity=="pos" ~ "Embedded: p",
                              polarity=="neg" ~ "Embedded: not p",
                              TRUE ~ "Polar")) %>% 
  mutate(polarity = fct_relevel(polarity, rev)) %>% 
  ungroup()

mean_predicted <- agr %>% 
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
  geom_point(data=mean_predicted, 
             aes(x=combined_prior,
                 y=mean_posterior_belief,
                 fill=predicate),
             shape=21,size=2,color="black",stroke=1) +
  geom_errorbar(data=mean_predicted,
                aes(x=combined_prior,y=mean_posterior_belief,
                    ymin=speaker_YMin, ymax=speaker_YMax),
                width=0.05,
                color="black") +
  facet_grid(.~polarity) +
  scale_color_manual(values=cbPalette[2:4]) +
  scale_fill_manual(values=cbPalette[2:4]) +
  scale_y_continuous(limits = c(0,1),
                     breaks=seq(0,1,by=.25),
                     name = "Posterior speaker belief\nin the embedded content") +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(0,1,by=0.2),
                     name = "Rating of prior belief in the embedded content")
ggsave("../../BDA/graphs/threshold_mix/thresholdMix_predictives-estimate-test.pdf",width=7,height=3)


### BDA ----
### model-empirical comparison
# reading the empirical results
df <- read.csv("../../../data/2_listenerSpeakerAH/main/bda_data.csv") %>% 
  filter(predicate %in% c("think", "know", "Polar"))

df_collapsed <- df %>% 
  group_by(predicate, item, polarity) %>%
  summarize(prob = mean(speaker_response)) %>% 
  ungroup() %>% 
  mutate(polarity = case_when(polarity=="pos" ~ "Embedded: p",
                              polarity=="neg" ~ "Embedded: not p",
                              TRUE ~ "Polar"))

predictives <- agr %>% 
  rename(prob = posterior_belief_emb,
         item = prior) %>% 
  select(c('predicate', 'polarity', 'prob', 'item')) %>% 
  filter(!item %in% c("Sophia_H","Sophia_L", "Mia_H", "Mia_L"))


# predictives$type = "prediction"
# df_test <- df_collapsed
# df_test$type = "observation"

# predictives <- rbind(predictives, 
#                      df_test) %>%
#   spread(type, prob)
# 
# correlation <- predictives %>% 
#   group_by(polarity) %>% 
#   summarise(r = cor(observation, prediction),
#             r_squared = round(r ^ 2, digits=3)) %>% 
#   ungroup()

# loaded from BDA_vizhelpers.R
graphPredictives_test(predictives, df_collapsed)
ggsave("../../BDA/graphs/threshold_mix/thresholdMix_predictives-empirical-estimate.pdf",width=8,height=4)



# line plot: predictions and empirical in one plot

observed_sp <- df_collapsed %>% 
  rename(prior = item) %>% 
  left_join(priors_by_condition, by=c("prior")) %>% 
  mutate(prior_mean = ifelse(polarity=="Embedded: not p", prior_neg_mean, prior_mean)) %>% 
  select(-prior_neg_mean) %>% 
  mutate(type = "observation")
  

sp <- agr %>% 
  select(-c(utterance, posterior_belief_p)) %>% 
  mutate(type="prediction") %>% 
  rename(prob = posterior_belief_emb) %>% 
  filter(!prior %in% c("Sophia_H","Sophia_L", "Mia_H", "Mia_L"))

combine_summary <- rbind(sp, observed_sp) %>% 
  rename(prior_prob = prior_mean) %>% 
  mutate(predicate=fct_relevel(predicate, "Polar", "think","know"),
         polarity=fct_relevel(polarity, "Polar", "Embedded: p", "Embedded: not p"))

mean_empirical <- observed_sp %>% 
  group_by(predicate, polarity) %>%  # collapse prior condition
  summarize(mean_posterior_belief = mean(prob),
            speaker_ci_low = ci.low(prob),
            speaker_ci_high = ci.high(prob)) %>%  
  ungroup() %>% 
  mutate(speaker_YMin = mean_posterior_belief - speaker_ci_low,
         speaker_YMax = mean_posterior_belief + speaker_ci_high,
         type = "observation")

mean_predicted$type = "prediction"
mean_summary <- rbind(mean_empirical, mean_predicted) %>% 
  mutate(predicate=fct_relevel(predicate, "Polar", "think","know"),
         polarity=fct_relevel(polarity, "Polar", "Embedded: p", "Embedded: not p"))


ggplot(combine_summary,aes(x=prior_prob,y=prob, color=predicate)) +
  geom_point(aes(shape=type),alpha=0.6) +
  geom_smooth(aes(linetype=type),method="lm", fullrange=TRUE) +
  geom_point(data=mean_summary,
             aes(shape=type,
                 x=combined_prior,
                 y=mean_posterior_belief,
                 fill=predicate),size=2,color="black",stroke=1) +
  geom_errorbar(data=mean_summary,
                aes(linetype=type,x=combined_prior,y=mean_posterior_belief,
                    ymin=speaker_YMin, ymax=speaker_YMax),
                width=0.05,
                color="black") +
  facet_grid(.~polarity) +
  scale_color_manual(values=cbPalette[2:4]) +
  scale_fill_manual(values=cbPalette[2:4]) +
  scale_linetype_manual(values = c(1, 3))+
  scale_y_continuous(limits = c(0,1),
                     breaks=seq(0,1,by=.25),
                     name = "Posterior speaker belief\nin the embedded content") +
  scale_x_continuous(breaks=seq(0,1,by=0.2),
                     name = "Rating of prior belief in the embedded content") 
# theme(legend.position = "top")
ggsave("../graphs/threshold_mix/PL_empirical-predicted_combine.pdf", width=7,height=3)
