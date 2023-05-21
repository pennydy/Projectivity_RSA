library(tidyverse)
library(lme4)
library(rwebppl)
library(ggpattern)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../helpers.R")
source("../../BDA/BDA_vizhelpers.R") # to load graphPredictives()

# Helper functions for the graph
theme_set(theme_bw())
# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 

# read in model
threshold_mix <- read_file("../chemo_production.wppl")

# evaluate the models
eval_webppl <- function(command) {
  webppl(paste(threshold_mix,command,sep="\n"))
}

# read in by-item priors
item_priors <- read.csv("../data/binary-priors.csv")
# ggsave("../prior.pdf",width=14,height=8)

priors_by_condition <- read.csv("../../interrogatives/data/condition-priors.csv") %>% 
  rename(prior_mean = prior_rating,
         prior = item_condition) %>% 
  mutate(prior_neg_mean = 1 - prior_mean)

combined_prior <- mean(priors_by_condition$prior_mean)

items <- unique(item_priors$item)

# view(eval_webppl)

# prepare the plots ----

# define the utterances
utterances = c("know-dances-?", 
               "know-doesnt_dance-?",
               "think-dances-?", 
               "think-doesnt_dance-?",
               "BARE-dances-?"
)

# define predicates
predicates = c("think", "know", "BARE")

# define the beliefs
# speaker_beliefs = c("dances","doesnt_dance")
speaker_beliefs = c("dances","doesnt_dance","uncertain")
ah_beliefs = c("dances","doesnt_dance","uncertain","null")

#  pragmatic speaker -----
### BDA ----
# load the BDA posterior
all_predictives <- data.frame(seed = character(), utterance = character(), speaker_belief = character(), ah_belief = character(), prob = numeric())

for (file in list.files("../results/chemo_production/bda/production")) {
  name <- gsub("^([[:alnum:]]+)-small.csv", "\\1", file)
  predictives <- read.csv(paste0("../results/chemo_production/bda/production/",file))
  predictives$seed <- name
  all_predictives <- rbind(all_predictives, predictives)
}


all_predictives = all_predictives %>% 
  mutate(utterance_type = ifelse(str_detect(utterance, "doesnt"), "not p","p"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         speaker_belief = case_when(str_detect(speaker_belief, "doesnt") ~ "not p",
                                    str_detect(speaker_belief, "uncertain") ~ "?",
                                    TRUE ~ "p"),
         ah_belief = case_when(str_detect(ah_belief, "null") ~ "null",
                               str_detect(ah_belief, "doesnt") ~ "not p",
                               str_detect(ah_belief, "uncertain") ~ "?",
                               TRUE ~ "p"),
         utterance = case_when(utterance == "BARE-dances-?" ~ "Polar",
                               utterance == "know-dances-?" ~ '"know p"',
                               utterance == "think-dances-?" ~ '"think p"',
                               utterance == "know-doesnt_dance-?" ~ '"know not p"',
                               TRUE ~ '"think not p"'),
         belief_states = paste("<",speaker_belief,", ",ah_belief,">", sep=""))

predictives_summary <- all_predictives %>% 
  group_by(utterance, belief_states, predicate, utterance_type) %>%  # collapse prior condition
  summarize(mean_prob = mean(prob),
            prob_ci_low = ci.low(prob),
            prob_ci_high = ci.high(prob)) %>%  
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)

predictives_summary <- predictives_summary %>% 
  mutate(utterance_type = ifelse(utterance_type == "p", "Embedded content:p", "Embedded content:not p")) %>% 
  mutate(belief_states = fct_relevel(belief_states, '<p, p>','<p, not p>', '<p, ?>','<p, null>', '<not p, p>', '<not p, not p>', '<not p, ?>', '<not p, null>', '<?, p>','<?, not p>', '<?, ?>', '<?, null>'),
           utterance = fct_relevel(utterance, 'Polar', '"know p"', '"know not p"', '"think p"', '"think not p"'),
           predicate = fct_relevel(predicate, 'Polar', 'think', 'know'),
           utterance_type = fct_relevel(utterance_type, "Embedded content:p", "Embedded content:not p"))

production <- ggplot(predictives_summary,
       aes(x=utterance, y=mean_prob, fill = predicate)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=prob_YMin,
                    ymax=prob_YMax),
                position=position_dodge(width=0.8),
                width=.2) + 
facet_wrap(belief_states~.) +
  scale_fill_manual(values=c("#56B4E9", "#009E73", "#F0E442"),
                    labels=c('Polar', 'think', 'know'),
                    name="Predicate") +
  scale_x_discrete(name="Utterance",
                   guide=guide_axis(angle = 65)) +
  theme(legend.position="top",
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14)) +
  ylab("Production probability")
production
ggsave(production, file="../graphs/chemo_production/model_eval/production.pdf", width=6, height=5)

### simulation ----
sp_belief <- "dances"
ah_belief <- "dances"
command <- paste("viz_speaker({speaker_belief:'",sp_belief,"', ah_belief:'",ah_belief,"'})",sep="")
PS_tmp = eval_webppl(command)

PS = data.frame(speaker_belief = character(), ah_belief = character(), utterance = character(), prob = numeric())

for (sp_belief in speaker_beliefs) {
  for (ah_belief in ah_beliefs) {
    PS_tmp = eval_webppl(paste("speaker({speaker_belief:'",sp_belief,"', ah_belief:'",ah_belief,"'})",sep=""))
    for (i in 1:nrow(PS_tmp)) {
      PS = PS %>% 
        add_row(speaker_belief = sp_belief, ah_belief = ah_belief, utterance = PS_tmp$support[i], prob = PS_tmp$prob[i])
    }
  }
}

PS_summary = PS %>% 
  mutate(utterance_type = ifelse(str_detect(utterance, "doesnt"), "not p","p"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         # speaker_belief = ifelse(str_detect(speaker_belief, "doesnt"), "not p", "p"),
         # speaker_belief = case_when(str_detect(speaker_belief, "doesnt") ~ "not p",
         #                            str_detect(speaker_belief, "null") ~ "null",
         #                            TRUE ~ "p"),
         speaker_belief = case_when(str_detect(speaker_belief, "doesnt") ~ "not p",
                                    str_detect(speaker_belief, "null") ~ "null",
                                    str_detect(speaker_belief, "uncertain") ~ "uncertain",
                                    TRUE ~ "p"),
         # ah_belief = case_when(str_detect(ah_belief, "doesnt") ~ "not p",
         #                       str_detect(ah_belief, "null") ~ "null",
         #                       TRUE ~ "p"),
         ah_belief = case_when(str_detect(ah_belief, "doesnt") ~ "not p",
                               str_detect(ah_belief, "null") ~ "null",
                               str_detect(ah_belief, "uncertain") ~ "uncertain",
                               TRUE ~ "p"),
         utterance = case_when(utterance == "BARE-dances-?" ~ "Polar",
                               utterance == "know-dances-?" ~ '"know-p"',
                               utterance == "think-dances-?" ~ '"think-p"',
                               utterance == "know-doesnt_dance-?" ~ '"know-not_p"',
                               TRUE ~ '"think-not_p"'),
         belief_states = paste("<",speaker_belief,", ",ah_belief,">", sep="")) %>%
  group_by(utterance,belief_states,predicate,utterance_type) %>% 
  summarize(mean_prob = mean(prob)) %>% 
  ungroup() %>% 
  mutate(belief_states = fct_relevel(belief_states, '<p, p>','<p, not p>', '<p, uncertain>', '<p, null>', '<not p, p>', '<not p, not p>', '<not p, uncertain>', '<not p, null>', '<uncertain, p>','<uncertain, not p>', '<uncertain, uncertain>', '<uncertain, null>'),
         utterance = fct_relevel(utterance, 'Polar', '"know-p"', '"know-not_p"', '"think-p"', '"think-not_p"'),
         predicate = fct_relevel(predicate, 'Polar', 'think', 'know'),
         utterance_type = fct_relevel(utterance_type, 'p', 'not p'))

# ggplot(PS_summary, 
#        aes(x=utterance, y=mean_prob, fill = predicate, pattern = utterance_type)) +
#   # geom_bar(stat="identity") +
#   geom_bar_pattern(stat="identity",
#                    position = position_dodge2(preserve = "single"),
#                    width = 0.8,
#                    color = "black", 
#                    pattern_fill = "black",
#                    pattern_angle = 45,
#                    pattern_density = 0.1,
#                    pattern_spacing = 0.05,
#                    pattern_key_scale_factor = 0.6) +
#   facet_grid(belief_states~.) +
#   scale_fill_manual(values=cbPalette[2:4],
#                     # labels=c('Polar', 'think', 'know'),
#                     name="Predicate") +
#   scale_pattern_manual(values = c("p" = "stripe", "not p" = "none")) +
#   scale_x_discrete(name="Utterance",
#                    guide=guide_axis(angle = 65)) +
#   theme(legend.position="top") +
#   ylab("Production probability")

ggplot(PS_summary,
       aes(x=utterance, y=mean_prob, fill = predicate)) +
  geom_bar(stat="identity") +
  facet_wrap(belief_states~.) +
  scale_fill_manual(values=cbPalette[2:4],
                    # labels=c('Polar', 'think', 'know'),
                    name="Predicate") +
  scale_x_discrete(name="Utterance",
                   guide=guide_axis(angle = 65)) +
  theme(legend.position="top") +
  ylab("Production probability")
ggsave("../graphs/chemo_production/chemo_production-PS1234-bin3.pdf",width=6,height=4)

# pragmatic listener ----
## BDA ----
### marginal sp_belief ----
listener_sp_predictives <- data.frame(seed = character(), item = character(), utterance = character(), speaker_belief = character(), prob = numeric())

sp_path <- "../results/chemo_production/bda/pragListener/speaker/"
for (file in list.files(sp_path)) {
  name <- gsub("^([[:alnum:]]+)-small.csv", "\\1", file)
  predictives <- read.csv(paste0(sp_path,file))
  predictives$seed <- name
  listener_sp_predictives <- rbind(listener_sp_predictives, predictives)
}

sp <- listener_sp_predictives %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              str_detect(utterance, "BARE") ~ "Polar",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         prior_condition = gsub("^([A-Za-z]+)_([A-Za-z]+)", "\\2", item)) %>% 
  mutate(prior_condition = case_when(polarity=="neg" & prior_condition=="H" ~ "L",
                                     polarity=="neg" & prior_condition=="L" ~ "H",
                                     TRUE ~ prior_condition)) %>% 
  filter((polarity == "pos" & speaker_belief == "dances") |
           polarity == "neg" & speaker_belief == "doesnt_dance" |
           polarity == "Polar" & speaker_belief == "dances") %>%
  mutate(polarity = fct_relevel(polarity, "Polar", "pos", "neg" ),
         predicate = fct_relevel(predicate, "Polar", "think", "know")) %>%
  # select(-c(utterance, speaker_belief))
  select(-utterance)

sp_summary_binary <- sp %>% 
  group_by(prior_condition, polarity, predicate) %>% 
  summarize(mean_prob = mean(prob),
            prob_ci_low = ci.low(prob),
            prob_ci_high = ci.high(prob)) %>%  
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)
sp_summary_binary$condition = "model"

#### bar graph ----
empirical <- read.csv("../../../data/2_listenerSpeakerAH/main/bda_data.csv")
empirical_speaker <- empirical %>% 
  filter(predicate %in% c("Polar", "know", "think")) %>% 
  mutate(prior_condition = gsub("^([A-Za-z]+)_([A-Za-z]+)", "\\2", item)) %>% 
  mutate(prior_condition = case_when(polarity=="neg" & prior_condition=="H" ~ "L",
                                     polarity=="neg" & prior_condition=="L" ~ "H",
                                     TRUE ~ prior_condition)) %>% 
  group_by(predicate,prior_condition, polarity) %>%
  summarize(mean_prob = mean(speaker_response),
            prob_ci_low = ci.low(speaker_response),
            prob_ci_high = ci.high(speaker_response)) %>% 
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)
empirical_speaker$condition = "empirical"

sp_summary_combined <- rbind(sp_summary_binary, empirical_speaker) %>% 
  mutate(polarity = ifelse(polarity %in% c("Polar","pos"), "pos", "neg"),
         polarity = ifelse(polarity == "pos", "p", "not p"))

speaker_belief_comparison <- ggplot(data = sp_summary_combined,
                                    mapping = aes(x = condition,
                                                  y = mean_prob,
                                                  alpha = prior_condition,
                                                  fill = predicate)) +
  geom_bar(stat="identity",
           position = position_dodge2(preserve = "single"),
           width = 0.8,
           color = "black") +
  geom_errorbar(aes(ymin=prob_YMin,
                    ymax=prob_YMax),
                position=position_dodge(width=0.8),
                width=.2) +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") +
  scale_fill_manual(values=cbPalette[2:4],
                    labels=c("simple", "think", "know"),
                    guide="none") +
  scale_alpha_discrete(range=c(.9,.4),
                       labels=c("high prob", "low prob"),
                       name="prior belief in\nembedded content",
                       guide="none") +
  facet_grid(polarity ~ predicate)+
             # labeller = content_predicate_labeller) +
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "",
       y = "Speaker belief\nin the embedded content") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
speaker_belief_comparison
ggsave(speaker_belief_comparison, file="../graphs/chemo_production/model_eval/sp_belief_comparison.pdf", width=7, height=4)

#### line plot ----
# not used
# item_priors <- item_priors %>% 
#   rename(speaker_belief = state,
#          prior_prob = prop)
# 
# sp_summary <- sp %>% 
#   left_join(item_priors, by=c("item", "speaker_belief"))
# 
# # plot speaker_belief ~ prior belief
# agr_sp = sp_summary %>%
#   # distinct(speaker_belief, ah_belief, utterance, prior, .keep_all = TRUE) %>%
#   group_by(speaker_belief, prior_prob,polarity,predicate, item) %>%
#   summarize(posterior_belief = mean(prob)) %>%
#   mutate(polarity = case_when(polarity=="pos" ~ "Embedded: p",
#                               polarity=="neg" ~ "Embedded: not p",
#                               TRUE ~ "Polar")) %>% 
#   # reorder for the graph
#   # mutate(predicate = fct_relevel(predicate, "Polar", "think", "know"),
#   #        polarity = fct_relevel(polarity, rev)) %>% 
#   ungroup()
# 
# combine_summary_sp <- agr_sp %>% 
#   group_by(predicate, polarity) %>%  # collapse prior condition
#   summarize(mean_posterior_belief = mean(posterior_belief),
#             speaker_ci_low = ci.low(posterior_belief),
#             speaker_ci_high = ci.high(posterior_belief)) %>%  
#   ungroup() %>% 
#   mutate(speaker_YMin = mean_posterior_belief - speaker_ci_low,
#          speaker_YMax = mean_posterior_belief + speaker_ci_high)
# 
# ggplot(agr_sp,aes(x=prior_prob,y=posterior_belief, color=predicate)) +
#   geom_point(alpha=0.6) +
#   geom_smooth(method="lm", fullrange=TRUE) +
#   geom_point(data=combine_summary_sp, 
#              aes(x=combined_prior,
#                  y=mean_posterior_belief,
#                  fill=predicate),
#              shape=21,size=2,color="black",stroke=1) +
#   geom_errorbar(data=combine_summary_sp,
#                 aes(x=combined_prior,y=mean_posterior_belief,
#                     ymin=speaker_YMin, ymax=speaker_YMax),
#                 width=0.05,
#                 color="black") +
#   facet_grid(.~polarity) +
#   scale_color_manual(values=cbPalette[2:4]) +
#   scale_fill_manual(values=cbPalette[2:4]) +
#   scale_y_continuous(limits = c(0,1),
#                      breaks=seq(0,1,by=.25),
#                      name = "Posterior speaker belief\nin the embedded content") +
#   scale_x_continuous(breaks=seq(0,1,by=0.2),
#                      name = "Rating of prior belief in the embedded content")

### marginal ah_belief ----
listener_ah_predictives <- data.frame(seed = character(), item = character(), utterance = character(), ah_belief = character(), prob = numeric())

ah_path <- "../results/chemo_production/bda/pragListener/ah/"
for (file in list.files(ah_path)) {
  name <- gsub("^([[:alnum:]]+)-small.csv", "\\1", file)
  predictives <- read.csv(paste0(ah_path,file))
  predictives$seed <- name
  listener_ah_predictives <- rbind(listener_ah_predictives, predictives)
}


ah <- listener_ah_predictives %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              str_detect(utterance, "BARE") ~ "Polar",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         prior_condition = gsub("^([A-Za-z]+)_([A-Za-z]+)", "\\2", item)) %>% 
  mutate(prior_condition = case_when(polarity=="neg" & prior_condition=="H" ~ "L",
                                     polarity=="neg" & prior_condition=="L" ~ "H",
                                     TRUE ~ prior_condition)) %>% 
  filter(polarity!="Polar") %>% # no ah in simple polar
  filter((polarity == "pos" & ah_belief == "dances") |
           polarity == "neg" & ah_belief == "doesnt_dance") %>%
  mutate(polarity = fct_relevel(polarity, "pos", "neg" ),
         predicate = fct_relevel(predicate, "think", "know")) %>%
  select(-utterance)

ah_summary_binary <- ah %>% 
  group_by(prior_condition, polarity, predicate) %>% 
  summarize(mean_prob = mean(prob),
            prob_ci_low = ci.low(prob),
            prob_ci_high = ci.high(prob)) %>%  
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)
ah_summary_binary$condition = "model"

#### bar graph ----
empirical_ah <- empirical %>% 
  filter(predicate %in% c("know", "think")) %>% 
  mutate(prior_condition = gsub("^([A-Za-z]+)_([A-Za-z]+)", "\\2", item)) %>% 
  mutate(prior_condition = case_when(polarity=="neg" & prior_condition=="H" ~ "L",
                                     polarity=="neg" & prior_condition=="L" ~ "H",
                                     TRUE ~ prior_condition)) %>% 
  group_by(predicate,prior_condition, polarity) %>%
  summarize(mean_prob = mean(ah_response),
            prob_ci_low = ci.low(ah_response),
            prob_ci_high = ci.high(ah_response)) %>% 
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)
empirical_ah$condition = "empirical"

ah_summary_combined <- rbind(ah_summary_binary, empirical_ah) %>% 
  mutate(polarity = ifelse(polarity == "pos", "p", "not p"))

ah_belief_comparison <- ggplot(data = ah_summary_combined,
                                    mapping = aes(x = condition,
                                                  y = mean_prob,
                                                  alpha = prior_condition,
                                                  fill = predicate)) +
  geom_bar(stat="identity",
           position = position_dodge2(preserve = "single"),
           width = 0.8,
           color = "black") +
  geom_errorbar(aes(ymin=prob_YMin,
                    ymax=prob_YMax),
                position=position_dodge(width=0.8),
                width=.2) +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") +
  scale_fill_manual(values=cbPalette[3:4],
                    labels=c("think", "know"),
                    guide="none") +
  scale_alpha_discrete(range=c(.9,.4),
                       labels=c("high prob", "low prob"),
                       name="prior belief in\nembedded content",) +
  facet_grid(polarity ~ predicate)+
  # labeller = content_predicate_labeller) +
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "",
       y = "Attitude holder belief\nin the embedded content") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
ah_belief_comparison
ggsave(ah_belief_comparison, file="../graphs/chemo_production/model_eval/ah_belief_comparison.pdf", width=7, height=4)

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

# u <- "know-dances-?"
# it <- "Charley_L"
# PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",it,"')",sep="")) 

PL = data.frame(utterance = character(), prior = character(), speaker_belief = character(), ah_belief = character(), prob = numeric())

for (u in utterances) {
  for (it in items) {
    PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",it,"')",sep="")) # %>% 
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
        add_row(utterance = u, prior = it, speaker_belief = PL_tmp$speaker_belief[i], ah_belief = PL_tmp$ah_belief[i], prob = PL_tmp$prob[i])
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

# plot speaker_belief ~ prior belief
agr_sp = PL_summary %>%
  filter(speaker_belief == "dances") %>% 
  mutate(predicate = ifelse(predicate == "BARE", "Polar", predicate)) %>% 
  mutate(predicate = fct_relevel(predicate, "Polar", "think", "know")) %>% # reorder for the graph
  # distinct(speaker_belief, ah_belief, utterance, prior, .keep_all = TRUE) %>%
  group_by(utterance,speaker_belief, prior_mean,polarity,predicate, prior) %>%
  summarize(posterior_belief_p = sum(prob)) %>%
  mutate(posterior_belief_emb = case_when(polarity == "neg" ~ 1 - posterior_belief_p,
                                          TRUE ~ posterior_belief_p),
         prior_mean = ifelse(polarity=="neg", 1-prior_mean, prior_mean),
         polarity = case_when(polarity=="pos" ~ "Embedded: p",
                              polarity=="neg" ~ "Embedded: not p",
                              TRUE ~ "Polar")) %>% 
  mutate(polarity = fct_relevel(polarity, rev)) %>% 
  ungroup()

combine_summary_sp <- agr_sp %>% 
  group_by(predicate, polarity) %>%  # collapse prior condition
  summarize(mean_posterior_belief = mean(posterior_belief_emb),
            speaker_ci_low = ci.low(posterior_belief_emb),
            speaker_ci_high = ci.high(posterior_belief_emb)) %>%  
  ungroup() %>% 
  mutate(speaker_YMin = mean_posterior_belief - speaker_ci_low,
         speaker_YMax = mean_posterior_belief + speaker_ci_high)

ggplot(agr_sp,aes(x=prior_mean,y=posterior_belief_emb, color=predicate)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm", fullrange=TRUE) +
  geom_point(data=combine_summary_sp, 
             aes(x=combined_prior,
                 y=mean_posterior_belief,
                 fill=predicate),
             shape=21,size=2,color="black",stroke=1) +
  geom_errorbar(data=combine_summary_sp,
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
ggsave("../../BDA/graphs/chemoProduction_predictives-estimate6789.pdf",width=7,height=3)
