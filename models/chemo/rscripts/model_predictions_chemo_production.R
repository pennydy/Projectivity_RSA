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
all_predictives_simulated <- data.frame(seed = character(), utterance = character(), speaker_belief = character(), ah_belief = character(), prob = numeric())

for (file in list.files("../results/chemo_production/bda/production")) {
  name <- gsub("^([[:alnum:]]+)-small.csv", "\\1", file)
  predictives <- read.csv(paste0("../results/chemo_production/bda/production/",file))
  predictives$seed <- name
  all_predictives_simulated <- rbind(all_predictives_simulated, predictives)
}


all_predictives <- all_predictives_simulated %>% 
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
ggsave(production, file="../graphs/chemo_production/cogsci_talk/production.pdf", width=6, height=5)

#### collapse by each sp belief ----
##### bare ----
bare <- all_predictives %>% 
  filter(utterance=="Polar") %>% 
  group_by(seed) %>%
  mutate(marg_prob = sum(prob)) %>% # how likely is Polar 
  ungroup() %>% 
  group_by(seed, ah_belief) %>% 
  mutate(ah_prob = sum(prob)/marg_prob) %>% 
  ungroup() %>% 
  group_by(ah_belief) %>% 
  summarize(mean_prob = mean(ah_prob),
            prob_ci_low = ci.low(ah_prob),
            prob_ci_high = ci.high(ah_prob)) %>%  
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)

bare_plot <- ggplot(bare %>% 
                      mutate(ah_belief=fct_relevel(ah_belief, "p", "not p", "?", "null")),
                    aes(x=ah_belief, y=mean_prob, fill=cbPalette[2])) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=prob_YMin,
                    ymax=prob_YMax),
                width=.2) +
  scale_fill_manual(values=cbPalette[2],
                    name="Predicate",
                    guide="none") +
  scale_y_continuous(limits = c(0,1)) + 
  scale_x_discrete(name="Attitude holder belief") +
  ylab("Production probability") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size=16))
bare_plot
ggsave(bare_plot,file="../graphs/chemo_production/cogsci_talk/production_bare.pdf",
       width=6,height=4)

bare_sp <- all_predictives %>% 
  filter(utterance=="Polar" & ah_belief == "null") %>% 
  group_by(seed) %>%
  mutate(marg_prob = sum(prob)) %>% 
  ungroup() %>% 
  group_by(seed, speaker_belief) %>% 
  mutate(ah_prob = prob/marg_prob) %>% 
  ungroup() %>% 
  group_by(speaker_belief) %>% 
  summarize(mean_prob = mean(ah_prob),
            prob_ci_low = ci.low(ah_prob),
            prob_ci_high = ci.high(ah_prob)) %>%  
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)

bare_sp_plot <- ggplot(bare_sp %>% 
                      mutate(speaker_belief=fct_relevel(speaker_belief, "p", "not p", "?")),
                    aes(x=speaker_belief, y=mean_prob, fill=cbPalette[2])) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=prob_YMin,
                    ymax=prob_YMax),
                width=.2) +
  scale_fill_manual(values=cbPalette[2],
                    name="Predicate",
                    guide="none") +
  scale_x_discrete(name="Speaker belief") +
  ylab("Production probability") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16))
bare_sp_plot
ggsave(bare_sp_plot,file="../graphs/chemo_production/cogsci_talk/production_bare_sp.pdf",
       width=6,height=4)

##### know ----
know <- all_predictives %>% 
  filter(predicate=="know") %>% 
  group_by(seed) %>%
  mutate(marg_prob = sum(prob)) %>% # how likely is Polar 
  ungroup() %>% 
  group_by(seed, utterance_type, speaker_belief) %>% 
  mutate(sp_prob = sum(prob)/marg_prob) %>% 
  ungroup() %>% 
  group_by(utterance_type, speaker_belief) %>% 
  summarize(mean_prob = mean(sp_prob),
            prob_ci_low = ci.low(sp_prob),
            prob_ci_high = ci.high(sp_prob)) %>%  
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)

know_plot <- ggplot(know %>% 
                      mutate(speaker_belief=fct_relevel(speaker_belief, "p", "not p", "?"),
                             utterance_type=fct_relevel(utterance_type, "p", "not p")),
                    aes(x=speaker_belief, y=mean_prob, fill=cbPalette[4])) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=prob_YMin,
                    ymax=prob_YMax),
                width=.2) +
  scale_fill_manual(values=cbPalette[4],
                    name="Predicate",
                    guide="none") +
  facet_grid(.~utterance_type) +
  scale_x_discrete(name="Speaker belief") +
  ylab("Production probability") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size=16))
know_plot
ggsave(know_plot,file="../graphs/chemo_production/cogsci_talk/production_know_sp.pdf",
       width=6,height=4)

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
                       name="prior belief in\nembedded content") +
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
ggsave(speaker_belief_comparison, file="../graphs/chemo_production/cogsci_model_eval/sp_belief_comparison_legend.pdf", width=8, height=4)

##### collapse prior (by predicates) ----

sp_summary_by_predicate <- sp %>% 
  group_by(polarity, predicate) %>% 
  summarize(mean_prob = mean(prob),
            prob_ci_low = ci.low(prob),
            prob_ci_high = ci.high(prob)) %>%  
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)
sp_summary_by_predicate$condition = "model"

empirical_speaker_by_predicate <- empirical %>% 
  filter(predicate %in% c("Polar", "know", "think")) %>% 
  mutate(prior_condition = gsub("^([A-Za-z]+)_([A-Za-z]+)", "\\2", item)) %>% 
  group_by(predicate,polarity) %>%
  summarize(mean_prob = mean(speaker_response),
            prob_ci_low = ci.low(speaker_response),
            prob_ci_high = ci.high(speaker_response)) %>% 
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)
empirical_speaker_by_predicate$condition = "empirical"

sp_summary_by_predicate <- rbind(sp_summary_by_predicate, empirical_speaker_by_predicate) %>% 
  mutate(polarity = ifelse(polarity %in% c("Polar","pos"), "pos", "neg"),
         polarity = ifelse(polarity == "pos", "p", "not p"))

comparison_by_predicate <- ggplot(data = sp_summary_by_predicate,
                                    mapping = aes(x = condition,
                                                  y = mean_prob,
                                                  fill = predicate)) +
  geom_bar(stat="identity",
           width = 0.8,
           color = "black") +
  geom_errorbar(aes(ymin=prob_YMin,
                    ymax=prob_YMax),
                width=.2) +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") +
  scale_fill_manual(values=cbPalette[2:4],
                    labels=c("simple", "think", "know"),
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
comparison_by_predicate
ggsave(comparison_by_predicate, file="../graphs/chemo_production/cogsci_talk/sp_belief_comparison_by_predicate.pdf", width=6, height=4)


comparison_by_condition <- ggplot(data = sp_summary_by_predicate,
                                  mapping = aes(x = condition,
                                                y = mean_prob,
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
                    labels=c("simple", "think", "know")) +
  facet_grid(polarity ~ .)+
  # labeller = content_predicate_labeller) +
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "",
       y = "Speaker belief\nin the embedded content") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.position="top")
comparison_by_condition
ggsave(comparison_by_condition, file="../graphs/chemo_production/cogsci_talk/sp_belief_comparison_by_condition.pdf", width=6, height=4)


##### collapse predicates (by prior) ----
sp_summary_by_prior <- sp %>% 
  mutate(polarity = ifelse(polarity %in% c("Polar","pos"), "pos", "neg")) %>% 
  group_by(prior_condition, polarity) %>% 
  summarize(mean_prob = mean(prob),
            prob_ci_low = ci.low(prob),
            prob_ci_high = ci.high(prob)) %>%  
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)
sp_summary_by_prior$condition = "model"

empirical_speaker_by_prior <- empirical %>% 
  filter(predicate %in% c("Polar", "know", "think")) %>% 
  mutate(polarity = ifelse(polarity %in% c("Polar","pos"), "pos", "neg")) %>% 
  mutate(prior_condition = gsub("^([A-Za-z]+)_([A-Za-z]+)", "\\2", item)) %>% 
  mutate(prior_condition = case_when(polarity=="neg" & prior_condition=="H" ~ "L",
                                     polarity=="neg" & prior_condition=="L" ~ "H",
                                     TRUE ~ prior_condition)) %>% 
  group_by(prior_condition,polarity) %>%
  summarize(mean_prob = mean(speaker_response),
            prob_ci_low = ci.low(speaker_response),
            prob_ci_high = ci.high(speaker_response)) %>% 
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)
empirical_speaker_by_prior$condition = "empirical"

sp_summary_by_prior <- rbind(sp_summary_by_prior, empirical_speaker_by_prior) %>% 
  mutate(polarity = ifelse(polarity == "pos", "p", "not p"))

comparison_by_prior <- ggplot(data = sp_summary_by_prior %>% 
                                mutate(prior_condition = fct_relevel(prior_condition, "L", "H")),
                                    mapping = aes(x = condition,
                                                  y = mean_prob,
                                                  alpha = prior_condition)) +
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
  scale_alpha_discrete(range=c(.9,.4),
                       labels=c("low prob", "high prob"),
                       name="prior belief in\nembedded content") +
  facet_grid(polarity ~ .)+
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "",
       y = "Speaker belief\nin the embedded content") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
comparison_by_prior
ggsave(comparison_by_prior, file="../graphs/chemo_production/cogsci_talk/sp_belief_comparison_by_prior.pdf", width=8, height=4)

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

### plots for talk intro ----
#### predicates ----
binary_predicates <- data.frame(predicate = c("think", "know"), mean_prob = 0:1)
binary_predicate_plot<- ggplot(data = binary_predicates %>% 
                                 mutate(predicate = fct_relevel(predicate, "think", "know")),
                        mapping = aes(x = predicate,
                                      y = mean_prob,
                                      fill = predicate)) +
  geom_bar(stat="identity",
           width = 0.8,
           color = "black") +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") +
  scale_fill_manual(values=cbPalette[3:4],
                    labels=c("think", "know"),
                    guide="none")+
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "",
       y = "Speaker belief in the\nembedded content") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
binary_predicate_plot
ggsave(binary_predicate_plot, file="../graphs/chemo_production/cogsci_talk/binary_predicate.pdf", width=6, height=4)

know_think <- empirical %>% 
  filter(predicate %in% c("know", "think")) %>% 
  # mutate(polarity = ifelse(polarity %in% c("Polar","pos"), "pos", "neg")) %>% 
  # mutate(prior_condition = gsub("^([A-Za-z]+)_([A-Za-z]+)", "\\2", item)) %>% 
  # mutate(prior_condition = case_when(polarity=="neg" & prior_condition=="H" ~ "L",
  #                                    polarity=="neg" & prior_condition=="L" ~ "H",
  #                                    TRUE ~ prior_condition)) %>% 
  group_by(predicate) %>%
  summarize(mean_prob = mean(speaker_response),
            prob_ci_low = ci.low(speaker_response),
            prob_ci_high = ci.high(speaker_response)) %>% 
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)

predicate_plot<- ggplot(data = know_think %>% 
                          mutate(predicate=fct_relevel(predicate, "think", "know")),
                                  mapping = aes(x = predicate,
                                                y = mean_prob,
                                                fill = predicate)) +
  geom_bar(stat="identity",
           width = 0.5,
           color = "black") +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") +
  scale_fill_manual(values=cbPalette[3:4],
                    labels=c("think", "know"),
                    guide="none")+
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "",
       y = "",
       title= "") +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title=element_text(size=12),
        legend.text=element_text(size=14))
predicate_plot
ggsave(predicate_plot, file="../graphs/chemo_production/cogsci_talk/predicate_no_title.pdf", width=4, height=4)

#### prior ----
prior <- empirical %>% 
  filter(predicate %in% c("know")) %>% 
  mutate(prior_condition = gsub("^([A-Za-z]+)_([A-Za-z]+)", "\\2", item),
         item = gsub("^([A-Za-z]+)_([A-Za-z]+)", "\\1", item)) %>%
  mutate(prior_condition = case_when(polarity=="neg" & prior_condition=="H" ~ "L",
                                     polarity=="neg" & prior_condition=="L" ~ "H",
                                     TRUE ~ prior_condition)) %>%
  filter(item=="Josie" & polarity == "pos") %>% 
  group_by(prior_condition) %>%
  summarize(mean_prob = mean(speaker_response),
            prob_ci_low = ci.low(speaker_response),
            prob_ci_high = ci.high(speaker_response)) %>% 
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)


prior_plot<- ggplot(data = prior %>% 
                      mutate(prior_condition = fct_relevel(prior_condition, "L", "H")),
                        mapping = aes(x = prior_condition,
                                      y = mean_prob,
                                      alpha = prior_condition)) +
  geom_bar(stat="identity",
           width = 0.5,
           color = "black") +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") +
  scale_alpha_discrete(range=c(.9,.4),
                       labels=c("high prob", "low prob"),
                       name="prior belief in\nembedded content",
                       guide="none") +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete(labels=c("German", "Cuban")) +
  labs(x = "",
       y = "",
       title = "Ascribed speaker belief in the\nembedded content(by prior)") +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title=element_text(size=12),
        legend.text=element_text(size=14))
prior_plot
ggsave(prior_plot, file="../graphs/chemo_production/cogsci_talk/prior.pdf", width=4, height=4)

#### at-issueness ----
at_issue <- data.frame(ai = c("At-issue", "Not at-issue"), mean_prob = c(0.3,0.7))
at_issue_plot<- ggplot(data = at_issue,
                               mapping = aes(x = ai,
                                             y = mean_prob,
                                             alpha = ai)) +
  geom_bar(stat="identity",
           width = 0.5,
           color = "black") +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") +
  scale_alpha_discrete(range=c(.9,.4),
                       labels=c("At-issue", "Not at-issue"),
                       name="At-issueness of the embedded clause",
                       guide="none") +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete(labels=c("JULIAN", "KNOW")) +
  labs(x = "",
       y = "",
       title = "") +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
at_issue_plot
ggsave(at_issue_plot, file="../graphs/chemo_production/cogsci_talk/at_issue_no_title.pdf", width=4, height=4)

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

ah_belief_comparison <- ggplot(data = ah_summary_combined %>% 
                                 mutate(prior_condition = fct_relevel(prior_condition, "L","H")),
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
                       labels=c("low prob", "high prob"),
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
ggsave(ah_belief_comparison, file="../graphs/chemo_production/cogsci_talk/ah_belief_comparison.pdf", width=7, height=4)


##### collapse prior (by predicates) ----

ah_summary_by_predicate <- ah %>% 
  group_by(polarity, predicate) %>% 
  summarize(mean_prob = mean(prob),
            prob_ci_low = ci.low(prob),
            prob_ci_high = ci.high(prob)) %>%  
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)
ah_summary_by_predicate$condition = "model"

ah_empirical_speaker_by_predicate <- empirical %>% 
  filter(predicate %in% c("know", "think")) %>% 
  mutate(prior_condition = gsub("^([A-Za-z]+)_([A-Za-z]+)", "\\2", item)) %>% 
  group_by(predicate,polarity) %>%
  summarize(mean_prob = mean(ah_response),
            prob_ci_low = ci.low(ah_response),
            prob_ci_high = ci.high(ah_response)) %>% 
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)
ah_empirical_speaker_by_predicate$condition = "empirical"

ah_summary_by_predicate <- rbind(ah_summary_by_predicate, ah_empirical_speaker_by_predicate) %>% 
  mutate(polarity = ifelse(polarity %in% c("Polar","pos"), "pos", "neg"),
         polarity = ifelse(polarity == "pos", "p", "not p"))

ah_comparison_by_predicate <- ggplot(data = ah_summary_by_predicate,
                                  mapping = aes(x = condition,
                                                y = mean_prob,
                                                fill = predicate)) +
  geom_bar(stat="identity",
           width = 0.8,
           color = "black") +
  geom_errorbar(aes(ymin=prob_YMin,
                    ymax=prob_YMax),
                width=.2) +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") +
  scale_fill_manual(values=cbPalette[3:5],
                    labels=c("think", "know"),
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
ah_comparison_by_predicate
ggsave(ah_comparison_by_predicate, file="../graphs/chemo_production/cogsci_talk/ah_belief_comparison_by_predicate.pdf", width=6, height=4)


ah_comparison_by_condition <- ggplot(data = ah_summary_by_predicate,
                                  mapping = aes(x = condition,
                                                y = mean_prob,
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
                    labels=c("think", "know")) +
  facet_grid(polarity ~ .)+
  # labeller = content_predicate_labeller) +
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "",
       y = "Speaker belief\nin the embedded content") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.position="top")
ah_comparison_by_condition
ggsave(ah_comparison_by_condition, file="../graphs/chemo_production/cogsci_talk/ah_belief_comparison_by_condition.pdf", width=6, height=4)


##### collapse predicates (by prior) ----
ah_summary_by_prior <- ah %>% 
  group_by(prior_condition, polarity) %>% 
  summarize(mean_prob = mean(prob),
            prob_ci_low = ci.low(prob),
            prob_ci_high = ci.high(prob)) %>%  
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)
ah_summary_by_prior$condition = "model"

ah_empirical_speaker_by_prior <- empirical %>% 
  filter(predicate %in% c("know", "think")) %>% 
  mutate(prior_condition = gsub("^([A-Za-z]+)_([A-Za-z]+)", "\\2", item)) %>% 
  mutate(prior_condition = case_when(polarity=="neg" & prior_condition=="H" ~ "L",
                                     polarity=="neg" & prior_condition=="L" ~ "H",
                                     TRUE ~ prior_condition)) %>% 
  group_by(prior_condition,polarity) %>%
  summarize(mean_prob = mean(ah_response),
            prob_ci_low = ci.low(ah_response),
            prob_ci_high = ci.high(ah_response)) %>% 
  ungroup() %>% 
  mutate(prob_YMin = mean_prob - prob_ci_low,
         prob_YMax = mean_prob + prob_ci_high)
ah_empirical_speaker_by_prior$condition = "empirical"

ah_summary_by_prior <- rbind(ah_summary_by_prior, ah_empirical_speaker_by_prior) %>% 
  mutate(polarity = ifelse(polarity == "pos", "p", "not p"))

ah_comparison_by_prior <- ggplot(data = ah_summary_by_prior %>% 
                                   mutate(prior_condition = fct_relevel(prior_condition, "L", "H")),
                              mapping = aes(x = condition,
                                            y = mean_prob,
                                            alpha = prior_condition)) +
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
  scale_alpha_discrete(range=c(.9,.4),
                       labels=c("low prob", "high prob"),
                       name="prior belief in\nembedded content") +
  facet_grid(polarity ~ .)+
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "",
       y = "Speaker belief\nin the embedded content") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
ah_comparison_by_prior
ggsave(ah_comparison_by_prior, file="../graphs/chemo_production/cogsci_talk/ah_belief_comparison_by_prior.pdf", width=8, height=4)


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
