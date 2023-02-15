library(lme4)
library(dplyr)
library(emmeans)
library(ggpattern)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Helper functions for the graph
theme_set(theme_bw())
# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 


content_label <- list("pos"="Embedded content:p",
                      "neg"="Embedded content:not p")
content_labeller <- function(variable,value){
  return(content_label[value])
}

predicate_label <- list("simple"="Polar",
                        "think"="think",
                        "know"="know")
predicate_labeller <- function(variable,value){
  return(predicate_label[value])
}

content_predicate_labeller <- function(variable,value){
  if (variable=='utterance_type') {
    return(content_label[value])
  } else {
    return(predicate_label[value])
  }
}

ah_predicate_label <- list("think"="think",
                        "know"="know")
ah_predicate_labeller <- function(variable,value){
  return(ah_predicate_label[value])
}

ah_content_predicate_labeller <- function(variable,value){
  if (variable=='utterance_type') {
    return(content_label[value])
  } else {
    return(ah_predicate_label[value])
  }
}


#### CERTAINTY
df.data.speaker.model <- read.csv("../results/certainty_production/speaker_model.csv", header=TRUE)
df.data.belief.model <- read.csv("../results/certainty_production/belief_model.csv", header=TRUE)
df.data.certainty.model <- read.csv("../results/certainty_production/certainty_model.csv", header=TRUE)


speaker_model_prediction <- ggplot(data = df.data.speaker.model |>
                                     mutate(utterances = fct_relevel(utterances, '"p?"','"knew p?"','"knew not p?"', '"thought p?"', '"thought not p?"')),
                                   mapping = aes(x = utterances,
                                                 y = probability)) +
  geom_bar(stat="identity",
           position="dodge",
           width = 0.8)+
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") +
  facet_grid(cols = vars(certainty)) +
  scale_y_continuous(limits = c(0,1)) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "Utterances",
       y = "Probability of producing the utterance")
speaker_model_prediction
ggsave(speaker_model_prediction, file="../graphs/certainty_production/pragSpeaker.png", width=5, height=3)

belief_model_prediction <- ggplot(data = df.data.belief.model |>
                                    filter(content == "p",
                                           utterance %in% c('"p?"','"thought p?"','"knew p?"')) |>
                                    mutate(utterance = fct_relevel(utterance, '"p?"','"thought p?"','"knew p?"')),
                                  mapping = aes(x = utterance,
                                                y = probability,
                                                fill = utterance)) +
  geom_bar(stat="identity",
           position="dodge",
           width = 0.8,
           color = "black") +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") +
  scale_fill_manual(values=cbPalette[2:4],
                    labels=c("p?", "thought p?", "knew p?"),
                    guide="none") +
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "Utterance",
       y = "Probability of the belief\n being 'p'")
belief_model_prediction
ggsave(belief_model_prediction, file="../graphs/certainty_production/pragListener_belief.png", width=5, height=3)

certainty_model_prediction <- ggplot(data = df.data.certainty.model |>
                                    filter(certainty == "certain",
                                           utterance %in% c('"p?"','"thought p?"','"knew p?"')) |>
                                    mutate(utterance = fct_relevel(utterance, '"p?"','"thought p?"','"knew p?"')),
                                  mapping = aes(x = utterance,
                                                y = probability,
                                                fill = utterance)) +
  geom_bar(stat="identity",
           position="dodge",
           width = 0.8,
           color = "black") +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") +
  scale_fill_manual(values=cbPalette[2:4],
                    labels=c('"p?"', '"thought p?"', '"knew p?"'),
                    guide="none") +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Utterance",
       y = "Probability of the speaker being certain\nabout the inferred belief")
certainty_model_prediction
ggsave(certainty_model_prediction, file="../graphs/certainty_production/pragListener_certainty.png", width=5, height=3)


#### CHEMO_PRODUCTION
# load the data
df.speaker_ah_belief <- read.csv("../results/chemo_production/praglistener_model_think.csv", header=TRUE)

# speaker belief
speaker_belief_comparison <- ggplot(data = df.speaker_ah_belief |>
                                    mutate(predicate = fct_relevel(predicate, 'simple','think','know'),
                                           utterance_type == fct_relevel(utterance_type, "pos", "neg")),
                                  mapping = aes(x = condition,
                                                y = speaker_belief,
                                                alpha = prior_condition,
                                                fill = predicate)) +
  geom_bar(stat="identity",
           position = position_dodge2(preserve = "single"),
           width = 0.8,
           color = "black") +
  geom_errorbar(aes(ymin=speaker_YMin,
                    ymax=speaker_YMax),
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
  facet_grid(utterance_type ~ predicate,
             labeller = content_predicate_labeller) +
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "",
       y = "Speaker belief in the embedded content") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
speaker_belief_comparison
# ggsave(speaker_belief_comparison, file="../graphs/chemo_production/speaker_belief_bare_comparison_test.pdf", width=6, height=4)

# ah belief
ah_belief_comparison <- ggplot(data = df.speaker_ah_belief |>
                                 filter(predicate != "simple") |>
                                      mutate(predicate = fct_relevel(predicate, 'think','know'),
                                             utterance_type == fct_relevel(utterance_type, "pos", "neg")),
                                    mapping = aes(x = condition,
                                                  y = ah_belief,
                                                  alpha = prior_condition,
                                                  fill = predicate)) +
  geom_bar(stat="identity",
           position = position_dodge2(preserve = "single"),
           width = 0.8,
           color = "black") +
  geom_errorbar(aes(ymin=ah_YMin,
                    ymax=ah_YMax),
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
                       name="prior belief in\nembedded content") +
  facet_grid(utterance_type ~ predicate,
             labeller = ah_content_predicate_labeller) +
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "",
       y = "Attitude holder belief in the embedded content") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12))
ah_belief_comparison
# ggsave(ah_belief_comparison, file="../graphs/chemo_production/ah_belief_bare_comparison_test.pdf", width=7, height=4)


df.production <- read.csv("../results/chemo_production/production_model_think.csv", header=TRUE)
production_model <- ggplot(data = df.production |>
                                 mutate(belief_states = fct_relevel(belief_states, '<p, p>','<p, not p>', '<not p, p>', '<not p, not p>', '<p, >', '<not p, >'),
                                        utterance = fct_relevel(utterance, 'Polar', '"know p"', '"know not p"', '"think p"', '"think not p"'),
                                        predicate = fct_relevel(predicate, 'Polar', 'think', 'know'),
                                        utterance_type = fct_relevel(utterance_type, 'p', 'not p')),
                                        
                               mapping = aes(x = utterance,
                                             y = prob,
                                             fill = predicate,
                                             pattern = utterance_type)) +
  # geom_bar(stat="identity",
  #          position = position_dodge2(preserve = "single"),
  #          width = 0.8,
  #          color = "black") +
  geom_bar_pattern(stat="identity",
                   position = position_dodge2(preserve = "single"),
                   width = 0.8,
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.6) +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") +
  scale_fill_manual(values=cbPalette[2:4],
                    # labels=c('Polar', '"know p"', '"know not p"', '"think p"', '"think not p"'),
                    labels=c('Polar', 'think', 'know'),
                    name="Predicate") +
  scale_pattern_manual(values = c("p" = "stripe", "not p" = "none")) +
  scale_x_discrete(name="Utterance",
                   guide=guide_axis(angle = 65)) +
  facet_grid(. ~ belief_states) +
  # scale_y_continuous(limits = c(0,1)) +
  labs(x = "",
       y = "Production probability",
       pattern = "Type of embedded\nclause") +
  theme(legend.position="top") +
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14))
production_model
ggsave(production_model, file="../graphs/chemo_production/production_test.pdf", width=6, height=3)
