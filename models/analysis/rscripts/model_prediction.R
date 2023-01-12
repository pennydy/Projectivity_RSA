library(lme4)
library(dplyr)
library(emmeans)
library(tidyverse)


theme_set(theme_bw())
# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
predicate_label <- list("simple"="Polar",
                        "think"="think",
                        "know"="know")
predicate_labeller <- function(variable,value){
  return(predicate_label[value])
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df.data.speaker.model <- read.csv("../results/speaker_model.csv", header=TRUE)
df.data.belief.model <- read.csv("../results/belief_model.csv", header=TRUE)
df.data.certainty.model <- read.csv("../results/certainty_model.csv", header=TRUE)

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
ggsave(speaker_model_prediction, file="../graphs/pragSpeaker.png", width=5, height=3)

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
ggsave(belief_model_prediction, file="../graphs/pragListener_belief.png", width=5, height=3)

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
ggsave(certainty_model_prediction, file="../graphs/pragListener_certainty.png", width=5, height=3)
