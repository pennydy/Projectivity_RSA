library(lme4)
library(dplyr)
library(emmeans)
library(ggpattern)
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
source("../../../helpers.R")
df.data.belief.model <- read.csv("../results/belief_summary_model.csv", header=TRUE)
df.data.certainty.model <- read.csv("../results/marginal_certainty_summary.csv", header=TRUE)


### BELIEF RATINGS
belief_model_comp <- ggplot(data = df.data.belief.model |>
                                    filter(predicate != "MC") |>
                                    mutate(utterance_type = ifelse(as.character(utterance_type) == "polar", "pos", as.character(utterance_type)),
                                           predicate = fct_relevel(predicate, "simple","think","know")),
                                  mapping = aes(x = condition,
                                                y = mean_belief_rating,
                                                alpha = utterance_type,
                                                fill = predicate)) +
  geom_bar(stat="identity",
           position="dodge",
           color = "black") +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=position_dodge(0.9),
                width=.2,
                color="black") +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  scale_alpha_discrete(range=c(.3,.9), # include both p and not p (more opaque). 
                       labels=c("not p", "p"),
                       name="Embedded\ncontent") +
  scale_fill_manual(values=cbPalette[2:4],
                    labels=c("Polar", "think", "know"),guide="none") +
  scale_y_continuous(limits = c(0,1)) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "",
       y = "Mean belief in p")
belief_model_comp
ggsave(belief_model_comp, file="../graphs/belief_model_prior.pdf", width=5, height=3)


### CERTAINTY RATINGS
certainty_model_comp <- ggplot(data = df.data.certainty.model |>
                               mutate(predicate = fct_relevel(predicate, "simple","think","know")),
                       mapping = aes(x = condition,
                                     y = mean_certainty_rating,
                                     alpha = utterance_type,
                                     fill = predicate)) +
  geom_bar(stat="identity",
           position="dodge",
           color = "black") +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=position_dodge(0.9),
                width=.2,
                color="black") +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  scale_alpha_discrete(range=c(.3,.9), # include both p and not p (more opaque). 
                       labels=c("not p", "p"),
                       name="Embedded\ncontent") +
  scale_fill_manual(values=cbPalette[2:4],
                    labels=c("Polar", "think", "know"),guide="none") +
  scale_y_continuous(limits = c(0,1)) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "",
       y = "Mean certainty in the rated content")
certainty_model_comp
ggsave(certainty_model_comp, file="../graphs/certainty_model_prior.pdf", width=5, height=3)
