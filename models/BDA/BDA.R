setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(jsonlite)
library(rwebppl)

source("./inferenceHelpers.R")
source("./BDA_vizhelpers.R")

# read the pre-processed empirical data


# empirical_df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv")
# df <- empirical_df %>% 
#   mutate(speaker_response = trunc(trunc(speaker_response * 100)/10),
#          ah_response = trunc(trunc(ah_response * 100)/10)) %>%
#   filter(predicate %in% c("think", "know", "BARE")) # for now

# testing -> 3 bins 
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>% 
  mutate(speaker_response = case_when(speaker_response > 0.6 ~ 2,
                                      speaker_response < 0.4 ~ 0,
                                      TRUE ~ 1),
         ah_response = case_when(ah_response > 0.6 ~ 2,
                                 ah_response < 0.4 ~ 0,
                                 TRUE ~ 1)) %>%
  # mutate(speaker_response = ifelse(speaker_response == 1, 9,
  #                                  trunc(trunc(speaker_response*100) / 10 ))) %>%
  filter(predicate %in% c("think", "know", "BARE")) %>% 
  # testing: one item, one utterance
  # filter(uttearnce=="know-dances-?" & item=="Frank_H") 
  # testing: one item, all utterances
  filter(item=="Frank_H")
  # testing: multiple items, all utterances
  # filter(item %in% c("Frank_H", "Frank_L", "Charley_H", "Charley_L","Danny_H", "Danny_L", "Emily_H", "Emily_L","Emma_H", "Emma_L", "Grace_H", "Grace_L"))


# make the model 
# model <- makeModel("threshold_chemo.txt")
model <- makeModel("threshold_mix.txt")

# generate the posterior
# we will have different models for comparison, so will keep this format
# thresholdChemoScript <- wrapInference(model,"threshold_chemo", 2500, 10, 500)
# 
# thresholdChemoPosteriors <- webppl(thresholdChemoScript, data = df, data_var = "df", random_seed = 3333)

thresholdMixScript <- wrapInference(model,"threshold_mix", 2500, 10, 500) # sample:4000, lag: 10, burn: 500

thresholdMixPosteriors <- webppl(thresholdMixScript, data = df, data_var = "df", random_seed = 3333)
graphPosteriors(thresholdMixPosteriors) + ggtitle("Continuous posteriors")
# saveRDS(thresholdMixPosteriors, "results/thresholdMixPosteriors_enumerate.RDS")


thresholdMixPosteriors_know <-readRDS("results/thresholdMixPosteriors_know_frank.RDS")
graphPosteriors(thresholdMixPosteriors_know) + ggtitle("Continuous posteriors")

# saveRDS(thresholdMixPosteriors, "results/thresholdMixPosteriors_know_frank.RDS")

# saveRDS(thresholdChemoPosteriors, "results/thresholdChemoPosteriors.RDS")

# load the posteriors instead of re-running
thresholdChemoPosteriors <- readRDS("results/thresholdChemoPosteriors.RDS")

graphPosteriors(thresholdChemoPosteriors) + ggtitle("Continuous posteriors")

ggsave("results/thresholdChemoPosteriors.png")

# PREDICTIVES

# thresholdChemoEstimates <- getEstimates(thresholdChemoPosteriors) 

# thresholdChemoPredictionScript <- wrapPrediction(model, thresholdChemoEstimates,
#                                                  "know-dances-?", 
#                                                  "Charley_H")

# chemoPredictives <- webppl(thresholdChemoPredictionScript, data = unique(df %>%  select(utterance, item)), data_var = "df")

thresholdMixEstimates <- getEstimates(thresholdMixPosteriors) 

thresholdMixPredictionScript <- wrapPrediction(model, thresholdMixEstimates,
                                                 "know-dances-?", 
                                                 "Frank_H")

thresholdMixPredictives <- webppl(thresholdMixPredictionScript, data = unique(df %>%  select(utterance, item)), data_var = "df")

graphPredictives(continuousPredictives, d_collapsed)

ggsave("results/continuousPredictives.png", width = 4, height = 3, units = "in")







# DEGEN 2020-STYLE POSTERIOR GRAPHS 

graphNoisePosteriors(continuousPosteriors %>% filter(Parameter %in% c("colorNoiseVal","sizeNoiseVal")))
ggsave("results/continuousPosteriors_degen.pdf")



# # GRAPH PREDICTIVES BY RED EXP / BY SCENE VARIATION

source("../_shared/regressionHelpers.r")

d_regressionFormat <- read_delim("../../data/SpanishMain/data_exp1.tsv", delim = "\t") %>%
  mutate(condition = paste(SufficientProperty,NumDistractors,NumSameDistractors,sep = ""))

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

visualize_sceneVariation_withModel = function(d, predictives1,predictives2,predictives3,predictives4) {
  agr <- d %>%
    select(redundant,RedundantProperty,NumDistractors,SceneVariation,condition) %>%
    gather(Utterance,Mentioned,-RedundantProperty,-NumDistractors,-SceneVariation,-condition) %>%
    group_by(Utterance,RedundantProperty,NumDistractors,SceneVariation,condition) %>%
    summarise(Probability=mean(Mentioned),ci.low=ci.low(Mentioned),ci.high=ci.high(Mentioned)) %>%
    ungroup() %>%
    mutate(YMin = Probability - ci.low, YMax = Probability + ci.high, Distractors=as.factor(NumDistractors)) %>%
    left_join(predictives1 %>% mutate(predictions1 = size_color) %>% select(condition,predictions1), by = c("condition")) %>%
    left_join(predictives2 %>% mutate(predictions2 = size_color) %>% select(condition,predictions2), by = c("condition")) %>%
    left_join(predictives3 %>% mutate(predictions3 = size_color) %>% select(condition,predictions3), by = c("condition")) %>%
    left_join(predictives4 %>% mutate(predictions4 = size_color) %>% select(condition,predictions4), by = c("condition"))
  # return(agr)
  ggplot(agr, aes(x=SceneVariation,y=Probability,shape=Distractors,group=1)) +
    geom_point() +
    geom_errorbar(aes(ymin=YMin,ymax=YMax)) +
    xlab("Scene variation") +
    ylab("Probability of redundant modifier") +
    scale_shape_discrete(name = "Number of\ndistractors") +
    facet_wrap(~RedundantProperty) +
    geom_point(data = agr %>% mutate(Probability = predictions1), aes(color = "Vanilla\n(R^2 = 0.87)")) +
    geom_point(data = agr %>% mutate(Probability = predictions2), aes(color = "Continuous\n(R^2 = 0.98)")) +
    geom_point(data = agr %>% mutate(Probability = predictions3), aes(color = "Incremental\n(R^2 = 0.88)")) +
    geom_point(data = agr %>% mutate(Probability = predictions4), aes(color = "Cont.-Incr.\n(R^2 = 0.92)")) +
    theme_bw() +
    labs(colour = "Model") +
    scale_color_manual(values = cbPalette,
                       breaks = c("Continuous\n(R^2 = 0.98)",
                                  "Cont.-Incr.\n(R^2 = 0.92)",
                                  "Incremental\n(R^2 = 0.88)",
                                  "Vanilla\n(R^2 = 0.87)")) +
    ylim(0,0.85) +
    ggtitle("Results and model predictions (Spanish)") + 
    guides(color = guide_legend(order=1),
           shape = guide_legend(order=2))
}

visualize_sceneVariation_withModel(d_regressionFormat,
                                   vanillaPredictives,
                                   continuousPredictives,
                                   incrementalPredictives,
                                   incrementalContinuousPredictives)

ggsave("results/model_preds.png", width = 7, height = 3, units = "in")
