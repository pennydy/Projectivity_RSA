library(lme4)
library(dplyr)
library(emmeans)
library(tidyverse)


theme_set(theme_bw())
# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 


###### Data ######
# get the data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../../../helpers.R")
df.data <- read.csv("../../../../data/2_listenerSpeakerAH/pilot/2_listenerSpeakerAH_pilot-trials.csv", header=TRUE)

length(unique(df.data$workerid))

# exclude bot check and select only the relevant columns
df.data.clean <- df.data |>
  filter(text != "bot_check") |>
  select(workerid, ah_response, item, predicate, prior_condition, prior_rating, question_order, speaker_response, trigger, trigger_class)

# check the number of each predicate + content (p/not p) + prior (high/low) combination
df.data.clean |>
  filter(trigger != "MC") |>
  group_by(item, trigger, prior_condition) |>
  summarize(count = n()) |>
  print(n=118)

# calculate the ratings for the alternative content and specify the belief
# of the certainty rating
df.data.all <- df.data.clean |>
  mutate(utterance_type = gsub("[a-z]+_", "\\1", trigger)) |> # either p (pos) or not p (neg)
  # the mutate for predicate, prior_condition are no longer needed
  mutate(predicate = ifelse(trigger %in% c("MC", "Polar"),
                             trigger,
                             as.character(predicate)),
         prior_condition = ifelse(is.na(prior_condition),
                                  "neutral",
                                  as.character(prior_condition)),
         prior_condition = ifelse(utterance_type != "neg",
                                  prior_condition,
                                  ifelse(prior_condition == "high_prob",
                                         "low_prob",
                                         "high_prob")), 
         speaker_belief_p = ifelse(utterance_type != "neg",
                                   speaker_response,
                                   1 - speaker_response),
         ah_belief_p = ifelse(utterance_type != "neg",
                              ah_response,
                              1 - ah_response)) |>
  # to reorder the predicate and utterance_type for graph
  mutate(predicate = fct_relevel(predicate, "MC","Polar","think","know","say","inform"),
         utterance_type = fct_relevel(utterance_type, "MC","Polar","pos", "neg"),
         prior_condition = fct_relevel(prior_condition, "neutral", "high_prob", "low_prob"))

## Data exclusion
group_mc_mean <- mean(df.data.all$belief_response[df.data.all$trigger == "MC"])
group_mc_sd <- sd(df.data.all$belief_response[df.data.all$trigger == "MC"])
exclusion_criteria <- group_mc_mean + 2*group_mc_sd
excludeid <- df.data.all |>
  filter(trigger == "MC") |>
  group_by(workerid) |>
  summarise(mean_belief_response = mean(belief_response)) |>
  mutate(exclude = ifelse(mean_belief_response >= exclusion_criteria, "yes", "no")) |>
  filter(exclude == "yes") |>
  select(workerid)
# workerid 59 did not respond to the native language question
excludeid <- excludeid$workerid
excludeid <- c(59)
df.data.summary <- df.data.all |>
  filter(!workerid %in% excludeid)

length(unique(df.data.summary$workerid))

table(df.data.summary$content, df.data.summary$predicate)


###### Graph ######
# additional helper functions/lists for graphs
dodge = position_dodge(.9)

utterance_label <- list("MC"="Filler",
                        "polar"="Polar",
                        "pos"="Embedded: p",
                        "neg"="Embedded: not p")
utterance_labeller <- function(variable,value){
  return(utterance_label[value])
}

predicate_label <- list("MC"="Filler",
                        "Polar"="Polar",
                        "think"="think",
                        "know"="know",
                        "say"="say",
                        "inform"="inform")
predicate_labeller <- function(variable,value){
  return(predicate_label[value])
}

content_label <- list("pos"="Belief content: p",
                      "neg"="Belief content: not p")
content_labeller <- function(variable,value){
  return(content_label[value])
}

rating_label <- list("mean_ah_belief"="ah",
                     "mean_speaker_belief"="speaker")
rating_labeller <- function(variable,value){
  return(rating_label[value])
}

content_predicate_labeller <- function(variable,value){
  if (variable=='utterance_type') {
    return(content_label[value])
  } else {
    return(predicate_label[value])
  }
}

rating_predicate_labeller <- function(variable,value){
  if (variable=='rating_type') {
    return(rating_label[value])
  } else {
    return(predicate_label[value])
  }
}


## SPEAKER BELIEF RATINGS
# # summary of speaker belief ratings (of the embedded content), by predicate and prior
# speaker_prior_predicate_summary <- df.data.summary |>
#   group_by(predicate, prior_condition) |>
#   summarize(mean_speaker_rating = mean(speaker_response),
#             ci_low = ci.low(speaker_response), # confidence interval
#             ci_high = ci.high(speaker_response)) |> 
#   ungroup() |>
#   mutate(YMin = mean_speaker_rating - ci_low,
#          YMax = mean_speaker_rating + ci_high)
# speaker_prior_predicate_summary
# write.csv(speaker_prior_predicate_summary, "../results/speaker_summary.csv" , row.names = FALSE)
# 
# # by prior, collapse over type of embedded content
# speaker_by_prior <- ggplot(data = speaker_prior_predicate_summary,
#                            mapping = aes(x = predicate,
#                                          y = mean_speaker_rating,
#                                          alpha = factor(prior_condition),
#                                          fill = predicate)) +
#   geom_bar(stat="identity",
#            position=dodge,
#            color="black") +
#   geom_errorbar(aes(ymin=YMin,
#                     ymax=YMax),
#                 position=dodge,
#                 width=.2) +
#   geom_hline(yintercept = 0.5,
#              alpha = 0.7,
#              color = "grey",
#              linetype = "dashed") +
#   labs(x = "Predicate",
#        y = "Mean belief in the embedded content",
#        fill = "Predicate") +
#   scale_fill_manual(values=cbPalette,
#                     labels=c("Filler", "Polar", "think", "know", "say", "inform")) +
#   scale_alpha_discrete(range=c(.9, .4), # include both low and high prior (more opaque). 
#                        labels=c("neutral", "high prob", "low prob"),
#                        name="Prior") +
#   theme(axis.text.x=element_blank(),
#         axis.title.x=element_blank()) 
# speaker_by_prior
# ggsave(speaker_by_prior, file="../graphs/speaker_by_prior.pdf", width=6, height=3)
# 
# 
# # by predicate, collapse over embedded content
# speaker_by_predicate <- ggplot(data = speaker_prior_predicate_summary,
#                                mapping = aes(x = prior_condition,
#                                              y = mean_speaker_rating,
#                                              alpha = as.factor(prior_condition),
#                                              fill = predicate)) +
#   geom_bar(stat="identity",
#            position=dodge,
#            color="black") +
#   geom_errorbar(aes(ymin=YMin,
#                     ymax=YMax),
#                 position=dodge,
#                 width=.2,
#                 color="black") +
#   geom_hline(yintercept = 0.5,
#              alpha = 0.7,
#              color = "grey",
#              linetype = "dashed") + 
#   facet_grid(. ~ predicate,
#              labeller = predicate_labeller) +
#   labs(y = "Mean belief in the embedded content") +
#   scale_alpha_discrete(range=c(.9,.4), # include both p and not p (more opaque).
#                        labels=c("netural","high prob", "low prob"),
#                        name="Prior belief") +
#   scale_fill_manual(values=cbPalette,
#                     labels=c("Filler", "Polar", "think", "know", "say", "inform"),
#                     guide="none") +
#   theme(axis.text.x=element_blank(),
#         axis.title.x=element_blank())
# speaker_by_predicate
# ggsave(speaker_by_predicate, file="../graphs/speaker_by_predicate.pdf", width=6, height=3)


# summary of speaker and ah belief ratings (of p)
speaker_ah_summary <- df.data.summary |>
  group_by(predicate, utterance_type, prior_condition) |>
  summarize(mean_speaker_rating = mean(speaker_belief_p),
            speaker_ci_low = ci.low(speaker_belief_p), # confidence interval
            speaker_ci_high = ci.high(speaker_belief_p),
            
            mean_ah_rating = mean(ah_belief_p),
            ah_ci_low = ci.low(ah_belief_p),
            ah_ci_high = ci.high(ah_belief_p)) |> 
  ungroup() |>
  mutate(speaker_YMin = mean_speaker_rating - speaker_ci_low,
         speaker_YMax = mean_speaker_rating + speaker_ci_high,
         ah_YMin = mean_ah_rating - ah_ci_low,
         ah_YMax = mean_ah_rating + ah_ci_high) |>
  mutate(utterance_type = ifelse(predicate %in% c("Polar", "MC"),
                                 "pos",
                                 as.character(utterance_type)),
         utterance_type = fct_relevel(utterance_type, "pos", "neg")) |>
  pivot_longer(cols = c("mean_speaker_rating", "mean_ah_rating"),
               names_to = "rating_type",
               values_to = "rating_value") |>
  mutate(YMax = ifelse(rating_type == "mean_speaker_rating",
                       speaker_YMax,
                       ah_YMax),
         YMin = ifelse(rating_type == "mean_speaker_rating",
                       speaker_YMin,
                       ah_YMin))
speaker_ah_summary
write.csv(speaker_ah_summary, "../results/speaker_ah_summary.csv" , row.names = FALSE)  

# by the type of embedded clause (pos or neg)
speaker_all_alt <- ggplot(data = speaker_ah_summary |>
                            filter(rating_type == "mean_speaker_rating"),
                      mapping = aes(x = utterance_type,
                                    # x = rating_type,
                                    y = rating_value,
                                    alpha = prior_condition,
                                    fill = predicate)) +
  geom_bar(stat="identity",
           position=dodge,
           color="black") +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=dodge,
                width=.2) +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  # facet_grid(utterance_type ~ predicate,
  #            labeller = content_predicate_labeller) +
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  labs(x = "Type of embedded clause",
       y = "Mean speaker belief in p",
       fill = "Predicate") +
  scale_fill_manual(values=cbPalette,
                    labels=c("Filler", "Polar", "think", "know", "say", "inform"),
                    guide="none") +
  scale_alpha_discrete(range=c(.9,.4), # include both low and high prior (more opaque).
                       labels=c("neutral", "high prob", "low prob"),
                       name="Prior belief\nin p") +
  scale_x_discrete(label = c("p", "not p"))
  # theme(axis.title.x = element_blank(),
  #       axis.text.x = element_blank())
speaker_all_alt
ggsave(speaker_all_alt, file="../graphs/speaker_all_alt.pdf", width=6, height=3)


## AH BELEIF RATING
# by the type of embedded clause (pos or neg)
ah_all <- ggplot(data = speaker_ah_summary |>
                    filter(rating_type == "mean_ah_rating") |>
                        filter(!predicate %in% c("Polar", "MC")),
                      mapping = aes(x = rating_type,
                                    # x = utterance_type,
                                    y = rating_value,
                                    alpha = prior_condition,
                                    fill = predicate)) +
  geom_bar(stat="identity",
           position=dodge,
           color="black") +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=dodge,
                width=.2) +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  facet_grid(utterance_type ~ predicate,
             labeller = content_predicate_labeller) +
  # facet_grid(. ~ predicate,
  #            labeller = predicate_labeller) +
  labs(x = "Type of embedded clause",
       y = "Mean ah belief in p",
       fill = "Predicate") +
  scale_fill_manual(values=cbPalette[3:7],
                    labels=c("think", "know", "say", "inform"),
                    guide="none") +
  scale_alpha_discrete(range=c(.9,.4), # include both low and high prior (more opaque).
                       labels=c("high prob", "low prob"),
                       name="Prior belief\nin p") +
  # scale_x_discrete(label = c("p", "not p"))
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
ah_all
ggsave(ah_all, file="../graphs/ah_all.pdf", width=6, height=3)


## SPEAKER AND AH
speaker_ah_all_alt <- ggplot(data = speaker_ah_summary |>
                               mutate(rating_type = fct_relevel(rating_type, "mean_ah_rating", "mean_speaker_rating")),
                         mapping = aes(x = utterance_type,
                                       # x = rating_type,
                                       y = rating_value,
                                       alpha = prior_condition,
                                       fill = predicate)) +
  geom_bar(stat="identity",
           position=dodge,
           color="black") +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=dodge,
                width=.2) +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  # facet_grid(utterance_type ~ predicate,
  #            labeller = content_predicate_labeller) +
  facet_grid(rating_type ~ predicate,
             labeller = rating_predicate_labeller) +
  labs(# x = "Type of rating",
      x = "Type of embedded clause",
       y = "Mean belief in p",
       fill = "Predicate") +
  scale_fill_manual(values=cbPalette,
                    labels=c("Filler", "Polar", "think", "know", "say", "inform"),
                    guide="none") +
  scale_alpha_discrete(range=c(.9,.4), # include both low and high prior (more opaque).
                       labels=c("neutral", "high prob", "low prob"),
                       name="Prior belief\nin p") +
  # scale_x_discrete(label = c("ah", "speaker"))
  scale_x_discrete(label = c("p", "not p"))
speaker_ah_all_alt
ggsave(speaker_ah_all_alt, file="../graphs/speaker_ah_all_alt.pdf")


###### Analysis ######
analysis_data <- df.data.summary |>
  filter(predicate %in% c("know", "think", "Polar")) |>
  mutate(predicate = relevel(predicate, ref = "Polar"),
          utterance_type = relevel(utterance_type, ref = "pos"),
          question_order = relevel(as.factor(question_order), ref = "speaker_first"),
         centered_prior_rating = scale(prior_rating)) # double check center prior_rating (scale=F)? 


speaker_model <- lmer(speaker_belief_p ~  predicate + centered_prior_rating + (predicate + centered_prior_rating | workerid) + (predicate + centered_prior_rating | item),
                      analysis_data)
joint_tests(speaker_model)


ah_model <- lmer(ah_belief_p ~  predicate + centered_prior_rating + (predicate + centered_prior_rating | workerid) + (predicate + centered_prior_rating | item),
                 analysis_data)
joint_tests(ah_model)
