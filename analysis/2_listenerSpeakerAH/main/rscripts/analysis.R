library(lme4)
library(brms)
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
df.data <- read.csv("../../../../data/2_listenerSpeakerAH/main/2_listenerSpeakerAH-trials.csv", header=TRUE)

length(unique(df.data$workerid))

# exclude bot check and select only the relevant columns
df.data.clean <- df.data |>
  filter(text != "bot_check") |>
  select(workerid, ah_response, item, predicate, prior_condition, prior_rating, question_order, speaker_response, trigger, trigger_class)

# calculate the ratings for the alternative content and specify the belief
# of the certainty rating
df.data.all <- df.data.clean |>
  mutate(utterance_type = gsub("[a-z]+_", "\\1", trigger)) |> # either p (pos) or not p (neg)
  mutate(prior_rating_embedded = ifelse(utterance_type != "neg",
                                        prior_rating,
                                        1 - prior_rating),
         prior_condition_embedded = ifelse(utterance_type != "neg",
                                           prior_condition,
                                           ifelse(prior_condition == "low_prob",
                                                  # flip the condition
                                                  "high_prob", # low_prob for p is high_prob of not p
                                                  "low_prob"))) |>
  rename(prior_rating_p = prior_rating,
         prior_condition_p = prior_condition)
  # mutate(speaker_belief_p = ifelse(utterance_type != "neg",
  #                                  speaker_response,
  #                                  1 - speaker_response),
  #        ah_belief_p = ifelse(utterance_type != "neg",
  #                             ah_response,
  #                             1 - ah_response))


## Data exclusion
group_mc_mean <- mean(df.data.all$speaker_response[df.data.all$trigger == "MC"])
group_mc_sd <- sd(df.data.all$speaker_response[df.data.all$trigger == "MC"])
exclusion_criteria <- group_mc_mean + 2*group_mc_sd
excludeid_list <- df.data.all |>
  filter(trigger == "MC") |>
  group_by(workerid) |>
  summarise(mean_speaker_response = mean(speaker_response)) |>
  mutate(exclude = ifelse(mean_speaker_response >= exclusion_criteria, "yes", "no")) |>
  filter(exclude == "yes") |>
  select(workerid)
excludeid <- excludeid_list$workerid
# not native american english speakers
excludeid <- c(excludeid, 551, 580, 446, 253, 507, 500, 472, 457, 459)
df.data.summary <- df.data.all |>
  filter(!workerid %in% excludeid)

length(unique(df.data.summary$workerid))

# excluding the frank item in the inform_pos condition
count_frank_inform_pos <- df.data.summary |>
  filter(item == "frank" & trigger == "inform_pos") |>
  summarize(count=n())

df.data.summary <- df.data.summary |>
  filter(item != "frank" | trigger != "inform_pos")

# checking the number of predicate-embedded_proposition("pos"/"neg")-prior combination of each item
# count <- df.data.summary |>
#   # filter(item != "frank" | trigger != "inform_pos") |>
#   filter(trigger != "MC") |>
#   group_by(item, trigger, prior_condition) |>
#   summarize(count = n())
# mean(count$count)
# max(count$count)
# min(count$count)

df.data.summary |>
  filter(trigger_class == "Critical") |>
  group_by(prior_condition_embedded) |>
  summarize(mean_prior = mean(prior_rating_embedded))

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


content_predicate_labeller <- function(variable,value){
  if (variable=='utterance_type') {
    return(content_label[value])
  } else {
    return(predicate_label[value])
  }
}

## SPEAKER BELIEF RATINGS
# summary of speaker and ah belief ratings (of p)
speaker_ah_summary <- df.data.summary |>
  group_by(predicate, utterance_type) |> # collapse prior condition
  summarize(mean_speaker_rating = mean(speaker_response),
            speaker_ci_low = ci.low(speaker_response),
            speaker_ci_high = ci.high(speaker_response),
            
            mean_ah_rating = mean(ah_response),
            ah_ci_low = ci.low(ah_response),
            ah_ci_high = ci.high(ah_response)) |> 
  ungroup() |>
  mutate(speaker_YMin = mean_speaker_rating - speaker_ci_low,
         speaker_YMax = mean_speaker_rating + speaker_ci_high,
         ah_YMin = mean_ah_rating - ah_ci_low,
         ah_YMax = mean_ah_rating + ah_ci_high) |>
  mutate(utterance_type = ifelse(predicate %in% c("Polar", "MC"),
                                 "pos",
                                 as.character(utterance_type)),
         utterance_type = fct_relevel(utterance_type, "pos", "neg")) |>
  # pivot_longer(cols = c("mean_speaker_rating", "mean_ah_rating"),
  #              names_to = "rating_type",
  #              values_to = "rating_value") |>
  # mutate(YMax = ifelse(rating_type == "mean_speaker_rating",
  #                      speaker_YMax,
  #                      ah_YMax),
  #        YMin = ifelse(rating_type == "mean_speaker_rating",
  #                      speaker_YMin,
  #                      ah_YMin)) |>
  mutate(predicate = fct_relevel(predicate, "MC","Polar","think","know","say","inform"),
         utterance_type = fct_relevel(utterance_type, "pos", "neg"))
speaker_ah_summary
# write.csv(speaker_ah_summary, "../results/speaker_ah_summary.csv" , row.names = FALSE)  

# alternatively, belief in p (instead of belief in the embedded clause)
# speaker_ah_summary <- read.csv("../results/speaker_ah_summary.csv") |>
#   mutate(predicate = fct_relevel(predicate, "MC","Polar","think","know","say","inform"),
#          utterance_type = fct_relevel(utterance_type, "pos", "neg"))

# bar graph by embedded clause (facet by predicate)
speaker_all_alt <- ggplot(data = speaker_ah_summary,
                      mapping = aes(x = utterance_type,
                                    y = mean_speaker_rating,
                                    # alpha = prior_condition,
                                    fill = predicate)) +
  geom_bar(stat="identity",
           # position=dodge,
           color="black") +
  geom_errorbar(aes(ymin=speaker_YMin,
                    ymax=speaker_YMax),
                position=dodge,
                width=.2) +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  scale_fill_manual(values=cbPalette,
                    labels=c("Filler", "Polar", "think", "know", "say", "inform"),
                    name="Predicate",
                    guide="none") +
  # scale_alpha_discrete(range=c(.9,.4), # include both low and high prior (more opaque).
  #                      labels=c("neutral", "high prob", "low prob"),
  #                      name="Prior belief\nin p") +
  scale_y_continuous(name="Mean speaker belief in p",
                     limits=c(0,1)) +
  scale_x_discrete(label=c("p", "not p"),
                   name="Type of embedded clause")
speaker_all_alt
ggsave(speaker_all_alt, file="../graphs/speaker_embedded_collapse_prior.pdf", width=6, height=3)

# line graph by prior (facet by embedded clause)
speaker_prior_embedded <- ggplot(data = df.data.summary |>
                          # to reorder the predicate and utterance_type for graph
                          mutate(predicate = fct_relevel(predicate, "MC","Polar","think","know","say","inform"),
                                 utterance_type = fct_relevel(utterance_type, "MC","Polar","pos", "neg"),
                                 prior_condition = fct_relevel(prior_condition_embedded, "neutral", "high_prob", "low_prob")) |>
                          filter(trigger != "MC"),
                          mapping = aes(x = prior_rating_embedded,
                                        y = speaker_response,
                                        color = predicate)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", fullrange=T) +  # extend to full range 0-1 
  facet_grid(. ~ utterance_type,
             labeller = utterance_labeller) +
  scale_x_continuous(name="Rating of prior belief in the embedded content", limits=c(0,1),
                     guide=guide_axis(angle = 60)) + 
  scale_y_continuous(name="Mean speaker belief\nin the embedded content", limits=c(0,1)) + 
  scale_color_manual(values=cbPalette[2:7],
                    labels=c("Polar", "think", "know", "say", "inform"),
                    name="Predicate")
speaker_prior_embedded
ggsave(speaker_prior_embedded, file="../graphs/speaker_prior_embedded.pdf", width=6, height=3)

## AH BELEIF RATING
# bar graph by embedded clause (facet by predicate)
ah_all_alt <- ggplot(data = speaker_ah_summary |>
                       filter(!predicate %in% c("Polar", "MC")),
                      mapping = aes(x = utterance_type,
                                    y = mean_ah_rating,
                                    # alpha = prior_condition,
                                    fill = predicate)) +
  geom_bar(stat="identity",
           # position=dodge,
           color="black") +
  geom_errorbar(aes(ymin=ah_YMin,
                    ymax=ah_YMax),
                position=dodge,
                width=.2) +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  scale_fill_manual(values=cbPalette[3:7],
                    labels=c("think", "know", "say", "inform"),
                    guide="none",
                    name="Predicate") +
  # scale_alpha_discrete(range=c(.9,.4), # include both low and high prior (more opaque).
  #                      labels=c("high prob", "low prob"),
  #                      name="Prior belief\nin p") +
  scale_x_discrete(label=c("p", "not p"),
                   name="Type of embedded clause") +
  scale_y_continuous(limits=c(0,1),
                     name="Mean attitude holder belief\nin the embedded content")
ah_all_alt
ggsave(ah_all_alt, file="../graphs/ah_embedded_collapse_prior.pdf", width=6, height=3)

# line plot by prior (facet by embedded clause)
ah_prior <- ggplot(data = df.data.summary |>
                     mutate(predicate = fct_relevel(predicate, "MC","Polar","think","know","say","inform"),
                            utterance_type = fct_relevel(utterance_type, "MC","Polar","pos", "neg"),
                            prior_condition = fct_relevel(prior_condition_embedded, "neutral", "high_prob", "low_prob")) |>
                     filter(trigger != "MC" & trigger != "Polar"),
                   mapping = aes(x = prior_rating_embedded,
                                 y = ah_response,
                                 color = predicate)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", fullrange=T) + 
  facet_grid(. ~ utterance_type,
             labeller = utterance_labeller) +
  scale_x_continuous(name="Rating of prior belief in the embedded content", limits=c(0, 1),
                     guide=guide_axis(angle = 60)) + 
  scale_y_continuous(name="Mean attitude holder belief\nin the embedded content", limits=c(0,1)) + 
  scale_color_manual(values=cbPalette[3:7],
                    labels=c("think", "know", "say", "inform"),
                    name="Predicate")
ah_prior
ggsave(ah_prior, file="../graphs/ah_prior_embedded.pdf", width=6, height=3)


###### Analysis ######
emm_options(pbkrtest.limit = 3450)
emm_options(lmerTest.limit = 3450)
analysis_data <- df.data.summary |>
  filter(predicate %in% c("know", "think", "Polar")) |>
  mutate(utterance_type = as.character(utterance_type),
         # embedded_content is the same as the utterance_type, but coded as numbers
         predicate = relevel(as.factor(predicate), ref = "Polar"),
          embedded_content = ifelse(predicate == "Polar",
                                  "pos",
                                  utterance_type),
         embedded_content = relevel(as.factor(embedded_content), ref="pos"),
         embedded_content = as.numeric(embedded_content),
         question_order = relevel(as.factor(question_order), ref = "speaker_first"),
        # using the prior rating of the embedded content
         centered_prior_rating = prior_rating_embedded - mean(prior_rating_embedded), 
         centered_embedded_content = embedded_content - mean(embedded_content))

table(analysis_data$predicate, analysis_data$centered_embedded_content, analysis_data$item)

speaker_model <- lmer(speaker_response ~ predicate + centered_prior_rating + centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content | workerid) + (predicate + centered_prior_rating + centered_embedded_content | item),
                      analysis_data)
# joint_tests(speaker_model)
summary(speaker_model)

speaker_interaction_model <- lmer(speaker_response ~  predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content || workerid) + (predicate + centered_prior_rating + centered_embedded_content || item),
                      analysis_data)
summary(speaker_interaction_model)

# check the effect of question order
# speaker_order_model <- lmer(speaker_response ~  predicate * centered_prior_rating * centered_embedded_content + question_order + (predicate + centered_prior_rating + centered_embedded_content || workerid) + (predicate + centered_prior_rating + centered_embedded_content || item),
#                                   analysis_data)
# summary(speaker_order_model)


# failed to converge, so exclude the intercept
speaker_p_model <-lmer(speaker_response ~ predicate * centered_prior_rating + (0 + predicate + centered_prior_rating || workerid) + (0 + predicate + centered_prior_rating || item),
                             analysis_data |>
                               filter(utterance_type != "neg"))
summary(speaker_p_model)

# check the effect of question order
# speaker_p_order_model <-lmer(speaker_response ~  predicate * centered_prior_rating + question_order + (predicate + centered_prior_rating + question_order| workerid) + (predicate + centered_prior_rating + question_order| item),
#                        analysis_data |>
#                          filter(utterance_type != "neg"))
# summary(speaker_p_order_model)


speaker_know_think_model <- lmer(speaker_response ~  predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content || workerid) + (predicate + centered_prior_rating + centered_embedded_content || item),
                                 analysis_data |>
                                   filter(predicate %in% c("think", "know")))
summary(speaker_know_think_model)

# check the effect of question order
# speaker_order_model <- lmer(speaker_response ~ predicate * centered_prior_rating * centered_embedded_content * question_order + (predicate + centered_prior_rating + centered_embedded_content + question_order | workerid) + (predicate + centered_prior_rating + centered_embedded_content + question_order | item),
#                                  analysis_data |>
#                                    filter(predicate %in% c("think", "know")))
# summary(speaker_order_model)


ah_model <- lmer(ah_belief_p ~  predicate * centered_prior_rating * centered_prior_rating + (predicate + centered_prior_rating + centered_prior_rating | workerid) + (predicate + centered_prior_rating + centered_prior_rating| item),
                 analysis_data)
joint_tests(ah_model)
summary(ah_model)


## Bayesian
speaker_interaction_bayesian <- brm(speaker_response ~  predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content || workerid) + (predicate + centered_prior_rating + centered_embedded_content || item),
                                  analysis_data)
                                  # control = list(max_treedepth = 15, adapt_delta = 0.99))
summary(speaker_interaction_bayesian)


