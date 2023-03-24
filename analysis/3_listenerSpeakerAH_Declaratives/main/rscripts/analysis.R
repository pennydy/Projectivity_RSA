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
df.data <- read.csv("../../../../data/3_listenerSpeakerAH_Declaratives/main/3_listenerSpeakerAH_Declaratives-trials.csv", header=TRUE)

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
  ungroup() |>
  summarize(mean_count=mean(count),
            max_count=max(count),
            min_count=min(count))

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

## Data exclusion

# exclusion based on language status/background and other comments
excludeid <- c(934, 1039, 641, 786, 978, 642, 640, 770) # non-native speakers
excludeid <- c(excludeid, 706) # neurodivergent 
excludeid <- c(excludeid, 927, 695, 714) # bilinguals

# exclusion based on performance in the filler items
df.data.all <- df.data.all |>
  filter(!workerid %in% excludeid)
group_mc_mean <- mean(df.data.all$speaker_response[df.data.all$trigger == "MC"])
group_mc_sd <- sd(df.data.all$speaker_response[df.data.all$trigger == "MC"])
exclusion_criteria <- group_mc_mean + 2*group_mc_sd
excludeid_filler <- df.data.all |>
  filter(trigger == "MC") |>
  group_by(workerid) |>
  summarise(mean_belief_response = mean(speaker_response)) |>
  mutate(exclude = ifelse(mean_belief_response >= exclusion_criteria, "yes", "no")) |>
  filter(exclude == "yes") |>
  select(workerid)
excludeid <- c(excludeid, excludeid_filler$workerid)

df.data.summary <- df.data.all |>
  filter(!workerid %in% excludeid)

length(unique(df.data.summary$workerid))

combined_prior <- df.data.summary |>
  filter(trigger_class == "Critical") |>
  summarize(mean_prior = mean(prior_rating_embedded))
combined_prior <- combined_prior$mean_prior


###### Graph ######
# additional helper functions/lists for graphs
dodge = position_dodge(.9)

utterance_label <- list("MC"="Filler",
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
  mutate(utterance_type = ifelse(predicate == "MC",
                                 "pos",
                                 as.character(utterance_type)),
         utterance_type = fct_relevel(utterance_type, "pos", "neg")) |>
  mutate(predicate = fct_relevel(predicate, "MC","Polar","think","know","say","inform"),
         utterance_type = fct_relevel(utterance_type, "pos", "neg"))
speaker_ah_summary
# write.csv(speaker_ah_summary, "../results/speaker_ah_summary.csv" , row.names = FALSE)

speaker_ah_prior <- df.data.summary |>
  group_by(predicate, utterance_type, prior_condition_embedded) |> # separate by prior condition
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
  mutate(utterance_type = ifelse(predicate == "MC",
                                 "pos",
                                 as.character(utterance_type)),
         utterance_type = fct_relevel(utterance_type, "pos", "neg")) |>
  mutate(predicate = fct_relevel(predicate, "MC","Polar","think","know","say","inform"),
         utterance_type = fct_relevel(utterance_type, "pos", "neg"))
# write.csv(speaker_ah_prior, "../results/speaker_ah_prior.csv" , row.names = FALSE)

# speaker belief
# bar graph by predicate and embedded clause
speaker_all <- ggplot(data = speaker_ah_summary,
                          mapping = aes(x = utterance_type,
                                        y = mean_speaker_rating,
                                        fill = predicate)) +
  geom_bar(stat="identity",
           position=dodge,
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
  scale_y_continuous(name="Mean speaker belief\nin the embedded content",
                     limits=c(0,1)) +
  scale_x_discrete(label=c("p", "not p"),
                   name="Type of embedded clause")
speaker_all
ggsave(speaker_all, file="../graphs/speaker_embedded_collapse_prior.pdf", width=6, height=3)

# straight-forward plot for when the fact is not consistent (low prob.) with the embedded clause
# bar graph by prior (facet by embedded clause) 
speaker_all_prior <- ggplot(data = speaker_ah_prior |>
                        filter(predicate != "MC"),
                      mapping = aes(x = utterance_type,
                                    y = mean_speaker_rating,
                                    alpha = prior_condition_embedded,
                                    fill = predicate)) +
  geom_bar(stat="identity",
           position=dodge,
           color="black") +
  geom_errorbar(aes(ymin=speaker_YMin,
                    ymax=speaker_YMax),
                position=dodge,
                width=.2) +
  scale_alpha_discrete(range=c(.9,.4), # include both low and high prior (more opaque).
                       labels=c("high prob", "low prob"),
                       name="Prior belief\nin embedded content") +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  scale_fill_manual(values=cbPalette[2:7],
                    labels=c("Polar", "think", "know", "say", "inform"),
                    name="Predicate",
                    guide="none") +
  scale_y_continuous(name="Mean speaker belief\nin the embedded content",
                     limits=c(0,1)) +
  scale_x_discrete(label=c("p", "not p"),
                   name="Type of embedded clause")
speaker_all_prior
ggsave(speaker_all_prior, file="../graphs/speaker_embedded_by_prior.pdf", width=6, height=3)

# line graph by prior (facet by embedded clause)
d_byitem_sp = df.data.summary %>% 
  group_by(item,predicate,utterance_type,prior_condition_embedded, prior_rating_embedded) %>% 
  summarize(speaker_response = mean(speaker_response)) %>% 
  ungroup() %>% 
  mutate(predicate = fct_relevel(as.factor(predicate), "MC","Polar","think","know","say","inform"),
         utterance_type = fct_relevel(as.factor(utterance_type), "MC","pos", "neg"),
         prior_condition_embedded = fct_relevel(as.factor(prior_condition_embedded), "neutral", "high_prob", "low_prob")) %>% 
  filter(predicate != "MC")

# no na in speaker_response
which(is.na(df.data.summary$speaker_response))
  

speaker_prior_embedded <- ggplot(data = df.data.summary |>
                                   # to reorder the predicate and utterance_type for graph
                                   mutate(predicate = fct_relevel(predicate, "MC","Polar","think","know","say","inform"),
                                          utterance_type = fct_relevel(utterance_type, "MC","pos","neg"),
                                          prior_condition = fct_relevel(prior_condition_embedded, "neutral", "high_prob", "low_prob")) |>
                                   filter(trigger != "MC"),
                                 mapping = aes(x = prior_rating_embedded,
                                               y = speaker_response,
                                               color = predicate)) +
  # geom_point(alpha = 0.1) +
  geom_point(data=d_byitem_sp, alpha=.1) +
  geom_smooth(method = "lm", fullrange=T) +  # extend to full range 0-1 
  facet_grid(. ~ utterance_type,
             labeller = utterance_labeller) +
  scale_x_continuous(name="Rating of prior belief in the embedded content",
                     limits=c(0,1),
                     breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  scale_y_continuous(name="Mean speaker belief\nin the embedded content", limits=c(0,1)) + 
  coord_cartesian(xlim=c(0,1), ylim=c(0,1)) +
  scale_color_manual(values=cbPalette[2:7],
                     labels=c("Polar", "think", "know", "say","inform"),
                     name="Predicate")
speaker_prior_embedded
ggsave(speaker_prior_embedded, file="../graphs/speaker_prior_embedded.pdf", width=7, height=3)


# ah belief
# bar graph by predicate and embedded clause
ah_all <- ggplot(data = speaker_ah_summary |>
                        filter(!predicate %in% c("Polar", "MC")),
                      mapping = aes(x = utterance_type,
                                    y = mean_ah_rating,
                                    fill = predicate)) +
  geom_bar(stat="identity",
           position=dodge,
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
                    name="Predicate",
                    guide="none") +
  scale_y_continuous(name="Mean ah belief in the embedded content",
                     limits=c(0,1)) +
  scale_x_discrete(label=c("p", "not p"),
                   name="Type of embedded clause")
ah_all
ggsave(ah_all, file="../graphs/ah_embedded_collapse_prior.pdf", width=6, height=3)

# bar graph by prior (facet by embedded clause) 
ah_all_prior <- ggplot(data = speaker_ah_prior |>
                         filter(!predicate %in% c("Polar", "MC")),
                            mapping = aes(x = utterance_type,
                                          y = mean_ah_rating,
                                          alpha = prior_condition_embedded,
                                          fill = predicate)) +
  geom_bar(stat="identity",
           position=dodge,
           color="black") +
  geom_errorbar(aes(ymin=ah_YMin,
                    ymax=ah_YMax),
                position=dodge,
                width=.2) +
  scale_alpha_discrete(range=c(.9,.4), # include both low and high prior (more opaque).
                       labels=c("high prob", "low prob"),
                       name="Prior belief\nin embedded content") +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  scale_fill_manual(values=cbPalette[3:7],
                    labels=c("think", "know", "say", "inform"),
                    name="Predicate",
                    guide="none") +
  scale_y_continuous(name="Mean ah belief in the embedded content",
                     limits=c(0,1)) +
  scale_x_discrete(label=c("p", "not p"),
                   name="Type of embedded clause")
ah_all_prior
ggsave(ah_all_prior, file="../graphs/ah_embedded_by_prior.pdf", width=6, height=3)

df.data.summary |>
  filter(!trigger %in% c("Filler", "MC")) |>
  summarise(across(everything(), ~ sum(is.na(.))))


# line graph by prior (facet by embedded clause)
d_byitem_ah = df.data.summary %>% 
  group_by(item,predicate,utterance_type,prior_condition_embedded, prior_rating_embedded) %>% 
  summarize(ah_response = mean(ah_response)) %>% 
  ungroup() %>% 
  mutate(predicate = fct_relevel(as.factor(predicate), "MC","Polar","think","know","say","inform"),
         utterance_type = fct_relevel(as.factor(utterance_type), "MC","pos", "neg"),
         prior_condition = fct_relevel(as.factor(prior_condition_embedded), "neutral", "high_prob", "low_prob")) %>% 
  filter(!predicate %in% c("MC","Polar"))

ah_prior_embedded <- ggplot(data = df.data.summary |>
                                   # to reorder the predicate and utterance_type for graph
                                   mutate(predicate = fct_relevel(predicate, "MC","Polar","think","know","say","inform"),
                                          utterance_type = fct_relevel(utterance_type, "MC","pos","neg"),
                                          prior_condition = fct_relevel(prior_condition_embedded, "neutral", "high_prob", "low_prob")) |>
                              filter(!predicate %in% c("MC","Polar")),
                                 mapping = aes(x = prior_rating_embedded,
                                               y = ah_response,
                                               color = predicate)) +
  # geom_point(alpha = 0.1) +
  geom_point(data=d_byitem_ah, alpha=.1) +
  geom_smooth(method = "lm", fullrange=T) +  # extend to full range 0-1 
  facet_grid(. ~ utterance_type,
             labeller = utterance_labeller) +
  scale_x_continuous(name="Rating of prior belief in the embedded content", 
                     limits=c(0,1),
                     breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  scale_y_continuous(name="Mean ah belief\nin the embedded content", 
                     limits=c(0,1)) + 
  coord_cartesian(xlim=c(0,1), ylim=c(0,1)) +
  scale_color_manual(values=cbPalette[3:7],
                     labels=c("think", "know", "say","inform"),
                     name="Predicate")
ah_prior_embedded
ggsave(ah_prior_embedded, file="../graphs/ah_prior_embedded.pdf", width=7, height=3)

###### Analysis ######
emm_options(pbkrtest.limit = 3450)
emm_options(lmerTest.limit = 3450)
analysis_data <- df.data.summary |>
  filter(predicate %in% c("know", "think", "Polar")) |>
  mutate(utterance_type = as.character(utterance_type),
         # embedded_content is the same as the utterance_type, but coded as numbers
         predicate = relevel(as.factor(predicate), ref = "Polar"),
         embedded_content = relevel(as.factor(utterance_type), ref="pos"),
         embedded_content = as.numeric(embedded_content),
         question_order = relevel(as.factor(question_order), ref = "speaker_first"),
         # using the prior rating of the embedded content
         centered_prior_rating = prior_rating_embedded - mean(prior_rating_embedded), 
         centered_embedded_content = embedded_content - mean(embedded_content))

full_analysis_data <- df.data.summary |>
  filter(predicate %in% c("know", "think", "Polar", "say", "inform")) |>
  mutate(utterance_type = as.character(utterance_type),
         # embedded_content is the same as the utterance_type, but coded as numbers
         predicate = relevel(as.factor(predicate), ref = "Polar"),
         embedded_content = relevel(as.factor(utterance_type), ref="pos"),
         embedded_content = as.numeric(embedded_content),
         question_order = relevel(as.factor(question_order), ref = "speaker_first"),
         # using the prior rating of the embedded content
         centered_prior_rating = prior_rating_embedded - mean(prior_rating_embedded), 
         centered_embedded_content = embedded_content - mean(embedded_content))

anyNA(full_analysis_data$speaker_response)

full_analysis_data |>
  filter(predicate != "Polar") |>
  anyNA()

table(full_analysis_data$predicate, full_analysis_data$item,full_analysis_data$centered_embedded_content)

## lmer
# speaker
speaker_model <- lmer(speaker_response ~ predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content | workerid) + (predicate + centered_prior_rating + centered_embedded_content | item), 
                      analysis_data)
summary(speaker_model)

speaker_model_decorrelated <- lmer(speaker_response ~ predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content || workerid) + (predicate + centered_prior_rating + centered_embedded_content || item), 
                      analysis_data)
summary(speaker_model_decorrelated)

# no significant main effect of question order
speaker_model_order <- lmer(speaker_response ~ predicate * centered_prior_rating * centered_embedded_content * question_order + (predicate + centered_prior_rating + centered_embedded_content + question_order || workerid) + (predicate + centered_prior_rating + centered_embedded_content + question_order|| item), 
                      analysis_data)
summary(speaker_model_order)

full_speaker_simple <- lmer(speaker_response ~ predicate * centered_prior_rating * centered_embedded_content + (1 | workerid) + (1 | item), 
                                    full_analysis_data)
summary(full_speaker_simple)

full_speaker_model <- lmer(speaker_response ~ predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating | workerid) + (1 | item), 
                      full_analysis_data)
summary(full_speaker_model)
print(full_speaker_model, correlation=TRUE)

full_speaker_model_decorrelated <- lmer(speaker_response ~ predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content || workerid) + (predicate + centered_prior_rating + centered_embedded_content || item), 
                           full_analysis_data)
summary(full_speaker_model_decorrelated)

# ah
ah_model <- lmer(ah_response ~ predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content | workerid) + (predicate + centered_prior_rating + centered_embedded_content | item), 
                 analysis_data |>
                   filter(predicate != "Polar"))
save.lmer.effects(ah_model, fileName = "../cache/ah_model")
summary(ah_model)

ah_model_decorrelated <- lmer(ah_response ~ predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content || workerid) + (predicate + centered_prior_rating + centered_embedded_content || item), 
                 analysis_data |>
                   filter(predicate != "Polar"))
summary(ah_model_decorrelated)

# no significant effect of quesiton order
ah_model_order <- lmer(ah_response ~ predicate * centered_prior_rating * centered_embedded_content * question_order + (predicate + centered_prior_rating + centered_embedded_content + question_order || workerid) + (predicate + centered_prior_rating + centered_embedded_content + question_order || item), 
                              analysis_data |>
                                filter(predicate != "Polar"))
summary(ah_model_order)

full_ah_model <- lmer(ah_response ~ predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content | workerid) + (predicate + centered_prior_rating + centered_embedded_content | item), 
                           full_analysis_data |>
                        filter(predicate != "Polar"))
summary(full_ah_model)

full_ah_model_decorrelated <- lmer(ah_response ~ predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content || workerid) + (predicate + centered_prior_rating + centered_embedded_content || item), 
                      full_analysis_data |>
                        filter(predicate != "Polar"))
summary(full_ah_model_decorrelated)

## Bayesian, brm
# speaker
# speaker_bayesian_simple <- brm(speaker_response ~ predicate * centered_prior_rating * centered_embedded_content + (1 | workerid) + (1 | item),  
#                         analysis_data,
#                         control=list(max_treedepth = 15, adapt_delta = 0.99),
#                         file="../cache/brm_speaker_simple")
# summary(speaker_bayesian_simple)

speaker_bayesian <- brm(speaker_response ~ predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content | workerid) + (predicate + centered_prior_rating + centered_embedded_content | item), 
                                     analysis_data,
                                     control=list(max_treedepth = 15, adapt_delta = 0.99),
                                     file="../cache/brm_speaker")
summary(speaker_bayesian)
plot(speaker_bayesian)

speaker_bayesian_decorrelated <- brm(speaker_response ~ predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content || workerid) + (predicate + centered_prior_rating + centered_embedded_content || item), # de-correlate
                               analysis_data,
                               control=list(max_treedepth = 15, adapt_delta = 0.99),
                               file="../cache/brm_speaker_decorrelated")
summary(speaker_bayesian_decorrelated)
plot(speaker_bayesian_decorrelated)

full_speaker_bayesian <- brm(speaker_response ~ predicate * centered_prior_rating * centered_embedded_content + (1 + predicate + centered_prior_rating + centered_embedded_content | workerid) + (1 + predicate + centered_prior_rating + centered_embedded_content | item), 
                               full_analysis_data,
                               control=list(max_treedepth = 15, adapt_delta = 0.99),
                               file="../cache/brm_speaker_full")
summary(full_speaker_bayesian)
plot(full_speaker_bayesian)

full_speaker_bayesian_simple <- brm(speaker_response ~ predicate * centered_prior_rating * centered_embedded_content + (1 | workerid) + (1 | item), 
                             full_analysis_data,
                             control=list(max_treedepth = 15, adapt_delta = 0.99),
                             file="../cache/brm_speaker_full_simple")
summary(full_speaker_bayesian_simple)
plot(full_speaker_bayesian_simple)

full_speaker_bayesian_decorrelated <- brm(speaker_response ~ predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content || workerid) + (predicate + centered_prior_rating + centered_embedded_content || item), 
                             full_analysis_data,
                             control=list(max_treedepth = 15, adapt_delta = 0.99),
                             file="../cache/brm_speaker_full_decorrelated")
summary(full_speaker_bayesian_decorrelated)
plot(full_speaker_bayesian_decorrelated)

# ah
# ah_bayesian_simple <- brm(ah_response ~ predicate * centered_prior_rating * centered_embedded_content + (1 | workerid) + (1 | item), # only include random intercept since not enough observations 
#                         analysis_data |>
#                      filter(predicate != "Polar"),
#                         control=list(max_treedepth = 15, adapt_delta = 0.99),
#                         file="../cache/brm_ah_simple")
# summary(ah_bayesian_simple)

ah_bayesian <- brm(ah_response ~ predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content | workerid) + (predicate + centered_prior_rating + centered_embedded_content | item), 
                                analysis_data |>
                                  filter(predicate != "Polar"),
                                control=list(max_treedepth = 15, adapt_delta = 0.99),
                                file="../cache/brm_ah")
summary(ah_bayesian)
plot(ah_bayesian)

ah_bayesian_decorrelated <- brm(ah_response ~ predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content || workerid) + (predicate + centered_prior_rating + centered_embedded_content || item), # decorrelated 
                          analysis_data |>
                            filter(predicate != "Polar"),
                          control=list(max_treedepth = 15, adapt_delta = 0.99),
                          file="../cache/brm_ah_decorrelated")
summary(ah_bayesian_decorrelated)
plot(ah_bayesian_decorrelated)

full_ah_bayesian <- brm(ah_response ~ predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content | workerid) + (predicate + centered_prior_rating + centered_embedded_content | item), 
                                     full_analysis_data |>
                                       filter(predicate != "Polar"),
                                     control=list(max_treedepth = 15, adapt_delta = 0.99),
                                     file="../cache/brm_ah_full")
summary(full_ah_bayesian)
plot(full_ah_bayesian)

full_ah_bayesian_decorrelated <- brm(ah_response ~ predicate * centered_prior_rating * centered_embedded_content + (predicate + centered_prior_rating + centered_embedded_content || workerid) + (predicate + centered_prior_rating + centered_embedded_content || item), 
                        full_analysis_data |>
                          filter(predicate != "Polar"),
                        control=list(max_treedepth = 15, adapt_delta = 0.99),
                        file="../cache/brm_ah_full_decorrelated")
summary(full_ah_bayesian_decorrelated)
plot(full_ah_bayesian_decorrelated)
                 