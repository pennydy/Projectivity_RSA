setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(jsonlite)
library(rwebppl)
library(stringr)

source("./inferenceHelpers.R")
source("./BDA_vizhelpers.R")

print(Sys.setenv(NODE_OPTIONS = "--max-old-space-size=16384"))
Sys.getenv("NODE_OPTIONS")

training_items <- c( "Charley_H", "Charley_L","Frank_H", "Frank_L","Danny_H", "Danny_L", "Emily_H", "Emily_L","Emma_H", "Emma_L", "Grace_H", "Grace_L", "Isabella_H","Isabella_L","Jackson_H","Jackson_L", "Jayden_H","Jayden_L","Jon_H","Jon_L","Josh_H","Josh_L","Josie_H","Josie_L","Julian_H", "Julian_L","Mary_H","Mary_L","Mia_H","Mia_L","Olivia_H", "Olivia_L")
testing_items <- c("Owen_H", "Owen_L", "Sophia_H", "Sophia_L", "Tony_H", "Tony_L", "Zoe_H", "Zoe_L")

# 1. threshold_mix ----
## DATA----
# Read the pre-processed empirical data
# empirical_df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv")
# df <- empirical_df %>% 
#   mutate(speaker_response = trunc(trunc(speaker_response * 100)/10),
#          ah_response = trunc(trunc(ah_response * 100)/10)) %>%
#   filter(predicate %in% c("think", "know", "BARE"))
 
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>% 
  # convert the belief ratings (about the embedded content) to belief about p
  mutate(speaker_response = ifelse(polarity=="neg", 1-speaker_response, speaker_response),
         ah_response = ifelse(polarity=="neg", 1-ah_response, ah_response)) %>%
  # discretize the ratings
  mutate(speaker_response_bin = ifelse(speaker_response == 1, 10, trunc(speaker_response * 10) + 1)) %>%
  filter(predicate %in% c("Polar", "think", "know"),
         item %in% training_items)

# Create a script that can be used in the WebPPL interpeter (for testing)
# dataHeader = sprintf("var df = %s", toJSON(head(df)))
# write_file(paste(dataHeader, thresholdMixScript, sep = "\n"), file = "testScript.txt")

## INFERENCE ----
model <- makeModel("threshold_mix.txt", "threshold_mix")

thresholdMixScript <- wrapInference(model,"threshold_mix", 1000, 5, 500) 

thresholdMixPosteriors <- webppl(thresholdMixScript, data = df, data_var = "df", random_seed = 1024)

# saveRDS(thresholdMixPosteriors, "results/threshold_mix/thresholdMixPosteriors_negCost_seed1024-full.RDS")

# plot the posterior
graphPosteriors(thresholdMixPosteriors) + ggtitle("Posteriors")

ggsave("graphs/threshold_mix/thresholdMix_posteriors-full.pdf", width = 6, height = 3, units = "in")

# ggplot(thresholdMixPosteriors %>%
#          filter(!Parameter %in% c("alpha","bareCost")) %>%
#          pivot_wider(names_from = Parameter, values_from = value) %>% 
#          group_by(negCost, embedCost) %>% 
#          summarize(count = n()), 
#   aes(x = negCost, y = embedCost)) +
#   theme_bw() +
#   geom_tile(aes(fill = count))

# graphJointPosteriors(thresholdMixPosteriors)


## PREDICTIVES ----
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>% 
  filter(predicate %in% c("think", "know", "Polar"))

df_collapsed <- df %>% 
  group_by(predicate, item, polarity) %>%
  summarize(prob = mean(speaker_response)) %>% 
  mutate(polarity = case_when(polarity=="pos" ~ "Embedded: p",
                              polarity=="neg" ~ "Embedded: not p",
                              TRUE ~ "Polar"))

# read the posteriors instead of rerunning
thresholdMixPosteriors <- readRDS("results/threshold_mix/thresholdMixPosteriors_testing.RDS")

thresholdMixEstimates <- getEstimates(thresholdMixPosteriors) 

# thresholdMixEstimates <- data.frame(alpha=3, embedCost =2, negCost=-1)

thresholdMixPredictionScript <- wrapPrediction(model, "threshold_mix", thresholdMixEstimates)

thresholdMixPredictives <- webppl(thresholdMixPredictionScript, data = unique(df %>%  select(utterance, item)), data_var = "df")

thresholdMixPredictives <- thresholdMixPredictives %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              str_detect(utterance, "BARE") ~ "Polar",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         prob = case_when(str_detect(utterance, "doesnt") ~ 1 - prob,
                          TRUE ~ prob), # convert model prediction to about the embedded content 
         # polarity = fct_relevel(polarity, "Polar", "pos", "neg" ),
         predicate = fct_relevel(predicate, "Polar", "think", "know")) %>%
  mutate(polarity = case_when(polarity=="pos" ~ "Embedded: p",
                              polarity=="neg" ~ "Embedded: not p",
                              TRUE ~ "Polar"),
         polarity = fct_relevel(polarity, rev)) %>% 
  select(-utterance)

# graphPredictives(thresholdMixPredictives, df_collapsed)
graphPredictives_test(thresholdMixPredictives, df_collapsed)

ggsave("graphs/threshold_mix/thresholdMix-full_predictive-emprical_combined.pdf", width = 8, height = 4, units = "in")

# 2. threshold_cg (wonky common ground) ----
## DATA ----
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>% 
  mutate(speaker_response = ifelse(polarity=="neg", 1-speaker_response, speaker_response),
         ah_response = ifelse(polarity=="neg", 1-ah_response, ah_response)) %>%
  mutate(speaker_response_bin = ifelse(speaker_response == 1, 10, trunc(speaker_response * 10) + 1)) %>%
  filter(predicate %in% c("think", "know", "Polar")) %>% 
  # filter(item %in% c( "Charley_H", "Charley_L","Frank_H", "Frank_L","Danny_H", "Danny_L", "Emily_H", "Emily_L","Emma_H", "Emma_L", "Grace_H", "Grace_L", "Isabella_H","Isabella_L","Jackson_H","Jackson_L", "Jayden_H","Jayden_L","Jon_H","Jon_L","Josh_H","Josh_L","Josie_H","Josie_L","Julian_H", "Julian_L","Mary_H","Mary_L","Mia_H","Mia_L"))
  # filter(item %in% c( "Charley_H", "Charley_L","Frank_H", "Frank_L","Danny_H", "Danny_L", "Emily_H", "Emily_L","Emma_H", "Emma_L", "Grace_H", "Grace_L", "Isabella_H","Isabella_L","Jackson_H","Jackson_L", "Jayden_H","Jayden_L","Jon_H","Jon_L"))
  filter(item %in% c("Frank_H", "Frank_L","Danny_H", "Danny_L", "Emily_H", "Emily_L","Emma_H", "Emma_L", "Grace_H", "Grace_L", "Isabella_H","Isabella_L","Jon_H","Jon_L", "Jayden_H", "Jayden_L","Josh_H","Josh_L"))
  # filter(item %in% training_items)


## INFERENCE ----
# check inferenceHelpers.R: it is not reading literalListener_cg.txt 
model <- makeModel("threshold_cg.txt", "threshold_cg")

thresholdCgScript <- wrapInference(model,"threshold_cg", 100, 5, 10) 

thresholdCgPosteriors <- webppl(thresholdCgScript, data = df, data_var = "df", random_seed = 789)

saveRDS(thresholdCgPosteriors, "results/thresholdCgPosteriors_smallTest.RDS")

# plot the posterior
graphPosteriors(thresholdCgPosteriors) + ggtitle("Posteriors")

ggsave("graphs/thresholdCg_posteriors.pdf", width = 6, height = 3, units = "in")


## PREDICTIVES ----
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>% 
  filter(predicate %in% c("think", "know", "Polar"))

df_collapsed <- df %>% 
  group_by(predicate, item, polarity) %>%
  summarize(prob = mean(speaker_response)) %>% 
  mutate(polarity = case_when(polarity=="neg" ~ "Embedded: not p",
                              polarity=="Polar" ~ "Polar",
                              TRUE ~ "Embedded: p"))

# read the posteriors instead of rerunning
thresholdMixPosteriors <- readRDS("results/thresholdCgPosteriors_smallTest.RDS")

thresholdCgEstimates <- getEstimates(thresholdCgPosteriors) 

thresholdCgPredictionScript <- wrapPrediction(model, "threshold_cg", thresholdCgEstimates)

thresholdCgPredictives <- webppl(thresholdCgPredictionScript, data = unique(df %>%  select(utterance, item)), data_var = "df")

thresholdCgPredictives <- thresholdCgPredictives %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "Embedded: not p",
                              str_detect(utterance, "BARE") ~ "Polar",
                              TRUE ~ "Embedded: p"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         prob = case_when(str_detect(utterance, "doesnt") ~ 1 - prob,
                          TRUE ~ prob),
         polarity = fct_relevel(polarity, "Polar", "Embedded: p", "Embedded: not p" ),
         predicate = fct_relevel(predicate, "Polar", "think", "know")) %>%
  select(-utterance)

graphPredictives_cg(thresholdCgPredictives, df_collapsed)

ggsave("graphs/thresholdCg_predictive-empirical_smallTest.pdf", width = 8, height = 4, units = "in")

### simulation plot ----

# get mean priors
priors_by_condition <- read.csv("../interrogatives/data/condition-priors.csv") %>% 
  rename(prior_mean = prior_rating,
         prior = item_condition) %>% 
  mutate(prior_neg_mean = 1 - prior_mean) %>% 
  rename(item = prior)

combined_prior <- mean(priors_by_condition$prior_mean)
combined_neg_prior <- mean(priors_by_condition$prior_neg_mean)

observed_sp <- df_collapsed %>% 
  mutate(type = "observation")
sp <- thresholdCgPredictives %>% 
  mutate(type="prediction")

combine_sp <- rbind(sp, observed_sp) %>% 
  left_join(priors_by_condition, by=c("item")) %>% 
  mutate(prior_mean = ifelse(polarity=="Embedded: not p", prior_neg_mean, prior_mean)) %>% 
  select(-prior_neg_mean) %>% 
  mutate(predicate=fct_relevel(predicate, "Polar", "think","know"),
         polarity=fct_relevel(polarity, "Polar", "Embedded: p", "Embedded: not p"))

source("../interrogatives/helpers.R")

mean_predicted_sp <- sp %>%
  group_by(predicate, polarity) %>%  # collapse prior condition
  summarize(mean_posterior_belief = mean(prob),
            speaker_ci_low = ci.low(prob),
            speaker_ci_high = ci.high(prob)) %>%
  ungroup() %>%
  mutate(speaker_YMin = mean_posterior_belief - speaker_ci_low,
         speaker_YMax = mean_posterior_belief + speaker_ci_high,
         type="prediction")

mean_empirical_sp <- df_collapsed %>%
  group_by(predicate, polarity) %>%  # collapse prior condition
  summarize(mean_posterior_belief = mean(prob),
            speaker_ci_low = ci.low(prob),
            speaker_ci_high = ci.high(prob)) %>%
  ungroup() %>%
  mutate(speaker_YMin = mean_posterior_belief - speaker_ci_low,
         speaker_YMax = mean_posterior_belief + speaker_ci_high,
         type="observation")

mean_sp <- rbind(mean_empirical_sp, mean_predicted_sp) %>% 
  mutate(predicate=fct_relevel(predicate, "Polar", "think","know"),
         polarity=fct_relevel(polarity, "Polar", "Embedded: p", "Embedded: not p"))


ggplot(combine_sp,aes(x=prior_mean,y=prob, color=predicate)) +
  geom_point(aes(shape=type),alpha=0.6) +
  geom_smooth(aes(linetype=type),method="lm", fullrange=TRUE) +
  geom_point(data=mean_sp,
             aes(shape=type,
                 x=ifelse(polarity=="Embedded: not p", combined_neg_prior, combined_prior),
                 y=mean_posterior_belief,
                 fill=predicate),size=2,color="black",stroke=1) +
  geom_errorbar(data=mean_sp,
                aes(linetype=type,
                    x=ifelse(polarity=="Embedded: not p", combined_neg_prior, combined_prior),
                    y=mean_posterior_belief,
                    ymin=speaker_YMin, ymax=speaker_YMax),
                width=0.05,
                color="black") +
  facet_grid(.~polarity) +
  scale_color_manual(values=c("#56B4E9", "#009E73", "#F0E442")) +
  scale_fill_manual(values=c("#56B4E9", "#009E73", "#F0E442")) +
  scale_linetype()+
  scale_shape()+
  theme_bw()+
  scale_y_continuous(limits = c(0,1),
                     breaks=seq(0,1,by=.25),
                     name = "Posterior speaker belief\nin the embedded content") +
  scale_x_continuous(breaks=seq(0,1,by=0.2),
                     name = "Rating of prior belief in the embedded content") 
ggsave("./graphs/threshold_cg/PL_empirical-predicted-smallTest.pdf", width=8,height=3)


# 3. threshold_qud ----
## DATA ----
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>% 
  mutate(speaker_response = ifelse(polarity=="neg", 1-speaker_response, speaker_response)) %>% 
  mutate(speaker_response_bin = ifelse(speaker_response == 1, 10, trunc(speaker_response * 10) + 1)) %>%
  filter(predicate %in% c("think", "know", "Polar")) %>% 
  filter(item %in% training_items)

# Create a script that can be used in the WebPPL interpeter (for testing)
# dataHeader = sprintf("var df = %s", toJSON(head(df)))
# write_file(paste(dataHeader, thresholdMixScript, sep = "\n"), file = "testScript.txt")

## INFERENCE ----
model <- makeModel("threshold_qud.txt", "threshold_qud")

thresholdQudScript <- wrapInference(model,"threshold_qud", 1000, 5, 500) 

thresholdQudPosteriors <- webppl(thresholdQudScript, data = df, data_var = "df", random_seed = 6789)

saveRDS(thresholdQudPosteriors, "results/thresholdQudPosteriors_seed6789.RDS")

# plot the posterior
graphPosteriors(thresholdQudPosteriors) + ggtitle("Posteriors")

# ggsave("graphs/thresholdQud_posteriors.pdf", width = 6, height = 3, units = "in")


## PREDICTIVES ----
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>% 
  filter(predicate %in% c("think", "know", "Polar"))

df_collapsed <- df %>% 
  group_by(predicate, item, polarity) %>%
  summarize(prob = mean(speaker_response)) %>% 
  mutate(polarity = case_when(polarity=="neg" ~ "Embedded: not p",
                              polarity=="Polar" ~ "Polar",
                              TRUE ~ "Embedded: p"))

# read the posteriors instead of rerunning
thresholdQudPosteriors <- readRDS("results/thresholdQudPosteriors_seed6789.RDS")

thresholdQudEstimates <- getEstimates(thresholdQudPosteriors) 

thresholdQudPredictionScript <- wrapPrediction(model, "threshold_qud", thresholdQudEstimates)

thresholdQudPredictives <- webppl(thresholdQudPredictionScript, data = unique(df %>%  select(utterance, item)), data_var = "df")

thresholdQudPredictives <- thresholdQudPredictives %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "Embedded: not p",
                              str_detect(utterance, "BARE") ~ "Polar",
                              TRUE ~ "Embedded: p"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         prob = case_when(str_detect(utterance, "doesnt") ~ 1 - prob,
                          TRUE ~ prob),
         polarity = fct_relevel(polarity, "Polar", "Embedded: p", "Embedded: not p" ),
         predicate = fct_relevel(predicate, "Polar", "think", "know")) %>%
  select(-utterance)

graphPredictives_test(thresholdQudPredictives, df_collapsed)

ggsave("graphs/thresholdQud_predictive-empirical.pdf", width = 7, height = 3, units = "in")

### simulation plot ----

# get mean priors
priors_by_condition <- read.csv("../interrogatives/data/condition-priors.csv") %>% 
  rename(prior_mean = prior_rating,
         prior = item_condition) %>% 
  mutate(prior_neg_mean = 1 - prior_mean) %>% 
  rename(item = prior)

combined_prior <- mean(priors_by_condition$prior_mean)
combined_neg_prior <- mean(priors_by_condition$prior_neg_mean)

observed_sp <- df_collapsed %>% 
  mutate(type = "observation")
sp <- thresholdQudPredictives %>% 
  mutate(type="prediction")

combine_sp <- rbind(sp, observed_sp) %>% 
  left_join(priors_by_condition, by=c("item")) %>% 
  mutate(prior_mean = ifelse(polarity=="Embedded: not p", prior_neg_mean, prior_mean)) %>% 
  select(-prior_neg_mean) %>% 
  mutate(predicate=fct_relevel(predicate, "Polar", "think","know"),
         polarity=fct_relevel(polarity, "Polar", "Embedded: p", "Embedded: not p"))

source("../interrogatives/helpers.R")

mean_predicted_sp <- sp %>%
  group_by(predicate, polarity) %>%  # collapse prior condition
  summarize(mean_posterior_belief = mean(prob),
            speaker_ci_low = ci.low(prob),
            speaker_ci_high = ci.high(prob)) %>%
  ungroup() %>%
  mutate(speaker_YMin = mean_posterior_belief - speaker_ci_low,
         speaker_YMax = mean_posterior_belief + speaker_ci_high,
         type="prediction")

mean_empirical_sp <- df_collapsed %>%
  group_by(predicate, polarity) %>%  # collapse prior condition
  summarize(mean_posterior_belief = mean(prob),
            speaker_ci_low = ci.low(prob),
            speaker_ci_high = ci.high(prob)) %>%
  ungroup() %>%
  mutate(speaker_YMin = mean_posterior_belief - speaker_ci_low,
         speaker_YMax = mean_posterior_belief + speaker_ci_high,
         type="observation")

mean_sp <- rbind(mean_empirical_sp, mean_predicted_sp) %>% 
  mutate(predicate=fct_relevel(predicate, "Polar", "think","know"),
         polarity=fct_relevel(polarity, "Polar", "Embedded: p", "Embedded: not p"))


ggplot(combine_sp,aes(x=prior_mean,y=prob, color=predicate)) +
  geom_point(aes(shape=type),alpha=0.6) +
  geom_smooth(aes(linetype=type),method="lm", fullrange=TRUE) +
  geom_point(data=mean_sp,
             aes(shape=type,
                 x=ifelse(polarity=="Embedded: not p", combined_neg_prior, combined_prior),
                 y=mean_posterior_belief,
                 fill=predicate),size=2,color="black",stroke=1) +
  geom_errorbar(data=mean_sp,
                aes(linetype=type,
                    x=ifelse(polarity=="Embedded: not p", combined_neg_prior, combined_prior),
                    y=mean_posterior_belief,
                    ymin=speaker_YMin, ymax=speaker_YMax),
                width=0.05,
                color="black") +
  facet_grid(.~polarity) +
  scale_color_manual(values=c("#56B4E9", "#009E73", "#F0E442")) +
  scale_fill_manual(values=c("#56B4E9", "#009E73", "#F0E442")) +
  scale_linetype()+
  scale_shape()+
  theme_bw()+
  scale_y_continuous(limits = c(0,1),
                     breaks=seq(0,1,by=.25),
                     name = "Posterior speaker belief\nin the embedded content") +
  scale_x_continuous(breaks=seq(0,1,by=0.2),
                     name = "Rating of prior belief in the embedded content") 
ggsave("./graphs/threshold_qud/PL_empirical-predicted.pdf", width=7,height=3)


# 4. chemo_production ----
# model reported in the cogsci paper (three possible belief states: p, not p, uncertain)
# and it uses all data
## DATA ----
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>% 
  # empirical: belief in the embedded content, convert to belief in p
  mutate(speaker_response = ifelse(polarity=="neg", 1-speaker_response, speaker_response),
         ah_response = ifelse(polarity=="neg", 1-ah_response, ah_response)) %>% 
  mutate(speaker_response = case_when(speaker_response > 0.6 ~ "dances", 
                                      speaker_response < 0.4 ~ "doesnt_dance",
                                      TRUE ~ "uncertain"),
         ah_response = case_when(is.na(ah_response) ~ "null",
                                 ah_response > 0.6 ~ "dances",
                                 ah_response < 0.4 ~ "doesnt_dance",
                                 TRUE ~ "uncertain")) %>%
  filter(predicate %in% c("think", "know", "Polar"))

# Create a script that can be used in the WebPPL interpeter (for testing)
# dataHeader = sprintf("var df = %s", toJSON(head(df)))
# write_file(paste(dataHeader, thresholdMixScript, sep = "\n"), file = "testScript.txt")


## INFERENCE ----
model <- makeModel("chemo_production.txt", "chemo_production")

chemoProductionScript <- wrapInference(model,"chemo_production", 1000, 10, 500)

chemoProductionPosteriors <- webppl(chemoProductionScript, data = df, data_var = "df", random_seed = 9999)

saveRDS(chemoProductionPosteriors, "results/chemo_production/chemoProductionPosteriors-seed9999_bin3_4-small.RDS")

# plot the posterior
graphPosteriors(chemoProductionPosteriors) + ggtitle("Posteriors")

# posteriorsPlot <- chemoProductionPosteriors %>% 
#   mutate(belief_states = gsub("^([A-Za-z]+_[A-Za-z]+)_([A-Za-z]+)", "\\1", Parameter),
#          predicate = gsub("^([A-Za-z]+_[A-Za-z]+)_([A-Za-z]+)", "\\2", Parameter))
# 
# posteriorsPlot$predicate=factor(posteriorsPlot$predicate, levels=c("knowDances","knowNot", "thinkDances","thinkNot","BARE"))

ggsave("graphs/chemoProductionPosteriors_posteriors-seed9876.pdf", width = 14, height = 8, units = "in")


## PREDICTIVES ----
chemoProductionPosteriors <- readRDS("results/chemo_production/chemoProductionPosteriors-seed9876_bin3_4-small.RDS")

chemoProductionEstimates <- getEstimates(chemoProductionPosteriors) 

### speaker ----
chemoProductionSpeakerScript <- wrapPrediction(model, "chemo_speaker", chemoProductionEstimates)

chemoProductionSpeakerPredictives <- webppl(chemoProductionSpeakerScript, data = unique(df %>%  select(speaker_response, ah_response)), data_var = "df")

chemoProductionSpeakerPredictives <- chemoProductionSpeakerPredictives %>% 
  unnest(predictives) %>% 
  rename(speaker_belief = belief_tuple.speaker_belief,
         ah_belief = belief_tuple.ah_belief,
         utterance = val,
         prob = prob)
write.csv(chemoProductionSpeakerPredictives, "../chemo/results/chemo_production/bda/production/seed9999-small.csv", row.names=FALSE)

PS_summary = chemoProductionSpeakerPredictives %>% 
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
                               utterance == "know-dances-?" ~ '"know-p"',
                               utterance == "think-dances-?" ~ '"think-p"',
                               utterance == "know-doesnt_dance-?" ~ '"know-not_p"',
                               TRUE ~ '"think-not_p"'),
         belief_states = paste("<",speaker_belief,", ",ah_belief,">", sep="")) %>%
  group_by(utterance,belief_states,predicate,utterance_type) %>% 
  summarize(mean_prob = mean(prob)) %>% 
  ungroup() %>% 
  mutate(belief_states = fct_relevel(belief_states, '<p, p>','<p, not p>', '<p, ?>','<p, null>', '<not p, p>', '<not p, not p>', '<not p, ?>', '<not p, null>', '<?, p>','<?, not p>', '<?, ?>', '<?, null>'),
         utterance = fct_relevel(utterance, 'Polar', '"know-p"', '"know-not_p"', '"think-p"', '"think-not_p"'),
         predicate = fct_relevel(predicate, 'Polar', 'think', 'know'),
         utterance_type = fct_relevel(utterance_type, 'p', 'not p'))

ggplot(PS_summary,
       aes(x=utterance, y=mean_prob, fill = predicate)) +
  geom_bar(stat="identity") +
  facet_wrap(belief_states~.) +
  scale_fill_manual(values=c("#56B4E9", "#009E73", "#F0E442"),
                    # labels=c('Polar', 'think', 'know'),
                    name="Predicate") +
  scale_x_discrete(name="Utterance",
                   guide=guide_axis(angle = 65)) +
  theme(legend.position="top") +
  ylab("Production probability")
ggsave("../chemo/graphs/chemo_production/chemo_production-PS9999-bin3_4-small.pdf",width=6,height=4)


### pragListsener ---- 
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>%
  filter(predicate %in% c("think", "know", "Polar"))

df_speaker <- df %>%
  group_by(predicate, item, polarity) %>%
  summarize(prob = mean(speaker_response))

df_ah <- df %>%
  filter(polarity!="Polar") %>%
  group_by(predicate, item, polarity) %>%
  summarize(prob = mean(ah_response))

chemoProductionPredictionScript <- wrapPrediction(model, "chemo_production", chemoProductionEstimates)

chemoProductionPredictives <- webppl(chemoProductionPredictionScript, data = unique(df %>%  select(utterance, item)), data_var = "df")

chemoProductionPredictives <- chemoProductionPredictives %>% 
  unnest(predictives) %>% 
  rename(speaker_belief = val.speaker_belief,
         ah_belief = val.ah_belief)

#### marginal sp_belief ----
chemoProductionPredictives_speaker <- chemoProductionPredictives %>% 
  group_by(utterance, item, speaker_belief) %>% 
    summarize(prob = sum(prob)) %>%
    ungroup()
write.csv(chemoProductionPredictives_speaker, "../chemo/results/chemo_production/bda/pragListener/speaker/seed9999-small.csv", row.names=FALSE)

sp <- chemoProductionPredictives_speaker %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              str_detect(utterance, "BARE") ~ "Polar",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         prior_condition = gsub("^([A-Za-z]+)_([A-Za-z]+)", "\\2", item)) %>% 
  filter((polarity %in% c("Polar", "pos") & speaker_belief == "dances") | 
           polarity == "neg" & speaker_belief == "doesnt_dance") %>%
  mutate(polarity = fct_relevel(polarity, "Polar", "pos", "neg" ),
         predicate = fct_relevel(predicate, "Polar", "think", "know")) %>%
  select(-c(utterance, speaker_belief))


graphPredictives(sp %>% select(-prior_condition), df_speaker)

# ggsave("graphs/chemoProduction_predictive-emprical-1234sp.pdf", width = 6, height = 3, units = "in")

#### marginal ah_belief ----
chemoProductionPredictives_ah <- chemoProductionPredictives %>% 
  group_by(utterance, item, ah_belief) %>% 
  summarize(prob = sum(prob)) %>%
  ungroup()
write.csv(chemoProductionPredictives_ah, "../chemo/results/chemo_production/bda/pragListener/ah/seed9999-small.csv", row.names=FALSE)

chemoProductionPredictives_ah <- chemoProductionPredictives %>% 
  filter(utterance != "BARE-dances-?") %>% 
  group_by(utterance, item, ah_belief) %>% 
  summarize(prob = sum(prob)) %>% 
  ungroup() %>% 
  filter(val.ah_belief == "dances")

ah <- chemoProductionPredictives_ah %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               TRUE ~ "think"),
         prob = case_when(str_detect(utterance, "doesnt") ~ 1 - prob,
                          TRUE ~ prob),
         polarity = fct_relevel(polarity, "pos", "neg" ),
         predicate = fct_relevel(predicate, "think", "know")) %>%
  select(-c(utterance, ah_belief))

graphPredictives(ah, df_ah)
ggsave("graphs/chemoProduction_predictive-emprical-ah6789.pdf", width = 6, height = 3, units = "in")



# 5. simple_chemo ----
## DATA ----
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>% 
  # empirical: belief in the embedded content, convert to belief in p
  mutate(speaker_response = ifelse(polarity=="neg", 1-speaker_response, speaker_response),
         ah_response = ifelse(polarity=="neg", 1-ah_response, ah_response)) %>% 
  mutate(speaker_response = ifelse(speaker_response > 0.5, "dances", "doesnt_dance"),
         ah_response = case_when(is.na(ah_response) ~ "null",
                                 ah_response > 0.5 ~ "dances",
                                 TRUE ~ "doesnt_dance")) %>%
  filter(predicate %in% c("think", "know", "Polar")) %>% 
  filter(item %in% training_items)

# Create a script that can be used in the WebPPL interpeter (for testing)
# dataHeader = sprintf("var df = %s", toJSON(head(df)))
# write_file(paste(dataHeader, thresholdMixScript, sep = "\n"), file = "testScript.txt")


## INFERENCE ----
model <- makeModel("simple_chemo.txt", "simple_chemo")

simpleChemoScript <- wrapInference(model,"simple_chemo", 1000, 10, 500)

simpleChemoPosteriors <- webppl(simpleChemoScript, data = df, data_var = "df", random_seed = 1024)

saveRDS(simpleChemoPosteriors, "results/simple_chemo/simpleChemoPosteriors-seed1024.RDS")

# plot the posterior
graphPosteriors(simpleChemoPosteriors) + ggtitle("Posteriors")

ggsave("graphs/simple_chemo/simpleChemo_posteriors.pdf", width = 14, height = 8, units = "in")


## PREDICTIVES ----
simpleChemoPosteriors <- readRDS("results/simple_chemo/simpleChemoPosteriors-seed1234.RDS")

simpleChemoEstimates <- getEstimates(simpleChemoPosteriors) 

simpleChemoScript <- wrapPrediction(model, "simple_chemo", simpleChemoEstimates)

simpleChemoPredictives <- webppl(simpleChemoScript, data = unique(df %>%  select(utterance, item)), data_var = "df")

simpleChemoPredictives <- simpleChemoPredictives %>% 
  unnest(predictives) %>% 
  rename(speaker_belief = val.speaker_belief,
         ah_belief = val.ah_belief) 
  # mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "Embedded: not p",
  #                             str_detect(utterance, "BARE") ~ "Polar",
  #                             TRUE ~ "Embedded: p"),
  #        predicate = case_when(str_detect(utterance, "know") ~ "know",
  #                              str_detect(utterance, "think") ~ "think",
  #                              TRUE ~ "Polar"),
  #        prob = case_when(str_detect(utterance, "doesnt") ~ 1 - prob,
  #                         TRUE ~ prob),
  #        polarity = fct_relevel(polarity, "Polar", "Embedded: p", "Embedded: not p" ),
  #        predicate = fct_relevel(predicate, "Polar", "think", "know")) %>%
  # select(-utterance)

# load the empirical data
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>%
  filter(predicate %in% c("think", "know", "Polar"))

df_speaker <- df %>%
  group_by(predicate, item, polarity) %>%
  summarize(prob = mean(speaker_response)) %>% 
  ungroup() %>% 
  mutate(polarity = case_when(polarity=="neg" ~ "Embedded: not p",
                              polarity=="pos" ~ "Polar",
                              TRUE ~ "Embedded: p")) 

df_ah <- df %>%
  filter(polarity!="Polar") %>%
  group_by(predicate, item, polarity) %>%
  summarize(prob = mean(ah_response)) %>% 
  ungroup() %>% 
  mutate(polarity = case_when(polarity=="neg" ~ "Embedded: not p",
                              polarity=="pos" ~ "Polar",
                              TRUE ~ "Embedded: p")) %>% 
  mutate(polarity = fct_relevel(polarity, "Polar", "Embedded: p", "Embedded: not p"),
         predicate = fct_relevel(predicate, "Polar", "think", "know"))

#### marginal sp_belief ----
simpleChemoPredictives_speaker <- simpleChemoPredictives %>% 
  group_by(utterance, item, speaker_belief) %>% 
  summarize(prob = sum(prob)) %>%
  ungroup()

sp <- simpleChemoPredictives_speaker %>%
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "Embedded: not p",
                              str_detect(utterance, "BARE") ~ "Polar",
                              TRUE ~ "Embedded: p"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         prior_condition = gsub("^([A-Za-z]+)_([A-Za-z]+)", "\\2", item)) %>%
  filter((polarity %in% c("Polar", "Embedded: p") & speaker_belief == "dances") |
           polarity == "Embedded: not p" & speaker_belief == "doesnt_dance") %>%
  select(-c(utterance, speaker_belief)) %>%
  mutate(polarity = fct_relevel(polarity, "Polar", "Embedded: p", "Embedded: not p"),
         predicate = fct_relevel(predicate, "Polar", "think", "know"))

graphPredictives_test(sp, df_speaker)

# ggsave("graphs/chemoProduction_predictive-emprical-1234sp.pdf", width = 6, height = 3, units = "in")

#### marginal ah_belief ----
chemoProductionPredictives_ah <- chemoProductionPredictives %>% 
  group_by(utterance, item, ah_belief) %>% 
  summarize(prob = sum(prob)) %>%
  ungroup()
write.csv(chemoProductionPredictives_ah, "../chemo/results/chemo_production/bda/pragListener/ah/seed9999-small.csv", row.names=FALSE)

chemoProductionPredictives_ah <- chemoProductionPredictives %>% 
  filter(utterance != "BARE-dances-?") %>% 
  group_by(utterance, item, ah_belief) %>% 
  summarize(prob = sum(prob)) %>% 
  ungroup() %>% 
  filter(val.ah_belief == "dances")

ah <- chemoProductionPredictives_ah %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               TRUE ~ "think"),
         prob = case_when(str_detect(utterance, "doesnt") ~ 1 - prob,
                          TRUE ~ prob),
         polarity = fct_relevel(polarity, "pos", "neg" ),
         predicate = fct_relevel(predicate, "think", "know")) %>%
  select(-c(utterance, ah_belief))

graphPredictives(ah, df_ah)
ggsave("graphs/chemoProduction_predictive-emprical-ah6789.pdf", width = 6, height = 3, units = "in")


# 6. simple_chemo_production ----
## DATA ----
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>% 
  mutate(speaker_response = ifelse(polarity=="neg", 1-speaker_response, speaker_response),
       ah_response = ifelse(polarity=="neg", 1-ah_response, ah_response)) %>% 
  mutate(speaker_response = ifelse(speaker_response > 0.5, "dances", "doesnt_dance"),
         ah_response = case_when(is.na(ah_response) ~ "null",
                                 ah_response > 0.5 ~ "dances",
                                 TRUE ~ "doesnt_dance")) %>%
  filter(predicate %in% c("think", "know", "Polar")) %>% 
  filter(item %in% training_items)

# Create a script that can be used in the WebPPL interpeter (for testing)
# dataHeader = sprintf("var df = %s", toJSON(head(df)))
# write_file(paste(dataHeader, thresholdMixScript, sep = "\n"), file = "testScript.txt")

## INFERENCE ----
model <- makeModel("simple_chemo_production.txt", "simple_chemo_production")

simplechemoProductionScript <- wrapInference(model,"simple_chemo_production", 1000, 10, 500)

simplechemoProductionPosteriors <- webppl(simplechemoProductionScript, data = df, data_var = "df", random_seed = 1234)

saveRDS(simplechemoProductionPosteriors, "results/chemo_production/simplechemoProductionPosteriors-seed1234.RDS")

# plot the posterior
# graphPosteriors(simplechemoProductionPosteriors) + ggtitle("Posteriors")
# ggsave("graphs/simple_chemo_production/simplechemoProductionPosteriors_posteriors-seed1234.pdf", width = 14, height = 8, units = "in")


## PREDICTIVES ----
simplechemoProductionPosteriors <- readRDS("results/chemo_production/simplechemoProductionPosteriors-seed1234.RDS")

simplechemoProductionEstimates <- getEstimates(simplechemoProductionPosteriors) 

### speaker ----
simplechemoProductionSpeakerScript <- wrapPrediction(model, "simple_chemo_production", simplechemoProductionEstimates)

simplechemoProductionSpeakerPredictives <- webppl(simplechemoProductionSpeakerScript, data = unique(df %>%  select(speaker_response, ah_response)), data_var = "df")

simplechemoProductionSpeakerPredictives <- simplechemoProductionSpeakerPredictives %>% 
  unnest(predictives) %>% 
  rename(speaker_belief = belief_tuple.speaker_belief,
         ah_belief = belief_tuple.ah_belief,
         utterance = val,
         prob = prob)
write.csv(simplechemoProductionSpeakerPredictives, "../chemo/results/simple_chemo_production/bda/production/seed1234.csv", row.names=FALSE)

PS_summary = simplechemoProductionSpeakerPredictives %>% 
  mutate(utterance_type = ifelse(str_detect(utterance, "doesnt"), "not p","p"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         speaker_belief = case_when(str_detect(speaker_belief, "doesnt") ~ "not p",
                                    TRUE ~ "p"),
         ah_belief = case_when(str_detect(ah_belief, "null") ~ "null",
                               str_detect(ah_belief, "doesnt") ~ "not p",
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
  mutate(belief_states = fct_relevel(belief_states, '<p, p>','<p, not p>', '<p, null>', '<not p, p>', '<not p, not p>', '<not p, null>'),
         utterance = fct_relevel(utterance, 'Polar', '"know-p"', '"know-not_p"', '"think-p"', '"think-not_p"'),
         predicate = fct_relevel(predicate, 'Polar', 'think', 'know'),
         utterance_type = fct_relevel(utterance_type, 'p', 'not p'))

ggplot(PS_summary,
       aes(x=utterance, y=mean_prob, fill = predicate)) +
  geom_bar(stat="identity") +
  facet_wrap(belief_states~.) +
  scale_fill_manual(values=c("#56B4E9", "#009E73", "#F0E442"),
                    # labels=c('Polar', 'think', 'know'),
                    name="Predicate") +
  scale_x_discrete(name="Utterance",
                   guide=guide_axis(angle = 65)) +
  theme(legend.position="top") +
  ylab("Production probability")
ggsave("graphs/simple_chemo_production/simple_chemo_production-PS.pdf",width=6,height=4)


### pragListsener ---- 
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>%
  filter(predicate %in% c("think", "know", "Polar"))

df_speaker <- df %>%
  group_by(predicate, item, polarity) %>%
  summarize(prob = mean(speaker_response))

df_ah <- df %>%
  filter(polarity!="Polar") %>%
  group_by(predicate, item, polarity) %>%
  summarize(prob = mean(ah_response))

# predicting the pragmatic listener
simplechemoProductionPredictionScript <- wrapPrediction(model, "chemo_production", simplechemoProductionEstimates)

simplechemoProductionPredictives <- webppl(simplechemoProductionPredictionScript, data = unique(df %>%  select(utterance, item)), data_var = "df")

simplechemoProductionPredictives <- simplechemoProductionPredictives %>%
  unnest(predictives) %>%
  rename(speaker_belief = val.speaker_belief,
         ah_belief = val.ah_belief)

#### marginal sp_belief ----
simplechemoProductionPredictives_speaker <- simplechemoProductionPredictives %>% 
  group_by(utterance, item, speaker_belief) %>% 
  summarize(prob = sum(prob)) %>%
  ungroup()
write.csv(simplechemoProductionPredictives_speaker, "../chemo/results/simple_chemo_production/bda/pragListener/speaker/seed1234.csv", row.names=FALSE)

sp <- simplechemoProductionPredictives_speaker %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              str_detect(utterance, "BARE") ~ "Polar",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         prior_condition = gsub("^([A-Za-z]+)_([A-Za-z]+)", "\\2", item)) %>% 
  filter((polarity %in% c("Polar", "pos") & speaker_belief == "dances") | 
           polarity == "neg" & speaker_belief == "doesnt_dance") %>%
  mutate(polarity = fct_relevel(polarity, "Polar", "pos", "neg" ),
         predicate = fct_relevel(predicate, "Polar", "think", "know")) %>%
  select(-c(utterance, speaker_belief))


graphPredictives(sp %>% select(-prior_condition), df_speaker)

ggsave("graphs/simple_chemo_production/simplechemoProduction_predictive-emprical-1234sp.pdf", width = 6, height = 3, units = "in")

#### marginal ah_belief ----
simplechemoProductionPredictives_ah <- simplechemoProductionPredictives %>% 
  filter(utterance != "BARE-dances-?") %>% 
  group_by(utterance, item, ah_belief) %>% 
  summarize(prob = sum(prob)) %>% 
  ungroup() %>% 
  filter(ah_belief == "dances")
write.csv(simplechemoProductionPredictives_ah, "../chemo/results/simple_chemo_production/bda/pragListener/ah/seed1234.csv", row.names=FALSE)

ah <- simplechemoProductionPredictives_ah %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               TRUE ~ "think"),
         prob = case_when(str_detect(utterance, "doesnt") ~ 1 - prob,
                          TRUE ~ prob),
         polarity = fct_relevel(polarity, "pos", "neg" ),
         predicate = fct_relevel(predicate, "think", "know")) %>%
  select(-c(utterance, ah_belief))

graphPredictives(ah, df_ah)
ggsave("graphs/simple_chemo_production/chemoProduction_predictive-emprical-ah1234.pdf", width = 6, height = 3, units = "in")



# 7. threshold_mix_production----
## DATA----
# Read the pre-processed empirical data
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>% 
  # convert the belief ratings (about the embedded content) to belief about p
  mutate(speaker_response = ifelse(polarity=="neg", 1-speaker_response, speaker_response),
         ah_response = ifelse(polarity=="neg", 1-ah_response, ah_response)) %>%
  # discretize the ratings
  mutate(speaker_response_bin = ifelse(speaker_response == 1, 10, trunc(speaker_response * 10) + 1)) %>%
  filter(predicate %in% c("Polar", "think", "know"),
         item %in% training_items)

## INFERENCE ----
model <- makeModel("threshold_mix_production.txt", "threshold_mix_production")

thresholdMixProductionScript <- wrapInference(model,"threshold_mix_production", 1000, 5, 500)

# Create a script that can be used in the WebPPL interpeter (for testing)
# dataHeader = sprintf("var df = %s", toJSON(head(df)))
# write_file(paste(dataHeader, thresholdMixProductionScript, sep = "\n"), file = "testScript.txt")

thresholdMixProductionPosteriors <- webppl(thresholdMixProductionScript, data = df, data_var = "df", random_seed = 1024)

saveRDS(thresholdMixProductionPosteriors, "results/threshold_mix_production/thresholdMixProductionPosteriors-seed1024.RDS")


## PREDICTIVES ----
thresholdMixProductionPosteriors <- readRDS("results/threshold_mix_production/thresholdMixProductionPosteriors-seed1024.RDS")

thresholdMixProductionEstimates <- getEstimates(thresholdMixProductionPosteriors) 

### speaker ----
thresholdMixProductionSpeakerScript <- wrapPrediction(model, "threshold_mix_speaker", thresholdMixProductionEstimates)

thresholdMixProductionSpeakerPredictives <- webppl(thresholdMixProductionSpeakerScript, data = unique(df %>%  select(speaker_response_bin)), data_var = "df")

thresholdMixProductionSpeakerPredictives <- thresholdMixProductionSpeakerPredictives %>% 
  unnest(predictives) %>% 
  rename(utterance = val,
         prob = prob)
# write.csv(simplechemoProductionSpeakerPredictives, "../interrogatives/results/threshold_mix/bda/production/seed1024.csv", row.names=FALSE)

PS_summary = thresholdMixProductionSpeakerPredictives %>% 
  mutate(utterance_type = ifelse(str_detect(utterance, "doesnt"), "not p","p"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         utterance = case_when(utterance == "BARE-dances-?" ~ "Polar",
                               utterance == "know-dances-?" ~ '"know-p"',
                               utterance == "think-dances-?" ~ '"think-p"',
                               utterance == "know-doesnt_dance-?" ~ '"know-not_p"',
                               TRUE ~ '"think-not_p"')) %>%
  group_by(utterance,speaker_belief,predicate,utterance_type) %>% 
  summarize(mean_prob = mean(prob)) %>% 
  ungroup() %>% 
  mutate(utterance = fct_relevel(utterance, 'Polar', '"know-p"', '"know-not_p"', '"think-p"', '"think-not_p"'),
         predicate = fct_relevel(predicate, 'Polar', 'think', 'know'),
         utterance_type = fct_relevel(utterance_type, 'p', 'not p'))

ggplot(PS_summary,
       aes(x=utterance, y=mean_prob, fill = predicate)) +
  geom_bar(stat="identity") +
  facet_grid(.~speaker_belief) +
  scale_fill_manual(values=c("#56B4E9", "#009E73", "#F0E442"),
                    # labels=c('Polar', 'think', 'know'),
                    name="Predicate") +
  scale_x_discrete(name="Utterance",
                   guide=guide_axis(angle = 65)) +
  theme(legend.position="top") +
  ylab("Production probability")


### pragListsener ---- 
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>%
  filter(predicate %in% c("think", "know", "Polar"))

df_speaker <- df %>%
  group_by(predicate, item, polarity) %>%
  summarize(prob = mean(speaker_response))

# predicting the pragmatic listener
thresholdMixProductionScript <- wrapPrediction(model, "threshold_mix", thresholdMixProductionEstimates)

thresholdMixProductionPredictives <- webppl(thresholdMixProductionScript, data = unique(df %>%  select(utterance, item)), data_var = "df")

sp <- thresholdMixProductionPredictives %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                              str_detect(utterance, "BARE") ~ "Polar",
                              TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         prior_condition = gsub("^([A-Za-z]+)_([A-Za-z]+)", "\\2", item)) %>% 
  mutate(speaker_belief = ifelse(polarity == "neg", 1-prob, prob)) %>%
  mutate(polarity = fct_relevel(polarity, "Polar", "pos", "neg" ),
         predicate = fct_relevel(predicate, "Polar", "think", "know"))


graphPredictives(sp %>% select(-prior_condition), df_speaker)

ggsave("graphs/simple_chemo_production/simplechemoProduction_predictive-emprical-1234sp.pdf", width = 6, height = 3, units = "in")


# 8. threshold_LL ----
## DATA ----
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>% 
  mutate(speaker_response = ifelse(polarity=="neg", 1-speaker_response, speaker_response),
         ah_response = ifelse(polarity=="neg", 1-ah_response, ah_response)) %>%
  mutate(speaker_response_bin = ifelse(speaker_response == 1, 10, trunc(speaker_response * 10) + 1)) %>%
  filter(predicate %in% c("think", "know", "Polar")) %>% 
  filter(item %in% training_items)

df_know <- df %>% 
  filter(predicate == "know")

df_bare <- df %>% 
  filter(predicate == "Polar")

# df_test <- head(df_know, 100)


## INFERENCE ----
model <- makeModel("threshold_LL.txt", "threshold_LL")

thresholdLLScript <- wrapInference(model,"threshold_LL", 1000, 5, 100) 

### know ----
thresholdLLPosteriors <- webppl(thresholdLLScript, data = df_know, data_var = "df", random_seed = 6789)

saveRDS(thresholdLLPosteriors, "results/threshold_LL/thresholdLLPosteriors_know_smallTest.RDS")

# plot the posterior
graphPosteriors(thresholdLLPosteriors) + ggtitle("Posteriors")

ggsave("graphs/thresholdLL_posteriors.pdf", width = 6, height = 3, units = "in")


### bare ----
thresholdLLPosteriors_bare <- webppl(thresholdLLScript, data = df_bare, data_var = "df", random_seed = 6789)

saveRDS(thresholdLLPosteriors_bare, "results/threshold_LL/thresholdLLPosteriors_BARE.RDS")

# plot the posterior
graphPosteriors(thresholdLLPosteriors) + ggtitle("Posteriors")

ggsave("graphs/thresholdLL_posteriors.pdf", width = 6, height = 3, units = "in")


## PREDICTIVES ----
thresholdLLPosteriors <- readRDS("results/threshold_LL/thresholdLLPosteriors_smallTest.RDS")
df <- read.csv("../../data/2_listenerSpeakerAH/main/bda_data.csv") %>% 
  # filter(predicate %in% c("think", "know", "Polar")) 
  filter(predicate == "know")

df_collapsed <- df %>% 
  group_by(predicate, item, polarity) %>%
  summarize(prob = mean(speaker_response)) %>% 
  mutate(polarity = case_when(polarity=="neg" ~ "Embedded: not p",
                              polarity=="Polar" ~ "Polar",
                              TRUE ~ "Embedded: p"))

# read the posteriors instead of rerunning
# thresholdLLPosteriors <- readRDS("results/thresholdLLPosteriors.RDS")

thresholdLLEstimates <- getEstimates(thresholdLLPosteriors) 

thresholdLLPredictionScript <- wrapPrediction(model, "threshold_LL", thresholdLLEstimates)

thresholdLLPredictives <- webppl(thresholdLLPredictionScript, data = unique(df %>% select(utterance, item)), data_var = "df")

predictivesTest <- thresholdLLPredictives %>% 
  unnest("prob") %>% 
  mutate(score = exp(score)) %>% # back to the prob space
  group_by(utterance, item) %>% 
  mutate(sum = sum(score)) %>% 
  ungroup() %>% 
  group_by(utterance, item, value) %>% 
  summarize(value_score = sum(score),
         prob = value_score/sum) %>% 
  ungroup() %>% 
  select(-value_score) %>%
  unique()

predictivesTest <- predictivesTest %>% 
  mutate(value = value/10,
         value_prob = value * prob) %>% 
  group_by(utterance, item) %>% 
  summarize(prob = sum(value_prob)) %>% 
  ungroup()
  
knowPredictives <- predictivesTest %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "Embedded: not p",
                              TRUE ~ "Embedded: p"),
         predicate = "know",
         prob = case_when(str_detect(utterance, "doesnt") ~ 1 - prob,
                          TRUE ~ prob),
         polarity = fct_relevel(polarity, "Embedded: p", "Embedded: not p")) %>% 
  select(-utterance)

graphPredictives_test(knowPredictives, df_collapsed)

thresholdLLPredictives <- thresholdLLPredictives %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "Embedded: not p",
                              str_detect(utterance, "BARE") ~ "Polar",
                              TRUE ~ "Embedded: p"),
         predicate = case_when(str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "think") ~ "think",
                               TRUE ~ "Polar"),
         prob = case_when(str_detect(utterance, "doesnt") ~ 1 - prob,
                          TRUE ~ prob),
         polarity = fct_relevel(polarity, "Polar", "Embedded: p", "Embedded: not p" ),
         predicate = fct_relevel(predicate, "Polar", "think", "know")) %>%
  select(-utterance)

graphPredictives_test(thresholdLLPredictives, df_collapsed)

ggsave("graphs/thresholdLL_predictive-empirical.pdf", width = 8, height = 4, units = "in")

### simulation plot ----

# get mean priors
priors_by_condition <- read.csv("../interrogatives/data/condition-priors.csv") %>% 
  rename(prior_mean = prior_rating,
         prior = item_condition) %>% 
  mutate(prior_neg_mean = 1 - prior_mean) %>% 
  rename(item = prior)

combined_prior <- mean(priors_by_condition$prior_mean)
combined_neg_prior <- mean(priors_by_condition$prior_neg_mean)

observed_sp <- df_collapsed %>% 
  mutate(type = "observation")
sp <- thresholdLLPredictives %>% 
  mutate(type="prediction")

combine_sp <- rbind(sp, observed_sp) %>% 
  left_join(priors_by_condition, by=c("item")) %>% 
  mutate(prior_mean = ifelse(polarity=="Embedded: not p", prior_neg_mean, prior_mean)) %>% 
  select(-prior_neg_mean) %>% 
  mutate(predicate=fct_relevel(predicate, "Polar", "think","know"),
         polarity=fct_relevel(polarity, "Polar", "Embedded: p", "Embedded: not p"))

source("../interrogatives/helpers.R")

mean_predicted_sp <- sp %>%
  group_by(predicate, polarity) %>%  # collapse prior condition
  summarize(mean_posterior_belief = mean(prob),
            speaker_ci_low = ci.low(prob),
            speaker_ci_high = ci.high(prob)) %>%
  ungroup() %>%
  mutate(speaker_YMin = mean_posterior_belief - speaker_ci_low,
         speaker_YMax = mean_posterior_belief + speaker_ci_high,
         type="prediction")

mean_empirical_sp <- df_collapsed %>%
  group_by(predicate, polarity) %>%  # collapse prior condition
  summarize(mean_posterior_belief = mean(prob),
            speaker_ci_low = ci.low(prob),
            speaker_ci_high = ci.high(prob)) %>%
  ungroup() %>%
  mutate(speaker_YMin = mean_posterior_belief - speaker_ci_low,
         speaker_YMax = mean_posterior_belief + speaker_ci_high,
         type="observation")

mean_sp <- rbind(mean_empirical_sp, mean_predicted_sp) %>% 
  mutate(predicate=fct_relevel(predicate, "Polar", "think","know"),
         polarity=fct_relevel(polarity, "Polar", "Embedded: p", "Embedded: not p"))


ggplot(combine_sp,aes(x=prior_mean,y=prob, color=predicate)) +
  geom_point(aes(shape=type),alpha=0.6) +
  geom_smooth(aes(linetype=type),method="lm", fullrange=TRUE) +
  geom_point(data=mean_sp,
             aes(shape=type,
                 x=ifelse(polarity=="Embedded: not p", combined_neg_prior, combined_prior),
                 y=mean_posterior_belief,
                 fill=predicate),size=2,color="black",stroke=1) +
  geom_errorbar(data=mean_sp,
                aes(linetype=type,
                    x=ifelse(polarity=="Embedded: not p", combined_neg_prior, combined_prior),
                    y=mean_posterior_belief,
                    ymin=speaker_YMin, ymax=speaker_YMax),
                width=0.05,
                color="black") +
  facet_grid(.~polarity) +
  scale_color_manual(values=c("#56B4E9", "#009E73", "#F0E442")) +
  scale_fill_manual(values=c("#56B4E9", "#009E73", "#F0E442")) +
  scale_linetype()+
  scale_shape()+
  theme_bw()+
  scale_y_continuous(limits = c(0,1),
                     breaks=seq(0,1,by=.25),
                     name = "Posterior speaker belief\nin the embedded content") +
  scale_x_continuous(breaks=seq(0,1,by=0.2),
                     name = "Rating of prior belief in the embedded content") 
ggsave("./graphs/threshold_cg/PL_empirical-predicted-smallTest.pdf", width=8,height=3)

