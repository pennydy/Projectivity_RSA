library(tidyverse)

theme_set(theme_bw())
# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 

# get the data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()
source("../../../helpers.R")

df.data <- read.csv("../../../../data/1_listenerProjection/main/1_listenerProjection_main-trials.csv", header=TRUE)

# exclude bot check and select only the relevant columns
df.data.clean <- df.data |>
  filter(slide_number_in_experiment != 1) |>
  select(workerid, belief_response, certainty_response, content, prior, trigger, trigger_class)

# calculate the ratings for the alternative content and specify the belief
# of the certainty rating
df.data.all <- df.data.clean |>
  mutate(utterance_type = gsub("[a-z]+_", "\\1", trigger)) |> # either p (pos) or not p (neg) 
  mutate(predicate = gsub("(.*)_[a-z]+", "\\1", trigger)) |>
  mutate(not_p = ifelse(utterance_type == "neg", 
                        belief_response,
                        1 - belief_response),
         p = ifelse(utterance_type == "neg",
                    1 - belief_response,
                    belief_response)) |>
  mutate(certainty_content = case_when(
    belief_response > 0.5 & utterance_type == "neg" ~ "not_p",
    belief_response > 0.5 & utterance_type != "neg" ~ "p",
    belief_response <= 0.5 & utterance_type == "neg" ~ "p",
    belief_response <= 0.5 & utterance_type != "neg" ~ "not_p")) |>
  pivot_longer(cols = c(p, not_p),
               names_to = "belief_content",
               values_to = "belief_rating") |>
  # to reorder the predicate and utterance_type for graph
  mutate(predicate = fct_relevel(predicate, "MC","simple","think","know","say","confirm"),
         utterance_type = fct_relevel(utterance_type, "MC","polar","pos", "neg"))

## Data exclusion

group_mc_mean <- mean(df.data.all$belief_response[df.data.all$trigger == "MC"])
group_mc_sd <- sd(df.data.all$belief_response[df.data.all$trigger == "MC"])
exclusion_criteria <- group_mc_mean + 2*group_mc_sd
excludeid <- df.data.all |>
  filter(trigger == "MC") |>
  group_by(workerid) |>
  summarise(mean_belief_response = mean(belief_response)) |>
  mutate(exclude = ifelse(mean_belief_response >= exclusion_criteria, "yes", "no")) |>
  filter(exclude == "yes") 

df.data.summary <- df.data.all |>
  filter(!workerid %in% excludeid$workerid)

# additional helper functions/lists for graphs
dodge = position_dodge(.9)

utterance_label <- list("MC"="Control",
                        "polar"="Polar",
                        "pos"="Embedded: p",
                        "neg"="Embedded: not p")
utterance_labeller <- function(variable,value){
  return(utterance_label[value])
}

predicate_label <- list("MC"="Control",
                        "simple"="Polar",
                        "think"="think",
                        "know"="know",
                        "say"="say",
                        "confirm"="confirm",
                        "inform"="inform")
predicate_labeller <- function(variable,value){
  return(predicate_label[value])
}

content_label <- list("not_p"="Belief content: not p",
                      "p"="Belief content: p")
content_labeller <- function(variable,value){
  return(content_label[value])
}

## BELIEF RATINGS
# summary of belief ratings
belief_summary <- df.data.summary |>
  group_by(predicate, belief_content, utterance_type) |>
  summarize(mean_belief_rating = mean(belief_rating),
            ci_low = ci.low(belief_rating), # confidence interval
            ci_high = ci.high(belief_rating)) |> 
  ungroup() |>
  mutate(YMin = mean_belief_rating - ci_low,
         YMax = mean_belief_rating + ci_high)
print(belief_summary)
write.csv(belief_summary, "../results/belief_summary.csv" , row.names = FALSE)


# by the type of embedded clause (pos, neg, or simple polar)
belief_by_p <- ggplot(data = belief_summary |>
                        filter(belief_content == "p"),
                      mapping = aes(x = predicate,
                                    y = mean_belief_rating,
                                    fill = predicate)) +
  geom_bar(stat="identity",
           position=dodge) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=dodge,
                width=.2) +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  facet_grid(. ~ utterance_type,
             labeller = utterance_labeller, scales="free_x") +
  labs(x = "Predicate",
       y = "Mean belief in p",
       fill = "Predicate") +
  scale_fill_manual(values=cbPalette,
                    labels=c("Control", "Polar", "think", "know", "say", "confirm", "inform")) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank())
belief_by_p
ggsave(belief_by_p, file="../graphs/belief_by_p.pdf")

# by predicate
belief_by_predicate <- ggplot(data = belief_summary |>
                        filter(belief_content == "p") |>
                        mutate(utterance_type = ifelse(as.character(utterance_type) %in% c("MC","polar"), "pos", as.character(utterance_type))),
                      mapping = aes(x = belief_content,
                                    y = mean_belief_rating,
                                    alpha = utterance_type,
                                    fill = predicate)) +
  geom_bar(stat="identity",
           position=dodge,
           color="black") +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=dodge,
                width=.2,
                color="black") +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  labs(y = "Mean belief in p") +
  scale_alpha_discrete(range=c(.3,.9), # include both p and not p (more opaque). 
                       labels=c("not p", "p"),
                       name="Embedded\ncontent") +
  scale_fill_manual(values=cbPalette,
                    labels=c("Control", "Polar", "think", "know", "say", "confirm", "inform"),
                    guide="none") +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank()) 
belief_by_predicate
ggsave(belief_by_predicate, file="../graphs/belief_by_predicate.pdf")


## CERTAINTY RATING
# certainty ratings (added by judith)
certainty_summary <- df.data.summary |>
  # penny, in the following line we're grouping by certainty_content -- this only makes sense if we first exclude all rows where the belief value was inferred instead of given, so we only retain the rows where participants actually gave a certainty rating for the content of that row. eg, if utterance was "pos", belief response > .5, we want to keep the row where certainty_content is "p", but not where it's "not p". similarly, if utterance was "neg", belief response < .5,  we only want to keep the row where certainty_content is "not p", etc.
  # that makes sense. The certainty_content column only keeps track of the content to which the participant gave a certainty rating. So for a given trail, the two belief_contents (one given, one inferred) map a to the same certainty_content. To get the belief rating for that content, we can just include rows that have the same certainty_content and belief_content value.
  filter(certainty_content == belief_content) |>
  group_by(predicate, certainty_content, utterance_type) |>
  summarize(mean_certainty_rating = mean(certainty_response),
            ci_low=ci.low(certainty_response),
            ci_high=ci.high(certainty_response),
            count = n()) |>
  ungroup() |>
  mutate(YMin = mean_certainty_rating - ci_low,
         YMax = mean_certainty_rating + ci_high) |>
  relocate(count, .after = utterance_type)
print(certainty_summary)
write.csv(certainty_summary, "../results/certainty_summary.csv" , row.names = FALSE)


# by the type of embedded clause (pos, neg, or simple polar)
certainty_by_p <- ggplot(data = certainty_summary,
                      mapping = aes(x = certainty_content,
                                    y = mean_certainty_rating,
                                    fill = predicate)) +
  geom_bar(stat="identity", 
           position=dodge) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=dodge,
                width=.2) +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") + 
  facet_grid(. ~ utterance_type,
             labeller = utterance_labeller, scales="free_x") +
  labs(x = "Rated content",
       y = "Mean certainty of the rated content",
       fill = "Predicate") +
  # scale_alpha_discrete(range=c(.3,.9),name="Embedded\ncontent") +
  scale_fill_manual(values=cbPalette,
                    labels=c("Control", "Polar", "think", "know", "say", "confirm", "inform")) # add guide="none" later
certainty_by_p
ggsave(certainty_by_p, file="../graphs/certainty_by_p.pdf")



# by predicate
certainty_by_predicate <- ggplot(data = certainty_summary |>
                                   # filter(!utterance_type %in% c("MC")) |>
                                   mutate(utterance_type = ifelse(as.character(utterance_type) %in% c("MC","polar"), "pos", as.character(utterance_type))),
                                 mapping = aes(x = certainty_content,
                                               y = mean_certainty_rating,
                                               alpha = utterance_type,
                                               color = predicate)) +
  geom_point(aes(size=count),
             stat="identity",
             position=dodge,
             show.legend = FALSE) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                position=dodge,
                width=.2,
                color="black") +
  geom_hline(yintercept = 0.5,
             alpha = 0.7,
             color = "grey",
             linetype = "dashed") +
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  scale_alpha_discrete(range=c(.3,.8), # include both p and not p (more opaque). 
                       labels=c("not p", "p"),
                       name="Embedded\ncontent") +
  scale_color_manual(values=cbPalette,
                    labels=c("Control", "Polar", "think", "know", "say", "confirm", "inform"),
                    guide="none") +
  labs(x = "Rated content",
       y = "Mean certainty of the rated content") +
  scale_y_continuous(limits = c(0,1))
certainty_by_predicate
ggsave(certainty_by_predicate, file="../graphs/certainty_by_predicate.pdf")


overall_certainty_belief_summary <- df.data.summary |>
  filter(certainty_content == belief_content) |>
  group_by(predicate) |>
  summarize(mean_certainty_rating = mean(certainty_response),
            mean_belief_rating = mean(belief_rating),
            count = n()) |>
  ungroup()
print(overall_certainty_belief_summary)
write.csv(overall_certainty_belief_summary, "../results/overall_certainty_belief_summary.csv" , row.names = FALSE)
overall_belief_certainty <- ggplot(data = overall_certainty_belief_summary,
                                   mapping = aes(x = mean_belief_rating,
                                                 y = mean_certainty_rating,
                                                 color = predicate)) +
  geom_point(size=4) +
  scale_color_manual(values=cbPalette,
                     labels=c("Control", "Polar", "think", "know", "say", "confirm", "inform")) + 
  labs(x = "Mean belief of the content",
       y = "Mean certainty of the rated content") 
overall_belief_certainty
ggsave(overall_belief_certainty, file="../graphs/overall_belief_certainty.pdf")



certainty_belief_summary <- df.data.summary |>
  filter(certainty_content == belief_content) |>
  group_by(predicate, utterance_type, certainty_content) |>
  summarize(mean_certainty_rating = mean(certainty_response),
            mean_belief_rating = mean(belief_rating),
            count = n()) |>
  ungroup()
print(certainty_belief_summary)
write.csv(certainty_belief_summary, "../results/certainty_belief_summary.csv" , row.names = FALSE)


belief_certainty <- ggplot(data = certainty_belief_summary |>
                             mutate(utterance_type = ifelse(as.character(utterance_type) %in% c("MC","polar"), "pos", as.character(utterance_type))),
                           mapping = aes(x = mean_belief_rating,
                                         y = mean_certainty_rating,
                                         alpha = utterance_type,
                                         color = predicate)) +
  geom_point(aes(size=count)) +
  scale_alpha_discrete(range=c(.3,.8), # include both p and not p (more opaque).
                       labels=c("not p", "p"),
                       name="Embedded\ncontent") +
  facet_grid(. ~ certainty_content,
             labeller = content_labeller) +
  scale_color_manual(values=cbPalette,
                     labels=c("Control", "Polar", "think", "know", "say", "confirm", "inform")) + 
  scale_size(guide = "none") + 
  labs(x = "Mean belief of the content",
       y = "Mean certainty of the rated content")
belief_certainty
ggsave(belief_certainty, file="../graphs/belief_certainty.pdf")


