library("tidyverse")

theme_set(theme_classic())

# get the data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../data/pilot")
df.data <- read.csv("1_listenerProjection-trials.csv", header=TRUE)

# exclude bot check and select only the relevant columns
df.data.clean <- df.data |>
  filter(slide_number_in_experiment != 1) |>
  select(workerid, belief_response, certainty_response, content, prior, trigger, trigger_class)

# calculate the ratings for the alternative content and specify the belief
# of the certainty rating
df.data.summary <- df.data.clean |>
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

# summarize the data before visualization
# judith, can you help me here? thanks!



# for the certainty analysis -> don't think this is needed anymore:
# this is a dataframe w/ a column of whether the inferred belief (p in the certainty
# question) is the same as the embedded clause in the utterance.
# The belief response is the belief rating for the inferred belief(either the belief 
# rating, or 1-the belief rating, if the belief rating is < 0.5 for the embedded content)
df.data.certainty <- df.data.summary |>
  mutate(utt_belief_comp = case_when(
    utterance_type == "polar" ~ "polar",
    utterance_type == "neg" & certainty_content == "not_p" ~ "same", 
    utterance_type == "pos" & certainty_content == "p" ~ "same",
    TRUE ~ "different")) |>
  filter(certainty_content == belief_content)


# additional helper functions/lists for graphs
utterance_label <- list("MC"="Control","polar"="Polar","pos"="p","neg"="not p")
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

## BELIEF RATINGS
# summary of belief ratings
belief_summary <- df.data.summary |>
  group_by(trigger, belief_content) |>
  summarize(beleif_rating_mean = mean(belief_rating))
print(belief_summary)

# by the type of embedded clause (pos, neg, or simple polar)
belief_by_p <- ggplot(data = df.data.summary |>
                        filter(belief_content == "p"),
       mapping = aes(x = belief_content,
                     y = belief_rating,
                     fill = predicate)) +
  geom_hline(yintercept = 0.5,
             alpha = 0.5,
             color = "grey",
             linetype = "dashed") + 
  stat_summary(fun = "mean",
               geom = "bar",
               position = position_dodge2(width=0.8, preserve = "total")) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = "linerange",
               position = position_dodge(0.9),
               color = "black") +
  facet_grid(. ~ utterance_type,
             labeller = utterance_labeller) +
  labs(x = "Belief Content",
       y = "Belief Rating",
       fill = "Predicate") +
  scale_fill_discrete(labels=c("Control", "Polar", "think", "know", "say", "confirm", "inform"))
belief_by_p
ggsave(belief_by_p, file="../../graphs/pilot/belief_by_p.pdf")

# by predicate
belief_by_predicate <- ggplot(data = df.data.summary |>
                                filter(belief_content == "p") |>
                                mutate(utterance_type = ifelse(as.character(utterance_type) %in% c("MC","polar"), "pos", as.character(utterance_type))),
       mapping = aes(x = belief_content,
                     y = belief_rating,
                     fill = utterance_type)) +
  geom_hline(yintercept = 0.5,
             alpha = 0.5,
             color = "grey",
             linetype = "dashed") + 
  stat_summary(fun = "mean",
               geom = "bar",
               position = position_dodge2(width=0.8, preserve = "total")) +
  stat_summary(fun.data = "mean_cl_boot",
               geom = "linerange",
               position = position_dodge(0.9),
               color = "black") +
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  labs(x = "Belief Content",
       y = "Belief Rating",
       fill = "Embedded \nContent \nin the \nutterance") +
  scale_fill_discrete(labels=c("not p", "p"))
belief_by_predicate
ggsave(belief_by_predicate, file="../../graphs/pilot/belief_by_predicate.pdf")


# create one graph for the certainty rating of each predicate
individual_predicate_belief <- function(p) {
  graph <- ggplot(data = df.data.summary |>
                    filter(predicate == p),
                  mapping = aes(x = belief_content,
                                y = belief_rating,
                                fill = utterance_type)) +
    stat_summary(fun = "mean",
                 geom = "bar",
                 position = position_dodge2(width=0.8, preserve = "total")) +
    stat_summary(fun.data = "mean_cl_boot",
                 geom = "linerange",
                 position = position_dodge(0.9),
                 color = "black") +
    labs(x = "Belief Content",
         y = "Belief Rating",
         fill = "Embedded \n Content") +
    scale_fill_discrete(labels=c("Control", "not p", "Polar", "p"))
  return(graph)
}
for (predicate in c("know", "think", "say", "inform", "confirm", "simple")) {
  graph <- individual_predicate_belief(predicate)
  file_name <- paste("../../graphs/pilot/belief_", predicate, ".pdf", sep="")
  ggsave(graph, file=file_name)
}


## CERTAINTY RATING
# summary of certainty
certainty_summary <- df.data.certainty |>
  group_by(trigger, certainty_content) |>
  summarize(certainty_rating_mean = mean(certainty_response),
            beleif_rating_mean = mean(belief_rating),
            count = n())
print(certainty_summary, n=72)


# certainty graph
# by the type of embedded content (pos, neg, or simple polar)
certainty_by_p <- ggplot(data = df.data.certainty |>
                           filter(!utterance_type %in% c("MC")),
       mapping = aes(x = certainty_content,
                     y = certainty_response,
                     color = predicate)) +
  stat_summary(fun.data = "mean", 
               geom = "point") +
  facet_grid(. ~ utterance_type,
             labeller = utterance_labeller) +
  labs(x = "Certainty Content",
       y = "Certainty Rating",
       color = "Predicate") +
  scale_fill_discrete(labels=c("confirm", "inform", "know", "say", "Polar", "think"))
certainty_by_p
ggsave(certainty_by_p, file="../../graphs/pilot/certainty_by_p.pdf")

# by predicate 
certainty_by_predicate <- ggplot(data = df.data.certainty |>
                                   filter(!utterance_type %in% c("MC")),
       mapping = aes(x = certainty_content,
                     y = certainty_response,
                     color = utt_belief_comp)) +
  geom_point(alpha = 0.5, 
             position=position_jitter(width = 0.1)) +
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  labs(x = "Certainty Content",
       y = "Certainty Rating",
       color = "Accordence between \n utterance and \n belief")
certainty_by_predicate
ggsave(certainty_by_predicate, file="../../graphs/pilot/certainty_by_predicate.pdf")

# create one graph for the certainty rating of each predicate
individual_predicate_certainty <- function(p){
  graph <- ggplot(data = df.data.certainty |>
           filter(predicate == p),
         mapping = aes(x = certainty_content,
                       y = certainty_response,
                       color = utt_belief_comp)) +
    geom_point(alpha = 0.5,
               position = position_jitterdodge(jitter.width = 0.1,
                                               dodge.width = 0.8)) +
    stat_summary(aes(color = utt_belief_comp),
                 fun.data="mean_cl_boot",
                 geom = "pointrange",
                 size = 0.4,
                 position = position_dodge(0.8)) + 
    labs(x = "Certainty Content",
         y = "Certainty Rating",
         color = "Accordence between \n utterance and \n belief") 
  return(graph)
}  

for (predicate in c("know", "think", "say", "inform", "confirm", "simple")) {
  graph <- individual_predicate_certainty(predicate)
  file_name <- paste("../../graphs/pilot/certainty_", predicate, ".pdf", sep="")
  ggsave(graph, file=file_name)
}


# # relationship between certainty and belief
# individual_trigger_belief_certainty <- function(t) {
#   graph <- ggplot(data = df.data.certainty |>
#                     filter(trigger == t),
#                   mapping = aes(x = belief_rating,
#                                 y = certainty_rating,
#                                 color = trigger)) +
#     geom_point() +
#     geom_smooth(method = "lm", se = TRUE)
#   return(graph)
# }
# for (trigger in c("know_pos", "think_pos", "say_pos", "inform_pos", "confirm_pos", 
#                   "know_neg", "think_neg", "say_neg", "inform_neg", "confirm_neg",
#                   "simple")) {
#   graph <- individual_trigger_belief_certainty(trigger)
#   file_name <- paste("../../graphs/pilot/belief_certainty_", trigger, ".pdf", sep="")
#   ggsave(graph, file=file_name)
# }

belief_certainty <- ggplot(data = df.data.certainty |>
         group_by(trigger) |>
           summarize(mean_belief_rating = mean(belief_rating),
                     mean_certainty_rating = mean(certainty_response)),
       mapping = aes(x = mean_belief_rating,
                     y = mean_certainty_rating,
                     color = trigger)) +
  geom_point()

belief_certainty
ggsave(belief_certainty, file="../../graphs/pilot/belief_certainty.pdf")
