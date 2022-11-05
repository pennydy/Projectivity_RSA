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
  # filter(trigger_class == "Critical") |>
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
               values_to = "belief_rating")

# additional helper functions/lists for graphs
utterance_label <- list("MC"="Control","neg"="not p", "polar"="Polar", "pos"="p")
utterance_labeller <- function(variable,value){
  return(utterance_label[value])
}

predicate_label <- list("confirm"="confirm", "inform"="inform", 
                        "know"="know", "MC"="Control",
                        "say"="say", "simple"="Polar", "think"="think")
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
belief_by_p <- ggplot(data = df.data.summary,
       mapping = aes(x = belief_content,
                     y = belief_rating,
                     fill = predicate)) +
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
  scale_fill_discrete(labels=c("confirm", "inform", "know", "Control", "say", "Polar", "think"))
belief_by_p
ggsave(belief_by_p, file="../../graphs/belief_by_p.pdf")

# by predicate
belief_by_predicate <- ggplot(data = df.data.summary,
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
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  labs(x = "Belief Content",
       y = "Belief Rating",
       fill = "Embedded \n Content") +
  scale_fill_discrete(labels=c("Control", "not p", "Polar", "p"))
belief_by_predicate
ggsave(belief_by_predicate, file="../../graphs/belief_by_predicate.pdf")

## CERTAINTY RATING
# summary of certainty
certainty_summary <- df.data.summary |>
  filter(belief_rating >= 0.5) |>
  distinct(content, workerid, .keep_all=TRUE) |>
  group_by(trigger, certainty_content) |>
  summarize(certainty_rating_mean = mean(certainty_response),
            beleif_rating_mean = mean(belief_rating),
            count = n())
print(certainty_summary, n=72)


# certainty graph
# by the type of embedded content (pos, neg, or simple polar)
certainty_by_p <- ggplot(data = df.data.summary |>
         filter(utterance_type != "MC") |>
         distinct(content, workerid, .keep_all=TRUE),
       mapping = aes(x = certainty_content,
                     y = certainty_response,
                     color = predicate)) +
  geom_point(alpha = 0.5, 
             position=position_jitter(height=0, width=.15)) +
  facet_grid(. ~ utterance_type,
             labeller = utterance_labeller) +
  labs(x = "Certainty Content",
       y = "Certainty Rating",
       color = "Predicate") +
  scale_fill_discrete(labels=c("confirm", "inform", "know", "say", "Polar", "think"))
certainty_by_p
ggsave(certainty_by_p, file="../../graphs/certainty_by_p.pdf")

# by predicate 
certainty_by_predicate <- ggplot(data = df.data.summary |>
         filter(!utterance_type %in% c("MC")) |>
         distinct(content, workerid, .keep_all=TRUE) |>
         mutate(utt_belief_comp = case_when(
           utterance_type == "polar" ~ "polar",
           utterance_type == "neg" & certainty_content == "not_p" ~ "same", 
           utterance_type == "pos" & certainty_content == "p" ~ "same",
           TRUE ~ "different")),
       mapping = aes(x = certainty_content,
                     y = certainty_response,
                     color = utt_belief_comp)) +
  geom_point(alpha = 0.5, 
             position=position_jitter(height=0, width=.2)) +
  facet_grid(. ~ predicate,
             labeller = predicate_labeller) +
  labs(x = "Certainty Content",
       y = "Certainty Rating",
       color = "Accordence between \n utterance and \n belief")
certainty_by_predicate
ggsave(certainty_by_predicate, file="../../graphs/certainty_by_predicate.pdf")

# # relationship between certainty and belief
# ggplot(data = df.data.summary,
#        mapping = aes(x = belief_rating,
#                      y = certaitny_rating))
  