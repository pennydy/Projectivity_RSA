library(tidyverse)
library(lme4)
library(rwebppl)
#library(ggpattern)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../helpers.R")

# Helper functions for the graph
theme_set(theme_bw())
# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 

# read in model
m_qud <- read_file("../../../models/threshold_qud.wppl")

# evaluate the models
eval_webppl <- function(command) {
  webppl(paste(m_qud,command,sep="\n"))
}
# view(eval_webppl)

# prepare the plots ----

# define the utterances
utterances = c("know-dances-?", 
               "know-doesnt_dance-?",
               # "know-dances-.", 
               # "be_right-dances-?", 
               # "be_right-dances-.",                   
               "think-dances-?", 
               "think-doesnt_dance-?",
               # "think-dances-.",                         
               "BARE-dances-?"
               # "BARE-dances-."
               )

utterances

# define the QUD 
quds = c("MC", "CC")

# define the beliefs
#beliefs = c("dances","not dances")

# define beliefs
speaker_beliefs = seq(0,9,by=1)

# define priors
priors = c("lowest","low","mid","high","highest") 

# convert priors to means based on input to webppl model
# var priors = {
#   lowest: [.3,.3,.3,.04,.01,.01,.01,.01,.01,.01],
#   low: [.01,.01,.3,.3,.3,.04,.01,.01,.01,.01],
#   mid: [.01,.01,.02,.02,.4,.4,.02,.02,.01,.01],  
#   high: [.01,.01,.01,.01,.04,.3,.3,.3,.01,.01],
#   highest: [.01,.01,.01,.01,.01,.01,.04,.3,.3,.3]  
# }

prior_means = data.frame(states = seq(1,10,by=1), lowest = c(.3,.3,.3,.04,.01,.01,.01,.01,.01,.01),
  low = c(.01,.01,.3,.3,.3,.04,.01,.01,.01,.01),
  mid = c(.01,.01,.02,.02,.4,.4,.02,.02,.01,.01),  
  high = c(.01,.01,.01,.01,.04,.3,.3,.3,.01,.01),
  highest = c(.01,.01,.01,.01,.01,.01,.04,.3,.3,.3))
prior_means = prior_means %>% 
  mutate(lowest = lowest/sum(lowest),low = low/sum(low), mid = mid/sum(mid), high = high/sum(high), highest = highest/sum(highest)) %>% 
  summarize(lowest = mean(lowest*states),
            low = mean(low*states),
            mid = mean(mid*states),
            high = mean(high*states),
            highest = mean(highest*states)) %>% 
  pivot_longer(lowest:highest,names_to="prior",values_to="prior_mean")

# literal listener ----

LL = data.frame(utterance = character(), qud = character(), speaker_belief = numeric(), prob = numeric())

for (u in utterances) {
  for (q in quds) {
    LL_tmp = eval_webppl(paste("literalListener('",u,"','",q,"')",sep="")) %>% 
      group_by(value) %>% 
      summarize(mean_score=mean(score)) %>% 
      mutate(sum_means = sum(mean_score)) %>% 
      mutate(prob=mean_score/sum_means) %>% 
      select(value,prob)
    
    for (i in 1:nrow(LL_tmp)) {
      LL = LL %>% 
        add_row(utterance = u, qud = q, speaker_belief = LL_tmp$value[i], prob = LL_tmp$prob[i])
    }
  }
}

# make bins go from .1 - 1
# .1 means "belief in p is < .1"
# .2 means "belief in p is >= .1 and < .1"
# ...
# 1 means "belief in p is >= .9 and <= 1"
LL$speaker_belief = (LL$speaker_belief + 1)/10

ggplot(LL, aes(x=speaker_belief, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(qud~utterance, scales = "free_y") +
# scale_fill_manual(values=cbPalette)
  scale_x_continuous(breaks=seq(0,1,by=.1))
ggsave("../graphs/threshold_qud/threshold_qud-LL.pdf",width=12,height=5)

#  pragmatic speaker -----

PS = data.frame(qud = character(), speaker_belief = numeric(), utterance = character(), prob = numeric())

for (sp_belief in speaker_beliefs) {
  for (q in quds) {
    PS_tmp = eval_webppl(paste("speaker(",sp_belief,",'",q,"')",sep=""))
    for (i in 1:nrow(PS_tmp)) {
      PS = PS %>% 
        add_row(qud = q, speaker_belief = sp_belief, utterance = PS_tmp$support[i], prob = PS_tmp$prob[i])
    }
  }  
}

# make bins go from .1 - 1
# .1 means "belief in p is < .1"
# .2 means "belief in p is >= .1 and < .1"
# ...
# 1 means "belief in p is >= .9 and <= 1"
PS$speaker_belief = (PS$speaker_belief + 1)/10

ggplot(PS, aes(x=speaker_belief, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(qud~utterance) +
  scale_x_continuous(breaks=seq(0,1,by=.1))
  # coord_flip()
ggsave("../graphs/threshold_qud/threshold_qud-PS-byutterance.pdf",width=12,height=5)

ggplot(PS, aes(x=utterance, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(qud~speaker_belief) +
  coord_flip()
  # scale_x_continuous(breaks=seq(0,1,by=.1))
# coord_flip()
ggsave("../graphs/threshold_qud/threshold_qud-PS-bybelief.pdf",width=12,height=3)



# pragmatic listener ----

# currently implemented as just returning beliefs as a function of utterance and qud, but ideally would be a joint inference model
# also inferring qud. not implemented for purely technical boring reasons: 
# rwebppl can't parse joint inference webppl output)

PL = data.frame(utterance = character(), qud = character(), prior = character(), speaker_belief = numeric(), prob = numeric())

for (u in utterances) {
  for (q in quds) {  
    for (pr in priors) {
      PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",q,"','",pr,"')",sep=""))
      # PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",pr,"')",sep=""))      
      for (i in 1:nrow(PL_tmp)) {
        PL = PL %>% 
          add_row(utterance = u, qud = q, prior = pr, speaker_belief = PL_tmp$support[i], prob = PL_tmp$prob[i])
      }
    }
  }
}

PL = PL %>% 
  left_join(prior_means, by=c("prior"))


# make bins go from .1 - 1
# .1 means "belief in p is < .1"
# .2 means "belief in p is >= .1 and < .1"
# ...
# 1 means "belief in p is >= .9 and <= 1"
PL$speaker_belief = (PL$speaker_belief + 1)/10

# plot mid prior results
ggplot(PL[PL$prior == "mid",], aes(x=speaker_belief, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(qud~utterance) +
  # scale_fill_manual(values=cbPalette)
  scale_x_continuous(breaks=seq(0,1,by=.1))
ggsave("../graphs/threshold_qud/threshold_qud-PL.pdf",width=12,height=5)


# 

agr = PL %>% 
  group_by(utterance,qud,prior_mean) %>% 
  summarize(posterior_belief_p = sum(prob*speaker_belief)) %>% 
  mutate(polarity = case_when(str_detect(utterance, "doesnt") ~ "neg",
                               TRUE ~ "pos"),
         predicate = case_when(str_detect(utterance, "think") ~ "think",
                               str_detect(utterance, "know") ~ "know",
                               str_detect(utterance, "BARE") ~ "BARE",
                               TRUE ~ "ERROR")) %>% 
  mutate(posterior_belief_emb = case_when(polarity == "neg" ~ 1 - posterior_belief_p,
                                          TRUE ~ posterior_belief_p))

ggplot(agr, aes(x=prior_mean,y=posterior_belief_emb,color=predicate)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_grid(qud~polarity) +
  scale_color_manual(values=cbPalette)





