## ---------------------------
## Script name: tract_rankings_division_cut_strategy.R
##
## Author:Sam Powers
## Date Created: 2021-03-18
##
## ---------------------------
## Purpose of script: To make a tract ranking metric
##   
##
## ---------------------------
## set working directory

setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/brhd-vaccines/assignment1_tractrankings")

## ---------------------------
## load up the packages we will need:

library(tidyverse)
library(extrafont)
library(psych)
library(sf)
loadfonts() ## Load in the fonts I want to use

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
select <- dplyr::select

## ---------------------------
## Implementation Plan

# Try a division cut strategy with distribution of identity in given tract out of th ecounty distribution 
# think of it as a deviance score above and below expected amoung of that identity

## ---------------------------
## read in data:

tract_dimensions <- read_csv("../data/tract_dimensions.csv")

describe(tract_dimensions)
tract_dimensions$indigE




# Get the outcomes data back in -------------------------------------------

prob_atleast_one <-
  tract_dimensions %>%
  select(GEOID, contains("outcome")) %>%
  mutate(across(contains("outcome"), ~1 - .x/100)) %>% # Probability of not having the comorbidity
  gather(outcome, prob_not, -GEOID) %>%
  group_by(GEOID) %>% 
  summarize(prob_none = prod(prob_not)) %>%            # Product of independent probabilities = prob(no comorbidities)
  mutate(perc_atleast_one = (1 - prob_none) *100)      # 1 - P(none) = p(1+)





# change percents into actual numbers -----------------------------------
perc_factors <-
c(names(tract_dimensions)[c(4:14, 16:23)], "perc_atleast_one")

revised_tract_dims <-
tract_dimensions %>%
  left_join(prob_atleast_one)  %>%
  mutate(across(perc_factors, ~totalpopE*.x/100)) %>%
  mutate(across(perc_factors, ~(.x/sum(.x)))*100 - (100/n())
         ) %>%
  select(-lifeexpCDC)

tract_indicators <- 
  revised_tract_dims %>%
  select(GEOID,
         blackE,
          indigE,  
         ltnxE,
         povrateE,
         lifeexpBRHD,
         health_outcomes = perc_atleast_one,
         pov65E
  ) %>%
  mutate(across(-GEOID,
                ~ (.x - min(.x)) / (max(.x) - min(.x)))) %>%
  mutate(lifeexpBRHD = 1 - lifeexpBRHD)


alpha(tract_indicators[,-1], check.keys = TRUE)
principal(tract_indicators[,-1], nfactors=1, rotate="none", scores=TRUE)

fa(tract_indicators[,-1], nfactors=1, rotate="promax", fm="ml", SMC=TRUE) # It is not a 1 factor solution anymore
fa(tract_indicators[,-1], nfactors=2, rotate="promax", fm="wls", SMC=TRUE) # two factors # this goes heywood too1! WHAT


final_rankings_pct_from <- 
  tract_indicators %>%
  mutate(overall = blackE + ltnxE + indigE + povrateE + lifeexpBRHD + health_outcomes + pov65E) %>%
  arrange(
    desc(overall)
  ) %>%
  mutate(rank_outcomes = 1:n())  


final_rankings_pct_from




# you need the things from the other script -------------------------------
compare_ranks <- 
  final_rankings_agepov %>%
  select(GEOID, pct_of = rank_outcomes) %>%
  left_join(
    final_rankings_pct_from %>%
      select(GEOID, pct_from = rank_outcomes)
  )


compare_ranks %>%
  left_join(tract_dimensions) %>%
  View()





















