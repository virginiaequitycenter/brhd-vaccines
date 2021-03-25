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
tract_dimensions %>%
  left_join(prob_atleast_one)























