## ---------------------------
## Script name: tract_rankings.R
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
loadfonts() ## Load in the fonts I want to use

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
select <- dplyr::select

## ---------------------------
## Implementation Plan

# 1. Generate an index and ranking using just the poverty, race as defined above (with equity atlas code), and 
# life expectancy using BRHD values (not sure that these are programmatically downloadable, though)

# 2. But then again using the more recent USASLEEP values -- just to make sure it doesn't make a marked difference -- if not, use 1

# 3. And then compare this with a version that incorporates additional health prevalence estimates -- here it will be important to not overweight these -- 
# so in my head, these are first combined and then included with life expectancy, race, and poverty.

# If there's, again, no marked difference, I'd stick with 1 (and note that 3 was tried); if there is a marked difference, I'd report both.

## ---------------------------
## read in data:

tract_dimensions <- read_csv("../data/tract_dimensions.csv")

describe(tract_dimensions)
tract_dimensions$indigE

# Step 1: Poverty,  Race,  BRHD LE ------------------------------------------------
step1_dat <- 
tract_dimensions %>%
  select(blackE, indigE, ltnxE, povrateE, lifeexpBRHD)

step1_eig <- eigen(cor(step1_dat))$values
step1_eig/sum(step1_eig)

scree(step1_dat) ## there might be two factors in here
alpha(step1_dat, check.keys = TRUE)

fa(step1_dat, nfactors=1, rotate="promax", fm="ml", SMC=TRUE)
# Latinx shares little variance with other factors 
# The RMSEA is good though

fa(step1_dat, nfactors=2, rotate="promax", fm="ml", SMC=TRUE)
# this is clearly overfit. 


# Step 2: Poverty,  Race,  CDC LE -----------------------------------------
step2_dat <- 
  tract_dimensions %>%
  select(blackE, indigE, ltnxE, povrateE, lifeexpCDC) %>%
  filter(!is.na(lifeexpCDC))

step2_eig <- eigen(cor(step2_dat))$values
step2_eig/sum(step2_eig)

scree(step2_dat) ## there might be two factors in here
alpha(step2_dat, check.keys = TRUE)

fa(step2_dat, nfactors=1, rotate="promax", fm="ml", SMC=TRUE)
# Indigenous really is not providing us with much variance. 
# The RMSEA is fine though

fa(step2_dat, nfactors=2, rotate="promax", fm="ml", SMC=TRUE)
# this is clearly overfit. 

# I would say there is a solid argument that these are all part of one factor

# Step 3: Poverty,  Race,  BRHD LE,  Health Outcomes ----------------------

# look at health outcomes 
outcomes <- 
tract_dimensions %>%
  select(contains("outcome"))

outcome_eig <- eigen(cor(outcomes))$values
outcome_eig/sum(outcome_eig) # 71.8% of the variance is in the first component. This should be fine to load together

scree(outcomes) ## there might be two factors in here

# maybe make a principal component out of them 
pca.outcomes <-
principal(outcomes, nfactors=1, rotate="none", scores=TRUE) # This gets 72% of the variance. Cancer is not high, but its there. 

principal(outcomes, nfactors=2, rotate="none", scores=TRUE) # This is a decent bit of crossed loadings


# Maybe we assume independence between the diseases and look at probability of having at least one co-morbidity?
# We know they aren't independent within persons, but this is definitely conservative?

prob_atleast_one <-
tract_dimensions %>%
  select(GEOID, contains("outcome")) %>%
  mutate(across(contains("outcome"), ~1 - .x/100)) %>%
  gather(outcome, prob_not, -GEOID) %>%
  group_by(GEOID) %>%
  summarize(prob_none = prod(prob_not)) %>%
  mutate(perc_atleast_one = (1 - prob_none) *100) 
  
combine_outcomes <- 
tibble(
GEOID = 
tract_dimensions$GEOID,
pca = pca.outcomes$scores[,1]
) %>%
  left_join(prob_atleast_one)

# they describe very similar phenomena
cor(combine_outcomes$perc_atleast_one, combine_outcomes$pca,  method = "spearman") # 0.9482
normed_outcomes <-
combine_outcomes %>%
  mutate(
    across(c("pca", "perc_atleast_one"),
           ~(.x - min(.x))/ (max(.x) - min(.x))
           )
  ) %>%
  select(GEOID, pca, perc_atleast_one) 

cor(normed_outcomes$pca, normed_outcomes$perc_atleast_one)

normed_outcomes %>%
arrange(pca) 
# They generally move in the same direction but they do not rank counties in exactly the same way. 

var(normed_outcomes$pca)
mean(normed_outcomes$pca) # this is centered high

var(normed_outcomes$perc_atleast_one) # This has more variance
mean(normed_outcomes$perc_atleast_one) # This is centered pretty well

# Create the final normed metric ------------------------------------------
normed_metrics <- 
tract_dimensions %>%
  select(GEOID,
         blackE,
         # indigE,  removing indigenous
         ltnxE,
         povrateE,
         lifeexpBRHD  # this one needs to be reversed
         ) %>%
         mutate(across(-GEOID,
                       ~ (.x - min(.x)) / (max(.x) - min(.x)))) %>%
           mutate(lifeexpBRHD = 1 - lifeexpBRHD) %>%
  left_join(
    normed_outcomes %>%
      select(GEOID, 
             health_outcomes = perc_atleast_one
             )
  )


principal(normed_metrics[,-1], nfactors=2, rotate="none", scores=TRUE)
# there is definitely a black, life expectancy, & health outcomes dimension and then a latinx & poverty rate dimension that seem oblique. 
# But based on theory we want to weight those all the same, so I think we are probably good to go to just add them up together

final_rankings  <- 
normed_metrics %>%
  mutate(overall = blackE + ltnxE + povrateE + lifeexpBRHD + health_outcomes,
         overall_no_outcomes = blackE + ltnxE + povrateE + lifeexpBRHD) %>%
  arrange(
    desc(overall_no_outcomes)
  ) %>%
  mutate(rank_no_outcomes = 1:n()) %>%
  arrange(
    desc(overall)
  ) %>%
  mutate(rank_outcomes = 1:n())  

max(final_rankings$overall) # 3.34
min(final_rankings$overall) # 0.448
# not quite the variance we would hope for, but this works! 
(max(final_rankings$overall) - min(final_rankings$overall))/5  # uses 0.5784 of its possible range


max(final_rankings$overall_no_outcomes) # 2.603
min(final_rankings$overall_no_outcomes) # 0.07239
(max(final_rankings$overall_no_outcomes)  - min(final_rankings$overall_no_outcomes))/4 # uses 0.6328 of its possible range

cor(final_rankings$overall, final_rankings$overall_no_outcomes, method = "spearman") 
# They correlate at 0.9239


write_csv(final_rankings, path = "final_rankings.csv")



# Add in geometry for graphing purposes -----------------------------------

tract_geos <- get_acs(geography = "tract",
                      variables = "DP05_0001",
                      state = "VA", 
                      county = region,
                      survey = "acs5", 
                      year = 2019, 
                      output = "wide",
                      geometry = TRUE)

rankings_geo <- 
final_rankings %>%
   mutate(GEOID = as.character(GEOID)) %>%
  left_join(tract_geos) %>%
  st_as_sf()

save(rankings_geo, file = "geo_rankings.Rdata")






  














