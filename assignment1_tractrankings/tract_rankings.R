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
library(sf)
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
  filter(!is.na(lifeexpCDC)) # too impatint to try FIML. Going to use complete cases to develop

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

scree(outcomes) ## there might be two factors in here. also there are some weird pattens. Factor analysis is out. 
alpha(outcomes) ## This alpha is high. I think we definitely can condense these. 

# maybe make a principal component out of them 
pca.outcomes <-
principal(outcomes, nfactors=1, rotate="none", scores=TRUE) # This gets 72% of the variance. Cancer is not high, but its there. 
pca.outcomes

principal(outcomes, nfactors=2, rotate="none", scores=TRUE) # This is decent but there are crossed loadings [smoking & cancer]


# Maybe we assume independence between the diseases and look at probability of having at least one co-morbidity?
# We know they aren't independent within persons, but this is definitely conservative?

prob_atleast_one <-
tract_dimensions %>%
  select(GEOID, contains("outcome")) %>%
  mutate(across(contains("outcome"), ~1 - .x/100)) %>% # Probability of not having the comorbidity
  gather(outcome, prob_not, -GEOID) %>%
  group_by(GEOID) %>% 
  summarize(prob_none = prod(prob_not)) %>%            # Product of independent probabilities = prob(no comorbidities)
  mutate(perc_atleast_one = (1 - prob_none) *100)      # 1 - P(none) = p(1+)
  
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

cor(normed_outcomes$pca, normed_outcomes$perc_atleast_one, method = "spearman") # 0.9498

normed_outcomes %>%
arrange(pca) 
# They generally move in the same direction but they do not rank counties in exactly the same way. 
# There is a weird trend of decreasing perc_atleast_one until a point then it jumps and then decreases again 

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
# there is definitely a black, life expectancy, & health outcomes dimension and then a latinx & poverty rate w/ good health outcomes 
# dimension that seem perpendicular 
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
library(tidycensus)
library(sf)

ccodes <- read_csv("../data/county_codes.csv") %>%
  filter(
    grepl(
      "Charlottesville|Albemarle|Fluvanna|Greene|Louisa|Nelson",
      name
    )
  )

region <- ccodes$code

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




# Put Indigenous back into the Ranking ------------------------------------

normed_metrics <- 
  tract_dimensions %>%
  select(GEOID,
         blackE,
         indigE,  # put indigenous back in it 
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
# there is definitely a black, life expectancy, & health outcomes dimension and then a latinx & poverty rate w/ good health outcomes 
# dimension that seem perpendicular 
# But based on theory we want to weight those all the same, so I think we are probably good to go to just add them up together

final_rankings_indigenous  <- 
  normed_metrics %>%
  mutate(overall = blackE + ltnxE + indigE + povrateE + lifeexpBRHD + health_outcomes ,
         overall_no_outcomes = blackE + ltnxE + indigE+ povrateE + lifeexpBRHD) %>%
  arrange(
    desc(overall_no_outcomes)
  ) %>%
  mutate(rank_no_outcomes = 1:n()) %>%
  arrange(
    desc(overall)
  ) %>%
  mutate(rank_outcomes = 1:n())  

max(final_rankings_indigenous$overall) # 3.81
min(final_rankings_indigenous$overall) # 0.448
# not quite the variance we would hope for, but this works! 
(max(final_rankings_indigenous$overall) - min(final_rankings_indigenous$overall))/6  # uses 0.5604 of its possible range


max(final_rankings_indigenous$overall_no_outcomes) # 3.074
min(final_rankings_indigenous$overall_no_outcomes) # 0.2138
(max(final_rankings_indigenous$overall_no_outcomes)  - min(final_rankings_indigenous$overall_no_outcomes))/5 # uses 0.572 of its possible range

cor(final_rankings_indigenous$overall, final_rankings_indigenous$overall_no_outcomes, method = "spearman") 
# They correlate at 0.9308

write_csv(final_rankings_indigenous, path = "final_rankings_indigenous.csv")




# Add in Geo with indigenous ----------------------------------------------

ccodes <- read_csv("../data/county_codes.csv") %>%
  filter(
    grepl(
      "Charlottesville|Albemarle|Fluvanna|Greene|Louisa|Nelson",
      name
    )
  )

region <- ccodes$code

tract_geos <- get_acs(geography = "tract",
                      variables = "DP05_0001",
                      state = "VA", 
                      county = region,
                      survey = "acs5", 
                      year = 2019, 
                      output = "wide",
                      geometry = TRUE)

rankings_geo_indigenous <- 
  final_rankings_indigenous %>%
  mutate(GEOID = as.character(GEOID)) %>%
  left_join(tract_geos) %>%
  st_as_sf()

save(rankings_geo_indigenous, file = "geo_rankings_indigenous.Rdata")


tibble(
GEOID = final_rankings_indigenous$GEOID,
indigenous = final_rankings_indigenous$rank_outcomes
) %>%
  left_join(
    final_rankings %>%
      select(GEOID, non_indigenous = rank_outcomes)
  ) %>%
  View()



# Try the rankings again but with Age -------------------------------------

normed_metrics <- 
  tract_dimensions %>%
  select(GEOID,
         blackE,
         indigE,  # put indigenous back in it 
         ltnxE,
         age65E,
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

alpha(normed_metrics[,-1], check.keys = TRUE)
principal(normed_metrics[,-1], nfactors=2, rotate="none", scores=TRUE)

fa(normed_metrics[,-1], nfactors=1, rotate="promax", fm="ml", SMC=TRUE) # It is not a 1 factor solution anymore
fa(normed_metrics[,-1], nfactors=2, rotate="promax", fm="ml", SMC=TRUE) # It could be a 2 factor now with health outcomes, latinx, & age in one factor # This is overfit tho
fa(normed_metrics[,-1], nfactors=3, rotate="promax", fm="ml", SMC=TRUE) # this should not exist. It goes heywood

fa(normed_metrics[,-1], nfactors=2, rotate="promax", fm="wls", SMC=TRUE) # This corrects the overfit. Yes. Age is completely reversed. 


final_rankings_age65  <- 
  normed_metrics %>%
  mutate(overall = blackE + ltnxE + indigE + povrateE + lifeexpBRHD + health_outcomes + age65E) %>%
  arrange(
    desc(overall)
  ) %>%
  mutate(rank_outcomes = 1:n())  

# Do poverty age instead of age -------------------------------------------
normed_metrics <- 
  tract_dimensions %>%
  select(GEOID,
         blackE,
         indigE,  # put indigenous back in it 
         ltnxE,
         pov65E,
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

alpha(normed_metrics[,-1], check.keys = TRUE)
principal(normed_metrics[,-1], nfactors=2, rotate="none", scores=TRUE) # this could convincingly be 1 factor, maybe. 

fa(normed_metrics[,-1], nfactors=1, rotate="promax", fm="ml", SMC=TRUE) # 
fa(normed_metrics[,-1], nfactors=2, rotate="promax", fm="ml", SMC=TRUE) # Overfit, really need wls

fa(normed_metrics[,-1], nfactors=1, rotate="promax", fm="wls", SMC=TRUE) # this could work -- life expectancy is messed up tho
fa(normed_metrics[,-1], nfactors=2, rotate="promax", fm="wls", SMC=TRUE) # ehhh. 


final_rankings_agepov  <- 
  normed_metrics %>%
  mutate(overall = blackE + ltnxE + indigE + povrateE + lifeexpBRHD + health_outcomes + pov65E) %>%
  arrange(
    desc(overall)
  ) %>%
  mutate(rank_outcomes = 1:n())  

max(final_rankings_agepov$overall) # 3.81
min(final_rankings_agepov$overall) # 0.448
# not quite the variance we would hope for, but this works! 
(max(final_rankings_agepov$overall) - min(final_rankings_agepov$overall))/7  # uses 0.5896 of its possible range


write_csv(final_rankings_agepov, path = "final_rankings_agepov.csv")



# Add in Geo with age poverty ----------------------------------------------

ccodes <- read_csv("../data/county_codes.csv") %>%
  filter(
    grepl(
      "Charlottesville|Albemarle|Fluvanna|Greene|Louisa|Nelson",
      name
    )
  )

region <- ccodes$code

tract_geos <- get_acs(geography = "tract",
                      variables = "DP05_0001",
                      state = "VA", 
                      county = region,
                      survey = "acs5", 
                      year = 2019, 
                      output = "wide",
                      geometry = TRUE)

rankings_geo_agepov <- 
  final_rankings_agepov %>%
  mutate(GEOID = as.character(GEOID)) %>%
  left_join(tract_geos) %>%
  st_as_sf()

save(rankings_geo_agepov, file = "geo_rankings_agepov.Rdata")


# Comparison of rankings --------------------------------------------------

compare_ranks <- 
final_rankings_agepov %>%  left_join(
final_rankings_indigenous %>%
  select(GEOID, usualrank = rank_outcomes)

)

compare_ranks %>%
mutate(dif_rank = rank_outcomes - usualrank) %>%
  arrange(dif_rank) %>%
  summarize(mean(dif_rank), sd(dif_rank))


compare_ranks%>%
  arrange(rank_outcomes) %>% View()



# Generate a table to present out
compare_scores <- 
  final_rankings_agepov %>% 
  select(GEOID, overall_pov65 = overall, rank_pov65 = rank_outcomes, everything()) %>% 
  left_join(
    final_rankings_indigenous %>%
      select(GEOID, overall_orig = overall, rank_orig = rank_outcomes)
  ) %>% 
  left_join(
    final_rankings_age65 %>% 
      select(GEOID, overall_age65 = overall, rank_age65 = rank_outcomes)
  ) %>% 
  select(GEOID, overall_orig, overall_pov65, overall_age65, rank_orig, rank_pov65, 
         rank_age65, everything())


save(compare_scores, file = "compare_scores.Rdata")

compare_scores %>% 
  left_join(tract_dimensions %>% 
              select(GEOID, countyname)) %>% 
  arrange(-overall_orig) %>% 
  mutate(tract = str_sub(GEOID, 6,11)) %>% 
  select(countyname, tract, everything()) %>% 
  select(-GEOID)

cor(compare_scores$overall_orig, compare_scores$overall_pov65, method = "spearman") 

scores <- compare_scores %>% 
  select(overall_orig, overall_pov65, overall_age65)
cor(scores, method = "spearman")
