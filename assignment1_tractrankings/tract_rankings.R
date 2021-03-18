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










