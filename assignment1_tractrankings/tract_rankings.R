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
loadfonts() ## Load in the fonts I want to use

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------
## read in data:

tract_dimensions <- read_csv("../data/tract_dimensions.csv")









