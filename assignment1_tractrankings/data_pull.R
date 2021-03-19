## ---------------------------
## Script name: data_pull.R
##
## Author:Sam Powers
## Date Created: 2021-03-18
##
## ---------------------------
## Purpose of script: Pulls data for the first assignment: To create census tract prioritization ratings for the BRHD. 
##   
##
## ---------------------------
## set working directory

setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/brhd-vaccines/assignment1_tractrankings") # Sam's WD


## ---------------------------
## load up the packages we will need:

library(tidyverse)
library(extrafont)
library(tidycensus)
library(readxl)
loadfonts() ## Load in the fonts I want to use

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------
## Implementation Plan

# 1. Generate an index and ranking using just the poverty, race as defined above (with equity atlas code), and 
# life expectancy using BRHD values (not sure that these are programmatically downloadable, though)

# 2. But then again using the more recent USASLEEP values -- just to make sure it doesn't make a marked difference -- if not, use 1

# 3. And then compare this with a version that incorporates additional health prevalence estimates -- here it will be important to not overweight these -- 
# so in my head, these are first combined and then included with life expectancy, race, and poverty.

# If there's, again, no marked difference, I'd stick with 1 (and note that 3 was tried); if there is a marked difference, I'd report both.



# Resources ---------------------------------------------------------------
# https://public.tableau.com/profile/thomas.jefferson.health.district#!/vizhome/MAPP2Health-ReduceHealthDisparitiesandImproveAccesstoCarePriorityIndicators/MAPPPriorityThree
# ^^ Public Tableau Dashboard



# Define the Region -------------------------------------------------------
# BRHD counties defined based on Tableau Dashboard Above
ccodes <- read_csv("../data/county_codes.csv") %>%
  filter(
    grepl(
    "Charlottesville|Albemarle|Fluvanna|Greene|Louisa|Nelson",
      name
    )
  )

region <- ccodes$code

# Pull Race/Ethnicity Data For the Region ---------------------------------

acs5_2019_prof <- load_variables(2019, "acs5/profile", cache = TRUE)

varlist_19 = c("DP05_0001", "DP05_0037P", "DP05_0038P", "DP05_0039P", "DP05_0044P", "DP05_0052P", "DP05_0057P", "DP05_0058P", "DP05_0071P")

acs5_2019_prof %>%
  filter(name %in% varlist_19)


# totalpop, white, black, indig, asian, oth1, oth2, multi, ltnx
tract_19 <- get_acs(geography = "tract",
                    variables = varlist_19,
                    state = "VA", 
                    county = region,
                    survey = "acs5", 
                    year = 2019, 
                    output = "wide")


# rename, add year, derive othrace
names(tract_19) <- c("GEOID", "NAME", "totalpopE", "totalpopM",
                     "whiteE", "whiteM", "blackE", "blackM", 
                     "indigE", "indigM", "asianE", "asianM",
                     "oth1E", "oth1M", "oth2E", "oth2M",
                     "multiE", "multiM", "ltnxE", "ltnxM")

tract_19 <- tract_19 %>% 
  mutate(year = "2019") %>% 
  mutate(othraceE = oth1E + oth2E,
         othraceM = NA) %>% 
  select(GEOID, NAME, year, everything(), -c(oth1E:oth2M))

write_csv(tract_19, path = "../data/brhd_race_ethnicity.csv")



# Pull Poverty Rate -------------------------------------------------------

varlist_s = c("S1701_C03_001", # povrate
              "S1701_C03_002"   # cpovrate
              )   


# pull variables
tract_data_s <- get_acs(geography = "tract",
                        variables = varlist_s,
                        state = "VA", 
                        county = region, 
                        survey = "acs5",
                        year = 2019, 
                        output = "wide")



# rename variables
names(tract_data_s) = c("GEOID", "NAME",
                        "povrateE", "povrateM",
                        "cpovrateE", "cpovrateM"
                        )

tract_data_poverty <- 
tract_data_s %>% 
  mutate(povrateE = ifelse(GEOID == "51003010903", NA_integer_, povrateE)
  )



# Pull BRHD Life Expectancies for the Region ------------------------------
# https://public.tableau.com/profile/thomas.jefferson.health.district#!/vizhome/MAPP2Health-ReduceHealthDisparitiesandImproveAccesstoCarePriorityIndicators/MAPPPriorityThree
## ^^ Pulling from this is not working. Will likely have to email Guleer 
brhd_le <- read_excel("../data/brhd_le.xlsx")

brhd_le <- 
ccodes %>%
  separate(
    name,
    c("county", NA),
    sep = " "
  ) %>%
  select(-id) %>%
left_join(brhd_le) %>%
separate(tractname, c(NA, NA, "tractnum"), sep =" " ) %>% 
mutate(
  tractnum  = str_sub(paste0( "000", str_replace_all(  as.character(str_trim(format(as.numeric(tractnum), nsmall = 2))), "\\.", "")), -6, -1),
  GEOID = paste0("51", code, tractnum)
  ) 



# Pull in CDC Life Expectancies ----------------------------------------

# Tract Level Life Expectancy
url <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/XLSX/VA_A.XLSX"
destfile <- "../data/tract_expectancy.xlsx"
download.file(url, destfile)

tract_expectancy_load <- read_excel(destfile)

tract_expectancy <- 
  tract_expectancy_load %>%
  rename_with(
    ~tolower(
      str_replace_all(.x, 
                      " ", "_")
    )    
  ) %>%
  rename(GEOID = tract_id, state_fips = state2kx, county_fips = cnty2kx, tract_fips = tract2kx, life_expectancy = `e(0)`, se = `se(e(0))` ) %>%
  select(-abridged_life_table_flag) %>%
  mutate(fips = paste0(state_fips, county_fips)) %>%
  filter(county_fips %in% region)

unique(tract_expectancy$county_fips)

write_csv(tract_expectancy, "../data/cdc_life_expectancies_brhd_region.csv")



# CDC Health Outcomes -----------------------------------------------------
# From the CDC Places metrics https://chronicdata.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Census-Tract-D/cwsq-ngmh
health_outcomes <- 
  map_df(region,
         ~ read_csv(
           paste0("https://chronicdata.cdc.gov/resource/cwsq-ngmh.csv?countyfips=51", .x)
         )
  )

# From the CDC https://www.cdc.gov/coronavirus/2019-ncov/need-extra-precautions/people-with-medical-conditions.html

## The following are confirmed as risk elevators:
#   Cancer
#   Chronic kidney disease
#   COPD (chronic obstructive pulmonary disease)
#   Down Syndrome
#   Heart conditions, such as heart failure, coronary artery disease, or cardiomyopathies
#   Immunocompromised state (weakened immune system) from solid organ transplant
#   Obesity (body mass index [BMI] of 30 kg/m2 or higher but < 40 kg/m2)
#   Severe Obesity (BMI ≥ 40 kg/m2)
#   Pregnancy
#   Sickle cell disease
#   Smoking
#   Type 2 diabetes mellitus

## The following *might* be risk elevators:
#   Asthma (moderate-to-severe)
#   Cerebrovascular disease (affects blood vessels and blood supply to the brain)
#   Cystic fibrosis
#   Hypertension or high blood pressure
#   Immunocompromised state (weakened immune system) from blood or bone marrow transplant, immune deficiencies, HIV, use of corticosteroids, or use of other immune weakening medicines
#   Neurologic conditions, such as dementia
#   Liver disease
#   Overweight (BMI > 25 kg/m2, but < 30 kg/m2)
#   Pulmonary fibrosis (having damaged or scarred lung tissues)
#   Thalassemia (a type of blood disorder)
#   Type 1 diabetes mellitus

# We have data on:
unique(health_outcomes$short_question_text)
unique(health_outcomes$measure)

#  "Core preventive services for older women" "Core preventive services for older men"   "Coronary Heart Disease"                   "Binge Drinking"                          
#  "Mental Health"                            "High Blood Pressure"                      "Mammography"                              "Physical Inactivity"                     
#  "Diabetes"                                 "Stroke"                                   "Current Smoking"                          "Cholesterol Screening"                   
#  "Cancer (except skin)"                     "Current Asthma"                           "Chronic Kidney Disease"                   "Taking BP Medication"                    
#  "Dental Visit"                             "High Cholesterol"                         "COPD"                                     "Obesity"                                 
#  "Colorectal Cancer Screening"              "Physical Health"                          "Arthritis"                                "Cervical Cancer Screening"               
#  "Health Insurance"                         "Teeth Loss"                               "Sleep <7 hours"                           "Annual Checkup"  


# I will select the following from the data: (short question text)
## "Cancer (except skin)" 
## "Chronic Kidney Disease"
## "COPD"
## "Coronary Heart Disease"
## "Obesity"
## "Current Smoking"
## "Diabetes"

at_risk_outcomes <- c("Cancer (except skin)", 
                      "Chronic Kidney Disease", 
                      "COPD", 
                      "Coronary Heart Disease", 
                      "Obesity", 
                      "Current Smoking", 
                      "Diabetes")

covid_risk_outcomes <- 
health_outcomes %>%
  filter(
    short_question_text %in%
      at_risk_outcomes
  )

unique(covid_risk_outcomes$data_value_unit) # They are all percents. We can drop this. 
unique(covid_risk_outcomes$data_value_type) # They are all crude prevalences
unique(covid_risk_outcomes$category)        # Health Outcomes & Unhealthy Behaviors
unique(covid_risk_outcomes$locationid)      # GEOID 
unique(covid_risk_outcomes$countyname)      # countyname
unique(covid_risk_outcomes$year)            # It is all 2018
unique(covid_risk_outcomes$totalpopulation) # Total population of the tract

tract_outcomes <-
covid_risk_outcomes %>%
  select(countyname, locationid,  short_question_text, data_value)  %>%
  mutate(
    short_question_text = paste0(str_replace_all(str_replace_all(str_to_lower(short_question_text), "\\(|\\)", ""), " ", "_"), "_outcome") 
  ) %>%
  spread(short_question_text, data_value)


write_csv(tract_outcomes, "../data/tract_health_outcomes.csv")


# put all the data together -----------------------------------------------
tract_prioritization_facts <- 
tract_19 %>%
  select(GEOID, contains("E")) %>%
  select(-NAME, -othraceM, -year, -whiteM) %>%
  left_join(
    
    tract_data_poverty %>%
      select(GEOID, povrateE, cpovrateE)
    
  ) %>% left_join(
    
    brhd_le %>%
      select(GEOID, lifeexpBRHD = lifeexp)
    
       ) %>%
  left_join(
    tract_expectancy %>%
      select(GEOID, lifeexpCDC = life_expectancy)
  ) %>%
left_join(
  tract_outcomes %>%
    mutate(GEOID = as.character(locationid)) %>%
    select(-locationid)
) %>%
  select(countyname, GEOID, everything()) %>%
  filter(

    !GEOID %in% c(      # Censoring per Recommendation by BRHD Life Expectancy estimates
      "51003010901",   
      "51003010903",  
      "51003010902",  
      "51003010202"   
      )
  )


write_csv(tract_prioritization_facts, "../data/tract_dimensions.csv")








