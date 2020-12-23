

############
setwd("C:/Users/longh/OneDrive/Documents/STA304/Final_assignment_Sta304")
# install.packages("tidyverse")
# install.packages("devtools")
devtools::install_github("hodgettsp/cesR")
library(haven)
library(tidyverse)
library(visdat)
library(cesR)
library(skimr)
library(labelled)


get_ces("ces2019_web")

raw_ces1 <- labelled::to_factor(ces2019_web)

raw_ces <- 
  ces2019_web %>% 
  select(cps19_votechoice, cps19_province, cps19_education, cps19_citizenship, cps19_yob, 
         cps19_gender, cps19_interest_elxn_1, cps19_v_likely, cps19_not_vote_for_1, cps19_not_vote_for_2) 

#Remove respondants who aren't citizens or with empty province values
raw_ces <- raw_ces %>% 
  filter(cps19_citizenship == 4, cps19_v_likely != 5) %>%
  drop_na(cps19_province,cps19_yob,cps19_votechoice)

#Post-stratification data set doesn't include records from territories
raw_ces <- raw_ces %>% 
  filter(cps19_province != 21)
raw_ces <- raw_ces %>%   
  filter(cps19_province != 26)
raw_ces <- raw_ces %>%   
  filter(cps19_province != 19)

#convert all "other" genders into female since post stratification data has sex
raw_ces$cps19_gender[raw_ces$cps19_gender == 3] <- 2


#create a "Vote_Liberal and Vote_Conservative column accounting for people who 
#don't have a vote choice, but don't want to vote for one of the major parties. 

#raw_ces$cps19_not_vote_for_1[is.na(raw_ces$cps19_not_vote_for_1)] <- 0
#raw_ces$cps19_not_vote_for_2[is.na(raw_ces$cps19_not_vote_for_2)] <- 0

raw_ces$Vote_Liberal = raw_ces$cps19_votechoice
raw_ces <- raw_ces %>%
  mutate(Vote_Liberal = 
           ifelse(Vote_Liberal == 1, 1, 0))
raw_ces$Vote_Liberal= (raw_ces$Vote_Liberal + raw_ces$cps19_not_vote_for_2)
raw_ces <- raw_ces %>%
  mutate(Vote_Liberal = 
           ifelse(Vote_Liberal == 0, 0, 1))


#raw_ces$Vote_Conservative = raw_ces$cps19_votechoice
#raw_ces <- raw_ces %>%
#  mutate(Vote_Conservative = 
#           ifelse(Vote_Conservative == 2, 1, 0))
#raw_ces$Vote_Conservative= (raw_ces$Vote_Conservative + raw_ces$cps19_not_vote_for_1)
#raw_ces <- raw_ces %>%
#  mutate(Vote_Conservative = 
#           ifelse(Vote_Conservative == 0, 0, 1))


#We map individuals who don't want to vote liberals to conservatives and people
#who don't want to vote conservatives to liberals 

  
#convert data to make it easier to work with and read for humans
raw_ces <- labelled::to_factor(raw_ces)

#Turn year of birth into age
#the yob data in the cesR records year of birth based on years since 1919
#actual year of birth = 1919 + yob
#formula for age: 2019 -1919 - yob
raw_ces$age = with(raw_ces, 100 - as.numeric(cps19_yob))



#renaming the column names  
raw_ces <- raw_ces %>%
  rename(Vote= cps19_votechoice,
         province = cps19_province,
         education = cps19_education,
         Vote_Prob = cps19_v_likely,
         sex = cps19_gender,
         Voting_Interest = cps19_interest_elxn_1 )



#select what we will need 

raw_ces <- raw_ces %>%
  select(province, age, sex, education, Vote, Vote_Liberal, Vote_Conservative, Voting_Interest, Vote_Prob)

#Generate "age_range" coloumn 
raw_ces$age_range <- raw_ces$age
raw_ces$age_range[raw_ces$age_range %in% 18:29] <- "18-29"
raw_ces$age_range[raw_ces$age_range %in% 30:39] <- "30-39"
raw_ces$age_range[raw_ces$age_range %in% 40:49] <- "40-49"
raw_ces$age_range[raw_ces$age_range %in% 50:59] <- "50-59"
raw_ces$age_range[raw_ces$age_range %in% 60:69] <- "60-69"
raw_ces$age_range[raw_ces$age_range %in% 70:79] <- "70-79"
raw_ces$age_range[raw_ces$age_range %in% 80:89] <- "80-89"
raw_ces$age_range[raw_ces$age_range %in% 90:99] <- "90-99"



#write our cleaned cesR data set
write_csv(raw_ces, "ProcessedData/cesR.csv", append = FALSE)








