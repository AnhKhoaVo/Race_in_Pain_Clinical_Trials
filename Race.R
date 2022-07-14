install.packages("RPostgreSQL")
library(RPostgreSQL)

drv <- dbDriver('PostgreSQL')

con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user="anhkhoavo", password="Voanhkhoa8!")

aact_sample <- dbGetQuery(con, "SELECT * FROM studies")
write.csv(aact_sample, file='aact_sample.csv', na=" ")


aact_race <- dbGetQuery(con, "SELECT * FROM baseline_measurements")
write.csv(aact_race, file='aact_race.csv', na=" ")

aact_conditions <- dbGetQuery(con, "SELECT * FROM conditions")

aact_sponsor <- dbGetQuery(con, "SELECT * FROM sponsors")

aact_summaries <- dbGetQuery(con, "SELECT * FROM brief_summaries")

aact_eligibilities <- dbGetQuery(con, "SELECT * FROM eligibilities")

aact_countries <- read.csv(file.choose())
aact_race_sql <- read.csv(file.choose())
aact_keywords <- read.csv(file.choose())
aact_interventions <- read.csv("aact_interventions.csv")
opioids_list <- read.csv("Opioids_list.csv")
aact_baseline <- read.csv("aact_baseline.csv")
aact_facilities <- read.csv("aact_facilities.csv")
aact_investigators <- read.csv("aact_investigators.csv")
aact_officials <- read.csv("aact_officials.csv")

library(plyr)
library(dplyr)
library(stringr)
library(reshape2)
library(tm)
library(stringi)
library(naniar)
library(tidyr)
library(party)
library(ggplot2)
library(metafor)
library(table1)
library(epitools)
library(tidyverse)
library(meta)
library(caret)
library(gtsummary)


#Select only "pain" conditions
pain_conditions <- aact_conditions %>%
  filter(str_detect(downcase_name, "pain"))

#How many pain studies have results:  
common_conditions <- intersect(pain_conditions$nct_id, aact_race$nct_id)

#Dataframe of all pain studies have results 
pain_studies <- join_all(list(pain_conditions,aact_sample,aact_race), by = 'nct_id', type = 'inner')

#Renamed duplicated columns 
colnames(pain_studies)[68] = "id_2"

#Pain studies that have race 
pain_studies_race <- pain_studies %>%
  filter(str_detect(title, paste(c("Race", "Region"), collapse = '|')))

#Picked only a few relevant columns and remove "ethnicity" and "not collected race data"
pain_studies_race_2 <- pain_studies_race %>% 
  filter(!str_detect(title, "Race and Ethnicity Not Collected")) %>%
  select(nct_id, study_type, phase, title, classification, category,
            ctgov_group_code, param_value) 

#Changing blank rows into NA
pain_studies_race_2[pain_studies_race_2 == ""] <- NA

#How many names for classification:
unique(pain_studies_race_2$classification)

#How to change Region name to "Total":
#Duplicate another column
pain_studies_race_2$classification_2 = pain_studies_race_2$classification 
#Turn all region names in duplicate columns to Total, the rest NA
pain_studies_race_2$classification_2 <- ifelse(pain_studies_race_2$title=="Region of Enrollment"|pain_studies_race_2$title=="Region Enroll"|
                                                 pain_studies_race_2$title=="Region Enroll (United States of America)"|pain_studies_race_2$title=="Geographic Region", 
                                               "Total", NA)
#Names of regions in a vector
regions <- c("Region of Enrollment", "Region Enroll", "Region Enroll (United States of America)", "Geographic Region")

#Now I turn all the name of each country in "Classification" col into NA based on the "region" of the "title" col:
pain_studies_race_2$classification[pain_studies_race_2$title %in% regions] <- NA

#Replace all the extra words of Race in the "Classification" column
removal <- c("Race/Ethnicity", "Race", "Self-identified race and ethnicity", "Region of Enrollment", 
             "Participant Race", "Baseline Measures", "Ethnicity")
pain_studies_race_2<- replace_with_na_at(pain_studies_race_2,
                                                         .vars = "classification",
                                                         condition = ~.x %in% removal)

#Create a new column to paste "classification", "classification_2" (dup) and "category" 
cols <- c('classification', 'category', 'classification_2')
pain_studies_race_2$race <- apply( pain_studies_race_2[ , cols] , 1 , paste , collapse = "" )

#Remove words: NA and extra white space
pain_studies_race_2$race <- str_replace(pain_studies_race_2$race, "NA", " ") #DO THIS TWICE
pain_studies_race_2$race<-str_trim(pain_studies_race_2$race) 

#After merging into a "race" column, this column only contains actual race, not some random words
#How many unique characters in this "race" column
unique(pain_studies_race_2$race)

#Changing "unknown"/"missing" to "unknown"
unknown <- c("Choose Not to Disclose", "Declined", "Don't know/Refused/Not Specified", "Unknown/Not reported",
             "Missing", "Missing/Unknown", "Data missing", "Race and Ethnicity Not Collected", "Other or unspecified",
             "Not Reported", "Other/Unknown", "Unknown or Not Reported", "Declined to Answer", "Missing information", 
             "Other or prefer not to answer")

pain_studies_race_2$race[pain_studies_race_2$race %in% unknown] <- "Unknown"

#Changing to Black 
Blacks <- c("Black or African American", "African-American", "Black/African American",
           "African American", "Non-Hispanic Black", "African American/Black", 
           "African American/African", "African American or Black", "Black or African-American",
           "Black/African-American", "Black (or African American)", "african american",
           "Black, not Hispanic", "African american", "Balck, not Hispanic",
           "Black/African American - not Hispanic", "African", "Nonhispanic Black", "African Descent",
           "Hispanic black")

pain_studies_race_2$race[pain_studies_race_2$race %in% Blacks] <- "Black"

#Changing to White
Whites <- c("White/Non-Hispanic", "White (hispanic)", "White (non-hispanic)", "Caucasian", "White/Caucasian", 
            "white", "Non-Hispanic White", "European (Caucasian)", "White Non-Hispanic", "WhiteNon-hispanic", 
            "WhiteHispanic", "White/Other", "White or Caucasian", "white, hispanic", "white, nonhispanic", 
            "Race, WhiteHispanic or Latino", "Race, WhiteNot Hispanic or Latino", "White - not Hispanic", 
            "Nonhispanic White", "caucasian", "NonHispanic white", "White Hispanic", "White, not Hispanic", 
            "Hispanic white")

pain_studies_race_2$race[pain_studies_race_2$race %in% Whites] <- "White"

#Changing to Asians/Mixed/Others
Asians <- c("More than one race", "Asian", 
          "Other",  "Asian/Pacific Islander",
          "Mixed", "More than one", "multiple", "asian", "Mixed or multi-racial", 
          "Multiracial", "Other or Multiple", 
          "Multiple", "East Asian/Asian", "Multiple or other",
          "Pacific Islander",
          "Asian/Indian", "Asian or Pacific Islander", "other", "Other Race", "Multi-racial",
          "Black and White", "Asian and White","Asian/Oriental", "Bi-racial", 
          "Asian/other", "Non-caucasian", "non-white", "East Asian", "West Asian", "Non-Caucasian", "Non-white")

pain_studies_race_2$race[pain_studies_race_2$race %in% Asians] <- "Asians/Others"

#Changing to Indigenous 
Natives <- c("Native Hawaiian or Other Pacific Islander", "American Indian or Alaska Native",
          "Native American", "Native Hawaiian/Pacific Islander", "American Indian",
          "Native Hawaiian or other Pacific Islander", "American Indian/Alaskan Native",
          "American-Indian or Alaska Native", "American Indian or Alaskan Native",
          "Indian",  "American Indian/Alaskan",
          "American Indian/Alaska Native")

pain_studies_race_2$race[pain_studies_race_2$race %in% Natives] <- "Natives"


#removing Ethnicity and latino
hispanics <- c("Not Hispanic or Latino","Hispanic or Latino","Hispanic","HAN",                                
               "Latinx","Hispanic/Latino","non hispanic latino","hispanic latino",
               "% black","% white","Latino","Hispanic/Latina","Not Hispanic/Latino",
               "NA","Cypriot","Hispanic or Latino Ethnicity","Non-Hispanic and non-Latino",
               "hispanic","Hispanic or Mixed Ethnic Background")

pain_studies_race_2$race[pain_studies_race_2$race %in% hispanics] <- "Ethnicity"

#Create a new data frame to remove ethnicity 
pain_studies_race_3 <- pain_studies_race_2 %>% 
  filter(!str_detect(race, "Ethnicity"))

#Merge groups and race together
race_cols <- c('race', 'ctgov_group_code')
pain_studies_race_3$race <- apply( pain_studies_race_3[ , race_cols] , 1 , paste , collapse = "_" )

#Selecting only relevant columns:
pain_studies_race_3 <- pain_studies_race_3 %>% 
  select(nct_id, race, param_value)

#Adding new ID for each trial
pain_studies_race_3<- pain_studies_race_3 %>%
  group_by(nct_id) %>%
  mutate(ID = 1:n()) 

#Turning all 0 into 0.001 so I can easily replace them later on 
pain_studies_race_3$param_value[pain_studies_race_3$param_value==0]<-0.001
#Turning ALL races into wide format while maintaining nct_id and ID as long 
pain_studies_race_wide <- dcast(pain_studies_race_3, nct_id+ID~race, value.var = "param_value")

#Deleting ID group so I can merge every rows into one
pain_studies_race_wide$ID <- NULL

#Turning all columns into numeric 
cols_numeric <- c(2:59)
pain_studies_race_wide[ , cols_numeric] <- apply(pain_studies_race_wide[ , cols_numeric], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))

#Turning all NA into 0
pain_studies_race_wide[is.na(pain_studies_race_wide)] <- 0

#Now I can merge EVERY row into one row
pain_studies_race_wide<-pain_studies_race_wide %>% 
  group_by(nct_id) %>% 
  summarise_each(funs(sum))

pain_studies_race_wide[pain_studies_race_wide==0.0]<-NA
pain_studies_race_wide[pain_studies_race_wide==0.001]<-0

pain_studies_race_wide[,2:59] <- round(pain_studies_race_wide[,2:59])

pain_studies_race_wide$Total_B1[pain_studies_race_wide$nct_id=="NCT02972359"]<-316


#Black population in pain trials
black_pain <- select(pain_studies_race_wide,"nct_id", 
                     starts_with("Black_"), starts_with("Total_"))

black_pain$Black_percent <- ifelse(!is.na(black_pain$Total_B8),black_pain$Black_B8/black_pain$Total_B8,
                                   ifelse(!is.na(black_pain$Total_B7), black_pain$Black_B7/black_pain$Total_B7,
                                          ifelse(!is.na(black_pain$Total_B6), black_pain$Black_B6/black_pain$Total_B6,
                                                        ifelse(!is.na(black_pain$Total_B5), black_pain$Black_B5/black_pain$Total_B5,
                                                                      ifelse(!is.na(black_pain$Total_B4), black_pain$Black_B4/black_pain$Total_B4,
                                                                             ifelse(!is.na(black_pain$Total_B3), black_pain$Black_B3/black_pain$Total_B3,
                                                                                    ifelse(!is.na(black_pain$Total_B1), black_pain$Black_B1/black_pain$Total_B1, NA
                                                                                           )))))))

black_pain<-black_pain[!(black_pain$Black_percent>1),]

black_pain_studies <- full_join(black_pain, aact_sample, by = "nct_id")

black_pain_studies <- select(black_pain_studies,"nct_id", "study_type", "phase",
                             "start_date", "completion_date", "official_title", "enrollment",
                     starts_with("Black_"), starts_with("Total_"))

black_pain_studies <- black_pain_studies[complete.cases(black_pain_studies$Black_percent),]


nrow(subset(black_pain_studies, phase=="Phase 1/Phase 2"))
nrow(subset(black_pain_studies, phase=="Phase 2/Phase 3"))
nrow(subset(black_pain_studies, phase=="Phase 2"))
nrow(subset(black_pain_studies, phase=="Phase 3"))
nrow(subset(black_pain_studies, phase=="Phase 4"))

black_pain_studies %>% group_by(phase) %>% 
  summarise(mean=mean(Black_percent), median=median(Black_percent))

#Looking for sex in pain studies 
pain_studies_sex <- pain_studies %>%
  filter(str_detect(title,  paste(c("Sex: Female, Male", "Sex/Gender, Customized"), collapse = '|'))) %>%
  select(nct_id, study_type, phase, title, classification, category,
         ctgov_group_code, param_value)

pain_studies_sex[pain_studies_sex == ""] <- NA

sex_cols <- c('classification', 'category')
pain_studies_sex$sex <- apply( pain_studies_sex[ , sex_cols] , 1 , paste , collapse = "" )

pain_studies_sex$sex <- str_replace(pain_studies_sex$sex, "NA", " ") 
pain_studies_sex$sex <-str_trim(pain_studies_sex$sex)

unique(pain_studies_sex$sex)

# Create List of Exclusion Phrases
sex_removed <- c("female pregnant patients", "Missing", "unspecified", "Unspecified",
                 "Unknown", "NA", "Maintenance period - Female", "Titration period - Female", 
                 "Female (Placebo Maintenance)", "Female (Tapentadol Maintenance)", 
                 "Female (Morphine (Maintenance)", "Female (Titration)", "Maintenance period - Male", "Titration period - Male",
                 "Males", "Male (Placebo Maintenance)", "Male (Tapentadol Maintenance)", 
                 "Male (Morphine Maintenance)", "Male (Titration)")
# Exclude
pain_studies_sex$sex[pain_studies_sex$sex %in% sex_removed] <- "REMOVED"
pain_studies_sex<-pain_studies_sex[!grepl('REMOVED',pain_studies_sex$sex),]

#Changing to Males and Females 
Female <- c("Female", "female","Females", "SexFemale","female participants", 
            "women")

pain_studies_sex$sex[pain_studies_sex$sex %in% Female] <- "Female"

Male <- c("Male", "male", "SexMale")

pain_studies_sex$sex[pain_studies_sex$sex %in% Male] <- "Male"

#Merge sex and group together 
sex_cols_2 <- c('sex', 'ctgov_group_code')
pain_studies_sex$sex <- apply( pain_studies_sex[ , sex_cols_2] , 1 , paste , collapse = "_" )

#Selecting only relevant columns
pain_studies_sex_2 <- pain_studies_sex %>% 
  select(nct_id, sex, param_value)

##Adding new ID for each trial
pain_studies_sex_2<-pain_studies_sex_2 %>%
  group_by(nct_id) %>%
  mutate(ID = 1:n())

#Remove this one private trial that has a lot of columns 
pain_studies_sex_2<- pain_studies_sex_2[!grepl('NCT01236053',pain_studies_sex_2$nct_id),]

#Turning 0 into 0.001
pain_studies_sex_2$param_value[pain_studies_sex_2$param_value==0]<-0.001

pain_studies_sex_wide <- dcast(pain_studies_sex_2, nct_id+ID~sex, value.var = "param_value")

pain_studies_sex_wide$ID <- NULL
#Turning all columns into numeric 
cols_sex_numeric <- c(2:23)
pain_studies_sex_wide[ , cols_sex_numeric] <- apply(pain_studies_sex_wide[ , cols_sex_numeric], 2,            # Specify own function within apply
                                                 function(x) as.numeric(as.character(x)))

#turning NA into 0 so I can easliy sum them 
pain_studies_sex_wide[is.na(pain_studies_sex_wide)] <- 0


pain_studies_sex_wide<-pain_studies_sex_wide %>% 
  group_by(nct_id) %>% 
  summarise_all(funs(sum))

pain_studies_sex_wide[pain_studies_sex_wide==0.0]<-NA
pain_studies_sex_wide[pain_studies_sex_wide==0.001]<-0
pain_studies_sex_wide[,2:23] <- round(pain_studies_sex_wide[,2:23])

#Calculate total for sex
pain_studies_sex_wide$total_sex <- ifelse(!is.na(pain_studies_sex_wide$Female_B8),pain_studies_sex_wide$Male_B8+pain_studies_sex_wide$Female_B8,
                                      ifelse(!is.na(pain_studies_sex_wide$Female_B7), pain_studies_sex_wide$Male_B7+pain_studies_sex_wide$Female_B7,
                                          ifelse(!is.na(pain_studies_sex_wide$Female_B6), pain_studies_sex_wide$Male_B6+pain_studies_sex_wide$Female_B6,
                                                 ifelse(!is.na(pain_studies_sex_wide$Female_B5), pain_studies_sex_wide$Male_B5+pain_studies_sex_wide$Female_B5,
                                                        ifelse(!is.na(pain_studies_sex_wide$Female_B4), pain_studies_sex_wide$Male_B4+pain_studies_sex_wide$Female_B4,
                                                               ifelse(!is.na(pain_studies_sex_wide$Female_B3), pain_studies_sex_wide$Male_B3+pain_studies_sex_wide$Female_B3,
                                                                      ifelse(!is.na(pain_studies_sex_wide$Female_B1), pain_studies_sex_wide$Male_B1+pain_studies_sex_wide$Female_B1, NA
                                                                      )))))))

pain_studies_sex_wide$female_percent <- ifelse(!is.na(pain_studies_sex_wide$Female_B8),pain_studies_sex_wide$Female_B8/pain_studies_sex_wide$total_sex,
                                          ifelse(!is.na(pain_studies_sex_wide$Female_B7), pain_studies_sex_wide$Female_B7/pain_studies_sex_wide$total_sex,
                                                 ifelse(!is.na(pain_studies_sex_wide$Female_B6), pain_studies_sex_wide$Female_B6/pain_studies_sex_wide$total_sex,
                                                        ifelse(!is.na(pain_studies_sex_wide$Female_B5), pain_studies_sex_wide$Female_B5/pain_studies_sex_wide$total_sex,
                                                               ifelse(!is.na(pain_studies_sex_wide$Female_B4), pain_studies_sex_wide$Female_B4/pain_studies_sex_wide$total_sex,
                                                                      ifelse(!is.na(pain_studies_sex_wide$Female_B3), pain_studies_sex_wide$Female_B3/pain_studies_sex_wide$total_sex,
                                                                             ifelse(!is.na(pain_studies_sex_wide$Female_B1), pain_studies_sex_wide$Female_B1/pain_studies_sex_wide$total_sex, NA
                                                                             )))))))


pain_studies_sex_wide_2 <- select(pain_studies_sex_wide,"nct_id", "total_sex", "female_percent")

black_sex_pain_studies <- full_join(black_pain_studies, pain_studies_sex_wide_2, by = "nct_id")

black_sex_pain_studies_2 <- select(black_sex_pain_studies,"nct_id", "study_type", "phase", "official_title",
                                   "start_date", "completion_date", "enrollment",
                                   "total_sex", "female_percent", "Black_percent")

black_sex_pain_studies_2 <- black_sex_pain_studies_2[complete.cases(black_sex_pain_studies_2$Black_percent),]

black_sex_pain_studies_2$female_percent <- round(black_sex_pain_studies_2$female_percent*100)
black_sex_pain_studies_2$Black_percent <- round(black_sex_pain_studies_2$Black_percent*100)

black_sex_pain_studies_2$start_year <- as.numeric(format(as.Date(black_sex_pain_studies_2$start_date, format="%d/%m/%Y"),"%Y"))
black_sex_pain_studies_2$end_year <- as.numeric(format(as.Date(black_sex_pain_studies_2$completion_date, format="%d/%m/%Y"),"%Y"))
black_sex_pain_studies_2$years_passed <- black_sex_pain_studies_2$end_year - black_sex_pain_studies_2$start_year

ggscatter(black_sex_pain_studies_2, x="female_percent", y="Black_percent")

black_sex_pain_studies_2$phase[black_sex_pain_studies_2$phase=="N/A"]<-NA

#White population in clinincal trials
white_pain <- select(pain_studies_race_wide,"nct_id", 
                     starts_with("White_"), starts_with("Total_"))

white_pain$White_percent <- ifelse(!is.na(white_pain$Total_B8),white_pain$White_B8/white_pain$Total_B8,   
                                   ifelse(!is.na(white_pain$Total_B7), white_pain$White_B7/white_pain$Total_B7,
                                          ifelse(!is.na(white_pain$Total_B6), white_pain$White_B6/white_pain$Total_B6,
                                                 ifelse(!is.na(white_pain$Total_B5), white_pain$White_B5/white_pain$Total_B5,
                                                        ifelse(!is.na(white_pain$Total_B4), white_pain$White_B4/white_pain$Total_B4,
                                                               ifelse(!is.na(white_pain$Total_B3), white_pain$White_B3/white_pain$Total_B3,
                                                                      ifelse(!is.na(white_pain$Total_B1), white_pain$White_B1/white_pain$Total_B1, NA
                                                                      )))))))

white_pain<-white_pain[!(white_pain$White_percent>1),]

white_pain <- select(white_pain, "nct_id", "White_percent")
#Merge
race_sex_pain_studies <- full_join(black_sex_pain_studies_2, white_pain, by = "nct_id")
race_sex_pain_studies$White_percent <- round(race_sex_pain_studies$White_percent *100)

#Added Sponsor
aact_sponsor_2 <- select(aact_sponsor, "nct_id", "agency_class")
aact_sponsor_2<-aact_sponsor_2[!duplicated(aact_sponsor_2), ]

race_sex_pain_studies_2 <- full_join(race_sex_pain_studies, aact_sponsor_2, by="nct_id")
race_sex_pain_studies_2 <- race_sex_pain_studies_2[complete.cases(race_sex_pain_studies_2$Black_percent),]

race_sex_pain_studies_2<-race_sex_pain_studies_2[!duplicated(race_sex_pain_studies_2$nct_id), ]
race_sex_pain_studies_2$POC_percent <- 100-(race_sex_pain_studies_2$White_percent+race_sex_pain_studies_2$Black_percent)
race_sex_pain_studies_2$Black_POC_percent <- race_sex_pain_studies_2$Black_percent + race_sex_pain_studies_2$POC_percent

race_sex_pain_studies_2$phase[race_sex_pain_studies_2$phase=="N/A"]<-NA

#Added back country 
pain_studies_countries <- pain_studies %>% 
  filter(str_detect(title, "Region of Enrollment")) %>%
  select(nct_id, title, classification) 

pain_studies_countries<-pain_studies_countries[!duplicated(pain_studies_countries$nct_id), ]
pain_studies_countries$title <- NULL
race_sex_pain_studies_3 <- full_join(race_sex_pain_studies_2, pain_studies_countries, by="nct_id")
race_sex_pain_studies_3 <- race_sex_pain_studies_3[complete.cases(race_sex_pain_studies_3$Black_percent),]

plot(ctree(Black_percent~female_percent+phase+agency_class+enrollment+classification, data=subset(race_sex_pain_studies_3, !is.na(Black_percent))))
summary(lm(Black_percent~female_percent+phase+agency_class+enrollment, data=subset(race_sex_pain_studies_3, classification=="United States")))

ggboxplot(data = race_sex_pain_studies_2, x="agency_class", y="Black_percent")

#Checking pubmed for common conditions
Other_studies <- paste(setdiff(common_conditions, race_sex_pain_studies_3$nct_id), collapse = "|")

#Added in 
race_other <- read.csv(file.choose())

#Round up number 
race_other[,2:6] <- round(race_other[,2:6])

# Left join a bunch of df
race_other_2 <-Reduce(function(x, y) merge(x, y,by="nct_id", all.x=TRUE), list(race_other, aact_sample, aact_sponsor_2, pain_studies_countries))

race_other_2<-race_other_2[!duplicated(race_other_2$nct_id), ]

race_other_2 <- select(race_other_2,"nct_id", "study_type", "phase",
                        "start_date", "completion_date", "official_title", "enrollment", "agency_class", "classification",
                       "White_percent", "Black_percent", "POC_percent", "Black_POC_percent", "female_percent")

race_sex_pain_studies_4 <- bind_rows(race_other_2, race_sex_pain_studies_3)
race_sex_pain_studies_4$phase[race_sex_pain_studies_4$phase=="N/A"]<-NA

race_sex_pain_studies_4$start_year <- as.numeric(format(as.Date(race_sex_pain_studies_4$start_date, format="%d/%m/%Y"),"%Y"))
race_sex_pain_studies_4$end_year <- as.numeric(format(as.Date(race_sex_pain_studies_4$completion_date, format="%d/%m/%Y"),"%Y"))
race_sex_pain_studies_4$years_passed <- race_sex_pain_studies_4$end_year - race_sex_pain_studies_4$start_year

race_sex_pain_studies_4$countries <- as.factor(ifelse(race_sex_pain_studies_4$classification=="United States"|race_sex_pain_studies_4$classification=="UNITED STATES"|
                                                        race_sex_pain_studies_4$classification=="Canada", "North America", "Others"))

plot(ctree(Black_percent~female_percent+phase+agency_class+study_type, 
           data=subset(race_sex_pain_studies_4, !is.na(Black_percent)&countries=="North America")), main="Outcome = Black percent")


plot(ctree(POC_percent~female_percent+phase+agency_class+study_type, 
           data=subset(race_sex_pain_studies_4, !is.na(POC_percent)&countries=="North America")), main="Outcome = POC percent in North America Only")


plot(ctree(Black_POC_percent~female_percent+phase+agency_class+study_type, 
           data=subset(race_sex_pain_studies_4, !is.na(Black_POC_percent)&countries=="North America")), main="Outcome = Black + POC percent in North America Only")

####Checking the rest of the pain conditions
Rest_pain <-paste(setdiff(pain_conditions$nct_id, race_sex_pain_studies_4$nct_id), collapse = "|")

#Added in race_other 2, 3 and 4
race_other_B <- read.csv(file.choose())
race_other_C <- read.csv(file.choose())
race_other_D <- read.csv(file.choose())
race_other_3 <- bind_rows(race_other_B, race_other_C, race_other_D)

race_other_4 <-Reduce(function(x, y) merge(x, y,by="nct_id", all.x=TRUE), list(race_other_3, aact_sample, aact_sponsor_2, pain_studies_countries))

race_other_4<-race_other_4[!duplicated(race_other_4$nct_id), ]

race_other_4 <- select(race_other_4,"nct_id", "study_type", "phase",
                       "start_date", "completion_date", "official_title", "enrollment", "agency_class", "classification",
                       "White_percent", "Black_percent", "POC_percent", "Black_POC_percent", "female_percent")

race_sex_pain_studies_5 <- bind_rows(race_other_4, race_sex_pain_studies_4)
race_sex_pain_studies_5$phase[race_sex_pain_studies_5$phase=="N/A"]<-NA

race_sex_pain_studies_5$start_year <- as.numeric(format(as.Date(race_sex_pain_studies_5$start_date, format="%d/%m/%Y"),"%Y"))
race_sex_pain_studies_5$end_year <- as.numeric(format(as.Date(race_sex_pain_studies_5$completion_date, format="%d/%m/%Y"),"%Y"))
race_sex_pain_studies_5$years_passed <- race_sex_pain_studies_5$end_year - race_sex_pain_studies_5$start_year

race_sex_pain_studies_5$countries <- as.factor(ifelse(race_sex_pain_studies_5$classification=="United States"|race_sex_pain_studies_5$classification=="UNITED STATES"|
                                                        race_sex_pain_studies_5$classification=="Canada", "North America", "Others"))

##ADDING COUNTRIES 
countries <- select(aact_countries, "nct_id", "name")
colnames(countries)[colnames(countries) == "name"] <- "Countries_name"
countries<-countries[!duplicated(countries$nct_id), ]


race_sex_pain_studies_6 <- merge(race_sex_pain_studies_5, countries, by="nct_id", all.x = T)

race_sex_pain_studies_6$Countries_name <- ifelse(race_sex_pain_studies_6$Countries_name == "NA", race_sex_pain_studies_6$classification, race_sex_pain_studies_6$Countries_name)

race_sex_pain_studies_6$countries <- as.factor(ifelse(race_sex_pain_studies_6$Countries_name=="United States"|race_sex_pain_studies_6$Countries_name=="UNITED STATES"|
                                                        race_sex_pain_studies_6$Countries_name=="Canada", "North America", "Others"))

race_sex_pain_studies_6$funding <- as.factor(ifelse(race_sex_pain_studies_6$agency_class=="Industry", "Industry", "Non-Industry"))

race_sex_pain_studies_6$start_year_binary <- as.factor(ifelse(race_sex_pain_studies_6$start_year>2013, "After 2013", "Earlier 2013"))

race_sex_pain_studies_6$start_year_binary = factor(race_sex_pain_studies_6$start_year_binary, levels=c('Earlier 2013','After 2013'))

plot(ctree(POC_percent~funding+start_year+phase+study_type+female_percent, controls = ctree_control(testtype = "Bonferroni"),
           data=subset(race_pain_NorthAmerica_2, !is.na(POC_percent))), main="Outcome = Black/African American Proportion in Pain Clinical Trials Overall")


aggregate( Black_percent ~funding+start_year_binary, data=subset(race_sex_pain_studies_6, countries=="North America"&start_year>1999), median)

summary(lm(Black_POC_percent~female_percent+funding+start_year+countries, data=race_sex_pain_studies_6))


Black_boxplot <-ggplot(data=subset(race_pain_NorthAmerica_2, !is.na(start_year_binary)), aes(x=start_year_binary, y=Black_percent, fill=funding))+
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  theme_bw()+
  scale_fill_brewer(palette="Blues", name="Funding Sources", 
                    labels=c("Industry Funded", "Non-Industry Funded"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size = 12))+
  labs(title="Black/African American People Enrollment in Pain Clinical Trials",x="Year", y = "%")+
  facet_grid(.~start_year_binary, scale="free_x", labeller = as_labeller(
    c('After 2013'="After 2013", 'Earlier 2013'="Before 2013")))

POC_boxplot <-ggplot(data=subset(race_pain_NorthAmerica_2, !is.na(start_year_binary)&countries=="North America"), aes(x=funding, y=POC_percent, fill=funding))+
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  theme_bw()+
  scale_fill_brewer(palette="Blues", name="Funding Sources", 
                    labels=c("Industry Funded", "Non-Industry Funded"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size = 12))+
  labs(title="People of Colour Enrollment in Pain Clinical Trials",x="Funding Sources", y = "%")+
  scale_x_discrete(labels=c("Industry Funded","Non-Industry Funded"))
 
Black_POC_boxplot <-ggplot(data=subset(race_pain_NorthAmerica_2, !is.na(start_year_binary)), aes(x=funding, y=Black_POC_percent, fill=funding))+
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  theme_bw()+
  scale_fill_brewer(palette="Blues", name="Funding Sources", 
                    labels=c("Industry Funded", "Non-Industry Funded"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size = 12))+
  labs(title="Black/African American and People of Colour Enrollment in Pain Clinical Trials",x="Funding Sources", y = "%")+
  scale_x_discrete(labels=c("Industry Funded","Non-Industry Funded"))

ggarrange(Black_boxplot, POC_boxplot, Black_POC_boxplot, ncol=2, nrow=2,
          labels = c("A", "B", "C"), common.legend = TRUE, legend="bottom")

ggarrange(ggarrange(Black_boxplot, POC_boxplot, ncol=2, labels = c("A", "B"), common.legend = TRUE, legend = "bottom"),
          Black_POC_boxplot,nrow=2, labels=c("A", "C"), legend="none")

#Graphin for reported race in pain clinical trials 
Race_Overall <- data.frame(
  `Race Information Included` = c("With Race Information", "Without Race Information"),
  Percentage = c(16, 84)
)

ggplot(data=Race_Overall, aes(x="", y=Percentage, fill=Race.Information.Included))+
  geom_bar(stat = "identity", width=1, color="white")+
  coord_polar("y", start=0)+
  theme_void()+
  theme(legend.position="none") +
  geom_text(aes(label = Race.Information.Included), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

pie(Race_Overall$Percentage, labels=c("With Race Information", "Without Race Information"))

ggplot(data=Pain_sample, aes(x=reported_race, y=start_year))+
  geom_boxplot()


race_sex_pain_studies_5$vi <- 0
Black_meta<-rma(Black_percent, vi, mods= ~female_percent+countries+agency_class+countries,  data=race_sex_pain_studies_5)
forest(Black_meta, slab=race_sex_pain_studies_5$nct_id)

ggplot(data=race_sex_pain_studies_6, aes(y=Black_percent, x=female_percent))+
  geom_jitter()

#Import Black Uninsured 
Black_Uninsured <- read_excel("C:/Users/giadi/Desktop/Black_Uninsured.xlsx")

ggplot(data=Black_Uninsured, aes(x=Year, y=Black_Uninsured))+
  geom_point()+
  geom_line()+
  theme_bw()+
  ylim(10, 20)+
  geom_vline(xintercept=2013, linetype="dashed", color = "red")+
  labs(title="Black/African American People Without Insurance Over Time",x="Year", y = "Percentage",
       caption="Data taken from census.gov")+
  geom_text(x=2014, y=18.6, label="Affordable Care Act Open Enrollment",
           color="red")+
  geom_segment(
    aes(x = 2014, y = 18.5, xend = 2013, yend = 18),
    data = Black_Uninsured,
    arrow = arrow(length = unit(0.03, "npc"))
  )

##Looking for fMRI via keywords
aact_keywords_2 <- aact_keywords[aact_keywords$nct_id %in% unique(race_sex_pain_studies_6$nct_id), ]

#TO see the progress of race report over time included with trials with no results reported 
Pain_sample <- filter(aact_sample, nct_id %in% common_conditions)

write.csv(Pain_sample, file="Pain_sample_CTgov.csv", na=" ")

#Unmatched rows in the final dataset 
race_sample <- filter(race_sex_pain_studies_6, nct_id %in% common_conditions)
Pain_sample_unmatched<-anti_join(race_sex_pain_studies_6, race_sample)
Pain_sample_unmatched <- filter(aact_sample, nct_id %in% Pain_sample_unmatched$nct_id)
Pain_sample_2 <- bind_rows(Pain_sample, Pain_sample_unmatched)
Pain_sample_nct <- select(Pain_sample_2, "nct_id")
PubMed_unmatched <- anti_join(pain_conditions, Pain_sample_nct, by="nct_id")
set.seed(124)
PubMed_sample <- sample_n(PubMed_unmatched, 1233)
PubMed_sample <- select(PubMed_sample, "nct_id")
PubMed_sample <- filter(aact_sample, nct_id %in% PubMed_sample$nct_id)

##REAL PUBMED
PubMed_real <- read_excel("PubMed_real.xlsx")
PubMed_real <- PubMed_real[!duplicated(PubMed_real$nct_id),]
PubMed_real <- filter(aact_sample, nct_id %in% PubMed_real$nct_id)

#Updated pain sample
Pain_sample_3 <- bind_rows(Pain_sample, PubMed_real)


Pain_sample_3 <- left_join(Pain_sample_3, countries, by="nct_id")
Pain_sample_3$reported_race <- as.factor(ifelse(Pain_sample_3$nct_id %in% race_pain_NorthAmerica_2$nct_id, 1, 0))
Pain_sample_3$start_year <- as.numeric(format(as.Date(Pain_sample_3$start_date, format="%d/%m/%Y"),"%Y"))
Pain_sample_3$study_type <- as.factor(Pain_sample_3$study_type)
Pain_sample_3$phase <- as.factor(Pain_sample_3$phase)
Pain_sample_3$countries <-as.factor(ifelse(Pain_sample_3$Countries_name=="United States"|Pain_sample_3$Countries_name=="UNITED STATES"|
                                           Pain_sample_3$Countries_name=="Canada", "North America", "Others"))


All_pain_NorthAmerica$reported_race <- as.factor(ifelse(All_pain_NorthAmerica$nct_id %in% race_pain_NorthAmerica_2$nct_id, 1, 0))

#How many trials in North America and >2000 in Pain_sample
Pain_sample_2 <- left_join(Pain_sample_2, countries, by="nct_id")

Pain_sample_2$countries <-as.factor(ifelse(Pain_sample_2$Countries_name=="United States"|Pain_sample_2$Countries_name=="UNITED STATES"|
                                             Pain_sample_2$Countries_name=="Canada", "North America", "Others"))


Pain_sample_2$start_year <- as.numeric(format(as.Date(Pain_sample_2$start_date, format="%d/%m/%Y"),"%Y"))
nrow(subset(Pain_sample_2, countries=="North America"&start_year>1999))

#How many trials in North America and >2000 in PubMed_sample
PubMed_sample <- left_join(PubMed_sample, countries, by="nct_id")

PubMed_sample$countries <-as.factor(ifelse(PubMed_sample$Countries_name=="United States"|PubMed_sample$Countries_name=="UNITED STATES"|
                                             PubMed_sample$Countries_name=="Canada", "North America", "Others"))


PubMed_sample$start_year <- as.numeric(format(as.Date(PubMed_sample$start_date, format="%d/%m/%Y"),"%Y"))
nrow(subset(PubMed_sample, countries=="North America"&start_year>1999))


#Calculate total trial each year
Total_trials <-subset(All_pain_NorthAmerica, countries=="North America") %>% group_by(start_year) %>% tally()
colnames(Total_trials)[colnames(Total_trials) == "n"] <- "Total_n"

#Calculate number of trials have reported race each year
Race_trials <-subset(All_pain_NorthAmerica, countries=="North America"&reported_race=="1") %>% group_by(start_year) %>% tally()
colnames(Race_trials)[colnames(Race_trials) == "n"] <- "Race_n"

Pain_trials <- merge(Total_trials, Race_trials, by="start_year")
Pain_trials$reported_percent <- (Pain_trials$Race_n/Pain_trials$Total_n)*100

#Percentage of trials reported race over time 
group_by(Pain_trials, start_year>1999) %>% summarize(m = mean(reported_percent))

#How many trials reported race in North America and >2000
group_by(Pain_trials, start_year>1999) %>% summarize(m = sum(Total_n))

#How many pain trials in North America and >2000 in total
nrow(subset(Pain_sample_3, countries=="North America"&start_year>1999))
Pain_sample_3_a <- merge(x=Pain_sample_3,y=aact_summaries,by="nct_id",all.x=TRUE)
Pain_sample_3_a <- subset(Pain_sample_3_a, Countries_name=="United States"&start_year>1999&study_type=="Interventional")

## DOUBLE CHECK ALL THE PAIN TRIALS
## TRIALS WITH PAIN IN TITLE 
pain_in_titles <- Pain_sample_3_a %>%
  filter(str_detect(brief_title, "Pain"))

## TRIALS WITH NO PAIN IN TITLES THEN CHECkED MANUALLY 
Pain_not_titles <- anti_join(Pain_sample_3_a, pain_in_titles, by='nct_id')
write.csv(Pain_not_titles, file="Pain_not_titles.csv", na=" ")

write.csv(Pain_sample_3_a, file="All_Pain.csv", na=" ")

All_pain_NorthAmerica <- read_excel("All_Pain_removed.xlsx")
All_pain_NorthAmerica <- All_pain_NorthAmerica[!duplicated(All_pain_NorthAmerica$nct_id), ]
#The proportion of trials reported race
392/1353

#Percentage of each race in the previous dataset (n=1433)
#race_sample <- filter(race_sex_pain_studies_6, nct_id %in% common_conditions)
#group_by(subset(race_sample,countries=="North America"), start_year) %>% summarize(m = mean(White_percent))
#group_by(subset(race_sample, countries=="North America"), start_year) %>% summarize(m = mean(Black_percent, na.rm = TRUE))
#group_by(subset(race_sample, countries=="North America"), start_year) %>% summarize(m = mean(POC_percent, na.rm = TRUE))
#group_by(subset(race_sample, countries=="North America"), start_year) %>% summarize(m = mean(Black_POC_percent, na.rm = TRUE))


#Percentage of each race in the final dataset (race_sex_pain_studies_#) 
race_NorthAmerica <- subset(race_sex_pain_studies_6, countries=="North America")

Raw_N_overtime <-dplyr::group_by(race_pain_NorthAmerica_2, start_year) %>% summarize(Total_N = sum(N_total, na.rm=TRUE))
White_overtime<-dplyr::group_by(race_pain_NorthAmerica_2, start_year) %>% summarize(White_N = sum(White_N, na.rm=TRUE))
Black_overtime  <- group_by(race_pain_NorthAmerica_2, start_year) %>% summarize(Black_N = sum(Black_N, na.rm=TRUE))
POC_overime <- group_by(race_pain_NorthAmerica_2, start_year) %>% summarize(POC_N = sum(POC_N, na.rm=TRUE))
Black_POC_overtime <- group_by(race_pain_NorthAmerica_2, start_year) %>% summarize(Black_POC_N = sum(Black_POC_N, na.rm=TRUE))

race_overtime <-Reduce(function(x, y) merge(x, y,by="start_year", all.x=TRUE), list(Raw_N_overtime,White_overtime, Black_overtime, POC_overime, Black_POC_overtime))
race_overtime$White_percent <-  (race_overtime$White_N/race_overtime$Total_N)*100
race_overtime$Black_percent <-  (race_overtime$Black_N/race_overtime$Total_N)*100
race_overtime$POC_percent <-  (race_overtime$POC_N/race_overtime$Total_N)*100


#Proportion of each race over time 
ddply(race_pain_NorthAmerica_2, .(start_year>1999), summarize, mean_White=mean(White_percent, na.rm = TRUE), median_White=median(White_percent, na.rm=TRUE),
      mean_Black=mean(Black_percent, na.rm = TRUE), median_Black=median(Black_percent, na.rm=TRUE), 
      mean_POC=mean(POC_percent, na.rm = TRUE), median_POC=median(POC_percent, na.rm=TRUE), 
      mean_Black_POC=mean(Black_POC_percent, na.rm = TRUE), median_Black_POC=median(Black_POC_percent, na.rm=TRUE))

#URP Percentage of race over the year the final dataset IN NORTH AMERICA?
plot(ctree(Black_percent~start_year, data=subset(race_overtime, !is.na(POC_percent))), main= "POC Proportion Overtime")

#URP How many trials reported race in a sample with both reported and not reported race?
All_pain_NorthAmerica_2 <-left_join(All_pain_NorthAmerica, aact_sponsor_2, by="nct_id")
All_pain_NorthAmerica_2$funding <- as.factor(ifelse(All_pain_NorthAmerica_2$agency_class=="Industry", "Industry", "Non-Industry"))
All_pain_NorthAmerica_2 <- All_pain_NorthAmerica_2[!duplicated(All_pain_NorthAmerica_2$nct_id), ]

All_pain_NorthAmerica_2$phase_2 <- as.factor(ifelse(All_pain_NorthAmerica_2$phase=="N/A", NA, All_pain_NorthAmerica_2$phase))

All_pain_NorthAmerica_2$phase_binary <-  as.factor(ifelse(All_pain_NorthAmerica_2$phase_2 == "Phase 3"|
                                                            All_pain_NorthAmerica_2$phase_2 == "Phase 4", "Late Phase", "Early Phase"))

##Updated reported race in All_pain_NorthAmerica_2 to only have in US that reports race
All_pain_US <- subset(All_pain_NorthAmerica_2, Countries_name =="United States"&study_type=="Interventional")
All_pain_US$reported_race_2 <- as.factor(ifelse(All_pain_US$nct_id %in% race_pain_US$nct_id, 1, 0))

All_pain_US$phase_binary <- as.factor(ifelse(All_pain_US$phase == "Phase 3"|
                                               All_pain_US$phase == "Phase 4", "Late Phase", 
                                             ifelse(All_pain_US$phase =="N/A", NA, "Early Phase")))

All_pain_US %>% group_by(reported_race_2, funding) %>% summarise(n=n())

plot(ctree(reported_race~start_year+funding, data=All_pain_NorthAmerica_2))

subset(All_pain_NorthAmerica_2, reported_race=="1") %>% group_by(start_year>2010) %>% tally()

#Race reporting changes over time -> create a new column in pain_trials
Pain_trials$start_year_binary <- as.factor(ifelse(Pain_trials$start_year>2010, "After 2010", "Before 2010"))
All_race_reporting <- Pain_trials %>% group_by(start_year_binary) %>% summarize(Sum_total = sum(Total_n),
                                                          Sum_race = sum(Race_n))

All_race_reporting$Sum_unreported <- All_race_reporting$Sum_total - All_race_reporting$Sum_race
All_race_reporting$percent_reported <- (All_race_reporting$Sum_race/All_race_reporting$Sum_total)*100
All_race_reporting$percent_unreported <- (All_race_reporting$Sum_unreported/All_race_reporting$Sum_total)*100

#Change to long format
All_race_reporting_long <- melt(All_race_reporting, id.vars = c("start_year_binary"),
                         measure.vars = c("percent_reported", "percent_unreported"),
                         variable.name = "Race_Reporting", value.name = "Percentage")

ggplot(data=All_race_reporting_long, aes(x=start_year_binary, y=Percentage, fill=Race_Reporting))+
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 2), labels=c("Trials reported race", "Trials did not report race"))+
  labs(title="Percentage of Race Reporting in Pain Clinical Trials In North America",x="Year", y = "%")+
  theme(legend.position = "bottom")+
  labs(fill = "Race Reporting")    

# Import race report 
Race_Report <- read_excel("Race_Report.xlsx")

colors <- c("White_total_percent" = "steelblue3", 
            "Black_total_percent" = "seagreen3", 
            "POC_total_percent" = "tan3")


ggplot(data=subset(Race_Report, start_year>2000), aes(x=start_year, y=reported_percent))+
  geom_bar(stat = "identity", fill="gray79")+
  theme_bw()+
  xlim(2000, 2020)+
  geom_point(data=Race_Report, aes(x=start_year, y=White_total_percent))+
  geom_line(data=Race_Report, aes(x=start_year, y=White_total_percent, colour="White_total_percent"), size=.75)+
  geom_point(data=Race_Report, aes(x=start_year, y=Black_total_percent))+
  geom_line(data = Race_Report, aes(x=start_year, y=Black_total_percent, colour="Black_total_percent"), size=.75)+
  geom_point(data=Race_Report, aes(x=start_year, y=POC_total_percent))+
  geom_line(data=Race_Report, aes(x=start_year, y=POC_total_percent,  colour="POC_total_percent"), size=.75)+
  labs(title="Race Reporting Over Time in Pain Clinical Trials In North America",x="Year", y = "Pain Clinical Trials, %", colour="Percenetage of Race")+
  scale_color_manual(values= c("White_total_percent" = "steelblue3", 
                               "Black_total_percent" = "seagreen3", 
                               "POC_total_percent" = "tan3"), labels=c("Black/Afrian American", 
                                                                       "People of Colour", "White/Caucasian"))+
  theme(legend.position="bottom")



Race_report_bar <- ggplot(data=Pain_trials, aes(x=start_year, y=reported_percent))+
  geom_bar(stat = "identity")+
  xlim(1999, 2020)+
  theme_bw()+
  labs(title="Race Reporting Over Time in Pain Clinical Trials In North America",x="Year", y = "Pain Clinical Trials, %")

Race_Report_long <- melt(Race_Report, id.vars = c("start_year", "reported_percent"),
                         measure.vars = c("White_mean_percent", "Black_mean_percent", "POC_mean_percent"),
                         variable.name = "Race", value.name = "Percentage")
 
Race_Report_long_full <- melt(race_overtime, id.vars = c("start_year"),
                         measure.vars = c("White_percent", "Black_percent", "POC_percent"),
                         variable.name = "Race", value.name = "Percentage")


Race_each_bar <- ggplot(data=Race_Report_long_full, aes(x=start_year, y=Percentage, fill=Race))+
  geom_bar(position = "fill", stat="identity")+
  theme_bw()+
  scale_y_continuous( labels=function(x)x*100 )+
  scale_fill_manual(values= c("White_percent" = "steelblue3", 
                               "Black_percent" = "seagreen3", 
                               "POC_percent" = "tan3"), labels=c("White/Caucasian","Black/Afrian American", 
                                                                       "People of Colour"))+
  labs(title="Percentage of Race Over Time in Pain Clinical Trials In North America",x="Year", y = "%")+
  theme(legend.position = "bottom")

ggarrange(Race_report_bar, Race_each_bar, nrow=2,
          labels = c("A", "B"), common.legend = TRUE, legend="bottom")

summary(glm(reported_race~start_year+phase, data=subset(Pain_sample, countries=="North America"&start_year>2000), family = "quasibinomial"))

lm_all <- lm(Black_percent~female_percent+phase+funding+start_year, data=subset(race_sex_pain_studies_6, countries=="North America"&start_year>2000), na.action = na.exclude)
lm_main <- lm(Black_percent~funding+start_year, data=subset(race_sex_pain_studies_6, countries=="North America"&start_year>2000), na.action = na.exclude)
Anova(lm_all, type=3)

set.seed(123)
train_lm <- train(Black_percent~funding+start_year+female_percent, data= race_sex_pain_studies_6, 
                  method="lmStepAIC", na.action = na.exclude,
                  trControl=trainControl(method="cv", number=10))

train_lm$results

summary(train_lm$finalModel)

#Types of PAIN
types_pain <-filter(aact_conditions, nct_id %in% race_sex_pain_studies_6$nct_id)

#Most prevalent pain?
tail(names(sort(table(types_pain$name))), 15)

#Subset for each pain 

#cancer
cancer_pain <- types_pain %>%
  filter(str_detect(name, 'Cancer'))

cancer_pain <- filter(race_sex_pain_studies_6, nct_id %in% cancer_pain$nct_id)

#Diabetes
diabetic_pain <- types_pain %>%
  filter(str_detect(name, 'Diabet'))

diabetic_pain <- filter(race_sex_pain_studies_6, nct_id %in% diabetic_pain$nct_id)

#Chronic
chronic_pain <- types_pain  %>%
  filter(str_detect(name, 'Chronic'))

chronic_pain <- filter(race_sex_pain_studies_6, nct_id %in% chronic_pain$nct_id)

#Heart 
heart_pain_1 <- types_pain %>%
  filter(str_detect(name, 'Cardiovascular'))

heart_pain_2 <- types_pain %>%
  filter(str_detect(name, 'Heart'))

heart_pain_3 <- types_pain %>%
  filter(str_detect(name, 'Myocardial'))

heart_pain_4 <- types_pain %>%
  filter(str_detect(name, 'Chest'))

heart_pain_5 <- types_pain %>%
  filter(str_detect(name, 'Coronary'))

heart_pain <- bind_rows(heart_pain_1, heart_pain_2, heart_pain_3, heart_pain_4, heart_pain_5)
heart_pain <- heart_pain[!duplicated(heart_pain$nct_id), ] 

heart_pain <- filter(race_sex_pain_studies_6, nct_id %in% heart_pain$nct_id)

cardiometabolic_pain <- bind_rows(diabetic_pain, heart_pain)

#Low back pain + back pain 
back_pain <- types_pain %>%
  filter(str_detect(name, 'Back'))

back_pain <- filter(race_sex_pain_studies_6, nct_id %in% back_pain$nct_id)

#Abdominal pain and pelvic pain 
pelvic_pain <- types_pain %>%
  filter(str_detect(name, 'Pelvi'))

abdominal_pain <- types_pain %>%
  filter(str_detect(name, 'Abdominal'))

pelvic_pain <- bind_rows(pelvic_pain, abdominal_pain)

pelvic_pain <- filter(race_sex_pain_studies_6, nct_id %in% pelvic_pain$nct_id)

#Postoperative 
postoperative_pain <- types_pain %>%
  filter(str_detect(name, 'Postoperative'))

postoperative_pain <- filter(race_sex_pain_studies_6, nct_id %in% postoperative_pain$nct_id)

#Depression
depression_pain <- types_pain %>%
  filter(str_detect(name, 'Depression'))

depression_pain <- filter(race_sex_pain_studies_6, nct_id %in% depression_pain$nct_id)

#acute pain 
acute_pain <- types_pain %>%
  filter(str_detect(name, 'Acute Pain'))



#Opioids
opioid <- types_pain %>%
  filter(str_detect(name, 'Opioid'))

opioid <- filter(race_sex_pain_studies_6, nct_id %in% opioid$nct_id)


# Stats to see if they change according to types of pain
summary(lm(Black_percent~start_year, data=subset(heart_pain, countries=="North America")))
plot(ctree(Black_POC_percent~start_year, data=subset(cancer_pain, !is.na(Black_POC_percent)&countries=="North America")))

#the mean and median over time
ddply(postoperative_pain, .(countries=="North America" & start_year>2000), summarize, mean=mean(Black_percent, na.rm=TRUE), median=median(Black_percent, na.rm=TRUE))

#Bar plot over time
cancer_pain_long <- melt(opioid, id.vars = c("start_year", "nct_id"),
                         measure.vars = c("White_percent", "Black_percent", "POC_percent"),
                         variable.name = "Race", value.name = "Percentage")

ggplot(data=subset(cancer_pain_long, start_year>2000), aes(x=start_year, y=Percentage, fill=Race))+
  geom_bar(position = "fill", stat="identity")+
  theme_bw()+
  xlim(2000, 2020)+
  scale_y_continuous( labels=function(x)x*100 )+
  scale_fill_manual(values= c("White_percent" = "steelblue3", 
                              "Black_percent" = "seagreen3", 
                              "POC_percent" = "tan3"), labels=c("White/Caucasian","Black/Afrian American", 
                                                                     "People of Colour"))+
  labs(title="Percenatge of Race Over Time of Opioids Use in Pain Clinical Trials In North America",x="Year", y = "%")+
  theme(legend.position = "bottom")


#Added description 
race_sex_pain_studies_7 <- merge(x=race_sex_pain_studies_6,y=aact_summaries,by="nct_id",all.x=TRUE)
pain_summaries <- filter(aact_summaries, nct_id %in% race_sex_pain_studies_6$nct_id)
pain_summaries$description <-str_trim(pain_summaries$description) 

write.csv(pain_summaries, file="pain_summaries.csv", na=" ")

#check out for interventions
pain_interventions <- filter(aact_interventions, nct_id %in% race_sex_pain_studies_6$nct_id)
pain_interventions$drugs <- as.factor(ifelse(pain_interventions$intervention_type=="Drug", 1, 0))
drugs_nct <- subset(pain_interventions, drugs==1) #all the trials with drugs in long format

#Turning column of opioids_list into a string
list<-paste0(opioids_list$ï..Opioids, collapse="|")
list_2 <- paste0("|",tolower(list))
list <- str_trim(paste(list, list_2,collapse="|"))

#Merge all the drugs names into one row and have a new column for if they take drugs
drugs_names<-aggregate(name ~ nct_id, data = pain_interventions, paste, collapse = ", ")
drugs_names$drugs <- as.factor(ifelse(drugs_names$nct_id %in% drugs_nct$nct_id, 1, 0))

race_sex_pain_studies_8 <- merge(race_sex_pain_studies_7, drugs_names, by="nct_id", all = TRUE)

nrow(subset(race_sex_pain_studies_8, countries=="North America"&drugs=="1"))
ddply(race_sex_pain_studies_8, .(countries=="North America"  & drugs=="1"), summarize, mean=mean(Black_percent, na.rm=TRUE), median=median(Black_percent, na.rm=TRUE))

#Calculate total trials that USES drugs each year
Drugs_trials <-subset(race_sex_pain_studies_8, countries=="North America"&drugs=="1") %>% group_by(start_year) %>% tally()
colnames(Drugs_trials)[colnames(Drugs_trials) == "n"] <- "Drugs_trials"

#Calculate number of trials
Total_drugs <-subset(race_sex_pain_studies_8, countries=="North America") %>% group_by(start_year) %>% tally()
colnames(Total_drugs)[colnames(Total_drugs) == "n"] <- "Total_n"

Drugs_total <- merge(Drugs_trials, Total_trials, by="start_year")
Drugs_total$reported_percent <- (Drugs_total$Drugs_trials/Drugs_total$Total_n)*100

#Percentage of trials use drugs over time 
group_by(Drugs_total) %>% summarize(m = mean(reported_percent))

Drugs_barplot <- ggplot(data=Drugs_total, aes(x=start_year, y=reported_percent))+
  geom_bar(stat = "identity")+
  theme_bw()+
  xlim(2000,2020)+
  labs(title="Proportion of Pain Clinical Trials Used Pharmaceutical Intervention",x="Year", y = "Pain Clinical Trials, %")

plot(ctree(Black_percent~start_year+funding+phase, data=subset(race_sex_pain_studies_8, !is.na(Black_percent)&countries=="North America"&drugs=="1")),
     main="Outcome = Black Percentage in Pain Clinical Trials That Use Pharmaceutical Interventions")

plot(ctree(POC_percent~start_year+funding+phase, data=subset(race_sex_pain_studies_8, !is.na(POC_percent)&countries=="North America"&drugs=="1")),
     main="Outcome = POC Percentage in Pain Clinical Trials That Use Pharmaceutical Interventions")

plot(ctree(Black_POC_percent~start_year+funding+phase, data=subset(race_sex_pain_studies_8, !is.na(Black_POC_percent)&countries=="North America"&drugs=="1")),
     main="Outcome = Black & POC Percentage Pain Clinical Trials That Use Pharmaceutical Interventions")


drugs_pain <- subset(race_sex_pain_studies_8, drugs=="1" &countries=="North America")

#Look in the description
opioid_pain <- filter(race_sex_pain_studies_8, grepl(list,description))
#Look in the name
opioid_pain_2 <- filter(race_sex_pain_studies_8, grepl(list,name))

opioid_pain_3 <- bind_rows(opioid_pain, opioid_pain_2)
opioid_pain_3 <- opioid_pain_3[!duplicated(opioid_pain_3$nct_id), ]
opioid_pain_3 <- subset(opioid_pain_3, countries=="North America")

write.csv(opioid_pain_3, file="opioids_pain.csv", na= " ")

###Insert back in excel files of opioids reporting race with fake opioids in methods 
opioid_pain_4 <- read_excel("opioids_pain_w_fake.xlsx")
opioid_pain_4 <- subset(opioid_pain_4, !methods_opioids=="0")

###Raw N only files
Raw_N <- subset(race_pain_NorthAmerica_2, select=c("nct_id", "N_total", "White_N", "Black_N", "POC_N", "Black_POC_N"))

#Added raw N into opioids trials
opioid_pain_4 <- left_join(opioid_pain_4, Raw_N, by="nct_id")

opioid_pain_4$phase_binary <-  as.factor(ifelse(opioid_pain_4$phase == "Phase 3"|
                                                opioid_pain_4$phase == "Phase 4", "Late Phase", "Early Phase"))

opioid_pain_5 <- opioid_pain_4 %>% drop_na(Black_N)

#Race reporting in pain trials using opioids
Total_opioid_trials <- Opioid_sample_2 %>% group_by(start_year) %>% tally()
colnames(Total_opioid_trials)[colnames(Total_opioid_trials) == "n"] <- "Total_opioids"

opioid_trials <-opioid_pain_4 %>% group_by(start_year) %>% tally()
colnames(opioid_trials)[colnames(opioid_trials) == "n"] <- "Race_opioids"

opioid_trials <- merge(opioid_trials, Total_opioid_trials, by="start_year")

opioid_trials$percentage <- (opioid_trials$Race_opioids/opioid_trials$Total_opioids)*100


plot(ctree(percentage~start_year, data=opioid_trials), main="Race Reporting Overtime for Pain Trials using Opioids")

#How many of them on average?
mean(opioid_trials$percentage)
group_by(opioid_trials, start_year>1999) %>% summarize(m = mean(percentage))
group_by(opioid_trials, start_year>1999) %>% summarize(sum = sum(Race_opioids))
group_by(opioid_trials, start_year>1999) %>% summarize(sum = sum(Total_opioids))


#Percentage of each race in the opioid dataset (opioid_pain_3) 
Raw_N_opioid <-dplyr::group_by(opioid_pain_4, start_year) %>% summarize(Total_N = sum(N_total, na.rm=TRUE))
White_opioid<-dplyr::group_by(opioid_pain_4, start_year) %>% summarize(White_N = sum(White_N, na.rm=TRUE))
Black_opioid  <- group_by(opioid_pain_4, start_year) %>% summarize(Black_N = sum(Black_N, na.rm=TRUE))
POC_opioid <- group_by(opioid_pain_4, start_year) %>% summarize(POC_N = sum(POC_N, na.rm=TRUE))
Black_POC_opioid <- group_by(opioid_pain_4, start_year) %>% summarize(Black_POC_N = sum(Black_POC_N, na.rm=TRUE))
opioid_overtime <-Reduce(function(x, y) merge(x, y,by="start_year", all.x=TRUE), list(Raw_N_opioid,White_opioid, Black_opioid, POC_opioid, Black_POC_opioid))
opioid_overtime$White_percent <-  (opioid_overtime$White_N/opioid_overtime$Total_N)*100
opioid_overtime$Black_percent <-  (opioid_overtime$Black_N/opioid_overtime$Total_N)*100
opioid_overtime$POC_percent <-  (opioid_overtime$POC_N/opioid_overtime$Total_N)*100

#Does each race enrollment change over time?
plot(ctree(Black_mean_percent~start_year, data=subset(opioid_overtime, start_year>1999)), main="POC Proportion in Pain Trials using Opioids")

#The proportion of each race in pain trials using opioids
ddply(opioid_pain_4, .( start_year>1999), summarize, mean=mean(Black_percent, na.rm=TRUE), median=median(Black_percent, na.rm=TRUE))

##How many opioids out of 1464 pain trials in NA and >2000
Opioid_sample <- filter(Pain_sample_3_a, grepl(list,description))


write.csv(Opioid_sample, file="All_opioids.csv", na=" ")

##Insert back in the Opioid_sample with both fake and real opioids in methods
Opioid_sample_2 <- read.csv("All_opioids.csv")
Opioid_sample_2 <- Opioid_sample_2[!duplicated(Opioid_sample_2$nct_id), ]
Opioid_sample_2 <- subset(Opioid_sample_2, !Methods_opioids=="0")

Opioid_sample_2$reported_race <- as.factor(ifelse(Opioid_sample_2$nct_id %in% opioid_pain_4$nct_id, 1, 0))
58/190

#URP
plot(ctree(reported_race~start_year, data=subset(Opioid_sample_2, !is.na(reported_race))))
plot(ctree(Black_POC_percent~start_year+funding+phase+study_type, data=subset(opioid_pain_4, !is.na(Black_POC_percent))),
     main="Black/African Amercain and POC Proportion in Pain Clinical Trials That Use opioids")


Opioids_barplot <- ggplot(data=opioid_trials, aes(x=start_year, y=percentage))+
  geom_bar(stat = "identity")+
  theme_bw()+
  xlim(2000,2020)+
  labs(title="Proportion of Pain Clinical Trials Using Opioids Analgesics in North America",x="Year", y = "Pain Clinical Trials, %")


drugs_pain_long <- melt(opioid_overtime, id.vars = c("start_year"),
                         measure.vars = c("White_percent", "Black_percent", "POC_percent"),
                         variable.name = "Race", value.name = "Percentage")

Drugs_race_barplot <- ggplot(data= drugs_pain_long, aes(x=start_year, y=Percentage, fill=Race))+
  geom_bar(position = "fill", stat="identity")+
  theme_bw()+
  xlim(2000, 2020)+
  scale_y_continuous( labels=function(x)x*100 )+
  scale_fill_manual(values= c("White_percent" = "steelblue3", 
                              "Black_percent" = "seagreen3", 
                              "POC_percent" = "tan3"), labels=c("White/Caucasian","Black/Afrian American", 
                                                                "People of Colour"))+
  labs(title="Percentage of Race Over Time in Pain Clinical Trials Using Opioids",x="Year", y = "%")+
  theme(legend.position = "bottom")

ggarrange(Opioids_barplot, Drugs_race_barplot, nrow=2,
          labels = c("A", "B"), common.legend = TRUE, legend="bottom")

opioid_pain_4$start_year_binary_opioid <- as.factor(ifelse(opioid_pain_4$start_year>2012, "After 2012", "Before 2012"))


Black_opioids_boxplot <-ggplot(data=opioid_pain_4, aes(x=start_year_binary_opioid, y=Black_percent))+
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  theme_bw()+
  theme(axis.title.x=element_blank(),
    text=element_text(size = 12))+
  labs(title="Black/African American Enrollment in Pain Clinical Trials Using Opioids",x="Funding Sources", y = "%")

POC_opioids_boxplot <-ggplot(data=opioid_pain_4, aes(x=funding, y=POC_percent, fill=funding))+
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  theme_bw()+
  scale_fill_brewer(palette="Blues", name="Funding Sources", 
                    labels=c("Industry Funded", "Non-Industry Funded"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size = 12))+
  labs(title="People of Colour Enrollment in Pain Clinical Trials Using Opioids",x="Funding Sources", y = "%")

Black_POC_opioids_boxplot <-ggplot(data=opioid_pain_4, aes(x=funding, y=Black_POC_percent, fill=funding))+
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  theme_bw()+
  scale_fill_brewer(palette="Blues", name="Funding Sources", 
                    labels=c("Industry Funded", "Non-Industry Funded"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size = 12))+
  labs(title="Black/African American and People of Colour Enrollment in Pain Clinical Trials Using Opioids",x="Funding Sources", y = "%")

ggarrange(ggarrange(Black_opioids_boxplot, POC_opioids_boxplot, ncol=2, labels = c("A", "B"), common.legend = TRUE, legend = "bottom"),
          Black_POC_opioids_boxplot,nrow=2, labels=c("A", "C"), legend="none")

race_sex_pain_studies_8$opioids <- as.factor(ifelse(race_sex_pain_studies_8$nct_id %in% opioid_pain_4$nct_id, "Opioids", "Non-opioids"))

## Dataset in North America and >2000, and removed all not about pain and included RAW N
race_pain_NorthAmerica <- subset(race_sex_pain_studies_8, countries=="North America"&start_year>1999)
write.csv(race_pain_NorthAmerica, file="race_pain_NorthAmerica", na=" ")

race_pain_NorthAmerica_2 <- read_excel("race_pain_NorthAmerica_removed.xlsx")

race_pain_NorthAmerica_2$phase_binary <- as.factor(ifelse(race_pain_NorthAmerica_2$phase == "Phase 3"|
                                                            race_pain_NorthAmerica_2$phase == "Phase 4", "Late Phase", "Early Phase"))

### Based on the excel "race_pain_NorthAmerica_remove", we have 392 rows, but in actuality only 388 rows, so use this to get rid of the extra 4
race_pain <- intersect(race_pain_NorthAmerica_2$nct_id, All_pain_NorthAmerica_2$nct_id)
race_pain_NorthAmerica_2<- filter(race_pain_NorthAmerica_2, nct_id %in% race_pain)

## ONLY IN US AND INTERVENTIONAL
race_pain_US <- subset(race_pain_NorthAmerica_2, Countries_name=="United States"&study_type=="Interventional")

plot(ctree(Black_percent~start_year+funding+phase+study_type+female_percent, data=subset(race_pain_NorthAmerica_3, !is.na(Black_percent))),
     main="Black/African American and POC Proportion in Pain Clinical Trials")

## Added another excel by types_pain from Kip
race_pain_NorthAmerica_3 <-left_join(race_pain_NorthAmerica_2, types_pain, by="nct_id")

race_pain_NorthAmerica_3 <- subset(race_pain_NorthAmerica_3, !pain_type=="not pain?")

#All Black NA is 46, All "not pain?" is 20, "not pain?" AND Black NA is 7 => 46+20-6=60
race_pain_NorthAmerica_3 <- race_pain_NorthAmerica_3 %>% drop_na(Black_N)

types_pain <- read_excel("types_pain.xlsx")

race_pain_NorthAmerica_3$opioids_2 <- as.factor(ifelse(race_pain_NorthAmerica_3$nct_id %in% opioid_pain_5$nct_id, "1", "0"))

race_pain_NorthAmerica_4 <- subset(race_pain_NorthAmerica_3, Countries_name =="United States"&study_type=="Interventional")

write.csv(race_pain_NorthAmerica_4, file="race_pain_NorthAmerica_4.csv", na=" ")
## Descriptive stats 

descriptive <-table1(~study_type+funding+phase+start_year+female_percent+White_percent+Black_percent+POC_percent+Black_POC_percent, data=opioid_pain_4)
write.csv(descriptive, file="descrip.csv")


## OR of Black in Industry funding vs Non-industry
Input =("
Race	     Industry	  Non-Industry
Non-Black	  27103	     40264
Black	      4146	     10955  

")


## OR of Black in time
Input =("
Race 	        Before-2010	After-2010
Non-Black	    33219	      34148
Black	        7136	      7965

")

## OR of Black in Phases
Input =("
Race           Early-phase   Late-phase 
Non-Black       6595            40006  
Black           1205            7622         
  ")


#Race reporting in time  in All_pain_US
Input =("
Race              Before-2010  After-2010
Non-reported       342          376
reported	         149          333

")

#### Race reporting in  Industry  in All_pain_US
Input =("
Race              Industry  Non-Industry
Non-reported       180         538
reported	         129         353

")

### Race reporting in Phase in All_pain_US
Input =("
Race              Early       Late
Non-reported       147        272
reported	         91         191

")

##opioids
Input =("
Race            Non-opiods    Opiods
Non-Black       43435         7718
Black	          8825          1871

")

## Race reporting in Total N in All_pain_US_2
Input =("
Race            small           large
Non-reported     655            63
reported	       401            81

")

## Black participation across different N
Input =("
Race           Small            Large 
Non-Black       27059           40308  
Black           5764            9337        
  ")


Matrix.1 = as.matrix(read.table(textConnection(Input),
                                header=TRUE,
                                row.names=1))



oddsratio(Matrix.1)
cohenW(Matrix.1,
       digits = 2)

esc_rpb(grp1n = 3448, grp2n = 12562, p=.001, es.type="d")

Industry <- subset(race_pain_NorthAmerica_3, race_pain_NorthAmerica_3$funding=="Industry")
Non_Industry <- subset(race_pain_NorthAmerica_3, race_pain_NorthAmerica_3$funding=="Non-Industry")

### Titles and ID from race_pain_NorthAmerica_3
titles_race_pain <- subset(race_pain_NorthAmerica_3, select=c("nct_id", "official_title"))

titles_race_pain$types_pain <- as.factor(ifelse(str_detect(titles_race_pain$official_title, "Chronic"), 1, 0))
write.csv(titles_race_pain, file = "titles_race_pain.csv", na=" ")

### Another content review from Kip
not_pain_Kip <- subset(types_pain, pain_type=="not pain?")

##Pain types vs population
pain_type <- race_pain_NorthAmerica_3 %>% group_by(pain_type) %>% summarise(Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N))
pain_type <- as.data.frame(pain_type)
pain_type$percentage <- (pain_type$Black_N/pain_type$Total_N)*100
pain_type$Non_Black_N <- (pain_type$Total_N - pain_type$Black_N)

pain_type_plot <-ggplot(data=pain_type, aes(x=pain_type, y=percentage, fill=pain_type))+
  geom_bar(stat="identity")+
  geom_hline(yintercept = 12.8, size=1, colour="blue")+
  scale_fill_brewer(palette="PuBuGn", name="Pain Type")+
  annotate("text", x=5.2, y=13.2, label= "National Average")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size = 12))+
  theme(legend.position = "bottom")+
  labs(title="Black/African American Participants Proportion Across Different Type of Pain Trials",x="Pain Type", y = "% Black Participants")

pain_type_population <- race_pain_NorthAmerica_3 %>% group_by(special_population) %>% summarise(Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N))
pain_type_population <- as.data.frame(pain_type_population)
pain_type_population$percentage <- (pain_type_population$Black_N/pain_type_population$Total_N)*100
pain_type_population$Non_Black_N <- (pain_type_population$Total_N - pain_type_population$Black_N)
pain_type_population <- na.omit(pain_type_population)

pain_population_plot<-ggplot(data=pain_type_population, aes(x=special_population, y=percentage, fill=special_population))+
  geom_bar(stat="identity")+
  geom_hline(yintercept = 12.8, size=1, colour="blue")+
  scale_fill_brewer(palette="PuBuGn", name="Special Population")+
  annotate("text", x=6.2, y=13.5, label= "National Average")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size = 12))+
  theme(legend.position = "bottom")+
  labs(title="Black/African American Participants Proportion Across Different Populations", y = "% Black Participants")

ggarrange(pain_type_plot, pain_population_plot, nrow=2,
          labels = c("A", "B"), legend="bottom")

pain_funding <- race_pain_NorthAmerica_3 %>% group_by(funding, start_year<2010) %>% summarise(Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N))
pain_funding <- as.data.frame(pain_funding)
pain_funding$percentage <- (pain_funding$Black_N/pain_funding$Total_N)*100
pain_funding$Non_Black_N <- (pain_funding$Total_N - pain_funding$Black_N)

ggplot(data=pain_funding, aes(x=funding, y=percentage, fill=funding))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Blues", name="Funding")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size = 12))+
  facet_grid(.~`start_year < 2010`)
  theme(legend.position = "bottom")+
  labs(title="Black/African American Participants Proportion Across Different Funding", y = "% Black Participants")

pain_type_phase <- race_pain_NorthAmerica_3 %>% group_by(phase_binary, pain_type) %>% summarise(Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N))
pain_type_phase <- as.data.frame(pain_type_phase)
pain_type_phase$percentage <- (pain_type_phase$Black_N/pain_type_phase$Total_N)*100
pain_type_phase$Non_Black_N <- (pain_type_phase$Total_N - pain_type_phase$Black_N)
write.csv(pain_type_phase, file="pain_type_phase.csv", na= " ")

pain_type_time <- race_pain_NorthAmerica_3 %>% group_by(start_year<2010, pain_type) %>% summarise(Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N))
pain_type_time <- as.data.frame(pain_type_time)
pain_type_time$percentage <- (pain_type_time$Black_N/pain_type_time$Total_N)*100
colnames(pain_type_time)[colnames(pain_type_time) == "start_year < 2010"] <- "Time"
pain_type_time$Time <- as.factor(ifelse(pain_type_time$Time == FALSE, "After 2010", "Before 2010"))
pain_type_time$Non_Black_N <- (pain_type_time$Total_N - pain_type_time$Black_N)
write.csv(pain_type_time, file="pain_type_time.csv", na= " ")

phase_time <- race_pain_NorthAmerica_3 %>% group_by(start_year<2010, phase_binary) %>% summarise(Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N))
colnames(phase_time)[colnames(phase_time) == "start_year < 2010"] <- "Time"
phase_time$Time <- as.factor(ifelse(phase_time$Time == FALSE, "After 2010", "Before 2010"))
phase_time <- na.omit(phase_time)
phase_time$percentage <-  (phase_time$Black_N/phase_time$Total_N)*100
phase_time$Time<-relevel(phase_time$Time, "Before 2010")

phase_time_plot <- ggplot(data=phase_time, aes(x=Time, y=percentage, fill=Time))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Reds", name="Time")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size = 12))+
  facet_grid(.~phase_binary)+
  theme(legend.position = "bottom")+
  labs(title="Black/African American Participants Proportion Across Different Phases Before and After 2010", y = "% Black Participants")

phase_funding <- race_pain_NorthAmerica_3 %>% group_by(funding, phase_binary) %>% summarise(Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N))
phase_funding <- na.omit(phase_funding)
phase_funding$percentage <-  (phase_funding$Black_N/phase_funding$Total_N)*100

funding_phase_plot <-  ggplot(data=phase_funding, aes(x=funding, y=percentage, fill=funding))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Blues", name="Funding")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size = 12))+
  facet_grid(.~phase_binary)+
  theme(legend.position = "bottom")+
  labs(title="Black/African American Participants Proportion Across Different Funding Before and After 2010", y = "% Black Participants")

ggarrange(funding_phase_plot, phase_time_plot, nrow=2, labels=c("A", "B"))

funding_only <- race_pain_NorthAmerica_3 %>% group_by(funding) %>% summarise(Total_N=sum(N_total, na.rm=TRUE), 
                                                                             Black_N=sum(Black_N), percentage=(Black_N/Total_N)*100)

funding_only_plot <- ggplot(data=funding_only, aes(x=funding, y=percentage, fill=funding))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Blues", name="Funding")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size = 12))+
  theme(legend.position = "bottom")+
  labs(title="Black/African American Participants Proportion Across Different Funding", y = "% Black Participants")

time_only <- race_pain_NorthAmerica_3 %>% group_by(start_year<2010) %>% summarise(Total_N=sum(N_total, na.rm=TRUE),
                                                                                  Black_N=sum(Black_N), percentage=(Black_N/Total_N)*100)

colnames(time_only)[colnames(time_only) == "start_year < 2010"] <- "Time"
time_only$Time <- as.factor(ifelse(time_only$Time == FALSE, "After 2010", "Before 2010"))
time_only$Time<-relevel(time_only$Time, "Before 2010")

time_only_plot <-ggplot(data=time_only, aes(x=Time, y=percentage, fill=Time))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Reds", name="Time")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size = 12))+
  theme(legend.position = "bottom")+
  labs(title="Black/African American Participants Proportion Before and After 2010", y = "% Black Participants")

phase_only <-  race_pain_NorthAmerica_3 %>% group_by(phase_binary) %>% summarise(Total_N=sum(N_total, na.rm=TRUE),
                                                                                    Black_N=sum(Black_N), percentage=(Black_N/Total_N)*100)

phase_only_plot <- ggplot(data=na.omit(phase_only), aes(x=phase_binary, y=percentage, fill=phase_binary))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Purples", name="Phase")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size = 12))+
  theme(legend.position = "bottom")+
  labs(title="Black/African American Participants Proportion Across Different Phases", y = "% Black Participants")

ggarrange(ggarrange(funding_only_plot, time_only_plot, ncol=2, labels = c("A", "B"), legend = "bottom"),
          phase_only_plot,nrow=2, labels=c("A", "C"), legend="bottom")

## Meta analysis 

Meta_all <- metaprop(Black_N, N_total, studlab=nct_id, sm="PLOGIT", data=race_pain_NorthAmerica_19, method="GLMM", method.tau="ML")
summary(Meta_all)


pdf(file = "meta_forest.pdf", width = 10, height = 25)

forest.meta(Meta_all, layout="RevMan5", xlab="Proportion", psize=0.1, 
       comb.r=T, comb.f=F, xlim = c(0,1), fontsize=10, digits=3, 
       label.right = "More Black Participants", col.label.right = "green", 
       label.left = "Fewer Black Participants", col.label.left = "red")

dev.off()

Meta_all_reg <-metareg(Meta_all, ~bi_year+funding+phase_binary)

drapery(Meta_all, 
        labels = "studlab",
        type = "pval", 
        legend = FALSE)

Meta_acute <- metaprop(Black_N, N_total, studlab=nct_id, sm="PLOGIT", data=subset(race_pain_NorthAmerica_4, pain_type=="acute"), method="GLMM", method.tau="ML")
Meta_chronic <- metaprop(Black_N, N_total, studlab=nct_id, sm="PLOGIT", data=subset(race_pain_NorthAmerica_4, pain_type=="chronic"), method="GLMM", method.tau="ML")
Meta_cancer <- metaprop(Black_N, N_total, studlab=nct_id, sm="PLOGIT", data=subset(race_pain_NorthAmerica_4, pain_type=="cancer"), method="GLMM", method.tau="ML")
Meta_cardio <- metaprop(Black_N, N_total, studlab=nct_id, sm="PLOGIT", data=subset(race_pain_NorthAmerica_4, pain_type=="cardio"), method="GLMM", method.tau="ML")
Meta_palla <- metaprop(Black_N, N_total, studlab=nct_id, sm="PLOGIT", data=subset(race_pain_NorthAmerica_4, pain_type=="pallative"), method="GLMM", method.tau="ML")
Meta_kids <- metaprop(Black_N, N_total, studlab=nct_id, sm="PLOGIT", data=subset(race_pain_NorthAmerica_4, special_population=="kids"), method="GLMM", method.tau="ML")
Meta_female <- metaprop(Black_N, N_total, studlab=nct_id, sm="PLOGIT", data=subset(race_pain_NorthAmerica_4, special_population=="female"), method="GLMM", method.tau="ML")

funnel(Meta_all, studlab = TRUE)
grid.text("My custom title", .5, .9, gp=gpar(cex=2))
metabias(Meta_all)
regtest(Meta_all)

Kip <- read_excel("Kip.xlsx")
kip_meta <- metaprop(Portion, Total, studlab=ID, sm="PFT", data=Kip, method="Inverse", method.tau="DL")
forest(kip_meta, layout="RevMan5", xlab="Proportion", 
       comb.r=T, comb.f=F, xlim = c(0,1), fontsize=10, digits=3)

ggplot(data=race_pain_NorthAmerica_4, aes(y=White_percent, x=N_total))+
  geom_point()+
  xlim(0,200)+
  ylim(0,100)

mar9 <- metaprop(Black, Total, studlab=Study, sm="PLOGIT", data=Mar_9_Data, method="GLMM", method.tau="ML")

## Some unreported trials DID actually report RACE so I need to double check 
Unreported_US <- subset(All_pain_US, reported_race_2=="0")

### PREDICT BLACK ON UNREPORTTED TRIALS
race_pain_NorthAmerica_4$cancer_2 <- as.factor(ifelse(race_pain_NorthAmerica_4$pain_type == "cancer", "cancer", "non-cancer"))
race_pain_NorthAmerica_4$chronic_2 <- as.factor(ifelse(race_pain_NorthAmerica_4$pain_type == "chronic", "chronic", "non-chronic"))
race_pain_NorthAmerica_4$kids_2 <- as.factor(ifelse(is.na(race_pain_NorthAmerica_4$special_population), "non-kids",
                                                    ifelse(race_pain_NorthAmerica_4$special_population == "kids", "kids", "non-kids")))
race_pain_NorthAmerica_4$females_2 <- as.factor(ifelse(is.na(race_pain_NorthAmerica_4$special_population), "non-female",
                                                       ifelse(race_pain_NorthAmerica_4$special_population == "female", "female", "non-female")))
race_pain_NorthAmerica_4$elderly_2 <- as.factor(ifelse(is.na(race_pain_NorthAmerica_4$special_population), "non-elderly",
                                                       ifelse(race_pain_NorthAmerica_4$special_population == "elderly", "elderly", "non-elderly")))
race_pain_NorthAmerica_4$veterans_2 <- as.factor(ifelse(is.na(race_pain_NorthAmerica_4$special_population), "non-veterans",
                                                       ifelse(race_pain_NorthAmerica_4$special_population == "veterans", "veterans", "non-veterans")))

#Unreported race results
Unreported_results <- left_join(Unreported_US, aact_race, by="nct_id")

Unreported_results <- Unreported_results %>%
  filter(str_detect(title, paste(c("Race")))) %>%
  filter(!str_detect(title, "Race and Ethnicity Not Collected")) %>% 
  select(nct_id, title, ctgov_group_code, classification, category, param_value) 

Unreported_results<-as.data.frame(unique(Unreported_results$nct_id))
write.csv(Unreported_results, file="Unreported_results.csv", na=" ")

## Adding in race/Black participants manually
Unreported_results <- read_excel("Unreported_results.xlsx")

Unreported_results<- left_join(Unreported_results, All_pain_US, by="nct_id")

#Update the Reported race proportions
All_pain_US$reported_race_3 <- as.factor(ifelse(All_pain_US$nct_id %in% race_pain_US$nct_id, 1, 
                                                ifelse(All_pain_US$nct_id %in% Unreported_results$nct_id, 1, 0)))


## Binary start year
All_pain_US$bi_year <- as.factor(ifelse(All_pain_US$start_year<2011, "Before", "After"))


### Intersecting names
Unreported_results <- select(Unreported_results, intersect(names(Unreported_results), names(race_pain_NorthAmerica_4)))

##New dataframe with race reporting 
race_pain_US_2 <- bind_rows(race_pain_US, Unreported_results)

race_pain_US_2$Black_percent_2 <- (race_pain_US_2$Black_N/race_pain_US_2$N_total)*100
## New dataframe with only  Black participants
race_pain_NorthAmerica_5 <- race_pain_US_2[!is.na(race_pain_US_2$Black_N), ]

### Add in pain type, population for the new race_pain_NorthAmerica_5, then double check in real life
race_4_updates <- select(race_pain_NorthAmerica_4, "nct_id", "phase_binary", "pain_type", "special_population")
race_5_updates <- select(race_pain_NorthAmerica_5, "nct_id", "phase_binary", "pain_type", "special_population")
race_6_updates <- anti_join(race_5_updates, race_4_updates, by="nct_id")
race_6_updates <- rbind(race_4_updates, race_6_updates)

race_pain_NorthAmerica_5 <- left_join(race_pain_NorthAmerica_5, race_6_updates, by="nct_id")
write_xlsx(race_pain_NorthAmerica_5, "race_pain_NorthAmerica_5.xlsx")

## The new updated unreported trials!!!!
Unreported_US_2 <- subset(All_pain_US, reported_race_3=="0")

## Count for unreported results
Unreported_count <- filter(aact_baseline, nct_id %in% Unreported_US_2$nct_id)

#How to have only total group number in for each trial
Unreported_count$group <- sub(".", "", Unreported_count$ctgov_group_code)
Unreported_max_group <-aggregate(group ~ nct_id, Unreported_count, max)

#Total number of participants for unreported trials + still looking for the total number in pubmed
Unreported_count_2 <- left_join(Unreported_max_group, Unreported_count, by=c("nct_id", "group"))

Unreported_pubmed_count <-anti_join(Unreported_US_2, Unreported_max_group, by="nct_id")

#DUPLICATE IN UNREPORTED_COUNT_2
Unreported_count_2<-Unreported_count_2[!duplicated(Unreported_count_2$nct_id),]

##Check the total N in unreported pubmed count manually 
Unreported_pubmed_id <- select(Unreported_pubmed_count, "nct_id")
write_xlsx(Unreported_pubmed_id, "Unreported_pubmed_id.xlsx")
Unreported_pubmed <- read_excel("Unreported_pubmed_id.xlsx")

Unreported_pubmed_2 <- select(Unreported_pubmed, "nct_id", "count")
Unreported_count_3 <- select(Unreported_count_2, "nct_id", "count")
Unreported_count_4 <- rbind(Unreported_count_3, Unreported_pubmed_2)

### The TOTAL N in the UNREPORTED RACE TRIALS
Unreported_US_3 <- merge(Unreported_US_2, Unreported_count_4, by="nct_id")
colnames(Unreported_US_3)[colnames(Unreported_US_3) == "count"] <- "N_total"

## NO NA in PHASE BINARY --> THIS IS FOR PREDICTIVE MODEL
Unreported_US_noPhaseNA <- subset(Unreported_US_3, !is.na(phase_binary))

## Pain facilities 
Pain_facilities <- filter(aact_facilities, nct_id %in% All_pain_US$nct_id)
Pain_facilities$name <- stri_trans_totitle(Pain_facilities$name)

##How many of trials have multiple locations?
Pain_facilities_2<-aggregate(.~nct_id, Pain_facilities, paste, collapse=",")
Pain_facilities_2$multiple_locations <- as.factor(ifelse(grepl(",", Pain_facilities_2$state), "yes", "no"))
Pain_facilities_2 <- select(Pain_facilities_2, "nct_id", "multiple_locations")

All_pain_US <- merge(All_pain_US, Pain_facilities_2, by="nct_id", all.x = TRUE)

race_pain_US_2 <- left_join(race_pain_US_2, Pain_facilities_2, by="nct_id")
race_pain_NorthAmerica_5 <- left_join(race_pain_NorthAmerica_5, Pain_facilities_2, by="nct_id")
Unreported_US_3 <- left_join(Unreported_US_3, Pain_facilities_2, by="nct_id")

ggplot(data=subset(race_pain_NorthAmerica_5, !is.na(multiple_locations)), aes(x=multiple_locations, y=Black_percent_2))+
  geom_boxplot()+
  ylim(0,200)

race_pain_NorthAmerica_5 %>% group_by(multiple_locations) %>% summarise(Black_N=mean(Black_percent_2), total_N=sum(N_total, na.rm = TRUE))

##Remove duplicated states and merge them again
## Number of Black Ppl in each state is taken from worldpopulationreview
Race_States <- read.csv("Race_States.csv")
histogram(Race_States$BlackPerc)

Pain_facilities_3 <- Pain_facilities[!duplicated(Pain_facilities[ , c("nct_id","state")]),]
Pain_facilities_3 <- merge(Pain_facilities_3, Race_States, by.x="state", by.y="ï..State")
Pain_facilities_3<-aggregate(.~nct_id, Pain_facilities_3, paste, collapse=",")
Pain_facilities_3$Location_Black_Percent <- sapply(strsplit(Pain_facilities_3$BlackPerc, ','), function(x) mean(as.numeric(x)))

Pain_States <- select(Pain_facilities, "nct_id", "state")

Location_Black <- select(Pain_facilities_3, "nct_id", "Location_Black_Percent")

race_pain_US_2 <- left_join(race_pain_US_2, Location_Black, by="nct_id")
race_pain_NorthAmerica_5 <- left_join(race_pain_NorthAmerica_5, Location_Black, by="nct_id")
Unreported_US_3 <- left_join(Unreported_US_3, Location_Black, by="nct_id")

ggplot(data=subset(race_pain_NorthAmerica_5, !is.na(Location_Black_Percent)), aes(x=Location_Black_Percent, y=Black_percent_2))+
  geom_jitter()
##Race states with raw N
Race_States_N <- read.csv("Race_States_N.csv")
histogram(Race_States_N$Black)

Pain_facilities_4 <- Pain_facilities[!duplicated(Pain_facilities[ , c("nct_id","state")]),]
Pain_facilities_4 <- merge(Pain_facilities_4, Race_States_N, by.x="state", by.y="ï..State")
Pain_facilities_4<-aggregate(.~nct_id, Pain_facilities_4, paste, collapse=",")
Pain_facilities_4$Location_Black_N <- sapply(strsplit(Pain_facilities_4$Black, ','), function(x) mean(as.numeric(x)))

Location_Black_N <- select(Pain_facilities_4, "nct_id", "Location_Black_N")

race_pain_US_2 <- left_join(race_pain_US_2, Location_Black_N, by="nct_id")
race_pain_NorthAmerica_5 <- left_join(race_pain_NorthAmerica_5, Location_Black_N, by="nct_id")
Unreported_US_3 <- left_join(Unreported_US_3, Location_Black_N, by="nct_id")

ggplot(data=subset(race_pain_NorthAmerica_5, !is.na(Location_Black_Percent)), aes(y=Location_Black_N, x=Black_N))+
  geom_jitter()+
  facet_zoom(xlim=c(0,500))

##Determine private vs public facilities from Homeland Infrastructure Data (HIFLD)
## Hopsitals + Colleges and Universities Datasets
## https://hifld-geoplatform.opendata.arcgis.com/datasets/geoplatform::colleges-and-universities/about

Hospitals <- read.csv("Hospitals.csv")
Hospitals_2 <- select(Hospitals, "NAME", "OWNER")
Hospitals_2$NAME <- tolower(Hospitals_2$NAME)
Hospitals_2$NAME <- stri_trans_totitle(Hospitals_2$NAME) 
Hospitals_2$NAME <- word(Hospitals_2$NAME,1,sep = "\\-")
Hospitals_2$NAME <- str_trim(Hospitals_2$NAME, "right")

Universities <- read.csv("Colleges_and_Universities.csv")
Universities_2 <- select(Universities, "NAME", "TYPE")
Universities_2$NAME <- tolower(Universities_2$NAME)
Universities_2$NAME <- stri_trans_totitle(Universities_2$NAME)
Universities_2$NAME <- word(Universities_2$NAME,1,sep = "\\-")
Universities_2$NAME <- str_trim(Universities_2$NAME, "right")

Pain_facilities_hospitals <-  merge(Pain_facilities, Hospitals_2, by.x = "name",  by.y = "NAME", all.x = TRUE)
Pain_facilities_hospitals_2 <-  merge(Pain_facilities_hospitals, Universities_2, by.x = "name",  by.y = "NAME", all.x = TRUE)

Pain_facilities_hospitals_2 <- unite(Pain_facilities_hospitals_2, Nature, OWNER, TYPE , na.rm = TRUE, sep= " ")
  
write_xlsx(Pain_facilities_hospitals_2, "Pain_facilities_hospitals_2.xlsx")

##Turn them into wide format
Pain_facilities_hospitals_3 <-aggregate(.~nct_id, Pain_facilities_hospitals_2, paste, collapse=",", na.action = na.pass)

## Get the facilities for one location
one_location <- subset(Pain_facilities_2, multiple_locations=="no")

one_location<-filter(Pain_facilities_hospitals_3, nct_id %in% one_location$nct_id)

write_xlsx(one_location, "one_location.xlsx")

## get the facilities for multiple locations
multiple_locations <- subset(Pain_facilities_2, multiple_locations=="yes")

multiple_locations <-filter(Pain_facilities_hospitals_3, nct_id %in% multiple_locations$nct_id)

write_xlsx(multiple_locations, "multiple_locations.xlsx")

##Import them back up!
one_location_updated <- read_excel("one_location_updated.xlsx")
multiple_locations_updated <- read_excel("multiple_locations_updated.xlsx")

##Cleaning data
one_location_updated$Nature <- sapply(strsplit(one_location_updated$Nature,","), `[`, 1)

stopwords <- c("- LOCAL", "- DISTRICT/AUTHORITY", "- STATE", "- FEDERAL")
one_location_updated$Nature <- gsub(paste0(stopwords,collapse = "|"),"", one_location_updated$Nature)

one_location_updated$Nature <- ifelse(one_location_updated$Nature == 1, "GOVERNMENT", 
                                      ifelse(one_location_updated$Nature == 2, "PROPRIETARY",one_location_updated$Nature))

multiple_locations_updated$Nature <- sapply(strsplit(multiple_locations_updated$Nature,","), `[`, 1)

multiple_locations_updated$Nature <- ifelse(multiple_locations_updated$Nature == 1, "GOVERNMENT", 
                                      ifelse(multiple_locations_updated$Nature == 2, "PROPRIETARY",multiple_locations_updated$Nature))

Pain_facilities_nature <- bind_rows(one_location_updated, multiple_locations_updated)

Pain_facilities_nature <- select(Pain_facilities_nature, "nct_id", "Nature")

Pain_facilities_nature$Nature_2 <- as.factor(ifelse(Pain_facilities_nature$Nature == "GOVERNMENT", "Government", "Non-Government"))


race_pain_NorthAmerica_6 <- left_join(race_pain_NorthAmerica_5, Pain_facilities_nature, by="nct_id")

race_pain_NorthAmerica_6$Nature <-str_trim(race_pain_NorthAmerica_6$Nature) 

race_pain_NorthAmerica_6$kids_2 <- as.factor(ifelse(is.na(race_pain_NorthAmerica_6$special_population), "non-kids",
                                                    ifelse(race_pain_NorthAmerica_6$special_population == "kids", "kids", "non-kids")))

race_pain_NorthAmerica_6$Nature[race_pain_NorthAmerica_6$Nature==""]<-NA

race_pain_NorthAmerica_6$Nature_2 <- as.factor(ifelse(race_pain_NorthAmerica_6$Nature == "GOVERNMENT", "Government", "Non-Government"))

race_pain_NorthAmerica_6$female_2 <- as.factor(ifelse(is.na(race_pain_NorthAmerica_6$special_population), "non-female",
                                                    ifelse(race_pain_NorthAmerica_6$special_population == "female", "female", "non-female")))


race_pain_NorthAmerica_6$veterans_2 <- as.factor(ifelse(is.na(race_pain_NorthAmerica_6$special_population), "non-veterans",
                                                      ifelse(race_pain_NorthAmerica_6$special_population == "veterans", "veterans", "non-veterans")))

### Transportation from Federal Transit Administration 
### Vehicles, Metrics, Track & Roadway 2019
## https://www.transit.dot.gov/ntd/ntd-data?field_product_type_target_id=All&year=all&combine=&page=0

Transportation_States <- read_excel("Transportation.xlsx")
Race_Transportation <- left_join(Race_States_N, Transportation_States, by = c("ï..State"="State"))

Race_Transportation$Person_Trips <- Race_Transportation$Unlinked_Passenger_Trips/Race_Transportation$Total

Pain_Transportation <- merge(Pain_States, Race_Transportation, by.x="state", by.y="ï..State")
Pain_Transportation<-aggregate(.~nct_id, Pain_Transportation, paste, collapse=",")
Pain_Transportation$Mean_Trips <- sapply(strsplit(Pain_Transportation$Person_Trips, ','), function(x) mean(as.numeric(x)))
Pain_Transportation$Mean_Vehicles <- sapply(strsplit(Pain_Transportation$Total_Vehicles, ','), function(x) mean(as.numeric(x)))
Pain_Transportation$Mean_Cost <- sapply(strsplit(Pain_Transportation$Cost_per_Passenger, ','), function(x) mean(as.numeric(x)))

Trips <- select(Pain_Transportation, "nct_id", "Mean_Trips", "Mean_Vehicles", "Mean_Cost")

race_pain_NorthAmerica_7 <- left_join(race_pain_NorthAmerica_6, Trips, by="nct_id")

## Education from National Center of Education Statistics 
## Rates of high school completion and bachelor's degree attainment among persons age 25 and over, by race/ethnicity and state: 2018
## https://nces.ed.gov/programs/digest/d19/tables/dt19_104.85.asp?current=yes

Education_States <- read_excel("Education.xlsx")
Pain_Education <- merge(Pain_States, Education_States, by.x="state", by.y="State")
Pain_Education <-aggregate(.~nct_id, Pain_Education, paste, collapse=",")
Pain_Education$Mean_Black_highschool <- sapply(strsplit(Pain_Education$Black_percent_highschool, ','), function(x) mean(as.numeric(x)))
Pain_Education$Mean_Black_bachelor <- sapply(strsplit(Pain_Education$Black_percent_bachelor, ','), function(x) mean(as.numeric(x)))

Education <- select(Pain_Education, "nct_id", "Mean_Black_highschool", "Mean_Black_bachelor")

race_pain_NorthAmerica_8 <- left_join(race_pain_NorthAmerica_7, Education, by="nct_id")

## Internet access from National Center of Education Statistics 
## 	Number and percentage of households with computer and internet access, by state: 2018
## https://nces.ed.gov/programs/digest/d19/tables/dt19_702.60.asp?current=yes

Internet_States <- read_excel("Internet.xlsx")
Pain_Internet <- merge(Pain_States, Internet_States, by.x="state", by.y="State")
Pain_Internet <- aggregate(.~nct_id, Pain_Internet, paste, collapse=",")
Pain_Internet$Mean_Computer <- sapply(strsplit(Pain_Internet$Percent_computer, ','), function(x) mean(as.numeric(x)))
Pain_Internet$Mean_Internet <- sapply(strsplit(Pain_Internet$Percent_internet, ','), function(x) mean(as.numeric(x)))

Internet <- select(Pain_Internet, "nct_id", "Mean_Computer", "Mean_Internet")

race_pain_NorthAmerica_9 <- left_join(race_pain_NorthAmerica_8, Internet, by="nct_id")

## Insurance from Census.gov 
## Health Insurance Coverage in the United States: 2019
## https://www.census.gov/library/publications/2020/demo/p60-271.html
## Table HIC-4_ACS. Health Insurance Coverage Status and Type of Coverage by State All Persons: 2008 to 2019

Insurance_States <- read_excel("Insurance.xlsx")
Insurance_States <- spread(Insurance_States, Coverage, Percent)
Insurance_States <- select(Insurance_States, "State","Any coverage", "Uninsured")
colnames(Insurance_States)[colnames(Insurance_States) == "Any coverage"] <- "Any_insurance"
Pain_Insurance <- merge(Pain_States, Insurance_States, by.x="state", by.y="State")
Pain_Insurance <- aggregate(.~nct_id, Pain_Insurance, paste, collapse=",")
Pain_Insurance$Mean_insurance <- sapply(strsplit(Pain_Insurance$Any_insurance, ','), function(x) mean(as.numeric(x)))
Pain_Insurance$Mean_uninsured <- sapply(strsplit(Pain_Insurance$Uninsured, ','), function(x) mean(as.numeric(x)))

Insurance <- select(Pain_Insurance, "nct_id", "Mean_insurance", "Mean_uninsured")

race_pain_NorthAmerica_10 <- left_join(race_pain_NorthAmerica_9, Insurance, by="nct_id")


## Difference between % in clinical trials and % in location 

race_pain_NorthAmerica_10$Black_diff <- as.factor(ifelse(race_pain_NorthAmerica_10$Black_percent_2-(race_pain_NorthAmerica_10$Location_Black_Percent*100)>0,1,0))
race_pain_NorthAmerica_10$Black_diff_2 <- race_pain_NorthAmerica_10$Black_percent_2- (race_pain_NorthAmerica_10$Location_Black_Percent*100)

race_pain_NorthAmerica_10 %>% group_by(Black_diff) %>% summarise(n=n())

table1(~phase+multiple_locations+Mean_Black_highschool + Nature + phase_binary+Mean_Vehicles+kids_2|Black_diff, data=race_pain_NorthAmerica_10)

summary(tableby(multiple_locations~Black_diff_2, data=race_pain_NorthAmerica_10))

## Median Income by Race and by State
## Source: 2015-2019 American Community Survey 5-Year Estimates
## https://www.census.gov/search-results.html?q=income&page=1&stateGeo=none&searchtype=web&cssp=SERP&_charset_=UTF-8

Income_States <- read_excel("Black_Income.xlsx")
Pain_Income <- merge(Pain_States, Income_States, by.x="state", by.y="State")
Pain_Income <- aggregate(.~nct_id, Pain_Income, paste, collapse=",")
Pain_Income$Median_Black_Income <- sapply(strsplit(Pain_Income$Black_Income, ','), function(x) mean(as.numeric(x)))

Income <- select(Pain_Income, "nct_id", "Median_Black_Income")

race_pain_NorthAmerica_11 <- left_join(race_pain_NorthAmerica_10, Income, by="nct_id")

## Police Shootings Data
## Washington Post 2015-
## https://github.com/washingtonpost/data-police-shootings
## or https://fatalencounters.org/ <- from 2000 onwards

Police_States <- read.csv("fatal-police-shootings-data.csv")
Police_States_2 <- read_excel("Police_data.xlsx")
States <- read_excel("states.xlsx")

Police_States <- merge(Police_States_2, States, by.x = "State", by.y="state")
Police_States <- subset(Police_States, Race=="African-American/Black")

Police_States <- aggregate(.~region, Police_States, paste, collapse=",")
Police_States$body_count <- sapply(strsplit(Police_States$region_2, ","), length)
Police <- select(Police_States, "region", "body_count")

Pain_Police <- merge(Pain_States, Police, by.x="state", by.y="region")
Pain_Police <- aggregate(.~nct_id, Pain_Police, paste, collapse=",")
Pain_Police$Mean_body_count <- sapply(strsplit(Pain_Police$body_count, ','), function(x) mean(as.numeric(x)))

race_pain_NorthAmerica_12 <- left_join(race_pain_NorthAmerica_11, Pain_Police, by="nct_id")

## Employment Data 
## US Bureau of Labour Statistics 
## https://www.bls.gov/opub/geographic-profile/home.htm
## Table 22. People at work by gender, age, race, Hispanic or Latino ethnicity, and hours of work, 2019 annual averages
## Table 26. Unemployed people by gender, race, Hispanic or Latino ethnicity, and duration of unemployment, 2019 annual averages

Employment_States <- read_excel("Employment.xlsx")
Pain_Employment <- merge(Pain_States, Employment_States, by.x="state", by.y="State")
Pain_Employment <- aggregate(.~nct_id, Pain_Employment, paste, collapse=",")
Pain_Employment$Mean_hours_work_Black <- sapply(strsplit(Pain_Employment$Hours_work_Black, ','), function(x) mean(as.numeric(x)))
Employment <- select(Pain_Employment, "nct_id", "Mean_hours_work_Black")

Unemployment_States <- read_excel("Unemployment.xlsx")
Pain_Unemployment <- merge(Pain_States, Unemployment_States, by.x="state", by.y="State")
Pain_Unemployment <- aggregate(.~nct_id, Pain_Unemployment, paste, collapse=",")
Pain_Unemployment$Mean_weeks_unemployed_black_2 <- sapply(strsplit(Pain_Unemployment$Mean_weeks_unemployed_black, ','), function(x) mean(as.numeric(x)))
Pain_Unemployment$Median_weeks_unemployed_black_2 <- sapply(strsplit(Pain_Unemployment$Median_weeks_unemployed_black, ','), function(x) mean(as.numeric(x)))
Unemployment <- select(Pain_Unemployment, "nct_id", "Mean_weeks_unemployed_black_2", "Median_weeks_unemployed_black_2")

race_pain_NorthAmerica_13 <-Reduce(function(x, y) merge(x, y,by="nct_id", all.x=TRUE), list(race_pain_NorthAmerica_12, Employment, Unemployment))

## Housing Data
## National, State, and County Housing Unit Totals: 2010-2019
## https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-housing-units.html
## Annual Estimates of Housing Units for the United States and States: April 1, 2010 to July 1, 2019
## 2019

Housing_States <- read_excel("Housing.xlsx")
Pain_Housing <- merge(Pain_States, Housing_States, by.x="state", by.y="State")
Pain_Housing <- aggregate(.~nct_id, Pain_Housing, paste, collapse=",")
Pain_Housing$Mean_housing_units <- sapply(strsplit(Pain_Housing$Housing_Units, ','), function(x) mean(as.numeric(x)))
Housing <- select(Pain_Housing, "nct_id", "Mean_housing_units")

race_pain_NorthAmerica_14 <- left_join(race_pain_NorthAmerica_13, Housing, by="nct_id")

## Homeownership
## Table 3. Homeownership Rates by State: 2005-present 
## https://www.census.gov/housing/hvs/data/rates.html 
## 2019

Homeowner_States <- read_excel("Homeownership.xlsx")
Homeowner_States$State <- gsub("\\.", "", Homeowner_States$State)
Homeowner_States$State<-str_trim(Homeowner_States$State) 
Homeowner_States$rates_2019 <-  ((Homeowner_States$First_quarter_2019+Homeowner_States$Second_quarter_2019+
                                    Homeowner_States$Third_quarter_2019+Homeowner_States$Fourth_quarter_2019)/4)
Homeowner_States <- select(Homeowner_States, "State", "rates_2019")

Pain_Homeowner <- merge(Pain_States, Homeowner_States, by.x="state", by.y="State")
Pain_Homeowner <- aggregate(.~nct_id, Pain_Homeowner, paste, collapse=",")
Pain_Homeowner$Mean_homeowner_rates <- sapply(strsplit(Pain_Homeowner$rates_2019, ','), function(x) mean(as.numeric(x)))
Homeownership <- select(Pain_Homeowner, "nct_id", "Mean_homeowner_rates")

race_pain_NorthAmerica_15 <- left_join(race_pain_NorthAmerica_14, Homeownership, by="nct_id")

## Poverty 
## Income and Poverty in the United States: 2018
## Percentage of People in Poverty by State Using 2- and 3-Year Averages: 2015-2016 and 2017-2018
## https://www.census.gov/data/tables/2019/demo/income-poverty/p60-266.html
## Average of povery by states 2016-2018

Poverty_States <- read_excel("Poverty.xlsx")
Poverty_States$State <- gsub("\\.", "", Poverty_States$State)
Poverty_States$State<-str_trim(Poverty_States$State) 

Pain_Poverty <- merge(Pain_States, Poverty_States, by.x="state", by.y="State")
Pain_Poverty <- aggregate(.~nct_id, Pain_Poverty, paste, collapse=",")
Pain_Poverty$Mean_poverty_rates <- sapply(strsplit(Pain_Poverty$'3_years_poverty_1618', ','), function(x) mean(as.numeric(x)))
Poverty <- select(Pain_Poverty, "nct_id", "Mean_poverty_rates")

race_pain_NorthAmerica_16 <- left_join(race_pain_NorthAmerica_15, Poverty, by="nct_id")

## Race of PI 
nct_all_us <- select(All_pain_US, "nct_id")
pain_investigators <- subset(aact_officials, nct_id %in% nct_all_us$nct_id)

write_xlsx(pain_investigators, "pain_investigators.xlsx")

## Import back for race of PI
PI_race <- read_excel("pain_investigators.xlsx")
PI_race_2 <- aggregate(.~nct_id, PI_race, paste, collapse="")

PI_BIPOC <- c("POC", "B")
PI_race_2$diverse_index_1 <- str_detect(PI_race_2$perceived_race, "POC")
PI_race_2$diverse_index_2 <- str_detect(PI_race_2$perceived_race, "B")                    
PI_race_2$diverse_index <- as.factor(ifelse(PI_race_2$diverse_index_1=="TRUE"|PI_race_2$diverse_index_2=="TRUE", 1, 0))

PI_race_3 <- select(PI_race_2, "nct_id", "diverse_index")

race_pain_NorthAmerica_16$bi_year <- as.factor(ifelse(race_pain_NorthAmerica_16$start_year<2011, "Before", "After"))

race_pain_NorthAmerica_17 <- left_join(race_pain_NorthAmerica_16, PI_race_3, by="nct_id")

## Religious
## 2021, World Population Review 
## https://worldpopulationreview.com/state-rankings/most-religious-states

Religious_States <- read.csv("Religious.csv")
Pain_Religious <- merge(Pain_States, Religious_States, by.x="state", by.y="ï..State")
Pain_Religious <- aggregate(.~nct_id, Pain_Religious, paste, collapse=",")
Pain_Religious$Mean_Religious <- sapply(strsplit(Pain_Religious$religiousAdults, ','), function(x) mean(as.numeric(x)))
Religious <- select(Pain_Religious, "nct_id", "Mean_Religious")

race_pain_NorthAmerica_18 <- left_join(race_pain_NorthAmerica_17, Religious, by="nct_id")

## Political Affiliations 
## 2021, State political parties, KFF
### https://www.kff.org/other/state-indicator/state-political-parties/?currentTimeframe=0&sortModel=%7B%22colId%22:%22State%20Senate%20Majority%20Political%20Affiliation%22,%22sort%22:%22asc%22%7D

Politics_States <- read_excel("Politics.xlsx")
Pain_Politics <- merge(Pain_States, Politics_States, by.x="state", by.y="Location")
Pain_Politics <- aggregate(.~nct_id, Pain_Politics, paste, collapse=",")
Politics <- select(Pain_Politics, "nct_id", "State_Senate")

Politics$State_politics <-sapply(strsplit(Politics$State_Senate, ","), function(x) paste(rle(x)$values, collapse=","))
Politics$State_politics <- as.factor(ifelse(Politics$State_politics=="Democrat", "D", 
                                            ifelse(Politics$State_politics=="Republican", "R", "Mixed")))
Politics <- select(Politics, "nct_id", "State_politics")

race_pain_NorthAmerica_19 <- left_join(race_pain_NorthAmerica_18, Politics, by="nct_id")

### ALL pain US COUNT of participants 
reported_count <- select(race_pain_US_2, "nct_id", "N_total")
colnames(Unreported_count_4)[colnames(Unreported_count_4) == "count"] <- "N_total"

All_pain_US_count <- bind_rows(reported_count, Unreported_count_4)
All_pain_US_2 <- left_join(All_pain_US, All_pain_US_count, by="nct_id")
All_pain_US_2$N_binary <- as.factor(ifelse(All_pain_US_2$N_total>300, "large", "small"))

All_pain_US_2 %>% group_by(N_binary, reported_race_3) %>% summarise(N=n())

race_pain_NorthAmerica_19$N_binary <- as.factor(ifelse(race_pain_NorthAmerica_19$N_total>300, "large", "small"))


## What IF we should use variables that are stratified by STATE only, and not race?
## Education from National Center of Education Statistics 
## Rates of high school completion and bachelor's degree attainment among persons age 25 and over, by race/ethnicity and state: 2018
## https://nces.ed.gov/programs/digest/d19/tables/dt19_104.85.asp?current=yes

Education_total_States <- read_excel("Education_total.xlsx")
Pain_total_Education <- merge(Pain_States, Education_total_States, by.x="state", by.y="State")
Pain_total_Education <-aggregate(.~nct_id, Pain_total_Education, paste, collapse=",")
Pain_total_Education$Mean_total_bachelor <- sapply(strsplit(Pain_total_Education$Total_percent_bachelor, ','), function(x) mean(as.numeric(x)))

Education_2 <- select(Pain_total_Education, "nct_id", "Mean_total_bachelor")

race_pain_NorthAmerica_20 <- left_join(race_pain_NorthAmerica_19, Education_2, by="nct_id")


### ADDED SIGNIFICANT VARIABLES INTO UNREPORTED FILES
Unreported_US_4 <-Reduce(function(x, y) merge(x, y, by="nct_id", all.x=TRUE), list(Unreported_US_3, Internet, Pain_facilities_nature))

## UNREPORTED RACE AND TRIALS DID NOT REPORT BLACK 
Anti_US <- anti_join(All_pain_US_2, race_pain_NorthAmerica_20,by=c("nct_id"))


## ADD THEM BACK UP
Unreported_US_5 <-Reduce(function(x, y) merge(x, y, by="nct_id", all.x=TRUE), list(Anti_US, Internet, Pain_facilities_nature, Location_Black, Pain_Police))

## Dataset of clinical trials reported race but did NOT report Black 
No_Black <- anti_join(race_pain_US_2, race_pain_NorthAmerica_20,by=c("nct_id"))
No_Black <-Reduce(function(x, y) merge(x, y, by="nct_id", all.x=TRUE), list(No_Black, Internet, Pain_facilities_nature))

## Pain types 
race_pain_NorthAmerica_20$acute_2 <- as.factor(ifelse(race_pain_NorthAmerica_20$pain_type == "acute", "acute", "non-acute"))
race_pain_NorthAmerica_20$chronic_2 <- as.factor(ifelse(race_pain_NorthAmerica_20$pain_type == "chronic", "chronic", "non-chronic"))
race_pain_NorthAmerica_20$cancer_2 <- as.factor(ifelse(race_pain_NorthAmerica_20$pain_type == "cancer", "cancer", "non-cancer"))

###ADD ALL SES VARIABLES TO ALL_PAIN_US_2

All_pain_US_3 <- Reduce(function(x, y) merge(x, y, by="nct_id", all.x=TRUE), list(All_pain_US_2, Location_Black, Pain_facilities_nature, Trips, 
                                                                                  Education, Internet, Insurance, Income, Pain_Police, Employment, 
                                                                                  Unemployment, Housing, Homeownership, Poverty, PI_race_3, Religious, Politics ))
### Add quartile of continous variales: (look into summary then make it factor)
summary(All_pain_US_3$N_total)
All_pain_US_3$N_total_quartile <- as.factor(ifelse(All_pain_US_3$N_total < 33, "0",
                                                   ifelse(All_pain_US_3$N_total<76&All_pain_US_3$N_total>32, "1", 
                                                          ifelse(All_pain_US_3$N_total<162&All_pain_US_3>75, "2", "3"))))


summary(race_pain_NorthAmerica_20$N_total)
race_pain_NorthAmerica_20$N_total_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$N_total < 36, "0",
                                                   ifelse(race_pain_NorthAmerica_20$N_total<86&race_pain_NorthAmerica_20$N_total>35, "1", 
                                                          ifelse(race_pain_NorthAmerica_20$N_total<219&race_pain_NorthAmerica_20>85, "2", "3"))))


All_pain_US_3$start_year_quartile <- as.factor(ifelse(All_pain_US_3$start_year < 2005, "0",
                                                   ifelse(All_pain_US_3$start_year<2010&All_pain_US_3$start_year>2004, "1", 
                                                          ifelse(All_pain_US_3$start_year<2015&All_pain_US_3$start_year>2009, "2", "3"))))

race_pain_NorthAmerica_20$start_year_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$start_year < 2005, "0",
                                                      ifelse(race_pain_NorthAmerica_20$start_year<2010&race_pain_NorthAmerica_20$start_year>2004, "1", 
                                                             ifelse(race_pain_NorthAmerica_20$start_year<2015&race_pain_NorthAmerica_20$start_year>2009, "2", "3"))))

summary(All_pain_US_3$Location_Black_Percent)
All_pain_US_3$Location_Black_Percent_quartile <- as.factor(ifelse(All_pain_US_3$Location_Black_Percent < 0.0642, "0",
                                                      ifelse(All_pain_US_3$Location_Black_Percent<0.1214&All_pain_US_3$Location_Black_Percent>0.0641, "1", 
                                                             ifelse(All_pain_US_3$Location_Black_Percent<0.1567&All_pain_US_3$Location_Black_Percent>0.1213, "2", "3"))))

summary(race_pain_NorthAmerica_20$Location_Black_Percent)
race_pain_NorthAmerica_20$Location_Black_Percent_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$Location_Black_Percent < 0.08187, "0",
                                                                  ifelse(race_pain_NorthAmerica_20$Location_Black_Percent<0.12251&race_pain_NorthAmerica_20$Location_Black_Percent>0.08186, "1", 
                                                                         ifelse(race_pain_NorthAmerica_20$Location_Black_Percent<0.15661&race_pain_NorthAmerica_20$Location_Black_Percent>0.12250, "2", "3"))))

summary(All_pain_US_3$Mean_Trips)
All_pain_US_3$Mean_Trips_quartile <- as.factor(ifelse(All_pain_US_3$Mean_Trips < 10.032, "0",
                                                                  ifelse(All_pain_US_3$Mean_Trips<21.059&All_pain_US_3$Mean_Trips>10.031, "1", 
                                                                         ifelse(All_pain_US_3$Mean_Trips<33.378&All_pain_US_3$Mean_Trips>21.058, "2", "3"))))

summary(race_pain_NorthAmerica_20$Mean_Trips)
race_pain_NorthAmerica_20$Mean_Trips_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$Mean_Trips < 9.887, "0",
                                                      ifelse(race_pain_NorthAmerica_20$Mean_Trips<19.827&race_pain_NorthAmerica_20$Mean_Trips>9.886, "1", 
                                                             ifelse(race_pain_NorthAmerica_20$Mean_Trips<32.264&race_pain_NorthAmerica_20$Mean_Trips>19.826, "2", "3"))))

summary(All_pain_US_3$Mean_Vehicles)
All_pain_US_3$Mean_Vehicles_quartile <- as.factor(ifelse(All_pain_US_3$Mean_Vehicles < 3311, "0",
                                                      ifelse(All_pain_US_3$Mean_Vehicles<6982&All_pain_US_3$Mean_Vehicles>3310, "1", 
                                                             ifelse(All_pain_US_3$Mean_Vehicles<10461&All_pain_US_3$Mean_Vehicles>6981, "2", "3"))))

summary(race_pain_NorthAmerica_20$Mean_Vehicles)
race_pain_NorthAmerica_20$Mean_Vehicles_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$Mean_Vehicles < 3311, "0",
                                                         ifelse(race_pain_NorthAmerica_20$Mean_Vehicles<6491&race_pain_NorthAmerica_20$Mean_Vehicles>3310, "1", 
                                                                ifelse(race_pain_NorthAmerica_20$Mean_Vehicles<10376&race_pain_NorthAmerica_20$Mean_Vehicles>6490, "2", "3"))))


summary(All_pain_US_3$Mean_Cost)
All_pain_US_3$Mean_Cost_quartile <- as.factor(ifelse(All_pain_US_3$Mean_Cost < 20.38, "0",
                                                         ifelse(All_pain_US_3$Mean_Cost<22.65&All_pain_US_3$Mean_Cost>20.37, "1", 
                                                                ifelse(All_pain_US_3$Mean_Cost<23.99&All_pain_US_3$Mean_Cost>22.64, "2", "3"))))
summary(race_pain_NorthAmerica_20$Mean_Cost)
race_pain_NorthAmerica_20$Mean_Cost_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$Mean_Cost < 20.38, "0",
                                                     ifelse(race_pain_NorthAmerica_20$Mean_Cost<22.65&race_pain_NorthAmerica_20$Mean_Cost>20.37, "1", 
                                                            ifelse(race_pain_NorthAmerica_20$Mean_Cost<23.99&race_pain_NorthAmerica_20$Mean_Cost>22.64, "2", "3"))))

summary(All_pain_US_3$Mean_Black_highschool)
All_pain_US_3$Mean_Black_highschool_quartile <- as.factor(ifelse(All_pain_US_3$Mean_Black_highschool < 85.87, "0",
                                                     ifelse(All_pain_US_3$Mean_Black_highschool<86.71&All_pain_US_3$Mean_Black_highschool>85.86, "1", 
                                                            ifelse(All_pain_US_3$Mean_Black_highschool<89.11&All_pain_US_3$Mean_Black_highschool>86.70, "2", "3"))))
summary(race_pain_NorthAmerica_20$Mean_Black_highschool)
race_pain_NorthAmerica_20$Mean_Black_highschool_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$Mean_Black_highschool < 85.72, "0",
                                                                 ifelse(race_pain_NorthAmerica_20$Mean_Black_highschool<86.71&race_pain_NorthAmerica_20$Mean_Black_highschool>85.71, "1", 
                                                                        ifelse(race_pain_NorthAmerica_20$Mean_Black_highschool<88.6&race_pain_NorthAmerica_20$Mean_Black_highschool>86.70, "2", "3"))))

summary(All_pain_US_3$Mean_Black_bachelor)
All_pain_US_3$Mean_Black_bachelor_quartile <- as.factor(ifelse(All_pain_US_3$Mean_Black_bachelor < 20.87, "0",
                                                                 ifelse(All_pain_US_3$Mean_Black_bachelor<22.97&All_pain_US_3$Mean_Black_bachelor>20.86, "1", 
                                                                        ifelse(All_pain_US_3$Mean_Black_bachelor<25.11&All_pain_US_3$Mean_Black_bachelor>22.96, "2", "3"))))

summary(race_pain_NorthAmerica_20$Mean_Black_bachelor)
race_pain_NorthAmerica_20$Mean_Black_bachelor_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$Mean_Black_bachelor < 20.74, "0",
                                                               ifelse(race_pain_NorthAmerica_20$Mean_Black_bachelor<22.81&race_pain_NorthAmerica_20$Mean_Black_bachelor>20.73, "1", 
                                                                      ifelse(race_pain_NorthAmerica_20$Mean_Black_bachelor<25.11&race_pain_NorthAmerica_20$Mean_Black_bachelor>22.80, "2", "3"))))

summary(All_pain_US_3$Mean_Computer)
All_pain_US_3$Mean_Computer_quartile <- as.factor(ifelse(All_pain_US_3$Mean_Computer < 91.04, "0",
                                                               ifelse(All_pain_US_3$Mean_Computer<91.99&All_pain_US_3$Mean_Computer>91.03, "1", 
                                                                      ifelse(All_pain_US_3$Mean_Computer<93.21&All_pain_US_3$Mean_Computer>91.98, "2", "3"))))
summary(race_pain_NorthAmerica_20$Mean_Computer)
race_pain_NorthAmerica_20$Mean_Computer_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$Mean_Computer < 91.04, "0",
                                                         ifelse(race_pain_NorthAmerica_20$Mean_Computer<91.85&race_pain_NorthAmerica_20$Mean_Computer>91.03, "1", 
                                                                ifelse(race_pain_NorthAmerica_20$Mean_Computer<92.82&race_pain_NorthAmerica_20$Mean_Computer>91.84, "2", "3"))))

summary(All_pain_US_3$Mean_Internet)
All_pain_US_3$Mean_Internet_quartile <- as.factor(ifelse(All_pain_US_3$Mean_Internet < 84.72, "0",
                                                         ifelse(All_pain_US_3$Mean_Internet<85.54&All_pain_US_3$Mean_Internet>84.71, "1", 
                                                                ifelse(All_pain_US_3$Mean_Internet<88.12&All_pain_US_3$Mean_Internet>85.53, "2", "3"))))
summary(race_pain_NorthAmerica_20$Mean_Internet)
race_pain_NorthAmerica_20$Mean_Internet_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$Mean_Internet < 84.72, "0",
                                                         ifelse(race_pain_NorthAmerica_20$Mean_Internet<85.54&race_pain_NorthAmerica_20$Mean_Internet>84.71, "1", 
                                                                ifelse(race_pain_NorthAmerica_20$Mean_Internet<87.33&race_pain_NorthAmerica_20$Mean_Internet>85.53, "2", "3"))))
summary(All_pain_US_3$Mean_insurance)
All_pain_US_3$Mean_insurance_quartile <- as.factor(ifelse(All_pain_US_3$Mean_insurance < 89.96, "0",
                                                         ifelse(All_pain_US_3$Mean_insurance<92.31&All_pain_US_3$Mean_insurance>89.95, "1", 
                                                                ifelse(All_pain_US_3$Mean_insurance<94.21&All_pain_US_3$Mean_insurance>92.3, "2", "3"))))
summary(race_pain_NorthAmerica_20$Mean_insurance)
race_pain_NorthAmerica_20$Mean_insurance_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$Mean_insurance < 89.80, "0",
                                                          ifelse(race_pain_NorthAmerica_20$Mean_insurance<92.19&race_pain_NorthAmerica_20$Mean_insurance>89.79, "1", 
                                                                 ifelse(race_pain_NorthAmerica_20$Mean_insurance<94.11&race_pain_NorthAmerica_20$Mean_insurance>92.18, "2", "3"))))

summary(All_pain_US_3$Median_Black_Income)
All_pain_US_3$Median_Black_Income_quartile <- as.factor(ifelse(All_pain_US_3$Median_Black_Income < 38574, "0",
                                                          ifelse(All_pain_US_3$Median_Black_Income<43261&All_pain_US_3$Median_Black_Income>38573, "1", 
                                                                 ifelse(All_pain_US_3$Median_Black_Income<49001&All_pain_US_3$Median_Black_Income>43260, "2", "3"))))
summary(race_pain_NorthAmerica_20$Median_Black_Income)
race_pain_NorthAmerica_20$Median_Black_Income_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$Median_Black_Income < 38574, "0",
                                                               ifelse(race_pain_NorthAmerica_20$Median_Black_Income<43241&race_pain_NorthAmerica_20$Median_Black_Income>38573, "1", 
                                                                      ifelse(race_pain_NorthAmerica_20$Median_Black_Income<48558&race_pain_NorthAmerica_20$Median_Black_Income>43240, "2", "3"))))

summary(All_pain_US_3$Mean_body_count)
All_pain_US_3$Mean_body_count_quartile <- as.factor(ifelse(All_pain_US_3$Mean_body_count < 145, "0",
                                                               ifelse(All_pain_US_3$Mean_body_count<293&All_pain_US_3$Mean_body_count>144, "1", 
                                                                      ifelse(All_pain_US_3$Mean_body_count<499&All_pain_US_3$Mean_body_count>292, "2", "3"))))
summary(race_pain_NorthAmerica_20$Mean_body_count)
race_pain_NorthAmerica_20$Mean_body_count_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$Mean_body_count < 211.5, "0",
                                                           ifelse(race_pain_NorthAmerica_20$Mean_body_count<293&race_pain_NorthAmerica_20$Mean_body_count>211.4, "1", 
                                                                  ifelse(race_pain_NorthAmerica_20$Mean_body_count<411.9&race_pain_NorthAmerica_20$Mean_body_count>292, "2", "3"))))

summary(All_pain_US_3$Mean_weeks_unemployed_black_2)
All_pain_US_3$Mean_weeks_unemployed_black_2_quartile <- as.factor(ifelse(All_pain_US_3$Mean_weeks_unemployed_black_2 < 20.91, "0",
                                                           ifelse(All_pain_US_3$Mean_weeks_unemployed_black_2<23.84&All_pain_US_3$Mean_weeks_unemployed_black_2>20.9, "1", 
                                                                  ifelse(All_pain_US_3$Mean_weeks_unemployed_black_2<26.51&All_pain_US_3$Mean_weeks_unemployed_black_2>23.83, "2", "3"))))
summary(race_pain_NorthAmerica_20$Mean_weeks_unemployed_black_2)
race_pain_NorthAmerica_20$Mean_weeks_unemployed_black_2_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$Mean_weeks_unemployed_black_2 < 21.12, "0",
                                                                         ifelse(race_pain_NorthAmerica_20$Mean_weeks_unemployed_black_2<23.93&race_pain_NorthAmerica_20$Mean_weeks_unemployed_black_2>21.11, "1", 
                                                                                ifelse(race_pain_NorthAmerica_20$Mean_weeks_unemployed_black_2<26.44&race_pain_NorthAmerica_20$Mean_weeks_unemployed_black_2>23.92, "2", "3"))))
summary(All_pain_US_3$Mean_housing_units)
All_pain_US_3$Mean_housing_units_quartile <- as.factor(ifelse(All_pain_US_3$Mean_housing_units < 2928733, "0",
                                                                         ifelse(All_pain_US_3$Mean_housing_units<5388067&All_pain_US_3$Mean_housing_units>2928732, "1", 
                                                                                ifelse(All_pain_US_3$Mean_housing_units<8404382&All_pain_US_3$Mean_housing_units>5388066, "2", "3"))))
summary(race_pain_NorthAmerica_20$Mean_housing_units)
race_pain_NorthAmerica_20$Mean_housing_units_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$Mean_housing_units < 2928733, "0",
                                                              ifelse(race_pain_NorthAmerica_20$Mean_housing_units<5232870&race_pain_NorthAmerica_20$Mean_housing_units>2928732, "1", 
                                                                     ifelse(race_pain_NorthAmerica_20$Mean_housing_units<8404382&race_pain_NorthAmerica_20$Mean_housing_units>5232869, "2", "3"))))
summary(All_pain_US_3$Mean_homeowner_rates)
All_pain_US_3$Mean_homeowner_rates_quartile <- as.factor(ifelse(All_pain_US_3$Mean_homeowner_rates < 62.02, "0",
                                                              ifelse(All_pain_US_3$Mean_homeowner_rates<65.06&All_pain_US_3$Mean_homeowner_rates>62.01, "1", 
                                                                     ifelse(All_pain_US_3$Mean_homeowner_rates<68.4&All_pain_US_3$Mean_homeowner_rates>65.05, "2", "3"))))

summary(race_pain_NorthAmerica_20$Mean_homeowner_rates)
race_pain_NorthAmerica_20$Mean_homeowner_rates_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$Mean_homeowner_rates < 62.43, "0",
                                                                ifelse(race_pain_NorthAmerica_20$Mean_homeowner_rates<65.06&race_pain_NorthAmerica_20$Mean_homeowner_rates>62.42, "1", 
                                                                       ifelse(race_pain_NorthAmerica_20$Mean_homeowner_rates<68.18&race_pain_NorthAmerica_20$Mean_homeowner_rates>65.05, "2", "3"))))
summary(All_pain_US_3$Mean_poverty_rates)
All_pain_US_3$Mean_poverty_rates_quartile <- as.factor(ifelse(All_pain_US_3$Mean_poverty_rates < 10.61, "0",
                                                                ifelse(All_pain_US_3$Mean_poverty_rates<11.81&All_pain_US_3$Mean_poverty_rates>10.6, "1", 
                                                                       ifelse(All_pain_US_3$Mean_poverty_rates<12.81&All_pain_US_3$Mean_poverty_rates>11.8, "2", "3"))))
summary(race_pain_NorthAmerica_20$Mean_poverty_rates)
race_pain_NorthAmerica_20$Mean_poverty_rates_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$Mean_poverty_rates < 10.71, "0",
                                                              ifelse(race_pain_NorthAmerica_20$Mean_poverty_rates<11.81&race_pain_NorthAmerica_20$Mean_poverty_rates>10.7, "1", 
                                                                     ifelse(race_pain_NorthAmerica_20$Mean_poverty_rates<12.86&race_pain_NorthAmerica_20$Mean_poverty_rates>11.8, "2", "3"))))
summary(All_pain_US_3$Mean_Religious)
All_pain_US_3$Mean_Religious_quartile <- as.factor(ifelse(All_pain_US_3$Mean_Religious  < 0.5, "0",
                                                              ifelse(All_pain_US_3$Mean_Religious <0.54&All_pain_US_3$Mean_Religious >0.49, "1", 
                                                                     ifelse(All_pain_US_3$Mean_Religious <0.5899&All_pain_US_3$Mean_Religious >0.53, "2", "3"))))
summary(race_pain_NorthAmerica_20$Mean_Religious)
race_pain_NorthAmerica_20$Mean_Religious_quartile <- as.factor(ifelse(race_pain_NorthAmerica_20$Mean_Religious  < 0.5, "0",
                                                          ifelse(race_pain_NorthAmerica_20$Mean_Religious <0.55&race_pain_NorthAmerica_20$Mean_Religious >0.49, "1", 
                                                                 ifelse(race_pain_NorthAmerica_20$Mean_Religious <0.5951&race_pain_NorthAmerica_20$Mean_Religious >0.54, "2", "3"))))



### All pain clinical trials with time 
Pain_time <- select(All_pain_US_3, "nct_id", "start_year")
Pain_States_Time <- merge(Pain_States, Pain_time, by="nct_id")
Pain_States_Time$start_year <- substr(Pain_States_Time$start_year, 1, 4)

### Updated Computer files with both states and time 
Computer_Overtime <- read_excel("~/AK/Race/Data/Computer and Internet Use/Computer_Overtime.xlsx")
Computer_Overtime_long <- gather(Computer_Overtime, start_year, Computer, '2000':'2019', factor_key = TRUE)

Pain_Computer_State_Overtime <- merge(Pain_States_Time, Computer_Overtime_long, by=c("state", "start_year"))
Pain_Computer_State_Overtime <- aggregate(.~nct_id, Pain_Computer_State_Overtime, paste, collapse=",")

Pain_Computer_State_Overtime$Mean_Computer_Overtime <- sapply(strsplit(Pain_Computer_State_Overtime$Computer, ','), function(x) mean(as.numeric(x)))
Pain_Computer_State_Overtime <- select(Pain_Computer_State_Overtime, "nct_id", "Mean_Computer_Overtime")

### Updated Internet files with both states and time
Internet_Overtime <- read_excel("~/AK/Race/Data/Computer and Internet Use/Internet_Overtime.xlsx")
Internet_Overtime_long <- gather(Internet_Overtime, start_year, Internet, '2000':'2019', factor_key = TRUE)

Pain_Internet_State_Overtime <- merge(Pain_States_Time, Internet_Overtime_long, by=c("state", "start_year"))
Pain_Internet_State_Overtime <- aggregate(.~nct_id, Pain_Internet_State_Overtime, paste, collapse=",")

Pain_Internet_State_Overtime$Mean_Internet_Overtime <- sapply(strsplit(Pain_Internet_State_Overtime$Internet, ','), function(x) mean(as.numeric(x)))
Pain_Internet_State_Overtime <- select(Pain_Internet_State_Overtime, "nct_id", "Mean_Internet_Overtime")

### Updated Highschool files with both states and time 
Highschool_Overtime <- read_excel("~/AK/Race/Data/Education/Highschool_Overtime.xlsx")
Highschool_Overtime_long <- gather(Highschool_Overtime, start_year, Highschool, '2000':'2019', factor_key = TRUE)

Pain_Highschool_State_Overtime <- merge(Pain_States_Time, Highschool_Overtime_long, by=c("state", "start_year"))
Pain_Highschool_State_Overtime <- aggregate(.~nct_id, Pain_Highschool_State_Overtime, paste, collapse=",")

Pain_Highschool_State_Overtime$Mean_Highschool_Overtime <- sapply(strsplit(Pain_Highschool_State_Overtime$Highschool, ','), function(x) mean(as.numeric(x)))
Pain_Highschool_State_Overtime <- select(Pain_Highschool_State_Overtime, "nct_id", "Mean_Highschool_Overtime")

### Updated Bachelor files with both states and time 
Bachelor_Overtime <- read_excel("~/AK/Race/Data/Education/Bachelor_Overtime.xlsx")
Bachelor_Overtime_long <- gather(Bachelor_Overtime, start_year, Bachelor, '2000':'2019', factor_key = TRUE)

Pain_Bachelor_State_Overtime <- merge(Pain_States_Time, Bachelor_Overtime_long, by=c("state", "start_year"))
Pain_Bachelor_State_Overtime <- aggregate(.~nct_id, Pain_Bachelor_State_Overtime, paste, collapse=",")

Pain_Bachelor_State_Overtime$Mean_Bachelor_Overtime <- sapply(strsplit(Pain_Bachelor_State_Overtime$Bachelor, ','), function(x) mean(as.numeric(x)))
Pain_Bachelor_State_Overtime <- select(Pain_Bachelor_State_Overtime, "nct_id", "Mean_Bachelor_Overtime")

### Updated Employment files with both states and time 
Employment_Overtime <- read_excel("~/AK/Race/Data/Employment and Unemployment Time/Employment_Overtime.xlsx")
Employment_Overtime_long <- gather(Employment_Overtime, start_year, Employment, '2000':'2019', factor_key = TRUE)

Pain_Employment_State_Overtime <- merge(Pain_States_Time, Employment_Overtime_long, by=c("state", "start_year"))
Pain_Employment_State_Overtime <- aggregate(.~nct_id, Pain_Employment_State_Overtime, paste, collapse=",")

Pain_Employment_State_Overtime$Mean_Employment_Overtime <- sapply(strsplit(Pain_Employment_State_Overtime$Employment, ','), function(x) mean(as.numeric(x)))
Pain_Employment_State_Overtime <- select(Pain_Employment_State_Overtime, "nct_id", "Mean_Employment_Overtime")

### Updated Homeownership files with both states and time
Homeownership_Overtime <- read_excel("~/AK/Race/Data/Homeownership/Homeownership_Overtime.xlsx")
Homeownership_Overtime_long <- gather(Homeownership_Overtime, start_year, Homeownership, '2000':'2019', factor_key = TRUE)

Pain_Homeownership_State_Overtime <- merge(Pain_States_Time, Homeownership_Overtime_long, by=c("state", "start_year"))
Pain_Homeownership_State_Overtime <- aggregate(.~nct_id, Pain_Homeownership_State_Overtime, paste, collapse=",")

Pain_Homeownership_State_Overtime$Mean_Homeownership_Overtime <- sapply(strsplit(Pain_Homeownership_State_Overtime$Homeownership, ','), function(x) mean(as.numeric(x)))
Pain_Homeownership_State_Overtime <- select(Pain_Homeownership_State_Overtime, "nct_id", "Mean_Homeownership_Overtime")

### Updated Housing files with both states and time 
Housing_Overtime <- read_excel("~/AK/Race/Data/Housing/Housing_Overtime.xlsx")
Housing_Overtime_long <- gather(Housing_Overtime, start_year, Housing, '2000':'2019', factor_key = TRUE)

Pain_Housing_State_Overtime <- merge(Pain_States_Time, Housing_Overtime_long, by=c("state", "start_year"))
Pain_Housing_State_Overtime <- aggregate(.~nct_id, Pain_Housing_State_Overtime, paste, collapse=",")

Pain_Housing_State_Overtime$Mean_Housing_Overtime <- sapply(strsplit(Pain_Housing_State_Overtime$Housing, ','), function(x) mean(as.numeric(x)))
Pain_Housing_State_Overtime <- select(Pain_Housing_State_Overtime, "nct_id", "Mean_Housing_Overtime")

### Updated Insurance files with both states and time (file 1 is long, file 2 is wide)
Insurance_Overtime_2 <- read_excel("~/AK/Race/Data/Insurance Time/Insurance_Overtime_2.xlsx")
Insurance_Overtime_2_long <- gather(Insurance_Overtime_2, start_year, Insurance, '2013':'2019', factor_key = TRUE)

Insurance_Overtime_1 <- read_excel("~/AK/Race/Data/Insurance Time/Insurance_Overtime_1.xlsx")
Insurance_Overtime <- rbind(Insurance_Overtime_1, Insurance_Overtime_2_long)

Pain_Insurance_State_Overtime <- merge(Pain_States_Time, Insurance_Overtime, by=c("state", "start_year"))
Pain_Insurance_State_Overtime <- aggregate(.~nct_id, Pain_Insurance_State_Overtime, paste, collapse=",")

Pain_Insurance_State_Overtime$Mean_Insurance_Overtime <- sapply(strsplit(Pain_Insurance_State_Overtime$Insurance, ','), function(x) mean(as.numeric(x)))
Pain_Insurance_State_Overtime <- select(Pain_Insurance_State_Overtime, "nct_id", "Mean_Insurance_Overtime")

### Updated Income files with both states and time
MedianIncome_Overtime <- read_excel("~/AK/Race/Data/Median Income Time/MedianIncome_Overtime.xlsx")
MedianIncome_Overtime_long <- gather(MedianIncome_Overtime, start_year, MedianIncome, '2000':'2019', factor_key = TRUE)

Pain_MedianIncome_State_Overtime <- merge(Pain_States_Time, MedianIncome_Overtime_long, by=c("state", "start_year"))
Pain_MedianIncome_State_Overtime <- aggregate(.~nct_id, Pain_MedianIncome_State_Overtime, paste, collapse=",")

Pain_MedianIncome_State_Overtime$Mean_MedianIncome_Overtime <- sapply(strsplit(Pain_MedianIncome_State_Overtime$MedianIncome, ','), function(x) mean(as.numeric(x)))
Pain_MedianIncome_State_Overtime <- select(Pain_MedianIncome_State_Overtime, "nct_id", "Mean_MedianIncome_Overtime")

### Updated Police files with both states and time
### copying from above
Police_States_2 <- read_excel("Police_data.xlsx")
Police_States <- merge(Police_States_2, States, by.x = "State", by.y="state")
Police_States <- subset(Police_States, Race=="African-American/Black")
Police_States$start_year <- format(as.Date(Police_States$Date, format="%d/%m/%Y"),"%Y")
Police_States <- subset(Police_States, start_year!= "2020" & start_year!= "2021")

Police_States<- Police_States %>% group_by_at(vars(c(region, start_year))) %>%
  summarize_all(paste, collapse=",")

Police_States$body_count <- sapply(strsplit(Police_States$Unique_ID, ","), length)
Police_2 <- select(Police_States, "region", "start_year", "body_count")
colnames(Police_2)[colnames(Police_2) == "region"] <- "state"

Pain_Police_State_Overtime <- merge(Pain_States_Time, Police_2, by=c("state", "start_year"))
Pain_Police_State_Overtime <- aggregate(.~nct_id, Pain_Police_State_Overtime, paste, collapse=",")

Pain_Police_State_Overtime$Mean_body_Overtime <- sapply(strsplit(Pain_Police_State_Overtime$body_count, ','), function(x) mean(as.numeric(x)))
Pain_Police_State_Overtime <- select(Pain_Police_State_Overtime, "nct_id", "Mean_body_Overtime")

### Updated Population files with both states and time 
Population_Overtime <- read_excel("~/AK/Race/Data/Population/Population_Overtime.xlsx")
Population_Overtime_long <- gather(Population_Overtime, start_year, Population, '2000':'2019', factor_key = TRUE)

Pain_Population_State_Overtime <- merge(Pain_States_Time, Population_Overtime_long, by=c("state", "start_year"))
Pain_Population_State_Overtime <- aggregate(.~nct_id, Pain_Population_State_Overtime, paste, collapse=",")

Pain_Population_State_Overtime$Mean_Population_Overtime <- sapply(strsplit(Pain_Population_State_Overtime$Population, ','), function(x) mean(as.numeric(x)))
Pain_Population_State_Overtime <- select(Pain_Population_State_Overtime, "nct_id", "Mean_Population_Overtime")

### Updated Poverty files with both states and time
Poverty_Overtime <- read_excel("~/AK/Race/Data/Poverty/Poverty_Overtime.xlsx")
Poverty_Overtime_long <- gather(Poverty_Overtime, start_year, Poverty , '2000':'2019', factor_key = TRUE)

Pain_Poverty_State_Overtime <- merge(Pain_States_Time, Poverty_Overtime_long, by=c("state", "start_year"))
Pain_Poverty_State_Overtime <- aggregate(.~nct_id, Pain_Poverty_State_Overtime, paste, collapse=",")

Pain_Poverty_State_Overtime$Mean_Poverty_Overtime <- sapply(strsplit(Pain_Poverty_State_Overtime$Poverty , ','), function(x) mean(as.numeric(x)))
Pain_Poverty_State_Overtime <- select(Pain_Poverty_State_Overtime, "nct_id", "Mean_Poverty_Overtime")

### Updated Transportation files with both states and time
Transport_Overtime <- read_excel("~/AK/Race/Data/Transportation/Transport_Overtime.xlsx")
Transport_Overtime_long <- gather(Transport_Overtime, start_year, Transport , '2000':'2019', factor_key = TRUE)

Pain_Transport_State_Overtime <- merge(Pain_States_Time, Transport_Overtime_long, by=c("state", "start_year"))
Pain_Transport_State_Overtime <- aggregate(.~nct_id, Pain_Transport_State_Overtime, paste, collapse=",")

Pain_Transport_State_Overtime$Mean_Transport_Overtime <- sapply(strsplit(Pain_Transport_State_Overtime$Transport , ','), function(x) mean(as.numeric(x)))
Pain_Transport_State_Overtime <- select(Pain_Transport_State_Overtime, "nct_id", "Mean_Transport_Overtime")

### Merge all of those updated files 
Updated_variables <- Reduce(inner_join, list(Pain_Computer_State_Overtime, Pain_Internet_State_Overtime, Pain_Highschool_State_Overtime,
                                             Pain_Bachelor_State_Overtime, Pain_Employment_State_Overtime, Pain_Homeownership_State_Overtime,
                                             Pain_Housing_State_Overtime, Pain_Insurance_State_Overtime, Pain_MedianIncome_State_Overtime,
                                             Pain_Police_State_Overtime, Pain_Population_State_Overtime, Pain_Poverty_State_Overtime, 
                                             Pain_Transport_State_Overtime))

Updated_variables$Mean_Computer_Overtime_quartile <- as.factor(ifelse(Updated_variables$Mean_Computer_Overtime < 81.79, "0",
                                                                      ifelse(Updated_variables$Mean_Computer_Overtime <86.18& Updated_variables$Mean_Computer_Overtime>81.78, "1", 
                                                                             ifelse(Updated_variables$Mean_Computer_Overtime<89.26&Updated_variables$Mean_Computer_Overtime>86.17, "2", "3"))))

Updated_variables$Mean_Internet_Overtime_quartile <- as.factor(ifelse(Updated_variables$Mean_Internet_Overtime < 69.01, "0",
                                                                      ifelse(Updated_variables$Mean_Internet_Overtime <75.41& Updated_variables$Mean_Internet_Overtime>69, "1", 
                                                                             ifelse(Updated_variables$Mean_Internet_Overtime<80.12&Updated_variables$Mean_Internet_Overtime>75.4, "2", "3"))))

Updated_variables$Mean_Highschool_Overtime_quartile <- as.factor(ifelse(Updated_variables$Mean_Highschool_Overtime < 84.01, "0",
                                                                      ifelse(Updated_variables$Mean_Highschool_Overtime <85.67& Updated_variables$Mean_Highschool_Overtime>84, "1", 
                                                                             ifelse(Updated_variables$Mean_Highschool_Overtime<88.51&Updated_variables$Mean_Highschool_Overtime>85.66, "2", "3"))))


Updated_variables$Mean_Bachelor_Overtime_quartile <- as.factor(ifelse(Updated_variables$Mean_Bachelor_Overtime < 17.73, "0",
                                                                        ifelse(Updated_variables$Mean_Bachelor_Overtime <19.43& Updated_variables$Mean_Bachelor_Overtime>17.72, "1", 
                                                                               ifelse(Updated_variables$Mean_Bachelor_Overtime<23.04&Updated_variables$Mean_Bachelor_Overtime>19.42, "2", "3"))))

Updated_variables$Mean_Employment_Overtime_quartile <- as.factor(ifelse(Updated_variables$Mean_Employment_Overtime < 52.01, "0",
                                                                      ifelse(Updated_variables$Mean_Employment_Overtime <55.31& Updated_variables$Mean_Employment_Overtime>52, "1", 
                                                                             ifelse(Updated_variables$Mean_Employment_Overtime<58.87&Updated_variables$Mean_Employment_Overtime>55.3, "2", "3"))))

Updated_variables$Mean_Homeownership_Overtime_quartile <- as.factor(ifelse(Updated_variables$Mean_Homeownership_Overtime < 62.24, "0",
                                                                      ifelse(Updated_variables$Mean_Homeownership_Overtime <66.45& Updated_variables$Mean_Homeownership_Overtime>62.23, "1", 
                                                                             ifelse(Updated_variables$Mean_Homeownership_Overtime<69.59&Updated_variables$Mean_Homeownership_Overtime>66.44, "2", "3"))))

Updated_variables$Mean_Housing_Overtime_quartile <- as.factor(ifelse(Updated_variables$Mean_Housing_Overtime < 2359194, "0",
                                                                      ifelse(Updated_variables$Mean_Housing_Overtime <4472160& Updated_variables$Mean_Housing_Overtime>2359193, "1", 
                                                                             ifelse(Updated_variables$Mean_Housing_Overtime<7253418&Updated_variables$Mean_Housing_Overtime>4472159, "2", "3"))))

Updated_variables$Mean_Insurance_Overtime_quartile <- as.factor(ifelse(Updated_variables$Mean_Insurance_Overtime < 83.8, "0",
                                                                      ifelse(Updated_variables$Mean_Insurance_Overtime <87.55& Updated_variables$Mean_Insurance_Overtime>83.79, "1", 
                                                                             ifelse(Updated_variables$Mean_Insurance_Overtime<91.1&Updated_variables$Mean_Insurance_Overtime>87.54, "2", "3"))))

Updated_variables$Mean_MedianIncome_Overtime_quartile <- as.factor(ifelse(Updated_variables$Mean_MedianIncome_Overtime < 48456, "0",
                                                                      ifelse(Updated_variables$Mean_MedianIncome_Overtime <52520& Updated_variables$Mean_MedianIncome_Overtime>48455, "1", 
                                                                             ifelse(Updated_variables$Mean_MedianIncome_Overtime<59330&Updated_variables$Mean_MedianIncome_Overtime>52519, "2", "3"))))

Updated_variables$Mean_body_Overtime_quartile <- as.factor(ifelse(Updated_variables$Mean_body_Overtime < 7.01, "0",
                                                                      ifelse(Updated_variables$Mean_body_Overtime <16.53& Updated_variables$Mean_body_Overtime>7, "1", 
                                                                             ifelse(Updated_variables$Mean_body_Overtime<25.01&Updated_variables$Mean_body_Overtime>16.52, "2", "3"))))

Updated_variables$Mean_Population_Overtime_quartile <- as.factor(ifelse(Updated_variables$Mean_Population_Overtime < 842061, "0",
                                                                      ifelse(Updated_variables$Mean_Population_Overtime <1653989& Updated_variables$Mean_Population_Overtime>842060, "1", 
                                                                             ifelse(Updated_variables$Mean_Population_Overtime<2154357&Updated_variables$Mean_Population_Overtime>1653988, "2", "3"))))

Updated_variables$Mean_Poverty_Overtime_quartile <- as.factor(ifelse(Updated_variables$Mean_Poverty_Overtime < 11.65, "0",
                                                                      ifelse(Updated_variables$Mean_Poverty_Overtime <13.73& Updated_variables$Mean_Poverty_Overtime>11.64, "1", 
                                                                             ifelse(Updated_variables$Mean_Poverty_Overtime<15.31&Updated_variables$Mean_Poverty_Overtime>13.72, "2", "3"))))

Updated_variables$Mean_Transport_Overtime_quartile <- as.factor(ifelse(Updated_variables$Mean_Transport_Overtime < 7.019, "0",
                                                                      ifelse(Updated_variables$Mean_Transport_Overtime <9.171& Updated_variables$Mean_Transport_Overtime>7.018, "1", 
                                                                             ifelse(Updated_variables$Mean_Transport_Overtime<10.834&Updated_variables$Mean_Transport_Overtime>9.17, "2", "3"))))



### Merge with pain files 
race_pain_NorthAmerica_21 <- left_join(race_pain_NorthAmerica_20, Updated_variables, by="nct_id")

### Merge with All pain files 

All_pain_US_4 <- left_join(All_pain_US_3, Updated_variables, by="nct_id")

### updated elderly 

race_pain_NorthAmerica_21$elderly_2 <- as.factor(ifelse(is.na(race_pain_NorthAmerica_21$special_population), "non-elderly",
                                                       ifelse(race_pain_NorthAmerica_21$special_population == "elderly", "elderly", "non-elderly")))

race_pain_NorthAmerica_21$acute_2 <- relevel(race_pain_NorthAmerica_21$acute_2, ref = "non-acute")
race_pain_NorthAmerica_21$chronic_2 <- relevel(race_pain_NorthAmerica_21$chronic_2, ref = "non-chronic")
race_pain_NorthAmerica_21$cancer_2 <- relevel(race_pain_NorthAmerica_21$cancer_2, ref = "non-cancer")
race_pain_NorthAmerica_21$kids_2 <- relevel(race_pain_NorthAmerica_21$kids_2, ref = "non-kids")
race_pain_NorthAmerica_21$female_2 <- relevel(race_pain_NorthAmerica_21$female_2, ref = "non-female")
race_pain_NorthAmerica_21$veterans_2 <- relevel(race_pain_NorthAmerica_21$veterans_2, ref = "non-veterans")
race_pain_NorthAmerica_21$elderly_2 <- relevel(race_pain_NorthAmerica_21$elderly_2, ref = "non-elderly")

### Maybe change Black percent into binary factor?
race_pain_NorthAmerica_21$Black_percent_binary <- as.factor(ifelse(race_pain_NorthAmerica_21$Black_percent_2 < "12.16", "0", "1"))

race_pain_NorthAmerica_21 %>% group_by(diverse_index) %>% summarise(n=n(),Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N))

whatev <- race_pain_NorthAmerica_21 %>% select(diverse_index, Black_N)
tbl_summary(whatev)


#### Update Black population per state with percentages 
Total_Population_Overtime <- read_excel("~/AK/Race/Data/Population/Total_Population_Overtime.xlsx")
Total_Population_Overtime_long <- gather(Total_Population_Overtime, start_year, Population, '2000':'2019', factor_key = TRUE)
Total_Population_Overtime_long <- merge(Population_Overtime_long, Total_Population_Overtime_long, by = c("state", "start_year")) 
Total_Population_Overtime_long$Black_percent_Population  <- (Total_Population_Overtime_long$Population.x/Total_Population_Overtime_long$Population.y)*100

Pain_Total_Population_State_Overtime <- merge(Pain_States_Time, Total_Population_Overtime_long, by=c("state", "start_year"))
Pain_Total_Population_State_Overtime <- aggregate(.~nct_id, Pain_Total_Population_State_Overtime, paste, collapse=",")

Pain_Total_Population_State_Overtime$Mean_Black_Percent_Population_Overtime <- sapply(strsplit(Pain_Total_Population_State_Overtime$Black_percent_Population, ','), function(x) mean(as.numeric(x)))
Pain_Total_Population_State_Overtime$Mean_Black_Percent_Population_Overtime_quartile <- as.factor(ifelse(Pain_Total_Population_State_Overtime$Mean_Black_Percent_Population_Overtime < 6.115, "0",
                                                                        ifelse(Pain_Total_Population_State_Overtime$Mean_Black_Percent_Population_Overtime <11.572& Pain_Total_Population_State_Overtime$Mean_Black_Percent_Population_Overtime>6.114, "1", 
                                                                               ifelse(Pain_Total_Population_State_Overtime$Mean_Black_Percent_Population_Overtime<14.41&Pain_Total_Population_State_Overtime$Mean_Black_Percent_Population_Overtime>11.571, "2", "3"))))



Pain_Total_Population_State_Overtime <- dplyr::select(Pain_Total_Population_State_Overtime, "nct_id", "Mean_Black_Percent_Population_Overtime", "Mean_Black_Percent_Population_Overtime_quartile")

race_pain_NorthAmerica_22 <- left_join(race_pain_NorthAmerica_21, Pain_Total_Population_State_Overtime, by="nct_id")

race_pain_NorthAmerica_22$special_population_2 <- ifelse(is.na(race_pain_NorthAmerica_22$special_population)|race_pain_NorthAmerica_22$special_population =="healthy", 
                             "general", race_pain_NorthAmerica_22$special_population)
race_pain_NorthAmerica_22$special_population_2<- relevel(factor(race_pain_NorthAmerica_22$special_population_2), ref = "general")



### What exactly is studying in women-only trials?
women_only <- subset(race_pain_NorthAmerica_22, special_population_2=="female")
women_only <- dplyr::select(women_only, "nct_id", "pain_type", "special_population")
write_xlsx(women_only, "women_only.xlsx")

women_only_2<- read_excel("women_only.xlsx")
women_only_2 %>% group_by(study) %>% summarize(percent=(n()/72)*100, Black=(Black_N/N_total)*100)

Black_N <- dplyr::select(race_pain_NorthAmerica_22, "nct_id","Black_N", "N_total")
women_only_3 <- left_join(women_only_2, Black_N, by="nct_id")
women_only_3 %>% group_by(study) %>% summarize(trials=n(),percent_trials=(n()/72)*100, Black=sum(Black_N), Total=sum(N_total), percent_Black=Black/Total)


opioids_factors <-race_pain_NorthAmerica_22 %>% group_by(opioids, pain_type) %>% summarize(percent_trials=n(), Black=sum(Black_N), Total=sum(N_total), percent_Black=Black/Total)


### Extract files to upload 
write.csv(All_pain_US_4, "All_pain_master_file.csv")
write.csv(race_pain_NorthAmerica_22, "Pain_w_Black.csv")
