library(gtsummary)
library(dplyr)

#### Logistic Regression for Race Reporting 
uni_glm_quartile <- tbl_uvregression(
  All_pain_master_file[c( "N_total_quartile","funding" , "start_year_quartile", "phase_binary" , "diverse_index" , "multiple_locations" , "Nature_2", "reported_race_3")],
  method = glm,
  y = reported_race_3, 
  method.args = list(family = binomial),
  exponentiate = TRUE
)

glm_outcomes_all_quartile <- glm(reported_race_3 ~ N_total_quartile + funding + start_year_quartile + phase_binary + diverse_index + multiple_locations + Nature_2 , data=All_pain_master_file, family="binomial")

multiple_glm_quartile <- tbl_regression(glm_outcomes_all_quartile, exponentiate = TRUE)

table_glm <- tbl_merge(
  tbls = list(uni_glm_quartile, multiple_glm_quartile),                          # combine
  tab_spanner = c("**Univariate**", "**Multivariable**"))%>%
  modify_header(label ~ "**Variables**") %>%
  bold_labels() %>%
  italicize_levels() %>% as_tibble()


### Linear Regression for Black Participation 
uni_lm_quartile_only_pain_2 <- tbl_uvregression(
  Pain_w_Black[c("pain_type","special_population_2","opioids","funding" , "start_year_quartile", "phase_binary" , "diverse_index" , "multiple_locations" , "Nature_2",
                              "Black_percent_2")],
  method = lm,
  y = Black_percent_2
)  %>% bold_p(t=0.05)

lm_quartile_internal_pain <- lm(Black_percent_2~pain_type+special_population_2+opioids+funding+start_year_quartile+phase_binary+diverse_index+multiple_locations+Nature_2, data=Pain_w_Black)
lm_quartile_internal_pain_tbl <- tbl_regression(lm_quartile_internal_pain) %>% bold_p(t=0.05)

tbl_merge(
  tbls = list(uni_lm_quartile_only_pain_2, lm_quartile_internal_pain_tbl),                          # combine
  tab_spanner = c("**Univariate**", 
                  "**Pain and Internal Characteristics Multivariable**" )) # set header names

### In multi-variable linear regression, the minority was NA due to complete case removal (because they only have 4 trials).

### Descriptive tables for race reporting 

All_pain_master_file %>% group_by(N_total_quartile, reported_race_3) %>% summarise(n=n())

All_pain_master_file %>% group_by(funding, reported_race_3) %>% summarise(n=n())

All_pain_master_file %>% group_by(start_year_quartile, reported_race_3) %>% summarise(n=n())

All_pain_master_file %>% group_by(phase_binary, reported_race_3) %>% summarise(n=n())

All_pain_master_file %>% group_by(diverse_index, reported_race_3) %>% summarise(n=n())

All_pain_master_file %>% group_by(multiple_locations, reported_race_3) %>% summarise(n=n())

All_pain_master_file %>% group_by(Nature_2, reported_race_3) %>% summarise(n=n())

### Descriptive tables for Black %
Pain_w_Black %>% group_by(pain_type) %>% 
  summarise(n=n(),Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N), Black_per = (Black_N/Total_N)*100, Non_Black = Total_N - Black_N, Non_Black_per = 100-Black_per)

Pain_w_Black %>% group_by(special_population_2) %>% 
  summarise(n=n(),Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N), Black_per = (Black_N/Total_N)*100, Non_Black = Total_N - Black_N, Non_Black_per = 100-Black_per)

Pain_w_Black %>% group_by(opioids) %>% 
  summarise(n=n(),Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N), Black_per = (Black_N/Total_N)*100, Non_Black = Total_N - Black_N, Non_Black_per = 100-Black_per)

Pain_w_Black %>% group_by(diverse_index) %>% 
  summarise(n=n(),Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N), Black_per = (Black_N/Total_N)*100, Non_Black = Total_N - Black_N, Non_Black_per = 100-Black_per)

Pain_w_Black %>% group_by(funding) %>% 
  summarise(n=n(),Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N), Black_per = (Black_N/Total_N)*100, Non_Black = Total_N - Black_N, Non_Black_per = 100-Black_per)

Pain_w_Black %>% group_by(multiple_locations) %>% 
  summarise(n=n(),Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N), Black_per = (Black_N/Total_N)*100, Non_Black = Total_N - Black_N, Non_Black_per = 100-Black_per)

Pain_w_Black %>% group_by(start_year_quartile) %>% 
  summarise(n=n(),Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N), Black_per = (Black_N/Total_N)*100, Non_Black = Total_N - Black_N, Non_Black_per = 100-Black_per)

Pain_w_Black %>% group_by(phase_binary) %>% 
  summarise(n=n(),Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N), Black_per = (Black_N/Total_N)*100, Non_Black = Total_N - Black_N, Non_Black_per = 100-Black_per)

Pain_w_Black %>% group_by(Nature_2) %>% 
  summarise(n=n(),Total_N=sum(N_total, na.rm=TRUE), Black_N=sum(Black_N), Black_per = (Black_N/Total_N)*100, Non_Black = Total_N - Black_N, Non_Black_per = 100-Black_per)


