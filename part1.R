

# load + copy the data---------------------------------------------------
library(tidyverse)
library(tidyselect)
library(plyr)
library(stargazer)
dfa <- read_csv("psam_pusa.csv")
#create a copy of dfa just in case
dfa_copy <- dfa


# selecting variables -----------------------------------------------------
dfa_subset = dfa %>% 
  select(WAGP, AGEP, SEX, COW, RAC1P, ENG, CIT, SCHL, NATIVITY, MIGSP)

# dropping NAs, filtering WAGP, SCHL, COW--------------------------------------------------------
dfa_subset = dfa_subset %>%
  drop_na() %>%
  filter(WAGP>0) %>% 
  filter(str_detect(SCHL, 
                    '01|16|19|21|22|23|24')) %>% 
  filter(str_detect(COW,
                    '1|2|3|4|7')) 



# creating variables ------------------------------------------------------
dfa_subset = dfa_subset %>%
  mutate(lwages = log(WAGP)) %>% 
  mutate(NATIVITY = factor(NATIVITY)) %>% 
  mutate(CIT = factor(CIT)) %>% 
  mutate(COW = factor(COW)) %>% 
  mutate(SEX = factor(SEX)) %>% 
  mutate(RAC1P = factor(RAC1P)) %>% 
  mutate(ENG = factor(ENG)) %>% 
  mutate(SCHL = factor(SCHL))
  


# renaming the factors ----------------------------------------------------
levels(dfa_subset$NATIVITY) <- c("Native", "Foreign")
levels(dfa_subset$SCHL) <- c("no_educ", "high_school", "some_college", "bachelors", "masters", "professional", "phd")
levels(dfa_subset$COW) <- c("for_profit", "non_profit", "local_gvt", "state_gvt", "self_emp")
levels(dfa_subset$SEX) <- c("male", "female")
levels(dfa_subset$ENG) <- c("very_well", "well", "not_well", "not_at_all")

# running the regression --------------------------------------------------
immi.wages <- lm(lwages ~ NATIVITY + AGEP + SEX + COW + SCHL 
                 + ENG, data = dfa_subset)
summary(immi.wages)

stargazer(immi.wages, type = "text", style = "default", title = "Table 1: Results", object.names = TRUE, no.space = FALSE, 
          intercept.bottom = FALSE)

# summary tables ----------------------------------------------------------
cow_nativity_meanwages = dfa_subset %>% 
  group_by(COW, NATIVITY) %>%
  dplyr::summarise(mean_wages = mean(WAGP)) %>%
  arrange(desc(mean_wages))

cow_nativity_medwages = dfa_subset %>% 
  group_by(COW, NATIVITY) %>%
  dplyr::summarise(median_wages = median(WAGP)) %>%
  arrange(desc(median_wages))

cow_nativity_avglogwages = dfa_subset %>% 
  group_by(COW, NATIVITY) %>%
  dplyr::summarise(avg_log_wages = mean(lwages)) %>%
  arrange(desc(avg_log_wages))

educ_nativity_meanwages = dfa_subset %>% 
  group_by(SCHL, NATIVITY) %>%
  dplyr::summarise(mean_wages = mean(WAGP)) %>% 
  arrange(desc(mean_wages))

educ_nativity_avglogwages = dfa_subset %>% 
  group_by(SCHL, NATIVITY) %>% 
  dplyr::summarise(avg_log_wages = mean(lwages)) %>%
  arrange(desc(avg_log_wages))

# plotting wages against wages, separated by nativity ---------------------------------------------
ggplot(cow_nativity_meanwages, aes(COW, mean_wages)) + 
  geom_col() + 
  facet_wrap(~NATIVITY) +
  xlab("Class of Worker") +
  ylab("Mean of Wages")

ggplot(cow_nativity_medwages, aes(COW, median_wages)) + 
  geom_col() + 
  facet_wrap(~NATIVITY) +
  xlab("Class of Worker") +
  ylab("Median of Wages")

cowlognat <- ggplot(cow_nativity_avglogwages, aes(COW, avg_log_wages))
cowlognat +  geom_col(fill = "#bc912c", color = "gray") + 
  facet_wrap(~NATIVITY) +
  xlab("Class of Worker") +
  ylab("Log of Wages")

ggplot(educ_nativity_meanwages, aes(SCHL, mean_wages, colour = "#bc912c")) +
  geom_col() +
  facet_wrap(~NATIVITY) +
  xlab("Highest Level of Education Attained") +
  ylab("Mean of Wages")

ggplot(educ_nativity_avglogwages, aes(SCHL, avg_log_wages)) +
  geom_col(aes(colour = "#bc912c")) +
  facet_wrap(~NATIVITY) +
  xlab("Highest Level of Education Attained") +
  ylab("Average Logarithm of Wages")

testcolorplot <- ggplot(educ_nativity_avglogwages, aes(SCHL, avg_log_wages, colour = "#bc912c"))

testcolorplot + geom_col(fill = "#bc912c", color = "gray") +
  facet_wrap(~NATIVITY) +
  xlab("Highest Level of Education Attained") +
  ylab("Average Logarithm of Wages")
