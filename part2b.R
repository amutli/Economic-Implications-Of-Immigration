
# find out average age for all immigrants -----------------
mean.age = dfa_subset_q2 %>% 
  group_by(POBP) %>% 
  dplyr::summarise(mean.age = mean(AGEP))

# Mexico --------------------------------------------------
mexico = dfa_subset_q2 %>% 
  filter(str_detect(POBP, "Mexico"))

names(which.max(table(mexico$SCHL)))# bachelors
names(which.max(table(mexico$SEX))) # female
names(which.max(table(mexico$ENG))) # very_well
names(which.max(table(mexico$COW))) # for_profit

int = coef(wages.POBP)[1]
beta_mex = coef(wages.POBP)[2]
beta_age = coef(wages.POBP)[22]
beta_bach = coef(wages.POBP)[14]
beta_female = coef(wages.POBP)[21]
beta_for_profit = 0
beta_very_well = 0

log.expected.wages.Mexico = int + beta_mex*1 + beta_age*42.4 + beta_bach*1 + beta_very_well*1 + beta_female*1 + beta_for_profit*1
expected.wages.Mexico = exp(log.expected.wages.Mexico) # $39,741


# China -------------------------------------------------------------------
china = dfa_subset_q2 %>% 
  filter(str_detect(POBP, "China"))

names(which.max(table(china$SCHL)))# bachelors
names(which.max(table(china$SEX))) # male
names(which.max(table(china$ENG))) # very_well
names(which.max(table(china$COW))) # for_profit

int = coef(wages.POBP)[1]
beta_china = coef(wages.POBP)[3]
beta_age = coef(wages.POBP)[22]
beta_bach = coef(wages.POBP)[14]
beta_female = coef(wages.POBP)[21]
beta_for_profit = 0
beta_very_well = 0

log.expected.wages.China = int + beta_china*1 + beta_age*48 + beta_bach*1 + beta_very_well*1 + beta_female*0 + beta_for_profit*1
expected.wages.China = exp(log.expected.wages.China) # $53,412

# India -------------------------------------------------------------------
india = dfa_subset_q2 %>% 
  filter(str_detect(POBP, "India"))

names(which.max(table(india$SCHL)))# bachelors
names(which.max(table(india$SEX))) # male
names(which.max(table(india$ENG))) # very_well
names(which.max(table(india$COW))) # for_profit

int = coef(wages.POBP)[1]
beta_india = coef(wages.POBP)[4]
beta_age = coef(wages.POBP)[22]
beta_bach = coef(wages.POBP)[14]
beta_female = coef(wages.POBP)[21]
beta_for_profit = 0
beta_very_well = 0

log.expected.wages.India = int + beta_india*1 + beta_age*46 + beta_bach*1 + beta_very_well*1 + beta_female*0 + beta_for_profit*1
expected.wages.India = exp(log.expected.wages.India) # $53,796

# Philippines -------------------------------------------------------------
philippines = dfa_subset_q2 %>% 
  filter(str_detect(POBP, "Philippines"))

names(which.max(table(philippines$SCHL)))# bachelors
names(which.max(table(philippines$SEX))) # female
names(which.max(table(philippines$ENG))) # very_well
names(which.max(table(philippines$COW))) # for_profit

int = coef(wages.POBP)[1]
beta_philippines = coef(wages.POBP)[5]
beta_age = coef(wages.POBP)[22]
beta_bach = coef(wages.POBP)[14]
beta_female = coef(wages.POBP)[21]
beta_for_profit = 0
beta_very_well = 0

log.expected.wages.Phil = int + beta_philippines*1 + beta_age*48 + beta_bach*1 + beta_very_well*1 + beta_female*1 + beta_for_profit*1
expected.wages.Phil = exp(log.expected.wages.Phil) # $40,623


# Vietnam -----------------------------------------------------------------
vietnam = dfa_subset_q2 %>% 
  filter(str_detect(POBP, "Vietnam"))

names(which.max(table(vietnam$SCHL)))# high school
names(which.max(table(vietnam$SEX))) # male
names(which.max(table(vietnam$ENG))) # very_well
names(which.max(table(vietnam$COW))) # for_profit

int = coef(wages.POBP)[1]
beta_vietnam = coef(wages.POBP)[7]
beta_age = coef(wages.POBP)[22]
beta_hs = coef(wages.POBP)[12]
beta_female = coef(wages.POBP)[21]
beta_for_profit = 0
beta_very_well = 0

log.expected.wages.Vietnam = int + beta_vietnam*1 + beta_age*41 + beta_hs*1 + beta_very_well*1 + beta_female*0 + beta_for_profit*1
expected.wages.Vietnam = exp(log.expected.wages.Vietnam) # $28,633

# Korea -------------------------------------------------------------------
korea = dfa_subset_q2 %>% 
  filter(str_detect(POBP, "Korea"))

names(which.max(table(korea$SCHL)))# high school
names(which.max(table(korea$SEX))) # male
names(which.max(table(korea$ENG))) # very_well
names(which.max(table(korea$COW))) # for_profit

int = coef(wages.POBP)[1]
beta_korea = coef(wages.POBP)[10]
beta_age = coef(wages.POBP)[22]
beta_hs = coef(wages.POBP)[12]
beta_female = coef(wages.POBP)[21]
beta_for_profit = 0
beta_very_well = 0

log.expected.wages.Korea = int + beta_korea*1 + beta_age*47 + beta_hs*1 + beta_very_well*1 + beta_female*0 + beta_for_profit*1
expected.wages.Korea = exp(log.expected.wages.Korea) # $27,354

# Cuba --------------------------------------------------------------------
cuba = dfa_subset_q2 %>% 
  filter(str_detect(POBP, "Cuba"))

names(which.max(table(cuba$SCHL)))# high school
names(which.max(table(cuba$SEX))) # male
names(which.max(table(cuba$ENG))) # very_well
names(which.max(table(cuba$COW))) # for_profit

int = coef(wages.POBP)[1]
beta_cuba = coef(wages.POBP)[8]
beta_age = coef(wages.POBP)[22]
beta_hs = coef(wages.POBP)[12]
beta_female = coef(wages.POBP)[21]
beta_for_profit = 0
beta_very_well = 0

log.expected.wages.Cuba = int + beta_cuba*1 + beta_age*43 + beta_hs*1 + beta_very_well*1 + beta_female*0 + beta_for_profit*1
expected.wages.Cuba =exp(log.expected.wages.Cuba) # $30,161

# DR ----------------------------------------------------------------------
dr = dfa_subset_q2 %>% 
  filter(str_detect(POBP, "DR"))

names(which.max(table(dr$SCHL)))# high school
names(which.max(table(dr$SEX))) # male
names(which.max(table(dr$ENG))) # very_well
names(which.max(table(dr$COW))) # for_profit

int = coef(wages.POBP)[1]
beta_dr = coef(wages.POBP)[9]
beta_age = coef(wages.POBP)[22]
beta_hs = coef(wages.POBP)[12]
beta_female = coef(wages.POBP)[21]
beta_for_profit = 0
beta_very_well = 0

log.expected.wages.DR = int + beta_dr*1 + beta_age*40 + beta_hs*1 + beta_very_well*1 + beta_female*0 + beta_for_profit*1
expected.wages.DR = exp(log.expected.wages.DR) # $28,431

# El Salvador -------------------------------------------------------------
el_salvador= dfa_subset_q2 %>% 
  filter(str_detect(POBP, "El_Salvador"))

names(which.max(table(el_salvador$SCHL)))# bachelors
names(which.max(table(el_salvador$SEX))) # female
names(which.max(table(el_salvador$ENG))) # very_well
names(which.max(table(el_salvador$COW))) # for_profit

int = coef(wages.POBP)[1]
beta_el_salvador = coef(wages.POBP)[6]
beta_age = coef(wages.POBP)[22]
beta_hs = coef(wages.POBP)[12]
beta_female = coef(wages.POBP)[21]
beta_for_profit = 0
beta_very_well = 0

log.expected.wages.El_Salvador = int + beta_el_salvador*1 + beta_age*45 + beta_bach*1 + beta_very_well*1 + beta_female*1 + beta_for_profit*1
expected.wages.El_Salvador = exp(log.expected.wages.El_Salvador) # $43,392

# Guatemala ---------------------------------------------------------------
guatemala = dfa_subset_q2 %>% 
  filter(str_detect(POBP, "Guatemala"))

names(which.max(table(guatemala$SCHL)))# high school
names(which.max(table(guatemala$SEX))) # female
names(which.max(table(guatemala$ENG))) # very_well
names(which.max(table(guatemala$COW))) # for_profit

int = coef(wages.POBP)[1]
beta_guatemala = coef(wages.POBP)[11]
beta_age = coef(wages.POBP)[22]
beta_hs = coef(wages.POBP)[12]
beta_female = coef(wages.POBP)[21]
beta_for_profit = 0
beta_very_well = 0

log.expected.wages.Guatemala = int + beta_guatemala*1 + beta_age*43 + beta_hs*1 + beta_very_well*1 + beta_female*1 + beta_for_profit*1
expected.wages.Guatemala = exp(log.expected.wages.Guatemala) # $17,862


# plot expected wages distribution ----------------------------------------
wages = c(expected.wages.China, expected.wages.Cuba, expected.wages.DR, expected.wages.El_Salvador,
                     expected.wages.Guatemala, expected.wages.India, expected.wages.Korea, expected.wages.Mexico,
                     expected.wages.Phil, expected.wages.Vietnam)

val_lab(wages) <- c("China" = 53412.17, "Mexico" = 39741.01, "India" = 53796.52, "Philippines" = 40623.09, 
            "El_Salvador" = 43392.29, "Vietnam" = 28633.73, "Cuba" = 30161.50, "DR" = 28431.94 , 
            "Korea" = 27354.4, "Guatemala" = 17862.66)


