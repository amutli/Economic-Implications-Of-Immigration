
# selecting new variables -------------------------------------------------
dfa_subset_q2 = dfa %>% 
  select(WAGP, AGEP, SEX, ENG, COW, SCHL, NATIVITY, POBP)
  
# create a copy -----------------------------------------------------------
dfa_q2_copy <- dfa_subset_q2

# creating variables ----------------------------------------------------
dfa_subset_q2 = dfa_subset_q2 %>%
  mutate(lwages = log(WAGP)) %>% 
  mutate(NATIVITY = factor(NATIVITY)) %>% 
  mutate(COW = factor(COW)) %>% 
  mutate(SEX = factor(SEX)) %>% 
  mutate(ENG = factor(ENG)) %>% 
  mutate(SCHL = factor(SCHL)) %>% 
  mutate(POBP = factor(POBP))

dfa_subset_q2 = dfa_subset_q2 %>% <-
  dfa_subset_q2$POBP = fct_collapse(dfa_subset_q2$POBP, 
                                    "US" = c("001", "002", "004","005", "006", "008",
                                             "009", "010", "011", "012", "013", "015",
                                             "016", "017", "018", "019", "020", "021",
                                             "022", "023", "024", "025", "026", "027",
                                             "028", "029", "030", "031", "032", "033",
                                             "034", "035", "036", "037", "038", "039", 
                                             "040", "041", "042", "044", "045", "046", 
                                             "047", "048", "049", "050", "051", "053", 
                                             "054", "055", "056"))
# dropping NAs, filtering SCHL, POBP --------------------------------
dfa_subset_q2 = dfa_subset_q2 %>%
  drop_na() %>%
  filter(WAGP>0) %>% 
  filter(str_detect(SCHL, 
                    '01|16|19|21|22|23|24')) %>% 
  filter(str_detect(POBP,
                    'US|303|207|212|233|312|247|327|329|217|313')) %>% 
  filter(str_detect(COW,
                    '1|2|3|4|7'))


# renaming the factors ----------------------------------------------------
levels(dfa_subset_q2$POBP) = c("US", "Mexico", "China", "India", "Philippines", "El_Salvador", "Vietnam", "Cuba", "DR", "Korea", "Guatemala")
levels(dfa_subset_q2$NATIVITY) = c("Native", "Foreign")
levels(dfa_subset_q2$SCHL) = c("no_educ", "high_school", "some_college", "bachelors", "masters", "professional", "phd")
levels(dfa_subset_q2$SEX) = c("male", "female")
levels(dfa_subset_q2$ENG) = c("very_well", "well", "not_well", "not_at_all")
levels(dfa_subset_q2$COW) = c("for_profit", "non_profit", "local_gvt", "state_gvt", "self_emp")


# running a regression ----------------------------------------------------
wages.POBP<- lm(lwages ~ POBP + SCHL + ENG + SEX + AGEP + COW, data = dfa_subset_q2)
summary(wages.POBP)


# summary tables ----------------------------------------------------------
stargazer(wages.POBP, type = "text", title = "Table 2: Results", object.names = TRUE, no.space = FALSE, 
          intercept.bottom = FALSE, keep = c("POBPMexico", "POBPChina", "POBPIndia", "POBPPhilippines", 
                                             "POBPEl_Salvador", "POBPVietnam", "POBPCuba", "POBPDR", 
                                             "POBPKorea", "POBPGuatemala"))

# plotting the summary tables ---------------------------------------------



