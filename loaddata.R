# load the data + packages ------------------------------------------------
##link to the google drive with the data file
## https://drive.google.com/file/d/1Vinq4MGqe5csCYqwjQcmXWrUZ8gAkN4j/view?usp=sharing
setwd("set_your_own_directory_here")
library(tidyverse)
dfa <- read_csv("psam_pusa.csv") #load the data
dim(dfa)


# navigate the data -------------------------------------------------------
names_dfa = names(dfa)
View(names(dfa))
##or
# create a data frame of NAMES of PUMS databases
df_names = cbind(names_dfa, names_dfb)
View(df_names)
# combine all the names, see the vectors and then pick what variables you like





