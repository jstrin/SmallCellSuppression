
#load and prepare test data


library(dplyr)
library(tidyr)
library(rlang)


library(nycflights13)
dfRaw <- flights

dfOutPut <- dfRaw %>%
  group_by(month, carrier) %>%
  summarise(count = n())

set.seed(42)
dfTest <- data.frame(number = rnorm(1000, mean = 100, sd = 10),
                     fact1 = rep(c("blue", "red", "green", "yellow", "orange"), 200)) %>%
  mutate(fact2 = if_else(number < 75, "v.low",
                         if_else(number < 90, "low",
                                 if_else(number < 110, "med",
                                         if_else(number < 115, "high",
                                                 if_else(number < 125, "v.high",
                                                         "ext.high")))))) %>%
  mutate(fact3 = factor(fact2, levels = c("v.low", "low", "med", "high", "v.high", "ext.high")))


#Proof of Concept

replace_basic <- function(n){
  if(is.numeric(n)){
    ifelse(n < 5, NA, n)
  }
}


dfOutPut2 <- dfOutPut %>%
  rowwise() %>%
  mutate(count = replace_basic(count))


###
#  replace basic will work on raw counts, but bigger issue is around grouping variables. 
#   1. if grouping variable is ordinal, we want to collapse into simarly ordered groups (e.g., Agree+Strongly Agree)
#   2. if grouping variable is unordered, 
#       a. we want to collapse smaller groups together -or- 
#       b. collapse groups with similar meaning
#   
#   2.a. is "easiest" to implement 
#
###

source("Small_Cell_Suppression_v0-1.R")

temp <- replace_loop(dfRaw, carrier, minCell = 5, month, year, arr_time)

temp_col <- ColumnCountCheck(dfTest, fact1, minCell = 5, fact2, fact3)




