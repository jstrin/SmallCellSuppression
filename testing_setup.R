
#load and prepare test data

library(nycflights13)
dfRaw <- flights

dfOutPut <- dfRaw %>%
  group_by(month, carrier) %>%
  summarise(count = n())


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



l.temp <- list( c("n()", "count"),  c("sum(count, na.rm=T)", "sum"))


