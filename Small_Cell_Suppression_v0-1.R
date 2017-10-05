
#' Small Cell Suppression
#' 
#' Iteratively replaces values of variables until minimum cell size of reporting
#' standards are met
#' 
#' @param df data frame
#' @param collapse_var the first variable of interest. May be numeric, ordinal,
#' un-ordered, if character, treated as un orderd factor
#' @param minCell minimum desired cell size, default = 5
#' @param ... used to pass additional grouping variables, typically ordinal or
#' un-ordered factors

#  
#  Small Cell Suppression
#






# Current to dos:
# 
# - adjust for ordered grouping variables
# - check that each grouping variable on it's own meets minimum cell criteria



library(dplyr)
library(tidyr)
library(rlang)
##Sample Data Load







replace_loop <-  function(df, collapse_var, minCell = 5, ...) {
  
  cVar <- enquo(collapse_var)
  
  l.gVar <- quos(...)
  
  # Get count of grouping variables
  gVar_counter <- length(l.gVar)
  
  
  # find out if there are any groups with fewer than the minimum cell size

  
  df_grouped <- df %>%
    filter( !UQ(cVar) %in% c("Other")) %>%
    group_by(UQ(cVar), !!!(l.gVar)) %>%
    summarise(count = n())
  
 
  if(min(df_grouped$count)<minCell){
    
  
    #create the key variable based on collapsing and grouping variables
    
    
    df <- mutate(df, xy = paste(UQ(cVar), UQS(l.gVar), sep = "_"))
   
    df_prime <- df %>%
      group_by(xy, UQ(cVar), UQS(l.gVar)) %>%
      summarise(Count = n()) %>%
      ungroup() %>%
      mutate(collapse_out = ifelse(Count < minCell, "Other", UQ(cVar))) %>%
      select(- Count, - UQ(cVar))
    

    l.groupNames <- sapply(l.gVar, quo_name)
    
   
    names(df_prime) <- c("xy", l.groupNames, quo_name(cVar))


    # remove the original collapse variable and replace with the modified collapse values
    
    df <- df %>%
      select(- UQ(cVar) ) %>%
      left_join(., df_prime, by = c("xy", l.groupNames))


    
    replace_loop(df, UQ(cVar), minCell, !!!l.gVar)
    
      }else{
       
        
        
          df_out <- df
         
      
  
  return(df_out)
}
}

    

