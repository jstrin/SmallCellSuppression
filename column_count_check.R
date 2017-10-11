
#' column count check
#' 
#' checks the count of each group within a column to ensure minimum size is met
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
# - remove recursion









ColumnCountCheck <-  function(df, collapse_var, minCell = 5, ...) {

  require(dplyr)
  require(tidyr)
  require(rlang)
  
    
  cVar <- enquo(collapse_var)
  
  l.gVar <- quos(...)
  
  # Get count of grouping variables
  gVar_counter <- length(l.gVar)
  
  
  for(i in l.gVar){
    
    df_grouped <- df %>%
      group_by(UQ(i) ) %>%
      summarise(count = n())
    
    if(min(df_grouped$count)<minCell){
      
      if(is.factor(select(df_grouped, UQ(i)))){
        df
      }
      
      
      df_prime <- df_grouped %>%
        ungroup() %>%
        mutate(collapse_out = ifelse(count < minCell, "Other", UQ(i)  )) %>%
        select(- count )
     
      
      
    
      
      
     
      
      
      
      
      
      # remove the original collapse variable and replace with the modified collapse values
      t.name <- quo_name(i)
      


      df <- df %>%
        left_join(., df_prime, by = c(t.name)) %>%
        select( - UQ(i)) 
      
      
      
      names(df)[which(names(df) %in% "collapse_out")] <- t.name
    } else{
    df <- df
    }
  }
  
  
  # find out if there are any groups with fewer than the minimum cell size

  
       
        
        
          df_out <- df
         
      
  
  return(df_out)

}

    

