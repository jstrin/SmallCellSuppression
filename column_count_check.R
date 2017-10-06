
#' Minimum Column Check
#' 
#' min_col_check calculates count of each group within a column to ensure 
#'   minimum size is met; if the minimum size is not met, the value is replaced
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









min_col_check <-  function(df, collapse_var, minCell = 5, ...) {

  require(dplyr)
  require(tidyr)
  require(rlang)
  
    
  cVar <- enquo(collapse_var)
  
  l.gVar <- quos(...)

  
  # Get count of grouping variables
  gVar_counter <- length(l.gVar)
  
  
  
  for(i in l.gVar){
    t.name <- quo_name(i)
    
    if(is.factor(unlist(select(df, UQ(i))))){
      df_n <- df %>%
        mutate(collapse_out = as.character(UQ(i)))
      
    }else{
      df_n <- df %>%
        mutate(collapse_out = UQ(i))
    }
    
   
    
    df_grouped <- df_n %>%
      group_by(UQ(i), collapse_out ) %>%
      summarise(count = n())
    
    
    
    if(min(df_grouped$count)<minCell){
      
      
      
      
      df_prime <- df_grouped %>%
        ungroup() %>%
        mutate(collapse_out = ifelse(count < minCell, "Other", collapse_out  )) %>%
        select(- count )
     
       
      # remove the original collapse variable and replace with the modified collapse values
      
      


      df <- df %>%
        left_join(., df_prime, by = c(t.name)) %>%
        select( - UQ(i)) 
      
      print(names(df))
      
      names(df)[which(names(df) %in% "collapse_out")] <- t.name
    } else{
    df <- df
    }
  }
  
  
 
          df_out <- df
         
      
  
  return(df_out)

}

    

