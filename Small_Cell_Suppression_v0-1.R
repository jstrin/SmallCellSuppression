#  
#  Small Cell Suppression
#




library(dplyr)
library(tidyr)
library(rlang)
##Sample Data Load




replace_loop <-  function(df, collapse_var, sum_function, minCell = 5, ...) {
  
 
  cVar <- enquo(collapse_var)
  
  l.gVar <- quos(...)

  # Get count of grouping variables, used to trigger events
  
  gVar_counter <- length(l.gVar)
  
  
  
  
  
  print("Number of Grouping Variables: ", gVar_counter)
  
  # quote all variables
  
 
  
  
  
  
  # find out if there are any groups with fewer than the minimum cell size
  
  df_grouped <- df %>%
    filter( !UQ(cVar) %in% "Other") %>%
    group_by(UQ(cVar), !!!(l.gVar)) %>%
    summarise(count = n())
  
  
  
  if(min(df_grouped$count)<minCell){
    
    print(min(df_grouped$count))
    
    
    #create the key variable based on collapsing and grouping variables
    
    
      df <- mutate(df, xy = paste(UQ(cVar), UQS(l.gVar), sep = "_"))
   
    

    
    df_prime <- df %>%
      group_by(xy, UQ(cVar), UQS(l.gVar)) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      mutate(collapse_out = ifelse(count < minCell, "Other", xy)) %>%
      select(xy, collapse_out, UQS(quo))
    
    
    
    l.groupNames <- sapply(l.gVar, quo_name)
    
    
    names(df_prime) <- c("xy", quo_name(cVar), l.groupNames)
    
    
    # remove the original collapse variable and replace with the modified collapse values
    
    df <- df %>%
      select(- UQ(cVar) ) %>%
      left_join(., df_prime, by = c("xy", l.groupNames))
    

    
      }else{
        
        if("Other" %in% unlist(select(df, matches(collapse_var)))){
          
          df_otherCheck <- df %>%
            group_by_(.dots = c(collapse_var, l.grouping_vars)) %>%
            summarise(count = n()) %>%
            ungroup() 
          
          if(min(df_otherCheck$count)<minCell){
            
           #need to identify l.grouping_vars where collapse_var == Other
            
            df_otherCorrection <- df_otherCheck %>%
              filter_(.dots = filter_out_other) %>%
              slice(min(count))
            
          }
          
        }else{
    
          #specify sum_functiona and sum_variable
          
          #not working (lines 174 - 193)
          
          qFunc <- enquo(sum_function)
          qVar <- enquo(sum_var)

                    sumFunc <- if_else(quo_text(qFunc) %in% c("n()"), 
                            qFunc,
                            enquo(sum_function (UQ(qVar))))
 
                    
                    qFunc <- enquo(sum_function)
                    qVar <- enquo(sum_var)
                    
                    sumFunc <- if_else(quo_text(qFunc) %in% c("n()"), 
                                       qFunc,
                                       FALSE)
                                               
                 
                
      
    df_out <- df %>%
      group_by(UQ(cVar), UQ(gVar1), UQ(gVar2), UQ(gVar3), UQ(gVar4) ) %>%
      summarise( outVar = UQ(qFunc(UQ(qVar))))
        
      }
  
      }
  
  return(df_out)
}


temp <- replace_loop(dfRaw, carrier, sum_function = "n()", minCell = 5, month, year, arr_time)



l.temp <- list( c("n()", "count"),  c("sum(count, na.rm=T)", "sum"))
dfTemp <- dfOutPut %>% ungroup() %>% group_by(month) %>% summarise_(.dots = setNames(l.temp[[1]][1], l.temp[[1]][2]))

