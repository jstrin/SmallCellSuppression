#'  Ordered Factor replacement
#'
#'  _________ is used when the minimum cell size is not met and the grouping variable is 
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'


replace_ofact <- function(df, ofact_in, minCell = 5, l.ofact = NULL, ... ){
  

  
  ofact <- enquo(ofact_in)
  
  

  
  if(is.null(l.ofact)){
    
    levels.ofact <- df %>%
      select(UQ(ofact)) %>%
      unlist() %>%
      levels()
    
    
    df <- df %>%
      mutate(newfactor = as.character(UQ(ofact)))
    
    }else{
  
  levels.ofact <- l.ofact
  
  df <- df %>% 
    mutate(newfactor = as.character(UQ(ofact)))
  
    }
  
  
  
  n.levels <- (length(levels.ofact))
  
  if( n.levels %% 2 == 0){
    #if even
    
    l.Low <- levels.ofact[1:(n.levels/2)]
    l.High <- levels.ofact[(n.levels/2+1):length(levels.ofact)]
    
    
      dfMid <- NULL
    
  }else{
    #if odd
    
    n.mid <- levels.ofact[ n.levels/2 + 0.5]
    
    
    c.mid <- df %>%
      group_by(newfactor) %>%
      summarise(count = n()) %>%
      filter(newfactor %in% n.mid) %>%
      select(count) %>%
      unlist()
    
    
    
    if(c.mid < minCell & 0 < c.mid){
      
     
      
      new.MidLevel <- paste0(levels.ofact[n.levels/2 - 0.5], ", ", levels.ofact[n.levels/2 + 0.5], ", & ", levels.ofact[n.levels/2 + 1.5])
      
      dfMid <- df %>%
        filter(newfactor %in% levels.ofact[c((n.levels/2 - 0.5), (n.levels/2 + 0.5), (n.levels/2 +1.5) )]) %>%
        mutate(newfactor = new.MidLevel)
      
      c.mid_test <- dfMid %>%
        group_by(newfactor) %>%
        summarise(count = n()) %>%
        select(count) %>%
        unlist()
        
      if(c.mid_test < minCell){
      
        }
      
      l.processed.levels <- c( levels.ofact[n.levels/2 - 0.5], 
                               levels.ofact[n.levels/2 + 0.5],
                               levels.ofact[n.levels/2 + 1.5])
      
    } else{
      l.processed.levels <- c(levels.ofact[n.levels/2 + 0.5])
      
      dfMid <- df %>%
        filter(newfactor %in% n.mid)
    }
    
    l.Low <- levels.ofact[1:(which(levels.ofact %in% l.processed.levels[1])-1)]
    l.High <- levels.ofact[(which(levels.ofact %in% l.processed.levels[length(l.processed.levels)])+1):length(levels.ofact)]
    
   
  }
  
  

  dfHigh <- df %>%
    filter(newfactor %in% l.High)
  
  dfLow <- df %>% 
    filter(newfactor %in% l.Low)
  
return(list(dfMid, dfHigh, dfLow))
  
  }
