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


replace_ofact <- function(df, ofact, minCell = 5, ... ){
  levels.ofact <- df %>%
    select(UQ(ofact))%>%
    unlist()%>%
    levels()
  
  n.levels (length(levels.ofact))
  if( n.levels %% 2 == 0){
    #if even
    
    
  }else{
    #if odd
    
    l.mid <- levels.ofact[ n.levels/2 + 0.5]
    
    
    
    
  }
}
