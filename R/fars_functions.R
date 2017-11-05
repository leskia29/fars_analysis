#perc_cis function 

library(dplyr)

perc_cis <- function(x,n){
  x <- clean_fars %>%
    select(positive_for_drug == TRUE)
  n <- clean_fars %>% 
    select(!is.na(positive_for_drug))
  percent <-(x/n)*100
  
  
return(percent(as.character(percent)))
}





