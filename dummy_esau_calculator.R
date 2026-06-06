library(dplyr)
library(purrr)

path <- "D:/Nobel/IDLC/Dummy-Project/dummy_data.RDS"

dummy_df <- readRDS(path)

ach1 <- 130
ach2 <- 110
ach3 <- 90


Ach_point_for_tpsp <- function(ach, des) {
  
  if (!des %in% c("SE", "SSE")) {
    return(NA_real_)
  }
  
  if (ach >= ach1) {
    return(80)
    
  } else if (ach >= ach2) {
    return(round((75 + ((ach - 110) * 4.99) / 20), 2))
    
  } else if (ach >= ach3) {
    return(round((70 + ((ach - 90) * 4.99) / 20), 2))
    
  } else {
    return(round((0.01 + (ach * 69.98) / 90), 2))
  }
}

scored_df <- dummy_df %>% 
  mutate(
    disb_score = pmap_dbl(list(ach , des), Ach_point_for_tpsp)
  )


  