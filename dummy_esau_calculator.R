library(dplyr)
library(purrr)

path <- "D:/Nobel/IDLC/Dummy-Project/dummy_data.RDS"

dummy_df <- readRDS(path)

ach1 <- 130
ach2 <- 110
ach3 <- 90

Ach_point_for_tpsp <- function(ach, des)
  {
  if (des %in% c("SE" , "SSE"))
  {
    if (ach >= ach1)
    {
      return(80)
    }
    else if (ach>=ach2 & ach<ach1)
    {
      return(75+((ach-110)*4.99)/20)
    }
    else if (ach>=ach3 & ach<ach2)
    {
      return(70+((ach-90)*4.99)/20)
    }
    else 
    {
      return(0.01+(ach*69.98)/90)
    }
  }
  else 
  {
    print("The RM is not a tpsp")
  }
}

scored_df <- dummy_df %>% 
  mutate(
    disb_score = map_dbl(ach , Ach_point_for_tpsp)
  )


  