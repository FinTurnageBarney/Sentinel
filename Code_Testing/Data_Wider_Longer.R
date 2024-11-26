library(dplyr)
library(tidyr)



Data <- read.csv("Result_Excel/2020_2021_all_scenes/2020_21_all_scenes.csv")

Data = Data[,-1]



Data_longer <- Data %>% 
  pivot_longer(cols=c("BL", "DY" , "FE" , "KE", "LL", "MA", "MO", "OH"), names_to='Site', values_to='Area')

Data_Wider <- Data_longer %>% 
  pivot_wider(names_from = "threshold" , values_from = "Area" )



write.csv(Data_Wider, file = "2020_21_all_scenes_better_format.csv")
