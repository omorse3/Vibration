#Package Management and Data Upload----

library(readxl)
library(dplyr)
library(tidyverse)


#Vibration_Soil_Fauna <- read_excel("Vibration_Soil_Fauna.xlsx")
#View(Vibration_Soil_Fauna)
#write as CSV
#write.csv(Vibration_Soil_Fauna, "Vibration_Soil_Fauna.csv")



Vibration_Soil_Fauna_csv <- read_csv("Vibration_Soil_Fauna.csv")

#Remove preliminary sample timepoint
Vibration_Soil_Fauna_csv <- Vibration_Soil_Fauna_csv %>% filter(Sample!="Exp_Start")

#fix NA values (set them to "0")
Vibration_Soil_Fauna_csv[is.na(Vibration_Soil_Fauna_csv)] <- 0

#Making factors
Vibration_Soil_Fauna_csv <- Vibration_Soil_Fauna_csv %>% mutate(Worm_Presence = factor(Worm_Presence, levels = c("No", "Yes")),
                                                                Vibration_Presence = factor(Vibration_Presence, levels = c("No", "Yes")),
                                                                Timepoint = factor(Timepoint, levels = c("1", "2")))


#Remove weird first column
Vibration_Soil_Fauna_csv <- Vibration_Soil_Fauna_csv %>% select(-...1)

Vibration_Soil_Fauna_csv <-as_tibble(Vibration_Soil_Fauna_csv)





#summary calcs; currently not working for some reason----

Vibration_Soil_Fauna_csv <- Vibration_Soil_Fauna_csv %>% 
  mutate(Total_Collembola = sum(Vibration_Soil_Fauna_csv$Sminthuridae:Vibration_Soil_Fauna_csv$Onychiuridae))
         
Vibration_Soil_Fauna_csv$Total_Collembola
