#Package Management and Data Upload----
#install.packages("lme4")

library(readxl)
library(dplyr)
library(tidyverse)
library(lme4)
library(agricolae)

#Vibration_Soil_Fauna <- read_excel("Vibration_Soil_Fauna.xlsx")
#View(Vibration_Soil_Fauna)
#write as CSV
#write.csv(Vibration_Soil_Fauna, "Vibration_Soil_Fauna.csv")



Vibration_Soil_Fauna_csv <- read_csv("Vibration_Soil_Fauna.csv")

#Remove preliminary sample timepoint
Vibration_Soil_Fauna_csv <- Vibration_Soil_Fauna_csv %>% filter(Sample!="Exp_Start")

#fix NA values (set them to "0")
Vibration_Soil_Fauna_csv[is.na(Vibration_Soil_Fauna_csv)] <- 0


#make a combined treatment column for later: 
Vibration_Soil_Fauna_csv <- Vibration_Soil_Fauna_csv %>%
  mutate(Combined_Treatment = case_when(
    Worm_Presence == "Yes" & Vibration_Presence == "Yes" ~ "Worms present, Vibration present",
    Worm_Presence == "Yes" & Vibration_Presence == "No" ~ "Worms present, Vibration absent",
    Worm_Presence == "No" & Vibration_Presence == "Yes" ~ "Worms absent, Vibration present",
    Worm_Presence == "No" & Vibration_Presence == "No" ~ "Worms absent, Vibration absent",
    TRUE ~ "Treatment unknown" # Fallback in case of any NA or unexpected values
  ))


#make our new combined treatment a factor
Vibration_Soil_Fauna_csv$Combined_Treatment <- as.factor(Vibration_Soil_Fauna_csv$Combined_Treatment)


#Remove weird first column
Vibration_Soil_Fauna_csv <- Vibration_Soil_Fauna_csv %>% select(-...1)




#Calculations and selecting data  ----

Summary_Data <- Vibration_Soil_Fauna_csv %>% 
  mutate(
    Total_Collembola = rowSums(across(Sminthuridae:Onychiuridae)),
         Total_Oribatida = rowSums(across(Oribatid1:Oribatid9)),
         Total_Mesostigmata = rowSums(across(Meso1:Meso8)),
         Total_Astigmata = rowSums(across(Astig1:Astig5)),
         Total_Prostigmata = rowSums(across(Prostig1:Prostig7)),
         Other_Fauna = rowSums(across(ImM1:Fly)))

# select the columns we want

Data <- Summary_Data %>% select(Sample:Timepoint, 
                        Combined_Treatment, #dont forget to select your treatments!
                        Total_Collembola, 
                        Total_Oribatida, 
                        Total_Mesostigmata, 
                        Total_Astigmata, 
                        Total_Prostigmata, 
                        Other_Fauna)
  

#filter timepoint for your final data
Timepoint_1_Data <- Data %>% filter(Timepoint == "1")
Timepoint_2_Data <- Data %>% filter(Timepoint == "2")




#Collembola ANOVA
  #Timepoint 1
  Collembola_AOV_T1 <- aov(Total_Collembola~Combined_Treatment, data = Timepoint_1_Data)
    summary(Collembola_AOV_T1) #view ANOVA results
    
    Collembola_Pairwise_T1 <- HSD.test(Collembola_AOV_T1, trt = "Combined_Treatment") #make pairwise comparison to denote significant differences
    Collembola_Pairwise_T1 #view pairwise comparison
    
  #Timepoint 2
  Collembola_AOV_T2 <- aov(Total_Collembola~Combined_Treatment, data = Timepoint_2_Data)
    summary(Collembola_AOV_T2) #view ANOVA results    
    
    Collembola_Pairwise_T2 <- HSD.test(Collembola_AOV_T2, trt = "Combined_Treatment") #make pairwise comparison to denote significant differences
    Collembola_Pairwise_T2 #view pairwise comparison
      

