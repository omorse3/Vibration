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





#Create two data frames by timepoint
Timepoint_1 <- Vibration_Soil_Fauna_csv %>% filter(Timepoint == "1") %>% select(-Timepoint)

Timepoint_2 <- Vibration_Soil_Fauna_csv %>% filter(Timepoint == "2") %>% select(-Timepoint)






#summary calcs; currently not working for some reason----

NewData <- Timepoint_1 %>% 
  group_by(Worm_Presence, Vibration_Presence) %>%
  summarise(Total_Collembola = sum(Sminthuridae, na.rm = TRUE) +
              sum(Sminthurididae, na.rm = TRUE) +
              sum(Isotomidae, na.rm = TRUE) +
              sum(Entomobryidae, na.rm = TRUE) +
              sum(Onychiuridae, na.rm = TRUE),
            .groups = 'drop')

NewData$Total_Collembola


NewData <- Vibration_Soil_Fauna %>% 
  group_by(Worm_Presence,Vibration_Presence,Timepoint) %>% 
  mutate(Total_Mesostigmata=sum(Meso1:Meso8))
Vibration_Soil_Fauna$Total_Mesostigmata

NewData <- Vibration_Soil_Fauna %>% 
  group_by(Worm_Presence,Vibration_Presence,Timepoint) %>% 
  mutate(Total_Oribatida=sum(Oribatid1:Oribatid9))
Vibration_Soil_Fauna$Total_Oribatida

NewData <- Vibration_Soil_Fauna %>% 
  group_by(Worm_Presence,Vibration_Presence,Timepoint) %>% 
  mutate(Total_Astigmata=sum(Astig1:Astig5))
Vibration_Soil_Fauna$Total_Astigmata

NewData <- Vibration_Soil_Fauna %>% 
  group_by(Worm_Presence,Vibration_Presence,Timepoint) %>% 
  mutate(Total_Prostigmata=sum(Prostig1:Prostig7))
Vibration_Soil_Fauna$Total_Prostigmata

NewData <- Vibration_Soil_Fauna %>% 
  group_by(Worm_Presence,Vibration_Presence,Timepoint) %>% 
  mutate(Total_Other=sum(Enchytraeid:Centipede))
Vibration_Soil_Fauna$Total_Other

#ANOVA All Timepoints----
#Currently timepoints 1 and 2

#Collembola
ANOVA <- aov(Sminthuridae~Worm_Presence+Vibration_Presence*Timepoint, data = Vibration_Soil_Fauna)
summary(ANOVA) #Effect on sminthuridae; Significance in timepoint only

ANOVA <- aov(Sminthurididae~Worm_Presence+Vibration_Presence*Timepoint, data = Vibration_Soil_Fauna)
summary(ANOVA) #Effect on sminthurididae; Significance in timepoint only

ANOVA <- aov(Isotomidae~Worm_Presence+Vibration_Presence+Timepoint, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect on isotomidae

ANOVA <- aov(Entomobryidae~Worm_Presence+Vibration_Presence+Timepoint, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect on entomobryidae

ANOVA <- aov(Onychiuridae~Worm_Presence+Vibration_Presence*Timepoint, data = Vibration_Soil_Fauna)
summary(ANOVA) #Effect on onychiuridae; Significance in timepoint only

#Mesostigmata
ANOVA <- aov(Meso1~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #Significance in worm presence

ANOVA <- aov(Meso2~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Meso3~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Meso4~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Meso4~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Meso5~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Meso6~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Meso7~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Meso8~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect (only found in Exp_Start)

ANOVA <- aov(ImM1~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(ImM2~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

#Oribatida
ANOVA <- aov(Oribatid1~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid2~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid3~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid4~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid5~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid6~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid7~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid8~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid9~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(ImO1~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(ImO2~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

#Astigmata

ANOVA <- aov(Astig1~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Astig2~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Astig3~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Astig4~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #Significant effect in vibration presence

ANOVA <- aov(Astig5~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

#Prostigmata
ANOVA <- aov(Prostig1~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Prostig2~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Prostig3~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Prostig4~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Prostig5~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect; none in timepoint 1

ANOVA <- aov(Prostig6~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect; none in timepoint 1

ANOVA <- aov(Prostig7~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect; none in timepoint 1

#Other
ANOVA <- aov(Enchytraeid~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Worm~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Thrips~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Cricket~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Snail~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Aphid~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Larvae~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Fly~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Millipede~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect

ANOVA <- aov(Ant~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect (only in Exp_Start)

ANOVA <- aov(Beetle~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect (only in Exp_Start)

ANOVA <- aov(Centipede~Worm_Presence+Vibration_Presence, data = Vibration_Soil_Fauna)
summary(ANOVA) #No effect (only in Exp_Start)

#ANOVA Timepoint 1----

#Collembola
ANOVA <- aov(Sminthuridae~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Sminthurididae~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Isotomidae~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Entomobryidae~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Onychiuridae~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

#Mesostigmata
ANOVA <- aov(Meso1~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Meso2~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Meso3~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Meso4~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Meso4~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Meso5~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Meso6~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Meso7~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Meso8~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect (none in timepoint 1)

ANOVA <- aov(ImM1~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(ImM2~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

#Oribatida
ANOVA <- aov(Oribatid1~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid2~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid3~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid4~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid5~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid6~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid7~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect; none in timepoint 1

ANOVA <- aov(Oribatid8~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect, none in timepoint 1

ANOVA <- aov(Oribatid9~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect, none in timepoint 1

ANOVA <- aov(ImO1~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(ImO2~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect; none in timepoint 1

#Astigmata

ANOVA <- aov(Astig1~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Astig2~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Astig3~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Astig4~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Astig5~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

#Prostigmata
ANOVA <- aov(Prostig1~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Prostig2~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Prostig3~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Prostig4~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Prostig5~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect; none in timepoint 1

ANOVA <- aov(Prostig6~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect; none in timepoint 1

ANOVA <- aov(Prostig7~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect; none in timepoint 1

#Other
ANOVA <- aov(Enchytraeid~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Worm~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Thrips~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Cricket~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Snail~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Aphid~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Larvae~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Fly~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Millipede~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Ant~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect, none in timepoint 1

ANOVA <- aov(Beetle~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect, none in timepoint 1

ANOVA <- aov(Centipede~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect, none in timepoint 1

#ANOVA Timepoint 2----

#Collembola
ANOVA <- aov(Sminthuridae~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Sminthurididae~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Isotomidae~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Entomobryidae~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Onychiuridae~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

#Mesostigmata
ANOVA <- aov(Meso1~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #Significant effect in worm presence, Worm_Presence:Vibration_Presence is not significant

ANOVA <- aov(Meso2~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #Significant effect in worm and vibration presence, Worm_Presence:Vibration_Presence is not significant

ANOVA <- aov(Meso3~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Meso4~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Meso4~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Meso5~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Meso6~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect, none in timepoint 2

ANOVA <- aov(Meso7~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect, none in timepoint 2

ANOVA <- aov(Meso8~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect, none in timepoint 2

ANOVA <- aov(ImM1~Worm_Presence*Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #Significant effect in worm presence, Worm_Presence:Vibration_Presence is not significant

ANOVA <- aov(ImM2~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

#Oribatida
ANOVA <- aov(Oribatid1~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid2~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid3~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid4~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid5~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid6~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid7~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid8~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Oribatid9~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(ImO1~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(ImO2~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

#Astigmata

ANOVA <- aov(Astig1~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Astig2~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Astig3~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Astig4~Worm_Presence*Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #Significant effect in worm presence, Worm_Presence:Vibration_Presence is not significant

ANOVA <- aov(Astig5~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

#Prostigmata
ANOVA <- aov(Prostig1~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Prostig2~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Prostig3~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Prostig4~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Prostig5~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Prostig6~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Prostig7~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

#Other
ANOVA <- aov(Enchytraeid~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Worm~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect, none in timepoint 2

ANOVA <- aov(Thrips~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Cricket~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect, none in timepoint 2

ANOVA <- aov(Snail~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect, none in timepoint 2

ANOVA <- aov(Aphid~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Larvae~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Fly~Worm_Presence+Vibration_Presence, data = Timepoint_2)
summary(ANOVA) #No effect

ANOVA <- aov(Millipede~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect

ANOVA <- aov(Ant~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect, only in Exp_Start

ANOVA <- aov(Beetle~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect, only in Exp_Start

ANOVA <- aov(Centipede~Worm_Presence+Vibration_Presence, data = Timepoint_1)
summary(ANOVA) #No effect, only in Exp_Start