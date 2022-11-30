library(tidyverse)
library(ggplot2)
library(MAP)
library(mapproj)
library(tidyr)
library(dplyr)
library(gridExtra)
library(reshape2)

################################################################################
# Fatal Shootings
################################################################################
fatalShootings = read.csv(
  '/Users/taddbackus/School/fall22/appliedStat/Project2/fatal-police-shootings-data.csv',
  header=TRUE)

fatalShootings$manner_of_death = as.factor(fatalShootings$manner_of_death)
fatalShootings$armed = as.factor(fatalShootings$armed)
fatalShootings$gender = as.factor(fatalShootings$gender)
fatalShootings$race = as.factor(fatalShootings$race)
fatalShootings$signs_of_mental_illness = as.factor(fatalShootings$signs_of_mental_illness)
fatalShootings$threat_level = as.factor(fatalShootings$threat_level)
fatalShootings$flee = as.factor(fatalShootings$flee)
fatalShootings$body_camera = as.factor(fatalShootings$body_camera)
fatalShootings$date = as.Date(fatalShootings$date)

summary(fatalShootings)

################################################################################
# Overall Population by state
################################################################################
byState = read.csv(
  '/Users/taddbackus/School/fall22/appliedStat/Project2/overallByState.csv',
  header=TRUE
)

byState$Total_Victims = as.numeric(byState$Total_Victims)
byState$Total_Victims_with_Race = as.numeric(byState$Total_Victims_with_Race)
byState$Black_total = as.numeric(byState$Black_total)
byState$White_total = as.numeric(byState$White_total)
byState$Hispanic_total = as.numeric(byState$Hispanic_total)
byState$Asian_total = as.numeric(byState$Asian_total)
byState$Non_Hispanic_total = as.numeric(byState$Non_Hispanic_total)

byState[is.na(byState)] = 0

summary(byState)

byState$totalPC = byState$Total_Victims / byState$Total_Population * 1000000
byState$whitePC = byState$White_total / byState$White_Alone_population * 1000000
byState$blackPC = byState$Black_total / byState$Black_Alone_population * 1000000
byState$asianPC = byState$Asian_total / byState$Asian_Alone_population * 1000000
byState$hispanicPC = byState$Hispanic_total / byState$Hispanic_poulation * 1000000
byState$nonHispanicPC = byState$Non_Hispanic_total / byState$Not_Hispanic_population * 1000000

stateDF = byState %>%
  group_by(State) %>%
  summarize(AvgTotalVictimsPerCapita = mean(totalPC),
            AvgWhiteVictimsPerCapita = mean(whitePC),
            AvgBlackVictimsPerCapita = mean(blackPC),
            AvgAsianVictimsPerCapita = mean(asianPC),
            AvgHispanicVictimsPerCapita = mean(hispanicPC),
            AvgNonHispanicVictimsPerCapita = mean(nonHispanicPC))


################################################################################
# Police
################################################################################
police = read.csv('/Users/taddbackus/School/fall22/appliedStat/Project2/copState.csv')

lookup = data.frame(abb=state.abb,state=state.name)
lookup = rename(lookup,State=state)

police = merge(police,lookup,by='State')
police = subset(police, select=-c(State))
police = rename(police, State=abb)

################################################################################
# Merging
################################################################################
fatalShootings = rename(fatalShootings,State=state)
testDF = merge(fatalShootings,stateDF,by='State')
finalDF = merge(testDF,police,by='State')

#testDF = finalDF
#summary(testDF$armed)

finalDF$armed <- str_replace_all(finalDF$armed, 
                                       c(".*air.*pistol.*"=" toy weapon",  
                                         ".*chain.*saw.*"= "chain saw", 
                                         ".*nail gun*"="household item",  
                                         ".*walking stick.*"="household item",  
                                         ".*wasp spray*"= "household item", 
                                         ".*vehicle and machete.*"= "knife/sword", 
                                         ".*knife.*"= "knife/sword", 
                                         ".*pipe.*"= "household item", 
                                         ".*pole.*"= "household item", 
                                         ".*piece of wood.*"= "household item", 
                                         ".*screwdriver.*"= "household item",  
                                         ".*sword.*"= "knife/sword",  
                                         ".*pair of scissors.*"= "household item", 
                                         ".*pick axe.*"= "misc weapon",  
                                         ".*air conditioner.*"= "household item", 
                                         ".*gun.*"= "gun", 
                                         "wasp spray"="household item", 
                                         "Airsoft pistol"="toy weapon", 
                                         ".*hammer.*"="household item", 
                                         ".*hand torch.*"="household item", 
                                         "box cutter"="knife/sword", 
                                         "chair"="household item", 
                                         "crowbar"="household item", 
                                         "flashlight"="household item", 
                                         "cordless drill"="household item", 
                                         "contractor's level"="household item", 
                                         "baton"="misc weapon", 
                                         "glass shard"="other", 
                                         "pitchfork"="misc weapon", 
                                         "pick-axe"="misc weapon", 
                                         "railroad spikes"="other", 
                                         "rock"="other", 
                                         "wrench"="household item", 
                                         "brick"="other", 
                                         "shovel"="household item", 
                                         "sharp object"="other", 
                                         "meat cleaver"="knife/sword", 
                                         "machete"="knife/sword", 
                                         "wrench"="household item", 
                                         "metal hand tool"="household item", 
                                         "carjack"="household item", 
                                         ".*ax.*"="misc weapon", 
                                         ".*bottle.*"="other", 
                                         "barstool"="household item", 
                                         "microphone"="other", 
                                         "binauculars"="other", 
                                         "brick"="other", 
                                         "stapler"="household item", 
                                         "meat cleaver"="knife/sword", 
                                         "oar"="household item", 
                                         "ice pick"="other", 
                                         "straight edge razor"="knife/sword", 
                                         "lawn mower blade"="knife/sword", 
                                         "hatchet"="misc weapon", 
                                         "garden tool"="household item", 
                                         "fireworks"="other", 
                                         "pen"="household item", 
                                         "baseball bat"="misc weapon", 
                                         "blunt object"="misc weapon", 
                                         "bow and arrow"="misc weapon", 
                                         "chain"="household item", 
                                         "chain saw"="misc weapon", 
                                         "claimed to be armed"="other", 
                                         "incendiary.*"="other", 
                                         "tire iron"="household item", 
                                         "houehold item"="household item", 
                                         "metal rake"="household item", 
                                         "metal stick"="other", 
                                         "motorcycle"="vehicle", 
                                         "Taser"="misc weapon",
                                         "unknown weapon"="misc weapon", 
                                         "spear"="misc weapon",  
                                         "pepper spray"="misc weapon", 
                                         "binoculars"="household item", 
                                         "sharp object"="other",  
                                         "stake"="other",  
                                         "NULL"="undetermined", 
                                         "crossbow"="misc weapon", 
                                         "household item saw"="other", 
                                         "metal object"="other", 
                                         "Gun"="gun", 
                                         "other and fireplace poker"="other", 
                                         "stone"="other", 
                                         "ax"="misc weapon",
                                         "misc weapon and fireplace poker"="misc weapon"
                                       ))
finalDF[finalDF==''] = 'undetermined'
finalDF$armed = as.factor(finalDF$armed)
levels(droplevels(finalDF$armed))
summary(finalDF$armed)
                                                              

################################################################################
# EDA - continuous
################################################################################
tlDF = finalDF
tlDF$threat_level = ifelse(tlDF$threat_level=='attack','attack','other')
tlDF$threat_level = as.factor(tlDF$threat_level)
summary(tlDF$threat_level)

tlDF$threatNum = ifelse(tlDF$threat_level=='attack',1,0)
summary(tlDF)
ggplot(tlDF,aes(x=age,y=threatNum))+
  geom_point()+
  geom_smooth(method='loess',span=1)

ggplot(tlDF,aes(x=AvgTotalVictimsPerCapita,y=threatNum))+
  geom_point()+
  geom_smooth(method='loess',span=1)

ggplot(tlDF,aes(x=AvgWhiteVictimsPerCapita,y=threatNum))+
  geom_point()+
  geom_smooth(method='loess',span=1)

ggplot(tlDF,aes(x=AvgBlackVictimsPerCapita,y=threatNum))+
  geom_point()+
  geom_smooth(method='loess',span=1)

ggplot(tlDF,aes(x=AvgAsianVictimsPerCapita,y=threatNum))+
  geom_point()+
  geom_smooth(method='loess',span=1)

ggplot(tlDF,aes(x=AvgHispanicVictimsPerCapita,y=threatNum))+
  geom_point()+
  geom_smooth(method='loess',span=1)

ggplot(tlDF,aes(x=AvgNonHispanicVictimsPerCapita,y=threatNum))+
  geom_point()+
  geom_smooth(method='loess',span=1)

ggplot(tlDF,aes(x=LawEnforcementPerCapita,y=threatNum))+
  geom_point()+
  geom_smooth(method='loess',span=1)

################################################################################
# EDA - categorical
################################################################################
summary(tlDF)
ggplot(tlDF,aes(x=manner_of_death,fill=threat_level))+
  geom_bar(position='dodge',stat='count')
ggplot(tlDF,aes(x=gender,fill=threat_level))+
  geom_bar(position='dodge',stat='count')
ggplot(tlDF,aes(x=race,fill=threat_level))+
  geom_bar(position='dodge',stat='count')
ggplot(tlDF,aes(x=signs_of_mental_illness,fill=threat_level))+
  geom_bar(position='dodge',stat='count')
ggplot(tlDF,aes(x=flee,fill=threat_level))+
  geom_bar(position='dodge',stat='count')
ggplot(tlDF,aes(x=body_camera,fill=threat_level))+
  geom_bar(position='dodge',stat='count')
ggplot(tlDF,aes(x=armed,fill=threat_level))+
  geom_bar(position='dodge',stat='count')+
  facet_wrap(~race)

summary(tlDF$armed)

tlDF$armed = ifelse(tlDF$armed=='knife','blade',tlDF$armed)

################################################################################
# Part 1 Model
################################################################################
tlModel = glm(threat_level~age+
                gender+
                race+
                State+
                flee+
                body_camera+
                AvgTotalVictimsPerCapita+
                LawEnforcementPerCapita,
              data=tlDF,
              family='binomial')
summary(tlModel)

################################################################################
# Part 2 Model
################################################################################






