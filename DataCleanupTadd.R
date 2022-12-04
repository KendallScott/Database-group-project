library(tidyverse)
library(ggplot2)
library(MAP)
library(mapproj)
library(tidyr)
library(dplyr)
library(gridExtra)
library(reshape2)
library(ResourceSelection) 
library(caret)
library(sjPlot)
library(lubridate)


################################################################################
# Fatal Shootings
################################################################################
fatalShootings = read.csv(
  '/Users/taddbackus/School/fall22/appliedStat/Project2/Shootings_dec.csv',
  header=TRUE,
  stringsAsFactors=TRUE)

# Checking duplicates
duplicates = fatalShootings %>%
  group_by(name, city, state) %>%
  mutate(dupe=n()>1) %>%
  arrange(desc(dupe),desc(name),city,state,date)

fatalShootings = subset(fatalShootings, !(id %in% c(8063,6853,8548,7389,7331,8609)))

# Dropping unused columns
fatalShootings = subset(fatalShootings, select=-c(id,longitude,latitude,is_geocoding_exact))

# Re-formatting date
fatalShootings$date = format(as.Date(fatalShootings$date),'%Y-%m-%d')
fatalShootings$date = as.Date(fatalShootings$date)
fatalShootings = fatalShootings %>%
  mutate(year=year(date))
fatalShootings$year = as.integer(fatalShootings$year)

# Re-coding race as full term
fatalShootings = fatalShootings %>%
  mutate(race = case_when(
    race == 'O'~'Other',
    race == 'B'~'Black',
    race == 'W'~'White',
    race == 'H'~'Hispanic',
    race == 'A'~'Asian',
    race == 'N'~'Native American',
    TRUE ~ 'Not Identified'
  ))
fatalShootings$race = as.factor(fatalShootings$race)

# Re-coding armed into more useable levels
fatalShootings$armed <- str_replace_all(fatalShootings$armed, 
                                 c(".*air.*pistol.*"="toy weapon",  
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
fatalShootings[fatalShootings==''] = 'undetermined'
fatalShootings$armed = as.factor(fatalShootings$armed)
levels(droplevels(fatalShootings$armed))
summary(fatalShootings$armed)

# Adding weapon or no weapon
fatalShootings = fatalShootings %>%
  mutate(weaponType = case_when(
    armed == 'gun'~'weapon',
    armed == 'vehicle'~'weapon',
    armed == 'knife/sword'~'weapon',
    armed == 'misc weapon'~'weapon',
    armed == 'household item'~'not weapon',
    armed == 'other'~'not weapon',
    armed == 'toy weapon'~'not weapon',
    armed == 'unarmed'~'not weapon',
    armed == 'undetermined'~'not weapon',
    TRUE ~ 'not weapon'
  ))
fatalShootings$weaponType = as.factor(fatalShootings$weaponType)

# Fixing threat level
fatalShootings$threat_level = as.factor(ifelse(fatalShootings$threat_level=='attack','attack','other'))

# Fixing NAs
sapply(fatalShootings, function(x) sum(is.na(x)))

fatalShootings$flee[is.na(fatalShootings$flee)] = 'Other'

fatalShootings$age[is.na(fatalShootings$age)] = mean(fatalShootings$age,na.rm=TRUE)

genderLevels = levels(droplevels(fatalShootings$gender))
genderLevels[length(genderLevels)+1] = 'Unknown'
fatalShootings$gender = factor(fatalShootings$gender, levels=genderLevels)

fatalShootings$gender[is.na(fatalShootings$gender)] = 'Unknown'

summary(fatalShootings)

################################################################################
# Overall by state
################################################################################
byState = read.csv('/Users/taddbackus/School/fall22/appliedStat/Project2/OverallStateYear.csv',
                   header=TRUE,
                   stringsAsFactors=TRUE)
byState = rename(byState,Native_American_total=Non_Hispanic_total)
byState = rename(byState,Native_American_population=American_Indian_or_Alaskan_Native_population)

byState$Total_Victims = as.numeric(byState$Total_Victims)
byState$Total_Victims_with_Race = as.numeric(byState$Total_Victims_with_Race)
byState$Black_total = as.numeric(byState$Black_total)
byState$White_total = as.numeric(byState$White_total)
byState$Hispanic_total = as.numeric(byState$Hispanic_total)
byState$Asian_total = as.numeric(byState$Asian_total)

byState$totalPC = byState$Total_Victims / byState$Total_Population * 1000000
byState$whitePC = byState$White_total / byState$White_Alone_population * 1000000
byState$blackPC = byState$Black_total / byState$Black_Alone_population * 1000000
byState$asianPC = byState$Asian_total / byState$Asian_Alone_population * 1000000
byState$hispanicPC = byState$Hispanic_total / byState$Hispanic_poulation * 1000000
byState$nativeAmericanPC = byState$Native_American_total / byState$Native_American_population * 1000000

stateDF = byState %>%
  group_by(State) %>%
  summarize(AvgTotalVictimsPerCapita = mean(totalPC),
            AvgWhiteVictimsPerCapita = mean(whitePC),
            AvgBlackVictimsPerCapita = mean(blackPC),
            AvgAsianVictimsPerCapita = mean(asianPC),
            AvgHispanicVictimsPerCapita = mean(hispanicPC),
            AvgNativeAmericanVictimsPerCapita = mean(nativeAmericanPC))

summary(byState)
summary(stateDF)

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
shootingsDF = merge(fatalShootings,stateDF,all.x=TRUE,by='State')
shootingsDF = merge(shootingsDF,police,all.x=TRUE,by='State')

shootingsDF = shootingsDF %>%
  filter(!State=='DC')

summary(shootingsDF)
sapply(shootingsDF, function(x) sum(is.na(x)))

# Adding per race per capita column
shootingsDF$racePerCapita = ifelse(
  shootingsDF$race=='Black',shootingsDF$AvgBlackVictimsPerCapita,
  ifelse(shootingsDF$race=='White',shootingsDF$AvgWhiteVictimsPerCapita,
         ifelse(shootingsDF$race=='Asian',shootingsDF$AvgAsianVictimsPerCapita,
                ifelse(shootingsDF$race=='Hispanic',shootingsDF$AvgHispanicVictimsPerCapita,
                       ifelse(shootingsDF$race=='Native American',shootingsDF$AvgNativeAmericanVictimsPerCapita,
                              ifelse(shootingsDF$race=='Not Identified',shootingsDF$AvgTotalVictimsPerCapita,0))))))
summary(shootingsDF$racePerCapita)


################################################################################
# Test Train Split
################################################################################
set.seed(1234)
trainIndex = createDataPartition(shootingsDF$threat_level,p=.6,list=F)

training = shootingsDF[trainIndex,]
testing = shootingsDF[-trainIndex,]

################################################################################
# Kendall
################################################################################

################################################################################
# Varun
################################################################################

################################################################################
# Roslyn
################################################################################

################################################################################
# Tadd
################################################################################
library(pROC)
library(gridExtra)


yearStats = shootingsDF %>%
  group_by(year,threat_level) %>%
  summarise(cnt=n()) %>%
  mutate(perc=round(cnt/sum(cnt),4)) %>%
  arrange(desc(perc))
yearStats = as.data.frame(yearStats)

yearAttack = yearStats %>%
  filter(threat_level=='attack')

ggplot(yearAttack,aes(x=year,y=perc))+
  geom_line()+
  ylim(0,1)+
  ylab('Percent of victims labeled Attack')

barYear = ggplot(yearAttack,aes(x=year,y=perc,color=year))+
  geom_bar(aes(fill=year),show.legend = FALSE,stat='identity')+
  ylim(0,1)+
  ylab('Percent of victims labeled Attack')+
  xlab('Year')+
  ggtitle('Victims labeled attack')

raceYear = shootingsDF %>%
  group_by(year,threat_level,race) %>%
  summarize(cnt=n()) %>%
  mutate(perc=round(cnt/sum(cnt),4)) %>%
  arrange(desc(perc))
raceYear = as.data.frame(raceYear)

raceAttack = raceYear %>%
  filter(threat_level=='attack')

lineRace = ggplot(raceAttack,aes(x=year,y=perc,color=race))+
  geom_line()+
  ylim(0,1)+
  ylab('Percent of victims labeled Attack')+
  xlab('')+
  ggtitle('Victims labeled attack by race')

ggplot(raceAttack,aes(x=year,y=perc,color=year))+
  geom_bar(aes(fill=year),show.legend = TRUE,stat='identity')+
  facet_wrap(~race)+
  ylim(0,0.5)

grid.arrange(lineRace,barYear,nrow=2)

ggplot(shootingsDF,aes(x=year,fill=threat_level))+
  geom_bar(stat='count',position='dodge')
################################################################################
# Simple Logistic Model
tlSimple = glm(threat_level~AvgTotalVictimsPerCapita+
                 weaponType+
                 race+
                 age+
                 signs_of_mental_illness+
                 flee+
                 year+
                 LawEnforcementPerCapita+
                 body_camera,
               data=training,
               family='binomial')
summary(tlSimple)
1/exp(confint(tlSimple))
simplePredict = predict(tlSimple,testing,type='response')
simpleROC = roc(response=testing$threat_level,predictor=simplePredict,levels=c('attack','other'))
plot(simpleROC,print.thres='best')

simpleClass = ifelse(simplePredict>0.35,'attack','other')
testing$predThreat = simpleClass
testing$predThreat = as.factor(testing$predThreat)
confusionMatrix(testing$threat_level,testing$predThreat)
simpleROC$auc

################################################################################
# Complex Logistic Model
fitControl = trainControl(method='repeatedcv',number=10,repeats=1,classProbs=TRUE)
tlLogModel = train(threat_level~armed+
                     age+
                     gender+
                     race+
                     State+
                     signs_of_mental_illness+
                     flee+
                     body_camera+
                     year+
                     racePerCapita+
                     LawEnforcementPerCapita+
                     race:year+
                     race:racePerCapita+
                     race:flee+
                     race:armed+
                     race:body_camera,
                   data=training,
                   method='glmnet',
                   trControl=fitControl,
                   metric='AUROC')
summary(tlLogModel$finalModel)
coef(tlLogModel$finalModel,tlLogModel$finalModel$lambdaOpt)

testPredict = predict(tlLogModel,testing,type='prob')[,'attack']
testROC = roc(response=testing$threat_level,predictor=testPredict,levels=c('attack','other'))
plot(testROC,print.thres='best')

testClass = ifelse(testPredict>.66,'attack','other')
testing$testThreat = testClass
testing$testThreat = as.factor(testing$testThreat)
confusionMatrix(testing$threat_level,testing$testThreat)
testROC$auc



plot(simpleROC,print.thres='best')
plot(testROC,print.thres='best',col='red',add=TRUE)
legend('bottomright',
       legend=c('Simple Model','Complex Model'),
       col=c('black','red'),
       lwd=4,cex=1,xpd=TRUE,horiz=FALSE)














################################################################################
# Not Used
tlComplex = glm(threat_level~armed+
                  age+
                  gender+
                  race+
                  State+
                  signs_of_mental_illness+
                  flee+
                  body_camera+
                  year+
                  racePerCapita+
                  LawEnforcementPerCapita+
                  race:year+
                  race:racePerCapita+
                  race:flee+
                  race:armed+
                  race:body_camera,
                data=training,
                family='binomial')
summary(tlComplex)
complexPredict = predict(tlComplex,testing,type='response')
complexROC = roc(response=testing$threat_level,predictor=complexPredict,levels=c('attack','other'))
plot(complexROC,print.thres='best')

complexClass = ifelse(complexPredict>0.337,'attack','other')
testing$complexThreat = complexClass
testing$complexThreat = as.factor(testing$complexThreat)
confusionMatrix(testing$threat_level,testing$complexThreat)































