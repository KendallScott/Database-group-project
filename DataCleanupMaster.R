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
library(pROC)
library(caTools)

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

genderLevels = levels(droplevels(as.factor(fatalShootings$gender)))
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
pop_shootings<-shootingsDF
#% by body camera
ggplot(pop_shootings, aes(x=as.factor(race), fill=as.factor(body_camera)))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="stack" , width=0.9) +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_stack(0.9),
            color = "black")+
  xlab('Race') +
  ylab('Body Camera')+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7))+
  scale_fill_viridis(option="viridis", discrete=TRUE, name="Body Camera", alpha=0.75 )+
  theme_classic()
                   
#body camera and threat level
ggplot(pop_shootings, aes(x=as.factor(threat_level), fill=body_camera))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="stack" , width=0.9) +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_stack(0.9), color = "black")+
  xlab('Threat Level') +
  ylab('Body Camera')+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7))+
  scale_fill_viridis(option="viridis", discrete=TRUE, name="Body Camera", alpha=0.75 )+
  theme_classic()
 #overal by year                  
g <- ggplot(pop_shootings, aes(year))
                   
g + geom_bar(aes(fill=year))+
  scale_fill_viridis(option="viridis", discrete=FALSE, name="Body Camera", alpha=0.75 )
ggplot(pop_shootings,aes(x=year))+
  geom_bar(stat='count')+
  ylab('# of Victims')+
  theme_classic()+
  scale_fill_viridis(option="viridis", discrete=FALSE, name="Body Camera", alpha=0.75 )

#comparisons by race for threat level
threat_not_black<-pop_shootings %>%
  filter(race != "Black")%>% 
  group_by(armed,threat_level) %>%
  summarise(cnt=n()) %>%
  mutate(perc=round(cnt/sum(cnt),4))%>%
  arrange(desc(threat_level),desc(perc))
threat<-pop_shootings %>%
  filter(race == "Black")%>% 
  group_by(armed,threat_level) %>%
  summarise(cnt=n()) %>%
  mutate(perc=round(cnt/sum(cnt),4))%>%
  arrange(desc(threat_level),desc(perc))
threat_not_black$group<-"Not Black"
threat$group<-"Black"

threat<-threat%>%filter(threat_level == "attack")
threat_not_black<-threat_not_black%>%filter(threat_level == "attack")
threat<-rbind(threat_not_black, threat)

ggplot(threat, aes(x=reorder(armed,-perc),y=perc, fill=perc))+ geom_bar(aes(fill=perc),show.legend=T,stat="identity")+
  ylab("Threat Level")+
  xlab("Armed Type") +scale_fill_viridis(option="viridis", discrete=FALSE, name="Percent at Attack Threat Level", alpha=0.9 )+ theme_classic()+facet_wrap(~group)+coord_flip()
                   
    gg<- ggplot(threat[c(14:20),],aes(x=reorder(race,-perc),y=perc, fill=perc))+
    geom_bar(aes(fill=perc),show.legend=T,stat="identity")+
    ylab("Threat Level")+
    xlab("Race") +scale_fill_viridis(option="viridis", discrete=FALSE, name="Percent of Attack Threat", alpha=0.9 )+ theme_classic()+facet_wrap(~group)
  gg+coord_flip()
                   
ggplot(pop_shootings, aes(x=as.factor(armed), fill=threat_level))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="stack" , width=0.9) +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ), stat="count", position=position_stack(0.9),
            color = "black")+
  xlab('Armed Type') +
  ylab('Threat Level')+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7))+
  scale_fill_viridis(option="viridis", discrete=TRUE, name="Threat Level", alpha=0.75 )+
  theme_classic()

threat<-pop_shootings %>% 
  group_by(armed,threat_level, race) %>%
  summarise(cnt=n()) %>%
  mutate(perc=round(cnt/sum(cnt),4))%>%
  arrange(desc(threat_level),desc(perc))
                   
 #rf data prep                  
 shooting_rf = data.frame(   
  manner_of_death=as.factor(shootingsDF$manner_of_death),
  armed=as.factor(shootingsDF$armed),
  #signs_of_mental_illness=as.factor(shootingsDF$signs_of_mental_illness),
  threat_level=as.factor(shootingsDF$threat_level),
  flee=as.factor(shootingsDF$flee),
 race=as.factor(shootingsDF$race),
  #year=as.factor(year(shootingsDF$date)), 
  LawEnforcementPerCapita=as.factor(shootingsDF$LawEnforcementPerCapita)
)
                   
#boruta selection, not used in presentation
boruta.train <- Boruta(threat_level~., data = shooting_rf, doTrace = 2)
#plot of feature selection for random forest
plot(boruta.train, xlab = "", xaxt = "n")

lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
           
#all attributes selected were deemed imporant-- was mentioned in presentation, but removed from slides
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
plot(final.boruta)

#test/train/validate for Random Forest
set.seed(1234)
trainIndex = createDataPartition(shootingsDF$threat_level,p=.8,list=F)

training = shooting_rf[trainIndex,]
testing = shooting_rf[-trainIndex,]

model <- randomForest(threat_level ~  ., data = training, importance=TRUE, ntree=50) 
importance=importance(model)
varImpPlot(model)

pred_rf <- predict(model, testing)

#inserting predictions into test
testing$pred<-predict(model, testing)
confusionMatrix(testing$threat_level, testing$pred)

d <- ggplot(testing, aes(threat_level, pred), alpha=0.5)+ ggtitle("Actual vs. Predicted Random Forest")+ theme_minimal() + ylab("Predicted Threat Level")+ xlab("Actual Threat Level")+ geom_point(aes(), alpha = 1/10)+geom_count()
d 
                   
require(pROC)
rf.roc<-roc(train$threat_level,model.rf$votes[,1])
plot(rf.roc)
auc(rf.roc)
                   
#rf minus race column
 shooting_rf = data.frame(   
  manner_of_death=as.factor(shootingsDF$manner_of_death),
  armed=as.factor(shootingsDF$armed),
  threat_level=as.factor(shootingsDF$threat_level),
  flee=as.factor(shootingsDF$flee),
  LawEnforcementPerCapita=as.factor(shootingsDF$LawEnforcementPerCapita)
)
library(caTools)
#test/train/validate for Random Forest
set.seed(1234)
trainIndex = createDataPartition(shootingsDF$threat_level,p=.8,list=F)

training = shooting_rf[trainIndex,]
testing = shooting_rf[-trainIndex,]

model <- randomForest(threat_level ~  ., data = training, importance=TRUE, ntree=50) 
importance=importance(model)
varImpPlot(model)

pred_rf <- predict(model, testing)

#inserting predictions into test
testing$pred<-predict(model, testing)
confusionMatrix(testing$threat_level, testing$pred)

d <- ggplot(testing, aes(threat_level, pred), alpha=0.5)+ ggtitle("Actual vs. Predicted Random Forest")+ theme_minimal() + ylab("Predicted Threat Level")+ xlab("Actual Threat Level")+ geom_point(aes(), alpha = 1/10)+geom_count()
d 

################################################################################
# Varun
################################################################################

#setting up for 5-fold CV
fitControl<-trainControl(method="cv",number=5,classProbs=TRUE, summaryFunction=mnLogLoss)

names(training)

class(training$year)

#fitting an LDA model
lda.fit<-train(threat_level~armed+race+signs_of_mental_illness+flee+body_camera,
               data=training,
               method="lda",
               trControl=fitControl,
               metric="logLoss")

#Metrics for the LDA model
confusionMatrix(predict(lda.fit,testing),testing$threat_level)

#fitting a QDA model
qda.fit<-train(threat_level~armed+race+signs_of_mental_illness+flee+body_camera,
               data=training,
               method="qda",
               trControl=fitControl,
               metric="logLoss")

#Metrics for the QDA model
confusionMatrix(predict(qda.fit,testing),testing$threat_level)

#logLoss for qda and lda
qda.fit$results[2]
lda.fit$results[2]

#ROC curve for QDA and LDA
lda.pred=predict(lda.fit,testing,type="prob")
qda.pred=predict(qda.fit,testing,type="prob")

head(lda.pred)

lda.roc=roc(response=testing$threat_level, predictor=lda.pred$attack, levels=c("attack","other"))
qda.roc=roc(response=testing$threat_level, predictor=qda.pred$attack, levels=c("attack","other"))

plot(lda.roc)
plot(qda.roc,print.thres="best",col="red",add=T,legend=T)
legend("bottomright",
       legend=c("LDA", "QDA"),
       col=c("black", "red"),
       lwd=4, cex =1, xpd = TRUE, horiz = FALSE)

#AUCs
auc(lda.roc)
auc(qda.roc)
################################################################################
# Roslyn
################################################################################

################################################################################
# Tadd
################################################################################








































