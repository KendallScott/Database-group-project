fatalShootings = read.csv('/Users/taddbackus/School/fall22/databases/Project/fatal-police-shootings-data.csv',header=TRUE)

fatalShootings

summary(fatalShootings)

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

sapply(fatalShootings, function(x) sum(is.na(x)))


library(ggplot2)
library(maps)
library(mapproj)
library(dplyr)
library(gridExtra)


ggplot(fatalShootings, aes(x=manner_of_death))+
  geom_bar(stat='count')
ggplot(fatalShootings, aes(y=armed))+
  geom_bar(stat='count')
ggplot(fatalShootings, aes(x=age))+
  geom_bar(stat='count')
ggplot(fatalShootings, aes(y=date))+
  geom_bar(stat='count')
ggplot(fatalShootings, aes(x=race,fill=gender))+
  geom_bar(stat='count')
ggplot(fatalShootings, aes(y=state))+
  geom_bar(stat='count')
ggplot(fatalShootings, aes(x=signs_of_mental_illness))+
  geom_bar(stat='count')
ggplot(fatalShootings, aes(x=threat_level))+
  geom_bar(stat='count')
ggplot(fatalShootings, aes(x=flee))+
  geom_bar(stat='count')
ggplot(fatalShootings, aes(x=body_camera))+
  geom_bar(stat='count')

################################################################################
# Mapping shootings per state
################################################################################
fsMapData = fatalShootings
fsMapData = fsMapData %>%
  group_by(state) %>%
  summarize(count=n())

fsMapData$state = factor(fsMapData$state)
lookup = data.frame(abb=state.abb,state=state.name)
fsMapData = rename(fsMapData,abb=state)
fsMapData$abb = trimws(fsMapData$abb)
fsMap = merge(fsMapData,lookup,by='abb',all=TRUE)
colnames(fsMap)[2] = 'Shootings'

fsMap$region = tolower(fsMap$state)
fsMap = fsMap[-1]

states = map_data('state')
map.df = merge(states,fsMap,by='region',all.x=TRUE)
map.df = map.df[order(map.df$order),]

ggplot(map.df,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Shootings))+
  geom_path()+
  scale_fill_gradient2(low='blue',mid='green',high='red',midpoint=500,na.value='grey90')+
  ggtitle('Number of shootings in each state')
  theme_void()
  
stateCount = fatalShootings %>%
  group_by(state) %>%
  summarize(count=n())

ggplot()+
  geom_polygon(data=states,aes(x=long,y=lat,group=group),fill='grey')+
  geom_point(data=fatalShootings,aes(x=longitude,y=latitude))+
  theme_void()

################################################################################
# OverTime
################################################################################
fsDF = fatalShootings

fsDF$dateMonth = format(as.Date(fsDF$date),'%Y-%m-01')
fsDF$dateMonth = as.Date(fsDF$dateMonth)
fsDF$dateYear = format(as.Date(fsDF$date),'%Y-01-01')
fsDF$dateYear = as.Date(fsDF$dateYear)

dateDF = fsDF %>%
  group_by(date) %>%
  summarize(count=n())
ggplot(dateDF,aes(x=date,y=count))+
  geom_line()

monthDF = fsDF %>%
  group_by(dateMonth) %>%
  summarize(count=n())
ggplot(monthDF,aes(x=dateMonth,y=count))+
  geom_line()

yearDF = fsDF %>%
  group_by(dateYear) %>%
  summarize(count=n())
ggplot(yearDF,aes(x=dateYear,y=count))+
  geom_line()

################################################################################
# DFW
################################################################################

dfwDF = fatalShootings %>%
  filter(city=='Dallas' | city=='Fort Worth')

modP = ggplot(dfwDF,aes(x=manner_of_death))+
  geom_bar(stat='count',fill='red')+
  ggtitle('Manner of Death')+
  xlab('')
armP = ggplot(dfwDF,aes(y=armed))+
  geom_bar(stat='count')
mentP = ggplot(dfwDF,aes(x=signs_of_mental_illness))+
  geom_bar(stat='count',fill='green')+
  ggtitle('Signs of Mental Illness')+
  xlab('')
threatP = ggplot(dfwDF,aes(x=threat_level))+
  geom_bar(stat='count',fill='orange')+
  ggtitle('Threat Level')+
  xlab('')
grid.arrange(modP,mentP,threatP,nrow=1)

ageP = ggplot(dfwDF,aes(x=age))+
  geom_histogram(fill='blue')+
  ggtitle('Age Distribution')+
  xlab('')
genP = ggplot(dfwDF,aes(x=gender))+
  geom_bar(stat='count',fill='forestgreen')+
  ggtitle('Gender')+
  xlab('')
raceP = ggplot(dfwDF,aes(x=race))+
  geom_bar(stat='count',fill='darkred')+
  ggtitle('Race')+
  xlab('')
grid.arrange(ageP,genP,raceP,nrow=1)

fleeP = ggplot(dfwDF,aes(x=flee))+
  geom_bar(stat='count',fill='purple')+
  ggtitle('Was the person fleeing?')+
  xlab('')
camP = ggplot(dfwDF,aes(x=body_camera))+
  geom_bar(stat='count',fill='steelblue')+
  xlab('')
grid.arrange(fleeP,camP,nrow=1)


texas = map_data('state') %>% 
  filter(region=='texas')

tDF = fatalShootings %>%
  filter(state=='TX')

ggplot()+
  geom_polygon(data=texas,aes(x=long,y=lat,group=group),fill='grey')+
  geom_point(data=tDF,aes(x=longitude,y=latitude))+
  theme_void()

################################################################################
# all
################################################################################


modP = ggplot(fatalShootings,aes(x=manner_of_death))+
  geom_bar(stat='count',fill='red')+
  ggtitle('Manner of Death')+
  xlab('')
armP = ggplot(fatalShootings,aes(y=armed))+
  geom_bar(stat='count',fill='blue')
mentP = ggplot(fatalShootings,aes(x=signs_of_mental_illness))+
  geom_bar(stat='count',fill='green')+
  ggtitle('Signs of Mental Illness')+
  xlab('')
threatP = ggplot(fatalShootings,aes(x=threat_level))+
  geom_bar(stat='count',fill='orange')+
  ggtitle('Threat Level')+
  xlab('')
grid.arrange(modP,mentP,threatP,nrow=1)

ageP = ggplot(fatalShootings,aes(x=age))+
  geom_histogram(fill='blue')+
  ggtitle('Age Distribution')+
  xlab('')
genP = ggplot(fatalShootings,aes(x=gender))+
  geom_bar(stat='count',fill='forestgreen')+
  ggtitle('Gender')+
  xlab('')
raceP = ggplot(fatalShootings,aes(x=race))+
  geom_bar(stat='count',fill='darkred')+
  ggtitle('Race')+
  xlab('')
grid.arrange(ageP,genP,raceP,nrow=1)
summary(fatalShootings$age)

fleeP = ggplot(fatalShootings,aes(x=flee))+
  geom_bar(stat='count',fill='purple')+
  ggtitle('Was the person fleeing?')+
  xlab('')
camP = ggplot(fatalShootings,aes(x=body_camera))+
  geom_bar(stat='count',fill='steelblue')+
  ggtitle('Was the body camera on?')+
  xlab('')
grid.arrange(fleeP,camP,nrow=1)