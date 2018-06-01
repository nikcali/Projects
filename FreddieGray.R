### Data Exploration for crime before and after Freddie Gray

# set WD
setwd("~/Documents/2018/Graduate School/Spring 2018/DATA_602/Group_Project_2018")
library(dplyr)
library(arules)

Victim <- read_excel("Victim.xlsx")
colnames(Victim)[19] <- "District_Name"


#Compare and contrast crimes before and after Freddie Grey

#Exploratoin of pre Freddie Grey
Victim2015 <- Victim[Victim$ImportantDate <= "2015-04-19", ] 
sort(table(Victim2015$IncidentOffenseCOMBINE), decreasing = TRUE)

#From this Victim sheet, there are 1232 days before Freddie Grey

# aggregate the counts of incidents
VictimSums_2015 <-table(Victim2015$IncidentOffenseCOMBINE)  

# convert the table to a data table
V2015df <-as.data.frame(VictimSums_2015)
View(V2015df)

# rename both columns of data
colnames(V2015df)<-c("Incident", "count") 
summary(V2015df)

# Then we transform our dataframe with this new order
V2015df <-transform(V2015df, Incident=reorder(Incident, count))
#compare with previous results
summary(V2015df)

# Plot them in a bar chart
ggplot(V2015df)+ geom_bar(aes(x=Incident,y=CrimePerDay),stat="identity", 
  color='purple', fill="gray") +coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8))) +
  ggtitle("Frequency of Incidents Pre Freddie Grey")+theme(plot.title=element_text(face="bold.italic", 
  color="black", lineheight=1.5, hjust=0.5), axis.text.y=element_text(size=rel(0.8)))

# same thing for after freddie grey (2015-04-19)
Victim2018 <- Victim[Victim$ImportantDate > "2015-04-19", ] 
sort(table(Victim2018$IncidentOffenseCOMBINE), decreasing = TRUE)


# aggregate the counts of incidents
VictimSums_2018 <-table(Victim2018$IncidentOffenseCOMBINE)  

# convert the table to a data table
V2018df <-as.data.frame(VictimSums_2018)
View(V2018df)

# rename both columns of data
colnames(V2018df)<-c("Incident", "count") 
summary(V2018df)

# Then we transform our dataframe with this new order
V2018df <-transform(V2018df, Incident=reorder(Incident, count))
#compare with previous results
summary(V2018df)

ggplot(V2018df)+ geom_bar(aes(x=Incident,y=CrimePerDay),
  stat="identity", color='purple', fill="gray") +coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8))) +ggtitle("Frequency of Incidents After Freddie Grey")+
  theme(plot.title=element_text(face="bold.italic", color="black",
  lineheight=1.5, hjust=0.5), axis.text.y=element_text(size=rel(0.8)))



Incident_Merge <- merge(V2015df, V2018df, by="Incident", all = FALSE)

# Better colnames
colnames(Incident_Merge)<-c("Incident", "Before", "After") 

library(reshape2)
Incident_Melt <-melt(Incident_Merge, id = "Incident")


library(scales)

ggplot(Incident_Melt,aes(x = variable, y = value,fill = Incident)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format()) + 
  ggtitle("Before and After Freddie Gray")
  
#Find the amount of crimes per day
# Make a new column for the ratio of crimes per day
# 2015 had 1232 days between Freddie Gray (Frediee grey 2015-04-19) and start of data
V2015df$CrimePerDay <- V2015df$count / 1232  

# Post Fredie Gray there are 885 days of data
V2018df$CrimePerDay <- V2018df$count / 885  

pie(V2015df$count, labels = V2015df$Incident , main="Pie Chart of Countries")

  
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  #scale_y_continuous(labels = percent_format())







## Aprioriiii



# Rule mine for after Freddie Grey
V2018 <- Victim2018 %>% select( MONTH, Floor,Day,INSIDE_OUTSIDE,District_Name,IncidentOffenseCOMBINE)             

#these don't seem useable for now
# NARRATIVE,ImportantDate,DOMESTIC,COMMERCIAL
# SHOOTING, Hour, POST_1,NBRDESC,SECTOR_1, LONG,HOUSING, RNDSTREET, HATE_CRIME
# PREMISE_TYPE, V_SEX,V_RACE, ZIP_CODE, Match_addr, DISTRICT_1,

V2018 = V2018 %>%  mutate( MONTH = factor(MONTH),Floor = factor(Floor), Day= factor(Day), 
INSIDE_OUTSIDE=factor(INSIDE_OUTSIDE),District_Name=factor(District_Name), IncidentOffenseCOMBINE= factor(IncidentOffenseCOMBINE))            

## these don't seem useable for now
# , NARRATIVE=factor(NARRATIVE),
# ImportantDate = factor(ImportantDate), SHOOTING=factor(SHOOTING), Hour=factor(Hour), 
# DOMESTIC=factor(DOMESTIC),COMMERCIAL=factor(COMMERCIAL),POST_1=factor(POST_1),NBRDESC=factor(NBRDESC),SECTOR_1=factor(SECTOR_1), 
# LONG=factor(LONG),HOUSING=factor(HOUSING), RNDSTREET=factor(RNDSTREET), HATE_CRIME=factor(HATE_CRIME)
# PREMISE_TYPE=factor(PREMISE_TYPE),V_SEX=factor(V_SEX),V_RACE=factor(V_RACE)
#  ZIP_CODE=factor(ZIP_CODE), Match_addr=factor(Match_addr), DISTRICT_1=factor(DISTRICT_1)


Rules_2015 = apriori(V2015,parameter = list(confidence = 0.0001, support=0.01))

# Store results in a dataframe
Results = as(Rules_2018,"data.frame")

# To see what variables we have available, use the names() function
names(Results)

# Let's see the rules that were found in our data
# All of these have at least 0.85 confidence
Results %>% select(rules, confidence, support, lift) %>% arrange(desc(confidence))


# Rule mine for before Freddie Grey

V2015 <- Victim2015 %>% select( MONTH, Floor,Day,INSIDE_OUTSIDE,District_Name,IncidentOffenseCOMBINE)             

#these don't seem useable for now
# NARRATIVE,ImportantDate,DOMESTIC,COMMERCIAL
# SHOOTING, Hour, POST_1,NBRDESC,SECTOR_1, LONG,HOUSING, RNDSTREET, HATE_CRIME
# PREMISE_TYPE, V_SEX,V_RACE, ZIP_CODE, Match_addr, DISTRICT_1,

V2015 = V2015 %>%  mutate( MONTH = factor(MONTH),Floor = factor(Floor), Day= factor(Day), 
INSIDE_OUTSIDE=factor(INSIDE_OUTSIDE),District_Name=factor(District_Name), IncidentOffenseCOMBINE= factor(IncidentOffenseCOMBINE))            

## these don't seem useable for now
# , NARRATIVE=factor(NARRATIVE),
# ImportantDate = factor(ImportantDate), SHOOTING=factor(SHOOTING), Hour=factor(Hour), 
# DOMESTIC=factor(DOMESTIC),COMMERCIAL=factor(COMMERCIAL),POST_1=factor(POST_1),NBRDESC=factor(NBRDESC),SECTOR_1=factor(SECTOR_1), 
# LONG=factor(LONG),HOUSING=factor(HOUSING), RNDSTREET=factor(RNDSTREET), HATE_CRIME=factor(HATE_CRIME)
# PREMISE_TYPE=factor(PREMISE_TYPE),V_SEX=factor(V_SEX),V_RACE=factor(V_RACE)
#  ZIP_CODE=factor(ZIP_CODE), Match_addr=factor(Match_addr), DISTRICT_1=factor(DISTRICT_1)

Rules_2018 = apriori(V2018,parameter = list(confidence = 0.0001, support=0.01))

# Store results in a dataframe
Results = as(Rules_2018,"data.frame")

# To see what variables we have available, use the names() function
names(Results)

# Let's see the rules that were found in our data
# All of these have at least 0.85 confidence
Results %>% select(rules, confidence, support, lift) %>% arrange(desc(confidence))
