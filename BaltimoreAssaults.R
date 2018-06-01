# Apriori Rule mining for Arrest Data (focus on Assault)


library(readxl)
#If you do not have arules installed then please use command install.packages("arules")
library(arules)
## heres the link to teh data https://data.baltimorecity.gov/Public-Safety/BPD-Arrest-Data/vpg7-k9kq
library(readr)
library(dplyr)
setwd("~/Documents/2018/Graduate School/Spring 2018/DATA_602/Group_Project_2018")

Weekend_Grouped <-read_excel("Weekend_Grouped_Incidents.xlsx")
View(Weekend_Grouped)
Weekend <- Weekend_Grouped
# Get a data frame with only weekends
library(tidyverse)
Weekend <- Weekend %>% filter(str_detect(Day, "Weekend"))

# Use only ages 25 and less
## Weekend <- Weekend[Weekend$Age <= 25, ] 

# Create Age intervals "20 and younger", "21-25", "26-30", "31 and over"

Weekend$age_cut <- cut(Weekend$Age,
                        breaks = c(-Inf, 21, 26, 31, Inf),  
                        labels = c("20 and younger", "21-25", "26-30", "31 and over"), 
                        right = FALSE)
View(Weekend)


Arrest <- Weekend %>% select(Floor,ChargeDescription, District, age_cut, Sex, Race)

Arrest = Arrest %>%  mutate(age_cut = factor(age_cut), Sex = factor(Sex), Race = factor(Race),
                      ChargeDescription = factor(ChargeDescription), 
                      District = factor(District), Floor= factor(Floor) )

# Let's observe how many counts of certain crimes we have. We grouped in Excel similar crimes
sort(table(Arrest$ChargeDescription), decreasing = TRUE)


# Now we can find the association rules in the data set
Rules = apriori(Arrest, 
                parameter = list(confidence = 0.01, support=0.01)) # Set minimum confidence level

# Store results in a dataframe
Results = as(Rules,"data.frame")
# write.csv(Results, file = "Results_Weekend.csv")
# To see what variables we have available, use the names() function
names(Results)

# Let's see the rules that were found in our data
# All of these have at least 0.85 confidence
Results %>% select(rules, confidence) %>% arrange(desc(confidence))


# Let's see popular crimes
sort(table(Arrest$ChargeDescription), decreasing=TRUE)[1:20]

### Take top two occurring crimes

# Now let's focus on a subset of the rules
# For this we have to use the subset function
SubRules = subset(Rules, subset = rhs %in% ("ChargeDescription=ASSAULT"))

# Turn our results into a dataframe
SubResults = as(SubRules,"data.frame")

# View Results
SubResults %>% select(rules, confidence, support, lift) %>% arrange(desc(confidence))



library(arulesViz)
subrules <- SubRules[quality(SubRules)$confidence > 0.5]



subrules2 <- head(SubRules, n = 9, by = "lift")


plot(subrules2, method = "graph")

sel <- plot(subrules2, measure=c("support", "lift"), shading = "confidence",interactive = TRUE)


##






