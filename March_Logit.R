setwd("~/Documents/2018/Graduate School/Spring 2018/DATA_601/March_Madness")
library(readxl)
library(readr)
Teams <- read_excel("Teams.xlsx")
TourneySeeds <- read_csv("TourneySeeds.csv")
TourneyDetailedResults <- read_csv("TourneyDetailedResults.csv")

# Change TourneySeeds to only have years in the range of 2003 to 2016 since those are the years 
# included in TourneyDetailed Results
TourneySeeds <- subset(TourneySeeds, TourneySeeds$Season >= 2003 & TourneySeeds$Season <= 2016)


# change Teams data into the same variable names as TourneySeeds
colnames(Teams) <- c("Team", "Team Name")

# merge TourneySeeds and team name df to get matching seed and team names
team_seeds <- merge(TourneySeeds, Teams, by="Team", all = FALSE)
MarchDetailedResults <- TourneyDetailedResults

# keep only the tournament results that I want 
MarchDetailedResults1 <- subset(MarchDetailedResults, select=c(Season,Wteam,Lteam))

# We find out through this that each team only appears once on team_seed list
colnames(team_seeds) <- c("Team", "Season", "Seed", "TeamName")
team_seeds %>% filter(str_detect(TeamName, "Duke"))

# Step 2, matching losing seed columns

team_seedsL <- team_seeds
colnames(team_seedsL) <- c("Lteam", "Season", "Seed", "TeamName")
MarchDetailedResults2 = merge(MarchDetailedResults1, team_seedsL,by.x=c("Season", "Lteam"), by.y=c("Season", "Lteam"))


# Problem. We need to get a winning seed column and a losing seed column
team_seedsW <- team_seeds
colnames(team_seedsW) <- c("Wteam", "Season", "Seed", "TeamName")
MarchDetailedResults3 = merge(MarchDetailedResults2, team_seedsW,by.x=c("Season", "Wteam"), by.y=c("Season", "Wteam"))




# Better column names 
colnames(MarchDetailedResults3) <- c("Season", "Loss", "Win", "Wseed", "Wteam_name", "Lseed", "Lteam_name")

# Disregard region and play in characters that come before and after seed number
March <- MarchDetailedResults3

March$Wseed = gsub("[^0-9]", "", March$Wseed)
March$Lseed = gsub("[^0-9]", "", March$Lseed)


# Make wide data longer with melt function. This is done so the winning
# teams and losing teams can get their own column
library(reshape2)
March1 <-melt(March, id = c('Season', 'Wseed', 'Wteam_name', 'Lseed', 'Lteam_name'))

# Better columns
colnames(March1) <- c('Season', 'Wseed', 'Wteam_name', 'Lseed', 'Lteam_name', 'Outcome', 'TeamId')

#good benchmark, let's write csv file
write.csv(March1, file = "March1.csv")

#read it
March1 <- read_csv("March1.csv")

# transform seed columns to numeric
March1 <- March1 %>% transform(Wseed = as.numeric(Wseed), Lseed = as.numeric(Lseed))
March2 <- March1

# New column that shows difference in seeds
March2$DifferenceInSeed <- abs(March2$Wseed - March2$Lseed)

# Let's recode the Outcome variable to be 1 if "Win" and 0 otherwise
March2 = March2 %>% mutate(Outcome = I(Outcome == "Win") %>% as.numeric())

# Split data into Training and Test sets
set.seed(95736) # Remember to always use set.seed
TrainIndex = sample(1:nrow(March2), round(0.7*nrow(March2)))

March2Train = March2[TrainIndex, ] 
March2Test = March2[-TrainIndex, ]


March2Logit = glm(Outcome ~ DifferenceInSeed, 
                  data = March2Train,
                  family = "binomial")

summary(March2Logit)

# First we will add the predicted probabilities to the Test data
March2Test = March2Test %>% 
  mutate(EstimatedProb = predict(March2Logit,
                                 newdata = March2Test,
                                 type = "response"))

# View the results
# This is a good example of the masking effect in R
# The MASS package also has a select function, so to aviod an error
# you must use dplyr::select in the code below
March2Test %>% dplyr::select(Outcome,EstimatedProb,Seed) %>% 
  slice(1:5) # returns the first 5 rows

# Testing multiple cut-offs
CutOffs = c(0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)

# Create empty dataframe to store results
MisClassRate = data.frame(CutOff = numeric(),
                          TestMSPE = numeric())

for(i in 1:length(CutOffs)) {
  March2Predictions = I(March2Test$EstimatedProb >= CutOffs[i]) %>% as.numeric()
  
  MisClassRate[i,"CutOff"] = CutOffs[i]
  MisClassRate[i,"TestMSPE"] = 
    (1/nrow(March2Test)) * sum(I(March2Predictions != March2Test$Outcome))
}

# View Results
MisClassRate

# Let's add our predictions for the 0.5 cut-off to the HeartTest dataframe
March2Test = March2Test %>% 
  mutate(OutcomePredicted = I(EstimatedProb >= 0.5) %>% as.numeric())

# Quick confusion matrix
table(March2Test$Outcome, # Vertical Values
      March2Test$OutcomePredicted) # Horizontal Values


# As a percentage
table(March2Test$Outcome, March2Test$OutcomePredicted) / nrow(March2Test)

# Compare cut-offs
March2Test = March2Test %>% 
  mutate(March2Predicted = I(EstimatedProb >= 0.45) %>% as.numeric())

table(March2Test$Outcome, March2Test$March2Predicted) / nrow(March2Test)

# If you need to save the results, use dplyr
HeartResults0.15 = HeartTest %>% 
  group_by(HeartDisease,HeartDiseasePredicted) %>% 
  summarise(Patients = n()) %>% 
  ungroup() %>% 
  mutate(Percentage = Patients/sum(Patients))

# View Results
HeartResults0.15

#####

install.packages("effects")
library(effects)

plot(allEffects(March2Logit))

