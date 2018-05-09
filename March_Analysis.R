# Load the data into the 

RegularSeasonDetailedResults <- read_csv("RegularSeasonDetailedResults.csv")

szn <- RegularSeasonDetailedResults


View(szn)


View(szn1)


# New column that shows difference in Score
szn$Score <- abs(szn$Wscore - szn$Lscore)

#check that it's an integer - it is
class(szn$Score)

# Difference in 3 pointer attempts
szn$attempt3 <- abs(szn$Wfga3 - szn$Lfga3)

# Sum up all the 3 pointer attempts so we can compare them year to year
szn_Attempt_summed <- aggregate(szn$Wfga3, by=list(Category=szn$Season), FUN=sum)

# Change column names to keep track of which column is the data column and which is the 
colnames(szn_Attempt_summed) <- c("Season", "Attempt")

# Let's plot a histogram to see the how the volume of 3 pointers attempted has changed over the years
library(ggplot2)

ggplot(szn_Attempt_summed)+ geom_bar(aes(x=Season,y=Attempt),stat="identity", 
fill="gold") +coord_flip() + ggtitle("Histogram of 3-Pointers Attempted")+theme(plot.title=element_text(face="bold.italic", 
color="black", lineheight=1.5, hjust=0.5), axis.text.y=element_text(size=rel(0.8)))


# Create df that summs season and number of 3 pointers made successfully 
szn_Made_summed <- aggregate(szn$Wfgm3, by=list(Category=szn$Season), FUN=sum)

# Accurate colnames
colnames(szn_Made_summed) <- c("Season", "Made")

# Merge two df's

szn_threes <- merge(szn_Attempt_summed,szn_Made_summed,  by= "Season")
head(szn_threes)

# Let's try the fill function
# First we need to melt the columns to make a category for three pointers made and three pointers attempted
library(reshape2)
threes_melt <-melt(szn_threes, id = c('Season'))
colnames(threes_melt) <- c("Season", "Type", "Points")
View(threes_melt)
# We create two Linear Regression models to see that they both increase, however this is done at a different rate

ggplot(data = threes_melt, mapping = aes(y = Points, x = Season)) +
geom_point(aes(color = Type), size = 2) +ggtitle("3's Attempted vs. 3's Made")+
geom_smooth(aes(color = Type), method = "lm", se = FALSE)  


# Compare correlations with this visual

pairs(szn_threes, pch = 20, col = "#006EA1")

# Confirm visuals with statistics
cor(szn_threes)


