# Neural Network 
Lend <- LendingClub
str(Lend)
#EDA
corrgram::corrgram(Lend)
#Count how many 0's and 1's
table(Lend$loan_default)

#remove column where data type is character
Lend <- subset(Lend, select= -residence_property)

#Let's just make this easier and practice with a few columns
Lend1 <- Lend
Lend1 <- Lend1 %>% select(loan_default, loan_amnt, adjusted_annual_inc, pct_loan_income, dti)

#normalize data, originally I started with these 4 variables to make it easier (data partitoining)
Lend1$loan_amnt <- (Lend1$loan_amnt - min(Lend1$loan_amnt)) / (max(Lend1$loan_amnt)- min(Lend1$loan_amnt))
Lend1$adjusted_annual_inc <- (Lend1$adjusted_annual_inc - min(Lend1$adjusted_annual_inc)) / (max(Lend1$adjusted_annual_inc)- min(Lend1$adjusted_annual_inc))
Lend1$pct_loan_income <- (Lend1$pct_loan_income - min(Lend1$pct_loan_income)) / (max(Lend1$pct_loan_income)- min(Lend1$pct_loan_income))
Lend1$dti <- (Lend1$dti - min(Lend1$dti)) / (max(Lend1$dti)- min(Lend1$dti))


# all variables now 

Lend1$months_since_first_credit <- (Lend1$months_since_first_credit - min(Lend1$months_since_first_credit)) / (max(Lend1$months_since_first_credit)- min(Lend1$months_since_first_credit))
Lend1$inq_last_6mths <- (Lend1$inq_last_6mths - min(Lend1$inq_last_6mths)) / (max(Lend1$inq_last_6mths)- min(Lend1$inq_last_6mths))
Lend1$open_acc <- (Lend1$open_acc - min(Lend1$open_acc)) / (max(Lend1$open_acc)- min(Lend1$open_acc))
Lend1$bc_util <- (Lend1$bc_util - min(Lend1$bc_util)) / (max(Lend1$bc_util)- min(Lend1$bc_util))
Lend1$num_accts_ever_120_pd <- (Lend1$num_accts_ever_120_pd - min(Lend1$num_accts_ever_120_pd)) / (max(Lend1$num_accts_ever_120_pd)- min(Lend1$num_accts_ever_120_pd))
Lend1$pub_rec_bankruptcies <- (Lend1$pub_rec_bankruptcies - min(Lend1$pub_rec_bankruptcies)) / (max(Lend1$pub_rec_bankruptcies)- min(Lend1$pub_rec_bankruptcies))


#Data partition
set.seed(222)
ind <- sample(2, nrow(Lend1), replace = TRUE, prob = c(0.7, 0.3))
training <- Lend1[ind==1,]
testing <- Lend1[ind==2,]

# Neural Networks
library(neuralnet)
set.seed(333)
n1 <- neuralnet(loan_default~loan_amnt+adjusted_annual_inc+pct_loan_income+dti,
                data = Lend1,
                hidden = 1,
                err.fct = "ce",
                linear.output = FALSE)
plot(n1)


# Prediction
output <- compute(n1, training[,-1])
head(output$net.result)
head(training[1,])

# Node Output Calculations with Sigmoid Activation Function
in4 <- 0.0455 + (0.82344*0.7586206897) + (1.35186*0.8103448276) + (-0.87435*0.6666666667)
out4 <- 1/(1+exp(-in4))
in5 <- -7.06125 +(8.5741*out4)
out5 <- 1/(1+exp(-in5))

# Confusion Matrix & Misclassification Error - training data
output <- compute(n1, training[,-1])
p1 <- output$net.result
pred1 <- ifelse(p1>0.22, 1, 0)
tab1 <- table(pred1, training$loan_default)
tab1
1-sum(diag(tab1))/sum(tab1)

# Confusion Matrix & Misclassification Error - testing data
output <- compute(n1, testing[,-1])
p2 <- output$net.result
pred2 <- ifelse(p2>0.22, 1, 0)
tab2 <- table(pred2, testing$loan_default)
tab2
1-sum(diag(tab2))/sum(tab2)

