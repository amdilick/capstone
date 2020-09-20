# Univariate stats - graphs for demographic details
#  rows = 197926, cols = 48
library(ggplot2)
library(gridExtra)

###############    SEX 

s1 <- ggplot(TN2019_init, aes(x=derived_sex)) + ggtitle("Derived Sex") + xlab("Sex") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
s2 <- ggplot(TN2019_init, aes(x=Applicant_Sex)) + ggtitle("Applicant Sex") + xlab("Sex") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
s3 <- ggplot(TN2019_init, aes(x=Co_Applicant_Sex)) + ggtitle("Co-applicant Sex") + xlab("Sex") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(s1, s2, s3, ncol=1)

# keep applicant_sex
TN2019_init <- subset(TN2019_init, select=-derived_sex)
TN2019_init <- subset(TN2019_init, select=-Co_Applicant_Sex)

###################  RACE

r1 <- ggplot(TN2019_init, aes(x=derived_race)) + ggtitle("Derived Race") + xlab("Race") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
r2 <- ggplot(TN2019_init, aes(x=Applicant_Race)) + ggtitle("Applicant Race") + xlab("Race") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
r3 <- ggplot(TN2019_init, aes(x=Co_Applicant_Race)) + ggtitle("Co-applicant Race") + xlab("Race") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(r1, r2, r3, ncol=1)

# keep derived_race
TN2019_init <- subset(TN2019_init, select=-Applicant_Race)
TN2019_init <- subset(TN2019_init, select=-Co_Applicant_Race)

################## ETHNICITY

e1 <- ggplot(TN2019_init, aes(x=derived_ethnicity)) + ggtitle("Derived Ethnicity") + xlab("Ethnicity") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
e2 <- ggplot(TN2019_init, aes(x=Applicant_Ethnicity)) + ggtitle("Applicant Ethnicity") + xlab("Ethnicity") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
e3 <- ggplot(TN2019_init, aes(x=Co_Applicant_Ethnicity)) + ggtitle("Co-applicant Ethnicity") + xlab("Ethnicity") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(e1, e2, e3, ncol=1)

# keep derived_ethnicity
TN2019_init <- subset(TN2019_init, select=-Applicant_Ethnicity)
TN2019_init <- subset(TN2019_init, select=-Co_Applicant_Ethnicity)


##################  AGE

a1 <- ggplot(TN2019_init, aes(x=applicant_age)) + ggtitle("Applicant Age") + xlab("Age") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
a2 <- ggplot(TN2019_init, aes(x=co_applicant_age)) + ggtitle("Co-Applicant Age") + xlab("Age") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(a1, a2, ncol=1)

# keep applicant_age
TN2019_init <- subset(TN2019_init, select=-co_applicant_age)


##################  LOAN TYPE

loan1 <- ggplot(TN2019_init, aes(x=derived_loan_product_type)) + ggtitle("Derived Loan Product Type") + xlab("Loan Product Type") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
loan2 <- ggplot(TN2019_init, aes(x=Loan_Type)) + ggtitle("Loan Type") + xlab("Loan Type") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
loan3 <- ggplot(TN2019_init, aes(x=Lien_Status)) + ggtitle("Lien Status") + xlab("Lien Status") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(loan1, loan2, loan3, ncol=1)

# remove derived_loan_product_type
TN2019_init <- subset(TN2019_init, select=-derived_loan_product_type)


##################  ACTION TAKEN

act1 <- ggplot(TN2019_init, aes(x=Application_Status)) + ggtitle("Application Status") + xlab("Application Status") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot(act1)
act2 <- ggplot(TN2019_init, aes(x=Action_Taken_Description)) + ggtitle("Action Taken") + xlab("Action Taken") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
act3 <- ggplot(TN2019_init, aes(x=Denial_Reason)) + ggtitle("Denial Reason") + xlab("Denial Reason") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(act2, act3, ncol=1)


##################  OTHER CATEGORIES

other1 <- ggplot(TN2019_init, aes(x=Loan_Purpose)) + ggtitle("Loan Purpose") + xlab("Loan Purpose") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
other2 <- ggplot(TN2019_init, aes(x=Construction_Method)) + ggtitle("Construction Method") + xlab("Construction Method") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
other3 <- ggplot(TN2019_init, aes(x=Occupancy_Type)) + ggtitle("Occupancy Type") + xlab("Occupancy Type") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(other1, other2, other3, ncol=1)

#  rows = 197926, cols = 40

# create subset for regression analysis
TN2019_regression <- TN2019_init

##################
# remove columns only applicable to approved applications
TN2019_regression <- subset(TN2019_regression, select =-combined_loan_to_value_ratio)
TN2019_regression <- subset(TN2019_regression, select =-interest_rate)
TN2019_regression <- subset(TN2019_regression, select =-rate_spread)
TN2019_regression <- subset(TN2019_regression, select =-total_loan_costs)
TN2019_regression <- subset(TN2019_regression, select =-total_points_and_fees)
TN2019_regression <- subset(TN2019_regression, select =-origination_charges)
TN2019_regression <- subset(TN2019_regression, select =-discount_points)
TN2019_regression <- subset(TN2019_regression, select =-lender_credits)
TN2019_regression <- subset(TN2019_regression, select =-loan_term)
TN2019_regression <- subset(TN2019_regression, select =-prepayment_penalty_term)
TN2019_regression <- subset(TN2019_regression, select =-intro_rate_period)

##################  numeric columns - loan amount, property value, income
library(dplyr)
library(ggpubr)

qqnorm(TN2019_regression$income, main="Normal Q-Q plot for Income")
qqnorm(TN2019_regression$property_value, main="Normal Q-Q plot for Property Value")
qqnorm(TN2019_regression$loan_amount, main="Normal Q-Q plot for Loan Amount")

TN2019_regression <- TN2019_init
TN2019_dTree <- TN2019_init
TN2019_approved <- subset(TN2019_init, Application_Status == 'approved')
TN2019_denied <- subset(TN2019_init, Application_Status == 'denied')



