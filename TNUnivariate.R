# Univariate stats - graphs for demographic details
#  rows = 197926, cols =27
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

################### CREDIT SCORE
c1 <- ggplot(TN2019_init, aes(x=Applicant_Credit_Score_Type)) + ggtitle("Applicant Credit Score Type") + 
  xlab("Applicant Credit Score Type") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + coord_flip() + theme_minimal()
c2 <- ggplot(TN2019_init, aes(x=Co_Applicant_Credit_Score_Type)) + ggtitle("Co-Applicant Credit Score Type") + 
  xlab("Co-Applicant Credit Score Type") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + 
  ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(c1, c2, ncol=1)

# keep applicant_credit_score_type
TN2019_init <- subset(TN2019_init, select=-Co_Applicant_Credit_Score_Type)
table(TN2019_init$Applicant_Credit_Score_Type)
TN2019_init$Applicant_Credit_Score_Type <- factor(TN2019_init$Applicant_Credit_Score_Type)


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


##################  APPLICATION STATUS

act1 <- ggplot(TN2019_init, aes(x=Application_Status)) + ggtitle("Application Status") + xlab("Application Status") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot(act1)


##################  OTHER CATEGORIES

other1 <- ggplot(TN2019_init, aes(x=Loan_Purpose)) + ggtitle("Loan Purpose") + xlab("Loan Purpose") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
other2 <- ggplot(TN2019_init, aes(x=Construction_Method)) + ggtitle("Construction Method") + xlab("Construction Method") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
other3 <- ggplot(TN2019_init, aes(x=Occupancy_Type)) + ggtitle("Occupancy Type") + xlab("Occupancy Type") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(other1, other2, other3, ncol=1)

other4 <- ggplot(TN2019_init, aes(x=Preapproval)) + ggtitle("Preapproval") + xlab("Preapproval") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
other5 <- ggplot(TN2019_init, aes(x=Open_End_Line_of_Credit)) + ggtitle("Open-end Line of Credit") + xlab("Open-end Line of Credit") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(other4, other5, ncol=1)

other14 <- ggplot(TN2019_init, aes(x=debt_to_income_ratio)) + ggtitle("Debt To Income Ratio") + xlab("Debt To Income Ratio") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(other14, ncol=1)

##################  numeric columns - loan amount, property value, income
library(dplyr)
library(ggpubr)

qqnorm(TN2019_init$income, main="Normal Q-Q plot for Income")
qqnorm(TN2019_init$property_value, main="Normal Q-Q plot for Property Value")
qqnorm(TN2019_init$loan_amount, main="Normal Q-Q plot for Loan Amount")

#  rows = 194873, cols = 17
TN_numeric <- subset(TN2019_init, select=c('income', 'property_value', 'loan_amount'))
TN_numeric <- subset(TN2019_init, income<= 1000)
ggdensity(TN_numeric$income)
ggdensity(TN_numeric$property_value)



# create subset for regression analysis
TN2019_regression <- TN2019_init


TN2019_init_backup <- TN2019_init

TN2019_dTree <- TN2019_init
TN2019_approved <- subset(TN2019_init, Application_Status == 'approved')
TN2019_denied <- subset(TN2019_init, Application_Status == 'denied')

tbl_appr_ethn <- table(TN2019_approved$derived_ethnicity)
tbl_den_ethn <- table(TN2019_denied$derived_ethnicity)
print(tbl_appr_ethn)
print(tbl_den_ethn)
prop.table(tbl_appr_ethn)
prop.table(tbl_den_ethn)

tbl_appr_race <- table(TN2019_approved$derived_race)
tbl_den_race <- table(TN2019_denied$derived_race)
print(tbl_appr_race)
print(tbl_den_race)
prop.table(tbl_appr_race)
prop.table(tbl_den_race)

tbl_appr_race <- table(TN2019_approved$derived_race)
tbl_den_race <- table(TN2019_denied$derived_race)
print(tbl_appr_race)
print(tbl_den_race)
prop.table(tbl_appr_race)
prop.table(tbl_den_race)

tbl_ethn <- table(TN2019_init$Application_Status, TN2019_init$derived_ethnicity)
print(tbl_ethn)
prop.table(tbl_ethn)
