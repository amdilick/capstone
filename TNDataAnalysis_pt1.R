# Regression analysis
#  rows = 197926, cols = 29

library(Hmisc)
TN2019_reg_backup <- TN2019_regression

#  bivariate
View(TN2019_regression)
contents(TN2019_regression)
describe(TN2019_regression)
summary(TN2019_regression)

tbl_appr_ethn <- table(TN2019_approved$derived_ethnicity)
tbl_den_ethn <- table(TN2019_denied$derived_ethnicity)
tbl_ethn <- table(TN2019_init$Application_Status, TN2019_init$derived_ethnicity)
print(tbl_ethn)
addmargins((tbl_ethn))
tbl1 <-prop.table(tbl_appr_ethn)
tbl2 <-prop.table(tbl_den_ethn)
tbl_prop_ethn <- rbind(tbl1, tbl2)
print(tbl_prop_ethn)
chisq.test(tbl_den_ethn, p=tbl1)

tbl_appr_race <- table(TN2019_approved$derived_race)
tbl_den_race <- table(TN2019_denied$derived_race)
tbl_race <- table(TN2019_init$Application_Status, TN2019_init$derived_race)
print(tbl_race)
tbl3 <-prop.table(tbl_appr_race)
tbl4 <-prop.table(tbl_den_race)
tbl_prop_race <- rbind(tbl3, tbl4)
print(tbl_prop_race)
chisq.test(tbl_den_race, p=tbl3)

tbl_appr_sex <- table(TN2019_approved$Applicant_Sex)
tbl_den_sex <- table(TN2019_denied$Applicant_Sex)
tbl_sex <- table(TN2019_init$Application_Status, TN2019_init$Applicant_Sex)
addmargins((tbl_sex))
tbl5 <-prop.table(tbl_appr_sex)
tbl6 <-prop.table(tbl_den_sex)
tbl_prop_sex <- rbind(tbl5, tbl6)
print(tbl_prop_sex)
chisq.test(tbl_den_sex, p=tbl5)

tbl_lien <- table(TN2019_init$Application_Status, TN2019_init$Lien_Status)
print(tbl_lien)
prop.table(tbl_lien)
chisq.test(tbl_lien)

tbl_debt <- table(TN2019_init$Application_Status, TN2019_init$debt_to_income_ratio)
print(tbl_debt)
prop.table(tbl_debt)
chisq.test(tbl_debt)

tbl_age <- table(TN2019_init$Application_Status, TN2019_init$applicant_age)
print(tbl_age)
prop.table(tbl_age)
chisq.test(tbl_age)

TN2019_X2 <- chiSquare(TN2019_regression$Application_Status ~ TN2019_regression$derived_race+
                          TN2019_regression$derived_ethnicity + TN2019_regression$Applicant_Sex + 
                          TN2019_regression$applicant_age + TN2019_regression$Loan_Type +
                          TN2019_regression$Lien_Status+ TN2019_regression$Occupancy_Type + 
                          TN2019_regression$Loan_Purpose + TN2019_regression$Applicant_Credit_Score_Type +
                          TN2019_regression$Construction_Method +  TN2019_regression$debt_to_income_ratio +
                          TN2019_regression$Open_End_Line_of_Credit + TN2019_regression$Preapproval)  
print(TN2019_X2)
plot(TN2019_X2)


#  correlation analysis on the numeric columns
library(corrplot)
TN2019_num <- TN2019_regression[, c('income', 'property_value', 'loan_amount')]
TN2019_corr <- cor(TN2019_num)
print(TN2019_corr)
corrplot(TN2019_corr, method='number')


# multifactor analysis
library(FactoMineR)
library(factoextra)
TN2019_Xval <- subset(TN2019_regression, select=-Application_Status)


TN.famd <- FAMD(TN2019_Xval, graph = FALSE)
fviz_screeplot(TN.famd)
var <- get_famd_var(TN.famd)
fviz_contrib(TN.famd, 'var', axes=1)
fviz_contrib(TN.famd, 'var', axes=2)
fviz_famd_var(TN.famd, choice='var', col.var='cos2', 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_famd_var(TN.famd, choice='quanti.var', col.var='cos2', 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

# remove columns from the dataset
TN2019_final <- subset(TN2019_regression, select =-income)


summary(TN2019_regression$Open_End_Line_of_Credit)

