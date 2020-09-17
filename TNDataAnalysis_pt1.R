# view proportions of each level in factor columns
TN2019_reg_backup <- TN2019_regression


TN2019_reg_X2 <- subset(TN2019_regression, select =c('Application_Status', 'derived_msa_md',
                                            'derived_loan_product_type', 'derived_dwelling_category', 'derived_ethnicity',
                                            'Applicant_Ethnicity_1', 'Co_Applicant_Ethnicity_1', 'derived_race', 'Applicant_Race_1', 
                                            'Co_Applicant_Race_1','derived_sex', 'Applicant_Sex', 'Co_Applicant_Sex', 'applicant_age', 
                                            'co_applicant_age', 'property_value', 'income', 'debt_to_income_ratio'))
View(TN2019_regression)
describe(TN2019_regression)
summary(TN2019_regression)


tbl_debt <- table(TN2019_regression$debt_to_income_ratio)
prop.table(tbl_debt)

library(Hmisc)
TN2019_X2 <- chiSquare(TN2019_regression$Application_Status ~ TN2019_regression$derived_msa_md +
                          TN2019_regression$derived_loan_product_type + TN2019_regression$derived_dwelling_category +
                          TN2019_regression$derived_ethnicity + TN2019_regression$Applicant_Ethnicity +
                          TN2019_regression$Co_Applicant_Ethnicity + TN2019_regression$derived_race + TN2019_regression$Applicant_Race + 
                          TN2019_regression$Co_Applicant_Race + TN2019_regression$derived_sex + 
                          TN2019_regression$applicant_age + TN2019_regression$Applicant_Sex)  
print(TN2019_X2)
plot(chiSquare(TN2019_regression$Application_Status ~ TN2019_regression$derived_msa_md +
                  TN2019_regression$derived_loan_product_type + TN2019_regression$derived_dwelling_category +
                  TN2019_regression$derived_ethnicity + TN2019_regression$Applicant_Ethnicity +
                  TN2019_regression$Co_Applicant_Ethnicity + TN2019_regression$derived_race + TN2019_regression$Applicant_Race + 
                  TN2019_regression$Co_Applicant_Race + TN2019_regression$derived_sex + 
                  TN2019_regression$applicant_age + TN2019_regression$Applicant_Sex))

#TN2019_Xval <- subset(TN2019_main, select=-property_value)
TN2019_Xval <- subset(TN2019_reg, select=-Application_Status)
TN2019_Xval <- subset(TN2019_Xval, select=-AUS_1)

library(FactoMineR)
library(factoextra)
TN.famd <- FAMD(TN2019_Xval, graph = FALSE)
fviz_screeplot(TN.famd)
var <- get_famd_var(TN.famd)
fviz_contrib(TN.famd, 'var', axes=1)
fviz_contrib(TN.famd, 'var', axes=2)
fviz_contrib(TN.famd, 'var', axes=3)
fviz_famd_var(TN.famd, choice='var', col.var='cos2', gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_famd_var(TN.famd, choice='quanti.var', col.var='cos2', gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

# remove columns from the dataset
TN2019_final <- subset(TN2019_main, select =-income)
TN2019_final <- subset(TN2019_final, select =-Applicant_Ethnicity_1)
TN2019_final <- subset(TN2019_final, select =-Applicant_Race_1)


TN2019_maybe <- subset(TN2019_final, select=c('Application_Status', 'derived_race', 'derived_ethnicity', 'applicant_age','Applicant_Sex' ))
