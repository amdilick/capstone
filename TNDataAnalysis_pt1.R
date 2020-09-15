# view proportions of each level in factor columns

tbl_debt <- table(TN2019_reg$debt_to_income_ratio)
prop.table(tbl_debt)


TN2019_X2 <- chiSquare(TN2019$Application_Status ~ TN2019$derived_msa_md +
      TN2019$derived_loan_product_type + TN2019$derived_dwelling_category +
      TN2019$derived_ethnicity + TN2019$derived_race + TN2019$derived_sex +
      TN2019$Occupancy_Type + TN2019$applicant_age + TN2019$Applicant_Sex +
      TN2019$Loan_Purpose + TN2019$Preapproval + TN2019$AUS_1 + 
      TN2019$Applicant_Credit_Score_Type)
print(TN2019_X2)
plot(chiSquare(TN2019$Application_Status ~ TN2019$derived_msa_md +
                  TN2019$derived_loan_product_type + TN2019$derived_dwelling_category +
                  TN2019$derived_ethnicity + TN2019$derived_race + TN2019$derived_sex +
                  TN2019$Occupancy_Type + TN2019$applicant_age + TN2019$Applicant_Sex +
                  TN2019$Loan_Purpose + TN2019$Preapproval +  
                  TN2019$Applicant_Credit_Score_Type))

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
TN2019_final <- subset(TN2019_final, select = -county_code)

TN2019_maybe <- subset(TN2019_final, select=c('Application_Status', 'derived_race', 'derived_ethnicity', 'applicant_age','Applicant_Sex' ))
