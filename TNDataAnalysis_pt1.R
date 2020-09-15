# view proportions of each level in factor columns
TN2019_backup <- TN2019_reg


tbl_debt <- table(TN2019_reg$debt_to_income_ratio)
prop.table(tbl_debt)
TN2019_reg$Applicant_Ethnicity <- TN2019_reg$Applicant_Ethnicity_1
TN2019_reg <- subset(TN2019_reg, select=-Applicant_Ethnicity_1)
TN2019_reg$Co_Applicant_Ethnicity <- TN2019_reg$Co_Applicant_Ethnicity_1
TN2019_reg <- subset(TN2019_reg, select=-Co_Applicant_Ethnicity_1)
TN2019_reg$Applicant_Race <- TN2019_reg$Applicant_Race_1
TN2019_reg <- subset(TN2019_reg, select=-Applicant_Race_1)
TN2019_reg$Co_Applicant_Race<- TN2019_reg$Co_Applicant_Race_1
TN2019_reg <- subset(TN2019_reg, select=-Co_Applicant_Race_1)

TN2019_X2 <- chiSquare(TN2019_reg$Application_Status ~ TN2019_reg$derived_msa_md +
      TN2019_reg$derived_loan_product_type + TN2019_reg$derived_dwelling_category +
      TN2019_reg$derived_ethnicity + TN2019_reg$Applicant_Ethnicity +
      TN2019_reg$Co_Applicant_Ethnicity + TN2019_reg$derived_race + TN2019_reg$Applicant_Race + 
      TN2019_reg$Co_Applicant_Race + TN2019_reg$derived_sex + 
      TN2019_reg$applicant_age + TN2019_reg$Applicant_Sex)  
print(TN2019_X2)
plot(chiSquare(TN2019_reg$Application_Status ~ TN2019_reg$derived_msa_md +
      TN2019_reg$derived_loan_product_type + TN2019_reg$derived_dwelling_category +
      TN2019_reg$derived_ethnicity + TN2019_reg$Applicant_Ethnicity +
      TN2019_reg$Co_Applicant_Ethnicity + TN2019_reg$derived_race + TN2019_reg$Applicant_Race + 
      TN2019_reg$Co_Applicant_Race + TN2019_reg$derived_sex + 
      TN2019_reg$applicant_age + TN2019_reg$Applicant_Sex))

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
