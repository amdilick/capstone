write.csv(TN2019, "TN2019.csv", row.names = FALSE)
write.csv(TN2019_init, "TN2019_init.csv", row.names = FALSE)
write.csv(TN2019_regression, "TN2019_regression.csv", row.names = FALSE)
write.csv(TN2019_reg, "TN2019_reg.csv", row.names = FALSE)
write.csv(TN2019_demographics, "TN2019_demographics.csv", row.names = FALSE)
write.csv(TN2019_race, "TN2019_race.csv", row.names = FALSE)
write.csv(TN2019_ethnicity, "TN2019_ethnicity.csv", row.names = FALSE)
write.csv(TN2019_main, "TN2019_main.csv", row.names = FALSE)

contents(TN2019)


TN2019_race_ethnicity <- subset(TN2019, select=c('action_taken', 'Purchaser_Type', 'derived_loan_product_type','applicant_ethnicity_1', 'applicant_race_1' ))
write.csv(TN2019_race_ethnicity, "TN2019_race_ethnicity.csv", row.names = FALSE)
