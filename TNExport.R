write.csv(TN2019, "TN2019.csv", row.names = FALSE)
write.csv(TN2019, "TN2019_v1.csv", row.names = FALSE)

*write.csv(TN2019_approved_refi, "TN2019_approved_refi.csv", row.names = FALSE)

write.csv(TN2019_gender, "TN2019_gender.csv", row.names= FALSE)


contents(TN2019)


TN2019_race_ethnicity <- subset(TN2019, select=c('action_taken', 'Purchaser_Type', 'derived_loan_product_type','applicant_ethnicity_1', 'applicant_race_1' ))
write.csv(TN2019_race_ethnicity, "TN2019_race_ethnicity.csv", row.names = FALSE)
