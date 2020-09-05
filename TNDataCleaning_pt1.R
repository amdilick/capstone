# TN Data Cleaning Part 1
# Begining info: row count = 389728, col count = 99

# Get info about the TN Dataset
library(Hmisc)
describe(X2019publicTN_allColumns)

# remove columns that have info we don't need
TN2019_init <- subset(X2019publicTN_allColumns, select=-activity_year)
TN2019_init <- subset(TN2019_init, select=-state_code)

# remove columns with duplicated age information
# use applicant_age and co_applicant_age columns
TN2019_init <- subset(TN2019_init, select=-applicant_age_above_62)
TN2019_init <- subset(TN2019_init, select=-co_applicant_age_above_62)

# remove columns that are covered in 'derived' columns that will be used in the analysis
# derived_loan_product_type
TN2019_init <- subset(TN2019_init, select=-loan_type)
TN2019_init <- subset(TN2019_init, select=-lien_status)
# derived_dwelling_category
TN2019_init <- subset(TN2019_init, select=-construction_method)
TN2019_init <- subset(TN2019_init, select=-total_units)
# derived_msa_md
TN2019_init <- subset(TN2019_init, select=-county_code)
TN2019_init <- subset(TN2019_init, select=-census_tract)

# remove columns with low data density or no data 
TN2019_init <- subset(TN2019_init, select=-total_points_and_fees)
TN2019_init <- subset(TN2019_init, select=-discount_points)
TN2019_init <- subset(TN2019_init, select=-lender_credits)
TN2019_init <- subset(TN2019_init, select=-prepayment_penalty_term)
TN2019_init <- subset(TN2019_init, select=-intro_rate_period)
TN2019_init <- subset(TN2019_init, select=-multifamily_affordable_units)
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_2)
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_3)
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_4)
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_5)
TN2019_init <- subset(TN2019_init, select=-applicant_race_2)
TN2019_init <- subset(TN2019_init, select=-applicant_race_3)
TN2019_init <- subset(TN2019_init, select=-applicant_race_4)
TN2019_init <- subset(TN2019_init, select=-applicant_race_5)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_2)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_3)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_4)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_5)
TN2019_init <- subset(TN2019_init, select=-applicant_ethnicity_2)
TN2019_init <- subset(TN2019_init, select=-applicant_ethnicity_3)
TN2019_init <- subset(TN2019_init, select=-applicant_ethnicity_4)
TN2019_init <- subset(TN2019_init, select=-applicant_ethnicity_5)
TN2019_init <- subset(TN2019_init, select=-aus_2)
TN2019_init <- subset(TN2019_init, select=-aus_3)
TN2019_init <- subset(TN2019_init, select=-aus_4)
TN2019_init <- subset(TN2019_init, select=-aus_5)
TN2019_init <- subset(TN2019_init, select=-denial_reason_2)
TN2019_init <- subset(TN2019_init, select=-denial_reason_3)
TN2019_init <- subset(TN2019_init, select=-denial_reason_4)

# remove columns with 'observed' data - not relevant info
TN2019_init <- subset(TN2019_init, select=-applicant_ethnicity_observed)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_observed)
TN2019_init <- subset(TN2019_init, select=-applicant_race_observed)
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_observed)
TN2019_init <- subset(TN2019_init, select=-applicant_sex_observed)
TN2019_init <- subset(TN2019_init, select=-co_applicant_sex_observed)

# remove other columns not relevant to this analysis
TN2019_init <- subset(TN2019_init, select=-conforming_loan_limit)
TN2019_init <- subset(TN2019_init, select=-lei)

describe(TN2019_init)
# remove remaining co_applicant columns
TN2019_init <- subset(TN2019_init, select=-co_applicant_race_1)
TN2019_init <- subset(TN2019_init, select=-co_applicant_ethnicity_1)
TN2019_init <- subset(TN2019_init, select=-co_applicant_age)
TN2019_init <- subset(TN2019_init, select=-co_applicant_sex)
TN2019_init <- subset(TN2019_init, select=-co_applicant_credit_score_type)

# row count = 389728, col count = 47
contents(TN2019_init)

# remove rows for demographics with missing values
TN2019_init <- subset(TN2019_init, applicant_race_1!='')
TN2019_init <- subset(TN2019_init, applicant_ethnicity_1!='')

# 'Not applicable' values in race, ethnicity, sex columns indicate  
# purchaser is not a 'natural person' or the info was not reported by
# the loan originator, cannot use these for bias analysis
TN2019_init <- subset(TN2019_init, applicant_ethnicity_1 != 4)
TN2019_init <- subset(TN2019_init, applicant_sex != 4)  
TN2019_init <- subset(TN2019_init, applicant_race_1 != 7)
TN2019_init <- subset(TN2019_init, applicant_age!='8888')

# remove rows in race, ethnicity, sex info columns 
# with codes that indicate information was not provided by the applicant
TN2019_init <- subset(TN2019_init, applicant_ethnicity_1 != 3)
TN2019_init <- subset(TN2019_init, applicant_sex != 3)  
TN2019_init <- subset(TN2019_init, applicant_race_1 != 6)


################################################################################
TN2019_approved <- TN2019_init
TN2019_reg <- TN2019_init




