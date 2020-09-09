# TN Data Cleaning Part 2
# Demographics:  remove rows, convert numeric levels to descriptives
# only keep loans originated and not sold
# only keep approved/denied applications, not incomplete or withdrawn
# Beginning info: row count = 162454, col count = 46

library(Hmisc)
describe(TN2019_reg)

# remove columns w/ data for approved apps and NA in rows for denied apps
TN2019_reg <- subset(TN2019_reg, select=-combined_loan_to_value_ratio)
TN2019_reg <- subset(TN2019_reg, select=-interest_rate)
TN2019_reg <- subset(TN2019_reg, select=-rate_spread)
TN2019_reg <- subset(TN2019_reg, select=-total_loan_costs)
TN2019_reg <- subset(TN2019_reg, select=-origination_charges)
TN2019_reg <- subset(TN2019_reg, select=-debt_to_income_ratio)

# remove columns w/





















# Ending info: row count = 162454,  col count = 55