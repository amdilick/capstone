TN2019_approved <- subset(TN2019, Action_Taken=='approved')
TN2019_denied <- subset(TN2019, Action_Taken == 'denied')

TN2019_approved_refi <- subset(TN2019_approved, Loan_Purpose=='Refinancing')

#############
##  NOTE:  'Exempt' values in fields are allowed for lending institutions originating fewer than 500 mortgage loans per
##  https://www.ffiec.gov/hmda/pdf/2019guide.pdf  pg. 36
#############

