##########################################################################

# convert action_taken levels into 'approved' or 'denied' based on code
convert_action <- function(action_taken){
  if (action_taken == 1){  return('approved')  }
  else if(action_taken == 2){  return('approved')  }
  else if (action_taken == 8){  return('approved')  }
  else if (action_taken == 3){  return('denied')  }
  else if (action_taken == 7){ return('denied') }
}

TN2019$Action_Taken <- sapply(TN2019$action_taken,convert_action)
TN2019$Action_Taken <- as.factor(TN2019$Action_Taken)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -action_taken)

##########################################################################

# convert purchaser_Type levels from numbers to descriptives
convert_purchaser <- function(purchaser_type){
  if (purchaser_type == 0){  return('Not applicable')  }
  else if(purchaser_type == 1){  return('Fannie Mae')  }
  else if (purchaser_type == 2){  return('Ginnie Mae')  }
  else if (purchaser_type == 3){  return('Freddie Mac')  }
  else if (purchaser_type == 4){  return('Farmer Mac')  }
  else if (purchaser_type == 5){  return('Private securitizer')  }
  else if (purchaser_type == 6){  return('Commercial or savings bank or savings association')  }
  else if (purchaser_type == 71){  return('Credit union mortgage company or finance company')  }
  else if (purchaser_type == 72){  return('Life insurance company')  }
  else if (purchaser_type == 8){  return('Affiliate institution')  }
  else if (purchaser_type == 9){  return('Other type of purchaser')  }
}

TN2019$Purchaser_Type <- sapply(TN2019$purchaser_type,convert_purchaser)
TN2019$Purchaser_Type<- as.factor(TN2019$Purchaser_Type)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -purchaser_type)

##########################################################################

# convert loan_purpose levels from numbers to descriptives
convert_loan_purpose <- function(loan_purpose){
  if (loan_purpose == 1){  return('Home purchase')  }
  else if (loan_purpose == 2){  return('Home improvement')  }
  else if (loan_purpose == 31){  return('Refinancing')  }
  else if (loan_purpose == 32){  return('Cash out refinancing')  }
  else if (loan_purpose == 4){ return('Other purpose') }
  else if (loan_purpose == 5){ return('Not applicable') }
}

TN2019$Loan_Purpose <- sapply(TN2019$loan_purpose,convert_loan_purpose)
TN2019$Loan_Purpose <- as.factor(TN2019$Loan_Purpose)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -loan_purpose)

##########################################################################

# convert preapproval levels from numbers to descriptives
convert_preapproval<- function(preapproval){
  if (preapproval== 1){  return('Preapproval requested')  }
  else if (preapproval  == 2){  return('Preapproval not requested')  }
}

TN2019$Preapproval <- sapply(TN2019$preapproval,convert_preapproval)
TN2019$Preapproval <- as.factor(TN2019$Preapproval)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -preapproval)

##########################################################################

# convert reverse_mortgage levels from numbers to descriptives
convert_reverse_mortgage <- function(reverse_mortgage){
  if (reverse_mortgage == 1){  return('Reverse Mortgage')  }
  else if (reverse_mortgage == 2){  return('Not a reverse mortgage')  }
  else if (reverse_mortgage == 1111){  return('Exempt')  }
}

TN2019$Reverse_Mortgage <- sapply(TN2019$reverse_mortgage,convert_reverse_mortgage)
TN2019$Reverse_Mortgage <- as.factor(TN2019$Reverse_Mortgage)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -reverse_mortgage)

##########################################################################

# convert open_end_line_of_credit levels from numbers to descriptives
convert_open_LOC <- function(open_end_line_of_credit){
  if (open_end_line-of_credit == 1){  return('Open-end line of credit')  }
  else if (open_end_line_of_credit == 2){  return('Not an open-end line of credit')  }
  else if (open_end_line-of_credit == 1111){  return('Exempt')  }
}

TN2019$Open_End_Line_of_Credit<- sapply(TN2019$open_end_line_of_credit,convert_open_LOC)
TN2019$Open_End_Line_of_Credit <- as.factor(TN2019$Open_End_Line_of_Credit)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -open_end_line_of_credit)

##########################################################################

# convert business_or_commercial_purpose levels from numbers to descriptives
convert_business_purpose <- function(business_or_commercial_purpose){
  if (business_or_commercial_purpose == 1){  return('Primarily for a business or commercial purpose')  }
  else if (business_or_commercial_purpose == 2){  return('Not primarily for a business or commercial purpose')  }
  else if (business_or_commercial_purpose == 1111){  return('Exempt')  }
}

TN2019$Business_or_Commmercial_Purpose <- sapply(TN2019$business_or_commercial_purpose,convert_business_purpose)
TN2019$Business_or_Commmercial_Purpose <- as.factor(TN2019$Business_or_Commmercial_Purpose)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -business_or_commercial_purpose)

##########################################################################

# convert hoepa_status levels from numbers to descriptives
convert_hoepa_status <- function(hoepa_status){
  if (hoepa_status == 1){  return('High-cost mortgage')  }
  else if (hoepa_status == 2){  return('Not a high-cost mortgage')  }
  else if (hoepa_status == 3){  return('Not applicable')  }
}

TN2019$Hoepa_Status <- sapply(TN2019$hoepa_status,convert_hoepa_status)
TN2019$Hoepa_Status <- as.factor(TN2019$Hoepa_Status)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -hoepa_status)

##########################################################################

# convert negative_amortization levels from numbers to descriptives
convert_negative_amortization <- function(negative_amortization){
  if (negative_amortization == 1){  return('High-cost mortgage')  }
  else if (negative_amortization == 2){  return('Not a high-cost mortgage')  }
  else if (negative_amortization == 3){  return('Not applicable')  }
}

TN2019$Negative_Amortization <- sapply(TN2019$negative_amortization,convert_negative_amortization)
TN2019$Negative_Amortization <- as.factor(TN2019$Negative_Amortization)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -negative_amortization)

##########################################################################

# convert interest_only_payment levels from numbers to descriptives
convert_interest_only_payment <- function(interest_only_payment){
  if (interest_only_payment == 1){  return('High-cost mortgage')  }
  else if (interest_only_payment == 2){  return('Not a high-cost mortgage')  }
  else if (interest_only_payment == 3){  return('Not applicable')  }
}

TN2019$Interest_Only_Payment <- sapply(TN2019$interest_only_payment,convert_interest_only_payment)
TN2019$Interest_Only_Payment(TN2019$Interest_Only_Payment)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -interest_only_payment)

##########################################################################

# convert balloon_payment levels from numbers to descriptives
convert_ballon_payment <- function(balloon_payment){
  if (balloon_payment == 1){  return('Balloon payment')  }
  else if (balloon_payment == 2){  return('Not a balloon payment')  }
  else if (balloon_payment == 1111){ return('Exempt')  }
}

TN2019$Balloon_Payment <- sapply(TN2019$balloon_payment,convert_baloon_payment)
TN2019$Balloon_Payment(TN2019$Balloon_Payment)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -balloon_payment)


