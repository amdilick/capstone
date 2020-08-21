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


# convert preapproval levels from numbers to descriptives
convert_preapproval<- function(preapproval){
  if (preapproval== 1){  return('Preapproval requested')  }
  else if (preapproval  == 2){  return('Preapproval not requested')  }
}

TN2019$Preapproval <- sapply(TN2019$preapproval,convert_preapproval)
TN2019$Preapproval <- as.factor(TN2019$Preapproval)
# drop action_taken (lower case) column 
TN2019 <- subset(TN2019, select= -preapproval)


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
