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


















