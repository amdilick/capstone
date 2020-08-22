# subset and examine applicant and co-applicant gender
TN2019_gender <- subset(X2019publicTN_allColumns, select=c('derived_sex', 'applicant_sex', 'co_applicant_sex', 'applicant_sex_observed', 'co_applicant_sex_observed'))

##########################################################################

# convert applicant_sex levels from numbers to descriptives
convert_applicant_sex <- function(applicant_sex){
  if (applicant_sex == 1){  return('Male')  }
  else if (applicant_sex == 2){  return('Female')  }
  else if (applicant_sex == 3){ return('Information not provided by applicant')  }
  else if (applicant_sex == 4){  return('Not applicable')  }
  else if (applicant_sex == 6){  return('Applicant selected both male and female')  }
}

TN2019_gender$Applicant_Sex <- sapply(TN2019_gender$applicant_sex,convert_applicant_sex)
TN2019_gender$Applicant_Sex <- as.factor(TN2019_gender$Applicant_Sex)
# drop action_taken (lower case) column 
TN2019_gender <- subset(TN2019_gender, select= -applicant_sex)

##########################################################################

# convert co_applicant_sex levels from numbers to descriptives
convert_co_applicant_sex <- function(co_applicant_sex){
  if (co_applicant_sex == 1){  return('Male')  }
  else if (co_applicant_sex == 2){  return('Female')  }
  else if (co_applicant_sex == 3){ return('Information not provided by applicant')  }
  else if (co_applicant_sex == 4){  return('Not applicable')  }
  else if (co_applicant_sex == 5){  return('No co-applicant')  }
  else if (co_applicant_sex == 6){  return('Applicant selected both male and female')  }
}

TN2019_gender$Co_Applicant_Sex <- sapply(TN2019_gender$co_applicant_sex,convert_co_applicant_sex)
TN2019_gender$Co_Applicant_Sex <- as.factor(TN2019_gender$Co_Applicant_Sex)
# drop action_taken (lower case) column 
TN2019_gender <- subset(TN2019_gender, select= -co_applicant_sex)

##########################################################################

# convert applicant_sex_observed levels from numbers to descriptives
convert_applicant_sex_observed <- function(applicant_sex_observed){
  if (applicant_sex_observed == 1){  return('Collected on the basis of visual observation or surname')  }
  else if (applicant_sex_observed == 2){  return('Not collected on the basis of visual observation or surname')  }
  else if (applicant_sex_observed == 3){  return('Not applicable')  }
}

TN2019_gender$Applicant_Sex_Observed <- sapply(TN2019_gender$applicant_sex_observed,convert_applicant_sex)
TN2019_gender$Applicant_Sex_Observed <- as.factor(TN2019_gender$Applicant_Sex_Observed)
# drop action_taken (lower case) column 
TN2019_gender <- subset(TN2019_gender, select= -applicant_sex_observed)

##########################################################################

# convert co_applicant_sex_observed levels from numbers to descriptives
convert_co_applicant_sex_observed <- function(co_applicant_sex_observed){
  if (co_applicant_sex_observed == 1){  return('Collected on the basis of visual observation or surname')  }
  else if (co_applicant_sex_observed == 2){  return('Not collected on the basis of visual observation or surname')  }
  else if (co_applicant_sex_observed == 3){  return('Not applicable')  }
  else if (co_applicant_sex_observed == 4){  return('No co-applicant')  }
}

TN2019_gender$Co_Applicant_Sex_Observed <- sapply(TN2019_gender$co_applicant_sex_observed,convert_co_applicant_sex)
TN2019_gender$Co_Applicant_Sex_Observed <- as.factor(TN2019_gender$Co_Applicant_Sex_Observed)
# drop action_taken (lower case) column 
TN2019_gender <- subset(TN2019_gender, select= -co_applicant_sex_observed)
