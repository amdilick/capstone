# TN Data Cleaning Part 3
# Beginning info: row count = 2212541, = 47

####################################################################
# convert levels with numeric representation to descriptives
####################################################################
# convert applicant_sex levels from numbers to descriptives
convert_applicant_sex <- function(applicant_sex){
  if (applicant_sex == 1){  return('Male')  }
  else if (applicant_sex == 2){  return('Female')  }
  else if (applicant_sex == 3){  return('Information not provided by applicant')  }
  else if (applicant_sex == 4){  return('Not applicable')  }
  else if (applicant_sex == 6){  return('Applicant selected both male and female')  }
}
TN2019$Applicant_Sex <- sapply(TN2019$applicant_sex,convert_applicant_sex)
TN2019$Applicant_Sex <- as.factor(TN2019$Applicant_Sex)
# drop applicant_sex (lower case) column 
TN2019 <- subset(TN2019, select= -applicant_sex)

##########################################################################
# convert applicant_ethnicity_1 levels from numbers to descriptives
convert_applicant_ethnicity_1<- function(applicant_ethnicity_1){
  if (applicant_ethnicity_1 == 1){  return('Hispanic or Latino')  }
  else if (applicant_ethnicity_1 == 11){  return('Mexican')  }
  else if (applicant_ethnicity_1 == 12){  return('Puerto Rican')  }
  else if (applicant_ethnicity_1 == 13){  return('Cuban')  }
  else if (applicant_ethnicity_1 == 14){  return('Other Hispanic or Latino')  }
  else if (applicant_ethnicity_1 == 2){  return('Not Hispanic or Latino')  }
  else if (applicant_ethnicity_1 == 3){  return('Information not provided by applicant in mail, internet, or telephone application')  }
  else if (applicant_ethnicity_1 == 4){  return('Not applicable')  }
}
TN2019$Applicant_Ethnicity_1 <- sapply(TN2019$applicant_ethnicity_1,convert_applicant_ethnicity_1)
TN2019$Applicant_Ethnicity_1 <- as.factor(TN2019$Applicant_Ethnicity_1)
# drop applicant_ethnicity_1 (lower case) column 
TN2019 <- subset(TN2019, select= -applicant_ethnicity_1)


##########################################################################
# convert applicant_race_1 levels from numbers to descriptives
convert_applicant_race_1<- function(applicant_race_1){
  if (applicant_race_1 == 1){  return('American Indian or Alaska Native')  }
  else if (applicant_race_1 == 2){  return('Asian')  }
  else if (applicant_race_1 == 21){  return('Asian Indian')  }
  else if (applicant_race_1 == 22){  return('Chinese')  }
  else if (applicant_race_1 == 23){  return('Filipino')  }
  else if (applicant_race_1 == 24){  return('Japanese')  }
  else if (applicant_race_1 == 25){  return('Korean')  }
  else if (applicant_race_1 == 26){  return('Vietnamese')  }
  else if (applicant_race_1 == 27){  return('Other Asian')  }
  else if (applicant_race_1 == 3){  return('Black or African American')  }
  else if (applicant_race_1 == 4){  return('Native Hawaiian or Other Pacific Islander')  }
  else if (applicant_race_1 == 41){  return('Native Hawaiian')  }
  else if (applicant_race_1 == 42){  return('Guamanian or Chamorro')  }
  else if (applicant_race_1 == 43){  return('Samoan')  }
  else if (applicant_race_1 == 44){  return('Other Pacific Islander')  }
  else if (applicant_race_1 == 5){  return('White')  }
  else if (applicant_race_1 == 6){  return('Information not provided by applicant in mail, internet, or telephone application')  }
  else if (applicant_race_1 == 7){  return('Not applicable')  }
}
TN2019$Applicant_Race_1 <- sapply(TN2019$applicant_race_1,convert_applicant_race_1)
TN2019$Applicant_Race_1 <- as.factor(TN2019$Applicant_Race_1)
# drop applicant_race_1 (lower case) column 
TN2019 <- subset(TN2019, select= -applicant_race_1)



# Beginning info: row count = 212541, col count = 47

