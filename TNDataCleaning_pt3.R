# TN Data Cleaning Part 3
# Beginning info: row count = 226911, = 68

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
# convert co_applicant_sex levels from numbers to descriptives
convert_co_applicant_sex <- function(co_applicant_sex){
  if (co_applicant_sex == 1){  return('Male')  }
  else if (co_applicant_sex == 2){  return('Female')  }
  else if (co_applicant_sex == 3){  return('Information not provided by applicant')  }
  else if (co_applicant_sex == 4){  return('Not applicable')  }
  else if (co_applicant_sex == 5){  return('No co-applicant')  }
  else if (co_applicant_sex == 6){  return('Applicant selected both male and female')  }
}
TN2019$Co_Applicant_Sex <- sapply(TN2019$co_applicant_sex,convert_co_applicant_sex)
TN2019$Co_Applicant_Sex <- as.factor(TN2019$Co_Applicant_Sex)
# drop co_applicant_sex (lower case) column 
TN2019 <- subset(TN2019, select= -co_applicant_sex)

##########################################################################
# convert applicant_sex_observed levels from numbers to descriptives
convert_applicant_sex_observed <- function(applicant_sex_observed){
  if (applicant_sex_observed == 1){  return('Collected on the basis of visual observation or surname')  }
  else if (applicant_sex_observed == 2){  return('Not collected on the basis of visual observation or surname')  }
  else if (applicant_sex_observed == 3){  return('Not applicable')  }
}
TN2019$Applicant_Sex_Observed <- sapply(TN2019$applicant_sex_observed,convert_applicant_sex_observed)
TN2019$Applicant_Sex_Observed <- as.factor(TN2019$Applicant_Sex_Observed)
# drop applicant_sex_observed (lower case) column 
TN2019 <- subset(TN2019, select= -applicant_sex_observed)

##########################################################################
# convert co_applicant_sex_observed levels from numbers to descriptives
convert_co_applicant_sex_observed <- function(co_applicant_sex_observed){
  if (co_applicant_sex_observed == 1){  return('Collected on the basis of visual observation or surname')  }
  else if (co_applicant_sex_observed == 2){  return('Not collected on the basis of visual observation or surname')  }
  else if (co_applicant_sex_observed == 3){  return('Not applicable')  }
  else if (co_applicant_sex_observed == 4){  return('No co-applicant')  }
}
TN2019$Co_Applicant_Sex_Observed <- sapply(TN2019$co_applicant_sex_observed,convert_co_applicant_sex_observed)
TN2019$Co_Applicant_Sex_Observed <- as.factor(TN2019$Co_Applicant_Sex_Observed)
# drop co_applicant_sex_observed (lower case) column 
TN2019 <- subset(TN2019, select= -co_applicant_sex_observed)

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
# convert co_applicant_ethnicity_1 levels from numbers to descriptives
convert_co_applicant_ethnicity_1<- function(co_applicant_ethnicity_1){
  if (co_applicant_ethnicity_1 == 1){  return('Hispanic or Latino')  }
  else if (co_applicant_ethnicity_1 == 11){  return('Mexican')  }
  else if (co_applicant_ethnicity_1 == 12){  return('Puerto Rican')  }
  else if (co_applicant_ethnicity_1 == 13){  return('Cuban')  }
  else if (co_applicant_ethnicity_1 == 14){  return('Other Hispanic or Latino')  }
  else if (co_applicant_ethnicity_1 == 2){  return('Not Hispanic or Latino')  }
  else if (co_applicant_ethnicity_1 == 3){  return('Information not provided by applicant in mail, internet, or telephone application')  }
  else if (co_applicant_ethnicity_1 == 4){  return('Not applicable')  }
  else if (co_applicant_ethnicity_1 == 5){  return('No co-applicant')  }
}
TN2019$Co_Applicant_Ethnicity_1 <- sapply(TN2019$co_applicant_ethnicity_1,convert_co_applicant_ethnicity_1)
TN2019$Co_Applicant_Ethnicity_1 <- as.factor(TN2019$Co_Applicant_Ethnicity_1)
# drop co_applicant_ethnicity_1 (lower case) column 
TN2019 <- subset(TN2019, select= -co_applicant_ethnicity_1)

##########################################################################
# convert applicant_ethnicity_observed levels from numbers to descriptives
convert_applicant_ethnicity_observed <- function(applicant_ethnicity_observed){
  if (applicant_ethnicity_observed == 1){  return('Collected on the basis of visual observation or surname')  }
  else if (applicant_ethnicity_observed == 2){  return('Not collected on the basis of visual observation or surname')  }
  else if (applicant_ethnicity_observed == 3){  return('Not applicable')  }
}
TN2019$Applicant_Ethnicity_Observed <- sapply(TN2019$applicant_ethnicity_observed,convert_applicant_ethnicity_observed)
TN2019$Applicant_Ethnicity_Observed <- as.factor(TN2019$Applicant_Ethnicity_Observed)
# drop applicant_ethnicity_observed (lower case) column 
TN2019 <- subset(TN2019, select= -applicant_ethnicity_observed)

##########################################################################
# convert co_applicant_ethnicity_observed levels from numbers to descriptives
convert_co_applicant_ethnicity_observed <- function(co_applicant_ethnicity_observed){
  if (co_applicant_ethnicity_observed == 1){  return('Collected on the basis of visual observation or surname')  }
  else if (co_applicant_ethnicity_observed == 2){  return('Not collected on the basis of visual observation or surname')  }
  else if (co_applicant_ethnicity_observed == 3){  return('Not applicable')  }
  else if (co_applicant_ethnicity_observed == 4){  return('No co-applicant')  }
}
TN2019$Co_Applicant_Ethnicity_Observed <- sapply(TN2019$co_applicant_ethnicity_observed,convert_co_applicant_ethnicity_observed)
TN2019$Co_Applicant_Ethnicity_Observed <- as.factor(TN2019$Co_Applicant_Ethnicity_Observed)
# drop co_applicant_ethnicity_observed (lower case) column 
TN2019 <- subset(TN2019, select= -co_applicant_ethnicity_observed)

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

##########################################################################
# convert co_applicant_race_1 levels from numbers to descriptives
convert_co_applicant_race_1<- function(co_applicant_race_1){
  if (co_applicant_race_1 == 1){  return('American Indian or Alaska Native')  }
  else if (co_applicant_race_1 == 2){  return('Asian')  }
  else if (co_applicant_race_1 == 21){  return('Asian Indian')  }
  else if (co_applicant_race_1 == 22){  return('Chinese')  }
  else if (co_applicant_race_1 == 23){  return('Filipino')  }
  else if (co_applicant_race_1 == 24){  return('Japanese')  }
  else if (co_applicant_race_1 == 25){  return('Korean')  }
  else if (co_applicant_race_1 == 26){  return('Vietnamese')  }
  else if (co_applicant_race_1 == 27){  return('Other Asian')  }
  else if (co_applicant_race_1 == 3){  return('Black or African American')  }
  else if (co_applicant_race_1 == 4){  return('Native Hawaiian or Other Pacific Islander')  }
  else if (co_applicant_race_1 == 41){  return('Native Hawaiian')  }
  else if (co_applicant_race_1 == 42){  return('Guamanian or Chamorro')  }
  else if (co_applicant_race_1 == 43){  return('Samoan')  }
  else if (co_applicant_race_1 == 44){  return('Other Pacific Islander')  }
  else if (co_applicant_race_1 == 5){  return('White')  }
  else if (co_applicant_race_1 == 6){  return('Information not provided by applicant in mail, internet, or telephone application')  }
  else if (co_applicant_race_1 == 7){  return('Not applicable')  }
  else if (co_applicant_race_1 == 8){  return('No co-applicant')  }
}
TN2019$Co_Applicant_Race_1 <- sapply(TN2019$co_applicant_race_1,convert_co_applicant_race_1)
TN2019$Co_Applicant_Race_1 <- as.factor(TN2019$Co_Applicant_Race_1)
# drop co_applicant_race_1 (lower case) column 
TN2019 <- subset(TN2019, select= -co_applicant_race_1)

##########################################################################
# convert applicant_race_observed levels from numbers to descriptives
convert_applicant_race_observed <- function(applicant_race_observed){
  if (applicant_race_observed == 1){  return('Collected on the basis of visual observation or surname')  }
  else if (applicant_race_observed == 2){  return('Not collected on the basis of visual observation or surname')  }
  else if (applicant_race_observed == 3){  return('Not applicable')  }
}
TN2019$Applicant_Race_Observed <- sapply(TN2019$applicant_race_observed,convert_applicant_race_observed)
TN2019$Applicant_Race_Observed <- as.factor(TN2019$Applicant_Race_Observed)
# drop applicant_race_observed (lower case) column 
TN2019 <- subset(TN2019, select= -applicant_race_observed)

##########################################################################
# convert co_applicant_race_observed levels from numbers to descriptives
convert_co_applicant_race_observed <- function(co_applicant_race_observed){
  if (co_applicant_race_observed == 1){  return('Collected on the basis of visual observation or surname')  }
  else if (co_applicant_race_observed == 2){  return('Not collected on the basis of visual observation or surname')  }
  else if (co_applicant_race_observed == 3){  return('Not applicable')  }
  else if (co_applicant_race_observed == 4){  return('No co-applicant')  }
}
TN2019$Co_Applicant_Race_Observed <- sapply(TN2019$co_applicant_race_observed,convert_co_applicant_race_observed)
TN2019$Co_Applicant_Race_Observed <- as.factor(TN2019$Co_Applicant_Race_Observed)
# drop co_applicant_race_observed (lower case) column 
TN2019 <- subset(TN2019, select= -co_applicant_race_observed)

######################################################
# check the 'observed' columns for information 

TN2019_Gender <- subset(TN2019, select =c('Application_Status', 
       'derived_sex', 'Applicant_Sex', 'Applicant_Sex_Observed', 
       'Co_Applicant_Sex','Co_Applicant_Sex_Observed'))
TN2019_Race <- subset(TN2019, select = c('Application_Status', 
       'derived_race', 'Applicant_Race_1', 'Applicant_Race_Observed', 
       'Co_Applicant_Race_1', 'Co_Applicant_Race_Observed'))
TN2019_Ethnicity<- subset(TN2019, select =c('Application_Status', 
       'derived_ethnicity', 'Applicant_Ethnicity_1', 
       'Applicant_Ethnicity_Observed', 'Co_Applicant_Ethnicity_1',
       'Co_Applicant_Ethnicity_Observed'))

# remove 'observed' columns as unneeded information
TN2019 <- subset(TN2019, select=-Applicant_Sex_Observed)
TN2019 <- subset(TN2019, select=-Applicant_Race_Observed)
TN2019 <- subset(TN2019, select=-Applicant_Ethnicity_Observed)

# Beginning info: row count = 226911, col count = 65


