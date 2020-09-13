# convert applicant_sex and co_applicant_sex to new column 
convert_applicans_sex_conv <- function(applicants_sex_conv){
  if (Applicant_Sex == 'Male' & Co_Applicant_Sex == 'Male'){  return('Male-Male')  }
  else if (Applicant_Sex == 'Male' & Co_Applicant_Sex == 'Female'){  return('Male-Female')  }
  else if (Applicant_Sex == 'Male' & Co_Applicant_Sex == 'Co-applicant selected both male and female'){  return('Male-Both')  }
  else if (Applicant_Sex == 'Male' & Co_Applicant_Sex == 'No co-applicant'){  return('Male-None')  }
  else if (Applicant_Sex == 'Female' & Co_Applicant_Sex == 'Male'){  return('Female-Male')  }
  else if (Applicant_Sex == 'Female' & Co_Applicant_Sex == 'Co-applicant selected both male and female'){  return('Female-Both')  }
  else if (Applicant_Sex == 'Female' & Co_Applicant_Sex == 'No co-applicant'){  return('Female-None')  }
  else if (Applicant_Sex == 'Female' & Co_Applicant_Sex == 'Female'){  return('Female-Female')  }
  else if (Applicant_Sex == 'Co-applicant selected both male and female' & Co_Applicant_Sex == 'Co-applicant selected both male and female'){  return('Both-Both')  }
  else if (Applicant_Sex == 'Co-applicant selected both male and female' & Co_Applicant_Sex == 'Male'){  return('Both-Male')  }
  else if (Applicant_Sex == 'Co-applicant selected both male and female' & Co_Applicant_Sex == 'Female'){  return('Both-Female')  }
  else if (Applicant_Sex == 'Co-applicant selected both male and female' & Co_Applicant_Sex == 'No co-applicant'){  return('Both-None')  }
}
TN2019_init$Applicant_Sex_Conv <- sapply(TN2019_init$Applicant_Sex, TN2019_init$Co_Applicant_Sex,convert_applicant_sex_conv)
TN2019_init$Applicant_Sex_Conv <- as.factor(TN2019_init$Applicant_Sex_Conv)
