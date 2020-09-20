library(ggplot2)
library(gridExtra)

###############    SEX 

s1 <- ggplot(TN2019_init, aes(x=derived_sex)) + ggtitle("Derived Sex") + xlab("Sex") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
s2 <- ggplot(TN2019_init, aes(x=Applicant_Sex)) + ggtitle("Applicant Sex") + xlab("Sex") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
s3 <- ggplot(TN2019_init, aes(x=Co_Applicant_Sex)) + ggtitle("Co-applicant Sex") + xlab("Sex") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
#p4 <- ggplot(TN2019_init, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  #geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(s1, s2, s3, ncol=1)


###################  RACE

r1 <- ggplot(TN2019_init, aes(x=derived_race)) + ggtitle("Derived Race") + xlab("Race") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
r2 <- ggplot(TN2019_init, aes(x=Applicant_Race)) + ggtitle("Applicant Race") + xlab("Race") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
r3 <- ggplot(TN2019_init, aes(x=Co_Applicant_Race)) + ggtitle("Co-applicant Race") + xlab("Race") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
#p4 <- ggplot(TN2019_init, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
#geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(r1, r2, r3, ncol=1)


################## ETHNICITY

e1 <- ggplot(TN2019_init, aes(x=derived_ethnicity)) + ggtitle("Derived Ethnicity") + xlab("Ethnicity") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
e2 <- ggplot(TN2019_init, aes(x=Applicant_Ethnicity)) + ggtitle("Applicant Ethnicity") + xlab("Ethnicity") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
e3 <- ggplot(TN2019_init, aes(x=Co_Applicant_Ethnicity)) + ggtitle("Co-applicant Ethnicity") + xlab("Ethnicity") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
#p4 <- ggplot(TN2019_init, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
#geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(e1, e2, e3, ncol=1)



