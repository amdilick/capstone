library(ggplot2)
library(gridExtra)

s1 <- ggplot(TN2019_init, aes(x=derived_sex)) + ggtitle("Derived Sex") + xlab("Sex") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
s2 <- ggplot(TN2019_init, aes(x=Applicant_Sex)) + ggtitle("Applicant Sex") + xlab("Sex") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
s3 <- ggplot(TN2019_init, aes(x=Co_Applicant_Sex)) + ggtitle("Co-applicant Sex") + xlab("Sex") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
#p4 <- ggplot(TN2019_init, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  #geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(s1, s2, s3, ncol=1)

s4 <- ggplot(TN2019_init, aes(x=derived_sex)) + ggtitle("Derived Sex") + xlab("Sex") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
s5 <- ggplot(TN2019_init, aes(x=Applicant_Sex)) + ggtitle("Applicant Sex") + xlab("Sex") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
s6 <- ggplot(TN2019_init, aes(x=Co_Applicant_Sex)) + ggtitle("Co-applicant Sex") + xlab("Sex") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
s7 <- ggplot(TN2019_init, aes(x=Combined_Applicant_Sex)) + ggtitle("Combined Applicant Sex") + xlab("Sex") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(s4, s5, s6, s7, ncol=1)

