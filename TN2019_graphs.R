library(ggplot2)
library(gridExtra)

p1 <- ggplot(TN2019_init, aes(x=derived_sex)) + ggtitle("Derived Sex") + xlab("Sex") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(TN2019_init, aes(x=Applicant_Sex)) + ggtitle("Applicant Sex") + xlab("Sex") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(TN2019_init, aes(x=Co_Applicant_Sex)) + ggtitle("Co-applicant Sex") + xlab("Sex") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
#p4 <- ggplot(TN2019_init, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  #geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, ncol=2)