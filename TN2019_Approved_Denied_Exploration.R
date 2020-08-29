## separate Exploration of Approved and Denied subsets

# create subsets of split data Approved / Denied for possible additional analysis
TN2019_denied <- subset(TN2019, Application_Status == 'denied')
TN2019_approved <- subset(TN2019, Application_Status == 'approved')


describe(TN2019_approved$Applicant_Sex)
describe(TN2019_denied$Applicant_Sex)

# bar graphs of sex
sex_denied <- ggplot(TN2019_denied, aes(x=Applicant_Sex)) + ggtitle("Denied by Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
sex_approved <- ggplot(TN2019_approved, aes(x=Applicant_Sex)) + ggtitle("Approved by Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
print(sex_denied)
print(sex_approved)


describe(TN2019_approved$Applicant_Race_1)
describe(TN2019_denied$Applicant_Race_1)
# bar graphs of race
race_denied <- ggplot(TN2019_denied, aes(x=Applicant_Race_1)) + ggtitle("Denied by Race") + xlab("Race") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
race_approved <- ggplot(TN2019_approved, aes(x=Applicant_Race_1)) + ggtitle("Approved by Race") + xlab("Race") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
print(race_denied)
print(race_approved)


describe(TN2019_approved$Applicant_Ethnicity_1)
describe(TN2019_denied$Applicant_Ethnicity_1)
# bar graphs of ethnicity



# bar graphs of age




