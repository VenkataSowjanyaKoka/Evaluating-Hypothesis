# Koka Venkata Sowjanya
#=========================================================================

# Question 1 : Read the dataset straight from the OpenIntro webpage

###Answer:
## Reading the dataset from webpage
NC_Births <- read.table("https://www.openintro.org/data/csv/ncbirths.csv",sep = ",",header = TRUE)

## Displaying the first three records from the dataset
head(NC_Births,3)
#NC_Births[1:3,]

#Question 2. Create a contingency table for these two variables: habit and premie. Round the probabilities to four
#decimal places. (check: P(W eight = full term ∩ Smoke = nonsmoker ) = 0.7405 and P(W eight =2. Create a contingency table for these two variables: habit and premie. Round the probabilities to four
#decimal places. (check: P(W eight = full term ∩ Smoke = nonsmoker ) = 0.7405 and P(W eight = premie) = 0.1523).

###Answer: 
CC_Tab_Freq_Tot1 <- addmargins(table(Mom_Type = NC_Births$habit, Birth_Outcome = NC_Births$premie))
CC_Tab_Freq_Tot1 #Cross Classification Table of total (frequencies)

                                                                  
CC_Tab_Rel_Freq_Tot1 <- addmargins(prop.table(table(Mom_Type = NC_Births$habit, Birth_Outcome = NC_Births$premie)))
CC_Tab_Rel_Freq_Tot1 # Cross Classification Table of relative frequencies (Probabilities) with totals
round(CC_Tab_Rel_Freq_Tot1,4) # Probabilities rounded to four decimal places

#Question 3. Conduct a test of significance (level of 0.01) to assess the relationship between a woman smoking
#habits during pregnancy and having a premature baby or not.
#• What are the hypotheses? Express using properly constructed hypothesis statement
#• Select the appropriate test
#• Conduct the test

###Answer: 

### I would like to consider the null hypothesis as,
#H0 : The Mom_Type(Smoker/Non-smoker) and birth(premie/full-term) have no relationship(independent)
### I would like to consider the alternative hypothesis as,
#Ha : The Mom_Type(Smoker/Non-smoker) and birth(premie/full-term) have a relationship(dependent)


###Using Chi-Square test of independence because both the variables we are
#considering are categorical variables(habit and premie) 

###Conducting the test

###Using the data from file
chisq.test(x = NC_Births$habit,y = NC_Births$premie, correct = FALSE)

#Question 4. Based on the results from the test in #3, what do statistical evidence suggest about the relationship.
#Provide an explanation of the meaning of the p-value in this case; and by meaning I am not
#referring to the mere slogan: “if it is smaller than the level of significance we reject H0”; or a generic
#interpretation or a story. No more than three sentences.


###Result : Do not Reject null hypothesis
#If P-value is small it means the random variation because of the sampling process alone is not likely to account for the observed difference but,
#the p-value we have for the test is 0.9597 which is greater than the level of significance (0.01), indicating that we do not have enough evidence to reject the null hypothesis
#The chi-squared value observed is 0.00255 and the P-value is sufficiently large we do not reject the null hypothesis we can also say 
#the data do not provide a sufficient evidence that #Mom_Type(Smoker/Non-smoker) has a relationship or would effect the birth of the baby to be (premie /full-term).




#Question 5. Conduct a test of significance (level of 0.01) to assess the difference in weight of babies from women
#who smoked during pregnancy and those who did not.

#• What are the hypotheses? Express using properly constructed hypothesis statement
#• Select the appropriate test
#• Conduct the test

###Answer: 

### I would like to consider the null hypothesis as,
#H0 : There is no significant difference in mean values of weights of babies based on women habit(smoker or non-smoker) during pregnancy
# H0 : (Mu(ns) - Mu(s))Diff = 0
### I would like to consider the alternative hypothesis as,
#Ha : There is significant difference in mean values of weights of babies  based on women habit(smoker or non-smoker) during pregnancy
# H0 : (Mu(ns) - Mu(s))Diff != 0

###Using T test of significance because one variable is
#(habit) is categorical and other variable(weight) is numeric

###Using the data from file

t.test(NC_Births$weight~NC_Births$habit,alternative = "two.side")

#Question 6. Based on the results from the test, what do statistical evidence suggest about the the difference in
#birth weights. Provide an explanation of the meaning of the p-value in this case; and by meaning I
#am not referring to the mere slogan: “if it is smaller than the level of significance we reject H0”; or a
#generic interpretation or a story. No more than three sentences.

###Answer: 
#Do not Reject null hypothesis
#The P-value is the probability of finding a t value 2.359 given H0 is true, which is 0.01945
###The p-value we have for the test is 0.01945 which is greater than the level of significance (0.01) indicating we do not have enough evidence to reject the null hypothesis
#the t-value = 2.359 indicates the evidence is 2.359 standard deviations away (far) from the null hypothesis
#so there is no sufficient evidence to say there is a significant difference in average birth weight of babies from 
#mothers who did not smoke during pregnancy and babies from mothers who did smoke during pregnancy.
