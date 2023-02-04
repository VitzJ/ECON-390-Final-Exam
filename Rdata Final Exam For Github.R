# FINAL EXAM ECON 390

library(readxl)
birthweight_smoking <- read_excel("C:/Users/vitzj/Desktop/Econ 390/Final Exam/birthweight_smoking.xlsx")
View(birthweight_smoking)
summary(birthweight_smoking)


nprevist<-birthweight_smoking$nprevist
alcohol<-birthweight_smoking$alcohol
tripre1<-birthweight_smoking$tripre1
tripre2<-birthweight_smoking$tripre2
tripre3<-birthweight_smoking$tripre3
tripre0<-birthweight_smoking$tripre0
birthweight<-birthweight_smoking$birthweight
smoker<-birthweight_smoking$smoker
unmarried<-birthweight_smoking$unmarried
educ<-birthweight_smoking$educ
age<-birthweight_smoking$age
drinks<-birthweight_smoking$drinks


var(nprevist)
var(alcohol)
var(tripre1)
var(tripre2)
var(tripre3)
var(tripre0)
var(birthweight)
var(smoker)
var(unmarried)
var(educ)
var(age)
var(drinks)


#################################################################################################
# Q1.a. + Q1.b.
# 
# I saw that we used variables that we would otherwise not have to define further into the Exam
# so I decided to identify and explain all of them in order to have a better understanding of 
# whatever results of whatever we do with the would-be "unclassified variables" are. 
# 
# I will sort the variables into two categories - Binary/Dummy variables and non-Binary variables
#
#
# Binary / Dummy Variable- A variable with 2 inputs; 1 or 0, that deals with grouping / Yes or no
# questions.
#
#
#------------------------------------------------------------------------------------------------ 
# There are 7 Binary / Dummy Variables in sample data set: 
#------------------------------------------------------------------------------------------------ 
#
# alcohol - 1 if mother drank alcohol during pregnancy, 0 if otherwise
# 
# Min(alcohol) = 0 : No, mother did not drink alcohol during pregnancy
# Max(alcohol) = 1 : Yes, mother drank alcohol during pregnancy
# Mean(alcohol) = 0.01933 Percentage of mothers who drank alcohol during pregnancy in the sample - 1.933%
# Variance(alcohol) = 0.01896588 
#
#
# tripre1 - 1 if 1st prenatal care visit in 1st trimester, 0 if otherwise
# 
# Min(tripre1) = 0 : 1st prenatal care visit not in 1st trimester (No)
# Max(tripre1) = 1 : 1st prenatal care visit in 1st trimester (Yes)
# Mean(tripre1) = 0.804 : Percentage of mothers whose first prenatal care visit in 1st trimester - 80.4%
# Variance(tripre1) = 0.1576365
#
#
# tripre2 - 1 if 1st prenatal care visit in 2nd trimester, 0 if otherwise
# 
# Min(tripre2) = 0 : 1st prenatal care visit not in 2nd trimester (No)
# Max(tripre2) = 1 : 1st prenatal care visit in 2nd trimester (Yes)
# Mean(tripre2) = 0.153 : Percentage of mothers whose first prenatal care visit in 2nd trimester - 15.3%
# Variance(tripre2) =  0.1296342
#
#
# tripre3 - 1 if 1st prenatal care visit in 3rd trimester, 0 if otherwise
# 
# Min(tripre3) = 0 : 1st prenatal care visit not in 3rd trimester (No)
# Max(tripre3) = 1 : 1st prenatal care visit in 3rd trimester (Yes)
# Mean(tripre3) = 0.033 : Percentage of mothers whose first prenatal care visit in 3rd trimester - 3.3%
# Variance(tripre3) = 0.03192164
#
#
# tripre0 - 1 if no prenatal visits, 0 if otherwise
# 
# Min(tripre0) = 0 : Yes prenatal care visits
# Max(tripre0) = 1 : No prenatal care visits at all
# Mean(tripre0) = 0.01 : This is the percentage of mothers who had no pre-natal care visits - 1%
# Variance(tripre0) = 0.009903301
#
#
# smoker - 1 if mother smoked during pregnancy, zero if mother didn't smoke during pregnancy
# 
# Min(smoker) = 0 : No, did not smoke during pregnancy
# Max(smoker) = 1 : Yes, smoked during pregancy
# Mean(smoker) = 0.194 : percentage of individuals who smoked during pregnancy - 19.4%
# Variance(smoker) = 0.1564161
# 
#
# unmarried - 1 if unmarried, 0 if married.
# 
# Min(unmarried) = 0 : No, married
# Max(unmarried) = 1 : Yes, unmarried
# Mean(unmarried) = 0.2267 : percentage of observations in the sample who are unmarried - 22.67%
# Variance(unmarried) = 0.1753473
#
#
#------------------------------------------------------------------------------------------------
# There are 5 Non-Binary / Non-dummy (normal) Variables in sample data set: 
#------------------------------------------------------------------------------------------------ 
#
# nprevist - total number of pre-natal visits for each observation in the sample
# 
# Min(nprevist) = 0 pre-natal visits
# Max(nprevist) = 35 pre-natal visits
# Mean(nprevist) = 10.99 pre-natal visits : average amount of pre-natal visits for all observations in sample
# Variance(nprevist) = 13.48409 pre-natal visits ^ 2
# 
#
# birthweight - birth weight of infant (in grams)
# 
# Min(birthweight) = 425 grams
# Max(birthweight) = 5755 grams
# Mean(birthweight) = 3383 grams : average birthweight of all infants in sample
# Variance(birthweight) = 350656.9 grams ^ 2
# 
#
# educ - years of educational attainment (more than 16 years coded as 17)
# 
# Min(educ) = 0 years of education
# Max(educ) = 17.00 years of education (This number is the default for anybody over 16, not necessarily accurate actual amount of years)
# Mean(educ) = 12.91 years of education : average number of years of education for all observations in sample
# Variance(educ) = 4.694583 years of education ^ 2
# 
#
# age - age in years
# 
# Min(age) = 14.00 years old
# Max(age) = 44.00 years old
# Mean(age) = 26.89 years : average age of all observations in sample
# Variance(age) = 28.75626 years ^ 2
#
#
# drinks - number of drinks per week
# 
# Min(drinks) = 0 drinks per week
# Max(drinks) = 21.00000 drinks per week
# Mean(drinks) = 0.05833 drinks per week : average drinks per week of all observations in sample
# Variance(drinks) = 0.4730883 drinks per week ^ 2
#################################################################################################

#################################################################################################
# Q1.b.(cont.) or Q1.c. (if typo)
# 
# There are 3000 observations of 12 variables. As mentioned above, there are dummy variables 
# and normal variables. There are 7 dummy variables and 5 normal variables. 
#
# The subjects of this census/data set seems to be mothers who were pregnant and then gave birth, 
# so this means the data observations are comprised of individuals. 
#################################################################################################


plot(age,birthweight)
regbirthweightoverage<-lm(birthweight~age)
abline(regbirthweightoverage)
summary(regbirthweightoverage)
8.842/2.010


#################################################################################################
# Q2.a.
# 
# Done.
#
#
# Q2.b.
# 
# Model - 
#
# ^birthweight = B0 + B1(input age) + U
# predicted birthweight = intercept + coefficient of age(input age) + U
#
#
# Predicted Equation -
#
# ^birthweight = 3145.175 + 8.842(input age) + U
#
#
# Q2.c.
#
# The slope is 8.842. It is the coefficient of age, and this means that for any valid input age,
# the final birthweight is affected by + 8.842 multiplied by that input age. 
# It is a positive slope, so an increase of age by 1 unit (1 year) corresponds to an increase in final
# birthweight by + 8.842. 
# 
#
# Q2.d.
# 
# 
# We will do a hypothesis test in order to figure out if the slope is significant. 
# 
# 
#^B1 = 8.842
#
# SE = 2.010
#
# H^o == ^B1^o == 0
# H^o == ^B1^o != 0
#
# 
#     ^B1  - ^B1^o     8.842 - 0        8.842
# z = ------------ = ------------- = ---------- = 4.399005 , z = 4.399005, p(z >= 4.399005) = 0%
#          SE           2.010           2.010
#
#
# p(z >= 4.399005) ~= 0.000 or 0% : The null fails T/Z score test. 
#
# P - Test
#
#
# p(z >= 4.399005) + p(z <= -4.399005) = ?
#
#      0.00        +        0.00       = 0.00
#
# 0.00 < 0.05 , 0.00 > 0.05 is false : The null fails the P-test as well. 
#
#
# Because the null hypothesis ^B1^o == 0 fails the T/Z score test and the P-test, we can reject 
# the idea that 0 could be a valid replacement for the coefficient of age, which means we can
# reject the null hypothesis, and that the coefficient of age in the SLRM is significant.
#################################################################################################


regbirthweightmulti<-lm(birthweight~age+educ+unmarried+smoker+alcohol)
summary(regbirthweightmulti)
(-2.293 - 0) / 2.307
(7.308 - 0) / 5.592
(-244.386 - 0) / 28.525
(-185.202 - 0) / 27.928
(-39.499 - 0) / 77.087


#################################################################################################
# Q3.a.
# 
# Done.
#
#
# Q3.b.
# 
# Model - 
#
# ^birthweight = B0 + B1(input age) + B2(input educ) + B3(input unmarried) + B4(input smoker) + B5(input alcohol) + U
# 
# predicted birthweight = intercept + coefficient of age(input age) + coefficient of educ(input educ) + 
# coefficient of unmarried(input unmarried) +coefficient of smoker(input smoker)+coefficient of alcohol(input alcohol) + U
#
# 
# Q3.c.
# 
# Predicted Equation -
#                intercept    B1                 B2                    B3                          B4                       B5 
# ^birthweight = 3442.335 + -2.293(input age) + 7.308(input educ) + -244.386(input unmarried) + -185.202(input smoker) + -39.499(input alcohol) + U
#
#
# Q3.d.
#
# age, educ, and alcohol all fail the t/z tests. unmarried and smoker both pass the t/z test. I will illustrate this below
#
# For all of the upcoming values, the test is the same: I am going to test the coefficient of each individual variable
# against a null hypothesis of 0.
#
# B(respective variable) == 0
# B(respective variable) != 0
#
#
# z(respective variable) = (^B(respective variable) - ^(respective variable)^o) / SE(respective variable)
#
# z < -1.96 or z > 1.96 is a pass
# 
# -1.96 <= z <= 1.96 is a fail
# 
#
# B1: age: z = (-2.293 - 0) / 2.307, z = -0.9939315, p(z <= -0.9939315) ~= 16.11% fail
#
# B2: educ: z = (7.308 - 0) / 5.592, z = 1.306867, p(z >= 1.306867) ~= 9.51% fail
#
# B3: unmarried: z= (-244.386 - 0) / 28.525, z = -8.567432, p(z <= -8.567432) ~= 0% pass
#
# B4: smoker: z= (-185.202 - 0) / 27.928, z = -6.631409, p(z <= -6.631409) ~= 0% pass
#
# B5: alcohol: z= (-39.499 - 0) / 77.087, z = -0.5123951, p(z <= -0.5123951) ~= 30.5% fail
#
#
# As you can see, the variables that fail the T/Z test are unable to reject the null hypothesis
# that their respective coefficients could be 0 in another sample (with identical methods and 
# different observations) which means that they are insignificant(age, educ, alcohol). 
# 
#
# The variables that succeed in rejecting the null hypothesis are significant (unmarried, smoker).
#################################################################################################


regunreestricted<-regbirthweightmulti
summary(regunreestricted)
regrestricted<-lm(birthweight~age+educ+unmarried)
summary(regrestricted)
0.0575 - 0.04313
1 - 0.0575
3000 - 5 - 1
0.01437/2
0.9425/2994
0.007185/0.00031479625
checkfstat<-anova(regunreestricted,regrestricted,test='F')


#################################################################################################
# Q3.e.
#
# Will do this on paper and upload it. Also will copy what I write on paper onto here. 
#
#
# We will test for simultaneous significance of the smoker and alcohol variables by running an 
# F-test in which we will remove smoker and alcohol variables from the unrestricted equation 
# to make the restricted equation.
#
#
# Unrestricted model: 
# 
# ^course_eval = B0 + B1(age) + B2(educ) + B3(unmarried) + B4(smoker) + B5(alcohol) + U
#
# Restricted model:
#
# ^course_eval = B0 + B1(age) + B2(educ) + B3(unmarried) + U
#
#
# q = number of variables in unrestricted model - number of variables in restricted model
# n = number of observations in sample
# k = number of regressors / independent variables in unrestricted model
#
#
# q = 2
# n = 3000
# k = 5
#
#                                                                                                   
#             |   R^2un - R^2r    |           | 0.0575 - 0.04313 |
#             |  ---------------  |           | ---------------- |
#             |         q         |           |        2         |
# F-stat=  --------------------------- = ---------------------------
#             |     1 - R^2un     |           |    1 - 0.0575    |
#             |  ---------------  |           | ---------------- |
#             |     n - k - 1     |           |   3000 - 5 - 1   |
#
#
#             |     0.01437     |
#             | --------------- |
#             |        2        |          0.007185
# F-stat= --------------------------- = --------------- = 22.82429
#             |      0.9425     |        0.00031479625
#             | --------------- |
#             |       2994      |
#
# 
# F-critical = (n1, n2) ---> n1 = q = 2, n2 = obsv - k - 1 = 2994
#
# on the F-test table, (n1, n2) ---> (2, inf) = 3.00
#
#
# Now we need to figure out which model to use, so we have to find the larger value and us the 
# model that corresponds to that value
#
#
# if : Fstat > Fcritical : reject the null (use unrestricted model)
#
# if : Fstat < Fcritical : accept / fail to reject the null (use restricted model)
#
#
# In our case Fstat = 22.82429 and Fcritical = 3. This means that since (Fstat > Fcritical) = true 
# that we should reject the null hypothesis and that we should use the unrestricted model to 
# evaluate the significance of the variables independently (1 by 1) through T / Zscore testing
##################################################################################################


-2.293*(27)
7.308*(13)
-244.386*(0)
-185.202*(0)
-39.499*(0)
3442.335 + (-61.911) + 95.004 + 0 + 0 + 0


#################################################################################################
# Q3.e. (cont.)
#
# Model from Q3.a.:
#
# ^birthweight = B0 + B1(input age) + B2(input educ) + B3(input unmarried) + B4(input smoker) + B5(input alcohol) + U
#
# ^birthweight = 3442.335 + -2.293(input age) + 7.308(input educ) + -244.386(input unmarried) + -185.202(input smoker) + -39.499(input alcohol) + U
#
#
# If: mother who is 27 years old, who has 13 years of education, who is married, who is not a smoker and does not drink alcohol.
#
# age = 27, ed = 13, unmarried = 0, smoker = 0, alcohol = 0
#
#
# ^birthweight = 3442.335 + -2.293(27) + 7.308(13) + -244.386(0) + -185.202(0) + -39.499(0) + U
#
#
# U is remainder and in this case it doesn't effect the answer because we are evaluating a predicted value based on a hypothetical
# observation not testing an existing observation's inputs and output against our predicted model equation. 
#
#
# In our case ^birthweight = 3442.335 + -2.293*(27) + 7.308*(13) + -244.386*(0) + -185.202*(0) + -39.499*(0)
#
# 3442.335 + (-61.911) + 95.004 + 0 + 0 + 0 = 3475.428
#
# 3475.428 grams is the predicted weight of her infant.
#################################################################################################


bwlog<-log(birthweight)
plot(age,bwlog)
reglog<-lm(bwlog~age)
summary(reglog)
abline(lm(bwlog~age))


#################################################################################################
# Q4.a.
# Log - Linear model was run, graph was produced, line was added, graph will be uploaded alongside assignment.
#
# 
# Q4.b.
#
# Predicted Equation - 
#
# ^bwlog = 8.0352856 + 0.0026650(age) + U
#
# The predicted slope is 0.0026650. This means that for every 1 unit change in age, there is an estimated 0.26650% change 
# in predicted birthweight 
#################################################################################################


plot(nprevist,birthweight)
npsq<-nprevist^2
npcu<-nprevist^3
reglin<-lm(birthweight~nprevist+unmarried+smoker)
regqu<-lm(birthweight~nprevist+npsq+unmarried+smoker)
regcu<-lm(birthweight~nprevist+npsq+npcu+unmarried+smoker)
summary(reglin)


#################################################################################################
# Q5.a. 
#
# Done. Will upload it alongside the assignment. 
#
# 
# Q5.b.
#
# I did this. However I feel that for this problem I have to compare all possible regressions within 
# the scope of the guidelines you offered in order to determine the answer to part c, which is why
# I have 3 regressions listed above rather than 1. 
#
#
# Q5.c.
#
# First I tested the non-log MLRM. All of the variables are significant. 
#
# Predicted Equation - 
#
# ^birthweight = 3133.957 + 29.623(nprevist) + -187.259(unmarried) + -176.212(smoker) + U
#
# 
# I then tested the quadratic MLRM. All of the variables are significant as well.
#
# ^birthweight = 2913.351 + 72.549(nprevist) + -1.913(nprevist^2) + -169.744(unmarried) + -167.676(smoker) + U 
#
#
# Lastly, I tested the cubic regression. One variable is not significant. 
#
# ^birthweight = 2868.51901 + 87.96034(nprevist) + -3.28162(nprevist^2) + 0.03325(nprevist^3) + -168.36746(unmarried) + -167.83047(smoker) + U
#
# ^npcu = ^(nprevist^3)
#
# Test H = ^npcu^o == 0
# Test H = ^npcu^o != 0
#
# ^npcu^o t value = 1.062
#
# p(z>1.062) = 0.14412 = 14.412% Fail to reject
# 
# ^npcu is not significant
#
# 
# The cubic model is not a good model because it includes an insignificant variable and it also 
# weakens the significance of another variable to the point where it can barely reject the null 
# at 95% certainty. 
# 
# This means that we definitely should not use the cubic model in order to describe the relationship
# between birthweight and nprevist. 
#
# Next, testing the non-log MLRM against the quadratic MLRM. I think that since these both contain entirely
# significant variables that we must combine a comparison of their R^2 / Adjusted R^2 and incorporate a bit 
# of creative thinking about their relationship as well. 
#
# Linear non-log MLRM:
#
# R^2 = 0.08858
#
# Adjusted R^2 = 0.08767
#
#
# Quadratic MLRM:
#
# R^2 = 0.09795
#
# Adjusted R^2 = 0.09674
#
#
# In this case, the R^2 and adjusted R^2 is higher for the quadratic MLRM than it is for the linear non-log MLRM.
#
# In addition to this, it intuitively would make sense that there may be diminishing returns on increased pre-natal
# visits after a certain point. In order to evaluate this idea, I will add lines to the plot that don't have anything
# to do with the actual answer to your problem, but have to do with the concept that I am trying to explain. I will
# only evaluate linear non-log vs quadratic lines.
#################################################################################################


abline(lm(birthweight~nprevist))
regexamplesq<-lm(birthweight~nprevist+npsq)
summary(regexamplesq)
curve(2717.1582 + 89.9440*(nprevist) + -2.4074*(nprevist^2),add = TRUE, xname = "nprevist",lty = 1, col = "blue")


#################################################################################################
# Q5.c. (cont.)
#
# The black line is the linear regression, and the green line is the quadratic regression line.
#
# I think that the quadratic equation illustrates the relationship that nprevist has with birthweight 
# in a way that I could use to support my claim. It seems that past a certain point, an increase in nprevist
# fails to have a significant effect on birthweight, which means that nprevist is a variable with diminishing returns past a certain point.
# 
#
# ANSWER:
#------------------------------------------------------------------------------------------------
# For this reason, I believe that in our actual model, that we should go with the quadratic relationship between 
# birthweight and nprevist. nprevist seems to have diminishing returns and when that is factored into the regression equation,
# the R^2 and adjusted R^2 increase because it more accurately depicts the effect that nprevist increases have on birthweight.
# That is, that past a certain point (local maximum?) an increase in nprevist fails to have a significant effect on birthweight.
#------------------------------------------------------------------------------------------------
#
#
# The local maximum (seemingly overall maximum) is actually 18.681; here is the desmos link
# https://www.desmos.com/calculator/2qmve5jlhj
# past that point increases in previst seem to have no significant effect on birthweight
#################################################################################################


#################################################################################################
# END
#################################################################################################