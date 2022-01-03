# Amanda Hernandez, INST314 HW4

# Q1b
# entering data
icecream <- matrix(c(100, 120, 60, 350, 200, 90), ncol=3, byrow=T)

# naming rows/columns
rownames(icecream) <- c("Male", "Female")
colnames(icecream) <- c("Strawberry", "Vanilla", "Chocolate")
icecream

# chi-square test
chisq.test(icecream)
sum(icecream)

# Q4b
# entering data
gif <- matrix(c(65, 32, 1, 98, 75, 22, 1, 98, 33, 34, 30, 97, 173, 88, 32, 293), ncol=4, byrow=T)

# naming rows/columns
rownames(gif) <- c("USA", "Europe", "Asia", "Total")
colnames(gif) <- c("Hard 'g'", "Soft 'g'", "All letters", "Total")
gif

# chi-square test
chisq.test(gif)
sum(gif)

#Q5a

library(summarytools)

load(file.choose()) # find & open the 2016 GSS dataset

class(gss$AGE)  # examine how R 'sees' the AGE variable
# 'permanently' convert age from factor to numeric as new variable
gss$age.r = as.numeric(gss$AGE)+17  # add 17, because when converted, age 18 = 1, so add 17 to make 18 = 18, 19=19, etc.
# report summary stats for age by gender
summary(gss$age.r[gss$SEX=="MALE"])
summary(gss$age.r[gss$SEX=="FEMALE"])
sd(gss$age.r[gss$SEX=="MALE"])
sd(gss$age.r[gss$SEX=="FEMALE"])
length(gss$age.r[gss$SEX=="MALE"])-sum(is.na(gss$age.r[gss$SEX=="MALE"])) # n
length(gss$age.r[gss$SEX=="FEMALE"])-sum(is.na(gss$age.r[gss$SEX=="FEMALE"])) # n

# create age groups with each age group = 5 years, plus low and high age caps
gss$age10 = cut(gss$age.r, breaks=c(18,25, #1 18-25
                                    30, #2 26-30
                                    35, #3 31-35
                                    40, #4 36-40 
                                    45, #5 41-45
                                    50, #6 46-50
                                    55, #7 51-55
                                    60, #8 56-60
                                    65, #9 61-65
                                    Inf)) #10 66+
table(gss$AGE, gss$age10)  # confirm recode
summarytools::freq(gss$age10[gss$SEX=="MALE"])  # check summary stats for men
summarytools::freq(gss$age10[gss$SEX=="FEMALE"])  # check summary stats for women
# take a look at the DV: pass good job opportunities for family benefit
# separate the DV by gender
summarytools::freq(gss$FAMORJOB[gss$SEX=="MALE"])  # check summary stats for men
summarytools::freq(gss$FAMORJOB[gss$SEX=="FEMALE"])  # check summary stats for women

# use the following code to create bivariate tables & compute Chi Square for each gender 
tab.job.men = table(gss$FAMORJOB[gss$SEX=="MALE"], gss$age10[gss$SEX=="MALE"])  # store table as an object
tab.job.women = table(gss$FAMORJOB[gss$SEX=="FEMALE"], gss$age10[gss$SEX=="FEMALE"])  #store table as an object
chisq.test(tab.job.men) # Chi-Square for job x age groups for men
sum(table(gss$FAMORJOB[gss$SEX=="MALE"], gss$age10[gss$SEX=="MALE"])) # n
chisq.test(tab.job.women)  # Chi-Square for job x age groups for women
sum(table(gss$FAMORJOB[gss$SEX=="FEMALE"], gss$age10[gss$SEX=="FEMALE"])) # n

#Q5c

summary(gss$age.r)

gss$age10 = cut(gss$age.r, breaks=c(18,34] #1 
                                   (34,49] #2
                                   (49,62] #3
                                  (62,Inf] #4

table(gss$age.r, gss$age4)  # confirm recode