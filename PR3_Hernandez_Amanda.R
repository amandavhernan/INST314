# Amanda Hernandez
# INST314 Project 3
# Date of Last Revision: 12/12/21

# upload data
num_children_analysis <- read.csv(file.choose(), header=T) # choose num_children_analysis.csv

# load dplyr package
library(dplyr)

# rename columns
num_children_analysis = num_children_analysis %>%
  dplyr::rename(year = Gss.year.for.this.respondent, 
                weekly_hrs = Number.of.hours.worked.last.week,
                num_children = Number.of.children,
                age = Age.of.respondent, 
                race = Race.of.respondent,
                fam_income = Total.family.income)

View(num_children_analysis) # check

# removing rows with text responses
num_children_analysis <- num_children_analysis[!(num_children_analysis$weekly_hrs=="No answer" | 
                                     num_children_analysis$weekly_hrs=="Don't know" |
                                     num_children_analysis$weekly_hrs=="Not applicable" |
                                     num_children_analysis$num_children=="Dk na" |
                                     num_children_analysis$age=="No answer" |
                                     num_children_analysis$age=="Don't know" |
                                     num_children_analysis$race=="Not applicable" ),]

View(num_children_analysis) # check

# recoding responses in age + num_children + fam_income

# load plyr package 
library(plyr)

# recoding age
num_children_analysis$age = revalue(num_children_analysis$age, 
                           c("89 or older"=89))

# recoding num_children
num_children_analysis$num_children = revalue(num_children_analysis$num_children, 
                                    c("Eight or more"=8))

# recoding fam_income
num_children_analysis$fam_income = revalue(num_children_analysis$fam_income, 
                                    c("Don't know"="$25000 or more", 
                                      "Refused"="Lt $1000",
                                      "No answer"="Lt $1000"))

View(num_children_analysis) # check

# drop empty rows using dplyr function
num_children_analysis <- num_children_analysis %>% slice(-c(64815, 64816)) # run first
num_children_analysis <- num_children_analysis %>% slice(-c(37305, 37306)) # run second

View(num_children_analysis) # check

# convert variables to numeric (age + num_children + weekly_hrs)
num_children_analysis$age <- as.numeric(num_children_analysis$age)
num_children_analysis$num_children <- as.numeric(num_children_analysis$num_children)
num_children_analysis$weekly_hrs <- as.numeric(num_children_analysis$weekly_hrs)

# convert other variables into factors (race + fam_income)
num_children_analysis$race <- as.factor(num_children_analysis$race)
num_children_analysis$fam_income <- as.factor(num_children_analysis$fam_income)

# check
class(num_children_analysis$age)
class(num_children_analysis$num_children)
class(num_children_analysis$weekly_hrs)
class(num_children_analysis$race)
class(num_children_analysis$fam_income)

# make dummy variables (race + fam_income)
library(car)

str(num_children_analysis$race)
table(num_children_analysis$race)
levels(num_children_analysis$race)

num_children_analysis$race.b <- car::recode(as.numeric(num_children_analysis$race),
                                            "1=1; 2=0; 3=0") # black = 1, else = 0

num_children_analysis$race.o <- car::recode(as.numeric(num_children_analysis$race),
                                            "1=0; 2=1; 3=0") # other = 1, else = 0

num_children_analysis$race.w <- car::recode(as.numeric(num_children_analysis$race),
                                            "1=0; 2=0; 3=1") # white = 1, else = 0

View(num_children_analysis) # check

str(num_children_analysis$fam_income)
table(num_children_analysis$fam_income)
levels(num_children_analysis$fam_income)

# key 
# income.1 = $1000 to 2999
# income.2 = $10000 - 14999
# income.3 = $15000 - 19999
# income.4 = $20000 - 24999
# income.5 = $25000 or more
# income.6 = $3000 to 3999
# income.7 = $4000 to 4999
# income.8 = $5000 to 5999
# income.9 = $6000 to 6999
# income.10 = $7000 to 7999
# income.11 = $8000 to 9999
# income.12 = Lt $1000

num_children_analysis$income.1 <- car::recode(as.numeric(num_children_analysis$fam_income),
                                             "1=1; 2:12=0") # $1000-2999 = 1, else = 0

num_children_analysis$income.2 <- car::recode(as.numeric(num_children_analysis$fam_income),
                                              "1=0; 2=1; 3:12=0") # $10000 - 14999 = 1, else = 0

num_children_analysis$income.3 <- car::recode(as.numeric(num_children_analysis$fam_income),
                                              "1:2=0; 3=1; 4:12=0") # $15000 - 19999 = 1, else = 0

num_children_analysis$income.4 <- car::recode(as.numeric(num_children_analysis$fam_income),
                                              "1:3=0; 4=1; 5:12=0") # $20000 - 24999 = 1, else = 0

num_children_analysis$income.5 <- car::recode(as.numeric(num_children_analysis$fam_income),
                                              "1:4=0; 5=1; 6:12=0") # $25000 or more = 1, else = 0

num_children_analysis$income.6 <- car::recode(as.numeric(num_children_analysis$fam_income),
                                              "1:5=0; 6=1; 7:12=0") # $3000 to 3999 = 1, else = 0

num_children_analysis$income.7 <- car::recode(as.numeric(num_children_analysis$fam_income),
                                              "1:6=0; 7=1; 8:12=0") # $4000 to 4999 = 1, else = 0

num_children_analysis$income.8 <- car::recode(as.numeric(num_children_analysis$fam_income),
                                              "1:7=0; 8=1; 9:12=0") # $5000 to 5999 = 1, else = 0

num_children_analysis$income.9 <- car::recode(as.numeric(num_children_analysis$fam_income),
                                              "1:8=0; 9=1; 10:12=0") # $6000 to 6999 = 1, else = 0

num_children_analysis$income.10 <- car::recode(as.numeric(num_children_analysis$fam_income),
                                              "1:9=0; 10=1; 11:12=0") # $7000 to 7999 = 1, else = 0

num_children_analysis$income.11 <- car::recode(as.numeric(num_children_analysis$fam_income),
                                               "1:10=0; 11=1; 12=0") # $8000 to 9999 = 1, else = 0

num_children_analysis$income.12 <- car::recode(as.numeric(num_children_analysis$fam_income),
                                               "1:11=0; 12=1") # Lt $1000 = 1, else = 0

View(num_children_analysis) # check

# research question: does one's age, race, total family income, and number of hours 
# worked weekly affect how many children they have?
# alpha = 0.01
# null: model coefficients are 0, no relationship between IVs and DV
# alt: model coefficients are not 0, relationship between IVs and DV

# summary stats for quantitative variables
round(summarytools::descr(num_children_analysis$age, na.rm=TRUE), 2)
round(summarytools::descr(num_children_analysis$num_children, na.rm=TRUE), 2)
round(summarytools::descr(num_children_analysis$weekly_hrs, na.rm=TRUE), 2)

# summary stats for categorical variables
summarytools::freq(num_children_analysis$race, order = "freq")
summarytools::freq(num_children_analysis$fam_income, order = "freq")

# regression
# reference groups: race.o and income.12

num_children_model <- lm(formula = num_children ~ age + weekly_hrs + race.b +
                         race.w + income.1 + income.2 + income.3 + income.4 +
                         income.5 + income.6 + income.7 + income.8 + income.9 +
                         income.10 + income.11, data = num_children_analysis)

summary(num_children_model)

library(stargazer)
stargazer(num_children_model, type = "html", out = "PR3_Hernandez_Amanda.html" , title = "Multiple Linear Regression")

# diagnostic plots

par(mfrow=c(2,2))
plot(num_children_model)
par(mfrow=c(1,1))

# multiple regression equation (ŷ)
# num_children = -0.41 + 0.05*age + 0.0*weekly_hrs + 0.15*race.b - 0.27*race.w + 
# 0.13*income.1 + 0.29*income.2 + 0.30 * income.3 + 0.27*income.4 + 0.07*income.5 
# + 0.32*income.6 + 0.36*income.7 + 0.17*income.8 + 0.35*income.9 + 0.27*income.10 + 
# 0.32*income.11

# ** reference groups: race.o and income.12 **

# assumptions

# 1 - linear in the parameters (violation)
# residuals vs. fitted plot shows that the red fitted line is not flat/straight

# 2 - correct variables (no violation)
# iv = age, weekly_hrs, race, fam_income (mix of interval-ratio, categorical)
# dv = number of children (interval-ratio)

# 3 - normally distributed errors (violation)
# normal q-q plot shows model is not normally distributed

# 4 - no influential outliers (violation)
# residuals vs. leverages plot shows points passing beyond the threshold (cook's distance)

# 5 - homoscedasticity (violation)
# residuals vs. fitted plot shows a pattern
# scale-location plot shows points are not equally or randomly distributed above 

# 6 - errors are independent (violation)
# residuals vs. fitted plot shows points do not seem randomly scattered
# there appears to be relationship between residuals and weight
# durbin watson test also shows residuals are autocorrelated

durbinWatsonTest(num_children_model) # p-value = 0, alpha = 0.01 > 0 

# 7 - no multicollinearity (slight violation, ok to proceed)
# most vif scores are between 1-4, so there's no multicollinearity for those variables
# vif score for income.5 is over 4

vif(num_children_model)

# effect size + power

# effect size = r^2/1-r^2
# 0.02 = small, 0.15 = medium, 0.35 = large

0.182/(1-0.182) # 0.2224939, medium effect size

# power = 1
library(pwr)

pwr.f2.test(u=4, v=37288, f2=0.2224939, sig.level=0.01, power=NULL)

# analysis

# R2 = 0.18, F(15, 37288) = 553.3, p < 0.01
# model is statistically significant, reject the null
# approximately 18% (r-sq = 0.182) of variation in number of children can be explained by the model
# (age, race, weekly_hrs, fam_income)
# # power of 1 shows that the probability of making type II error is very low
# medium effect size of 0.2224939 (0.22 rounded) indicates limited practical applications
# still not a strong enough effect size to have significant real-world applications

# independent variable interpretations

# age 

# a one-unit change in age corresponds to a 0.05 average increase in number of
# children holding all other variables in the model constant

# weekly_hrs

# a one-unit change in number of hours worked last week corresponds to a zero 
# average change in number of children holding all other variables in the model constant

# race

# black - those who identify as black correspond to an average 0.15 rank increase in number of children
# compared to those who identify as other, holding all other variables in the model constant

# white - those who identify as white correspond to an average 0.27 rank decrease in number of children
# compared to those who identify as other, holding all other variables in the model constant

# fam_income

# key 
# income.1 = $1000 to 2999
# income.2 = $10000 - 14999
# income.3 = $15000 - 19999
# income.4 = $20000 - 24999
# income.5 = $25000 or more
# income.6 = $3000 to 3999
# income.7 = $4000 to 4999
# income.8 = $5000 to 5999
# income.9 = $6000 to 6999
# income.10 = $7000 to 7999
# income.11 = $8000 to 9999
# income.12 = Lt $1000

# income.1 - those who fall under the $1000-$2999 income bracket correspond to an
# average 0.13 rank increase in number of children compared to those who fall under 
# the less than $1000 income bracket, holding all other variables in the model constant

# income.2 - those who fall under the $10000-$14999 income bracket correspond to an
# average 0.29 rank increase in number of children compared to those who fall under 
# the less than $1000 income bracket, holding all other variables in the model constant

# income.3 - those who fall under the $15000-$19999 income bracket correspond to an
# average 0.30 rank increase in number of children compared to those who fall under 
# the less than $1000 income bracket, holding all other variables in the model constant

# income.4 - those who fall under the $20000-$24999 income bracket correspond to an
# average 0.27 rank increase in number of children compared to those who fall under 
# the less than $1000 income bracket, holding all other variables in the model constant

# income.5 - those who fall under the $25000 or more income bracket correspond to an
# average 0.07 rank increase in number of children compared to those who fall under 
# the less than $1000 income bracket, holding all other variables in the model constant

# income.6 - those who fall under the $3000-$3999 income bracket correspond to an
# average 0.32 rank increase in number of children compared to those who fall under 
# the less than $1000 income bracket, holding all other variables in the model constant

# income.7 - those who fall under the $4000-$4999 income bracket correspond to an
# average 0.36 rank increase in number of children compared to those who fall under 
# the less than $1000 income bracket, holding all other variables in the model constant

# income.8 - those who fall under the $5000-$5999 income bracket correspond to an
# average 0.17 rank increase in number of children compared to those who fall under 
# the less than $1000 income bracket, holding all other variables in the model constant

# income.9 - those who fall under the $6000-$6999 income bracket correspond to an
# average 0.35 rank increase in number of children compared to those who fall under 
# the less than $1000 income bracket, holding all other variables in the model constant

# income.10 - those who fall under the $7000-$7999 income bracket correspond to an
# average 0.27 rank increase in number of children compared to those who fall under 
# the less than $1000 income bracket, holding all other variables in the model constant

# income.11 - those who fall under the $8000-$9999 income bracket correspond to an
# average 0.32 rank increase in number of children compared to those who fall under 
# the less than $1000 income bracket, holding all other variables in the model constant

# extra credit plot

library(dotwhisker)
library(ggplot2)

dwplot(num_children_model) %>%
  relabel_predictors(age = "Age",
                     weekly_hrs = "Weekly Hours",
                     race.b = "Race (Black)",
                     race.w = "Race (White)",
                     income.1 = "$1000 - $2999",
                     income.6 = "$3000 - $3999",
                     income.7 = "$4000 - $4999",
                     income.8 = "$5000 - $5999",
                     income.9 = "$6000 - $6999",
                     income.10 = "$7000 - $7999",
                     income.11 = "$8000 - $9999",
                     income.2 = "$10000 - $14999", 
                     income.3 = "$15000 - $19999",
                     income.4 = "$20000 - $24999",
                     income.5 = "$25000 or more") +
  theme_bw() + 
  xlab("Coefficient Estimate") + ylab("Predictor Variables") +
  geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  ggtitle("Predicting Number of Children") +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position="none")
