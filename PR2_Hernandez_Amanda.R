# Amanda Hernandez
# INST314 Project 2
# Date of Last Revision: 11/30/21

gss_age_children <- read.csv(file.choose(), header=T) # choose age_children.csv

# removing text responses
age_children <- gss_age_children[!(gss_age_children$Age.of.respondent=="No answer" | 
                                     gss_age_children$Age.of.respondent=="Don't know" | 
                                     gss_age_children$Number.of.children=="Dk na"),]
View(age_children) # check

# load dplyr package
library(dplyr)

# renaming columns
age_children = age_children %>%
  dplyr::rename(num_children = Number.of.children,
         age = Age.of.respondent,
         year = Gss.year.for.this.respondent)

View(age_children) # check

# recoding responses in age + num_children 

# 89 or older -> 89 (age)
# Eight or more -> 8 (num_children)

# load plyr package 
library(plyr)

# recoding age
age_children$age = revalue(age_children$age, 
                          c("89 or older"=89))

# recoding num_children
age_children$num_children = revalue(age_children$num_children, 
                           c("Eight or more"=8))

View(age_children) # check

# drop empty rows using dplyr function
age_children <- age_children %>% slice(-c(64815, 64816)) # run first
age_children <- age_children %>% slice(-c(64403, 64404)) # run again

View(age_children) # check

# convert character columns to numeric (age + num_children)
age_children$age <- as.numeric(age_children$age)
age_children$num_children <- as.numeric(age_children$num_children)

# check
class(age_children$age)
class(age_children$num_children)

# research question: does one's age affect how many children they have?
# alpha = 0.01
# null: β1 = 0
# alt: β1 ≠ 0

# summary stats
# iv = age
# dv = number of children
round(summarytools::descr(age_children$age, na.rm=TRUE), 2)
round(summarytools::descr(age_children$num_children, na.rm=TRUE), 2)

# correlation = 0.3, weak positive correlation
cor.test(age_children$num_children, age_children$age)

# checking assumptions + regression
install.packages("stargazer")
library(stargazer)

age_num_model <- lm(age_children$num_children ~ age_children$age)
stargazer(age_num_model, type = "html", out = "PR2_Hernandez_Amanda.html" , title = "Age & Number of Children")

summary(lm(age_children$num_children ~ age_children$age))

# linear regression equation
# number of children = 0.2620139 + 0.0364136 * age
# ŷ = 0.26 + 0.04 * x (rounded two decimals)

# regression diagnostic plots

par(mfrow=c(2,2))
plot(age_num_model)
par(mfrow=c(1,1))

# assumptions

# 1 - linear in the parameters
# residuals vs. fitted plot shows that the red fitted line is somewhat flat/straight
# flagging for potential issues

# 2 - right variables
# iv = age
# dv = number of children
# both variables are numeric, interval-ratio

# 3 - normally distributed errors
# normal q-q plot shows model is not normally distributed

# 4 - no influential outliers
# residuals vs. leverages plot shows Cook's distance is closer to -1, outliers
# seem to be affecting plot
# advice to future future: focus certain age group, remove outliers

# 5 - homoscedasticity
# residuals vs. fitted plot show a pattern
# scale-location plot shows points are not equally or randomly distributed above 

# 6 - errors are independent
# errors are independent because GSS respondents were asked these questions 
# separately (asked their age and number of children)

# 7 - no multicollinearity
# not necessary to check because dataset only has one independent variable

# checking effect size and power
# effect size = r^2/1-r^2
# 0.02 = small, 0.15 = medium, 0.35 = large

0.1291/(1-0.1291) # 0.1482375, small effect size

# power = 1
library(pwr)

pwr.f2.test(u=1, v=64400, f2=0.1482375, sig.level=0.01, power=NULL)

# analysis: a one-unit change in age corresponds to an average 0.0364136 (0.04 rounded)
# change in number of children OR a 100 unit change in age corresponds 
# to an average 3.64136 change in number of children
# statistically significant, reject the null (not practically significant)
# r-squared: 0.1291 (0.13 rounded), shows predictor variable (age) only explains ~13% of 
# the response variable variability
# power of 1 shows that the probability of making type II error is very low
# small/medium effect size of 0.1482375 (0.15 rounded) indicates limited practical applications

# scatter plot

library(ggplot2)

age_num_plot <- ggplot(age_children, aes(x=age, y=num_children)) +
  geom_point(size=3, shape=18) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, linetype="dashed", color="deepskyblue3")

print(age_num_plot + labs(title= "Linear Regression Fit for Number of Children by Age", y="Number of Children", x = "Age"))
