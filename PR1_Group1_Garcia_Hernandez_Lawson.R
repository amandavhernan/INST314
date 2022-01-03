# INST314 - Project 1
# Team Members: Vanessa Garcia, Amanda Hernandez, Joe Lawson
# October 24, 2021

# import and view data
mental_health <- read.csv(file.choose(), header=T)
View(mental_health)

# view column names
colnames(mental_health) # names of columns

# installing tidyverse package
install.packages("tidyverse")
library(tidyverse)

# renaming columns
new_mentalhealth = mental_health %>% 
  rename(
    company_size = "How.many.employees.does.your.company.or.organization.have.",
    benefits = "Does.your.employer.provide.mental.health.benefits.as.part.of.healthcare.coverage."
  )
View(new_mentalhealth)

# find unique values in company_size column
unique(new_mentalhealth[c("company_size")])

# original company_size categories -> new categories

# 1-5, 6-25, and 26-100 -> 0-100
# 100-500 -> stays the same
# 500-1000 -> stays the same
# More than 1000 -> 1000+

# load package 
library(plyr)

# recode/combine company size categories
new_mentalhealth$company_size = revalue(new_mentalhealth$company_size, 
                                        c("1-5"="0-100", "6-25"="0-100",
                                          "26-100"="0-100", 
                                          "More than 1000"="1000+"))
View(new_mentalhealth)

# subset dataset, only includes company_size and benefits
mentalhealth_subset <- head(new_mentalhealth[,c("company_size", "benefits")], 417)

# remove empty/missing values
mentalhealth_subset = mentalhealth_subset[!apply(mentalhealth_subset == "", 1, all),]

View(mentalhealth_subset)

## analysis using mentalhealth_subset ##

# independent variable -> company_size | dependent variable -> benefits (mental health benefit offerings)
# summary stats
summarytools::freq(mentalhealth_subset$company_size, order = "freq") # iv
summarytools::freq(mentalhealth_subset$benefits, order = "freq") # dv

# bivariate tables

# load kableExtra package
library(kableExtra)

# frequency table
freq_table <- addmargins(table(mentalhealth_subset$benefits, mentalhealth_subset$company_size))
kbl(freq_table)
freq_table %>%
  kbl(caption = "The Relationship Between Company Size & Mental Health Benefits") %>%
  kable_classic(full_width = F)

# proportions table
prop_table <- round(addmargins(prop.table(table(mentalhealth_subset$benefits, mentalhealth_subset$company_size),2)),3)
kbl(prop_table)
prop_table %>%
  kbl(caption = "The Relationship Between Company Size & Mental Health Benefits") %>%
  kable_classic(full_width = F)

# t-test and effect size test

# t-test: X-squared = 38.846, df = 9, p-value = 1.228e-05 (0.00001228)
chisq.test(mentalhealth_subset$benefits, mentalhealth_subset$company_size)

# alpha = 0.05
0.00001228 < 0.05 # p-value is less than alpha, reject null + results are statistically significant

# effect size test
# cramer’s v = sqrt of x^2/n(k-1)

# creating table for company_size, benefits variables
new_table <- table(mentalhealth_subset$benefits, mentalhealth_subset$company_size)

# n = 361
sum(new_table)
n <- 361

# k = 4 (number of rows or columns, whichever is smaller)
min(nrow(new_table), ncol(new_table))
k <- 4

# effect size calculation, effect size = 0.1893908
sqrt(38.846/(n*(k-1)))

0.1893908 < 0.25 # weak association between variables