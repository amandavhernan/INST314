# Amanda Hernandez, INST314 - HW5

# Q2

fifa <- read.csv(file.choose(), header=T) # load fifa data

# construct composite mean scale
fifa$kicking_ability.mean <- (fifa$Shot_Power + fifa$Finishing + fifa$Long_Shots + fifa$Curve + fifa$Freekick_Accuracy + fifa$Short_Pass + fifa$Long_Pass) / 7
fifa #check

# creating subset for FC Barcelona
fifa_barcelona <- subset(fifa, fifa$Club == "FC Barcelona")

# creating subset for Longford Town
fifa_longford <- subset(fifa, fifa$Club == "Longford Town")

# Q2a

library(summarytools)
library(kableExtra)

# summary statistics + pq table for all players

round(summarytools::descr(fifa$kicking_ability.mean), 2) # all players, n = 17,588
# results rounded two decimals

stats_allplayers <- data.frame(
  col1 = c("N", "Min", "Max", "Q1", "Q3", "Median", "Mean", "Standard Deviation"), 
  col2 = c(17588, 10.57, 88.86, 41.43, 61.29, 52.57, 49.89, 15.65)
)

colnames(stats_allplayers) <- c('Summary Statistics','Results') # renaming columns

kbl(stats_allplayers)
stats_allplayers %>%
  kbl(caption = "Summary Statistics for Average Kicking Ability of All Players") %>%
  kable_minimal("hover", full_width = F)

# summary statistics + pq table for FC Barcelona

round(summarytools::descr(fifa_barcelona$kicking_ability.mean), 2) # FC Barcelona, n = 33
# results rounded two decimals

stats_barcelona <- data.frame(
  col1 = c("N", "Min", "Max", "Q1", "Q3", "Median", "Mean", "Standard Deviation"), 
  col2 = c(33, 19.14, 88.86, 53.67, 75.86, 67.43, 60.86, 19.79)
)

colnames(stats_barcelona) <- c('Summary Statistics','Results') # renaming columns

kbl(stats_barcelona)
stats_barcelona %>%
  kbl(caption = "Summary Statistics for Average Kicking Ability of FC Barcelona Club") %>%
  kable_minimal("hover", full_width = F)

# summary statistics + pq table for Longboard Town

round(summarytools::descr(fifa_longford$kicking_ability.mean), 2) # Longford Town, n = 25
# results rounded two decimals

stats_longford <- data.frame(
  col1 = c("N", "Min", "Max", "Q1", "Q3", "Median", "Mean", "Standard Deviation"), 
  col2 = c(25, 16.86, 56.43, 27.71, 45.29, 40.71, 37.71, 11.71)
)

colnames(stats_longford) <- c('Summary Statistics','Results') # renaming columns

kbl(stats_longford)
stats_longford %>%
  kbl(caption = "Summary Statistics for Average Kicking Ability of Longford Town Club") %>%
  kable_minimal("hover", full_width = F)

# Q2b

# checking confidence intervals

t.test(fifa$kicking_ability.mean)
# 95 percent confidence interval: 49.65762 50.12021

t.test(fifa_barcelona$kicking_ability.mean)
# 95 percent confidence interval: 53.84434 67.87860

t.test(fifa_longford$kicking_ability.mean)
# 95 percent confidence interval:32.87401 42.54314

# Q3

# Q3b

# create item scale

attach(gss)

# Do you agree or disagree that...
# 1) In uncertain times, I usually expect the best.
LOTR1.r <- as.numeric(LOTR1)  
table(LOTR1, LOTR1.r)         
# 2) If something can go wrong for me, it will. 
LOTR2.r <- as.numeric(LOTR2)  
LOTR2.r <- 6 - LOTR2.r        
table(LOTR2, LOTR2.r)         
# 3) I'm always optimistic about my future.
LOTR3.r <- as.numeric(LOTR3)  
table(LOTR3, LOTR3.r)         
# 4) I hardly ever expect things to go my way.  
LOTR4.r <- as.numeric(LOTR4)  
LOTR4.r <- 6 - LOTR4.r        
table(LOTR4, LOTR4.r)         
# 5) I rarely count on good things happening to me. 
LOTR5.r <- as.numeric(LOTR5)  
LOTR5.r <- 6 - LOTR5.r        
table(LOTR5, LOTR5.r)         
# 6) Overall, I expect more good things to happen to me than bad.
LOTR6.r <- as.numeric(LOTR6)  
table(LOTR6, LOTR6.r)         

LOTR.scale <- (LOTR1.r + LOTR2.r + LOTR3.r + LOTR4.r + LOTR5.r + LOTR6.r) -5

# summary statistics + pq table

summarytools::descr(LOTR.scale)

stats_lotr <- data.frame(
  col1 = c("N", "Min", "Max", "Q1", "Q3", "Median", "Mean", "Standard Deviation"), 
  col2 = c(2867, 1, 25, 14, 19, 17, 16.38, 3.86)
)

colnames(stats_lotr) <- c('Summary Statistics','Results') # renaming columns

kbl(stats_lotr)
stats_lotr %>%
  kbl(caption = "Summary Statistics for LOTR, Optimism Scale") %>%
  kable_minimal("hover", full_width = F)

# Q3c

hist(LOTR.scale, 
     main="Distribution of Reported Levels of Optimism",
     xlab="Levels of Optimism",
     col="darkgreen",
     )
