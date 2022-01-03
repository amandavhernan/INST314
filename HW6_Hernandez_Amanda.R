# Amanda Hernandez, INST314 - HW6

# loading packages
library(summarytools)
library(kableExtra)

# Q1c

# insert data
regular_gas <- c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28)
premium_gas <- c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)

# perform paired t-test

t.test(regular_gas, premium_gas, na.rm=T, pooled=F, paired=T)

# Q2

# insert, name data
comics_movies <- read.csv(file.choose(), header=T)

# create subsets for dc + marvel

dc <- subset(comics_movies, comics_movies$Studio == "DC")

marvel <- subset(comics_movies, comics_movies$Studio == "Marvel")

# summary stats, excluding missing values using na.rm=TRUE + pq table

round(summarytools::descr(dc$Worldwide, na.rm=TRUE), 2) # dc

round(summarytools::descr(marvel$Worldwide, na.rm=TRUE), 2) # marvel

round(summarytools::descr(comics_movies$Worldwide, na.rm=TRUE), 2) # combined

# insert summary stats into data frame
boxoffice_stats_pq <- data.frame(
  Studios = c("DC", "Marvel", "Combined"),
  N = c(25, 47, 72),
  Min = c(6.20, 12.60, 6.20),
  Max = c(1253.10, 1703.60, 1703.60),
  Q1 = c(118.60, 298.20, 249.80),
  Q3 = c(803.00, 847.70, 822.10),
  Median = c(533.10, 564.10, 558.30),
  Mean = c(526.86, 616.90, 585.63),
  SD = c(401.80, 393.05, 395.64)
)

# pq table
kbl(boxoffice_stats_pq)
boxoffice_stats_pq %>%
  kbl(caption = "Summary Statistics of Worldwide Box Office Success by Studio") %>%
  kable_minimal("hover", full_width = F) %>%
  column_spec(1, bold = T) %>%
  footnote(general = "Box office sales are expressed in millions of U.S. dollars.",
           footnote_as_chunk = T)

# Q2b

# independent two sample t-test
t.test(dc$Worldwide, marvel$Worldwide, na.rm=T, pooled=T, paired=F)
# results: t = -0.91202, df = 48.142, p-value = 0.3663

# mean of x (dc) 
# mean of y (marvel)

# Q2c

install.packages("effsize")
library(effsize)

# cohen's d
cohen.d(dc$Worldwide, marvel$Worldwide, na.rm=T, pooled=T, paired=F)

# difference in means
round(616.8957-526.8640, 2) # = 90.03

# Q3a

# creating subsets for pre/post-Disney

past_2009 <- subset(marvel, marvel$Year <= 2009)

present_2010 <- subset(marvel, marvel$Year >= 2010)

# summary stats

round(summarytools::descr(past_2009$Review, na.rm=TRUE), 2) # pre-Disney

round(summarytools::descr(present_2010$Review, na.rm=TRUE), 2) # post-Disney

round(summarytools::descr(marvel$Review, na.rm=TRUE), 2) # total

# inserting summary stats into data frame
# marvel -> past_2009, disney_marvel -> present_2010

review_stats_pq <- data.frame(
  Studios = c("Marvel", "Disney-Marvel", "Total"),
  N = c(21, 26, 47),
  Min = c(30, 26, 26),
  Max = c(89, 88, 89),
  Q1 = c(42, 71, 49),
  Q3 = c(68, 84, 82),
  Median = c(58, 78.50, 72),
  Mean = c(57.33, 74.50, 66.83),
  SD = c(18.45, 14.83, 18.49)
)

# pq chart

kbl(review_stats_pq)
review_stats_pq %>%
  kbl(caption = "Summary Statistics for Reviews of Marvel Films Pre/Post Disney Purchase") %>%
  kable_minimal("hover", full_width = F) %>%
  column_spec(1, bold = T)

# Q3b

# independent two sample t-test
t.test(past_2009$Review, present_2010$Review, na.rm=T, pooled=T, paired=F)
# results: t = -3.4556, df = 38.034, p-value = 0.001365

# Q4b

install.packages("pwr")
library(pwr)

pwr.t.test(power=NULL, n=126, sig.level=0.05, type="two.sample", alt="two.sided", d=0.5)
# power = 0.9768897, 0.98 rounded

round(0.9768897, 2)
