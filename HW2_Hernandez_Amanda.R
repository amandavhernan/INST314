# Amanda Hernandez, INST314 HW2

# Q4a-e
airbags <- read.csv(file.choose(), header=T)

# Q4a - Amanda Hernandez, INST314 HW2
nrow(airbags)
ncol(airbags)

# Q4b (missing values, percentage) - Amanda Hernandez, INST314 HW2
sum(is.na(airbags$injSeverity))

table(airbags$injSeverity)
addmargins(table(airbags$injSeverity))
# 3 + 4 (represents # incapacitated, # killed)
8495+1118
(9613/26064)*100 # result is 36.9% (rounded) of the obs were incapacitated/killed
round(prop.table(table(airbags$injSeverity)), 3)*100

# Q4c - Amanda Hernandez, INST314 HW2
install.packages("kableExtra") # install kableExtra package
library(kableExtra)

# frequency
# dt1 -> data table 1
dt1 <- addmargins(table(airbags$seatbelt, airbags$dead))
kbl(dt1)
dt1 %>%
  kbl(caption = "The Relationship Between Seatbelt Use & Death") %>%
  kable_paper("hover", full_width = F)

# percentage
# dt2 -> data table 2
dt2 <- round(addmargins(prop.table(table(airbags$seatbelt, airbags$dead),2)*100),1)
kbl(dt2)
dt2 %>%
  kbl(caption = "The Relationship Between Seatbelt Use & Death") %>%
  kable_paper("hover", full_width = F)

# no design
addmargins(table(airbags$seatbelt, airbags$dead))
round(addmargins(prop.table(table(airbags$seatbelt, airbags$dead),2)*100),1)

install.packages("summarytools") # install summary tools package
library(summarytools)
# Q4d - Amanda Hernandez, INST314 HW2

sum <- summarytools::descr(airbags$yearVeh)
kbl(sum)
sum %>%
  kbl(caption = "Summary Statistics of yearVeh Variable") %>%
  kable_paper("hover", full_width = F)

# no design
summarytools::descr(airbags$yearVeh)

# Q4e - Amanda Hernandez, INST314 HW2
hist(airbags$yearVeh,
     col="lightblue", # color of bins
     main="Year Model of Vehicles Involed in Fatal Accidents",  # title of histogram
     xlab="Year of Vehicle", # x-axis label
     )

# Q5a-c
load(file.choose()) # download gss data

# Q5a - Amanda Hernandez, INST314 HW2
table(gss$HRS1)
class(gss$HRS1)

# Q5b - Amanda Hernandez, INST314 HW2
HRS1_rc <- gss$HRS1

levels(HRS1_rc) <- c(levels(gss$HRS1), '100')
HRS1_rc[HRS1_rc == '89+ hrs']<- '100'
HRS1_rc <- factor(HRS1_rc)
levels(HRS1_rc)

HRS1_rc<-as.numeric(as.character(HRS1_rc))

table(HRS1_rc)
class(HRS1_rc)

# Q5c - Amanda Hernandez, INST314 HW2

gss$weeklyhrs <- factor(ifelse(gss$HRS1_rc < 40, 1, ifelse(gss$HRS1_rc == 40, 2, ifelse(gss$HRS1_rc > 40, 3, NA))), 
                       labels = c("part-time","full-time","more than full-time"), ordered = T)
table(gss$weeklyhrs)

# code for pq format
# insert object name <- gss$weeklyhrs code above
# kbl(object name)
# object name %>%
# kbl(caption = "title") %>%
# kable_paper("hover", full_width = F)