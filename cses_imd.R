setwd("C:/Users/test/Desktop/dados/cses/csesimd")
load("cses_imd.rdata")

# dataset with France data
csesfr <- cses_imd[cses_imd$IMD1004 %in% c("FRA_2002", "FRA_2007", 
                                                 "FRA_2012"),]

# table of variable of vote in FN
with(csesharm, table(IMD1008_YEAR, IMD3002_PR_1))


# creating a dummy variable to the vote on the Front National. Vote = 1, non-vote = 0
csesfr$vote_fn <- ifelse(csesfr$IMD3002_PR_1 == "2500007", 1, 0)

##### renaming some variables of interest #####
head(csesfr$IMD2001_2)

# age
names(csesfr)[names(csesfr) == "IMD2001_1"] <- "age"

# age in categories
names(csesfr)[names(csesfr) == "IMD2001_2"] <- "age_cat"

# transforming to factor
csesfr$age_cat <- as.factor(csesfr$age_cat)

# gender
names(csesfr)[names(csesfr) == "IMD2002"] <- "gender"

# adjusting values of gender: women = 0
csesfr$gender[csesfr$gender == 2] <- '0'

# education
names(csesfr)[names(csesfr) == "IMD2003"] <- "edu"

# inserting NA's
csesfr$edu <- as.numeric(csesfr$edu)
csesfr$edu[csesfr$edu > 6] <- NA

# converting to factor
csesfr$edu <- as.factor(csesfr$edu)

# household income
names(csesfr)[names(csesfr) == "IMD2006"] <- "h_income"

# inserting NA's
csesfr$h_income <- as.numeric(csesfr$h_income)
csesfr$h_income[csesfr$h_income > 5] <- NA

# as factor
csesfr$h_income <- as.numeric(csesfr$h_income)

# rural or urban
names(csesfr)[names(csesfr) == "IMD2007"] <- "res_area"

# creating a variable based on residence area of respondent. If the respondent
# asks 4, it is an inhabitant of a large town. I want to joint people who lives
# in small and rural villages in only one category. If the respondent lives in
# a rural or small village = 1, if else, 0.
csesfr$res_area[csesfr$res_area > 4] <- NA
csesfr$res_area <- ifelse(csesfr$res_area < 3, 1, 0)

# recoding to "rural"
names(csesfr)[names(csesfr) == "res_area"] <- "rural"

# recoding party identification
names(csesfr)[names(csesfr) == "IMD3005_3"] <- "party_id"

# recoding party closeness
names(csesfr)[names(csesfr) == "IMD3005_4"] <- "party_close"
str(csesfr$party_close)

# self left-right positionment
names(csesfr)[names(csesfr) == "left-right"] <- "LR"

# satisfaction with democracy
names(csesfr)[names(csesfr) == "IMD3010"] <- "satisf_dem"
str(csesfr$satisf_dem)
csesfr$satisf_dem[csesfr$satisf_dem >= 7] <- NA
table(csesfr$satisf_dem)

# who people votes can make a difference - used as proxy to democratic values
names(csesfr)[names(csesfr) == "IMD3012"] <- "vote_makes_diff"
str(csesfr$vote_makes_diff)
table(csesfr$vote_makes_diff)
hist(csesfr$vote_makes_diff)
csesfr$vote_makes_diff[csesfr$vote_makes_diff >= 7] <- NA

# state of economy in the last 12 months
names(csesfr)[names(csesfr) == "IMD3013_1"] <- "state_econ"
table(csesfr$state_econ)
csesfr$state_econ[csesfr$state_econ >= 7] <- NA
csesfr$state_econ[csesfr$state_econ %in% c(1, 3)] <- 0
csesfr$state_econ[csesfr$state_econ == 5] <- 1


# state of economy better
names(csesfr)[names(csesfr) == "IMD3013_2"] <- "econ_better"

# state of economy worse
names(csesfr)[names(csesfr) == "IMD3013_3"] <- "econ_worse"
table(csesfr$econ_worse)

# expert judgement of party position
# FN position left-right
names(csesfr)[names(csesfr) == "IMD5012_C"] <- "FN_lr"
table(csesfr$FN_lr)

hist(csesfr$FN_lr)


##### adding the variable labor structure, of another database of the same survey #####

# extracting data for France in the CSES fourth wave
setwd("C:/Users/test/Desktop/dados/cses/cses4")
load("cses4.rdata")

# extracting data relative to France
cses4fr <- cses4[cses4$D1006_NAM %in% "France",]

# reading the harmonized dataset CSES first to third wave
setwd("C:/Users/test/Desktop/dados/cses/csesharmonized")
if(require(readstata13) == F) install.packages("readstata13"); require(readstata13)
csesharm <- read.dta13("CSES1-3_harmonized_only.dta")

# extracting french data
fr <- csesharm[csesharm$study_id %in% c("FRA_2002", "FRA_2007"),]

# joining the variables of two different datasets into two objects
emp_sts <- fr$iA2007_m
emp_sts <- as.numeric(emp_sts)
emp_sts4 <- cses4fr$D2010
emp_sts4 <- as.numeric(emp_sts4)

# adding the new variable to the harmonized dataset of France
csesfr$employ_st <- cbind(c(emp_sts, emp_sts4))

##### creating the variable "unemployed" based on the value 5 of the variable "employ_st" #####
csesfr$unemp <- ifelse(csesfr$employ_st == 5, 1, 0)

# recoding levels of the variable "employ_st". The values added from the dataset
# CSES4 was with different values to the variables smaller than 5.
csesfr$employ_st[csesfr$employ_st < 5] <- 1

##### extracting the variable "socioeconomic status" of the harmonized dataset cses 
# first to third wave #####
fr$iB2012_m <- as.numeric(fr$iB2012_m)
ss <- fr$iB2012_m
summary(ss)
table(ss)

# extracting the same variable of the cses fourth wave france data
cses4fr$D2012 <- as.numeric(cses4fr$D2012)
ss2 <- cses4fr$D2012
table(ss2)

# making bind and inserting variable in the csesfr dataset
csesfr$socioec_status <- cbind(c(ss, ss2))
table(csesfr$socioec_status)
csesfr$socioec_status[csesfr$socioec_status >= 8] <- NA

# creating the "bluecollar" variable
csesfr$bluecol <- ifelse(csesfr$socioec_status %in% c(2, 3), 1, 0)
table(csesfr$bluecol)

# creating the variable "self employed"
csesfr$selfemp <- ifelse(csesfr$socioec_status == 4, 1, 0)
table(csesfr$selfemp)

# extracting data of the 2012's elections
fr2012 <- csesfr[csesfr$IMD1004 %in% "FRA_2012",]

##### test model #####
logit1 <- glm(data = fr2012, vote_fn ~ age + gender + h_income + rural + 
                unemp + bluecol + satisf_dem + state_econ + LR + 
                vote_makes_diff + gender*rural, family = binomial)
summary(logit1)

# saving coefficients
beta <- logit1$coefficients

# saving confidence intervals
intervals <- confint(logit1)

# converting from log odds to predicted probabilities
beta <- exp(beta) / (1 + exp(beta))

# confidence intervals  
intervals <- exp(intervals) / (1 + exp(intervals))

## calculating standard deviation from the confidence intervals mean
std <- (intervals[,2] - beta) / 1.96

## plotting model
if(require(arm) == F) install.packages("arm"); require(arm)

coefplot(beta,
         std,
         varnames = c('Intercept', "age", "gender", "h_income", "rural", 
                    "unemp", "bluecol", "Satisf_dem", "State Econ", "Left-right", 
                    "Vote makes diff", "Male and rural"),
         main = 'Logit model 1. 
         Dependent variable : Probability of vote to FN',
         mar = c(1, 5, 6.5, 2),
         cex.var = 1.0,
         cex.pts = 1.5)
abline(v = 0.5)

table(fr3$C3023_PR_1)
plot(fitted(logit1), residuals(logit1))
