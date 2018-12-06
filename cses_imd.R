setwd("C:/Users/test/Desktop/dados/cses/csesimd")
load("cses_imd.rdata")

# dataset with France data
csesfr <- cses_imd[cses_imd$IMD1004 %in% c("FRA_2002", "FRA_2007", 
                                                 "FRA_2012"),]

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
csesfr$h_income <- as.factor(csesfr$h_income)

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

# adding the variable labor structure, of another database of the same survey

# extracting data for France in the CSES fourth wave
setwd("C:/Users/test/Desktop/dados/cses/cses4")
load("cses4.rdata")

# extracting data relative to France
cses4fr <- cses4[cses4$D1006_NAM %in% "France",]

# reading the harmonized dataset CSES first to third wave
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

# creating the variable "unemployed" based on the value 5 of the variable "employ_st"
csesfr$unemp <- ifelse(csesfr$employ_st == 5, 1, 0)

# recoding levels of the variable "employ_st". The values added from the dataset
# CSES4 was with different values to the variables smaller than 5.
csesfr$employ_st[csesfr$employ_st < 5] <- 1


# extracting a variable of the harmonized dataset cses first to third wave
fr$iA2011_m <- as.numeric(fr$iA2011_m)
fr$iA2011_m[fr$iA2011_m >= 96] <- NA
occ <- fr$iA2011_m
summary(occ)
table(occ)

# recoding the same variable of the cses fouth wave
cses4fr$D2011[cses4fr$D2011 %in% 100 : 143] <- 1
cses4fr$D2011[cses4fr$D2011 %in% 200 : 265] <- 2
cses4fr$D2011[cses4fr$D2011 %in% 300 : 352] <- 3
cses4fr$D2011[cses4fr$D2011 %in% 400 : 441] <- 4
cses4fr$D2011[cses4fr$D2011 %in% 500 : 552] <- 5
cses4fr$D2011[cses4fr$D2011 %in% 600 : 634] <- 6
cses4fr$D2011[cses4fr$D2011 %in% 700 : 754] <- 7 
cses4fr$D2011[cses4fr$D2011 %in% 800 : 835] <- 8
cses4fr$D2011[cses4fr$D2011 %in% 900 : 962] <- 9
cses4fr$D2011[cses4fr$D2011 %in% 000 : 031] <- 10
cses4fr$D2011[cses4fr$D2011 > 962] <- NA

# attaching the variable to a object
occ2 <- cses4fr$D2011

# making bind and inserting variable in the csesfr dataset
csesfr$occ_status <- cbind(c(occ, occ2))

# creating the "bluecollar" variable
csesfr$blue_c <- ifelse(csesfr$occ_status %in% c(7, 8, 9), 1, 0)

# test model
logit1 <- glm(data = csesfr, vote_fn ~ age + gender + edu + h_income + rural + unemp, family = binomial)
summary(logit1)

