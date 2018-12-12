setwd("C:/Users/test/Desktop/dados/ess/ess4fr")

# loading packages
library(foreign)

# loading dataset
ess4fr <- read.dta("ESS4FR.dta")

# recoding country name
ess4fr$country <- ess4fr$cntry
ess4fr$country[ess4fr$country == "FR"] <- "France"
table(ess4fr$country)

# creating variable "age"
ess4fr$age <- ess4fr$agea
summary(ess4fr$age)

# creating variable "gender"
ess4fr$gender <- ess4fr$gndr

# creating variable "men"
ess4fr$men <- ifelse(ess4fr$gender == "Male", 1, 0)
str(ess4fr$men)

# creating variable "education"
ess4fr$education <- ess4fr$edulvla
ess4fr$education <- as.numeric(ess4fr$education) - 1
table(ess4fr$edulvla)
table(ess4fr$education)

# creating dummy variable for unemployeds
ess4fr$unemployed <- ifelse(ess4fr$uemp3m == "Yes", 1, 0)
summary(ess4fr$unemployed)

# creating dummy for blue-collars
summary(ess4fr$iscoco)
ess4fr$blue_collar <- as.numeric(ess4fr$iscoco)
ess4fr$blue_collar <- ifelse(ess4fr$blue_collar > 394 & ess4fr$blue_collar < 568, 1, 0)
table(ess4fr$blue_collar)

# creating dummy to self employed
summary(ess4fr$tporgwk)
ess4fr$self_employed <- ifelse(ess4fr$tporgwk == "Self-employed", 1, 0)

# creating household income
ess4fr$household <- ess4fr$hinctnta
table(ess4fr$household)
ess4fr$household <- as.numeric(ess4fr$household)

# creating dummy for rural inhabitants
table(ess4fr$domicil)
ess4fr$rural <- ifelse(ess4fr$domicil %in% c("Country village", 
                                             "Farm or home in countryside"), 1, 0)
table(ess4fr$rural)

# creating dummy for vote to Front National
table(ess4fr$prtvtbfr)
ess4fr$vote_fn <- ifelse(ess4fr$prtvtbfr == "FN (Front National)", 1, 0)
table(ess4fr$vote_fn)

# creating variable to government performance
ess4fr$gov_perform <- ess4fr$stfgov
ess4fr$gov_perform <- as.numeric(ess4fr$gov_perform)
table(ess4fr$gov_perform)

# creating variable to satisfaction with democracy
ess4fr$diss_dem <- ess4fr$stfdem

# satisfaction with economy
ess4fr$diss_econ <- ess4fr$stfeco
ess4fr$diss_econ <- as.numeric(ess4fr$diss_econ)
table(ess4fr$diss_econ)

# wish to ban parties that want to overthrow democracy. Used as a proxy to the 
# extent for how much democracy is important
ess4fr$democrats <- ess4fr$prtyban
ess4fr$democrats <- as.numeric(ess4fr$democrats)
table(ess4fr$democrats)

# placement of respondent on left-right scale
ess4fr$lrself <- as.numeric(ess4fr$lrscale)
table(ess4fr$lrself)

# trust in politicians
ess4fr$trust_pol <- ess4fr$trstplt
table(ess4fr$trust_pol)
ess4fr$trust_pol <- as.numeric(ess4fr$trust_pol)

# creating "vars" object
vars <- c("country", "age", "gender", "men", "education", "unemployed", "blue_collar",
          "self_employed", "household", "rural", "gov_perform", "diss_dem", 
          "diss_econ", "democrats", "lrself", "trust_pol", "vote_fn")

# inserting variables of interest
fr07 <- ess4fr[vars]

# saving file
write.table(fr07, "C:/Users/test/Desktop/dados/ess/ess4fr/data_ess_fr07.txt", 
            sep = ';')