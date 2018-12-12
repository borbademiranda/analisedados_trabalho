setwd("C:/Users/test/Desktop/dados/ess/ess2fr")

# loading packages
library(foreign)

# loading dataset
ess2fr <- read.dta("ESS2FR.dta")

# recoding country name
ess2fr$country <- ess2fr$cntry
ess2fr$country[ess2fr$country == "FR"] <- "France"
table(ess2fr$country)

# creating variable "age"
ess2fr$age <- ess2fr$agea
summary(ess2fr$age)

# creating variable "gender"
ess2fr$gender <- ess2fr$gndr

# creating variable "men"
ess2fr$men <- ifelse(ess2fr$gender == "Male", 1, 0)
table(ess2fr$men)

# creating variable "education"
ess2fr$education <- as.numeric(ess2fr$edulvla)
ess2fr$education <- ess2fr$education - 1
table(ess2fr$education)


# creating dummy variable for unemployeds
ess2fr$unemployed <- ifelse(ess2fr$uemp3m == "Yes", 1, 0)
table(ess2fr$unemployed)

# creating dummy for blue-collars
summary(ess2fr$iscoco)
levels(as.factor(ess2fr$iscoco))
ess2fr$blue_collar <- as.numeric(ess2fr$iscoco)
ess2fr$blue_collar <- ifelse(ess2fr$blue_collar > 278 & ess2fr$blue_collar < 457, 1, 0)
table(ess2fr$blue_collar)

# creating dummy to self employed
summary(ess2fr$emplrel)
ess2fr$self_employed <- ifelse(ess2fr$emplrel == "Self-employed", 1, 0)
table(ess2fr$self_employed)

# creating household income
ess2fr$household <- ess2fr$hinctnt
table(ess2fr$household)
ess2fr$household <- as.numeric(ess2fr$household)

# creating dummy for rural inhabitants
table(ess2fr$domicil)
ess2fr$rural <- ifelse(ess2fr$domicil %in% c("Country village", 
                                             "Farm or home in countryside"), 1, 0)
table(ess2fr$rural)

# creating dummy for vote to Front National
table(ess2fr$prtvtfr)
ess2fr$vote_fn <- ifelse(ess2fr$prtvtfr == "FN (Front National)", 1, 0)
table(ess2fr$vote_fn)

# creating variable to government performance
ess2fr$gov_perform <- ess2fr$stfgov
ess2fr$gov_perform <- as.numeric(ess2fr$gov_perform)
table(ess2fr$gov_perform)

# creating variable to satisfaction with democracy
ess2fr$diss_dem <- ess2fr$stfdem
ess2fr$diss_dem <- as.numeric(ess2fr$diss_dem)
table(ess2fr$diss_dem)

# satisfaction with economy
ess2fr$diss_econ <- ess2fr$stfeco
ess2fr$diss_econ <- as.numeric(ess2fr$diss_econ)
table(ess2fr$diss_econ)

# wish to ban parties that want to overthrow democracy. Used as a proxy to the 
# extent for how much democracy is important
ess2fr$democrats <- ess2fr$prtyban
ess2fr$democrats <- as.numeric(ess2fr$democrats)
table(ess2fr$democrats)

# placement of respondent on left-right scale
ess2fr$lrself <- as.numeric(ess2fr$lrscale)
table(ess2fr$lrself)

# trust in politicians
ess2fr$trust_pol <- ess2fr$trstplt
ess2fr$trust_pol <- as.numeric(ess2fr$trust_pol)
table(ess2fr$trust_pol)

# creating "vars" object
vars <- c("country", "age", "gender", "men", "education", "unemployed", "blue_collar",
          "self_employed", "household", "rural", "gov_perform", "diss_dem", 
          "diss_econ", "democrats", "lrself", "trust_pol", "vote_fn")

# inserting variables of interest
fr02 <- ess2fr[vars]

# saving file
write.table(fr02, "C:/Users/test/Desktop/dados/ess/ess2fr/data_ess_fr02.txt", 
            sep = ';')