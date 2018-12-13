setwd("C:/Users/test/Desktop/dados/ess/ess6fr")

# loading packages
library(foreign)

# loading dataset
ess6fr <- read.dta("ESS6FR.dta")

# recoding country name
ess6fr$country <- ess6fr$cntry
ess6fr$country[ess6fr$country == "FR"] <- "France"
table(ess6fr$country)

# inserting variable "election year"
ess6fr$election_year <- rep(2012)
table(ess6fr$election_year)

# creating variable "age"
ess6fr$age <- ess6fr$agea
summary(ess6fr$age)

# creating variable "gender"
ess6fr$gender <- ess6fr$gndr

# creating variable "men"
ess6fr$men <- ifelse(ess6fr$gender == "Male", 1, 0)
str(ess6fr$men)

# creating variable "education"
ess6fr$education <- ess6fr$edulvlb
ess6fr$education
table(ess6fr$education)

# recoding values of the variable education
Standard_Class <- function(x){
  xx <- gsub(',','',x)
  xx <- gsub('A','',xx)
  xx <- gsub('B','',xx)
  xx <- gsub('C','',xx)
  xx <- unlist(strsplit(xx,split = ' '))
  
  for(w in xx){
    if(w == 'Not'){
      return(0)
      break
    }
    else if(w == '1'){
      return(1)
      break
    }
    else if(w == '2'){
      return(2)
      break
    }
    else if(w == '3'){
      return(3)
      break
    }
    else if(w == '4'){
      return(4)
      break
    }
    else if(w == '5'){
      return(5)
      break
    }
    else if(w == '6'){
      return(5)
      break
    }
    else if(w == 'Other'){
      return(NA)
      break
    }
    
  }
}

education <- lapply(ess6fr$edulvlb, Standard_Class)
ess6fr$education <- unlist(education)
ess6fr$education <- ifelse(ess6fr$education == 0, 1, ess6fr$education)
table(ess6fr$education)

# creating dummy variable for unemployeds
ess6fr$unemployed <- ifelse(ess6fr$uemp3m == "Yes", 1, 0)
table(ess6fr$unemployed)

# creating dummy for blue-collars
levels(as.factor(ess6fr$isco08))
ess6fr$blue_collar <- as.numeric(ess6fr$isco08)
ess6fr$blue_collar <- ifelse(ess6fr$blue_collar > 394 & ess6fr$blue_collar < 568, 1, 0)
table(ess6fr$blue_collar)

# creating dummy to self employed
summary(ess6fr$tporgwk)
ess6fr$self_employed <- ifelse(ess6fr$tporgwk == "Self employed", 1, 0)

# creating household income
ess6fr$household <- ess6fr$hinctnta
table(ess6fr$household)
ess6fr$household <- as.numeric(ess6fr$household)

# creating dummy for rural inhabitants
table(ess6fr$domicil)
ess6fr$rural <- ifelse(ess6fr$domicil %in% c("Country village", 
                                             "Farm or home in countryside"), 1, 0)
table(ess6fr$rural)

# creating dummy for vote to Front National
table(ess6fr$prtvtcfr)
ess6fr$vote_fn <- ifelse(ess6fr$prtvtcfr == "FN (Front National)", 1, 0)
table(ess6fr$vote_fn)

# creating variable to government performance (there is a simple way to invert the values?)
ess6fr$gov_perform <- ess6fr$stfgov
ess6fr$gov_perform <- as.numeric(ess6fr$gov_perform)
table(ess6fr$gov_perform)

# creating variable to satisfaction with democracy
ess6fr$diss_dem <- ess6fr$stfdem
ess6fr$diss_dem <- as.numeric(ess6fr$diss_dem)
table(ess6fr$diss_dem)

# satisfaction with economy (also there is a way to invert values?)
ess6fr$diss_econ <- ess6fr$stfeco
ess6fr$diss_econ <- as.numeric(ess6fr$diss_econ)
table(ess6fr$diss_econ)

# how important is to your country to be democratically governed
ess6fr$democrats <- ess6fr$implvdm
ess6fr$democrats <- as.numeric(ess6fr$democrats)
table(ess6fr$democrats)

# placement of respondent on left-right scale
ess6fr$lrself <- as.numeric(ess6fr$lrscale)
table(ess6fr$lrself)

# trust in politicians
ess6fr$trust_pol <- ess6fr$trstplt
ess6fr$trust_pol <- as.numeric(ess6fr$trust_pol)
table(ess6fr$trust_pol)

# creating "vars" object
vars <- c("country", "election_year", "age", "gender", "men", "education", "unemployed", 
          "blue_collar", "self_employed", "household", "rural", "gov_perform", 
          "diss_dem", "diss_econ", "democrats", "lrself", "trust_pol", "vote_fn")

# inserting variables of interest
fr12 <- ess6fr[vars]

# saving file
write.table(fr12, "C:/Users/test/Desktop/dados/ess/ess6fr/data_ess_fr12.txt", 
            sep = ';')
