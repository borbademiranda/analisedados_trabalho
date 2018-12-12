setwd("C:/Users/test/Desktop/dados/cses/cses4")
load("cses4.rdata")

# recoding country name
cses4$country <- cses4$D1006_NAM
head(cses4$country)

# recoding election year
cses4$electionyear <- cses4$D1008
head(cses4$electionyear)

# creating variable "age"
cses4$age <- 2012 - cses4$D2001_Y
head(cses4$age)

# gender
cses4$gender <- cses4$D2002
head(cses4$gender)

# creating variable "men"
cses4$men <- ifelse(cses4$D2002 ==  1, 1, 0)
head(cses4$men)

# recoding education
cses4$education <- ifelse(cses4$D2003 > 9, NA , cses4$D2003)
head(cses4$education)

# creating dummy for unemployed
cses4$unemployed <- ifelse(cses4$D2010 ==  5, 1, 0)
head(cses4$unemployed)

# creating dummy for blue-collar
cses4$blue_collar <- ifelse(cses4$D2011 > 700 & cses4$D2011 < 933, 1, 0)
table(cses4$blue_collar)
summary(cses4$blue_collar[cses4$country == "France"])

# creating dummy for self-employed
cses4$self_employed <- ifelse(cses4$D2012 == 4, 1, 0)
summary((cses4$self_employed[cses4$country == "France"]))

# creating household income variable
cses4$household <- ifelse(cses4$D2020 > 5, NA , cses4$D2020)
head(cses4$household)

# creating dummy for rural inhabitants
cses4$rural <- ifelse(cses4$D2031 == 1, 1, 0)
head(cses4$rural)
summary(cses4$rural)

# creating dummy for vote in FN
cses4$vote_fn <- ifelse(cses4$D3006_PR_1 == 9 & cses4$D1006_NAM == 'France', 1 ,0)
table(cses4$D3006_PR_1[cses4$D1006_NAM == 'France'])

# variable for government performance
cses4$gov_perform <- ifelse(cses4$D3011 > 4, NA , cses4$D3011)

# variable for dissatisfection with democracy
cses4$diss_dem <- ifelse(cses4$D3017 > 5, NA, cses4$D3017)
summary(cses4$diss_dem)
cses4$diss_dem[cses4$diss_dem == 4] <- '3'
cses4$diss_dem[cses4$diss_dem == 5] <- '4'
cses4$diss_dem <- as.numeric(cses4$diss_dem)

# creating variable for anti-democrats
cses4$anti_dem <- ifelse(cses4$D3015 > 5, NA, cses4$D3015)

# variable for partisanship
cses4$partisan <- ifelse(cses4$D3018_1 > 5, NA, cses4$D3018_1)
cses4$partisan <- ifelse(cses4$partisan == 1, 1, 0)

# FN partisanship
summary(cses4$D3018_3)
cses4$partisan_fn <- ifelse(cses4$D3018_3 == 9 & cses4$D1006_NAM == 'France', 1 ,0)

# left-right scale of respondent
cses4$lrscale <- ifelse(cses4$D3014 > 10, NA, cses4$D3014)

# who people votes for makes difference
cses4$vote_makes_diff <- ifelse(cses4$D3010 > 5, NA, cses4$D3010)

# creating variable for tolerance
cses4$tolerance <- ifelse(cses4$D3043 > 4, NA, cses4$D3043)

# perception of corruption
cses4$wide_corrup <- ifelse(cses4$D3044 > 4, NA, cses4$D3044)

# creating "vars" object
vars <- c("country", "electionyear", "age", "gender", "men", "education", 
          "unemployed", "household", "rural", "blue_collar", "self_employed",
          "vote_fn", "gov_perform", "diss_dem", "anti_dem", "partisan", 
          "partisan_fn", "lrscale", "tolerance", "wide_corrup")

# inserting interest variables in an object
fr12 <- cses4[vars]
vars[!(colnames(cses4) %in% vars)]

# extracting french data
fr12 <- fr12[fr12$country == 'France',]

# saving file

write.table(fr12, "C:/Users/test/Desktop/dados/cses/cses4", sep = ';')
