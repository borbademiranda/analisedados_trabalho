setwd("C:/Users/test/Desktop/dados/cses/cses2")
load("cses2.rdata")

# recoding country name
cses2$country <- cses2$B1006_NAM

# recoding election year
cses2$electionyear <- cses2$B1008

# creating variable "age"
cses2$age <- cses2$B2001

# gender
cses2$gender <- cses2$B2002

# creating variable "men"
cses2$men <- ifelse(cses2$B2002 ==  1, 1, 0)

# recoding education
cses2$education <- ifelse(cses2$B2003 > 8, NA , cses2$B2003)

# creating dummy for unemployed
cses2$unemployed <- ifelse(cses2$B2010 ==  5, 1, 0)

# creating dummy for blue-collar
cses2$blue_collar <- ifelse(cses2$B2011 > 70 & cses2$B2011 < 95, 1, 0)
summary(cses2$B2011[cses2$country == "France"])

# creating dummy for self-employed
cses2$self_employed <- ifelse(cses2$B2012 == 4, 1, 0)
summary((cses2$B2012[cses2$country == "France"]))

# creating household variable
cses2$household <- ifelse(cses2$B2020 > 5, NA , cses2$B2020)

# creating dummy for rural inhabitants
cses2$rural <- ifelse(cses2$B2030 == 1, 1, 0)

# creating dummy for vote in FN
cses2$vote_fn <- ifelse(cses2$B3005_1 == 2 & cses2$B1006_NAM == 'France', 1 ,0)


# variable for government performance
cses2$gov_perform <- ifelse(cses2$B3011 > 4, NA , cses2$B3011)

# variable for dissatisfection with democracy
cses2$diss_dem <- ifelse(cses2$B3012 > 4, NA, cses2$B3012)
str(cses2$diss_dem)

# creating variable for anti-democrats
cses2$anti_dem <- ifelse(cses2$B3015 > 4, NA, cses2$B3015)

# who people votes for makes a difference
cses2$vote_makes_diff <- ifelse(cses2$B3014 > 5, NA, cses2$B3014)


# variable for partisanship
cses2$partisan <- ifelse(cses2$B3028 > 2, NA, cses2$B3028)
cses2$partisan <- ifelse(cses2$partisan == 1, 1, 0)

# FN partisanship
cses2$partisan_fn <- ifelse(cses2$B3029_1 == 2 & cses2$B1006_NAM == 'France', 1 ,0)

# left-right scale of respondent
cses2$lrscale <- ifelse(cses2$B3045 > 10, NA, cses2$B3045)

# creating variable for tolerance
cses2$tolerance <- ifelse(cses2$B3043 > 4, NA, cses2$B3043)

# perception of corruption
cses2$wide_corrup <- ifelse(cses2$B3044 > 4, NA, cses2$B3044)

# creating "vars" object
vars <- c("country", "electionyear", "age", "gender", "men", "education", 
          "unemployed", "household", "rural", "blue_collar", "self_employed",
          "vote_fn", "gov_perform", "diss_dem", "anti_dem", "partisan", 
          "partisan_fn", "lrscale", "tolerance", "wide_corrup")

# inserting interest variables in an object
fr02 <- cses2[vars]

# extracting french data
fr02 <- fr02[fr02$country == 'France',]

write.table(fr02, "C:/Users/test/Desktop/dados/cses/cses2/data_france_02.txt", 
            sep = ';')
