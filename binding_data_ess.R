# setting working directory
setwd("C:/Users/test/Desktop/dados/ess/ess2fr")

# loading dataset
fr02 <- read.delim("data_ess_fr02.txt", sep = ";")

# setting working directory
setwd("C:/Users/test/Desktop/dados/ess/ess4fr")

# loading dataset
fr07 <- read.delim("data_ess_fr07.txt", sep = ";")

# setting directory
setwd("C:/Users/test/Desktop/dados/ess/ess6fr")

# loading dataset
fr12 <- read.delim("data_ess_fr12.txt", sep = ";")

# binding datasets relative to the three elections (2002, 2007, 2012)
fr02_12 <- rbind(fr02, fr07)
fr02_12 <- rbind(fr02_12, fr12)

# saving dataset
write.table(fr02_12, "C:/Users/test/Documents/GitHub/analisedados_trabalho/ess_fr_2002-2012.txt", 
            sep = ";")

# load test
setwd("C:/Users/test/Documents/GitHub/analisedados_trabalho")

essfr0212 <- read.delim("ess_fr_2002-2012.txt", sep = ";")
