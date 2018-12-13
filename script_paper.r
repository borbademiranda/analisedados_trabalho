setwd("C:/Users/test/Documents/GitHub/analisedados_trabalho")

# loading packages
library(tidyverse)
library(ggplot2)
library(devtools)

# loading dataset
fr0212full <- read.delim("ess_fr_2002-2012.txt", sep = ";")
fr0212 <- na.omit(fr0212full)

# histogram of the variable for self positionment on the left right scale by year
fr0212$election <- as.character(fr0212$election_year)

p <- ggplot(fr0212, aes(x = lrself)) +
  geom_histogram(color = "black") +
  geom_density(alpha = .2) +
  facet_grid(election ~ .) +
  labs(title = "Left-Right frequency of the individuals by year of election",
       x = "Left-Right scale", y = "Count")
p

# histogram for variable satisfaction with economy
p2 <- ggplot(fr0212, aes(x = diss_econ)) +
  geom_histogram(color = "black") +
  facet_grid(election ~ .) +
  labs(title = "Satisfaction with economy by year of election",
       x = "Satisfaction with economy")
p2

# boxplot of satisfaction with economy
ggplot(data = fr0212, aes(x = election, y = diss_econ)) +
  geom_boxplot() +
  labs(title = "Satisfaction with economy by year of election",
       y = "satisfaction with economy", x = "election year")

# line plot of satisfaction with government
ggplot(data = fr0212, aes(x = election, y = gov_perform)) +
  geom_boxplot() +
  labs(title = "Satisfaction with actual government by election year",
       y = "satisfaction with government", x = "election year")

# logit model 1: demographic variables
logit1 <- glm(data = fr0212, vote_fn ~ age + men + education + household + 
                factor(election), family = binomial)
summary(logit1)

# calculating pseudo r2 Mc Fadden's
LLK_Full <- logLik(logit1)[1]
LLK_Int <- logLik(glm(vote_fn ~ 1, data = fr0212, family = binomial))[1]

1 - (LLK_Full / LLK_Int)

# Mc Fadden's Adjusted
1 - ((LLK_Full - 5) / LLK_Int)

# saving coefficients
beta1 <- logit1$coefficients

# saving confidence intervals
int1 <- confint(logit1)

# converting from log odd to predicted probabilities
beta1 <- exp(beta1) / (1 + exp(beta1))
int1 <- exp(int1) / (1 + exp(int1))

# calculating standard deviation from the confidence intervals 
std1 <- (int1[,2] - beta1) / 1.96

## ploting model
library(arm)

coefplot(beta1,
         std1,
         varnames = c("Intercept", "Age", "Men", "Education", "Income",
                      "2007's elec", "2012's elec"),
         main = "Logit model 1: probability of voting in the FN",
         cex.var = 0.8,
         cex.pts = 1.5,
         mar = c(1, 0, 5.1, 2),
         var.las = 2)
abline(v = 0.5)

# dot-and-whisker plot of logit1 model
library(dotwhisker)
library(broom)
library(dplyr)

dwplot(logit1,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))

# logit model 2: sociodemographic variables
logit2 <- glm(data = fr0212, vote_fn ~ unemployed + blue_collar +
                rural + factor(election), family = binomial)
summary(logit2)

# calculating pseudo r2 Mc Fadden's
LLK_Full2 <- logLik(logit2)[1]
LLK_Int2 <- logLik(glm(vote_fn ~ 1, data = fr0212, family = binomial))[1]

1 - (LLK_Full2 / LLK_Int2)

# Mc Fadden's Adjusted
1 - ((LLK_Full2 - 5) / LLK_Int2)

# saving coefficients
beta2 <- logit2$coefficients

# saving confidence intervals
int2 <- confint(logit2)

# converting from log odd to predicted probabilities
beta2 <- exp(beta2) / (1 + exp(beta2))
int2 <- exp(int2) / (1 + exp(int2))

# calculating standard deviation from the confidence intervals 
std2 <- (int2[,2] - beta2) / 1.96

# ploting model
coefplot(beta2,
         std2,
         varnames = c("Intercept", "Unemployed", "Blue-collar", "Rural inh"),
         main = "Logit model 2: probability of voting in the FN",
         cex.var = 0.8,
         cex.pts = 1.5,
         mar = c(1, 0, 5.1, 2),
         var.las = 2)
abline(v = 0.5)

# logit model 3: political variables
fr0212$diss_dem <- as.numeric(fr0212$diss_dem)
logit3 <- glm(data = fr0212, vote_fn ~ lrself + gov_perform + diss_dem + 
                diss_econ + trust_pol, family = binomial)
summary(logit3)

# Mc Fadden's
LLK_Full3 <- logLik(logit3)[1]
LLK_Int3 <- logLik(glm(vote_fn ~ 1, data = fr0212, family = binomial))[1]

1 - (LLK_Full3/ LLK_Int3)

# Mc Fadden's Adjusted
1 - ((LLK_Full3 - 4) / LLK_Int3)

# saving coefficients
beta3 <- logit3$coefficients

# saving confidence intervals
int3 <- confint(logit3)

# converting from log odd to predicted probabilities
beta3 <- exp(beta3) / (1 + exp(beta3))
int3 <- exp(int3) / (1 + exp(int3))

# calculating standard deviation from the confidence intervals 
std3 <- (int3[,2] - beta3) / 1.96

# ploting logit 3
coefplot(beta3,
         std3,
         varnames = c("Intercept", "LRscale", "Satisf gov", "Satisf dem", 
                      "Satisf econ", "Trust polity"),
         main = "Logit model 3: probability of voting in the FN",
         cex.var = 0.8,
         cex.pts = 1.5,
         mar = c(1, 0, 5.1, 2),
         var.las = 2)
abline(v = 0.5)

logitgen <- glm(data = fr0212, vote_fn ~ age + men + education + household + 
                  unemployed + blue_collar + rural + lrself + 
                  gov_perform + diss_dem + diss_econ + trust_pol + election,
                family = binomial)
summary(logitgen)

# Mc Fadden's
LLK_Full4 <- logLik(logitgen)[1]
LLK_Int4 <- logLik(glm(vote_fn ~ 1, data = fr0212, family = binomial))[1]

1 - (LLK_Full4 / LLK_Int4)

# Mc Fadden's Adjusted
1 - ((LLK_Full4 - 4) / LLK_Int4)

# dot whisker of model 4's coefficients
dwplot(logitgen,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))

# saving coefficients
betagen <- logitgen$coefficients

# saving confidence intervals
intgen <- confint(logitgen)

# converting from log odd to predicted probabilities
betagen <- exp(betagen) / (1 + exp(betagen))
intgen <- exp(intgen) / (1 + exp(intgen))

# calculating standard deviation from the confidence intervals 
stdgen <- (intgen[,2] - betagen) / 1.96

# plotting model 4
coefplot(betagen[2:14],
         stdgen[2: 14],
         varnames = c("Age", "Men", "Educ", "Income", "Unemployed",
                      "Blue Col", "Rural inhab", "LR Self", "Satisf gov",
                      "Satisf democracy", "Satisf econ", "Trust Politicians", 
                      "Election 2007", "Election 2012"),
         main = "Logit model 4: probability of voting in the FN",
         cex.var = 0.8,
         cex.pts = 1.5,
         mar = c(1, 0, 5.1, 2),
         var.las = 2)
abline(v = 0.5)

plot(logitgen)
hist(residuals(logitgen))

# simulating data
simdata <- data.frame(age = mean(fr0212$age), 
                      men = 0,
                      education = mean(fr0212$education),
                      household = mean(fr0212$household),
                      unemployed = 0,
                      blue_collar = 0,
                      rural = 0,
                      lrself = mean(fr0212$lrself),
                      gov_perform = mean(fr0212$gov_perform),
                      diss_dem = mean(fr0212$diss_dem),
                      diss_econ = mean(fr0212$diss_econ),
                      trust_pol = 1 : 11,
                      election = factor(2012))

prob_trust <- predict.glm(logitgen, simdata, type = 'response')

ggplot(data = simdata, aes(x = simdata$trust_pol, y = prob_trust)) +
  geom_smooth(se = T) +
  geom_point()

plot(prob_trust ~ simdata$trust_pol)

write.csv(fr0212, "C:/Users/test/Documents/GitHub/analisedados_trabalho/fr0212.csv", 
          sep = ";")
