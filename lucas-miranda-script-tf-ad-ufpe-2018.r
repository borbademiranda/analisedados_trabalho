################# AUTHOR: LUCAS BORBA DE MIRANDA ####################
#################### DATE: january 16, 2018 #########################
# plots and models for the paper The rise of radical right in France: 
# searching the determinants of the vote in the Front National. 

# loading packages
if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(ggplot2) == F) install.packages("ggplot2"); require(ggplot2)
if(require(devtools) == F) install.packages("devtools"); require(devtools)
if(require(arm) == F) install.packages("arm"); require(arm)
if(require(dotwhisker) == F) install.packages("dotwhisker"); require(dotwhisker)
if(require(broom) == F) install.packages("broom"); require(broom)
if(require(dplyr) == F) install.packages("dplyr"); require(dplyr)
if(require(stargazer) == F) install.packages("stargazer"); require(stargazer)
if(require(car) == F) install.packages("car"); require(car)

# setting working directory
setwd("C:/Users/test/Documents/GitHub/analisedados_trabalho")

# loading dataset
fr0212full <- read.delim("lucas-miranda-bd-tf-ad-ufpe-2018.txt", sep = ";")
fr0212 <- na.omit(fr0212full)

# separating datasets by year
fr02 <- fr0212[fr0212full$election_year == 2002,]
fr07 <- fr0212[fr0212full$election_year == 2007,]
fr12 <- fr0212[fr0212full$election_year == 2012,]

##### descriptive analysis ####
# creating a character variable for "election"
fr0212$election <- as.character(fr0212$election_year)

# plot for trust in politicians
plot(tapply(fr0212$trust_pol, fr0212$election_year, median) ~ c(2002,2007,2012),
     type = 'l', ylab = "Trust in politicians (median)", ylim = c(0, 5),
     xlab = "Year", lwd = 2, cex.lab = 1, cex.axis = 1, family = 'serif',
     main = "Trust in politicians and politics - 2002-2012",
     sub = "Figure 1 - Author's ellaboration")

# density plot comparing density of satisfaction with government by election year
stfgov <- ggplot(fr0212, aes(x = gov_perform)) +
  geom_line(stat = "density", aes(color = election)) +
  scale_x_continuous(limits = c(1, 11), breaks = 1 : 11) +
  labs(y = "Density", x = "Satisfaction with government", 
       caption = "Figure 2 - Author's ellaboration") +
  ggtitle("Satisfaction with government performance") +
  theme_classic() +
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = .5))

# saving and exporting plot
ggsave("satisfgov.png", plot = stfgov, width = 8, height = 5)

# density plot for satisfaction with economy
stfecon <- ggplot(fr0212, aes(x = diss_econ)) +
  geom_line(stat = "density", aes(color = election), adjust = 3) +
  scale_x_continuous(limits = c(1, 11), breaks = 1 : 11) +
  labs(y = "Density", x = "Satisfaction with economy", 
       caption = "Figure 3 - Author's ellaboration") +
  ggtitle("Satisfaction with economy by year of election") +
  theme_classic() +
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = .5))

# saving and exporting plot
ggsave("satisf_economy.png", plot = stfecon, device = "png", width = 8, 
       height = 5)

# density plot satisfaction with democracy
fr0212$diss_dem <- as.numeric(fr0212$diss_dem)

stfdem <- ggplot(fr0212, aes(diss_dem)) +
  geom_line(stat = "density", aes(color = election)) +
  scale_x_continuous(limits = c(1, 11), breaks = 1 : 11) +
  labs(y = "Density", x = "Satisfaction with democracy", 
       caption = "Figure 4 - Author's ellaboration") +
  ggtitle("Satisfaction with the way democracy works") +
  theme_classic() +
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = .5))

# saving and exporting plot
ggsave("satisfdemocracy.png", plot = stfdem, device = "png", width = 8, 
       height = 5)

# density plot for left-right self-positionment
lr <- ggplot(fr0212, aes(x = lrself)) +
  geom_line(stat = "density", aes(color = election)) +
  scale_x_continuous(limits = c(1, 11), breaks = 1 : 11) +
  labs(y = "Density", x = "Left-right", 
       caption = "Figure 5 - Author's ellaboration") +
  ggtitle("Left-right self positionment by year of election") +
  theme_classic() +
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
        plot.caption = element_text(hjust = .5))

# saving and exporting plot
ggsave("lrself.png", plot = lr, device = "png", width = 8, height = 5)

# boxplot of satisfaction with economy
ggplot(data = fr0212, aes(x = election, y = diss_econ)) +
  geom_boxplot() +
  labs(title = "Satisfaction with economy by year of election",
       y = "satisfaction with economy", x = "election year")

# box plot of satisfaction with government
ggplot(data = fr0212, aes(x = election, y = gov_perform)) +
  geom_boxplot() +
  labs(title = "Satisfaction with actual government by election year",
       y = "satisfaction with government", x = "election year")

# boxplot for trust in politicians
ggplot(data = fr0212, aes(x = election, y = trust_pol)) +
  geom_boxplot() +
  labs(title = "Trust in politicians by year of elections in France",
       y = "trust in politiciants", x = "election")

##### logit model 1: demographic variables #####
fr0212$vote_fn <- as.factor(fr0212$vote_fn)
logit1 <- glm(data = fr0212, vote_fn ~ age + men + factor(election), 
              family = binomial)
summary(logit1)

# calculating pseudo r2 Mc Fadden's
LLK_Full <- logLik(logit1)[1]
LLK_Int <- logLik(glm(vote_fn ~ 1, data = fr0212, family = binomial))[1]

mf <- 1 - (LLK_Full / LLK_Int)

# Mc Fadden's Adjusted
mfadjusted <- 1 - ((LLK_Full - 5) / LLK_Int)

# saving coefficients
beta1 <- logit1$coefficients

# saving confidence intervals
int1 <- confint(logit1)

# converting from log odd to predicted probabilities
beta1 <- exp(beta1) / (1 + exp(beta1))
int1 <- exp(int1) / (1 + exp(int1))

# calculating standard deviation from the confidence intervals 
std1 <- (int1[,2] - beta1) / 1.96

## ploting model in predicted probabilities
coefplot(beta1,
         std1,
         varnames = c("Intercept", "Age", "Men", "2007's elec", "2012's elec"),
         main = "Logit model 1: probability of voting in the FN",
         cex.var = 0.8,
         cex.pts = 1.5,
         mar = c(1, 0, 5.1, 2),
         var.las = 2)
abline(v = 0.5)

# dot-and-whisker plot of logit1 model, with coefficient's log odds
l1 <- dwplot(logit1,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  ggtitle("Logit model 1: demographic variables") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"))

##### logit model 2: sociodemographic variables #####
logit2 <- glm(data = fr0212, vote_fn ~ education + household + unemployed + 
                blue_collar + rural + factor(election), family = binomial)
summary(logit2)

# calculating pseudo r2 Mc Fadden's
LLK_Full2 <- logLik(logit2)[1]
LLK_Int2 <- logLik(glm(vote_fn ~ 1, data = fr0212, family = binomial))[1]

mf2 <- 1 - (LLK_Full2 / LLK_Int2)

# Mc Fadden's Adjusted
mf2adjusted <- 1 - ((LLK_Full2 - 5) / LLK_Int2)

# dot and whisker of the coefficients
dwplot(logit2, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))

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
         varnames = c("Intercept", "Unemployed", "Blue-collar", "Rural inh", 
                      "election 2007", "election 2012"),
         main = "Logit model 2: probability of voting in the FN",
         cex.var = 0.8,
         cex.pts = 1.5,
         mar = c(1, 0, 5.1, 2),
         var.las = 2)
abline(v = 0.5)

##### logit model 3: political variables #####
fr0212$diss_dem <- as.numeric(fr0212$diss_dem)
logit3 <- glm(data = fr0212, vote_fn ~ lrself + gov_perform + diss_dem + 
                diss_econ + trust_pol + factor(election), family = binomial)
summary(logit3)

# Mc Fadden's
LLK_Full3 <- logLik(logit3)[1]
LLK_Int3 <- logLik(glm(vote_fn ~ 1, data = fr0212, family = binomial))[1]

mf3 <- 1 - (LLK_Full3/ LLK_Int3)

# Mc Fadden's Adjusted
mf3adjusted <- 1 - ((LLK_Full3 - 4) / LLK_Int3)

# plotting model 3's coefficients
dwplot(logit3,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))

# saving coefficients
beta3 <- logit3$coefficients

# saving confidence intervals
int3 <- confint(logit3)

# converting from log odd to predicted probabilities
beta3 <- exp(beta3) / (1 + exp(beta3))
int3 <- exp(int3) / (1 + exp(int3))

# calculating standard deviation from the confidence intervals 
std3 <- (int3[,2] - beta3) / 1.96

# ploting logit 3 in predicted probabilities
coefplot(beta3,
         std3,
         varnames = c("Intercept", "LRscale", "Satisf gov", "Satisf dem", 
                      "Satisf econ", "Trust polity", "election 2007", 
                      "election 2012"),
         main = "Logit model 3: probability of voting in the FN",
         cex.var = 0.8,
         cex.pts = 1.5,
         mar = c(1, 0, 5.1, 2),
         var.las = 2)
abline(v = 0.5)

##### model 4 with all the variables #####
logitgen <- glm(data = fr0212, vote_fn ~ age + men + education + household + 
                  unemployed + blue_collar + rural + lrself + 
                  gov_perform + diss_dem + diss_econ + trust_pol + factor(election),
                family = binomial)
summary(logitgen)

# Mc Fadden's
LLK_Full4 <- logLik(logitgen)[1]
LLK_Int4 <- logLik(glm(vote_fn ~ 1, data = fr0212, family = binomial))[1]

mf4 <- 1 - (LLK_Full4 / LLK_Int4)

# Mc Fadden's Adjusted
mf4adj <- 1 - ((LLK_Full4 - 4) / LLK_Int4)

# dot whisker of model 4's coefficients
dwplot(logitgen,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))

# dot and whisker plot comparing the coefficients for all the four models
all <- dwplot(list(logit1, logit2, logit3, logitgen), 
              vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  theme_classic() +
  ggtitle("Logit models - Dependent variable: Vote in the FN") +
  labs(x = "Coefficients", caption = "Figure 6 - Author's ellaboration") +
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "right",
        legend.title.align = .5) + 
  scale_color_discrete(name = "Models")

# saving and exporting plot
ggsave("allmodels.png", plot = all, device = "png", width = 6, height = 5)

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
coefplot(betagen[2:15],
         stdgen[2: 15],
         varnames = c("Age", "Men", "Educ", "Income", "Unemp",
                      "Blue Col", "Rural", "LR Self", "Satisf gov",
                      "Satisf dem", "Satisf econ", "Trust Pol", 
                      "2007", "2012"),
         main = "Predicted probability of voting in the FN (data from model 4)",
         xlab = "Probability", lwd = 2, cex.lab = 1, cex.axis = 1,
         sub = "Figure 7 - Author's ellaboration",
         cex.var = 0.8,
         cex.pts = 1.2,
         cex.main = 1,
         mar = c(1, 0, 5, 2),
         var.las = 2)
abline(v = 0.5, lty = 2)

##### residuals #####
# saving residuals, predicted and fitted values of model 4
genres <- resid(logitgen)
genfit <- fitted(logitgen)
genpred <- predict(logitgen)

# plot residuals vs fitted values
fitres <- ggplot(logitgen, aes(genfit, genres)) +
  geom_point() +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Fitted vs residual plot - Model 4") +
  labs(x = "Fitted values", y = "Residuals") +
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"))

# plot predicted vs residuals
predres <- ggplot(logitgen, aes(genpred, genres)) +
  geom_point() +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Predicted vs residual plot - Model 4") +
  labs(x = "Predicted values", y = "Residuals") +
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"))

# saving and exporting plots
ggsave("fitted_residuals.png", plot = fitres, device = "png", width = 8, height = 5)
ggsave("predict_residuals.png", plot = predres, device = "png", width = 8, height = 5)  

# standardized residuals of the observations. Looking for outliers and influential
# observation

# computing the standardized residuals and cook's distance
model.data <- augment(logitgen) %>% mutate(index = 1:n())

# function to name the top 3 largest values
model.data %>% top_n(3, .cooksd)

# ploting std. residuals
stdresid <- ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = vote_fn), alpha = .5) +
  theme_classic() +
  ggtitle("Observations vs. Standardized residuals - Model 4") +
  labs(x = "Index", y = "Standardized residuals", 
       caption = "Figure 8 - Author's elaboration") + 
  theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "right",
        legend.title.align = .5) + 
  scale_color_discrete(name = "Vote in the FN")

# saving and exporting plot
ggsave("obs_vs_stdresid.png", plot = stdresid, device = "png", width = 6, 
       height = 4)

# vif
vifgen <- vif(logitgen)

stargazer(vifgen[, 1], 
          title = "VIF model 4",
          covariate.labels = c("Age", "Men", "Educ", "Income", "Unemployed",
          "Blue Col", "Rural inhab", "LR Self", 
          "Satisf gov","Satisf democracy", "Satisf econ", 
          "Trust Politicians"),
          type = "html", align = T, out = "vifgen.htm")

##### table with coefficients of model 4 (remember to open the file) #####
stargazer(logit1, logit2, logit3, logitgen, title = "Results",
          dep.var.labels = "Vote in FN",
          covariate.labels = c("Age", "Men", "Educ", "Income", "Unemployed",
                               "Blue Col", "Rural inhab", "LR Self", 
                               "Satisf gov","Satisf democracy", "Satisf econ", 
                               "Trust Politicians", "Election 2007", 
                               "Election 2012"),
          type = "html", align = TRUE, out = "modelstest.htm")
