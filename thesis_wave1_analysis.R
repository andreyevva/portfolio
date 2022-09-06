setwd("/Users/pirozhok/Desktop/OneDrive - Central European University/MATHESIS/data")
library(haven)
nastya_survey <- read_dta("thesis_wave1_nastya_survey.dta")
summary(nastya_survey$cc1)
climate <- subset(nastya_survey, subset = (non_secession_split == "nastya"))
climate_loc <- subset(nastya_survey, subset=(non_secession_split == "nastya") & 
                        (nastya_survey$nastya_experiment == "local"))
climate_glob <- subset(nastya_survey, subset=(non_secession_split == "nastya") & 
                        (nastya_survey$nastya_experiment == "global"))
summary(climate_loc$nastya_mc)

## looking at the manipulation check:
treat_local <- table(climate_loc$nastya_mc)
barplot(treat_local,
        main = "Manipulation check (local)",
        names.arg = c("Local", "Global", "Both", "Not sure"))

treat_global <- table(climate_glob$nastya_mc)
barplot(treat_global,
        main = "Manipulation check (Global)",
        names.arg = c("Local", "Global", "Both", "Not sure"))

## plotting party
summary(climate$pid1)
summary(climate$pidd)
t_pid <- table(climate$pid1)
barplot(t_pid,
        main = "Party Identity",
        names.arg = c("Rep", "Dem", "Ind", "Smth else"))

## plotting cc concern
summary(climate$cca2)
t_ccconc <- table(climate$cca2)
barplot(t_ccconc,
        main = "How worried are you about CC?",
        names.arg = c("Not at all", "Not very", "Somewhat", "Very", "Extremely"))

## coding treatment variable as numeric binary:
summary(climate$nastya_experiment)
climate$n_treatment <- 0
climate$n_treatment <- ifelse(grepl(pattern = "global", climate$nastya_experiment), "1", "0")
print(climate$n_treatment)

## tried it with loop first but here's a bug here somewhere (dont run!!!):
climate$n_treatment = rep(length(climate$nastya_experiment))
for(i in 1:length(climate$nastya_experiment)){
  if (climate$nastya_experiment[i] == "local"){
    climate$n_treatment[i] == 1
  }
  else if (climate$nastya_experiment[i] == "global"){
    climate$n_treatment[i] == 2
  }
}
summary(climate$n_treatment)
print(climate$n_treatment)

## coding outcome (policy pref) vars as 0 and 1 (for log reg):
climate$energy = climate$cc1 - 1

climate$tax_corp = climate$cc2 - 1

climate$enviprotec = climate$cc3 - 1

climate$tax_house = climate$cc4 - 1

climate$tax_corp1 = climate$cc5 - 1

## frequency tables for outcome variables
energy <- table(climate$energy)
barplot(energy,
        main = "Development of alternative sources of energy should be a priority in addressing America’s energy supply",
        names.arg = c("Support", "Oppose"))

tax_corp <- table(climate$tax_corp)
barplot(tax_corp,
        main = "Corporations should be taxed based on their carbon emissions in order to incentivize them to reduce pollution.",
        names.arg = c("Support", "Oppose"))

enviprotec <- table(climate$enviprotec)
barplot(enviprotec,
        main = "The government should take more active steps in the field of environmental protection ",
        names.arg = c("Support", "Oppose"))

tax_house <- table(climate$tax_house)
barplot(tax_house,
        main = "Households should be taxed based on their carbon emissions in order to incentivize individuals to reduce pollution.",
        names.arg = c("Support", "Oppose"))

tax_corp1 <- table(climate$tax_corp1)
barplot(tax_corp1,
        main = "Corporations should be taxed based on their carbon emissions in order to incentivize them to reduce pollution.",
        names.arg = c("Support", "Oppose"))

## dummy for democrats:
summary(climate$pid1)
climate$dem <- 0
climate$dem[climate$pid1 == 2] <- 1
climate$dem[climate$pid1 != 2] <- 0
climate$dem

## logistic regression:
log_energy <- glm(energy ~ n_treatment + cca2 + dem, data = climate, family = "binomial")
summary(log_energy)

log_taxcorp <- glm(tax_corp ~ n_treatment + cca2 + dem, data = climate, family = "binomial")
summary(log_taxcorp)

log_enviprotec <- glm(enviprotec ~ n_treatment + cca2 + dem, data = climate, family = "binomial")
summary(log_enviprotec)

log_tax_house <- glm(tax_house ~ n_treatment + dem + cca2, data = climate, family = "binomial")
summary(log_tax_house)

log_taxcorp1 <- glm(tax_corp1 ~ n_treatment, data = climate, family = "binomial")
summary(log_taxcorp1)

## predicted probabilities:
clim_prob <- with(climate, data.frame(energy = mean(energy),
                                      tax_corp = mean(tax_corp),
                                      enviprotec = mean(enviprotec),
                                      tax_house = mean(tax_house),
                                      dem = mean(dem),
                                      cca2 = mean(cca2),
                                      n_treatment = factor(0:1)))
clim_prob

clim_prob$treatPen <- predict(log_energy, newdata = clim_prob, type = "response")
clim_prob
## ^^ doesnt make sense bc there is no point in predicting the assignment to treatment...
## but how to put 5 regressions into 1 predicted probability model??

## individual predicted probabilities:
newdata <- data.frame(n_treatement = 1, cca2 = 5, dem = 1)
predict(log_taxcorp, newdata, type = "response")

