setwd("/Users/pirozhok/Desktop/OneDrive - Central European University/MATHESIS/data")
library(haven)
lucid_2021_dec <- read_dta("thesis_pilot_lucid_2021_dec.dta")

dim(lucid_2021_dec)
names(lucid_2021_dec)
print(lucid_2021_dec$nastya_experiment)
summary(lucid_2021_dec$nastya_experiment)
print(lucid_2021_dec$nastya_mc)
print(lucid_2021_dec$non_secession_split)

climate <- subset(lucid_2021_dec, subset = (non_secession_split == "nastya"))
climate_loc <- subset(lucid_2021_dec, subset=(non_secession_split == "nastya") & 
                        (nastya_experiment == "local"))
climate_glob <- subset(lucid_2021_dec, subset=(non_secession_split == "nastya") & 
                         (nastya_experiment == "global"))

# female dummies
climate$female[climate$gender == "2"] <- 1
climate$female[climate$gender != "2"] <- 0
climate_loc$female[climate_loc$gender == "2"] <- 1
climate_loc$female[climate_loc$gender != "2"] <- 0
climate_glob$female[climate_glob$gender == "2"] <- 1
climate_glob$female[climate_glob$gender != "2"] <- 0
# pid dummy
library(tidyverse)
climate_loc$pid1 <- as.numeric(climate_loc$pid1)
climate_glob$pid1 <- as.numeric(climate_glob$pid1)
climate_loc$rep[climate_loc$pid1 == 1] <- 1
climate_loc$rep[climate_loc$pid1 != 1] <- 0
climate_glob$rep[climate_glob$pid1 == 1] <- 1
climate_glob$rep[climate_glob$pid1 != 1] <- 0
climate_loc$dem[climate_loc$pid1 == 2] <- 1
climate_loc$dem[climate_loc$pid1 != 2] <- 0
climate_glob$dem[climate_glob$pid1 == 2] <- 1
climate_glob$dem[climate_glob$pid1 != 2] <- 0
climate_loc$ind[climate_loc$pid1 == 3] <- 1
climate_loc$ind[climate_loc$pid1 != 3] <- 0
climate_glob$ind[climate_glob$pid1 == 3] <- 1
climate_glob$ind[climate_glob$pid1 != 3] <- 0

climate$rep[climate$pid1 == 1] <- 1
climate$rep[climate$pid1 != 1] <- 0
climate$dem[climate$pid1 == 2] <- 1
climate$dem[climate$pid1 != 2] <- 0
climate$ind[climate$pid1 == 3] <- 1
climate$ind[climate$pid1 != 3] <- 0
# converting the age into numeric
climate$age <- as.numeric(climate$age)
summary(climate$age)
descr <- cbind(climate$age, climate$female, climate$rep, climate$dem, climate$ind)
descr <- as.data.frame(descr)
names(descr) <- c("age", "female", "rep", "dem", "ind")
summary(descr$age)
climate_loc$age <- as.numeric(climate_loc$age)
summary(climate_loc$age)
climate_glob$age <- as.numeric(climate_glob$age)
summary(climate_glob$age)
fem_loc <- table(climate_loc$female)
fem_loc
543/(1018/100)
fem_glob <- table(climate_glob$female)
fem_glob
540/(1020/100)
dem_loc <- table(climate_loc$dem)
dem_loc
366/(1018/100)
rep_loc <- table(climate_loc$rep)
rep_loc
311/(1018/100)
ind_loc <- table(climate_loc$ind)
ind_loc
285/(1018/100)
dem_glob <- table(climate_glob$dem)
dem_glob
370/(1020/100)
rep_glob <- table(climate_glob$rep)
rep_glob
278/(1020/100)
ind_glob <- table(climate_glob$ind)
ind_glob
303/(1020/100)

## looking at the manipulation check: ----
summary(climate_loc$nastya_mc)
dim(climate_loc$nastya_mc)
print(climate_loc$nastya_mc)
treat_local <- table(climate_loc$nastya_mc)
treat_local
barplot(treat_local,
        main = "Manipulation check (Local)",
        names.arg = c("Local", "Global", "Both", "Not sure"))

treat_global <- table(climate_glob$nastya_mc)
barplot(treat_global,
        main = "Manipulation check (Global)",
        names.arg = c("Local", "Global", "Both", "Not sure"))

# correlation matrix for DVs
corr <- cbind(climate$nastya_outcome_1, climate$nastya_outcome_2,
              climate$nastya_outcome_3, climate$nastya_outcome_4,
              climate$nastya_outcome_5, climate$nastya_outcome_6)
corr <- as.data.frame(corr)
colnames(corr) <- c("require renewable", "renewable - expensive", "corp tax", "household tax", "urgent policy", "long-term policy")
library(ggcorrplot)
ggcorrplot(cor(corr, use = "pairwise.complete.obs"))


# plotting party????????????? ----
head(lucid_2021_dec$pid1)
t_pid <- table(climate$pid1)
barplot(t_pid,
        main = "Party Identity",
        names.arg = c("Rep", "Dem", "Ind", "Smth else"))

# PID ----
## generating dummies for republicans, democrats and independents
freq_pid <- table(climate$pid1)
freq_pid

loc_dem <- subset(climate_loc, subset = (dem == 1))
glob_dem <- subset(climate_glob, subset = (dem == 1))
loc_rep <- subset(climate_loc, subset = (rep == 1))
glob_rep <- subset(climate_glob, subset = (rep == 1))


# difference in means DEMOCRATS:
avg.means_ts_pid <- c((mean(loc_dem$nastya_outcome_1, na.rm = TRUE) - mean(glob_dem$nastya_outcome_1,
                                                                           na.rm = TRUE)),
                  (mean(loc_dem$nastya_outcome_2, na.rm = TRUE) - mean(glob_dem$nastya_outcome_2,
                                                                           na.rm = TRUE)),
                  (mean(loc_dem$nastya_outcome_3, na.rm = TRUE) - mean(glob_dem$nastya_outcome_3,
                                                                           na.rm = TRUE)),
                  (mean(loc_dem$nastya_outcome_4, na.rm = TRUE) - mean(glob_dem$nastya_outcome_4,
                                                                           na.rm = TRUE)),
                  (mean(loc_dem$nastya_outcome_5, na.rm = TRUE) - mean(glob_dem$nastya_outcome_5,
                                                                           na.rm = TRUE)),
                  (mean(loc_dem$nastya_outcome_6, na.rm = TRUE) - mean(glob_dem$nastya_outcome_6,
                                                                           na.rm = TRUE)))
show(avg.means_ts_pid)
install.packages("rstatix")
library(rstatix)
loc_dem %>% get_summary_stats(nastya_outcome_1, nastya_outcome_2, 
                                  nastya_outcome_3, nastya_outcome_4, 
                                  nastya_outcome_5, nastya_outcome_6, type = "mean_sd")
glob_dem %>% get_summary_stats(nastya_outcome_1, nastya_outcome_2, 
                                  nastya_outcome_3, nastya_outcome_4, 
                                  nastya_outcome_5, nastya_outcome_6, type = "mean_sd")
t.test(climate_loc$nastya_outcome_1,climate_glob$nastya_outcome_1)
t.test(climate_loc$nastya_outcome_2,climate_glob$nastya_outcome_2)
t.test(climate_loc$nastya_outcome_3,climate_glob$nastya_outcome_3)
t.test(climate_loc$nastya_outcome_4,climate_glob$nastya_outcome_4)
t.test(climate_loc$nastya_outcome_5,climate_glob$nastya_outcome_5)
t.test(climate_loc$nastya_outcome_6,climate_glob$nastya_outcome_6)
# t-test for treatments among democrats:
t.test(loc_dem$nastya_outcome_1,glob_dem$nastya_outcome_1) 
t.test(loc_dem$nastya_outcome_2,glob_dem$nastya_outcome_2)
t.test(loc_dem$nastya_outcome_3,glob_dem$nastya_outcome_3)
t.test(loc_dem$nastya_outcome_4,glob_dem$nastya_outcome_4)
t.test(loc_dem$nastya_outcome_5,glob_dem$nastya_outcome_5)
t.test(loc_dem$nastya_outcome_6,glob_dem$nastya_outcome_6)

t.test(loc_rep$nastya_outcome_1,glob_rep$nastya_outcome_1)
t.test(loc_rep$nastya_outcome_2,glob_rep$nastya_outcome_2)
t.test(loc_rep$nastya_outcome_3,glob_rep$nastya_outcome_3)
t.test(loc_rep$nastya_outcome_4,glob_rep$nastya_outcome_4)
t.test(loc_rep$nastya_outcome_5,glob_rep$nastya_outcome_5)
t.test(loc_rep$nastya_outcome_6,glob_rep$nastya_outcome_6)

# t-test for differences across democrats and republicans:
t.test(loc_rep$nastya_outcome_1,loc_dem$nastya_outcome_1) #TINY


# plotting outcome variables 
# TWO BARPLOTS FOR TWO TREATMENT GROUPS ----
head(climate$nastya_outcome_1)
loc_pref_o1 <- table(climate_loc$nastya_outcome_1)
loc_pref_o2 <- table(climate_loc$nastya_outcome_2)
loc_pref_o3 <- table(climate_loc$nastya_outcome_3)
loc_pref_o4 <- table(climate_loc$nastya_outcome_4)
loc_pref_o5 <- table(climate_loc$nastya_outcome_5)
loc_pref_o6 <- table(climate_loc$nastya_outcome_6)

## i want 6 of those to be plotted side by side with each other - how??
barplot(loc_pref_o1,
        main = "Policy preference (local treatment) - my state renewables")

## this is f-ing how!!!:

loc_tr_out <- cbind(loc_pref_o1, loc_pref_o2, loc_pref_o3,
                    loc_pref_o4, loc_pref_o5, loc_pref_o6)

barplot(loc_tr_out,
        xlab = "Policy support (1 - Strongly agree, 5 - Strongly disagree)",
        ylab = "Frequency",
        axisnames = TRUE,
        names.arg = c("More renewables", "More fossil fuels", "Tax corp", "Tax household", "Mitigate now", "Focus on long-term"),
        col = c("chartreuse4", "chartreuse3", "chartreuse2", "chartreuse1", "chartreuse"),
        main = "Policy Support (local treatment)",
        beside = TRUE)

glob_pref_o1 <- table(climate_glob$nastya_outcome_1)
glob_pref_o2 <- table(climate_glob$nastya_outcome_2)
glob_pref_o3 <- table(climate_glob$nastya_outcome_3)
glob_pref_o4 <- table(climate_glob$nastya_outcome_4)
glob_pref_o5 <- table(climate_glob$nastya_outcome_5)
glob_pref_o6 <- table(climate_glob$nastya_outcome_6)

glob_tr_out <- cbind(glob_pref_o1, glob_pref_o2, glob_pref_o3,
                     glob_pref_o4, glob_pref_o5, glob_pref_o6)

barplot(glob_tr_out,
        xlab = "Policy support (1 - Strongly agree, 5 - Strongly disagree)",
        ylab = "Frequency",
        main = "Policy Support (global treatment)",
        names.arg = c("More renewables", "More fossil fuels", "Tax corp", "Tax household", "Mitigate now", "Focus on long-term"),
        col = c("darkolivegreen", "darkolivegreen4", "darkolivegreen3", "darkolivegreen2", "darkolivegreen1"),
        beside = TRUE)

## BOXPLOTS FOR DIFFERENT GROUPS: ----
head(climate$nastya_experiment)
class(climate$nastya_experiment)
set.seed(100)
Y_range <- range(climate$nastya_outcome_1, climate$nastya_outcome_2,
                 climate$nastya_outcome_3, climate$nastya_outcome_4,
                 climate$nastya_outcome_5, climate$nastya_outcome_6, na.rm = TRUE)
par(mfrow=c(2,3))
boxplot(nastya_outcome_1 ~ nastya_experiment,
        data = climate,
        ylim = Y_range,
        ylab = "My state - renewables",
        xlab = NA,
        cex.lab = 1.6,
        col = "darkolivegreen1")
boxplot(nastya_outcome_2 ~ nastya_experiment,
        data = climate,
        ylim = Y_range,
        ylab = "Renewables are expensive",
        xlab = NA,
        cex.lab = 1.6,
        col = "darkorange")
boxplot(nastya_outcome_3 ~ nastya_experiment,
        data = climate,
        ylim = Y_range,
        ylab = "Corporation tax",
        xlab = NA,
        cex.lab = 1.6,
        col = "deepskyblue2")
boxplot(nastya_outcome_4 ~ nastya_experiment,
        data = climate,
        ylim = Y_range,
        ylab = "Household tax",
        xlab = NA,
        cex.lab = 1.6,
        col = "gold2")
boxplot(nastya_outcome_5 ~ nastya_experiment,
        data = climate,
        ylim = Y_range,
        ylab = "Urgent mitigation policies",
        xlab = NA,
        cex.lab = 1.6,
        col = "brown1")
boxplot(nastya_outcome_6 ~ nastya_experiment,
        data = climate,
        ylim = Y_range,
        ylab = "Focus on long-term policies",
        xlab = NA,
        cex.lab = 1.6,
        col = "darkorchid3")
par(mfrow=c(1,1))

# correlation structure ----
cor(climate$nastya_outcome_1, climate$nastya_outcome_2, use = "complete.obs")
cor(climate$nastya_outcome_3, climate$nastya_outcome_4, use = "complete.obs")
cor(climate$nastya_outcome_5, climate$nastya_outcome_6, use = "complete.obs")

cor(climate_loc$nastya_outcome_1, climate_loc$nastya_outcome_2, use = "complete.obs")
cor(climate_glob$nastya_outcome_3, climate_glob$nastya_outcome_4, use = "complete.obs")

# difference in means:
avg.means_ts <- c((mean(climate_loc$nastya_outcome_1, na.rm = TRUE) - mean(climate_glob$nastya_outcome_1,
                                                                           na.rm = TRUE)),
                   (mean(climate_loc$nastya_outcome_2, na.rm = TRUE) - mean(climate_glob$nastya_outcome_2,
                                                                           na.rm = TRUE)),
                   (mean(climate_loc$nastya_outcome_3, na.rm = TRUE) - mean(climate_glob$nastya_outcome_3,
                                                                           na.rm = TRUE)),
                   (mean(climate_loc$nastya_outcome_4, na.rm = TRUE) - mean(climate_glob$nastya_outcome_4,
                                                                           na.rm = TRUE)),
                   (mean(climate_loc$nastya_outcome_5, na.rm = TRUE) - mean(climate_glob$nastya_outcome_5,
                                                                           na.rm = TRUE)),
                   (mean(climate_loc$nastya_outcome_6, na.rm = TRUE) - mean(climate_glob$nastya_outcome_6,
                                                                           na.rm = TRUE)))
show(avg.means_ts)



# I have never figured out how to proceed with identification of correlation
# Creating the plot
plot(climate$nastya_outcome_1, climate$nastya_outcome_2, pch = 19, col = "lightblue")
# Regression line
abline(lm(climate$nastya_outcome_1 ~ climate$nastya_outcome_2), col = "red", lwd = 3)
# Pearson correlation
text(paste("Correlation:", round(cor(x, y), 2)), x = 25, y = 95)



# figuring out regressions ----
library(ggeffects)
class(climate$pid1)
climate$pid1 <- as.numeric(climate$pid1)
fit1 <- lm(nastya_outcome_1 ~ age + rep, data = climate)
summary(fit1)
fit <- ggpredict(fit1)
plot(fit)
fit5 <- lm(nastya_outcome_5 ~ age + rep, data = climate)
summary(fit5)
fit6 <- lm(nastya_outcome_6 ~ age + ind, data = climate)
summary(fit6)



# PARTY GGPLOT
summary(climate$pid1)
head(climate$pid1)
library(tidyverse)
climate$pid1 <- as.factor(climate$pid1)
climate$nastya_outcome_1 <- as.numeric(climate$nastya_outcome_1)
climate$nastya_outcome_2 <- as.numeric(climate$nastya_outcome_2)
climate$nastya_outcome_3 <- as.numeric(climate$nastya_outcome_3)
climate$nastya_outcome_5 <- as.numeric(climate$nastya_outcome_5)
climate$nastya_outcome_6 <- as.numeric(climate$nastya_outcome_6)

O1_pid <- ggplot(climate, aes(x=pid1, y=nastya_outcome_1))+
        geom_boxplot(aes(color = pid1)) +
        ggtitle('Support renewables in my state even if prices increase a little') +
        scale_x_discrete(labels = c('Rep','Dem','Ind', 'Smth Else')) +
        scale_color_manual(values = c('#bb0b00', '#0052bb', '#E7B800', '#808080')) +
        labs(subtitle = "1 - strongly agree, 5 - strongly disagree",
             x = "Party ID", y = "Climate action now, even if costly")
O1_pid

O2_pid <- ggplot(climate, aes(x=pid1, y=nastya_outcome_2))+
        geom_boxplot(aes(color = pid1)) +
        ggtitle('Switching to renewables - expensive; govt should focus on fossil fuels') +
        scale_x_discrete(labels = c('Rep','Dem','Ind', 'Smth Else')) +
        scale_color_manual(values = c('#bb0b00', '#0052bb', '#E7B800', '#808080')) +
        labs(subtitle = "1 - strongly agree, 5 - strongly disagree",
             x = "Party ID", y = "Climate action now, even if costly")
O2_pid

O3_pid <- ggplot(climate, aes(x=pid1, y=nastya_outcome_3)) +
        geom_boxplot(aes(color = pid1)) +
        ggtitle('Tax corporations based on emissions') +
        scale_x_discrete(labels = c('Rep','Dem','Ind', 'Smth Else')) +
        scale_color_manual(values = c('#bb0b00', '#0052bb', '#E7B800', '#808080')) +
        labs(subtitle = "1 - strongly agree, 5 - strongly disagree",
             x = "Party ID", y = "Climate action now, even if costly")
O3_pid

O5_pid <- ggplot(climate, aes(x=pid1, y=nastya_outcome_5)) +
        geom_boxplot(aes(color = Party_ID)) +
        ggtitle('Immediate climate action preference') +
        scale_x_discrete(labels = c('Rep','Dem','Ind', 'Smth Else')) +
        scale_color_manual(values = c('#bb0b00', '#0052bb', '#E7B800', '#808080')) +
        labs(subtitle = "1 - strongly agree, 5 - strongly disagree",
             x = "Party ID", y = "Climate action now, even if costly")
O5_pid

O6_pid <- ggplot(viz56, aes(x=pid1, y=nastya_outcome_6)) +
        geom_boxplot(aes(color = Party_ID)) +
        ggtitle('Long-term climate policy preference') +
        scale_x_discrete(labels = c('Rep','Dem','Ind', 'Smth Else')) + 
        scale_color_manual(values = c('#bb0b00', '#0052bb', '#E7B800', '#808080')) +
        labs(subtitle = "1 - strongly agree, 5 - strongly disagree",
             x = "Party ID", y = "Gov't should focus on long-term effects of CC")
O6_pid

# ANOVA
o1_aov <- aov(nastya_outcome_1 ~ nastya_experiment+pid1, data = climate)
summary(o1_aov)
o2_aov <- aov(nastya_outcome_2 ~ nastya_experiment+pid1, data = climate)
summary(o2_aov)
o3_aov <- aov(nastya_outcome_3 ~ nastya_experiment+pid1, data = climate)
summary(o3_aov)
o4_aov <- aov(nastya_outcome_4 ~ nastya_experiment+pid1, data = climate)
summary(o4_aov)
o5_aov <- aov(nastya_outcome_5 ~ nastya_experiment+pid1, data = climate)
summary(o5_aov)
o6_aov <- aov(nastya_outcome_6 ~ nastya_experiment+pid1, data = climate)
summary(o6_aov)








