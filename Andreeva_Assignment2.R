setwd("/Users/pirozhok/Desktop/OneDrive - Central European University/FALL2020/intermeditequants/Assignment 2")
CCES19_assignment <- read.csv("/Users/pirozhok/Desktop/OneDrive - Central European University/FALL2020/intermeditequants/Assignment 2/CCES19_assignment.csv")
" In order to optimize futher work with the data, I will omit the missing values from the dataset:"
CCES19_assignment = na.omit(CCES19_assignment)
## Exercise 1.1 ----
"Create an index of sexism, racism and immigration preferences using the corresponding survey 
items. Describe their distribution. To do so, identify which variables can be used to measure 
sexism, racism, and immigration, respectively. Then, calculate the index by averaging these 
variables. Be careful here! The scales need to be the same before you average. This means that 
if for one variable the maximum denotes a given attitude and for another, the minimum denotes 
that attitude, you need to reverse one of the scales scale."
# Sexism ----
"
To create an index of respondents' preferences in regards to sexism I am going to use two variables.
The first is `r summary(CCES19_assignment$CC19_343d)`. 
This variable demonstrates the attitude of people towards the statement 'Feminists make reasonable
demands' on a scale from 1 to 5, where 1 - strongly agree and 5 - strongly disagree.
The second variable is `r summary(CCES19_assignment$CC19_343c)`. Here, the respondents indicated
their attitude towards the statement: 'Women complain about being discriminated' on the same 1
to 5 scale.
Those statements are polar opposites to each other, so, in order to generate the index, I will
flip one of the variables - CC19_343d - the one that indicated that feminists' claims are fair.
So, in the final index of attitudes towards sexism 1 is going to indicate a strong sexist 
attitude and 5 is going to indicate strong non-sexist attitude. 
"
CCES19_assignment$CC19_343d_flip = rep(length(CCES19_assignment$CC19_343d))
for(i in 1:length(CCES19_assignment$CC19_343d)){
  if (is.na(CCES19_assignment$CC19_343d[i]) == FALSE){
    if (CCES19_assignment$CC19_343d[i] == 1){
      CCES19_assignment$CC19_343d_flip[i] = 5
    }
    else if (CCES19_assignment$CC19_343d[i] == 2){
      CCES19_assignment$CC19_343d_flip[i] = 4
    }
    else if (CCES19_assignment$CC19_343d[i] == 3){
      CCES19_assignment$CC19_343d_flip[i] = 3
    }
    else if (CCES19_assignment$CC19_343d[i] == 4){
      CCES19_assignment$CC19_343d_flip[i] = 2
    }
    else if (CCES19_assignment$CC19_343d[i] == 5){
      CCES19_assignment$CC19_343d_flip[i] = 1
    }
  }
}
" Now that one of the variables is flipped and scores of both of the variables are comparable, 
I will create a new variable - sexism, which is going to indicate the mean of two analyzed 
variables."
CCES19_assignment$sexism = rep(length(CCES19_assignment$CC19_343d))
for(i in 1:length(CCES19_assignment$sexism)){
    CCES19_assignment$sexism[i] = mean(CCES19_assignment$CC19_343c[i],
                                       CCES19_assignment$CC19_343d_flip[i])
  }

sexismindex<-table(CCES19_assignment$sexism)
barplot(sexismindex, ylim=c(0,6000), 
     main="Attitudes To Sexism - Score Distribution",
     xlab="Score (1 - strongly approve sexism, 5 - strongly disapprove sexism)",
     cex.lab = 1, 
     cex.axis = 1,
     col="salmon",
     border=NA)
"
We can see that most respondents do not have any strong feelings about sexist statements (as most
people chose the middle score - 3, which was framed as 'Neither agree nor disagree'.
Though, slightly more people tend to approve, rather than to disapprove them - the frequency
of scores 1 and 2 are notably higher than the frequency of 4 and 5."

# Racism ----
"
I am going to follow the similar steps in creating the index for racism. Again, I am using two
variables. The first one - `r summary(CCES19_assignment$CC19_343a)`, which indicates the respondents
attitude toward the following statement: 'White people have certain advantages' (1 - strongly agree,
5 - strongly disagree).
The second variable that I am going to use for this index is `r summary(CCES19_assignment$CC19_343b)` - 
'Racial problems are rare, isolated'. I am going to flip the values of the first variable,
so it is going to be comparable with the values of the second variable."
CCES19_assignment$CC19_343a_flip = rep(length(CCES19_assignment$CC19_343a))
for(i in 1:length(CCES19_assignment$CC19_343a)){
    if (CCES19_assignment$CC19_343a[i] == 1){
      CCES19_assignment$CC19_343a_flip[i] = 5
    }
    else if (CCES19_assignment$CC19_343a[i] == 2){
      CCES19_assignment$CC19_343a_flip[i] = 4
    }
    else if (CCES19_assignment$CC19_343a[i] == 3){
      CCES19_assignment$CC19_343a_flip[i] = 3
    }
    else if (CCES19_assignment$CC19_343a[i] == 4){
      CCES19_assignment$CC19_343a_flip[i] = 2
    }
    else if (CCES19_assignment$CC19_343a[i] == 5){
      CCES19_assignment$CC19_343a_flip[i] = 1
    }
  }

" The variable is flipped, and now I am going to create a new variable - racism, which is going
to reflect the mean of the two values described above."
CCES19_assignment$racism = rep(length(CCES19_assignment$CC19_343a))
for(i in 1:length(CCES19_assignment$racism)){
    CCES19_assignment$racism[i] = mean(CCES19_assignment$CC19_343b[i],
                                       CCES19_assignment$CC19_343a_flip[i])
  }
" Now let's take a look at the distribution"
racismindex<-table(CCES19_assignment$racism)
barplot(racismindex, ylim=c(0,6000), 
        main="Attitudes To Racism - Score Distribution",
        xlab="Score (1 - strongly approve of racism, 5 - strongly disapprove of racism)",
        cex.lab = 1, 
        cex.axis = 1,
        col="paleturquoise",
        border=NA)
" We can see that most of the respondents tend to be disapproved of racism. Actually, if we take
a look at the summary of the variable `r summary(CCES19_assignment$racism)`, we can see that the
median is exactly 4, whicsh means that exactly half of the respondents are disapprove/strongly
disapprove of racism (the score they chose being 4 and higher). "

# Immigration ----
"
Finally, I will generate a similar index for respondents attitudes towards legal status of 
immigrants. There are quite a few variables that are tackling the issue of immigration, but I am
going to pick just two, that have an opposing meaning, so one of them could be flipped easily to
create an immigration index. Here are the variables that I am going to work with:
1. `r summary(CCES19_assignment$CC19_322c)` - Reduce legal immigration by 50 percent.
2. `r summary(CCES19_assignment$CC19_322d)' - Grant legal status to all illegal immigrants who 
have held jobs and paid taxes for at least 3 years, and not been convicted of any felony crimes.
I am going to flip the second variable.
This index is a little bit different then the two previous ones, because instead of 5-level score,
respondents were could chose one of two options - 'Support' and 'Oppose'.
 "
CCES19_assignment$CC19_322d_flip = rep(length(CCES19_assignment$CC19_322d))
for(i in 1:length(CCES19_assignment$CC19_322d)){
    if (CCES19_assignment$CC19_322d[i] == 1){
      CCES19_assignment$CC19_322d_flip[i] = 2
    }
    else if (CCES19_assignment$CC19_322d[i] == 2){
      CCES19_assignment$CC19_322d_flip[i] = 1
    }
}

CCES19_assignment$immigration = rep(length(CCES19_assignment$CC19_322d))
for(i in 1:length(CCES19_assignment$immigration)){
    CCES19_assignment$immigration[i] = mean(CCES19_assignment$CC19_322c[i],
                                       CCES19_assignment$CC19_322d_flip[i])
}

immigrationindex<-table(CCES19_assignment$immigration)
barplot(immigrationindex, ylim=c(0,12000), 
        main="Attitudes Towards Pro-Immigration Policies",
        cex.lab = 1, 
        cex.axis = 1,
        col="royalblue",
        names.arg=c("Disaprove", "Approve"),
        border=NA)
" We can see that more than half of the respondents approve the pro-immigration policies. 
`r summary(CCES19_assignment$immigration)` We can also see it by looking at the mean of the
variable."
summary(CCES19_assignment$CC19_343d_flip)



## Exercise 1.2 ----
"How does the distribution of sexism vary across men and women? Illustrate the difference by 
plotting the distributions for each category."
sexism.mw<-table(CCES19_assignment$gender, CCES19_assignment$sexism)
barplot(sexism.mw, beside=TRUE, legend=c("male", "female"),
        main="Attitudes To Sexism - Score Distribution (Gender)",
        cex.main=1,
        xlab="Score (1 - strongly approve sexism, 5 - strongly disapprove sexism)",
        ylab="Proportion of gender",
        ylim=c(0,3500),
        col=c("cadetblue", "coral1"))
"Generally, the distribution of sexism disapproval score is rather similar in both men and women - 
most respondents of both genders tend to choose the middle option - neither approve or disapprove
of sexism. On the barplots we can vividly see that more women than men have a strong anti-sexism
position, so the distribution on the right side of the bar plot is higher for women, than for men."
men <- subset(CCES19_assignment, subset = (gender==1))
women <- subset(CCES19_assignment, subset =(gender==2))
summary(men$sexism)
summary(women$sexism)
"We can also see that by looking at the mean, which is a mesure sensitive to extreme values.
Whereas the median is the same for both men and women, but the mean is noticeably higher for women."


## Exercise 1.3 ----
"How does the distribution of racism vary across racial/ethnic groups? How would you interpret 
this? Illustrate the difference by plotting the distributions for each category."
racism.dist<-table(CCES19_assignment$race, CCES19_assignment$racism)
barplot(racism.dist, beside=TRUE, 
        legend=c("white", "black", "hispanic", "asian", "indigenous", "middle eastern", "mixed race", "other"),
        args.legend = list(x ="right", bty="n", inset=c(-0.12,0), xpd = TRUE),
        main="Attitudes Towards Racism - Score Distribution (based on race/ethnicity)",
        cex.main=1.3,
        xlab="Score (1 - strongly approve of racism, 5 - strongly disapprove of racism)",
        ylab="Proportion of race/ethnicity",
        ylim=c(0,3800),
        col=c("bisque", "darkgoldenrod1", "darkorange1", "darkred", "darkseagreen3", "deepskyblue", "darkgreen", "gray"))
" Respondetns of all races and ethnicities tend to be strongly disapproval of racism, rather
than being undecided or to be approve of it, though the distribution is less steep for white people. 
We can also see a significanlty bigger share of people of color in 'Disapprove' and 'Disapprove 
strongly' categories (score 4 and 5), then in approval categories.
Another interesting trend that we can see on the barplot is the distribution of frequencies among
black and hispanic respondents in comparison. As any other respondents, most of them tend to be 
strongly disapprove of racism, but there are significantly more hispanic survey-takers that
do not oppose racism strongly."


## Exercise 1.4 ----
"How does the extent of anti-immigration sentiment vary across states? List the top 5 and 
bottom 5 states and comment on their geographical pattern!"
" For this exercise, I will have to work with 5 variables, that identify the attitudes of
respondents towards immigration.
To begin with, I will have to flip one of the variables - CC19_322a 'Overturn President Trump’s 
order to use $6 billion of defense funds to pay for the construction of a wall.', so the values
of all of the five analyzed variables would have the same meaning: if the respondent is choosing
option 1, it means that their attitude is rather anti-immigrant and if they choose 2, they are
rather pro-immigrant. Another variable, CC19_322d - 'Grant legal status to all illegal immigrants 
who have held jobs and paid taxes for at least 3 years, and not been convicted of any felony 
crimes.' has already been flipped in the exercise 1.1. Values of another three variables are in 
line with the meaning of 1=anti-immigrant attitude, 2=pro-immigrant attitude."
CCES19_assignment$CC19_322a_flip = rep(length(CCES19_assignment$CC19_322a))
for(i in 1:length(CCES19_assignment$CC19_322a)){
    if (CCES19_assignment$CC19_322a[i] == 1){
      CCES19_assignment$CC19_322a_flip[i] = 2
    }
    else if (CCES19_assignment$CC19_322a[i] == 2){
      CCES19_assignment$CC19_322a_flip[i] = 1
    }
  }
" In order to create an overall index, it will be much easier, if the variables are going to be
coded with values 0 and 1, instead of 1 and 2. To keep the code less chunky and make further code
easier, I will only replace the value 2 in each variable with a 0.
So the meaning of the values of the immigration-related variables will be the following:
0 - if the respondent indicates pro-immigrant attitude;
1 - if the respondent indicates anti-immigrant attitude."
for(i in 1:length(CCES19_assignment$CC19_322a_flip)){
  if (CCES19_assignment$CC19_322a_flip[i] == 2){
    CCES19_assignment$CC19_322a_flip[i] = 0
}
}
for(i in 1:length(CCES19_assignment$CC19_322b)){
  if (CCES19_assignment$CC19_322b[i] == 2){
    CCES19_assignment$CC19_322b[i] = 0
  }
}
for(i in 1:length(CCES19_assignment$CC19_322c)){
  if (CCES19_assignment$CC19_322c[i] == 2){
    CCES19_assignment$CC19_322c[i] = 0
  }
}
for(i in 1:length(CCES19_assignment$CC19_322d_flip)){
  if (CCES19_assignment$CC19_322d_flip[i] == 2){
    CCES19_assignment$CC19_322d_flip[i] = 0
  }
}
for(i in 1:length(CCES19_assignment$CC19_322e)){
  if (CCES19_assignment$CC19_322e[i] == 2){
    CCES19_assignment$CC19_322e[i] = 0
  }
}
"Now, I will store the average value of these five variables for each of the observations in a
separate variable:"
a = CCES19_assignment[,c(9, 10, 12, 23, 24)]
CCES19_assignment$migr_avg<-rowMeans(a, na.rm = FALSE, dims = 1)
"Then I am going to calculate the average value for every state:"
install.packages("plyr")
library(plyr)
b = ddply(CCES19_assignment, .(inputstate), summarise, mean(migr_avg))
names(b)[names(b) == "mean(migr_avg)"] <- "avg.immigr"
"I tried using dplyr's arrange() function to print top-5 and bottom-5 states, but it did not
run properly, unfortunately, so I checked the results manually, by opening the data `r view (b)` 
and filtering 'avg.immigr' variable from highest to lowest and vice versa."
" Top-5 states with highest anti-immigrant sentiment: 
1. Idaho
2. South Dakota
3. Mississippi
4. West Virginia
5. Tennessee
Bottom-5 states with lowest anti-immigrant sentiment:
1. Vermont
2. District of Colombia
3. Hawaii
4. Washington
5. New York
Anti-immigrant states are located in the South and Midwest, whereas pro-immigrant states are in
the Northeast and West. Those findings are not suprising at all, as the southern states are widely
known to be historically conservative, whereas northeastern and western states are usually the
most progressive."


## Exercise 1.5 ----
"Calculate the average of sexism by age and gender and compute the gender difference in sexism 
for 4 quartiles of the sample by age. Visualize and interpret your findings!"
"First, I will create the variable age, as the initial dataset only has the variable that
indicates the birth year of the respondents."
CCES19_assignment$age <- 2019 - CCES19_assignment$birthyr
summary(CCES19_assignment$age)
"Now I can see the values of the age quartiles, so I can generate the following variable
indicating that:"
CCES19_assignment$age.quart <- NA
CCES19_assignment$age.quart[CCES19_assignment$age <= 34] <- 1
CCES19_assignment$age.quart[CCES19_assignment$age > 34 & CCES19_assignment$age <= 49.26] <- 2
CCES19_assignment$age.quart[CCES19_assignment$age > 49.26 & CCES19_assignment$age<=63] <- 3
CCES19_assignment$age.quart[CCES19_assignment$age > 63] <- 4
"Then I am going yo calculate the average of sexism by age quartiles and gender:"
c = ddply(CCES19_assignment, .(age.quart, gender), summarise, mean(sexism))
"Finally, I am going to calculate the gender difference for age quartiles (to do that, I will use
cast function, which will efficiently summarize the variables:"
install.packages("reshape2")
library(reshape2)
c_L_to_W = dcast(c, age.quart~gender,sum)
"I am renaming the variables, and it might not seem neccesary, but R treats the names of the
variables 1 and 2 as numbers and does not want to run the further code."
names(c_L_to_W)[names(c_L_to_W) == "1"] <- "male"
names(c_L_to_W)[names(c_L_to_W) == "2"] <- "female"
c_L_to_W$diff = c_L_to_W$female - c_L_to_W$male
head(c_L_to_W$diff)
"By looking at the differences in age quartiles among male and female respondents, we can see 
that the difference in attitudes towards sexism is almost twice as bigger for older
people, than for younger. Also, in all of the age categories women are more disapprove of sexism,
than men. Intrestigly, especially in older women seem to tend to share rather anti-sexist views, 
unlike the men their age. Conversely, among youger people (the first age quartile), the 
difference between men and women is nearly twice as small. So, I can conclude that generally
gender equality values and principles are getting widely picked up among young people - both among
men and women. Whereas, for older generations it is rather more women than men who share that
values.
Now, I will plot the differences between men and women's attutides towards sexism based on their
age."
age.gender <- ddply(CCES19_assignment, .(age, gender), summarise, mean(sexism, na.rm = T))
names(age.gender)[3]<-"mean"
age.gender.m<-subset(age.gender, subset=(gender==1))
age.gender.f<-subset(age.gender, subset=(gender==2))
plot(age.gender.m$age, age.gender.m$mean, col = "white",
     ylim = c(1,5), xlim = c(18,101),
     xlab = "Age", ylab = "Sexism sentiment (1 - strongly approve, 5 - strongly disapprove",
     main = "Sexism variance across men and women")
lines(age.gender.m$age, age.gender.m$mean, col = "navyblue")
lines(age.gender.f$age, age.gender.f$mean, col = "darkred")
text(80, 2.1, "Male", col = "navyblue", cex = 0.75)
text(80, 3.2, "Female", col = "darkred", cex = 0.75)
"The visualization shows us a more detailed overview of score distributions.
Firstly, we can see that there are clear differences between men's and women's attitudes towards
sexism in the age group around 50-70 (women tending oppose sexism strongly). For younger and
older people that difference is not that clear - we see that in that age groups the two lines
are overlapping a lot. Also we can see that young people in general tend to be more anti-sexist
than older people."


## Exercise 1.6 ----
"Use the ddply function to calculate the correlation between sexism and racism by education."
"I am going to calculate correlation between sexism and racism by six groups - levels of education,
where 1 is the lowest level of education - 'Did not graduate from high school', and 6 is the
highest level - 'Has a postgraduate degree'."
d = ddply(CCES19_assignment, .(educ), summarise, cor(sexism, racism, use = "complete.obs"))
names(d)[names(d) == "..1"] <- "sexism_racism_cor"
head(d)
"By looking at the results of applying ddply function, we can see that the correlation between
sexism and racism scores is getting higher with higher level of education, meaning that the
more educated the respondents are, they tend to be less sexist and racist, as in the initial
racism and sexism index the higher values indicated the least sexist and racist attitudes."


## Exercise 2.1 ----
"Recode CC19308a by placing people saying “not sure” in a middle category. Show the results by 
summarizing the proportions in a table and barplot."
"I am going to use the loop to change the values of the variable. Before running the loop,
the analyzing scale represents the following:
1 - Strongly approve
2 - Somewhat approve
3 - Somewhat disapprove
4 - Strongly disapprove
5 - Not sure
I will replace 3 with 4, 4 with 5 and 5 with 3."
summary(CCES19_assignment$CC19_308a)
CCES19_assignment$CC19_308a_repl = rep(length(CCES19_assignment$CC19_308a))
for(i in 1:length(CCES19_assignment$CC19_343a)){
  if (CCES19_assignment$CC19_308a[i] == 1){
    CCES19_assignment$CC19_308a_repl[i] = 1
  }
  else if (CCES19_assignment$CC19_308a[i] == 2){
    CCES19_assignment$CC19_308a_repl[i] = 2
  }
  else if (CCES19_assignment$CC19_308a[i] == 3){
    CCES19_assignment$CC19_308a_repl[i] = 4
  }
  else if (CCES19_assignment$CC19_308a[i] == 4){
    CCES19_assignment$CC19_308a_repl[i] = 5
  }
  else if (CCES19_assignment$CC19_308a[i] == 5){
    CCES19_assignment$CC19_308a_repl[i] = 3
  }
}
summary(CCES19_assignment$CC19_308a_repl)
trump_approv <- table(CCES19_assignment$CC19_308a_repl)
barplot(trump_approv, ylim=c(0,8300), 
        main="Job approval score of President Trump",
        xlab="Score (1 - strongly approve, 5 - strongly disapprove)",
        cex.lab = 1, 
        cex.axis = 1,
        col="darkorange",
        border=NA)
"By looking at the summary of score distribution represented in a barplot above, we can see that
the vast majority of respondents tend to be strongly disapprove of President Trump. Another
interesting thing about this distribution is that a very small amount of respondents are undecided
on the question. Also, we can vividly see the polarization trend - as I already said, most 
respondents are strongly disapprove of Trump, but second most frequent score is 1 - strognly
approve of president Trump. Overall, I can say that the distribution of this score is roughly
'reverse normal' - with extremelly small frequency of scores in the middle, and with very
frequent extreme values."


## Exercise 2.2 ----
"Run a simple regression predicting approval of Trump by anti immigration preferences. Interpret 
your findings!"
migr_trump <- lm(CC19_308a_repl ~ migr_avg, data = CCES19_assignment)
summary(migr_trump)
"Firstly, we can see that the coefficient of anti-immigration sentiment is highly significant - 
there are three stars that indicate that there is only 0.01% chance that the null hypothesis
(in that case that would be that the hugh level of anti-immigrant sentiment can in no way be a 
predictor of high Trump's approval rating) might be true, which is insignificantly small chance,
so we can confidently reject the null hypothesis and state that there is certainly
some relation between high level of anti-immigrant attitude and high level of Trump approval.
Moreover, overall fit of the model, indicated by adjusted R-squared is pretty high - at 50% 
(especially considering that we are doing social science research). It means that 50% of the 
data fit the model. So our regression fits the observed data pretty well.
Finally, let's take a look at the coefficient. 
`r coef(migr_trump)` 
We can see that there is a negative correlation between dependent and independent variable.
It means that one unit increase in anti-immigration sentiment index leads to 5 point decrease in
Trump job approval rating. So the higher the level of anti-immigrant sentiment, the lower would
be the disapproval of Trump (just to recall the job approval score - 1 indicated strog approval
of Trump and 5 indicated strong disapproval).
To put it all in a very simple sentence: we can certainly say that higher level of 
anti-immigration prefernces is a valid predictor of stronger approval of President Trump."


## Exercise 2.3 ----
"Run a regression predicting approval of Trump by racism. Repeat this exercise separately for 
Whites, Blacks and Hispanics. How do the regression coefficients compare? Explain!"
"Running a regression predicting approval of Trump by racism:"
racism_trump <- lm(CC19_308a_repl ~ racism, data = CCES19_assignment)
"Subsetting the data to run subgroup regressions:"
racism_white <- subset(CCES19_assignment, subset = (race==1))
racism_black <- subset(CCES19_assignment, subset = (race==2))
racism_hispanic <- subset(CCES19_assignment, subset = (race==3))
"Running regressions for Whites, Blacks and Hispanics:"
racism_trump_white <- lm(CC19_308a_repl ~ racism, data = racism_white)
racism_trump_black <- lm(CC19_308a_repl ~ racism, data = racism_black)
racism_trump_hispanic <- lm(CC19_308a_repl ~ racism, data = racism_hispanic)
coef(racism_trump)
"By looking at the regression of Trump aproval score and overall racism index, we can see that
higher level of Trump disapproval corresponds with higher level of racism disapproval."
coef(racism_trump_white)
coef(racism_trump_black)
coef(racism_trump_hispanic)
"The coefficient is positive for all of the three subgroups, so the positive relationship
between higher level of Trump disapproval and higher level of racism disapproval prevails for
white, black and hispanic respondents.
Though, we can see that the slope of the regression line for white respondents is much steeper
than for blacks and hispanics. For whites, 1 unit increase in racism approval score, leads to
0.78 unit increase in Trump disapproval. Whereas for hispanics it is only 0.64 unit increase and
it is even lower for blacks - 0.3.
What does it tell us?
For white respondents, anti-racist sentiment is more closely connected to Trump disapproval,
than for blacks and hispanics. This is the only conclusion that I can draw from this observation.
It is important to point out that it does not mean that white respondents tend to be more 
anti-racist or more disapprove of Trump, just for that group the slope of regression coefficient
is steeper. I would assume that the level of racism and Trump disapproval for blacks and 
hispanics there are some others predictors that just were not controlled for in this particular 
model."


## Exercise 2.4 ----
"Run a regression predicting approval of Trump by sexism. Then, run the same regression including 
two indicators for Republicans and Democrats (based on CC19308a). How do your results change? 
Explain!"
sexism_trump <- lm(CC19_308a_repl ~ sexism, data = CCES19_assignment)
summary(sexism_trump)
"Generating dummies for republicans and democrats:"
CCES19_assignment$dem[CCES19_assignment$pid3 == 1] <- 1
CCES19_assignment$dem[CCES19_assignment$pid3 != 1] <- 0
CCES19_assignment$rep[CCES19_assignment$pid3 == 2] <- 1
CCES19_assignment$rep[CCES19_assignment$pid3 != 2] <- 0
"Fitting the model:"
sexism_party = lm(CC19_308a_repl ~ sexism + dem + rep, data = CCES19_assignment)
summary(sexism_party)
"The significance of the coefficients did not change much from the first model to the second,
but adjusted R-squared is noticeably higher in the second model (0.52 compared to 0.18).
This significant increase in the fit of the model can be explained simply by the fact that
in the second model more variables were controlled, so this model is more complex.
`r coef(sexism_trump)`
The coefficient slope of sexism got slightly less steep, but this again can be explained by
the presence of another regressors in the second model.
`r coef(sexism_party)`
In terms of interpretation of the results, everything is pretty much in line with what has
already been pointed out in previous exercise. Higher disapproval of sexism is overall
reliable predictor of higher disapproval of President Trump, and this effect is especially
strong for democrats - the coefficient for them is a few times higher than for the whole sample.
However, the coefficient for republicans is negative. So for them 1 level increase in disapproval
of Trump leads to 1.6 level increase in approval for sexism. "


## Exercise 3.1 ----
"Calculate the average number of Hispanics in each state as well as the average of your measure 
of anti-immigration attitude using ddplyr."
hisp <- subset(CCES19_assignment, subset=(race==3))
hisp$race = 1
library(plyr)
hisp_by_state = ddply(hisp, .(inputstate), summarise, sum(race))
imm_states = ddply(hisp, .(inputstate), summarise, mean(migr_avg))

hisp_imm_states = merge(hisp_by_state, imm_states, by = "inputstate")

names(hisp_imm_states)[names(hisp_imm_states) == "..1.y"] <- "avg.immigr"
names(hisp_imm_states)[names(hisp_imm_states) == "..1.x"] <- "race"


## Exercise 3.2 ----
"Visualize and explain your findings!"
plot(hisp_imm_states$avg.immigr, hisp_imm_states$race, pch = 5, col = "purple4",
     ylab = "Number of hispanic respondents per state",
     xlab = "Averaged migration score (0 - pro-immigrant, 1 - anti-immigrant)")
"According to distribution of the data shown on the plot above, we can see that a big number
of hispanic residents in the state does not neccessarily lead to very strong pro-immigrant or
anti-immigrant attitudes - most datapoints are concentrated closer to the middle of the
score."







