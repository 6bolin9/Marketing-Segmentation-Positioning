# **********************************************************************
#                     MSCA 673 - DR. K. BUYUKKURT

# THE FOLLOWING CODE IS PART OF THE HELP ASSOCIATED WITH R-PACKAGE
# VCDEXTRA BY DR. MICHAEL FRIENDLY, YORK UNIVERSITY. THE CODE AND ITS 
# RESULTS ARE DISCUSS IN A PAPER THAT IS TITLED "VISUALIZING GLMS FOR
# BINARY OUTCOMES. A COPY OF THE PAPER IS POSTED ON MOODLE. 
# THE RELEVANT SECTION OF THE PAPER THE "EFFECTS" DISCUSSION THAT STARTS 
# ON PAGES 5-8. 

# FILE NAME: TITANIC INTERACTION EFFECT.R
# LAST REVISION: 17 APRIL, 2018
#****************************************************************


# install the following packages if you have not done so.
# install.packages("vcd")
# install.packages("vcdExtra")
# install.packages("effects")
library(vcd)
library(vcdExtra)
library(effects)


# PLEASE REFER TO "Visualizing GLMs for Binary Outcomes" by 
# Michael Friendly for the details associated with the following 
# R Script and the logic behind the effect plots in 
# Generalized Linear Models (GLM's). Logistic Regression is a 
# type of GLM. The mentioned paper is posted on Moodle.

# OuR objective here is to plot interaction
# and main effects in Logistic Regression. The data and the context are described 
# in the mentioned paper. You are not responsible for the content 
# of the paper. I just provided it for completeness. The following R script
# is related to the EFFECT PLOTS that starts on page 5.

# The data are on 1046 passengers on the Titanic and you can
# easily access it as shown below after you install the "vcdExtra"
# package. The dependent variable SURVIVED indicates if a 
# passenger survived the terrrible incident. The predictors
# are PCLASS (passenger class on Titanic (classes 1, 2, and 3)), AGE and SEX.
# So, PCLASS AND SEX are categorical variables. 

data(Titanicp, package="vcdExtra")
Titanicp <- Titanicp[!is.na(Titanicp$age),]
# print the first 6 observations of the data file. We are not
# using the last two variables in this example. 
# The last statement deletes cases with missing values for AGE. 
head(Titanicp) 

# Estimate three different logistic regression models
titanic.glm1 <- glm(survived ~ pclass + sex + age, data=Titanicp, family=binomial)
titanic.glm2 <- glm(survived ~ (pclass + sex + age)^2, data=Titanicp, family=binomial)
titanic.glm3 <- glm(survived ~ (pclass + sex + age)^3, data=Titanicp, family=binomial)

# Compute and print chi-square statistics associated with the 
# test that all regression coefficients are zero. Present chi-square tests 
# in a table 
anova(titanic.glm1, titanic.glm2, titanic.glm3, test="Chisq")

# AIC AND BIC statistics for these models (calling function 
# that deals with Likelihood Ratio (LR) statistics)
LRstats(glmlist(titanic.glm1, titanic.glm2, titanic.glm3))
# AIC and BIC both suggest that the second model with all two-way
# interactions, that is titanic.glm2, is better

# Print a summary of titanic.glm2
summary(titanic.glm2)
# Let us see how the categorical variable PCLASS was coded
contrasts(Titanicp$pclass) # First class is the reference group

# Compute the fitted values for each effect in the GLM2 model
titanic.eff2 <- allEffects(titanic.glm2)


# Get the names of the effects
names(titanic.eff2)
# There are three effects to be plotted: 
# [1] "pclass:sex" [2]"pclass:age" and [3] "sex:age"   


# The following is not central to our discussion. If you are curious
# about what these statement do, please refer to the paper by Dr. Friendly
# that I mentioned above and posted on on Moodle.
titanic.eff2a <- allEffects(titanic.glm2,
            typical=median,
            given.values=c(pclass2nd=1/3, pclass3rd=1/3, sexmale=0.5)
)

# DEFINE TICKS FOR THE VERTICAL AXIS TO DISPLAY PREDICTED PROBABILITIES
ticks <- list(at=c(.01, .05, seq(.1, .9, by=.2), .95, .99))


#===============================================================
#               EFFECT PLOTS

# AS WE SAW ABOVE, THERE ARE THREE EFFECTS TO BE PLOTTED: 
# There are three effects to be plotted: 
# [1] "pclass:sex" [2]"pclass:age" and [3] "sex:age"   

# PLOT THE FIRST EFFECT: PCLASS X SEX INTERACTION
plot(titanic.eff2[1], ticks=ticks, multiline=TRUE, ci.style="bars", 
     key=list(x=.7, y=.95))
plot(titanic.eff2[1], ticks=ticks, multiline=TRUE, ci.style="bars")

# PLOT PCLASS X AGE INTERACTION: 
plot(titanic.eff2[2], ticks=ticks, multiline=TRUE, ci.style="bars")
# Note how critical the effect of age was on survival for 2nd Class:
# the log odds of survival (and note the probability tick marks also on 
# the vertical axis) declined dramatically with age more so for the 
# second class passengers than for the first and third class passengers.

# PLOT SEX X AGE INTERACTION
plot(titanic.eff2[3], ticks=ticks, multiline=TRUE, ci.style="bars")
# Note that the survival rate was much higher for females especially for older 
# age passengers. 