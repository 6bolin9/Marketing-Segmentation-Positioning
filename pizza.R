#==================================================================
#                   MSCA 673 - DR. K. BUYUKKURT
#                       TAKE-HOME EXAM2 - PART 2
#                        LOGISTIC REGRESSION
#         SIMULATED TEST MARKETING OF MCCAINS'S MINI PIZZA
# FILE NAME: Sim_Test_Pizza.R
# LAST REVISION: 18 April, 2018
# Disclaimer: The simulated test marketing situation that is mentioned
# in the WORD document that presents the questions and instructions is 
# not real. The brand name McCain is mentioned only to make the research
# scenario sound more realistic. 
# =================================================================

# If the following R packages are not intalled on your PC you need to 
# install them.
library(aod)
library(vcd)
library(vcdExtra)
library(effects)

df = read.csv("pizzadat.csv") # read data file
# Print the "head" (the first six observations) of the data file
head(df)
# Define categorical variables as R "factors"
d = df
d$suburb = factor(df$suburb, levels=c(0,1), labels=c("suburbs", "urban"))
d$income = factor(df$income, levels=c(1,2,3), labels=c("lt40k","41to80k", "gt80k"))
d$agechildrn = factor(df$agechildrn, levels=c(1,2), labels=c("btw1and6", "btw7and12"))
d$educ = factor(df$educ, levels=c(0,1), labels=c("hisch", "univ"))
# Print the data frame with categorical variables are defined as factors
# and labels for the levels of factors.
head(d)

#===================================================================
# "TOP-DOWN" MODEL BUILDING

# LOGIT ANALYSIS OF FULL MODEL

# m15: Model with 15 covariates (including the dummy variables for 
# categorical variables. Full model is specified as the model with 
# all main effects and some of the two way interactions including
# income:careful, income:timepressr and suburb:edu
# 
m15 <- glm(resp ~ suburb + numbchildrn + agechildrn + income + 
           careful + active + timepressr + educ + income:careful + income:timepressr +
           suburb:educ, data=d, family=binomial)

# CREATE THE MODEL MATRIX FOR FULL MODEL
# MODEL MATRIX SHOWS HOW THE PREDICTORS ARE REPRESENTED "INTERNATLLY"
# PAY ATTENTION TO THE LABELS THAT ARE USED FOR THE VARIABLES. 
# For example, income41to80k is the column that represents the second level 
# "41to80k" of dummy coded income. incomegt80k represents the third level.
# The first level of income is the reference category "lt40k".
# Ineraction labels indicate the variables (continuous or categorical)
# that are involved in the interactions.
modmat15 = model.matrix(~suburb + numbchildrn + agechildrn + income + 
                      careful + active + timepressr + educ + income:careful + income:timepressr +
                      suburb:educ, data=d )
# Print the first 6 observations as they are represented internally according
# model m15. 
head(modmat15)
# Understanding what variables that the column labels correspond to makes it 
# easier to interpret the regression coefficients that are printed in summary
# statements. 
# Print the estimated regression coefficients for all terms in the 
# model and then decide which term to delete from the model
summary(m15)

# New model: 
# Take out the term that you want to delete from the following GLM funtion 
# call to run Logistic Regression. Note that you will be taking out a term 
# from the previous model called m15
newmod1 <- glm(resp ~ suburb + numbchildrn + agechildrn + income + 
             careful + active + timepressr + educ + income:careful + income:timepressr +
             suburb:educ, data=d, family=binomial)
# Print the estimated regression coefficients for all terms in the 
# model and then decide which term to delete from the current model newmod
str(newmod2)

# Repeat the statements 72-77 as many times as necessary until you end up 
# with a model where the regression coefficients have low p-values. 
# Caution: if an interaction is statistically significant, you keep
# all the effects that are involved in that interaction in the regression
# equation. 

# MODEL CHOICE
# Using the AIC and BIC statistics printed by the following function, decide
# which equation you would like to keep. Note that you need to add
# the object name for each GLM call for each new model. Below, I called them
# newmod1, newmod2, etc. Use any name you want for different models as long
# as the names are different. In your answer, be sure to discuss how AIC
# and BIC apply "penalties" to the deviance measure
# (that is, 2xLoglikelihoo) to search for parsimonious results.
LRstats(glmlist(m15, newmod1, newmod2,.......))




# ASSUME THAT YOU DECIDED TO KEEP THE FOLLOWING  MODEL CALLED mfin (final model)
mfin <- glm(resp ~ numbchildrn + agechildrn + income + 
             careful + active + timepressr + income:timepressr,
           data=d, family=binomial)
summary(mfin)
pizza.mfin <- allEffects(mfin, xlevels=list(numbchildrn=1:4, careful= 1:3, active= 1:4))


# INTERPRETATION OF REGRESSION COEFFICIENTS AS ODDS RATIOS:
betas = as.matrix(mfin$coefficients)
odds.ratios = exp(betas)
bsandodds =cbind(betas,odds.ratios)
colnames(bsandodds)= c("Beta","OddsRatio")
options(digits=4)
bsandodds 
# Notation:  NNN.e+3 means  NNN x (10 to the power 3
# Notation:  NNN.e+0 means  NNN X (10 to the power 0) = NNN
# because any number to the power 0 is 1.0

# HYPOTHESIS TESTING INVOLVING MODEL mfin: 
#*****************************************************************
# TESTING THE SIGNIFICANCE OF THE OVERALL MODEL mfin AGAINST THE "NULL" MODEL
# Does the mfin model with predictors fit better than a model with just an
# intercept (that is, the NULL model). 
with(mfin, null.deviance - deviance) # the difference in deviance
with(mfin, df.null - df.residual) # degrees of freedom for deviance difference
# Now we can calculate the p-value associated with this Chi-Square test
with(mfin, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
# Chi-square value=295.2317, d.f.=9, p-value <0.000.

# Testing if the regression coefficients for number of children and age of 
# children differ.
# When you print summary(mfin) you see that number of children is the second
# and the age of children is the third term in model mfin with 10 terms
# Define the related constrast for the difference of the beta coefficients
# Below the constrast is calle NChvsNAge (Number of children vs Age of children )
NChvsAgeCh <- cbind(0,1,-1,0,0,0,0,0,0,0)
wald.test(b=coef(mfin), Sigma = vcov(mfin), L=NChvsAgeCh)

# Testing if the regression coefficients for income level 2 and income level
# 3 differ
income2vsincome3 <- cbind(0,0,0,-1,1,0,0,0,0,0)
wald.test(b=coef(mfin), Sigma = vcov(mfin), L=income2vsincome3)

#=========================================================================
# THE FOLLOWING STATEMENT GENERATES THE PREDICTED PROBABILITIES THAT YOU NEED
# TO INTERPRET THE MAIN AND INTERACTION EFFECT PLOTS BELOW. ALSO PRINTED ARE
# THE 95 % CONFIDENCE INTERVALS FOR THOSE PREDICTED PROBABILITIES
# ONLY THE VALUES OF THE VARIABLES THAT ARE DISPLAYED ON THE PLOT ARE 
# VARIED IN THE PLOT. 
# THE VALUES OF OTHER VARIABLES THAT ARE IN THE LOGISIC REGRESSION EQUATION 
# ARE FIXED USING THESE RULES: 
# (1) IF THE VARIABLE IS A CONTINUOUS VARIABLE,ITS VALUE IS FIXED AS THE MEAN
# OF THAT VARIABLE. 
# (2) IF THE VARIABLE IS A CATEGORICAL VARIABLE, ITS VALUE IS FIXED AS THE 
# VALUE OF THE REFERENCE CATEGORY.
summary(pizza.mfin)

# Compute the fitted values for each effect in the m12 model
mfin.eff <- allEffects(mfin)
# Get the names of the effects that we can plot
names(mfin.eff)

# DEFINE TICKS FOR THE VERTICAL AXIS TO DISPLAY PREDICTED PROBABILITIES
ticks <- list(at=c(0.0,.01, .03, .05, seq(.1, .9, by=.05), .95, .99))



# EFFECT PLOTS
# Plot for Number of Children
# Be patient and wait. The plot may take a few seconds
plot(mfin.eff[1], ticks=ticks, multiline=TRUE, ci.style="bars")

# Plot for Age of Children
plot(mfin.eff[2], ticks=ticks, multiline=TRUE, ci.style="bars")

# Plot for Active
plot(mfin.eff[3], ticks=ticks, multiline=TRUE, ci.style="bars")

# Plot for time pressure
plot(mfin.eff[4], ticks=ticks, multiline=TRUE, ci.style="bars")

# Plot for Income x Time Pressure Interaction
plot(mfin.eff[5], ticks=ticks, multiline=TRUE, ci.style="bars")


