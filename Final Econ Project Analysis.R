# Econometrics Final Project Analysis
library(readr)
library(tidyverse)
library(haven)
library(sqldf)
library(fastDummies)
library(corrplot)
library(ggplot2)

#setwd("~/Documents/Econometrics")

#download data
gun <- data.frame(read_dta('guns.dta'))
dim(gun)
head(gun)

################################## initial investigation of the data #######################################
### collinearity
cor <- cor(gun) # seems to be high correlation between vio and rob, mur, incarc_rate
heatmap(cor, symm=TRUE) # rob and vio may measure the same thing for the purposes of this project, so they ought to be treated as different output variables
corrplot(cor)


######## organize data into various formats and sub sets for analysis #######
# normnal regression
gun.original <- gun[,-c(1,12)]
head(gun.original)

# stateid dummies
gun.state.dummy <- dummy_cols(gun, select_columns = 'stateid', remove_first_dummy = T) # first dummy is removed to avoid exact collinearity
head(gun.state.dummy)

gun.state.dummy <- gun.state.dummy[,-c(1,12)] # remove year and stateid columns to prepare data for analysis
head(gun.state.dummy)

# year id dummies
gun.year.dummy <- dummy_cols(gun, select_columns = 'year', remove_first_dummy = T) # first dummy is removed to avoid exact collinearity
head(gun.year.dummy)

gun.year.dummy <- gun.year.dummy[,-c(1,12)] # remove year and stateid columns to prepare data for analysis
head(gun.year.dummy)

# combined year and state id dummies
gun.yr.st.dummy <- cbind(gun.state.dummy, gun.year.dummy[,-c(1:11)])
head(gun.yr.st.dummy)



############################################ regressions and f-tests #########################################################

##### do the given variables in the gun.dta dataset have significant explanatory value?
#reduced model with only shall as a standalone variable
original.red.reg <- lm(gun.original$vio ~ gun.original$shall, data= gun.original)
summary(original.red.reg)
# full model only using original variables
original.reg <- lm(gun.original$vio ~ ., data = gun.original)
summary(original.reg)
# F-test comparing models
anova(original.reg, original.red.reg)
# conclusion - there is significant explanatory value in the original variables given in the guns.dta dataset


##### does highly correlated values - rob, mur, and incarc_rate - have significant explanatory value?
# reduced model without rob, mur, incarc_rate
red.cor <- lm(gun.original$vio ~ gun.original$pb1064 + gun.original$pw1064 + gun.original$pm1029 + gun.original$pop +
                gun.original$avginc + gun.original$density + gun.original$shall, data = gun.original)
summary(red.cor)
# full model including correlated variables
full.cor <- lm(gun.original$vio ~ ., data = gun.original)
summary(full.cor)
# f-test between two models with and without correlated data
anova(full.cor, red.cor)
# conclusion - the correlated variables rob, mur, and incarc_rate have significant explanatory value


##### is the fixed effects model appropriate using stateids as dummy variables?
# full model including state FE variables
full.st.reg <- lm(gun.state.dummy$vio ~ ., data = gun.state.dummy)
summary(full.st.reg)
# reduced model excluding state FE varaibles
red.st.reg <- lm(gun.state.dummy$vio ~ gun.state.dummy$mur + gun.state.dummy$rob + gun.state.dummy$incarc_rate +
                   gun.state.dummy$pb1064 + gun.state.dummy$pw1064 + gun.state.dummy$pm1029 + gun.state.dummy$pop +
                   gun.state.dummy$avginc + gun.state.dummy$density + gun.state.dummy$shall, data = gun.state.dummy)
summary(red.st.reg)
# f-test against the two models
anova(full.st.reg, red.st.reg)
# conclusion - stateids FEs are significant


#### do yearly FEs have significant predictive value?
# full model including yearly FE variables
full.yr.reg <- lm(gun.year.dummy$vio ~ ., data = gun.year.dummy)
summary(full.yr.reg)
# reduced model excluding state FE varaibles
red.yr.reg <- lm(gun.year.dummy$vio ~ gun.year.dummy$mur + gun.year.dummy$rob + gun.year.dummy$incarc_rate +
                   gun.year.dummy$pb1064 + gun.year.dummy$pw1064 + gun.year.dummy$pm1029 + gun.year.dummy$pop +
                   gun.year.dummy$avginc + gun.year.dummy$density + gun.year.dummy$shall, data = gun.year.dummy)
summary(red.yr.reg)
# f-test against the two models
anova(full.yr.reg, red.yr.reg)
# conclusion - yearly FEs are significant


#### does the combination of time and entity fixed effects make a more significant model than the two prior models with only one or the other?
# Is shall still a significant variable when all state and year FEs are included?
gun.yr.st.dummy.reg <- lm(gun.yr.st.dummy$vio ~ ., data = gun.yr.st.dummy)
summary(gun.yr.st.dummy.reg)
# shall variable is significant in this model where both time and entity fixed effects variables are used


#### are non-linear variables appropriate in explaining this model?
## quadratic
# full model with quadratic varaibles
nonlin.reg.full <- lm(gun.original$vio ~ . + I(gun.original$incarc_rate^2) + I(gun.original$density^2)
                  + I(gun.original$rob^2) + I(gun.original$mur^2) + I(gun.original$avginc^2) + 
                    I(gun.original$pb1064^2) + I(gun.original$pw1064^2) + I(gun.original$pop^2) +
                    I(gun.original$pm1029^2), data= gun.original)
summary(nonlin.reg.full)
# reduced model excluding quadrtics
nonlin.reg.red <- lm(gun.original$vio ~ ., data = gun.original)
summary(nonlin.reg.red)
# f test comparing quadratic to simple linear model
anova(nonlin.reg.full, nonlin.reg.red)
# conclusion - some quadratic variables are significant predictors of violence


## interaction variables
# full model with interaction varaibles
int.full.reg <- lm(gun.original$vio ~ . + .^2, data = gun.original)
summary(int.full.reg)
# reduced model excluding interaction variables
int.red.reg <- lm(gun.original$vio ~ ., data = gun.original)
summary(int.red.reg)
# f test comparing interaction variable model to reduced excluding interaction variables
anova(int.full.reg, int.red.reg)
# conclusion - some interaction variables are significant predictors of crime rate



############################### final model selection  #################################
# reduced
red.mod.reg <- lm(gun.yr.st.dummy$vio ~ 1, data = gun.yr.st.dummy)
summary(red.mod.reg)
# full
full.mod.reg <- lm(gun.yr.st.dummy$vio ~ . + I(gun.yr.st.dummy$incarc_rate^2) + I(gun.yr.st.dummy$density^2)
                   + I(gun.yr.st.dummy$rob^2) + I(gun.yr.st.dummy$mur^2) + I(gun.yr.st.dummy$avginc^2) + 
                     I(gun.yr.st.dummy$pb1064^2) + I(gun.yr.st.dummy$pw1064^2) + I(gun.yr.st.dummy$pop^2) +
                     I(gun.yr.st.dummy$pm1029^2) + I(gun.yr.st.dummy$mur*gun.yr.st.dummy$rob) + I(gun.yr.st.dummy$mur*gun.yr.st.dummy$incarc_rate) + I(gun.yr.st.dummy$mur*gun.yr.st.dummy$pb1064) + I(gun.yr.st.dummy$mur*gun.yr.st.dummy$pw1064) +
                     I(gun.yr.st.dummy$mur*gun.yr.st.dummy$pm1029) + I(gun.yr.st.dummy$mur*gun.yr.st.dummy$pop) + I(gun.yr.st.dummy$mur*gun.yr.st.dummy$avginc) + I(gun.yr.st.dummy$mur*gun.yr.st.dummy$density) + I(gun.yr.st.dummy$mur*gun.yr.st.dummy$shall) + I(gun.yr.st.dummy$rob*gun.yr.st.dummy$incarc_rate) +
                     I(gun.yr.st.dummy$rob*gun.yr.st.dummy$pb1064) + I(gun.yr.st.dummy$rob*gun.yr.st.dummy$pw1064) + I(gun.yr.st.dummy$rob*gun.yr.st.dummy$pm1029) + I(gun.yr.st.dummy$rob*gun.yr.st.dummy$pop) + I(gun.yr.st.dummy$rob*gun.yr.st.dummy$avginc) + I(gun.yr.st.dummy$rob*gun.yr.st.dummy$density) + 
                     I(gun.yr.st.dummy$rob*gun.yr.st.dummy$shall) + I(gun.yr.st.dummy$incarc_rate*gun.yr.st.dummy$pb1064) + I(gun.yr.st.dummy$incarc_rate*gun.yr.st.dummy$pw1064) + I(gun.yr.st.dummy$incarc_rate*gun.yr.st.dummy$pm1029) + I(gun.yr.st.dummy$incarc_rate*gun.yr.st.dummy$pop) +
                     I(gun.yr.st.dummy$incarc_rate*gun.yr.st.dummy$avginc) + I(gun.yr.st.dummy$incarc_rate*gun.yr.st.dummy$density) + I(gun.yr.st.dummy$incarc_rate*gun.yr.st.dummy$shall) + I(gun.yr.st.dummy$pb1064*gun.yr.st.dummy$pw1064) * I(gun.yr.st.dummy$pb1064*gun.yr.st.dummy$pm1029) +
                     I(gun.yr.st.dummy$pb1064*gun.yr.st.dummy$pop) + I(gun.yr.st.dummy$pb1064*gun.yr.st.dummy$avginc) + I(gun.yr.st.dummy$pb1064*gun.yr.st.dummy$density) + I(gun.yr.st.dummy$pb1064*gun.yr.st.dummy$shall) + I(gun.yr.st.dummy$pw1064*gun.yr.st.dummy$pop) + I(gun.yr.st.dummy$pw1064*gun.yr.st.dummy$avginc) +
                     I(gun.yr.st.dummy$pw1064*gun.yr.st.dummy$density) + I(gun.yr.st.dummy$pw1064*gun.yr.st.dummy$shall) + I(gun.yr.st.dummy$pm1029*gun.yr.st.dummy$pop) + I(gun.yr.st.dummy$pm1029*gun.yr.st.dummy$avginc) + I(gun.yr.st.dummy$pm1029*gun.yr.st.dummy$density) + I(gun.yr.st.dummy$pm1029*gun.yr.st.dummy$shall) +
                     I(gun.yr.st.dummy$pop*gun.yr.st.dummy$avginc) + I(gun.yr.st.dummy$pop*gun.yr.st.dummy$density) + I(gun.yr.st.dummy$pop*gun.yr.st.dummy$shall) + I(gun.yr.st.dummy$avginc*gun.yr.st.dummy$density) + I(gun.yr.st.dummy$avginc*gun.yr.st.dummy$shall) + I(gun.yr.st.dummy$density*gun.yr.st.dummy$shall), 
                   data = gun.yr.st.dummy)
summary(full.mod.reg)
# f- test between models
anova(full.mod.reg, red.mod.reg)
# conclusion, the model's variables are very significant. So, information contained in these variables has strong interpretive value

##### first iteration #####
FinalModel <- step(red.mod.reg, scope = list(upper = full.mod.reg), direction = "both")
summary(FinalModel)

##################### significant findings related to shall laws in order of addition to the model - first iteration #############################
#I(gun.yr.st.dummy$pw1064 * gun.yr.st.dummy$shall)            coef = -1.426e+00;  SE = 4.565e-01;  t-stat = -3.123;  p-val = 0.001836 ** 
#I(gun.yr.st.dummy$rob * gun.yr.st.dummy$shall)               coef = 3.815e-01;   SE = 7.274e-02;  t-stat =  5.245;  p-val = 1.88e-07 ***
#I(gun.yr.st.dummy$mur * gun.yr.st.dummy$shall)               coef = -4.902e+00;  SE = 1.765e+00;  t-stat = -2.778;  p-val = 0.005565 ** 
#I(gun.yr.st.dummy$pm1029 * gun.yr.st.dummy$shall)            coef = 3.923e+00;  SE =  2.061e+00;  t-stat = 1.903;  p-val = 0.057275 . 

# interestingly, shall itself does not appear as a standalone varaible. shall is only significant in this model when interacted with other variables. 
# this could mean that shall laws are only significant under certain demographic conditions.
# however, not all of these variables are negative. Although relatively small and insignificant, shall's interaction with robbery and with pm1029 both have positive effects!
# if shall laws have positive contributions to violence in some demographic contexts, that could indicate that there are negative, un-intended consequences from shall laws.
# According to this model, when shall laws are implemented where pw1064 is relatively higher, violent crime seems to be significantly reduced. However, this may be more 
# corrolary than causal.Additionally, shall laws seem to reduce violent crime when there are relatively high murder rates in certain states. 
# however, shall laws seem to increase the amount of violent crime when there are more robberies in certain states. This may be because robbers are more inclined to rob 
# someone when they have confidence that their victim will not be armed due to the fact that there are laws prohibiting many people from having weapons. this increased likelihood 
# to rob people will naturally result in more violent crimes. finally, the least significant variable is the interaction between pm1029 and whether there is shall laws in a certain state. 
# interestingly, this variable is positive indicating that shall laws increase the number of violent crimes in areas where the pm1029 is relatively higher. this may be due to similar reasons per above. 
# young males are more likely to commit robberies. so, if those young males expect their victims to be unarmed, they are more likely to attempt a crime. then, it would make sense that with more robbery attempts, 
# there will also be more violent crimes as there are more of those sorts of interactions.
# Again, this data can only provide insight into relationships and may not necessarily be causal. However, it does provide insight into the relationships between these variables.




################################### final model with complete dummy variables to be compliant with econometric theory (backward and forward) - second iteration ####################################
finalModel.red <- lm(gun.yr.st.dummy$vio ~ ., data = gun.yr.st.dummy)
  summary(finalModel.red)

##### second iteration #####
finalModel.full <- step(finalModel.red, scope = list(upper = full.mod.reg), direction = "both")
summary(finalModel.full)


# analysis of results from second itreration of this model

# I(gun.yr.st.dummy$rob * gun.yr.st.dummy$shall)           2.720e-01  8.966e-02   3.034 0.002473 ** 
# I(gun.yr.st.dummy$density * gun.yr.st.dummy$shall)       1.932e+02  7.667e+01   2.519 0.011906 *  
# I(gun.yr.st.dummy$incarc_rate * gun.yr.st.dummy$shall)   1.150e-01  3.903e-02   2.947 0.003278 ** 
# shall                                                   -2.227e+02  5.201e+01  -4.283 2.01e-05 ***
# I(gun.yr.st.dummy$pm1029 * gun.yr.st.dummy$shall)        9.885e+00  3.037e+00   3.255 0.001170 ** 
# I(gun.yr.st.dummy$mur * gun.yr.st.dummy$shall)          -4.475e+00  1.975e+00  -2.265 0.023684 *      ############ in this model, but not the below one

# interesting findings from above methodology which removed some variables as they were seen as less valuable for the model
# shall as a standalone variable was very significant, and so was its interaction with various other variables. this indicates that there are 
# complex effects from the implementation of shall laws, or that there are specific aspects of states that tend to implement those shall laws. 
# This may simply be an insight into the types of states which implement shall laws as opposed to causal effects from shall laws being implemented
# for example, interpretation could read that shall laws increase violence when x, or that shall laws tend to be implemented in areas where x is the case

