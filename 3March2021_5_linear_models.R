## 3 March 2021
## 3.5 Linear models

## Linear regression is one of the most commonly-used and easy-to-understand approaches
## to modeling. Linear regression involves a numerical outcome variable y and explanatory
## variable(s) x that are either numerical or categorical. We'll start with simple 
## (univariate) linear regression where y is modeled as a function of a single x 
## variable. 

## The mathematical equation for simplre regression is as follows: 
####   y = B1 + B2*x + E
#### where B1 is the y-intercept, B2 is the slope, and E is the error term, or the 
#### portion of y that the regression model can't explain. 

## Linear regression has 5 key assumptions:
#### -- Linear relationship
#### -- Multivariate normality
#### -- No or little multicollinearity
#### -- No auto-correlation (samples are independent)
#### -- Homoscedasticity (variance is equal along the regression line)
## Additionally, a good sample size rule of thumb is that the regression analysis requires
## at least 20 cases per independent variable in the analysis. 

## Here are some plots you want to look at before building a linear regression model:
#### -- Scatter plot: Visualize the linear relationship between the predictor and 
####    response, check for auto-correlation (if needed) and heteroscedasticity
#### -- Box plot: To spot any outlier observations in the variable. Having outliers in 
####    your predictor can drastically affect the predictions as they can affect the
####    direction/slope of the line of best fit.
#### -- Histogram/density plot: To see the distribution of the predictor variable. 
####    Ideally, a close to normal distribution (a bell-shaped curve), without being
####    skewed to the left or right is preferred.
#### -- QQ plot: Also good for checking normality of your predictor variable. 
#### -- Correlation matrix (like GGally::ggpairs()) to check for multicollinearity in 
####    your explanatory variables.

## If you do encounter problems with your data, there are solutions that can help make
## linear regression an appropriate analysis. For example, if the explanatory variables
## aren't normal or you have heteroscedasticity, a nonlinear transformation may solve
## the issue. If you have some nasty outliers, think about whether you might be 
## (scientifically) justified in removing them from the dataset. If several of your 
## explanatory variables are correlated, you can remove some of them using stepwise 
## regression.

## Now we're going to use the penguins data to look again at the relationship between 
## bill length and bill depth. I'll do some exploratory data analysis by viewing the
## first few data points and calculating summary statistics, then I'll look at the
## density plots and correlation with GGally::ggpairs().

library(tidyverse)
library(palmerpenguins)
library(GGally) # ggPairs()
library(ggiraph)
library(ggiraphExtra) # ggPredict()
library(broom)  # tidy() augment()
library(car) # vif()

## Exploratory data analysis:
glimpse(penguins)
summarize(penguins)
penguins %>% 
  select(bill_depth_mm, bill_length_mm) %>%
  ## Calling out the library can avoid ambiguity for common-named functions, or just
  ## serve as a reminder to you.
  GGally::ggpairs() 

## The linearity of the explanatory variable bill_depth_mm and the independent variable
## bill_depth_mm doesn't look very promising. Let's see what happens if we build the
## linear regression. We'll use the function lm() which stands for "linear model" and
## we'll indicate the relationship we want to test by putting a formula of the format
## y~x as a parameter. We'll save the model results in a variable, then use the summary() 
## function to display the model results:

lm_1 = lm(bill_depth_mm ~ bill_length_mm, data=penguins)
summary(lm_1)

## Both coefficients B1 (the y-intercept) and B2 (the slope associated with bill length)
## are statistically significant. HOwever, the R2 = 0.05, which means that the bill
## length only explains about 5% of the variance in the bill depth observations. That's
## pretty pathetic. Since this is just a simple univariate regression model, we can 
## quickly plot the data and the linear model using geom_smooth() with method="lm" in
## ggplot. 

ggplot(data=penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
     geom_point() +
     geom_smooth(method = "lm")

## There are clear clusters in the data that are being ignored by our regression 
## model, and the line doesn't seem to capture any interesting trend. The negative
## coefficient for bill_length_mm (as well as the plotted model) indicates that as 
## bill length increases, bill depth decreases. Does that make sense? What did we do
## wrong?

## Well, grouping all three species into the data is pretty illogical. It actually 
## violates another, less cited, assumption of linear regression: "All necessary 
## independent variables are included in the regression that are specified by existing
## theory and/or research." We probably should have included species, or separated our
## analysis out into three separate models, one for each species. 

## To check some of the assumptions of our linear model post hoc, you can send your
## saved model to the plot() function in base R:

class(lm_1) ## Note that your model output is a variable of the class "lm"
plot(lm_1)  ## This actually calls plot.lm() since the first parameter is class "lm"

## You can learn more about this quick-and-dirty diagnostics plotting trick by looking
## up the help page ?plot.lm. The function plot.lm() is the version of the plot() function
## that is called when the parameter that you pass plot() is of the class "lm". This 
## output provides you four useful plots:
#### -- Residuals vs. Fitted Values, to check constant variance in residuals and 
####    linearity of association between predictors and outcome (look for a relatively
####    straight line and random-looking scatterplot). By default, the 3 points with 
####    the highest residuals are labeled (i.e., the row number is printed on the figure).
#### -- Normal QQ Plot, to check the assumption of normally distributed residuals.
#### -- Root of Standardized residuals vs. Fitted values, this is very similar to 
####    number 1, where the y-axis of residuals is in a different metric.
#### -- Residuals vs. Leverage, to check if the leverage of certain observations are
####    driving abnormal residual distributions, thus violating assumptions and biasing
####    statistical tests.
## There are many objective statistical tests that can be performed to check the 
## assumptions of your data. 

## Let's do the logical thing and test the same relationship bill_depth_mm ~ bill_length_mm
## but only looking at one species.

gentoo = penguins %>% filter(species=="Gentoo")
lm_2 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
summary(lm_2)
ggplot(data=gentoo, aes(x = bill_length_mm, y = bill_depth_mm)) +
     geom_point() +
     geom_smooth(method = "lm")

## This trend looks better. The R2 = 0.41, so the model explains about 41% of the variation.
## That's pretty impressive considering there's only one variable in there. Also, 
## the model passes the sanity check because it seems logical that bill depth will 
## increase with increasing bill length. 

## We can actually use geom_smooth() to examine separate linear models for each of the
## three penguin species at once without formall running the regression:

ggplot(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
     geom_point() +
     geom_smooth(method = "lm")

## That makes it clear that running all three species together was a serious logical 
## error, and it produced an untrue result that bill depth increases as bill length
## decreases. In fact, when we split the three species apart, we can see that bill
## depth increases with increasing bill length (as expected) and the relationships 
## look pretty similar for each species. This is an example of Simpson's paradox, which
## occurs when trends that appear when a dataset is separated into groups reverse when
## the data are aggregated. Basically, when doing statistics, use your brain and gut to
## build models that make sense.

## Exercise 5.1: Build a model predicting Gentoo bill length as a function of flipper
## length. Plot the predictions. Which explanatory variable (bill length v. flipper
## length) does a better job of predicting bill depth? What is your evidence?

gentoo = penguins %>% filter(species == "Gentoo")

lm_bill_length = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
summary(lm_bill_length)

lm_flip = lm(bill_depth_mm ~ flipper_length_mm, data=gentoo)
summary(lm_flip)

ggplot(data=gentoo, aes(x=flipper_length_mm, y=bill_depth_mm)) +
  geom_point() +
  geom_smooth(method="lm")
## The adjusted R2 is higher in the flipper model than in the bill length model, 
## so flipper length does a better job of predicting bill depth for Gentoo penguins.

## Multiple linear regression
## In life, we typically can't do a very good job explaining variation in some dependent
## variable using just a single independent variable. Multiple linear regression is 
## when we build a function with multiple independent variables of the form:
####    y = B0 + B1*x1 + B2*x2 + ... + E
## When conducting multiple linear regression, all of the same assumptions and diagnostics
## apply, with one important addition: the interpretation of the associated effect 
## of any one explanatory variable must be made in conjunction with the other explanatory
## variables included in your model. 

## A note on model goals: 
## The statistical decisions you make should account for your end goals. These are the
## two types of goals when building a model: 
#### 1. Modeling for explanation: When you want to explicitly describe and quantify 
####    the relationship between the outcome variable y and a set of explanatory 
####    variables x, determine the significance of any relationships, have measures
####    summarizing these relationships, and possibly identify any causal relationships
####    between the variables. 
#### 2. Modeling for prediction: When you want to predict an outcome variable y based
####    on the information contained in a set of predictor variables x. Unlike modeling
####    for explanation, however, you don't care so much about understanding how all the
####    variables relate and interact with one another, but rather only whether you can
####    make good predictions about y using the information in x. 

## In this lesson, and in general when you are trying to "play it safe" in your 
## analyses, you will be modeling for explanation. This means if you are modeling some
## y as a function of x1 and x2, but x1 and x2 are quite collinear, then you won't be
## able to differentiate the unique impact of either x variable on y because, for example,
## x1 may be stealing some of the variation from x2. Then the total impact of x2 on y 
## will be unfairly biased small, and the impact of x1 will be unfairly biased large.
## Who knows what chaos will ensue when you use these biased models to guide science,
## policy, etc.?

## However, if you are modeling for prediction, and don't actually care what the relative
## impact of x1 or x2 is on y, but you want your y predictions to be as accurate and
## precise as possible, then you don't have to worry about multicollinearity. For example,
## weather predictions are like this. Meteorologists just throw everything they can into
## their models, even though their explanatory variables can be highly correlated, 
## because they aren't trying to demonstrate a relationship between rainfall and pressure,
## they're just trying to let you know whether it'll be a good weekend for a beach trip.

## Continuous vs. categorical variables: 
#### -- Continuous variable: numeric variables that can have an infinite number of 
####    values between any two values. 
#### -- Categorical variable: (aka discrete or nominal variables) contain a finite 
####    number of categories or distinct groups. These should typically be in the factor
####    class in R. 

## There are cases when you, the data scientist, can make a conscientious choice between
## assigning a variable as continuous vs. categorical. A good example is "year". If 
## you are looking for a trend in your data over time, year should be treated as a 
## continuous variable. If you are trying to account for some wacky conditions that can 
## change from year to year, but that probably don't constitute a temporal trend, you 
## could treat year as a categorical variable. 

## One continuous and one categorical explanatory variable:
## OUr adventures in simple regression taught us that we shouldn't model bill depth 
## as a function of bill length without accounting for species. We built three separate
## models, one for each species. Another option is to build one model, but include 
## species as an explanatory variable with the function. 

## This is very simple to implement in the lm() function. We'll try a function of the 
## form bill_depth_mm ~ bill_length_mm + species. This model will estimate a single 
## slope associated with bill length and a different intercept for each species. The 
## resulting prediction will look like three parallel lines, one for each species. 

## I'm also going to filter out the NA data before fitting the model, which will 
## help further down the line with predict().

penguins_lm_3 = penguins %>%
  filter(!is.na(bill_depth_mm),
         !is.na(bill_length_mm),
         !is.na(species))
lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data=penguins_lm_3)

## There are several different ways to access your model results. You can copy and 
## paste coefficient estimates, p-values, etc. from the summary() output, or you can
## extract various elements from the model variable using specialized functions like
## coef(). If you want your results to look like an ANOVA table (remember, ANOVA and
## linear regression are mathematically equivalent), you can use the anova() function.
## Probably the most efficient way to get at your model results makes use of the tidy()
## function in the broom package in the tidyverse. Note that broom is one of the packages
## that, although it is part of the tidyverse, it does not load up with the 
## library(tidyverse), it must be loaded separately with library(broom). This outputs
## the model results in a neat data frame, which you can easily export to a .csv file
## to save or create a table in your manuscript. Check the help page ?tidy.lm and you 
## can see that you can ask for confidence intervals to be output on your coefficients.


summary(lm_3)
coef(lm_3)
anova(lm_3)
## Adding confidence intervals to output.
broom::tidy(lm_3, conf.int = TRUE, conf.level = 0.95) 

## Now let's plot the data, the linear model predictions, and the standard errors on 
## those predictions. Although it seems super similar, recall that the plot we made
## at the end of our simple linear regression section actually had the results of 
## three distinct linear models plotted onto the same figure: one for each species.
## This multiple regression model lm_3 that we created is a single model with two 
## explanatory variables, bill_length_mm and species. The slopes associated with each
## species must be equivalent in this model formulation.

## The combined libraries ggiraph and ggiraphExtra have a really convenient plot function
## for examining a model called ggPredict(). You can set the parameters so that standard
## errors are printed around the model and you can also make the plot interactive. 
## That means if you click on a point or a line, a box will pop up with information
## on that datapoint:

library(ggiraph)
library(ggiraphExtra)
ggPredict(lm_3, se=TRUE, interactive=TRUE)

## While ggPredict() is a fabulous function, there is not a lot of room to customize 
## it. The more formal and customizable method for accessing your model predictions 
## is using the predict() function in base R. 

## This will calculate the lm predictions for the original dataset. 
lm_3_predictions = predict(lm_3, interval="confidence") 
head(lm_3_predictions)
penguins_lm_3_predict = cbind(penguins_lm_3, lm_3_predictions)
ggplot(penguins_lm_3_predict, aes(x = bill_length_mm, y = bill_depth_mm, 
                                  color = species)) +
     geom_point() +
     geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species, color = NULL), 
                 alpha = .1) +
     geom_line(aes(y = fit), size = 1) +
     theme_bw()

## The fitted lines in all the plots so far are different lengths. This is because we
## have slightly different ranges of bill length data for each species category in the
## dataset. By default when using predict() we get the fitted values; i.e., the 
## predicted values from the dataset used in model fitting.

## I think having different line lengths is fine here, but there are times when we want
## to draw each line across the entire range of the variable in the dataset. Also, 
## sometimes our data are so sparse that our fitted line ends up not being very smooth; 
## this can be especially problematic for non-linear fits. In both of these situations
## we'd want to make a new dataset for making the predictions. 

## Let's make group lines using the entire range of bill length instead of the 
## within-species range. We can make a variable with the full range of bill length via
## seq(), making a sequence from the minimum to maximum dataset value. I use 0.1 as the
## increment in seq(); the increment value you'll want to use depends on the range of
## your variable. Then to get the full range of bill length associated with each 
## species category we can use expand.grid() in base R. 

## Building a new bill_length_mm dataset that spans the full range of the original data
## at even intervals.
newdata_bill_length_mm = seq(min(penguins_lm_3$bill_length_mm), 
                             max(penguins_lm_3$bill_length_mm), by = .1)
## Repeat complete bill_length_mm data for each species.
newdata = expand.grid(bill_length_mm = newdata_bill_length_mm, 
                      species = unique(penguins_lm_3$species) )
head(newdata)

## The key to making a dataset for prediction is that it must have every variable
## used in the model in it. You will get an error if you forget a variable or make a
## typo in one of the variable names. Note that the prediction dataset does not need
## to contain the response variable. 

## We use this prediction dataset with the newdata argument in predict(). We can add
## the predicted values to the prediction dataset using cbind(). When we make the plot
## of the fitted lines now we can see that the line for each species covers the same
## range. There are now two datasets used in the plotting code: the original for the 
## points and newdata for the predicted line and 95% confidence intervals. 

newdata_predict_lm_3 = cbind(newdata, predict(lm_3, interval="confidence", 
                                              newdata = newdata))
dim(newdata_predict_lm_3)
ggplot() +
     geom_point(data=penguins_lm_3_predict, aes(x = bill_length_mm, y = bill_depth_mm, 
                                                color = species)) +
     geom_ribbon(aes(ymin=lwr, ymax=upr, x = bill_length_mm, fill = species, 
                     color = NULL), alpha = .1, data=newdata_predict_lm_3) +
     geom_line(aes(y = fit, x = bill_length_mm, color=species), size = 1, 
               data=newdata_predict_lm_3) +
     theme_bw()

## Another way to generate model predictions is using the augment() function in the 
## broom package. This function fits into the dplyr coding flow, so you call it with
## a pipe and the model estimate columns automatically append to the data that you are
## using. This avoids the extra call to cbind(), and perhaps a mistake in joining 
## data frames. 

## Get model predictions.
lm_3_predict = lm_3 %>%
  augment(penguins_lm_3, se_fit=TRUE) %>%
  ## Calculate 95% CI using SE
  mutate(lwr = .fitted - 1.96 * .se.fit, upr = .fitted + 1.96 * .se.fit) 
## Plot the data and the model predictions.
ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species), data=lm_3_predict) +
  geom_point() +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species, color = NULL), alpha = .15) +
  geom_line( aes(y = .fitted), size = 1)

## Similarly, we can use augment() to generate predictions with new data, so that our
## predicted model plots extend beyond the range of the data used to fit the model. To
## stay within the tidyverse, we'll use tidyr::expand() in place of expand.grid() to 
## generate all possible combinations of the explanatory variables used in our model.

## Get model predictions with newdata>
newdata = penguins_lm_3 %>% expand(bill_length_mm, species)
lm_3_predict = lm_3 %>%
  augment(newdat = newdata, se_fit=TRUE) %>%
  ## Calculate 95% CI using SE
  mutate(lwr = .fitted - 1.96 * .se.fit, upr = .fitted + 1.96 * .se.fit)
## Plot the data and the model predictions
ggplot() +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species), 
             data=penguins_lm_3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = bill_length_mm, fill = species, 
                  color = NULL), alpha = .15, data=lm_3_predict) +
  geom_line(data=lm_3_predict, aes(y = .fitted, x = bill_length_mm, color=species), 
            size = 1)

## Why am I showing you two completely different ways to generate the same predictions
## and the same plot? Well, that's the secret of programming. There's always more than
## one way to get something done. By trying out the different methods to accomplish
## the same task, you'll become a more versatile and efficient programmer. For your 
## own linear models, use the prediction method that makes the most sense to you. 
## However, one benefit of the tidyverse method is that if you wind up running 
## statistical analyses on Big Data and need to build many similar models, these methods
## combined with tools in the tidyverse purrr package make this very easy and 
## efficient to code. 

## I'm proud of the last plot so I'm going to make it prettier and save it.

ggplot() +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species), 
             data=penguins_lm_3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = bill_length_mm, fill = species, 
                  color = NULL), alpha = .15, data=lm_3_predict) +
  geom_line(data=lm_3_predict, aes(y = .fitted, x = bill_length_mm, color=species), 
            size = 1) +
  theme_bw() +
  xlab("Bill length (mm)") + ylab("Bill depth (mm)") +
  ggsave(filename = "figures/bill_depth_model.png", device = "png", width = 5, 
         height = 3, units = "in", dpi = 300)


## Adding an interaction term:
## Remember our model lm_3:
lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data=penguins_lm_3)
## We allow bill depth to depend on both the continuous variable bill length and the
## categorical variable species, however, the species independent variable only has 
## the power to change the y-intercept of the predictions for each of the three species.
## In this formulation, the prediction lines associated with each species are parallel.
## But what if this isn't a good formulation for predicting bill depth? Perhaps, for
## example, Chinstrap bill depth increases more slowly with increasing bill length
## than Gentoo bill depth. To explore the possibility that the relationship between bill
## depth and bill length changes for each of the three species, we can add an 
## interaction term. If we model the interaction between bill length and species, the
## bill depth prediction lines for each species are no longer forced to be parallel. 
## Variable interactions are indicated with the * sign in R model formulas. 

lm_4 = lm(bill_depth_mm ~ bill_length_mm*species, data=penguins_lm_3)

## Note that bill_length_mm*species is short-hand for bill_length_mm + species +
## bill_length_mm*species. Interaction terms are coded this way because when you build
## a model with an interaction, it's important to always include the interacting 
## variables on their own as well. You wouldn't want a model that looked like 
## bill_depth_mm ~ bill_length_mm*species without including bill length and species as
## additional terms because then the interaction coefficients would be tasked with 
## accounting for all of the variation from each of those two independent variables in
## addition to the interaction between those variables. This would make interpretation
## difficult and may bias our understanding of the importance of the interaction. 

## If we look at the model results for lm_3 side by side with lm_4, we can see that 
## the inclusion of the interaction term is not helping our model very much. The R2 
## measure barely improves with lm_4, and the Adjusted-R2 measure is actually lower.
## None of the interaction coefficients in lm_4 are statistically significant.

summary(lm_3)
summary(lm_4)

## If we use the AIC() function to compare the Akaike Information Criterion between the
## 2 models, the AIC for lm_4 is higher, meaning the fitness is poorer compared to lm_3.
## Another option for comparing a complex model with a simpler model is to use the 
## step() function. You can input the most complicated model formulation that you are
## interested in and this function will reduce the model's complexity one step at a time 
## and check the fitness at each step. Read the documentation on step() to learn more
## about how decisions are made.

AIC(lm_3, lm_4)
best_model = step(lm_4)
best_model

## If we plotted the lm_4 predictions for each of the 3 species, the lines will probably
## look pretty close to parallel, since the interaction terms were not significant. We
## can build our predictions with the same methods we used for lm_3. 

## Plotting predictions with ggPredict(): 
ggPredict(lm_4, se=TRUE, interactive=TRUE)  #E Easy, but less customizable

## Get model predictions with newdata:
newdata = penguins_lm_3 %>% expand(bill_length_mm, species)
head(newdata)
lm_4_predict = lm_4 %>%
  augment(newdat = newdata, se_fit=TRUE) %>%
  ## Calculate 95% CI with SE
  mutate(lwr = .fitted - 1.96 * .se.fit, upr = .fitted + 1.96 * .se.fit) 

## Plot the data and the model predictions
ggplot() +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species), 
             data=penguins_lm_3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = bill_length_mm, fill = species, 
                  color = NULL), alpha = .15, data=lm_4_predict) +
  geom_line(data=lm_4_predict, aes(y = .fitted, x = bill_length_mm, color=species), 
            size = 1)

## As we expected, the distinct slopes of bill depth vs. bill length for each of the 
## 3 species are almost parallel. Since the AIC values are lower (i.e., better fit) for
## lm_3, we would select that as our final model instead of lm_4.

## Multiple regression with 2 continuous variables:
## When including multiple independent variables in a regression, especially multiple
## continuous variables, you need to check the collinearity of your predictors. If 2
## predictors x1 and x2 are extremely collinear, then you can bias your interpretation
## of the model because you can't objectively determine how much variation in y is 
## caused by x1 vs. x2. To deal with this, you should calculate the Variance Inflation
## Factor of your independent variables. If the VIF = 1, then your independent variables
## are not correlated. There are different thresholds set for VIF, but generally, if
## the VIF > 4 or 5, you have a moderate problem with multicollinearity and if the 
## VIF > 10, you have a big problem with multicollinearity in your model. This statistic
## should be reported and model results should be interpreted with the VIF in mind. If
## you are more interested in a mechanistic model (i.e., figuring out WHAT drives y) 
## rather than a predictive model (i.e., getting the BEST estimate of y), then you 
## should use your best human judgment to remove predictor variables until the model 
## results are more interpretable. 

## Let's try out a regression with 2 continuous variables. We'll look at Gentoo penguin
## bill depth as a function of bill length, flipper length and body mass. We'll use 
## AIC() and step() to compare this complex model with the simpler models. We'll also
## use vif() to check if we have a problem with multicollinearity.

library(car)  # vif()
library(ggiraph)
library(ggiraphExtra) # ggPredict()

gentoo = penguins %>%
  filter(species=="Gentoo")

## Build simple linear regression.
lm_gentoo_1 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)

## Build multiregression with 2 variables
lm_gentoo_2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data=gentoo)

## Build multiregression with 3 variables
lm_gentoo_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, 
                 data=gentoo)

vif(lm_gentoo_3) ## vif values ~ 2, mild multicollinearity
step(lm_gentoo_3) ## Doesn't remove any variables
AIC(lm_gentoo_1, lm_gentoo_2, lm_gentoo_3) ## lm_gentoo_3 performs best

## Plot predictions: Now let's plot the model predictions. This is where things get 
## a bit tricky, because we are interested in changes along four dimensions (bill depth,
## bill length, flipper length and body mass), but we can only easily visualize 2 
## dimensions in our plots. The simplest solution here is to plot changes in our y 
## variable against changes in our x variables one at a time, while holding the other
## x variables constant. A good way to this is to set 2 of your x variables to their 
## median value in the observations, and then look at how y changes with the remaining
## x variable. We can do this by generating a new data set, and then using the augment()
## function in the tidyverse (or predict() in base R) and plotting just like we did 
## earlier. 

## Look at bill depth ~ body mass while holding bill length and flipper length constant.

## Use expand to get full range of 1 variable, then add in median of other variable(s) 
## from original data.
newdata = gentoo %>% 
  ## We want full range of body mass data (gets rid of other variables).
  expand(body_mass_g) %>% 
  mutate(bill_length_mm = median(gentoo$bill_length_mm, na.rm=TRUE), 
         flipper_length_mm = median(gentoo$flipper_length_mm, na.rm=TRUE))

lm_gentoo_3_predict = lm_gentoo_3 %>%
  augment(newdat = newdata, se_fit=TRUE) %>%
  ## Calculate 95% CI using SE
  mutate(lwr = .fitted - 1.96 * .se.fit, upr = .fitted + 1.96 * .se.fit) 

## Plot the data and the model predictions
ggplot() +
  geom_point(aes(x = body_mass_g, y = bill_depth_mm), data=gentoo) + ## original data
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = body_mass_g), alpha = .15, 
              data=lm_gentoo_3_predict) +
  geom_line(data=lm_gentoo_3_predict, aes(y = .fitted, x = body_mass_g), size = 1) +
  annotate("text", x=4250, y=17, label= paste0("flipper length = ", 
                                               median(gentoo$flipper_length_mm, 
                                                      na.rm=TRUE), "mm")) +
  annotate("text", x=4250, y=16.5, label= paste0("bill length = ", 
                                                 median(gentoo$bill_length_mm, 
                                                        na.rm=TRUE), "mm")) 

## Exercise 5.2: Plot the model predictions from our model lm_gentoo_3 so that we can 
## see the variation in bill depth vs. flipper length while holding bill length and 
## body mass constant at their medians. 

gentoo = penguins %>% filter(species == "Gentoo")
lm_gentoo_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g,
                 data = gentoo)
## Look at bill depth ~ flipper length while holding bill length and body mass constant.
## Use expand to get full range of 1 variable, then add in median of other variable(s)
## from original data.
newdata = gentoo %>%
  ## Full range of data (gets rid of other variables)
  expand(flipper_length_mm) %>%
  mutate(bill_length_mm = median(gentoo$bill_length_mm, na.rm=TRUE), 
         body_mass_g = median(gentoo$body_mass_g, na.rm=TRUE))

lm_gentoo_3_predict = lm_gentoo_3 %>%
  augment(newdat = newdata, se_fit=TRUE) %>%
  ## Calculating 95% CI using SE
  mutate(lwr = .fitted - 1.96 * .se.fit, upr = .fitted + 1.96 * .se.fit)
## Plot the data and the model predictions.
ggplot() +
  ## Original data
  geom_point(aes(x = flipper_length_mm, y = bill_depth_mm), data = gentoo) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = flipper_length_mm), alpha = 0.15,
              data = lm_gentoo_3_predict) +
  geom_line(data = lm_gentoo_3_predict, aes(y = .fitted, x = flipper_length_mm), 
            size = 1)

## ANOVA
## Analysis of Variance (ANOVA) is just a special case of linear regression where the 
## independent predictor variables are all categorical variables. Mathematically, 
## running an ANOVA is the same as running a linear model. You can build your linear 
## model using lm() as before, and then print the output of the model using the function
## anova(), which presents the statistical output in a classic ANOVA table. I always 
## like to emphasize how there are multiple ways to accomplish the same goals in 
## programming. Another way to run an ANOVA is to use the aov() function to run an 
## ANOVA model directly. 

head(penguins)
## Conduct an ANOVA using lm()
penguin_lm = lm(body_mass_g ~ species + sex, data=penguins)
summary(penguin_lm)
anova(penguin_lm)

## Conduct the same ANOVA using aov()
penguin_anova = aov(body_mass_g ~ species + sex, data=penguins)
summary(penguin_anova)

## Note that the output of the anova(penguin_lm) has all of the same statistics and 
## results as summary(penguin_anova). The output tells us that both species and sex are
## significant predictors of penguin body mass, but it doesn't tell us more than that.
## To find out which groups are associated with heavier vs. lighter body mass, simply
## calculate and compare the mean of the two groups. With groups with 3 or more 
## categories, we need to conduct a post hoc Tukey test. 

## which sex has higher body mass?
penguins %>%
  group_by(sex) %>%
  summarize(mean_body_mass_g = mean(body_mass_g))

## which species has higher body mass?
penguins %>%
  group_by(species) %>%
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm=TRUE))

TukeyHSD(penguin_anova)  ## Requires the output of the aov() function

## So the post-hoc Tukey test tells us that Chinstrap and Adelie penguin body mass is 
## not significantly different (p>0.05), but both Chinstrap and Adelie penguins body 
## mass are each significantly different from Gentoo penguins (p<0.05). We know from 
## simply calculating the means for those species separately that, in fact, Adelie and
## Chinstrap body mass is significantly less than Gentoo body mass. Tada!

## ANOVA is pretty robust to not-quite-normal data, but if you have a big normality 
## problem you can switch to the Kruskall Wallace test kruskal.test(). 

## Exercise 5.3: Conduct an ANOVA to determine whether Adelie body mass is significantly 
## different between the three islands where observations were collected. Conduct a 
## post-hoc Tukey test if appropriate. 

penguin_island_anova = aov(body_mass_g ~ sex + island, data=penguins %>%
                             filter(species=="Adelie"))
summary(penguin_island_anova)
## This shows no significant difference in body mass between island. 
