## 1 March 2021
## 3.3 t-tests

## t-tests are used to assess the difference between two means. When conducting a t-test,
## you're testing the null hypothesis (H0) that the means are the same. As a standard
## practice in classical (frequentist) statistics, if the p-value resulting from your
## t-test is < 0.05, you reject the null hypothesis in favor of the alternative 
## hypothesis, which states that the means are significantly different. 

## There are 3 types of t-test:
#### -- a one-sample t-test
#### -- independent sample t-test
#### -- paired t-test

## We'll use the tidyverse and our penguins data to run through examples of each type
## of t-test. We'll also load in the package rstatix because it contains pipe-friendly
## statistics functions that play nicely with the tidyverse. Don't forget to install
## the rstatix package if it is your first time using it! I'm also going to use the 
## kable() function in the knitr package to print out some of the results tables
## neatly for this tutorial. This is just for aesthetics, and is not necessary for any
## of the plotting or statistical tests in this lesson.


library(tidyverse)
library(palmerpenguins)
library(rstatix)
library(knitr)  ## prints pretty tables.

## The one-sample t-test, also known as the single-parameter t-test or single-sample 
## t-test, is used to compare the mean of one sample to a known standard (or 
## theoretical/hypothetical) mean. Generally, the theoretical mean comes from somewhere 
## in the literature or from a previous experiment in your own lab. 

## The one-sample t-test assumes the following characteristics about the data: 
#### -- no significant outliers in the data
#### -- data should be approximately normally distributed

## Before conducting any statistical analyses on your data, it's important to understand
## the data. People often skip this step, but it's important. Here are some good goals 
## for exploratory data analysis:
#### 1) Looking at the raw data values. 
#### 2) Computing summary statistics.
#### 3) Creating data visualizations.

## These are things we've already been doing in the course, but now that we're trying 
## to run a statistical test with the penguins data, we should use the functions in 
## base R and the tidyverse to formally run through these steps. We're going to look
## at the difference in body mass between the three penguin species in our observations:
## Gentoo, Chinstrap, and Adelie. 

## Two great functions for looking at the first few rows of your variables:
head(penguins)
glimpse(penguins)

## Summary statistics. Note # of observations and NAs.
summary(penguins)

ggplot(data=penguins) +
  geom_histogram(aes(x=body_mass_g, fill=species))

## Here's an example. Are the Gentoo penguin body mass observations in our penguins 
## dataset significantly different than the mean Gentoo penguin body mass accepted
## in the literature? We can use the body mass value from the Encyclopedia of Life:

https://eol.org/traitbank

## Search for "Gentoo Penguin" in the search bar and you will find that the trait bank
## lists body mass as 6500g. Let's see what our Gentoo body mass observations look like:

## Here, I'm separating just the Gentoo from all the penguin data.
gentoo = penguins %>% 
  filter(species=="Gentoo") 

## Now take a look at the body mass data.
ggplot(data=gentoo) +
  geom_histogram(aes(x=body_mass_g))

## Now calculate the mean and standard deviation of Gentoo body mass in our data (and
## this time it's going to be easier to use base R than dplyr).
mean(gentoo$body_mass_g, na.rm=TRUE)
sd(gentoo$body_mass_g, na.rm=TRUE)

## Now, before we conduct our one-sample t-test, we need to check assumptions. Are there
## any significant outliers in the data? The function identify_outliers() uses boxplot
## methods to return a data frame of outliers.

## Testing for the presence of outliers in the Gentoo body mass data.
gentoo %>%
  identify_outliers(body_mass_g)

## Note: here is a result from a made-up dataset where I added an outlier
## data.frame(dat=c(rnorm(100), 312)) %>% identify_outliers()  

## The identify_outliers() test returned nothing, so there were no outliers in the 
## Gentoo body_mass_g data. 

## The normality assumption can be checked by plotting the data in a Quantile-Quantile
## plot (QQ plot), and see if it mostly falls along the 1:1 line.

ggplot(gentoo) +
  stat_qq(aes(sample=body_mass_g))

## If the data are not normally distributed, it's recommended to use a non-parametric 
## test such as the one-sample Wilcoxon signed-rank test. This test is similar to the 
## one-sample t-test, but focuses on the median rather than the mean. 

## Now let's do our one-sample t-test to see if our body mass data is significantly 
## different from the body mass value of mu = 6500g published literature:

t.test(gentoo$body_mass_g, mu = 5950) ## Base R function for t-test.

t_test_results = gentoo %>% t_test(body_mass_g ~ 1, mu = 5950) # dplyr-friendly version
kable(t_test_results)

## The results of our one-sample t-test show the following components:
#### -- .y.: the outcome variable used in the test.
#### -- group1,group2: generally, the compared groups in the pairwise tests. Here
####     we have null model (one-sample test). 
#### -- statistic: test statistic (t-value) used to compute the p-value.
#### -- df: degrees of freedom.
#### -- p: p-value.

## The output p-value is much less than 0.05 (it's almost p=0). So we reject our null
## hypothesis that our Gentoo body mass observations are similar to the literature
## value. 

## To calculate an effect size, called Cohen's d, for the one-sample t-test, you need 
## to divide the mean difference by the standard deviation of the difference, as 
## shown below. Note that since mu is a constant: sd(x-mu) = sd(x). 

## Cohen's d formula:
## ---   d = (mean(x) - mu)/sd(x), where:
#### x is a numeric vector containing the data
#### mu is the mean against which the mean of x is compared (default value is mu = 0).

gentoo %>% cohens_d(body_mass_g ~ 1, mu = 6500)

## Recall that, t-test conventional effect sizes are 0.2 (small effect), 0.5 (moderate
## effect), and 0.8 (large effect). As the effect size, d, is -2.82, you can conclude
## that there is a large effect, and our sample data is less than the supplied literature
## value mu (=6500). 

## Welp. That means that our observed mean Gentoo body mass is 5076 g. Our penguins
## were probably pretty skinny. Probably, the Encyclopedia of Life body mass trait is 
## based on adult Gentoo penguins, and we include juveniles in our observations. Or
## maybe the Encyclopedia value is shit. This would be a good jumping off point to 
## do some more research.

## Independent sample t-test:
## The independent samples t-test (or unpaired samples t-test) is used to compare the 
## mean of two independent groups. For example, you could compare the average weights
## of individuals grouped by gender: male and female groups, which are two unrelated/
## independent groups. The independent samples t-test comes in two different forms:

## Assumptions:
#### -- Independence of the observation. There is no relationship between the observations
####      in each group.
#### -- No significant outliers in the groups.
#### -- The two groups of samples should be normally distributed.
#### -- If using the Student's t-test, the variances of the two groups should not be
####      significantly different. This assumption is relaxed in the Welch's t-test.

## Example: Gentoo v. Adelie body mass. Let's use the independent sample t-test to see
## if there is a significant difference in the mean body mass of Gentoo penguins v. 
## Adelie penguins.

## Subsetting our data, removing the NA data, and removing the Chinstrap level from
## the species factor.
data_for_t_test = penguins %>%
  filter(species %in% c("Gentoo", "Adelie"),
         !is.na(body_mass_g)) %>%
  select(species, body_mass_g) %>%
  droplevels() # This removes the "Chinstrap" level from the species factor

## Calculate summary stats
data_for_t_test %>%
  group_by(species) %>%
  summarize(mean=mean(body_mass_g), sd=sd(body_mass_g))

## Plot a quick histogram:
ggplot(aes(x=body_mass_g), data=data_for_t_test) +
  geom_histogram() +
  facet_wrap(~species)

## Look for the presence of outliers
data_for_t_test %>%
  group_by(species) %>%
  identify_outliers(body_mass_g)

## Check normality assumption with a qqplot:
ggplot(data_for_t_test) +
  stat_qq(aes(sample=body_mass_g)) +
  facet_wrap(~species)

## Check equality of variances
data_for_t_test %>% levene_test(body_mass_g ~ species)

## We examined the distribution of body mass observations for Gentoo and Adelie penguins,
## and the histograms looked normal. No outliers were found. The QQ plots looked fine.
## Levene's test checks for equality of variances, and because p>0.05, we accept the 
## null hypothesis that the variances are equal. That means we can use the Student's
## t-test if we want (but we'll be "safe" and use Welch's t-test anyway). 

## Now to run the actual t-test. This is base R version.
t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species)

## dplyr-friendly version:
data_for_t_test %>% 
  t_test(body_mass_g ~ species) 

## Calculate the effect size:
data_for_t_test %>%  cohens_d(body_mass_g ~ species)

## So we can reject the null hypothesis that the means are equal, and accept the 
## alternative hypothesis. Adelie body mass is significantly lower than Gentoo body
## mass (Welch's t-test, p<0.001).

## If we had wanted to run a Student's t-test, we could have just included a parameter
## in the t-test function var.equal=TRUE. An advantage of the dplyr version is that 
## the output is in a simple table, so transferring these results into a report is 
## easier, especially if you have a bunch of t-tests to do and you can build a data 
## frame with a new test result in each row. 

## Paired sample t-test:
## The paired sample t-test is used to compare the means of two related groups of 
## samples. Put into other words, it's used in a situation where there are two pairs
## of values measured for the same samples. For example, you can compare the average
## weight of 20 sea urchins before and after some experimental treatment. The paired
## t-test can be used to compare the mean weights before and after treatment. 

## Assumptions:
#### -- No significant outliers in the differences between groups. 
#### -- The difference of pairs should follow a normal distribution.

## Conducting the paired t-test looks almost identical to the independent sample t-test,
## except that you test your assumptions for outliers and normality on the difference 
## between the paired data (i.e., for each individual urchin, the difference is the 
## weight before treatment minus the weight after treatment). Then run t_test() with 
## the parameter paired=TRUE.

## Note on assumptions: 
### -- Assessing normality: With large enough samples size (n > 30), the violation of
###    the normality assumption should not cause major problems, according to the central
###    limit theorem. This implies that we can ignore the distribution of the data and
###    use parametric tests. However, to be consistent, the Shapiro-Wilk test can be used
###    to ascertain whether data show or not a serious deviation from normality.
###
### -- Assessing equality of variances: Homogeneity of variances can be checked using the
###    Levene's test. Note that, by default, the t_test() function does not assume
###    equal variances; instead of the standard Student's t-test, it uses the Welch
###    t-test by default, which is considered the safer one. To use Student's t-test, 
###    set var.equal = TRUE. The two methods give very similar results unless both 
###    the group sizes and the standard deviations are very different.
### In the situations where the assumptions are violated, non-parametric tests, such 
### as Wilcoxon test, are recommended. 

## Exercise 3.1: Are Adelie penguin flipper lengths significantly different between 
## males and females? Do some exploratory data analysis. Compute summary statistics and
## plot histograms. Then conduct an independent sample t-test. What do your results show?


