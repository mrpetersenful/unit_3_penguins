## 1 March 2021
## 3.4 Correlations


## In stats, we use correlation tests to evaluate the association between two or more
## variables. Pearson correlation (r), measures a linear dependence between two 
## variables (x and y). Pearson correlation is also known as a parametric correlation 
## test because it depends on the distribution of the data. It can only be used when 
## x and y are from normal distribution. The plot of y=f(x) is named the linear 
## regression curve. 

## If your data aren't normal, you can use Kendall tau and Spearman rho, which are 
## rank-based correlation coefficients (non-parametric). 

## Let's see how bill length and bill depth relate to each other. We can start with 
## a simple plot. For now, let's just look at Gentoos.

library(tidyverse)
library(palmerpenguins)

gentoo = penguins %>% 
  filter(species=="Gentoo")

## Exploratory data analysis:
glimpse(gentoo)
summarize(gentoo)
ggplot() +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm), data=gentoo)

## So, as expected, it looks like longer bill length typically corresponds with increased
## bill depth. We can check normality with QQ plots.

ggplot(gentoo) +
  stat_qq(aes(sample=bill_length_mm))
ggplot(gentoo) +
  stat_qq(aes(sample=bill_depth_mm))

## Great, those QQ plots look fine, so we can move on to a correlation analysis. What is
## the correlation between Gentoo penguin bill length and bill depth? We can use cor()
## to calculate Pearson's r correlation coefficient. There's an NA in the Gentoo bill
## data, so we have to handle the missing data with the use parameter in cor(), or 
## the function will throw an error. You can check ?cor to see the options for the use
## parameter. Another option is to filter NAs out of the data before running it through
## the correlation test. You'll note that the default correlation method is Pearson's r,
## but if your data weren't normal, you can use the method parameter in cor() to run the 
## Kendall or Spearman correlation tests with sorted data. 

## cor() returns just the correlation coefficient r
cor(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm, use="complete.obs")

## cor.test() returns the t-statistic, df, p-value, etc.
cor.test(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm, use="complete.obs")

## Interpreting Pearson's r:
#### -- -1 indicates a strong negative correlation: this means that every time x increases,
####     y decreases
#### -- 0 means that there is no association between the two variables (x and y) 
#### -- 1 indicates a strong positive correlation: this means that y increases with x

## So for bill length v. bill depth, Pearson's r = 0.64. That's a pretty solid correlation
## coefficient - there's a positive linear relationship. For cor.test() we can see that 
## the p-value is much less than 0.05, so the correlation is significant. 

## Exercise 4.1: Calculate the correlation between bill length and bill depth with
## all 3 penguin species combined into a single data set. Is the correlation stronger or
## weaker compared to the correlation between bill length and bill depth for Gentoo 
## penguins alone? Do you notice anything unexpected about this correlation?

ggplot(penguins) +
  stat_qq(aes(sample=bill_length_mm))
ggplot(penguins) + 
  stat_qq(aes(sample=bill_depth_mm))

cor.test(x=penguins$bill_length_mm, y=penguins$bill_depth_mm, use="complete.obs")
## Now the correlation is negative, and weaker (-0.235), but still significant. 

## Correlation matrix:
## Now I want to get a quick sense of how all of the variables relate to each other.
## Which penguin body measurements are most related? I can send multiple columns from 
## our data.frame to cor() at once, and it will return a matrix with the correlations
## of each variable vs. all of the others. 

## I'm checking to see which columns would be interesting to include in the correlation
## analysis
head(gentoo) 
cor(gentoo[,3:6], use="complete.obs")
## Welp, everything is related, but the strongest correlation is between body mass 
## and bill depth. 

## If you want to add more pizazz and flair to the correlation analysis, you can use
## ggpairs() in the GGally package. ggpairs() will plot x-y scatterplots between variable
## pairs on the lower left-hand side of the plot matrix and print out correlation 
## coefficients on the upper right hand side of the matrix. The matrix diagonal shows 
## the density plot for each single variable (a density plot is like a histogram, but
## with a line instead of bars). This let's you handle quick data visualization, 
## normality tests and a correlation analysis in a one-stop-shop.


library(GGally) # ggpairs()

gentoo %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  ggpairs()

## If we want to be really fancy, we can run ggpairs() on the complete dataset rather 
## than just the Gentoo penguins. To keep the species separated in our correlation
## matrix, we can map the species variable to a color.

penguins %>%
  ## using ends_with() to select variables with names that end in "_mm".
  select(species, body_mass_g, ends_with("_mm")) %>% 
  GGally::ggpairs(aes(color = species))

## This presents a lot of information. Every correlation presented in this matrix is
## significant. R shows this with the number of asterisks next to the correlation 
## coefficient. That's not a big surprise, different measurements on a bird's body 
## are most likely correlated. It seems like the correlation between body metrics is 
## typically higher in Gentoo penguins than either Chinstrap or Adelie penguins. There's
## also a few relationships where the correlation of the pooled data (i.e., all three
## species) is negative, but each of the species-specific correlations for the same 
## relationship is positive. This seems to be true for the relationship between bill
## depth and any of the other three body measurements. That's interesting, and something
## we'll look out for in future statistical analyses in this dataset.

## Remember: Correlation does not imply causation. It's only a tool to understand the
## relationship. 

