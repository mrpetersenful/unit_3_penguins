## ----setup, include=FALSE-------------------------------------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.asp = 0.618, collapse=TRUE) 


## ---- message=FALSE-------------------------------------------------------------------------
library(tidyverse)
library(palmerpenguins)
library(rstatix)
library(knitr)  # prints pretty tables


## -------------------------------------------------------------------------------------------
# Two great functions for looking at the first few rows of your variables:
head(penguins)
glimpse(penguins)

# Summary statistics. Note # of observations and NAs
summary(penguins)

ggplot(data=penguins) +
  geom_histogram(aes(x=body_mass_g, fill=species))


## -------------------------------------------------------------------------------------------
# Separate just the Gentoo from all the penguin data
gentoo = penguins %>% 
  filter(species=="Gentoo") 

# Quickly visualize the body mass data
ggplot(data=gentoo) +
  geom_histogram(aes(x=body_mass_g))

# Calculate the mean and standard deviation Gentoo body mass in our data (sometimes base R is more sensible than dplyr)
mean(gentoo$body_mass_g, na.rm=TRUE)
sd(gentoo$body_mass_g, na.rm=TRUE)


## -------------------------------------------------------------------------------------------
# Test for the presence of outliers in the Gentoo body mass data
gentoo %>%
  identify_outliers(body_mass_g)

# Note: here is a result from a made-up dataset where I added an outlier
# data.frame(dat=c(rnorm(100), 312)) %>% identify_outliers()  


## -------------------------------------------------------------------------------------------
# Check normality assumption with a qqplot:
ggplot(gentoo) +
  stat_qq(aes(sample=body_mass_g))


## -------------------------------------------------------------------------------------------
t.test(gentoo$body_mass_g, mu = 5950) # Base R

t_test_results = gentoo %>% t_test(body_mass_g ~ 1, mu = 5950) # dplyr-friendly version
kable(t_test_results)


## -------------------------------------------------------------------------------------------
gentoo %>% cohens_d(body_mass_g ~ 1, mu = 6500)


## -------------------------------------------------------------------------------------------
# Simplify the dataset to what we need
data_for_t_test = penguins %>%
  filter(species %in% c("Gentoo", "Adelie"),
         !is.na(body_mass_g)) %>%
  select(species, body_mass_g) %>%
  droplevels() # This removes the "Chinstrap" level from the species factor

# Calculate summary stats
data_for_t_test %>%
  group_by(species) %>%
  summarize(mean=mean(body_mass_g), sd=sd(body_mass_g))

# Plot a quick histogram:
ggplot(aes(x=body_mass_g), data=data_for_t_test) +
  geom_histogram() +
  facet_wrap(~species)

# Look for the presence of outliers
data_for_t_test %>%
  group_by(species) %>%
  identify_outliers(body_mass_g)

# Check normality assumption with a qqplot:
ggplot(data_for_t_test) +
  stat_qq(aes(sample=body_mass_g)) +
  facet_wrap(~species)

# Check equality of variances
data_for_t_test %>% levene_test(body_mass_g ~ species)


## -------------------------------------------------------------------------------------------
# Base R version:
t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species)

# dplyr-friendly version:
data_for_t_test %>% 
  t_test(body_mass_g ~ species) 

# Calculate the effect size:
data_for_t_test %>%  cohens_d(body_mass_g ~ species)

