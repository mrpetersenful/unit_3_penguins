## ----setup, include=FALSE-------------------------------------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.asp = 0.618, collapse=TRUE) 


## -------------------------------------------------------------------------------------------
library(tidyverse)
library(palmerpenguins)

gentoo = penguins %>% 
  filter(species=="Gentoo")

# Exploratory data analysis:
glimpse(gentoo)
summarize(gentoo)
ggplot() +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm), data=gentoo)


## -------------------------------------------------------------------------------------------
# Check normality assumption with a qqplot:
ggplot(gentoo) +
  stat_qq(aes(sample=bill_length_mm))
ggplot(gentoo) +
  stat_qq(aes(sample=bill_depth_mm))


## -------------------------------------------------------------------------------------------
# cor() returns just the correlation coefficient r
cor(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm, use="complete.obs")

# cor.test() returns the t-statistic, df, p-value, etc.
cor.test(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm, use="complete.obs")


## -------------------------------------------------------------------------------------------
head(gentoo) # Check which columns would be interesting to include in the correlation matrix
cor(gentoo[,3:6], use="complete.obs")


## ---- fig.height = 6, fig.width = 8, message=FALSE, warning=FALSE---------------------------
library(GGally) # ggpairs()

gentoo %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  ggpairs()


## ---- fig.height = 7, fig.width = 9, message=FALSE, warning=FALSE---------------------------
penguins %>%
  select(species, body_mass_g, ends_with("_mm")) %>% # clever way to select variables with names that end in "_mm"
  GGally::ggpairs(aes(color = species))

