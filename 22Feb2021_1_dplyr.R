## 22Feb2021
## 3.1 Intro to penguins


## The Tidyverse package makes coding much more efficient, easier to write and read, 
## and creates great visualizations. Developed by RStudio's chief scientist Hadley
## Wickham, the Tidyverse provides a well-documented workflow for general data modeling, 
## wrangling, and visualization tasks. The Tidyverse is a collection of R packages built
## around the basic concept that data in a table should have one observation per row, 
## one variable per column, and only one value per cell. 

## First, we have to install the Tidyverse package. We can do this in RStudio by going
## into the Tools tab and going to Install Packages. I can also do this from the 
## command line. 
install.packages("tidyverse")
## You only have to install packages on your computer once. However, to use Tidyverse
## in your R script, you need to load the package library at least every R session 
## where you intend to use that package. 

library("tidyverse")


tidyverse_packages()
## These are the packages that are loaded when you load the Tidyverse library.

## The packages we will be using consistently throughout the rest of the course are 
## dplyr (for data wrangling) and ggplot2 (for visualization). Note that the lubridate
## package that we've already used is part of the Tidyverse as well. 

## The Tidyverse is built around the basic concept that data in a table should have
## one observation per row, one variable per column, and only one value per cell. 
## Once data is in this 'tidy' format, it can be transformed, visualized, and modelled
## for analysis. 

## When using functions in the Tidyverse ecosystem, most data is returned as a tibble
## object. Tibbles are very similar to the data.frames we have been working with, and 
## it is perfectly fine to use Tidyverse functions on a data.frame object. Just be 
## aware that in most cases, the Tidyverse function will transform your data into a 
## tibble. If you're unobservant, you won't even notice a difference. However, there 
## are a few differences between the two flat data types, most of which are just designed
## to make your life easier. The most obvious differences when you're working with
## tibbles is: 

## 1) printing in the console looks different
## 2) never changes the type of the inputs (e.g., it never converts strings to factors)
## 3) never creates row names
## 4) never changes the names of variables
## 5) tibbles generate a warning if the column you're trying to access doesn't exist.

## Some older functions don't work with tibbles. If you find one of these functions, use
## as.data.frame() to turn a tibble back to a data.frame. 

## Example:   my_data = as.data.frame(my_data)


## The dplyr package is designed to make it easier to manipulate flat (2-D) data. dplyr
## provides simple "verbs", functions that correspond to the most common data 
## manipulation tasks, to help you translate your thoughts into code. This package
## also uses efficient backends, so you spend less time waiting for the computer. 
## Here are the most common functions that we will be using in dplyr:

####    filter() chooses rows based on column values.
####    arrange() changes the order of the rows. 
####    select() changes whether or not a column is included. 
####    rename() changes the name of columns. 
####    mutate() changes the values of columns and creates new columns. 
####    summarize() collapses a group into a single row. 
####    group_by() group data into rows with the same values
####    ungroup() remove grouping information from data frame.
####    distinct() remove duplicate rows. 




## I'm also going to install the palmer pengins package.
install.packages("palmerpenguins")

## Now that it's installed, I'm going to load it in.
library(palmerpenguins)


## I just want to check to see what the data looks like now. 
head(penguins)

summary(penguins)
dim(penguins)


## So we look at head all of the time to check out our rows, but the tidyverse
## version of head() is glimpse.
glimpse(penguins)

## Let's check out what the class of our dataset penguins is. We want to have tbl
## dataframes -- actually, that's what it's called in the tidyverse.
class(penguins)


## So our list of functions for the tidyverse is a bit different. They are a list
## of verbs that help visualize what the changes to our data are that we want.

## Now I want to look at the gentoo penguins. With the filter() function, the first
## parameter of these functions is the data that I'm going to apply the function to.
## Then I add in the rest of these 
gentoo = filter(penguins, species=="Gentoo")

## Now I want to look at just the gentoo ladies. 
gentoo_ladies = filter(gentoo, sex=="female")
summary(gentoo_ladies)


## So these are the parameters that we've already done, just out of the big data set
## of penguins. 
gentoo_ladies = filter(penguins, species=="Gentoo", sex=="female")


## However, there is a different way to do this called a pipe %>% and that helps 
## us filter the data without using the filter() function. 
## These two lines of code are equivalent:
gentoo_ladies = filter(penguins, species=="Gentoo", sex=="female")
gentoo_ladies = penguins %>% filter(species=="Gentoo", sex=="female")

## Something that we can do to make readability of our code better (that is pretty
## common) is to include carriage returns after pipes for readability. Ex:
gentoo_ladies = pengins %>%   # include carriage returns after pipe for readability
  filter(sex=="female",       # include carriage returns in list of filter rules
         species=="Gentoo") 



female_mean_mass = penguins %>% 
  filter(sex == "female") %>% 
  summarize(mean_mass_g = mean(body_mass_g))
female_mean_mass

## Okay, so I just created a filtered mean mass for all of the female penguins. 
## Now I want to be be more specific and find out the mean body mass for all 
## of the lady gentoo penguins.
gentoo_ladies_body_mass = gentoo_ladies %>%
  summarize(mean_mass = mean(body_mass_g))   ## Calculate mean of female gentoo body mass
gentoo_ladies_body_mass

## This is more spacing review.
  filter(sex == "female") %>% 
  summarize(mean_mass_g = mean(body_mass_g))


## Compare this to base R code.
female_penguins = penguins[which(penguins$sex == "female"), ] 
female_mean_mass = mean(female_penguins$body_mass_g)

# Or to do it all in one line of code: 
female_mean_mass = mean(unlist(penguins[which(penguins$sex == "female"), "body_mass_g"]))

## Exercise 1.1: 
## Build a data set containing only Chinstrap penguins. Then build another data set 
## that contains only Chinstrap penguins with a flipper length > 200 mm. What is the 
## sex ratio of Chinstrap penguins? How does that compare to the sex ratio of 
## Chinstrap penguins with a flipper length >200 mm? Use the summary() function to
## examine sex ratios. Given this analysis, what do you think the relationship is
## between sex and flipper length? 

chinstrap = penguins %>%
  filter(species == "Chinstrap")
glimpse(chinstrap)

chinstrap_200 = chinstrap %>%
  filter(flipper_length_mm > 200)
glimpse(chinstrap_200)

summarize(chinstrap)   ## For some reason this is not working for me :'(



## -------------------------------------------------------------------------------------------
# Calculate mass of each species
species_mean_mass = penguins %>% 
  group_by(species) %>%
  summarize(mean_mass_g = mean(body_mass_g, na.rm=TRUE)) 

# Calculate mass of each species by sex
species_sex_mean_mass = penguins %>% 
  filter(!is.na(sex)) %>%   # Removes rows where sex is NA. Read the ! as the word "not" here - i.e. it flips the logical value
  group_by(species, sex) %>%
  summarize(mean_mass_g = mean(body_mass_g)) %>%
  print()

# Save the table
write_csv(species_sex_mean_mass, path="data/processed/peguin_mean_body_mass_g.csv")


## -------------------------------------------------------------------------------------------
# Which species has the most observations?
n_by_species = penguins %>%
  group_by(species) %>%
  summarize(n = n()) 

# Use mutate() to convert body mass units:
penguins_for_america = penguins %>%
  mutate(body_mass_lb = body_mass_g * 0.0022) # 0.0022 lb/g

# Quickly display the names of all of the islands surveyed:
penguins %>%
  distinct(island)

# Grab just the species and sex columns:
penguins_brief = penguins %>% 
  select(species, sex)

# Remove bill data:
penguins_no_bill = penguins %>%
  select(-bill_length_mm, -bill_depth_mm)

# Sort data by body mass, then species:
penguins_sorted = penguins %>%
  arrange(body_mass_g, species)

# Sort data by body mass (highest to lowest), then species:
penguins_sorted = penguins %>%
  arrange(rev(body_mass_g), species)

