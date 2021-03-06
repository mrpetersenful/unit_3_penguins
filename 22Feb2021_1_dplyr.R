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


## Now let's look at some penguin data!

## I'm also going to install the palmer pengins package.
install.packages("palmerpenguins")

## Now that it's installed, I'm going to load it in.
library(palmerpenguins)


## I just want to check to see what the data looks like now. 
head(penguins)

summary(penguins)
dim(penguins)


## So we look at head() all of the time to check out our rows, but the tidyverse
## version of head() is glimpse.
glimpse(penguins)

## Let's check out what the class of our dataset penguins is. We want to have tbl
## dataframes -- actually, that's what it's called in the tidyverse.
class(penguins)


## So our list of functions for the tidyverse is a bit different. They are a list
## of verbs that help visualize what the changes to our data are that we want.

## Now we're going to look at how the dplyr functions can be used to subset, transform, 
## and summarize data. First, we're going to use the filter() function in dplyr to grab
## only the gentoo penguins.
gentoo = filter(penguins, species=="Gentoo")
## Here I named a new set of just the Gentoo species of penguin. I can use this parameter
## to filter further down. 

## Now I want to look at just the gentoo ladies. 
gentoo_ladies = filter(gentoo, sex=="female")
summary(gentoo_ladies)

## We can see in the summary output that the number of Adelie and Chinstrap penguins 
## now equals zero and the males and NA's have been zeroed out, also. 


## We could also have separated out the female gentoos in one line of code. Love this.
gentoo_ladies = filter(penguins, species=="Gentoo", sex=="female")


## However, there is a different way to do this called a pipe %>% and that helps 
## us filter the data without using the filter() function. 
## These two lines of code are equivalent:
gentoo_ladies = filter(penguins, species=="Gentoo", sex=="female")
gentoo_ladies = penguins %>% filter(species=="Gentoo", sex=="female")

## All of the dplyr functions take a data frame (or tibble) as the first argument. 
## Rather than forcing the user to either save intermediate objects or nest functions, 
## dplyr provides the pipe operater %>% from the package magrittr. The pipe operator
## allows us to combine multiple operations in R into a single sequential chain of 
## actions. 

## Here's a hypothetical example. I want to perform a sequence of operations on data
## frame x using hypothetical functions f(), g(), and h():
#### 1) Take x then
#### 2) Use x as an input to a function f() then
#### 3) Use the output of f(x) as an input to a function g() then
#### 4) Use the output of g(f(x)) as an input to a function h().

## One way to achieve this sequence of operations is by using nesting parentheses as 
## follows: h(g(f(x))).

## That code isn't so hard to read because we're only applying three functions, and 
## each of the functions is short in its name. Further, each of these functions also
## has only one argument. However, this can get progressively harder to read as the 
## number of functions applied in the sequence increases and the arguments in each
## function increase as well. This is where the pipe operator %>% comes in handy. 
## %>% takes the output of one function and then "pipes" it to be the input of the 
## next function. Furthermore, a helpful trick is to read %>% as "then" or "and then".
## For example, you can obtain the same output as the hypothetical sequence of 
## functions as follows: 

x %>%
  f() %>%
  g() %>%
  h()

## While both approaches achieve the same goal, the latter is much more readable 
## because you can clearly read the sequence of operations line-by-line. 

## Here's how a single transformation with filter() looks with and without a pipe:
gentoo_ladies = filter(penguins, species=="Gentoo", sex=="female")
gentoo_ladies = penguins %>% filter(species=="Gentoo", sex=="female")

## To make the lines using a pipe easier to read, you can include carriage returns 
## after the pipe, as follows.
gentoo_ladies = pengins %>%   # include carriage returns after pipe for readability
  filter(sex=="female",       # include carriage returns in list of filter rules
         species=="Gentoo") 

## This piping style of writing code becomes really efficient when you start to string
## lots of data manipulation tasks together. 

## Now let's use summarize() to find the mean mass (g) of all of the female penguins 
## in the dataset.
female_mean_mass = penguins %>% 
  filter(sex == "female") %>% 
  summarize(mean_mass_g = mean(body_mass_g))
female_mean_mass
## First, I started with the dataset (penguins), then used filter() to separate out 
## females, then used summarize() to find the mean value of the column body_mass_g. 
## So we linked together several actions using the %>% pipe. Note how I used a carriage
## return after every pipe to make my code run more readable. This is standard coding
## etiquette used when writing piped dplyr commands because it's easier to skim 
## and see the actions being performed and in what order. 

## The dplyr syntax is simpler, and the name of the original dataset (penguins) 
## doesn't need to be repeated when using filter(), unlike when using which(). Base
## R also requires the creation of an interim variable, or if you want to fit it all 
## into one line of code, you must use the which() function to subset across rows 
## and columns simultaneously and then unlist() your data.frame or tibble so that it
## can be read into the mean() function as an array. Woof. Go dplyr!!

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
summary(chinstrap)
sex_ratio_all = 34/68

chin_long_wing = chinstrap %>%
  filter(flipper_length_mm > 200)
glimpse(chin_long_wing)

summarize(chin_long_wing)   ## For some reason this is not working for me :'(
## Oh, it's not working because I want summary() lolol.
summary(chin_long_wing)
sex_ratio_chin_long_wing = 1/18
## Clearly the males have longer flipper length than the female in the chinstrap 
## species. 


## More dplyr functions.
## Let's dig in deeper. Now we need to know the mean body mass for each sex of each 
## penguin species. I also want to throw out unknown sex birds so they don't bias 
## the data. We can use group_by() to indicate that I want to consider each sex and 
## each species as a distinct dataset when I use the summarize() function. We'll finish
## it out by printing to the console with print(). These can be valuable summary 
## statistics that we want to refer back to when we are writing up our analysis, or 
## maybe we'll want to include the results in a table in a presentation at a conference.
## Either way, we'll save our new summary table of mean body mass to a .csv in our 
## project folder. 

## First I'm going to calculate the mass of each species. I'm telling it to look at 
## the penguins dataset, then group the set by species, then summarize by looking 
## at the average of the body mass, and removing the NA data. 
species_mean_mass = penguins %>% 
  group_by(species) %>%
  summarize(mean_mass_g = mean(body_mass_g, na.rm=TRUE)) 

## Now I want to calculate the mass of each species by sex. I'm going to use the 
## dataset penguins, then filter by removing the rows where the sex is NA (! means 
## "not" here, so it flips the logical value). After removing the NA from the sex
## data, I'm grouping the dataset by species and by sex. Then I'm summarizing the
## mean of the body mass for each group of data. 
species_sex_mean_mass = penguins %>% 
  filter(!is.na(sex)) %>%   # Removes rows where sex is NA. Read the ! as the word "not" here - i.e. it flips the logical value
  group_by(species, sex) %>%
  summarize(mean_mass_g = mean(body_mass_g)) %>%
  print()

## Now, save the table.
write_csv(species_sex_mean_mass, path="data/processed/peguin_mean_body_mass_g.csv")
## You could also use write.csv(), which is the base R function. 

## So, we just made a pretty in-depth analysis that takes advantage of the consistent
## syntactical format across dplyr functions and avoids unnecessary repetition. If we
## calculated the same summary statistics in base R, we'd have to repeat the analysis
## separately for each combination of species/sex, or write a goddamn stupid for loop
## that stepped through the data subsets. 

## We used the mean() function in our dplyr summarize() command, but there are many
## different summary functions that can be used inside of summarize(): 
#### Center: mean(), median()
#### Spread: sd(), IQR(), mad()
#### Range: min(), max(), quantile()
#### Position: first(), last(), nth()
#### Count: n(), n_distinct()
#### Logical: any(), all()



## Exercise 1.2. Repeat Exercise 1.1, but this time use group_by() along with the n() 
## function inside summarize() to count the number of Chinstrap penguins of each sex.
## Again compare the sex ratio of all Chinstrap observations vs. the sex ratio of 
## Chinstrap penguins with a flipper length > 200 mm. 

penguins %>%
  filter(species == "Chinstrap") %>%
  group_by(sex) %>%
  summarize(n=n())

penguins %>%
  filter(species == "Chinstrap", 
         flipper_length_mm > 200) %>%
  group_by(sex) %>%
  summarize(n=n())



## Here are more examples of what we can do with dplyr functions: group_by(), 
## summarize(), mutate(), distinct(), select(), arrange()

## Which species has the most observations?
n_by_species = penguins %>%
  group_by(species) %>%
  summarize(n = n()) 

## Use mutate() to convert body mass units:
penguins_for_america = penguins %>%
  mutate(body_mass_lb = body_mass_g * 0.0022) ## 0.0022 lb/g

## Quickly display the names of all of the islands surveyed:
penguins %>%
  distinct(island)

## Grab just the species and sex columns:
penguins_brief = penguins %>% 
  select(species, sex)

## Remove bill data:
penguins_no_bill = penguins %>%
  select(-bill_length_mm, -bill_depth_mm)

## Sort data by body mass, then species:
penguins_sorted = penguins %>%
  arrange(body_mass_g, species)

## Sort data by body mass (highest to lowest), then species:
penguins_sorted = penguins %>%
  arrange(rev(body_mass_g), species)


## Exercise 1.3: What is the mean bill length (in inches) of Adelie penguins found 
## on either Dream Island or Biscoe Island? What is the standard deviation? Is the 
## mean larger or smaller than the mean bill length of Adelie penguins found on 
## Torgersen Island?

penguins %>%
  ## Here I'm filtering by species and the two islands.
  filter(species == "Adelie", island %in% c("Biscoe", "Dream")) %>%
  ## Here I'm naming a new column, and changing the bill length to inches.
  mutate(bill_length_in = bill_length_mm * 0.039) %>% ## Conv: 0.0393701 in/mm
  ## Here I'm summarizing the average and std dev of bill length in inches.
  summarize(mean_bill_length_in = mean(bill_length_in), 
            sd_bill_length_in = sd(bill_length_in))

penguins %>%
  filter(species=="Adelie",
         island=="Torgersen",
         ## Removing any NA data in this column.
         !is.na(bill_length_mm)) %>%
  mutate(bill_length_in = bill_length_mm * 0.039) %>%
  summarize(mean_bill_length_in = mean(bill_length_in),
            sd_bill_length_in = sd(bill_length_in))
## The mean of the bill length of the Adelie penguins on Biscoe and Dream island is 
## 0.01 inches smaller than the mean of the bill length of the Adelie penguins on 
## Torgersen Island. 




