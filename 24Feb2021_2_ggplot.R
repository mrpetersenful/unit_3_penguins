## 24 February 2021
## 3.2: ggplot


## ggplot2 and visualizing data. R has several systems for making graphs, but ggplot2 
## is one of the most elegant and versatile. You can do more faster by learning one 
## system and applying it to many different types of figures. Once you've gone through
## the basics, it can be helpful to look at a ggplot cheat sheet to remind yourself
## about functionality and syntax or to learn more. 

## To use ggplot2, load the tidyverse. Note again, that you can just load the ggplot2
## and that would also give access to all of the ggplot2 functions. Loading the tidyverse
## gives us ggplot2 and dplyr at the same time. 

library(tidyverse)
library(palmerpenguins)

## Note: We are loading a series of packages in this instance, and all of the package
## versions are listed. The warning about conflicts means that there are libraries 
## loaded with functions that have the exact same name as functions in other libraries
## that we loaded. For example, there is a filter() function in the R stats package
## (which is part of base R), and a filter() function in dplyr. 

## If two packages use the same function name, then the package loaded last will hide
## the function from earlier packages. This is called masking. When masking occurs and
## you're not sure which package you loaded last, you can use the find() function to
## figure out which package the function you're using is pulled from. If multiple 
## package names are returned, the first package returned in the vector will be the
## one that is used by default. 

find("filter")

## However, if you want to use the stats package version of filter, you can specify
## exactly what namespace to use in front of the function like this:
stats::filter

## In many scripts, it is helpful to use dplyr's select() function to choose which 
## columns you're interested in keeping. However, another package that is frequently
## used has a select() function, so it's necessary to specify the dplyr package before
## calling select(): 
species_islands = penguins %>% dplyr::select(species, island)

## If you're getting unexpected behavior or output and you've triple-checked your code, 
## it could be that you're not using the function from the package that you want 
## because of masking. 

## With ggplot2, you begin a plot with the function ggplot(). ggplot() creates a
## coordinate system that you can add layers to. The first argument of ggplot() is the 
## dataset to use in the graph. So ggplot(data = penguins) creates an empty graph, but 
## it's not very interesting. 

ggplot(data=penguins)

## You complete your graph by adding one or more layers to ggplot() with the addition
## operator +. The function geom_point() adds a layer of points to your plot, which
## creates a scatterplot. ggplot2 comes with many geom functions that each add a 
## different type of layer to a plot. 

## Each geom function in ggplot2 takes a mapping argument. The visual properties in 
## the plot are mapped onto your dataset using the aesthetics mapping function aes() 
## inside of the geom function. That's a fancy way of saying you use aes() to choose 
## which variable in the data you want to be plotted along the x-axis, and which you
## want to be plotted along the y axis. 

## Let's use ggplot2 to visualize the relationship between penguin body mass and 
## penguin flipper length. To remember what my variable names were, I'm going to 
## look at my data. 


glimpse(penguins)


## Now let's plot!
ggplot(data=penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g))

## Cool, so as flipper length goes up, so does penguin mass. It also prints a warning
## that 2 rows were removed because they had missing values (i.e., NAs). We can check
## this by checking a summary of the data. 

summary(penguins)

## If we wanted to check it more explicitly, we could count the NAs by using dplyr:

penguins %>%
  filter(is.na(body_mass_g)) %>% ## Keep only values that are NA
  summarize(n_na = n())  ## Count the NAs

## However, default strategy with ggplot2 is to warn me that there are NAs, but plot
## the data anyways and leave out the observations that are missing. Sounds bueno to 
## me. 

## How do the mass and flipper length play out for the three different penguin species?
## We can visualize this by making the same plot, but mapping the color aesthetic 
## to species. 

ggplot(data=penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species))

## So mass and flipper length looks pretty similar across all three species, but Gentoo
## penguins are bigger than Adelie and Chinstrap. The color legend provided is made
## for us automatically by ggplot, but we can customize it if we need to. But heavy birds 
## have longer flippers. 

## We'll learn how to run stats and models to explicitly examine the relationship 
## between variables at a later time. However, we can use geom_smooth() to add in a 
## simple smoother line to help guide our eye to see the relationship between these
## two variables. We'll do this by adding geom_smooth() as another layer to our ggplot:

ggplot(data=penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species)) +
  geom_smooth(aes(x=flipper_length_mm, y=body_mass_g))

## Now we're going to make the same scatterplot, but customize the x- and y-axis labels,
## and add a title to the plot. We can do this by adding more layers to our ggplot() 
## call:

ggplot(data=penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species)) + 
  xlab("Flipper length (mm)") +
  ylab("Body mass (g)") +
  ggtitle("Penguins are cute")

## Exercise 2.1: Build a scatter plot of bill depth vs. bill length for Adelie penguins.
## Map the point colors to the island that the Adelie penguin was observed on. Add
## axis labels and a main title. 

ggplot(data=penguins) +
  geom_point(aes(x=bill_depth_mm, y=bill_length_mm, color=island)) +
  xlab("Bill depth (mm)") +
  ylab("Bill length (mm)") +
  ggtitle("Bill depth vs. length by island")

## In this penguin dataset, data were only collected in 2007-2009, so this is a short
## time series. How many penguins of each species were caught in each year? We can use
## dplyr to group our data set by year and species, then use summarize() with the n() 
## function to count penguin observations within each group. Then we can just plot 
## our new summary dataset as lines using geom_line(), just like we used geom_point().

## Here, we're counting penguins caught per species per year.
penguin_ts = penguins %>%
  group_by(species, year) %>%
  summarize(n=n())

ggplot(data=penguin_ts) +
  geom_line(aes(x=year, y=n, color=species))

## Mmkay, so there are fewer Chinstrap observations compared to Adelie and Gentoo, 
## but there doesn't seem to be too much change in variation in observations over time
## for any of the three species. That helps us to rule out gross changes in 
## environmental conditions when we're trying to understand the distribution of our 
## variables. 

## Let's get an idea of the penguin beak data by making a quick histogram. 

ggplot(penguins) + 
  geom_histogram(aes(x=flipper_length_mm))

## Lol so ggplot told me to pick better bins. So I'm going to plot a histogram again, 
## but this time, I'll set binwidth = 5 so that flippers within 5mm are put into the 
## same histogram bin. Also, our histogram probably has 2 peaks because the 2 smaller 
## species are on the left and the big Gentoos are on the right. I'm going to filter 
## the data right in the plot call to look at just the Gentoo penguin flipper length
## histogram:

ggplot(penguins %>% filter(species=="Gentoo")) + 
  geom_histogram(aes(x=flipper_length_mm), binwidth=5)
## This looks more normal. 

## I can also color the histogram bars by species. If I use the color aesthetic, that
## will just outline the feature with color. Instead, I'm going to use the fill 
## aesthetic to fill the bars in with color. Since I'm interested in seeing flipper 
## length histograms for all three species, I'm going to change the bin positions from 
## "stack" (the default) to "identity" so that bins for each species can overlap each
## other. By specifying alpha = 0.5 in my geom_histogram function, I'm making the bar 
## colors to be somewhat transparent, so we can see if multiple colors are overlapping.
## Finally, instead of going with default fill colors, I'm adding another layer 
## scale_fill_manual to specify the exact colors I want to use. 

ggplot(data = penguins) +
  geom_histogram(aes(x = flipper_length_mm, fill = species), alpha = 0.5, binwidth=5, 
                 position="identity") +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4"))
## Great, it's pretty clear now that each species has a pretty normal distribution.

## It may be more intuitive to look at the spread of flipper length data between the
## three species with boxplots. We can use geom_boxplot() to make simple boxplots 
## that compactly display the distribution of a continuous variable. It visualizes 
## five summary statistics (the median, two hinges and two whiskers), and all "outlying"
## points individually. The lower and upper hinges correspond to the first and third 
## quartiles (the 25th and 75th percentiles). The whiskers extend from the hinge to 
## the largest value no further than 1.5 * IQR from the hinge (where IQR is the inter-
## quartile range, or distance between the first and third quartiles). 

## I want to combine the summary statistics of the boxplot along with the actual data
## plotted on top. To add the flipper length observations to our boxplots, we can use
## geom_jitter(), which plots the data with a jitter (random incremental change in 
## position) so that we can easily see points that may have fallen right on top of 
## each other. 

ggplot(penguins) + 
  geom_boxplot(aes(y = flipper_length_mm, x = species)) +
  geom_jitter(aes(y = flipper_length_mm, x = species, color = species), width = 0.2) 

## Now let's build bar charts using geom_bar to help us get to know our data better. 
## We can visualize the number of observations of each sex for the three different
## species to determine whether male and female penguins were sampled evenly. 

ggplot(data = penguins) +
  geom_bar(aes(x = sex, fill = species)) 

## The stacked bar charts show that the number of males and females observed across all 
## three species is pretty even, and there are relatively few NAs, so the sex of most
## penguins could be identified. 

## However, it would be easier to interpret the figure if the bars weren't stacked on
## top of each other, and if each species had its own separate plot. The function 
## facet_wrap in ggplot is a great tool for taking whatever plot you're making and 
## splitting it into several plots according to another variable. 

ggplot(data = penguins) +
  geom_bar(aes(x = sex, fill = species)) +
  ## In the facet wrap, we're telling it to create a new plot for each species, 
  ## and to line them all up into 1 row.
  facet_wrap(~species, nrow = 1)

## Cool. So ggplot split our sex barplots into 3 separate figures, 1 for each species,
## and it automatically labeled the figures. This makes it easy to see the sex 
## distribution for each species as well as the total count across species. Woot!

## Let's make a similar set of bar plots, but this time, we'll visualize how many 
## penguins of each species were observed at each of the three islands in the study. 
## We'll use facet_wrap() again, but we're going to plot the bar charts on their side
## so that the island names (which are long) can fit nicely along the x-axis. We're 
## going to use coord_flip(). 

ggplot(penguins) +
  geom_bar(aes(x = island, fill = species)) +
  facet_wrap(~species, ncol = 1) +
  coord_flip()

## Cool. Now we can see that Adelie were found on three islands, but Gentoo and 
## Chinstrap were each only found on one island. That's important background information
## if I'm going to run an analysis on these penguins. When working with new data, it's 
## smart to visualize it in many different ways to get to know the distributions, 
## what's missing, and the general idiosyncracies of the dataset. Some of these graphs
## may be useful to put into a presentation when you're sharing your data with a group.
## Occasionally, these graphs can be published in papers.

## I'm going to save this graph to get back to it later. I can do this by adding one 
## more layer to the ggplot to save it. 

ggplot(penguins) +
  geom_bar(aes(x = island, fill = species)) +
  facet_wrap(~species, ncol = 1) +
  coord_flip() +
  ggsave(filename = "figures/penguin_species_per_island.png", device = "png", width = 5, 
         height = 4, units = "in", dpi = 300)
## I decided to save it as a .png, and I provided a width and height for the figure in
## units of inches. I also specified that I wanted the output to have a high resolution 
## of 300 dots per inch. I didn't need to include that, since dpi=300 is the default, 
## but now you see how to change it. Note, I also didn't need to specify "png" for the 
## device because ggplot would have been smart enough to notice that my filename ends 
## in .png. 

## There are lots of options for saving figures, and to learn more, you can check through
## the ggsave() documentation to learn more (by entering ??ggsave into the console).

## So .png files are very handy because they have "lossless compression". That means 
## that when you import your .png figures into a powerpoint and then move the 
## powerpoint onto a different advice (like loading it onto large conference software
## before presenting at AGU), you don't end up with crappy low-resolution figures. 
## Erin's been burned in the past by embedding .jpg or .pdf figures. However, if I want
## to bring a saved figure into a different type of software for additional editing 
## (like Adobe Illustrator), I can save my original figures as .pdf because it can be 
## easier to interact with distinct features of the graph, change fonts, etc. 


## I want to see a list of the named colors in R, so I can use the following function:
colors()

## There are additional R packages and crafted color palettes available. 

## I used the scale_fill_manual() function earlier to choose the exact colors that I 
## wanted to fill in the bars of my histogram. There's a lot of functionality in this
## type of function (check the documentation). You can change very specific things 
## about how figures look in ggplot. There's a whole suite of functions in ggplot that
## start with "scale_" followed by an aesthetic (x, y, color, fill, etc.) that you're
## trying to customize. Check the ggplot cheat sheet to get a sense of the broad 
## functionality. 

## You can customize the non-data elements of your plot with a theme. ggplot2 includes
## eight themes by default (shown below), and many more are included in add-on packages
## like ggthemes. 

## For example, we can recreate our first scatterplot with the black and white theme:

ggplot(penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species)) +
  theme_bw()

## Exercise 2.2: Build another scatter plot of bill depth vs. bill length for all three
## penguin species. Map the point colors to the penguin's sex. Use facet_wrap() to 
## plot each species in a separate panel. Look at the documentation for facet_wrap() 
## and play around with the scales parameter. What's the default value? Try plotting 
## with the parameter scales="free". Instead of using the default theme, choose a 
## different pre-packaged theme, and save the plot. 

summary(penguins)
ggplot(data=penguins) +
  geom_point(aes(y=bill_depth_mm, x=bill_length_mm, color=species)) + 
  xlab("Bill depth (mm)") +
  ylab("Bill length (mm)") + 
  ggtitle("Bill depth vs. length by species") + 
  facet_wrap(~species, scales="free") + 
  theme_bw() + 
  ggsave(filename = "figures/bill_depth_v_length.png", device = "png", width = 6,
         height = 3.5, units = "in", dpi = 300)
## The default is to put the plots in one row, not one column. Cool. 

## There's a great book called ggplot2: Elegant graphics for data analysis (link in 
## html file). It goes into depth about underlying theory, and has many examples of
## how to combine the individual pieces to solve practical problems. You can also use 
## the ggplot2 extensions gallery (link available in html file). This site lists
## many packages that extend ggplot2 with new geoms and scales. 
