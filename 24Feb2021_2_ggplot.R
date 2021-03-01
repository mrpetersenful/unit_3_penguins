## ----setup, include=FALSE-------------------------------------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.asp = 0.618, collapse=TRUE) 
knitr::opts_chunk$set(echo = TRUE, warning=FALSE)


## ---- warning=FALSE, message=FALSE----------------------------------------------------------
library(tidyverse)
library(palmerpenguins)


## -------------------------------------------------------------------------------------------
find("filter")


## ---- eval=FALSE----------------------------------------------------------------------------
## stats::filter


## -------------------------------------------------------------------------------------------
species_islands = penguins %>% dplyr::select(species, island)


## ---- eval=FALSE----------------------------------------------------------------------------
## ggplot(data=penguins)


## -------------------------------------------------------------------------------------------
glimpse(penguins)


## ---- warning=TRUE--------------------------------------------------------------------------
ggplot(data=penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g))


## -------------------------------------------------------------------------------------------
summary(penguins)


## -------------------------------------------------------------------------------------------
penguins %>%
  filter(is.na(body_mass_g)) %>% # Keep only values that are NA
  summarize(n_na = n())  # Count the NAs


## -------------------------------------------------------------------------------------------
ggplot(data=penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species))


## -------------------------------------------------------------------------------------------
ggplot(data=penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species)) +
  geom_smooth(aes(x=flipper_length_mm, y=body_mass_g))


## -------------------------------------------------------------------------------------------
ggplot(data=penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species)) + 
  xlab("Flipper length (mm)") +
  ylab("Body mass (g)") +
  ggtitle("Penguins are cute")


## -------------------------------------------------------------------------------------------
# Count number of observations per species per year
penguin_ts = penguins %>%
  group_by(species, year) %>%
  summarize(n=n())

ggplot(data=penguin_ts) +
  geom_line(aes(x=year, y=n, color=species))


## -------------------------------------------------------------------------------------------
ggplot(penguins) + 
  geom_histogram(aes(x=flipper_length_mm))


## -------------------------------------------------------------------------------------------
ggplot(penguins %>% filter(species=="Gentoo")) + 
  geom_histogram(aes(x=flipper_length_mm), binwidth=5)


## -------------------------------------------------------------------------------------------
ggplot(data = penguins) +
  geom_histogram(aes(x = flipper_length_mm, fill = species), alpha = 0.5, binwidth=5, position="identity") +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4"))


## -------------------------------------------------------------------------------------------
ggplot(penguins) + 
  geom_boxplot(aes(y = flipper_length_mm, x = species)) +
  geom_jitter(aes(y = flipper_length_mm, x = species, color = species), width = 0.2) 


## -------------------------------------------------------------------------------------------
ggplot(data = penguins) +
  geom_bar(aes(x = sex, fill = species)) 


## -------------------------------------------------------------------------------------------
ggplot(data = penguins) +
  geom_bar(aes(x = sex, fill = species)) +
  facet_wrap(~species, nrow = 1)  # Create a new plot for each species, line them all up into 1 row


## -------------------------------------------------------------------------------------------
ggplot(penguins) +
  geom_bar(aes(x = island, fill = species)) +
  facet_wrap(~species, ncol = 1) +
  coord_flip()


## -------------------------------------------------------------------------------------------
ggplot(penguins) +
  geom_bar(aes(x = island, fill = species)) +
  facet_wrap(~species, ncol = 1) +
  coord_flip() +
  ggsave(filename = "figures/penguin_species_per_island.png", device = "png", width = 5, height = 4, units = "in", dpi = 300)


## ---- eval=FALSE----------------------------------------------------------------------------
## colors()


## -------------------------------------------------------------------------------------------
ggplot(penguins) + 
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=species)) +
  theme_bw()

