## ----setup, include=FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.asp = 0.618, collapse=TRUE) 


## ---- warning=FALSE, message=FALSE--------------------------------------------------------
library(tidyverse)
library(palmerpenguins)
library(GGally) # ggPairs()
library(ggiraph)
library(ggiraphExtra) # ggPredict()
library(broom)  # tidy() augment()
library(car) # vif()

# Exploratory data analysis:
glimpse(penguins)
summarize(penguins)
penguins %>% 
  select(bill_depth_mm, bill_length_mm) %>%
  GGally::ggpairs()  # calling out the library can avoid ambiguity for common-named functions, or just serve as a reminder to you


## -----------------------------------------------------------------------------------------
lm_1 = lm(bill_depth_mm ~ bill_length_mm, data=penguins)
summary(lm_1)


## -----------------------------------------------------------------------------------------
ggplot(data=penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
     geom_point() +
     geom_smooth(method = "lm")


## -----------------------------------------------------------------------------------------
class(lm_1) # Note that your model output is a variable of the class "lm"
plot(lm_1)  # This actually calls plot.lm() since the first parameter is class "lm"


## -----------------------------------------------------------------------------------------
gentoo = penguins %>% filter(species=="Gentoo")
lm_2 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
summary(lm_2)
ggplot(data=gentoo, aes(x = bill_length_mm, y = bill_depth_mm)) +
     geom_point() +
     geom_smooth(method = "lm")


## -----------------------------------------------------------------------------------------
ggplot(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
     geom_point() +
     geom_smooth(method = "lm")


## -----------------------------------------------------------------------------------------
# Drop NA data before fitting model. This helps me avoid problems down the line with predict()
penguins_lm_3 = penguins %>%
  filter(!is.na(bill_depth_mm),
         !is.na(bill_length_mm),
         !is.na(species))
lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data=penguins_lm_3)


## -----------------------------------------------------------------------------------------
summary(lm_3)
coef(lm_3)
anova(lm_3)
broom::tidy(lm_3, conf.int = TRUE, conf.level = 0.95) # Added confidence intervals to output


## -----------------------------------------------------------------------------------------
library(ggiraph)
library(ggiraphExtra)
ggPredict(lm_3, se=TRUE, interactive=TRUE)


## -----------------------------------------------------------------------------------------
lm_3_predictions = predict(lm_3, interval="confidence") # Calculates lm predictions for the original dataset
head(lm_3_predictions)
penguins_lm_3_predict = cbind(penguins_lm_3, lm_3_predictions)
ggplot(penguins_lm_3_predict, aes(x = bill_length_mm, y = bill_depth_mm, color = species) ) +
     geom_point() +
     geom_ribbon( aes(ymin = lwr, ymax = upr, fill = species, color = NULL), alpha = .1) +
     geom_line(aes(y = fit), size = 1) +
     theme_bw()


## -----------------------------------------------------------------------------------------
# Build a new bill_length_mm dataset that spans the full range of the original data at even intervals
newdata_bill_length_mm = seq(min(penguins_lm_3$bill_length_mm), max(penguins_lm_3$bill_length_mm), by = .1)
# Repeat complete bill_length_mm data for each species
newdata = expand.grid(bill_length_mm = newdata_bill_length_mm, species = unique(penguins_lm_3$species) )
head(newdata)


## -----------------------------------------------------------------------------------------
newdata_predict_lm_3 = cbind(newdata, predict(lm_3, interval="confidence", newdata = newdata))
dim(newdata_predict_lm_3)
ggplot() +
     geom_point(data=penguins_lm_3_predict, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
     geom_ribbon(aes(ymin=lwr, ymax=upr, x = bill_length_mm, fill = species, color = NULL), alpha = .1, data=newdata_predict_lm_3) +
     geom_line(aes(y = fit, x = bill_length_mm, color=species), size = 1, data=newdata_predict_lm_3) +
     theme_bw()


## -----------------------------------------------------------------------------------------
# Get model predictions
lm_3_predict = lm_3 %>%
  augment(penguins_lm_3, se_fit=TRUE) %>%
  mutate(lwr = .fitted - 1.96 * .se.fit, upr = .fitted + 1.96 * .se.fit) # Calculate 95% C.I. using SE
# Plot the data and the model predictions
ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species), data=lm_3_predict) +
  geom_point() +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species, color = NULL), alpha = .15) +
  geom_line( aes(y = .fitted), size = 1)


## -----------------------------------------------------------------------------------------
# Get model predictions with newdata
newdata = penguins_lm_3 %>% expand(bill_length_mm, species)
lm_3_predict = lm_3 %>%
  augment(newdat = newdata, se_fit=TRUE) %>%
  mutate(lwr = .fitted - 1.96 * .se.fit, upr = .fitted + 1.96 * .se.fit) # Calculate 95% C.I. using SE
# Plot the data and the model predictions
ggplot() +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species), data=penguins_lm_3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = bill_length_mm, fill = species, color = NULL), alpha = .15, data=lm_3_predict) +
  geom_line(data=lm_3_predict, aes(y = .fitted, x = bill_length_mm, color=species), size = 1)


## -----------------------------------------------------------------------------------------
ggplot() +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species), data=penguins_lm_3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = bill_length_mm, fill = species, color = NULL), alpha = .15, data=lm_3_predict) +
  geom_line(data=lm_3_predict, aes(y = .fitted, x = bill_length_mm, color=species), size = 1) +
  theme_bw() +
  xlab("Bill length (mm)") + ylab("Bill depth (mm)") +
  ggsave(filename = "figures/bill_depth_model.png", device = "png", width = 5, height = 3, units = "in", dpi = 300)


## ---- eval=FALSE--------------------------------------------------------------------------
## lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data=penguins_lm_3)


## -----------------------------------------------------------------------------------------
lm_4 = lm(bill_depth_mm ~ bill_length_mm*species, data=penguins_lm_3)


## -----------------------------------------------------------------------------------------
summary(lm_3)
summary(lm_4)


## -----------------------------------------------------------------------------------------
AIC(lm_3, lm_4)
best_model = step(lm_4)
best_model


## -----------------------------------------------------------------------------------------
# Plotting predictions with ggPredict(): 
# ggPredict(lm_4, se=TRUE, interactive=TRUE)  # easy, but less customizable

# Get model predictions with newdata
newdata = penguins_lm_3 %>% expand(bill_length_mm, species)
head(newdata)
lm_4_predict = lm_4 %>%
  augment(newdat = newdata, se_fit=TRUE) %>%
  mutate(lwr = .fitted - 1.96 * .se.fit, upr = .fitted + 1.96 * .se.fit) # Calculate 95% C.I. using SE
# Plot the data and the model predictions
ggplot() +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species), data=penguins_lm_3) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = bill_length_mm, fill = species, color = NULL), alpha = .15, data=lm_4_predict) +
  geom_line(data=lm_4_predict, aes(y = .fitted, x = bill_length_mm, color=species), size = 1)


## -----------------------------------------------------------------------------------------
library(car)  # vif()
library(ggiraph)
library(ggiraphExtra) # ggPredict()

gentoo = penguins %>%
  filter(species=="Gentoo")

# Build simple linear regression 
lm_gentoo_1 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)

# Build multiregression with 2 variables
lm_gentoo_2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data=gentoo)

# Build multiregression with 3 variables
lm_gentoo_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, data=gentoo)

vif(lm_gentoo_3) # vif values ~ 2, mild multicollinearity
step(lm_gentoo_3) # Doesn't remove any variables
AIC(lm_gentoo_1, lm_gentoo_2, lm_gentoo_3) # lm_gentoo_3 performs best


## -----------------------------------------------------------------------------------------
### Look at bill depth ~ body mass while holding bill length and flipper length constant

# Use expand to get full range of 1 variable, then add in median of other variable(s) from original data
newdata = gentoo %>% 
  expand(body_mass_g) %>% # Full range of body mass data (gets rid of other variables)
  mutate(bill_length_mm = median(gentoo$bill_length_mm, na.rm=TRUE), 
         flipper_length_mm = median(gentoo$flipper_length_mm, na.rm=TRUE))

lm_gentoo_3_predict = lm_gentoo_3 %>%
  augment(newdat = newdata, se_fit=TRUE) %>%
  mutate(lwr = .fitted - 1.96 * .se.fit, upr = .fitted + 1.96 * .se.fit) # Calculate 95% C.I. using SE

# Plot the data and the model predictions
ggplot() +
  geom_point(aes(x = body_mass_g, y = bill_depth_mm), data=gentoo) + # original data
  geom_ribbon(aes(ymin = lwr, ymax = upr, x = body_mass_g), alpha = .15, data=lm_gentoo_3_predict) +
  geom_line(data=lm_gentoo_3_predict, aes(y = .fitted, x = body_mass_g), size = 1) +
  annotate("text", x=4250, y=17, label= paste0("flipper length = ", median(gentoo$flipper_length_mm, na.rm=TRUE), "mm")) +
  annotate("text", x=4250, y=16.5, label= paste0("bill length = ", median(gentoo$bill_length_mm, na.rm=TRUE), "mm")) 


## -----------------------------------------------------------------------------------------
head(penguins)
# Conduct an ANOVA using lm()
penguin_lm = lm(body_mass_g ~ species + sex, data=penguins)
summary(penguin_lm)
anova(penguin_lm)

# Conduct the same ANOVA using aov()
penguin_anova = aov(body_mass_g ~ species + sex, data=penguins)
summary(penguin_anova)


## -----------------------------------------------------------------------------------------
# which sex has higher body mass?
penguins %>%
  group_by(sex) %>%
  summarize(mean_body_mass_g = mean(body_mass_g))

# which species has higher body mass?
penguins %>%
  group_by(species) %>%
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm=TRUE))

TukeyHSD(penguin_anova)  # Requires the output of the aov() function

