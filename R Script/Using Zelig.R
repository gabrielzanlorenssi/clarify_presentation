# Install libraries -------------------------------------------------------

# install.packages("broom")
# install.packages("Zelig")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("haven")
# install.packages('zeligverse')

# Libraries ---------------------------------------------------------------

# regressions
library(broom)

# zelig, from Gary King
library(Zelig)
library(zeligverse)

# data manipulation
library(dplyr)

# plot graphs
library(ggplot2)

# to read stata files
library(haven)

## must be in the working directory
fair <- read_dta("./Fair_complete.dta")

## set seed
seed(1234)

# Using Zelig -------------------------------------------------------------

# library(Zelig)

## Model
zelig <- zelig(data = fair, 
               model = "ls",
               formula = VOTE ~ (GROWTH * GOODNEWS) + INFLATION)

summary(zelig)


# Simulations -------------------------------------------------------------

## Backup
zelig2 <- zelig

## Set values for variables
zelig2 <- setx(zelig2, GROWTH = mean(fair$GROWTH))
zelig2 <- setx1(zelig2, GROWTH = mean(fair$GROWTH) + sd(fair$GROWTH))

summary(zelig2)

## Simulation
zelig3 <- zelig2 ## store in other object

zelig3 <- sim(zelig3)
summary(zelig3)


# Zelig with Tidyverse ----------------------------------------------------

## Generate a dataframe
sims <- fair %>% 
  zelig(VOTE ~ GROWTH * GOODNEWS + INFLATION, model = 'ls', data = .) %>%
  setx(GROWTH = -10:10) %>%
  sim() %>%
  zelig_qi_to_df()

View(sims)


# Plotting EV vs PV -------------------------------------------------------


library(tidyr)

sims %>%
  gather(variable, value, expected_value:predicted_value) %>% 
  ggplot(aes(y=value, x=variable)) +
  geom_boxplot() +
  theme_minimal() +
  labs(y="EV and PV", x="Type")
  

# Scatter --------------------------------------------------------------

ggplot(sims, aes(x=expected_value, y=predicted_value)) + 
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=F) +
  coord_equal() + 
  scale_x_continuous(limits=c(30, 80)) + 
  scale_y_continuous(limits=c(30, 80)) +
  theme_minimal() +
  geom_rug(alpha=0.2) +
  labs(y="PV", x="EV")



# Slimming ----------------------------------------------------------------

sims.slimmed <- qi_slimmer(sims)
View(sims.slimmed)




# Comparing ---------------------------------------------------------------

## YHAT
library(broom)
model <- lm(data = fair, VOTE ~ GROWTH * GOODNEWS + INFLATION)

model %>%
  augment_columns(data=fair) %>% 
  ggplot(aes(x=GROWTH, y=.fitted)) +
  geom_point() +
  geom_smooth(method="lm") +
  ylab('Yhat') +
  scale_y_continuous(limits=c(35,70)) +
  theme_minimal()


# By default, qi_slimmer uses Expected Values. That is the default:
sims.slimmed <- qi_slimmer(sims, qi_type = "ev", ci=0.95)

## Sims slimmed
ggplot(sims.slimmed, aes(GROWTH, qi_ci_median)) +
  geom_ribbon(aes(ymin = qi_ci_min, ymax = qi_ci_max), alpha = 0.3) +
  geom_line() + 
  ylab('Expected Vote') +
  scale_y_continuous(limits=c(35,70))


