# Install libraries -------------------------------------------------------

install.packages("broom")
install.packages("Zelig")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("haven")
install.packages('zeligverse')

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


# Seed --------------------------------------------------------------------

set.seed(1234)

# Open Fair Dataset -------------------------------------------------------

## must be in the working directory
fair <- read_dta("./Fair_complete.dta")

## check working directory
getwd()

## set a new working directory
setwd()

# Overview ----------------------------------------------------------------

## View the dataset
View(fair)

## List of variables
glimpse(fair)

## Summary 
summary(fair)

## Summary a specific variable
summary(fair$VOTE)

# Regress (base R) --------------------------------------------------------

## Model
model_additive <- lm(data = fair,
                     VOTE ~ GROWTH + INFLATION + GOODNEWS)

## Summary
summary(model_additive)

## Assessing coefficients
model_additive$coefficients


# Interaction -------------------------------------------------------------

## Model
model_interative <- lm(data = fair,
                       VOTE ~ GROWTH * GOODNEWS + INFLATION)

## Summary
summary(model_interative)


# With fixed effects ------------------------------------------------------

## Generate decade variable
fair2 <- fair %>% 
  mutate(DECADE = floor(YEAR/10)*10)

## View the new variable
head(fair2)

## Model
model_fe <- lm(data = fair2,
               VOTE ~ (GROWTH * GOODNEWS) + INFLATION + factor(DECADE))

## Summary
summary(model_fe)


# Using broom -------------------------------------------------------------

library(broom)

## Tidy
tidy(model_fe)

## Glance
glance(model_fe)

## Augment columns
fair3 <- augment_columns(x = model_fe, data = fair2)

View(fair3)
glimpse(fair3)

## Plotting leverage vs. residuals
ggplot(fair3, aes(x = .std.resid^2, y = .cooksd)) + 
  geom_point() +
  labs(x = "Normalized resid. squared",
       y = "Cook distance (Leverage)")


# Using Zelig -------------------------------------------------------------

# library(Zelig)

## Model
zelig <- zelig(data = fair, 
               model = "ls",
               formula = VOTE ~ (GROWTH * GOODNEWS) + INFLATION)

summary(zelig)
