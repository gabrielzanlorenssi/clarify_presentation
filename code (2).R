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

## Plotting PV x EV
ggplot(sims, aes(x=expected_value, y=predicted_value)) + 
  geom_point(alpha=0.3) +
  geom_smooth(method="lm", se=F) 
  
ggplot(sims, aes(x=expected_value, y=predicted_value)) + 
  geom_point(alpha=0.3) +
  geom_smooth(method="lm", se=F) +
  coord_equal() + 
  scale_x_continuous(limits=c(30, 80)) + 
  scale_y_continuous(limits=c(30, 80))


## Slim
sims.slimmed <- qi_slimmer(sims)
View(sims.slimmed)

# By default, qi_slimmer uses Expected Values. That is the default:
sims.slimmed <- qi_slimmer(sims, qi_type = "pv", ci=0.95)

## Sims slimmed
ggplot(sims.slimmed, aes(GROWTH, qi_ci_median)) +
  geom_ribbon(aes(ymin = qi_ci_min, ymax = qi_ci_max), alpha = 0.3) +
  geom_line() + 
  geom_smooth(data=fair, aes(x=GROWTH, y=VOTE), method="lm") +
  geom_point(data=fair, aes(x=GROWTH, y=VOTE)) +
  ylab('Expected Vote') 

## Add rugplot
ggplot(sims.slimmed, aes(GROWTH, qi_ci_median)) +
  geom_ribbon(aes(ymin = qi_ci_min, ymax = qi_ci_max), alpha = 0.3) +
  geom_line() + 
  geom_rug(data=fair, aes(GROWTH, VOTE), sides = "b", alpha = 0.5, position = "jitter") +
  ylab('Expected Vote') 


# Simulation --------------------------------------------------

# seed
set.seed(1234)

# generate normal x
x <- rnorm(n = 200, mean = 0, sd = 1)
# generate uniform z
z <- runif(n = 200, min = 0, max = 1)
# generate random error
e <- rnorm(n = 200, mean = 0, sd = 1)
# multicolinear a
a <- x + rnorm(n = 200, mean = 0, sd = 1)
cor (a, x)
# generate Y
y <- x + 2*z + e

# create dataframe
df <- data.frame(e, x, y, z)

# regress
zelig(y ~ x + z, model = "ls", data=df)

# setx 
zelig(y ~ x + z, model = "ls", data=df) %>% 
  setx(x = mean(df$x)) %>% 
  sim() %>% 
  zelig_qi_to_df() %>% 
  qi_slimmer()

# setx with multi colinearity
zelig(y ~ x + a + z, model = "ls", data=df) %>% 
  setx(x = mean(df$x)) %>% 
  sim() %>% 
  zelig_qi_to_df() %>% 
  qi_slimmer()

# setx with bias
zelig(y ~ x, model = "ls", data=df) %>% 
  setx(x = mean(df$x)) %>% 
  sim() %>% 
  zelig_qi_to_df() %>% 
  qi_slimmer()

zelig(y ~ x + a + z, model = "ls", data=df) %>% 
  setx(x = mean(df$x)) %>% 
  sim() %>% 
  zelig_qi_to_df() %>% 
  qi_slimmer(qi_type = "pv")





























