# Debugging Zelig

library(Zelig)
library(Zeligchoice)
library(dplyr)
library(haven)

# Debbuging zelig ---------------------------------------------------------

## Generate a dataframe
fair %>% 
  #   estsimp reg Y X1 X2 X1*X2
  zelig(VOTE ~ GROWTH * GOODNEWS + INFLATION, model = 'ls', data = .) %>% 
  #   setx mean /* Set X’s to their means. */
  setx(GROWTH = mean(fair$GROWTH)) %>% 
  #   simqi /* Report Pr(Y=1) conditional on the X’s */
  sim() %>%
  zelig_qi_to_df() 

## Vary growth
zfair <- fair %>% 
  #   estsimp reg Y X1 X2 X1*X2
  zelig(VOTE ~ GROWTH * GOODNEWS + INFLATION, model = 'ls', data = .) %>% 
  #   setx mean /* Set X’s to their means. */
  setx(GROWTH = -10:10) %>% 
  #   simqi /* Report Pr(Y=1) conditional on the X’s */
  sim() %>%
  zelig_qi_to_df() 


# Set X -------------------------------------------------------------------

# Cria uma copia do modelo colocando os valores para simulação 

 # salva os objetos zelig em obj
  obj = zfair
 # cria uma copia
  copia <- obj$copy()
 # lista de valores
  s <- list(GROWTH = -10:10)
  # controle para número de argumentos
  if (length(s) > 0) {
    hold <- rep(1, length(s))
    for (i in 1:length(s)) {
      hold[i] <- length(s[i][[1]])
    }
  } else {
    hold <- 1
  }
  # estabelece os valores
  if (max(hold) > 1) {
    copia$setrange(GROWTH = -10:10)
  } else {
    copia$setx(GROWTH = -10:10)
  } 
  # retorna o objeto
  copia

# Simulation --------------------------------------------------------------

## Object
obj2 <- copia

## Number  
num = 1000
 
## Copy
s5 <- obj2$copy()

## Simulate
s5$sim(num = num)

##Print
s5
  



