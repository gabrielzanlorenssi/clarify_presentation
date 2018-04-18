# Debugging Zelig

# Debbuging zelig ---------------------------------------------------------

## Generate a dataframe
zfair <- fair %>% 
  zelig(VOTE ~ GROWTH * GOODNEWS + INFLATION, model = 'ls', data = .)

  sim() %>%
  zelig_qi_to_df()


## Set x

## Cria uma copia do modelo colocando os valores para simulação 

 # salva os objetos zelig em obj
  obj = zfair
 # cria uma copia
  x5 <- obj$copy()
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
    x5$setrange(GROWTH = -10:10)
  } else {
    x5$setx(GROWTH = -10:10)
  } 
  # retorna o objeto
  x5


## Sim
obj <- sims

function (obj, x, x1, y = NULL, num = 1000, bootstrap = F, bootfn = NULL, 
          cond.data = NULL, ...)
  
{
  
  
  if (!missing(x1)) {
    s15 <- x1$copy()
    if (!is.null(s15$setx.out$x)) {
      s5$setx.out$x1 <- s15$setx.out$x
      s5$bsetx1 <- TRUE
    }
    if (!is.null(s15$setx.out$range)) {
      s5$range1 <- s15$range
      s5$setx.out$range1 <- s15$setx.out$range
      s5$bsetrange1 <- TRUE
    }
  }
  if (missing(x)) 
    s5 <- obj$copy()
  s5$sim(num = num)
  return(s5)
}




