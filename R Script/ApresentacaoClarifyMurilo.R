
# Script da apresentacao sobre o Clarify no Departamento de Ciencia Politica
# dia 2018-04-19

# Criado por Murilo Junqueira

# Data criacao: 2018-04-18
# Ultima modificacao: 2018-04-18


################## Prepara ?rea de trabalho ##################

#clean memory
rm(list=ls(all=TRUE))
gc()

#install.packages("zeligverse")
library(tidyverse)
library(zeligverse)
library(readstata13)
library(ggplot2)
library(purrr)

# Seed 
set.seed(1234)

# L? dados Fair
fair <- read.dta13("Fair_complete.dta")

# Checa banco de dados
names(fair)
head(fair)  

################## Simulando os Par?metros ##################


# Model
model_additive <- lm(data = fair,
                     VOTE ~ GROWTH + INFLATION + GOODNEWS)

# Summary
summary(model_additive)

# Guardas os coeficientes
coefficients <- model_additive$coefficients

# Guarda os erros padr?o.
se <- summary(model_additive)[[4]][,2]

# Guarda o valor do sigma
model_sigma <- sigma(model_additive)


# Verifica os valores
coefficients
se
model_sigma

# Determina o n?mero de simula??es
M <- 1000

# Cria uma base de dados vazia para agregar os valores
randomBetas <- data.frame(matrix(nrow=M, ncol=0))

# Para cada vari?vel:
for (i in seq_len(length(coefficients))) {
  
  # Debug line
  # i <- 2
  
  # Gera M n?meros aleat?rio entre 0 e 1
  randomVector <- runif(M)
  
  # Vetor para guardar os valores Beta[i]
  randomBeta.i <- numeric(M)
  
  for (j in seq_len(length(randomVector))) {
    
    # Debug line
    # j <- 1
  
    newBeta <- qnorm(p = randomVector[j], 
                     mean = coefficients[i], 
                     sd = se[i])
    
    randomBeta.i[j] <- as.numeric(newBeta)
  }
  
  # randonBeta.i
  
  # Transforma em data frame e corrige nome
  randomBeta.i.Format <- randomBeta.i %>% 
    as.data.frame() %>% 
    setNames(names(coefficients[i])) 
  
  # Agrega ao data frame principal
  randomBetas <- cbind(randomBetas, randomBeta.i.Format)
    
  # Libera memoria
  rm(randomVector,randomBeta.i, newBeta, randomBeta.i.Format)
}


# Observando o resultado
head(randomBetas)
View(randomBetas)


# Vendo o resultado em graficos
qplot(randomBetas$GROWTH)
qplot(randomBetas$GROWTH, geom = "density")



################## Predicted values ##################

names(fair)
summary(fair$GROWTH)


# Verificaremos o que ocorre nos valores preditos quando o valor do crescimento
# econ?mico passa de 0 para 3 pontos


## Estabelece um contrafactual:

# Seleciona as vari?veis do modelo
fair.Data <- fair[,names(coefficients)[2:length(coefficients)]] %>% 
  mutate("(Intercept)" = 1) %>% select("(Intercept)", everything())

# Parametros todos na media, mas GROWTH = 0
X <- purrr::map_dbl(fair.Data, mean)
X[which(names(X) == "GROWTH")] <- 0
X # Verifica o vetor

# Parametros todos na media, mas GROWTH = 3
Xc <- purrr::map_dbl(fair.Data, mean)
Xc[which(names(X) == "GROWTH")] <- 3
Xc # Verifica o vetor


# Calcula o valor predito para o X

# Verifica as dimensoes das matrizes (para ver se elas sao multiplicaveis)
names(randomBetas)
names(X)
names(Xc)
dim(as.matrix(randomBetas))
dim(as.matrix(X))
dim(as.matrix(Xc))


##### Adicionar Um valor aleatario


# Valores preditos de X
yhat.1 <- as.matrix(randomBetas) %*% as.matrix(X)
head(yhat.1)

# Valores preditos de Xc
yhat.2 <- as.matrix(randomBetas) %*% as.matrix(Xc)
head(yhat.2)

mean(yhat.1[,1])
mean(yhat.2[,1])

mean(yhat.2[,1]) - mean(yhat.1[,1])


graph.Data <- cbind(yhat.1[,1], yhat.2[,1]) %>% 
  as.data.frame() %>% 
  setNames(c("GROWTH0", "GROWTH3")) %>% 
  gather(GROWTH_Sim, Vote_Simulation)


ggplot(graph.Data, aes(x = Vote_Simulation, fill = GROWTH_Sim)) + 
  geom_density(alpha = 0.5)


################## Expected values ##################


