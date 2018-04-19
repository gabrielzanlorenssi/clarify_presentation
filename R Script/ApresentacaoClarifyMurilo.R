
# Script da apresentação sobre o Clarify no Departamento de Ciência Política
# dia 2018-04-19

# Criado por Murilo Junqueira

# Data criação: 2018-04-18
# Ultima modificação: 2018-04-18


################## Prepara área de trabalho ##################

#clean memory
rm(list=ls(all=TRUE))
gc()

#install.packages("zeligverse")
library(tidyverse)
library(zeligverse)
library(readstata13)
library(ggplot2)

# Os diretórios de inserção dos dados Brutos (InputFolder), destino dos 
# dados (OutputFolder) e localização dos scripts (ScriptFolder). Atualize se necessário!

# InputFolder <- "C:/Users/mjunqueira/Dropbox/Acadêmico e Educação/Grupos de Estudo/Grupo Lorena/2017-04 Clarify King/Rotina R Local/"
# OutputFolder <- "C:/Users/mjunqueira/Dropbox/Acadêmico e Educação/Grupos de Estudo/Grupo Lorena/2017-04 Clarify King/Rotina R Local/"

InputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Grupos de Estudo/Grupo Lorena/2017-04 Clarify King/Rotina R Local/"
OutputFolder <- "E:/Users/Murilo/Dropbox/Acadêmico e Educação/Grupos de Estudo/Grupo Lorena/2017-04 Clarify King/Rotina R Local/"


# Seed 
set.seed(1234)


# Lê dados Fair
fair <- read.dta13(paste0(InputFolder, "Fair_complete.dta"))

# Checa banco de dados
names(fair)
head(fair)  

################## Simulando os Parâmetros ##################


# Model
model_additive <- lm(data = fair,
                     VOTE ~ GROWTH + INFLATION + GOODNEWS)

# Summary
summary(model_additive)

# Guardas os coeficientes
coefficients <- model_additive$coefficients

# Guarda os erros padrão.
se <- summary(model_additive)[[4]][,2]

# Guarda o valor do sigma
model_sigma <- sigma(model_additive)

# Guarda os graus de liberdade
df <- summary(model_additive)$df[2]

# Verifica os valores
coefficients
se
model_sigma
df

# Determina o número de simulações
M <- 1000

# Cria uma base de dados vazia para agregar os valores
randonBetas <- data.frame(matrix(nrow=M, ncol=0))

# Para cada variável:
for (i in seq_len(length(coefficients))) {
  
  # Debug line
  # i <- 2
  
  # Gera M números aleatório entre 0 e 1
  randonVector <- runif(M)
  
  # Vetor para guardar os valores Beta[i]
  randonBeta.i <- numeric(M)
  
  for (j in seq_len(length(randonVector))) {
    
    # Debug line
    # j <- 1
    
    # Distribuição normal
    #newBeta <- qnorm(p = randonVector[j], 
    #                 mean = coefficients[i], 
    #                 sd = se[i])
    
    # Distribuição t de student
    newBeta <- qt(p = randonVector[j], 
                  df = df)
    
    # Deixa os valores na escala das variáveis
    newBeta <- (newBeta * se[i]) + coefficients[i]
    
    randonBeta.i[j] <- as.numeric(newBeta)
  }
  
  # randonBeta.i
  
  # Transforma em data frame e corrige nome
  randonBeta.i.Format <- randonBeta.i %>% 
    as.data.frame() %>% 
    set_names(names(coefficients[i])) 
  
  # Agrega ao data frame principal
  randonBetas <- cbind(randonBetas, randonBeta.i.Format)
    
  # Libera memória
  rm(randonVector, j, randonBeta.i, newBeta, randonBeta.i.Format)
}
# Libera memória
rm(i)

# Observando o resultado
head(randonBetas)
View(randonBetas)

# Compara os coeficientes com a média das simulações
coefficients
map_dbl(randonBetas, mean)

# O mesmo com os desvios padrão
se
map_dbl(randonBetas, sd)


# Vendo o resultado em gráficos
hist(randonBetas$GROWTH)
plot(density(randonBetas$GROWTH))


################## Predicted values ##################

names(fair)
summary(fair$GROWTH)


# Verificaremos o que ocorre nos valores preditos quando o valor do crescimento
# econômico passa de 0 para 3 pontos

## Estabelece um contrafactual:

# Seleciona as variáveis do modelo
fair.Data <- fair[,names(coefficients)[2:length(coefficients)]] %>% 
  mutate("(Intercept)" = 1) %>% select("(Intercept)", everything())

head(fair.Data)


# Parâmetros todos na média, mas GROWTH = 0
X <- map_dbl(fair.Data, mean)
X[which(names(X) == "GROWTH")] <- 0
X # Verifica o vetor


# Parâmetros todos na média, mas GROWTH = 3
Xc <- map_dbl(fair.Data, mean)
Xc[which(names(X) == "GROWTH")] <- 3
Xc # Verifica o vetor


## Calcula o valor predito para o X

# Verifica as dimensões das matrizes (para ver se elas são multiplicáveis)
names(randonBetas)
names(X)
names(Xc)
dim(as.matrix(randonBetas))
dim(as.matrix(X))
dim(as.matrix(Xc))


# Cria uma variável de erro aleatório:

# Essa parte incorpora a "incerteza fundamental" nas simulações.

# Gera os erros aleatórios (proporcionais ao sigma)
u1 <- qnorm(runif(nrow(randonBetas))) * model_sigma
u2 <- qnorm(runif(nrow(randonBetas))) * model_sigma

# Checa os valores do erro:
head(u1)
head(u2)
hist(u1)
hist(u2)


# Valores preditos de X
yhat.1 <- as.matrix(randonBetas) %*% as.matrix(X) + as.matrix(u1)
head(yhat.1)


# Valores preditos de Xc
yhat.2 <- as.matrix(randonBetas) %*% as.matrix(Xc) + as.matrix(u2)
head(yhat.2)

#compara os resultados
mean(yhat.1[,1])
mean(yhat.2[,1])

mean(yhat.2[,1]) - mean(yhat.1[,1])


## Podemos ver melhor os valores preditos plotando um gráfico

# Um data frame para podermo formatar melhor o gráfico
graph.Data <- cbind(yhat.1[,1], yhat.2[,1]) %>% 
  as.data.frame() %>% 
  setNames(c("GROWTH0", "GROWTH3")) %>% 
  gather(GROWTH_Sim, Vote_Simulation)

# Gráfico de densidade com amplos os valores simulados.
ggplot(graph.Data, aes(x = Vote_Simulation, fill = GROWTH_Sim)) + 
  geom_density(alpha = 0.5)

# Podemos ver que nesse modelo, esses valores de interesses mostra que 
# a vantagem de crescer mais três pontos percentuais é pequena.

# Vamos relembrar o que os coeficientes estava dizendo:
summary(model_additive)
mean(yhat.2[,1]) - mean(yhat.1[,1])

# O resultado é coerente com os coeficientes, afinal...


# Limpa a memória
rm(graph.Data, yhat.1, yhat.2)
rm(u1, u2)


################## Expected values ##################


# Verificaremos o que ocorre nos valores preditos quando o valor do crescimento
# econômico passa de 0 para 3 pontos

# Usaremos novamente X e Xc nessa simulação
X
Xc

# Em primeiro lugar, calculamos os yhat sem o componente aleatório
# systematic component of the model

# Valores preditos de X
yhat.1 <- as.matrix(randonBetas) %*% as.matrix(X)
head(yhat.1)

# Valores preditos de Xc
yhat.2 <- as.matrix(randonBetas) %*% as.matrix(Xc)
head(yhat.2)


## Agora incorpora algum grau de incerteza fundamental nas simulações

# Determine o valor m (página 351)
# Quanto maior o valor de m, menor o peso da incerteza fundamental
m <- 100

# Não confundir M (número de simulações), com m (peso da incerteza fundamental nos expected values)
M
m


# Cria uma base de dados vazia para agregar os valores
ExpectedValues <- matrix(nrow=nrow(yhat.1), ncol=0) %>% data.frame()

# Cria a variável para o crescimento igual a zero e igual a três
ExpectedValues$GROWTH0 <- NA
ExpectedValues$GROWTH3 <- NA


## Dá um peso para a incerteza fundamental em cada vetor beta simulado

# Começando por GROWTH0
for(i in seq_len(nrow(yhat.1))) {
  
  # Cria uma variável de erro aleatório:
  # Essa parte incorpora a "incerteza fundamental" nas simulações.
  # Gera m erros aleatórios (proporcionais ao sigma)
  u <- qnorm(runif(m)) * model_sigma
  
  # Gera m valores aleatórios de Ykc
  Ykc <- rep(yhat.1[1,], m)  + u
  
  # Calcula a média dos valores gerados
  ExpectedValues$GROWTH0[i] <- mean(Ykc)
}


# Agora GROWTH3
for(i in seq_len(nrow(yhat.2))) {
  
  # Cria uma variável de erro aleatório:
  # Essa parte incorpora a "incerteza fundamental" nas simulações.
  # Gera m erros aleatórios (proporcionais ao sigma)
  u <- qnorm(runif(m)) * model_sigma
  
  # Gera m valores aleatórios de Ykc
  Ykc <- rep(yhat.2[1,], m)  + u
  
  # Calcula a média dos valores gerados
  ExpectedValues$GROWTH3[i] <- mean(Ykc)
}

head(ExpectedValues)


#compara os resultados
mean(ExpectedValues$GROWTH0)
mean(ExpectedValues$GROWTH3)

mean(ExpectedValues$GROWTH3) - mean(ExpectedValues$GROWTH0)


## Podemos ver melhor os valores preditos plotando um gráfico

# Um data frame para podermo formatar melhor o gráfico
graph.Data <- ExpectedValues %>% 
  gather(GROWTH_Sim, Vote_Simulation)

# Gráfico de densidade com amplos os valores simulados.
ggplot(graph.Data, aes(x = Vote_Simulation, fill = GROWTH_Sim)) + 
  geom_density(alpha = 0.5)

# Podemos ver que nesse caso, apesar das médias serem parecidas com os 
# predictec values, a distribuição é bem mais concentrada, fazendo com
# que exista menores intecceções entre as distribuições.


# Fim