##############################################################
# 4.2.3 Criando modelo de regressao e visualizando resultados
##############################################################

# Vizualizar conteudo do dataset iris
iris

# Vizualizar conteudo inicial do dataset iris
head(iris, 10)

# Cria modelo modLM, regressao linear entre Sepal.Length e Petal.Width.     
modLM <- lm(Sepal.Length ~ Petal.Width, data = iris)

# visualizando resumo dos resultados da execucao da regressao linear
summary(modLM)



######################################################################
# 4.2.4 Visualizando os dados e resultados da regressao em um grafico
######################################################################

# Visualizando os dados e resultados da regressao em um grafico
# Plota as informacoes do dataset (Sepal.Length ~ Petal.Width)
plot(Sepal.Length ~ Petal.Width, data = iris)

# Plota a reta resultante dos resultados do modelo
abline(modLM, col='red')



###########################################################
# 4.2.5 Criando o codigo R para uma regressao multivariada
###########################################################
# Quadro 4.1 – Codigo Regressao Multivariada ##############
###########################################################

# Armazena hora, min, seg inicio processo
startTime <- Sys.time()

# PRIMEIRO - Criando funcoes para calcular MSE e RMSE
# onde p=valor da predicao  e  r=valor real  (ambos relativos aos test set)
MSE = function(p, r){mean((p - r)^2)}
RMSE = function(p, r){sqrt(mean((p - r)^2))}

# SEGUNDO - Criando uma lista de preditores e outra dos percentuais de particao do train set
listaPreditores <- c('Sepal.Width',
                     'Petal.Length',
                     'Petal.Width',
                     'Sepal.Width + Petal.Length',
                     'Sepal.Width + Petal.Width',
                     'Petal.Length + Petal.Width')

listaParticaoTrain <- c(95,90,85,80,75,70,65,60)

# TERCEIRO - Inicializando um dataset que ira armazenar os resultados de cada iteracao
dsResultados <- c()

# QUARTO - LOOP com preditores com outro LOOP interno das particoes
# LOOP com preditores
for(preditor in listaPreditores){
  
  # LOOP com particoes
  for(particaoTrain in listaParticaoTrain) {
    
    # Acao 1 - Dividir o dataset original (iris) em irisTrain (train set) e irisTest (test set)
    # set seed garante reprodutibilidade para valores gerados randomnicamente nas funcoes
    set.seed(1234)
    # Gera vetor aleatorio de indices em funcao do valor de particaoTrain atribuido no Loop
    indiceTrain <- sample(1:nrow(iris), nrow(iris) * particaoTrain / 100)
    # Cria dataset irisTrain (train set)
    irisTrain <- iris[indiceTrain,]
    # Cria dataset irisTest (test set)
    irisTest <- iris[-indiceTrain,]
    
    # Acao 2 - Treina modelo em funcao do preditor atribuido no Loop sobre o irisTrain (train set)
    # O codigo desta acao poderia ser escrito em uma unica linha com alguns recursos de programacao
    # Deixamos assim para melhor interpretabilidade
    if(preditor == 'Sepal.Width'){
      set.seed(1234)
      modLM <- lm(Sepal.Length ~ Sepal.Width, data = irisTrain)
    }else if(preditor == 'Petal.Length'){
      set.seed(1234)						
      modLM <- lm(Sepal.Length ~ Petal.Length, data = irisTrain)
    }else if(preditor == 'Petal.Width'){
      set.seed(1234)
      modLM <- lm(Sepal.Length ~ Petal.Width, data = irisTrain)
    }else if(preditor == 'Sepal.Width + Petal.Length'){
      set.seed(1234)
      modLM <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = irisTrain)
    }else if(preditor == 'Sepal.Width + Petal.Width'){
      set.seed(1234)
      modLM <- lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = irisTrain)
    }else if(preditor == 'Petal.Length + Petal.Width'){
      set.seed(1234)
      modLM <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = irisTrain)			
    }else if(preditor == 'Sepal.Width + Petal.Length + Petal.Width'){
      set.seed(1234)
      modLM <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = irisTrain)			
    }
    
    # Acao 3 - Realiza a predicao em funcao dos valores dos preditores do irisTest (test set)
    dsPredicao <- predict(modLM, irisTest)
    # Concatena valor da predicao e valor real num unico dataset
    dsFinal <- as.data.frame(cbind(r=irisTest$Sepal.Length, p=dsPredicao))
    
    # Acao 4 - Calcula MSE e RMSE
    mse1 <- MSE(dsFinal$p,dsFinal$r)
    rmse1 <- RMSE(dsFinal$p,dsFinal$r)
    
    # Acao 5 - Monta e acumula resultado de todas as iteracoes de ambos Loops
    dsResultados <- rbind(dsResultados,
                          cbind(Preditor=preditor,
                                PartTrain=particaoTrain,
                                MSE1=mse1,
                                RMSE1=rmse1)
    )
  }
}

# QUINTO - Mostra resultado (ordenado por RMSE)
dsResultados <- as.data.frame(dsResultados)
print(dsResultados[order(dsResultados$RMSE1),])

# Mostra tempo de execucao
print(Sys.time()-startTime)



#######################################################
# 4.3.1 Visualizando o Dataset Pima Indians Diabetes2 #

# Instala pacote mlbench caso nao esteja instalado
if(!is.element('mlbench', installed.packages()[,1])){
  install.packages('mlbench')
}

# Carrega pacote mlbench
library(mlbench) 

# Carrega dataset PimaIndians Diabetes 2
data(PimaIndiansDiabetes2)

# Visualiza conteudo inicial do dataset Pima Indians Diabetes 2
head(PimaIndiansDiabetes2,10)

# Visualiza tipos de dados do dataset Pima Indians Diabetes 2
str(PimaIndiansDiabetes2)



#############################
# 4.3.2 Ajustando o dataset #
#############################

# Cria dataset dsPima sem valores omissos
dsPima <- na.omit(PimaIndiansDiabetes2)

# Transforma valores da variavel dependente diabetes para 0 e 1
levels(dsPima$diabetes) <- 0:1

# Visualiza tipos de dados do dataset dsPima ajustado
str(dsPima)



#########################################
# 4.3.3 Criando subconjuntos do dataset #
#########################################

# Fixa seed para garantir reproducibilidade
set.seed(1234)

# Gera vetor de indices com 85% do conjunto de dados 
indiceTrain <- sample(1:nrow(dsPima), nrow(dsPima)*0.85)

# Cria dataset dsPimaTrain e verifica dimensao
dsPimaTrain <- dsPima[indiceTrain,]
dim(dsPimaTrain)

# Cria dataset dsPimaTest e verifica dimensao (linhas x colunas)
dsPimaTest <- dsPima[-indiceTrain,]
dim(dsPimaTest)



#################################################
# 4.3.4 Criando o modelo de regressao logistica #
#################################################

# Criando e treinando um modelo de regressao logistica
modLOGI <- glm(diabetes~., data = dsPimaTrain, family = "binomial")

# Visualizando resumo dos resultados do modelo de regressao logistica
summary(modLOGI)



################################################################
# 4.3.5 Realizando a predicao e gerando uma matriz de confusao #
################################################################

# Realiza predicao com base no dataser dsPimaTest
pred <- predict(modLOGI, dsPimaTest,  type="response") 

# Arredonda probabilidades. Se > 0.5 -> 1 caso contrario -> 0 (0.5 e o cut-off)
pred <- round(pred) 

# Visualiza a predicao
pred


# Criando uma matriz de confusao
matConf <- table(Predicao = pred, Real = dsPimaTest$diabetes)

# Visualizando matriz de confusao
matConf

# Calcula e visualiza acuracia
Acuracia <- (36+8)/(36+8+7+8)
Acuracia



##################################################################
# 4.4.1 Fazendo o download e visualizando o Dataset Computadores #
##################################################################

# Instala pacote dplyr caso nao esteja instalado
if(!is.element('dplyr', installed.packages()[,1])){
  install.packages('dplyr')
}
# Instala pacote ggplot2 caso nao esteja instalado
if(!is.element('ggplot2', installed.packages()[,1])){
  install.packages('ggplot2')
}
# Instala pacote factoextra caso nao esteja instalado
if(!is.element('factoextra', installed.packages()[,1])){
  install.packages('factoextra')
}

# Carrega o dataset computadores (importante que o arquiv esteja no diretorio padrao do RStudio)
# Para conhecer o diretorio padrao digite a seguinte linha de comando no RStudio: getwd()
dfCp <- read.csv("datasets_1223_2191_Computers.csv")

# Visualiza estrutura do dataset
str(dfCp)



##########################################
# 4.4.2 Preparando e ajustando o dataset #
##########################################

# Carregando biblioteca dplyr
library(dplyr)

# Eliminando colunas X, cd, multi e premium
dfCp <- dfCp %>%
  select(-c(X, cd, multi, premium))

# Visualizando nova estrutura do dataset
str(dfCp)

# Verificar se existem registros com valores ausentes
dfCp[rowSums(is.na(dfCp))!=0,]

# Redimensionar valore numericos
dfCp <- dfCp %>%
  mutate(price_Red = scale(price),
         speed_Red = scale(speed),
         hd_Red = scale(hd),
         ram_Red = scale(ram),
         screen_Red = scale(screen),
         ads_Red = scale(ads),
         trend_Red = scale(trend))  %>%
  select(price_Red, speed_Red, hd_Red, ram_Red, screen_Red, ads_Red, trend_Red)

# Visualizando estrutura do dataset redimensionado
str(dfCp)



##################################################################
# 4.4.3 Criando modelo de agrupamento com o algoritmo k-means ####
##################################################################
# Quadro 4.9 – Codigo para gerar o grafico do metodo do cotovelo #
##################################################################


# Criar funcao que retorna tot.withinss em funcao de valor de k
fTwss <- function(k) {kmeans(dfCp, k, nstart = 25)$tot.withinss} 

# Definir valor maximo para k
kMax <- 25

# Criar um dataset com os resultados de cada valor de k
dsElbow <- data.frame(k=2:kMax,totwss=sapply(2:kMax, fTwss))

# Plot the graph with gglop
library(ggplot2)
ggplot(dsElbow, aes(x = k, y = totwss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 25, by = 1))



###############################################################
# 4.4.3 Criando modelo de agrupamento com o algoritmo k-means #
###############################################################
# Quadro 4.10 – Codigo modelo e visualizacao dos agrupamentos #
###############################################################

# Roda k-mean com k = 7
Clusters <- kmeans(dfCp, 7, nstart = 25)

# Visualiza clusters em grafico
library(factoextra)
fviz_cluster(Clusters, data = dfCp,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800","#5B9456", 
                         "#C903A3", "#E7DF14","#D01B1B"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)



###############################################################
# 4.4.3 Criando modelo de agrupamento com o algoritmo k-means #
###############################################################
# Quadro 4.11 – Codigo para visualizar resultados do modelo ###
###############################################################

# Mostra qste elementos por cluster
Clusters$size

# Mostra pontuacao de cada cluster
Clusters$centers



###########################################################################
# 4.4.3 Criando modelo de agrupamento com o algoritmo k-means #############
###########################################################################
# Quadro 4.12 – Codigo para combinar numero do cluster ao dataset inicial #
###########################################################################

# Combina clusters com o dataset original criando um dataset Final
dsFinal <- cbind(dfCp,Clusters$cluster) 

# Ver amostra do dataset Final
tail(dsFinal,20