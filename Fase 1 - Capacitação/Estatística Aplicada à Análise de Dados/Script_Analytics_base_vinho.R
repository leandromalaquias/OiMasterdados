#---------------------------------------------------#
# Phortes - Oi MasterDados
# Eduardo Moré de Mattos - eduardo@geplant.com.br
# 05/03/2022
# Estatística Aplicada e Análse de dados (Analytics)
#---------------------------------------------------#

# 1. Preparação do ambiente de trabalho-----------------

#Limpando a área de trabalho----
rm(list = ls(all = T))

#Diretório de trabalho----
setwd("C:/Users/User/Desktop/CAPACIT. DATA SCIENCE/Estatística Aplicada à Análise de Dados")

#Carregando os pacotes necessários----
pacman::p_load(ggplot2, ggthemes, dplyr, psych, reshape2, corrgram, gridExtra, readxl)

# 2. Ingestão de dados----------------------------------
dados<- readxl::read_excel("Vinho.xlsx", sheet = "Plan1")
#dados<- read.table("Vinho.txt", h = T, sep = "\t", dec = ",") #"\t" é o separador de texto (tabulação)

#Ajustando a base----
str(dados)
summary(dados)

#Transformando e adicionando variáveis----
dados$id<- rownames(dados)
dados$Qualidade<- as.factor(dados$Qualidade)
dados$Tipo<- as.factor(dados$Tipo)


# 3. Análise exploratória-------------------------------

#formato longo da base----
dados.long<- reshape2::melt(dados, id.vars = c("id", "Tipo", "Qualidade"))

#Tabelas----
table(dados$Qualidade)

prop.table(table(dados$Qualidade))

prop.table(table(dados$Qualidade, dados$Tipo), 2)

round(prop.table(table(dados$Qualidade, dados$Tipo)), 2)
round(prop.table(table(dados$Qualidade, dados$Tipo)), 2)*100

#boxplots----
ggplot(dados, aes(x = Tipo, y = pH))+
  geom_boxplot()+
  stat_summary(geom = "point", col = "red", size = 3, fun = mean)+
  facet_wrap(~Qualidade)+
  labs(title = "Meu boxplot bonitão",
       subtitle = "Acidez dos Vinhos",
       x = "",
       y = "pH",
       caption = "6463 vinhos analisados")
theme_few()


ggplot2::ggplot(dados.long, ggplot2::aes(x = Tipo, y = value, col = Tipo))+
  ggplot2::geom_boxplot()+
  ggplot2::facet_wrap(~variable, scales = "free_y")+
  ggplot2::labs(title = "Comparação entre vinhos por tipo")


ggplot2::ggplot(dados.long, 
                ggplot2::aes(x = Qualidade, y = value, col = Tipo))+
  ggplot2::geom_boxplot()+
  ggplot2::facet_wrap(~variable, scales = "free_y")+
  ggplot2::labs(title = "Comparação entre vinhos por classe de qualidade")


#histogramas----
p1<- ggplot2::ggplot(dados, ggplot2::aes(x = Álcool, fill = Tipo))+
  ggplot2::geom_histogram()+
  ggplot2::labs(y = "Frequência Absoluta")

p2<- ggplot2::ggplot(dados, ggplot2::aes(x = Açúcar, fill = Tipo))+
  ggplot2::geom_histogram()+
  ggplot2::labs(y = "Frequência Absoluta")

gridExtra::grid.arrange(p1, p2, ncol = 2)

#correlações----
with(dados, plot(Álcool, Açúcar))
cor.test(dados$Álcool, dados$Açúcar)

corrgram::corrgram(dados[,2:12], 
                   order = T,
                   lower.panel = corrgram::panel.pts,
                   upper.panel = corrgram::panel.cor,
                   diag.panel = corrgram::panel.density)


# 4. Modelagem de dados -------------------------------

#Visualização----
plot(dados$`Acidez Volátil`, dados$pH)

#Ajuste do modelo (linear)----
mod<- lm(pH ~ `Acidez Volátil`, data = dados)

#Verificando o ajuste----
summary(mod)

#Normalidade dos resíduos----
shapiro.test(mod$residuals)
nortest::lillie.test(mod$residuals)

#Análise gráfica dos resíduos----
par(mfcol = c(2,2)); plot(mod); par(mfcol = c(1,1))

#Predizendo----
dados$pred<- predict(mod, newdata = dados)

#Comparação----
with(dados, plot(pH, pred, xlim = c(2.5, 4), ylim = c(2.5, 4)))
abline(0, 1, col = "red")
