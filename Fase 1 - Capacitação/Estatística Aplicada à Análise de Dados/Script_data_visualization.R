#---------------------------------------------------#
# Phortes - Oi MasterDados
# Eduardo Moré de Mattos - eduardo@geplant.com.br
# 05/03/2022
# Manipulação e visualização de dados no R
#---------------------------------------------------#

#Limpando a área de trabalho
rm(list = ls(all = T))

#Redirecionando ao diretório de trabalho
setwd("D:\\Documents\\CURSOS\\R\\EnsinarAprendendo\\3.Dataviz")

#Carregando o banco de dados "mtcars"
mtcars; help(mtcars); data(mtcars)
str(mtcars)
attributes(mtcars)
summary(mtcars)

#Visualizando o banco de dados - PACOTE BÁSICO ------------------------------
plot(mtcars$mpg, mtcars$hp)
plot(hp ~ mpg, data = mtcars)

#Dispersão
with(mtcars, plot(x = mpg, y = hp))
with(mtcars, plot(x = mpg, y = hp, pch = "@")) #muda o marcador

#Dipersão + linha
with(mtcars, plot(x = mpg, y = hp))
abline(h = 200, add = T) #adiciona uma linha horizontal na posição 200
abline(h = 100, col = "red", lty = 2, add = T) #muda as opções de linha

#Dispersão + linha de suavização
scatter.smooth(mtcars$mpg ~ mtcars$hp)

scatter.smooth(mtcars$mpg, mtcars$hp, col = "red",
               xlab = "Miles per gallon", ylab = "Horse power")

scatter.smooth(mtcars$mpg, mtcars$hp, lpars = list(col = "red"),
               xlab = "Miles per gallon", ylab = "Horse power")

scatter.smooth(mtcars$mpg, mtcars$hp, lpars = list(col = "red", lwd = 2, lty = 3),
               xlab = "Miles per gallon", ylab = "Horse power")

#Gráfico de barras e dotplot
barplot(height = mtcars$wt)
barplot(height = mtcars$wt, names.arg = rownames(mtcars))
barplot(height = sort(mtcars$wt), names.arg = rownames(mtcars), horiz = T)

x<- mtcars[order(mtcars$wt),]
dotchart(x$wt, labels = rownames(x))

x<- mtcars[order(mtcars$hp),]
dotchart(x$hp, labels = rownames(x))

#Boxplot
boxplot(mtcars$mpg ~ mtcars$cyl)
boxplot(mtcars$mpg ~ mtcars$cyl, main = "n° de Cilíndros vs. Consumo")

#histogramas
with(mtcars, hist(qsec))
hist(mtcars$qsec)
hist(mtcars$qsec, probability = T)
lines(density(mtcars$qsec), col = "red")

#Matriz de dispersão
pairs(mtcars)
pairs(mtcars, upper.panel = NULL)
pairs(mtcars, upper.panel = NULL, lower.panel = panel.smooth)


#Visualização de dados com o PACOTE LATTICE ---------------------------------
library(lattice)

#Carregando o banco de dados "iris"
iris
data(iris)
str(iris)

#Histogramas em painéis
histogram(~iris$Sepal.Length)
histogram(~iris$Sepal.Length | iris$Species)

#xyplot
xyplot(iris$Petal.Length ~ iris$Petal.Width)
xyplot(iris$Petal.Length ~ iris$Petal.Width, col = iris$Species)
xyplot(iris$Petal.Length ~ iris$Petal.Width | iris$Species)

#Manipulação de dados PACOTE DPLYR ------------------------------------------
library(dplyr)

#Seleção
iris[,c("Petal.Width", "Petal.Length")] #indexação original
iris[,c(2, 3)] #indexação original

iris %>% 
  select(Petal.Width, Petal.Length, Species)

#Filtros
iris[iris$Species == "setosa", ] #indexação original

iris.setosa<- iris %>%
  filter(Species == "setosa")

#Encadeando comandos
iris %>%
  select(Petal.Width, Petal.Length, Species) %>%
  filter(Species == "setosa")

iris %>%
select(Petal.Width, Petal.Length, Species) %>%
  arrange(desc(Petal.Length)) %>% #organiza sentido decrescente
  filter(Species == "setosa")

#Criando variáveis
iris %>%
  mutate(Sepal.Length_m = Sepal.Length/100)

#Sumarização
iris %>%
  group_by(Species) %>%
  summarise(media.petal.w = mean(Petal.Width, na.rm = T))

resumo.esp<- iris %>%
  group_by(Species) %>%
  summarise(media.petal.w = mean(Petal.Width, na.rm = T),
            desvio.petal.w = sd(Petal.Width, na.rm = T),
            erro.petal.w = desvio.petal.w/sqrt(50))

resumo.esp<- iris %>%
  group_by(Species) %>%
  summarise(media.petal.w = mean(Petal.Width, na.rm = T),
            desvio.petal.w = sd(Petal.Width, na.rm = T),
            erro.petal.w = desvio.petal.w/sqrt(n()))

resumo.esp

#Visualização de dados com o PACOTE GGPLOT2 --------------------------------
library(ggplot2)
library(ggthemes)

#Gráfico de barras com as médias por Espécie do banco de dados iris
grafico1<- ggplot(resumo.esp, aes(x = Species, 
                       y = media.petal.w, 
                       fill = Species, 
                       label = round(media.petal.w,2)))+
  geom_bar(stat = "identity")
  #geom_text(vjust = -2, col = "black", size = 4, fontface = "bold")+
  #geom_errorbar(aes(ymin = media.petal.w - erro.petal.w,
  #                  ymax = media.petal.w + erro.petal.w, colour = Species), width = 0.2)+
  #scale_fill_manual(name = " ", values = c("goldenrod", "brown", "tomato"))+
  #scale_color_manual(name = " ", values = c("goldenrod", "brown", "tomato"))+
  #scale_y_continuous(lim = c(0,2.5), breaks = seq(0, 2.5, 0.5))+
  #labs(title = "Meu gráfico",
  #     subtitle = "Curso R",
  #     x = "Espécies",
  #     y = "Média da largura das pétalas (cm)",
  #     caption = "Iris dataset")+
  #theme_hc()+
  #theme(plot.title = element_text(hjust = 0.5),
  #      plot.subtitle = element_text(hjust = 0.5),
  #      axis.title = element_text(face = "bold"),
  #      legend.position = "none")

png("meu_grafico.jpeg", w = 1800, h = 1800, res = 300)
grafico1
dev.off()


#Dispersões e Linhas de tendencia - Banco de dados mtcars
ggplot(mtcars, aes(x = mpg, y = hp, colour = gear))+
  geom_point()

ggplot(mtcars, aes(x = mpg, y = hp, colour = factor(gear)))+
  geom_point()

ggplot(mtcars, aes(x = mpg, y = hp, colour = factor(gear)))+
  geom_point(size = 3, aes(shape = factor(cyl)))
  
ggplot(mtcars, aes(x = hp, y = mpg, colour = factor(gear)))+
  geom_point(aes(size = factor(cyl)))

ggplot(mtcars, aes(x = hp, y = mpg, colour = factor(gear)))+
  geom_point(aes(size = factor(cyl)))+
  geom_smooth()

ggplot(mtcars, aes(x = hp, y = mpg, colour = factor(gear)))+
  geom_point(aes(size = factor(cyl)))+
  geom_smooth(aes(group = 1))

ggplot(mtcars, aes(x = hp, y = mpg, colour = factor(gear)))+
  geom_point(aes(size = factor(cyl)))+
  geom_smooth(aes(group = 1), colour = "red", alpha = 0)+
  theme_bw()

ggplot(mtcars, aes(x = hp, y = mpg, colour = factor(gear)))+
  geom_point(aes(size = factor(cyl)))+
  geom_smooth(aes(group = 1), colour = "red", alpha = 0)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold"))

ggplot(mtcars, aes(x = hp, y = mpg, colour = factor(gear)))+
  geom_point(aes(size = factor(cyl)))+
  geom_smooth(aes(group = 1), colour = "red", alpha = 0)+
  scale_x_continuous(lim = c(50, 350), breaks = seq(50,350,50))+
  scale_y_continuous(lim = c(10, 40), breaks = seq(10,40,10))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold"))

ggplot(mtcars, aes(x = hp, y = mpg, colour = factor(gear)))+
  geom_point(aes(size = factor(cyl)))+
  geom_smooth(aes(group = 1), colour = "red", alpha = 0)+
  scale_x_continuous(lim = c(50, 350), breaks = seq(50,350,50))+
  scale_y_continuous(lim = c(10, 40), breaks = seq(10,40,10))+
  labs(xlab = "Horse Power", ylab = "Miles per gallon")+
  ggtitle("Gráfico teste", subtitle = "03/07/2017")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold"))

ggplot(mtcars, aes(x = hp, y = mpg, colour = factor(gear)))+
  geom_point(aes(size = factor(cyl)))+
  geom_smooth(aes(group = 1), span = 2, colour = "red", alpha = 0)+
  scale_x_continuous(lim = c(50, 350), breaks = seq(50,350,50))+
  scale_y_continuous(lim = c(10, 40), breaks = seq(10,40,10))+
  labs(x = "Horse Power", y = "Miles per gallon")+
  ggtitle(label = "Gráfico", subtitle = "03/07/2017")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom")# "top", "right", "left", "none"

#Bd iris
ggplot(iris, aes(x = Petal.Width, y = Petal.Length))+
  geom_point(aes(colour = Species))+
  facet_wrap(~Species)+
  geom_smooth(method = "lm", colour = "black")+
  theme_few()

ggplot(iris, aes(x = Petal.Width, y = Petal.Length))+
  geom_point(aes(colour = Species))+
  facet_wrap(~Species)+ #comando scales = "free", "free_x", "free_y"
  geom_smooth(method = "lm", colour = "black")+
  theme_few()

ggplot(iris, aes(x = Species, y = Petal.Length))+
  geom_boxplot(aes(fill = Species))+
  scale_fill_manual(values = c("darkgreen", "blue", "darkgoldenrod"))+
  theme_few()

#Mudando o label
levels(iris$Species)<- c("Setosa", "Versicolor", "Virginica")

p1<- ggplot(iris, aes(x = Species, y = Petal.Length))+
  geom_boxplot(aes(fill = Species))+
  scale_fill_brewer(type = "seq", palette = "Greens")+
  theme_few()+
  theme(legend.position = "none")

p2<- ggplot(iris, aes(x = Species, y = Petal.Length))+
  geom_boxplot(aes(fill = Species))+
  scale_fill_brewer(type = "div", palette = "Spectral")+
  theme_few()+
  theme(legend.position = "none")

#Organizando 
library(gridExtra)
grid.arrange(p1, p2, nrow = 1)
grid.arrange(p1, p2, nrow = 2)
grid.arrange(p1, p2, ncol = 2)

#Salvando
png("grafico1.tiff", w = 1800, h = 1500, res = 300)
grid.arrange(p1, p2, nrow = 1)
dev.off()


#Croqui produtividade ---------------------------------------------
rm(list = ls(all = T))

croqui<- read.table("croqui_produtividade.txt", h = T, sep = "\t", dec = ",")
str(croqui)

#Visualizando os resultados do experimento junto ao croqui
ggplot(croqui, aes(x = parcela, y = bloco))+
  geom_tile()
  
ggplot(croqui, aes(x = parcela, y = bloco, fill = exp1))+
  geom_tile()

ggplot(croqui, aes(x = parcela, y = bloco, fill = exp1))+
  geom_tile()+
  geom_text(aes(label = round(exp1, 1)))

ggplot(croqui, aes(x = parcela, y = bloco, fill = exp1))+
  geom_tile()+
  geom_text(aes(label = round(exp1, 1)))+
  scale_x_continuous(lim = c(0.5,13.5), breaks = seq(1,13,1))

ggplot(croqui, aes(x = parcela, y = bloco, fill = exp1))+
  geom_tile()+
  geom_text(aes(label = round(exp1, 1)), fontface = "bold", colour = "white")+
  geom_text(aes(label = trat, vjust = -1, fontface = "bold"))+
  scale_x_continuous(lim = c(0.5,13.5), breaks = seq(1,13,1))+
  scale_fill_gradient(low = "salmon", high = "lightblue")+
  theme_few()

