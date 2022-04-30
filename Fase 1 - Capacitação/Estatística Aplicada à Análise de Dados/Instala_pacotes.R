#Instalando pacotes--------------------------


#Opção mais simples

install.packages("pacman") #instala o pactoes "pacman" - só precisa fazer 1 vez

library(pacman) #carrega o pacote

p_load(lubridate, gridExtra, ggplot2, ggthemes, 
       dplyr, psych, reshape2, corrgram, gridExtra, readxl)


#--------------------------------------------

#Podemos fazer 1 a 1 também.

install.packages("ggplot2") #somente 1 vez
library(ggplot2) #depois é somente carregar as funcionalidades

