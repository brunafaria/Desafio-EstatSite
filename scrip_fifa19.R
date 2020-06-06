# -*- coding: utf-8 -*-
#__author__ = 'Bruna Faria'
#__version__ = '1.0'

#---- Limpeza
rm(list=ls(all=T))

#---- Pacotes
require(dplyr)
require(ggplot2)
require(stringr)

#---- Leitura base de dados

df_fifa = read.csv2('fifa19.csv', sep = ',')
df_fifa %>% head()
df_fifa %>% dim()

#---- Q1

# Se fossemos classificar a força dos clubes de 
# acordo com a média do campo Overall de seus jogadores, 
# considerando somente clubes com pelo menos 25 jogadores, 
# qual seria o clube mais forte? E o mais fraco?

# Mais forte: Real Madrid (78,2)
# Mais fraco: Crewe Alexandra (56,8)

df_fifa %>% 
  dplyr::select('ID', 'Overall', 'Club') %>% 
  group_by(Club) %>% 
  summarise(Quant = n(),
            Media_Overall = mean(Overall, na.rm=T)) %>% 
  filter(Quant > 25) %>% 
  arrange(Media_Overall)

#---- Q2

# Se fossemos olhar somente para os 20 melhores jogadores de cada seleção, 
# qual nação teria o time mais forte utilizando o critério da média do Overall 
# de seus jogadores? Em outras palavras, filtre somente os 20 melhores jogadores 
# de cada seleção, sendo o critério de “melhor” o campo Overall, e, utilizando 
# o mesmo campo, verifique qual seleção tem a melhor média.

# Real Madrid (85,2)

# 20 melhores jogadores de cada time

df_better_20 = df_fifa %>% 
  dplyr::select('Name', 'Overall', 'Club') %>% 
  group_by(Club) %>% 
  top_n(20, Overall) %>% 
  top_n(20, Name) %>% # quando há empate, seleciona-se os jogadores pelo Nome
  arrange(Club) %>% 
  dplyr::filter(Club != "")


# Melho seleção com os 20 melhores jogadores
df_better_20 %>%
  group_by(Club) %>% 
  summarise(Quant = n(),
            Media_Overall = mean(Overall, na.rm=T)) %>% 
  arrange(-Media_Overall)


#---- Q3

# Neste exercício, considere o campo Release Clause como sendo o valor do jogador. 
# Considerando somente os clubes que possuem mais de 25 jogadores, quais são os 5 clubes mais valiosos?


df_fifa$Release.Clause.Aux = gsub('â‚¬', '', as.character(df_fifa$Release.Clause))
df_fifa$Release.Clause.Aux = gsub('M', '', as.character(df_fifa$Release.Clause.Aux)) %>% 
  as.numeric()

# 5 clubes mais valiosos

df_fifa %>% 
  dplyr::select('ID', 'Release.Clause.Aux', 'Club') %>% 
  group_by(Club) %>% 
  summarise(Quant = n(),
            Mean_Release.Clause = mean(Release.Clause.Aux, na.rm=T)) %>% 
  filter(Quant > 25) %>% 
  arrange(-Mean_Release.Clause) %>% 
  slice(1:5)

#---- Q4

# 11 melhores jogadores de acordo com as condições impostas

df_fifa %>% 
  dplyr::select('Name', 'Age','Release.Clause.Aux', 'Club', 'Overall') %>% 
  dplyr::filter(Age <=29 & Release.Clause.Aux <= 15) %>% 
  arrange(-Overall) %>% 
  slice(1:11) -> df_selec_players

df_selec_players

#----- Q5 

df_selec_players %>% 
  mutate('High_Price' = 
           ifelse(Release.Clause.Aux > median(Release.Clause.Aux, na.rm = T), 1, 0))
df_selec_players

#---- Q6

df_fifa$WeightAux = gsub('lbs', '', as.character(df_fifa$Weight)) %>% as.numeric()
df_fifa$WageAux = gsub('â‚¬', '', as.character(df_fifa$Wage)) 
df_fifa$WageAux = gsub('K', '', as.character(df_fifa$WageAux)) %>% as.numeric()

df_fifa %>%
  ggplot(aes(WeightAux)) +
  geom_histogram(fill="skyblue4", alpha=0.5, position="identity")

df_fifa %>%
  ggplot(aes(Age)) +
  geom_histogram(fill="skyblue4", alpha=0.5, position="identity")

df_fifa %>%
  ggplot(aes(WageAux)) +
  geom_histogram(fill="skyblue4", alpha=0.5, position="identity")
