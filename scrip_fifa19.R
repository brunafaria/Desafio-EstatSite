# -*- coding: utf-8 -*-
#__author__ = 'Bruna Faria'
#__version__ = '1.0'

#---- Limpeza
rm(list=ls(all=T))

#---- Pacotes
require(dplyr)
require(ggplot2)
require(stringr)
require(Hmisc)
require(fastDummies)

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


#---- Data set iris

df_iris = read.csv2('Iris.csv', sep = ',')
df_iris %>% head()
df_iris %>% dim()

#---- Q7

# Através de um gráfico de dispersão (scatterplot), 
# verifique se há relação linear entre comprimento da pétala (Petal Length) 
# e o comprimento da sépala (Sepal Length). 
# Adicione também diferentes cores aos pontos de acordo com a espécie da flor. 
# A resposta aqui é somente o gráfico, não se preocupe em fazer análises mais aprofundadas.

# R.: Há correlação significativa e positiva entre 'sepal length' e 'petal length' das espécies versicolor e 
# virginia. A correlação da espécie setosa se motrou marginalmente significativa.

ggplot(df_iris,aes(x=as.numeric(as.character(df_iris$SepalLengthCm)), 
                   y=as.numeric(as.character(df_iris$PetalLengthCm)), colour=Species)) +
  geom_point() +
  xlab("Sepal Length (cm)") +
  ylab("Petal Length (cm)") +
  theme_classic() +
  ggtitle("Correletion between sepal length and petal length")

table_corr = df_iris %>% 
  mutate(SepalLengthCm = as.numeric(as.character(SepalLengthCm)),
         PetalLengthCm = as.numeric(as.character(PetalLengthCm))) %>% 
  group_by(Species) %>% 
  group_map(~rcorr(cbind(.$SepalLengthCm, .$PetalLengthCm), type="spearman")) %>% 
  do.call(rbind, .) 

#---- Q8 

# Primeiro, apague a substring “Iris-” da coluna Species. Em seguida, adicione 3 novas colunas 
# à tabela inicial, sendo que cada coluna receberá uma dummy referente a cada uma das species. 
# Ou seja, você deve criar uma coluna chamada Dummy_Setosa, que recebe 1 se a flor for da espécie 
# Setosa e 0 caso contrário. O mesmo para as demais espécies.

df_iris %>% 
  mutate(Species = gsub('Iris-', '', as.character(.$Species))) %>% 
  dummy_cols(., select_columns = "Species")

#---- Data set MGLU3 e lren3

df_mglu = read.csv2('MGLU3.SA.csv', sep = ',')
df_lren = read.csv2('LREN3.SA.csv', sep = ',')

df_mglu %>% head()
df_lren %>% head()

df_mglu %>% dim()
df_lren %>% dim()

#---- Q9 

# Mostre através de um gráfico de linhas a evolução do preço de fechamento 
# das duas ações durante os anos de 2017, 2018 e 2019. No mesmo gráfico, 
# trace um gráfico de linhas pontilhadas com a evolução do preço de abertura 
# das duas ações no mesmo período. Utilize cores diferentes para cada linha e insira uma 
# legenda para as cores/linhas. A legenda deve ficar no canto inferior direito, como este exemplo:

df_mglu %>% 
  mutate(Acao = rep('MGLU3', dim(.)[1])) -> df_mglu

df_lren %>% 
  mutate(Acao = rep('LREN3', dim(.)[1])) -> df_lren


df_acoes = rbind(df_mglu, df_lren)

df_acoes_aux_lren = df_acoes %>% 
  mutate(Date2 = format(as.Date(.$Date), "%Y-%m"), 
         DateMes = format(as.Date(.$Date), "%m"), 
         Ano_Filter = as.numeric(format(as.Date(.$Date), "%Y")),
         Year = as.factor(format(as.Date(.$Date), "%Y")),
         Close = as.numeric(as.character(Close)),
         Open = as.numeric(as.character(Open))) %>% 
  dplyr::filter(Ano_Filter > 2016 & Ano_Filter < 2020 & Acao == 'LREN3') %>% 
  group_by(Date2, DateMes, Year, Acao) %>% 
  summarise(Media_Open = mean(Open, na.rm = T),
            Media_Close = mean(Close, na.rm = T)) 

df_acoes_aux_mglu = df_acoes %>% 
  mutate(Date2 = format(as.Date(.$Date), "%Y-%m"), 
         DateMes = format(as.Date(.$Date), "%m"), 
         Ano_Filter = as.numeric(format(as.Date(.$Date), "%Y")),
         Year = as.factor(format(as.Date(.$Date), "%Y")),
         Close = as.numeric(as.character(Close)),
         Open = as.numeric(as.character(Open))) %>% 
  dplyr::filter(Ano_Filter > 2016 & Ano_Filter < 2020 & Acao == 'MGLU3') %>% 
  group_by(Date2, DateMes, Year, Acao) %>% 
  summarise(Media_Open = mean(Open, na.rm = T),
            Media_Close = mean(Close, na.rm = T)) 

windows(10, 6)
ggplot(df_acoes_aux_lren, aes(x = DateMes, y = Media_Close, group = Year)) +
  theme_classic() +
  geom_line(aes(color=Year), size=1)+
  geom_point(aes(color=Year)) +
  xlab('Close') +
  ylab('Month') +
  geom_line(aes(y = Media_Open, color=Year), size=1, linetype = "dashed") +
  theme(legend.position= c(0.9, 0.15))


windows(10, 6)
ggplot(df_acoes_aux_mglu, aes(x = DateMes, y = Media_Close, group = Year)) +
  theme_classic() +
  geom_line(aes(color=Year), size=1)+
  geom_point(aes(color=Year)) +
  xlab('Close') +
  ylab('Month') +
  geom_line(aes(y = Media_Open, color=Year), size=1, linetype = "dashed") 




