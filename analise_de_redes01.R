#########################
# Aula - Análise de Redes
# Prof. Dr. Neylson Crepalde
# Rede SESC
#########################

# Carregar os pacotes necessários
library(tidyverse)
library(readxl)
library(questionr)
library(igraph)

## Carregando o banco de dados para o R
bd = read_xlsx("dados_rede_sesc.xlsx")
bd

##############
names(bd)  # verifica os nomes das variáveis

##############
# Começando a montar a rede 1
varint1 = bd %>% 
  select(nomeinst, receiver1:receiver1_5)
varint1

## Precisamos transformar os dados que estão em formato
## wide em formato long, ou seja, duas colunas apenas,
## uma para o sender e outra para os receivers
# tidyr
#install.packages("tidyr")

varint1
## Criando um bd com a edge list
el1 = varint1 %>% 
  pivot_longer(-nomeinst) %>% 
  filter(!is.na(value))
el1

## Verificar a classe
class(el1)

# Transformando el1 em uma matriz
el1 = as.matrix(el1)
class(el1)
head(el1)  # exibe os primeiros casos

# Tirar a coluna do meio
el1 = el1[ ,c(1,3)]
head(el1)

## Criar a rede 1 - Colaboração
g1 = graph_from_edgelist(el1, directed = T)
g1

plot(g1, vertex.label.cex = .3,
     edge.arrow.size = .1,
     vertex.size = 4)

## 3 métricas de redes
# Densidade
# NLaçosObservados / NLaçosPossiveis
edge_density(g1)
