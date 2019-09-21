#########################
# Aula - Análise de Redes
# Prof. Dr. Neylson Crepalde
# Rede SESC
#########################

# Carregar os pacotes necessários
library(tidyverse)  # manipulação de dados
library(readxl)     # leitura de dados em excel
library(questionr)  # estatísticas descritivas
library(igraph)     # ARS

## Carregando o banco de dados para o R
bd = read_xlsx("dados_rede_sesc.xlsx", sheet = 1)
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

#########################
## Visualização de redes
# Visualização padrão é ruim
plot(g1)

# Precisamos de mais informações
# para construir uma visualização
# eficiente para a rede
# Define o layout

# Algoritmo Fruchterman-Rheingold
l = layout_with_fr(g1)
# Algoritmo Kamada Kawai
#l = layout_with_kk(g1) 
# Algoritmo em círculo
# Útil para perceber as conexões
#l = layout_in_circle(g1)
# Escalonamento multidimensional
# Cálculo de distância euclidiana
#l = layout_with_mds(g1)
# Layout em esfera
#l = layout_on_sphere(g1)
# Layout aleatório
#l = layout_randomly(g1)
plot(g1,
     vertex.size = 4, # tamanho do vért.
     edge.arrow.size = 0.2, # tamanho da seta
     vertex.label = NA,
     vertex.color = 'green',
     edge.color = 'grey',
     layout = l)

### Juntando atributos
## Vamos juntar dois atributos presentes
## no banco nessa rede que estamos
# analisando. Vamos escolher um atributo
# qualitativo e um atributo quantitativo
names(bd)

# Verificando os tipos de atividade
freq(bd$ativ1)

## Vamos juntar o tipo de atividade
## usando como chave o nome da inst.
t1 = tibble(nomeinst = V(g1)$name  )
t1
t2 = bd %>% select(nomeinst, ativ1, nrem)
t2

t = t1 %>% left_join(t2, by = "nomeinst")

t$ativ1[is.na(t$ativ1)] = "Sem informação"

freq(t$ativ1)

## Criando cor para cada atividade
t = t %>% 
  mutate(cores = case_when(
    ativ1 == "Arte/Cultura" ~ 'blue',
    ativ1 == "Assitência Social" ~ 'orange',
    ativ1 == "Educação" ~ 'green',
    ativ1 == "Saúde" ~ 'red',
    ativ1 == "Sem informação" ~ "grey",
    TRUE ~ "yellow"
  ))
t

## Agora vamos alocar a variável ativ1
## já tratada na rede como atributo
V(g1)$ativ1 = t$ativ1
g1
V(g1)$cores = t$cores
g1
V(g1)$nrem = t$nrem

# Imputar 0 e adicionar 1 para todos
V(g1)$nrem[is.na(V(g1)$nrem)] = 0
V(g1)$nrem = V(g1)$nrem + 1
V(g1)$nrem
# Vamos utilizar as cores para plotar
# a rede por atividade
plot(g1,
     vertex.label=NA,
     #vertex.size = 6,
     edge.arrow.size = 0.2,
     vertex.color = V(g1)$cores,
     vertex.size = V(g1)$nrem/15)

legend("bottomright",
       c("Arte/Cultura", "Ass. Soc",
         "Educação", "Saúde", 
         "Outros", "Sem info"),
       fill = c('blue', 'orange', 'green',
               'red', 'yellow', 'grey'),
       cex=0.4)

#### Extraindo a matriz de adjacência
M1 = get.adjacency(g1)
M1

#############################
## Indicadores de rede

# Temos 2 níveis de indicadores de rede
# 1 nível são os indicadores da própria rede
# outro nível são os indicadores ao nível individual
##################
# Antes, vamos retirar um vértice intitulado 
# TIRAR para não viesar as análises.

g1 = delete_vertices(g1, "TIRAR")
g1

## Algumas métricas das rede:::
# Densidade
# NLaçosObservados / NLaçosPossiveis
# NLaçosObservados / (NVértices - 1)
help("edge_density")

# Densidade é uma métrica que varia de 0 a 1
# 0 representa um grafo sem nenhuma conexão.
# 1 representa um grafo onde todos os nós
# possuem laços com todos os outros.
edge_density(g1)

# Diâmetro
# A maior distância geodésica entre 2 nós.
diameter(g1)

# Distância geodésica média
# Média de todos as distâncias geodésicas entre
# todos os nós
mean_distance(g1)

# Transitividade
# Coeficiente de clusterização
?transitivity
transitivity(g1)

# Grau médio
mean(degree(g1))

#################################
# Métricas no nível individual
# Centralidade

# Um nó é mais central com relação ao grau
# quanto mais conexões ele possui
# Centralidade de grau total
grau = degree(g1)
sort(grau)

# Como a rede é direcional, podemos calcular de maneira
# separada o grau de entrada e o grau de saída
# O grau de entrada está relacionado a prestígio, 
# poder
indeg = degree(g1, mode = "in")
sort(indeg)

# O grau de saída está relacionado a atividade
outdeg = degree(g1, mode = "out")
sort(outdeg)

# Centralidade de Intermediação
# Betweeness Centrality
# Quanto mais um ator faz ponte ou conexão entre 
# outros dois, maior será sua centralidade
# de intermediação
inter = betweenness(g1)
sort(inter)

### Centralidade de proximidade
## Closeness centrality
proximidade = closeness(g1)
sort(proximidade)

#############################
# Constraint
constrangimento = constraint(g1)
sort(constrangimento, decreasing = T)
