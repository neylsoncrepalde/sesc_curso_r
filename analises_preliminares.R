######################################
## Preojto Rede SESC Ação Comuntária
## Curso de Introdução ao R
## Dia 01 - Introdução ao R e Indicadores
## Prof. Dr. Neylson Crepalde
## Setembro 2019
######################################

# Carregando os pacotes necessários
library(tidyverse)
library(readxl)
library(questionr)

# Carrega o banco de dados
bd = read_xlsx("dados_rede_sesc.xlsx")

# Visualiza os primeiros casos do banco de dados
bd

# Checando os nomes das variáveis
names(bd)

## Algumas análises preliminares

# Nome da instituição
freq(bd$nomeinst)

# Sexo do respondente
freq(bd$sexo)

# Gráfico do sexo do respondente
# Sem labels
ggplot(bd, aes(sexo)) +
  geom_bar()

# Com labels, precisamos de um pequeno pré-processamento
bd %>% 
  group_by(sexo) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x=sexo, y=n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -.5)

