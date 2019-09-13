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

## Para visualizar com eficiência a informação
## do sexo, vamos utilizar um gráfico de barras
barplot(table(bd$sexo))

# Gráfico do sexo do respondente
# Sem labels
ggplot(bd, aes(sexo)) +
  geom_bar()


## Gráfico de barras com ggplot mais elaborado
bd %>% 
  ggplot(aes(x=sexo,fill=sexo)) +
  geom_bar() +
  theme_bw() +
  labs(x="Sexo", y="Frequência",
       title = "Sexo",
       subtitle = "Distribuição",
       caption = "Elaboração do autor") +
  scale_fill_manual(values = c("purple","orange"))

######################################
## colocando o rótulo
## Para colocar o rótulo, precisamos de um pequeno pré-processamento
bd %>% 
  group_by(sexo) %>% 
  summarise(n = n(),
            med = mean(nrem)) %>% 
  ggplot(aes(x=sexo, y=n, label=n,
             fill = sexo)) +
  geom_col() +
  geom_text(hjust = -0.2) +
  ylim(0,30) +
  coord_flip()
