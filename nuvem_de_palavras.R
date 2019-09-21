###################################
### Montando uma nuvem de palavras
### Prof. Dr. Neylson Crepalde
### SESC - 2019
###################################
#install.packages("tm")  #Mineração de textos
#install.packages("wordcloud") # nuvens de palavras

library(tm)
library(wordcloud)
library(tidyverse)
library(readxl)

bd = read_xlsx("dados_rede_sesc.xlsx")
bd

############################
# Remove accent function
# Adapted from https://github.com/harvesthq/chosen/issues/1880
remove_accent = function(x) {
  if (class(x) != "character") stop("Input is not a character")
  
  r = tolower(x)
  r = gsub("[àáâãäå]", "a", r)
  r = gsub("æ", "ae", r)
  r = gsub("ç", "c", r)
  r = gsub("[èéêë]", "e", r)
  r = gsub("[ìíîï]", "i", r)
  r = gsub("ñ", "n", r)                  
  r = gsub("[òóôõö]", "o", r)
  r = gsub("œ", "oe", r)
  r = gsub("[ùúûü]", "u", r)
  r = gsub("[ýÿ]", "y", r)
  return(r)
}

# Teste
remove_accent("Ação")

#########################################
# Missão do dia:
# Analisar o conteúdo dos comentários dos entrevistados
# sobre os pontos fortes da Rede SESC

# Dicas para pré-processamento de texto:
# 1) Tranformar tudo em letras minúsculas
# 2) Remover números
# 3) Remover pontuação
# 4) Remover espaços em branco
# 5) Remover StopWords
# 6) Remove acentos
# 7) Remover outras palavras chave

texto = bd$fort_other
texto

texto = texto %>% 
  tolower() %>% 
  removeNumbers() %>% 
  removePunctuation() %>% 
  removeWords(stopwords("pt")) %>% 
  stripWhitespace() %>% 
  remove_accent() %>%
  removeWords(c("outro", "outra", "outros", "outras"))

texto
ong("nuvem.png", height = 10, width = 10,
    units = "in", res = 100)
wordcloud(texto)
dev.off()
