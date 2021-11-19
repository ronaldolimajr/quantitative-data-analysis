
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 7.1 - (simple) linear regression Homework
# Last updated in Nov/2021

# =================================================

# Carregar pacotes
library(tidyverse)
library(languageR)


# 1. Como avaliamos o poder explicativo do modelo como um todo?
# Pelo intercept
# Pelo slope
# Pelos valores-p dos teste-ts
# *Pelo R quadrado

# 2. O que quer dizer o intercept?
# *É o valor de y quando x é igual a zero
# É o valor de x quando y é igual a zero
# É o valor do tamanho do efeito
# É quanto da variação da variável resposta pode ser explicado pela variável preditora

# 3. Qual coeficiente indica o tamanho do efeito da variável preditora?
# Intercept
# *Slope
# Coeficiente linear
# Variável dependente

# 4. O que quer dizer um valor-p < 0.05 no teste-t do slope?
# Que o valor de y quando x é zero difere de zero
# *Que o ângulo da linha é diferente de zero
# Que o modelo é adequado para explicar a realação entre as variáveis
# Que não há correlação entre as variáveis respota e previsora

# 5. Como chamamos a diferença entre os valores observados e os valores estimados pelo modelo?
# Intercept
# Slope
# *Resíduos
# Teste-F

##############################
# Análise 1 - dados vogaisPB
##############################

# 6. Remova os valores de F1 acima de 900Hz e 
# plote duração em função de F1 com uma linha de tendência. Há indício de uma 
# associação entre duração e F1?
vogais = read_csv("data/vogaisPB.csv") %>% 
  filter(F1 < 900)

ggplot(data = vogais, aes(x = F1, y = dur)) + 
  geom_point() + 
  stat_smooth(method = lm)


# Crie um modelo para estimar duração a partir de um valor de F1. 
# 7. Há efeito de F1 sobre duração?
# 8. Qual é o tamanho do efeito?
# 9. O efeito é estatisticamente significativo?
# 10. O modelo é adequado para explicar/prever duração de vogais?
modVogais = lm(dur ~ F1, data = vogais)
summary(modVogais)


##############################
# Análise 2 - dados english
##############################

# 11. Plote tempo de reação (RTlexdec) em função da 
# frequência escrita da palavra (WrittenFrequency). 
# Há indício de uma associação entre tempo de reação e frequência escrita?
data(english)
english = english %>% 
  as_tibble() %>% 
  select(RTlexdec, WrittenFrequency, WordCategory)

ggplot(data = english, aes(x = WrittenFrequency, y = RTlexdec)) + 
  geom_point(alpha = 0.1) + 
  stat_smooth(method = lm) 


# Crie um modelo para estimar tempo de reação a partir de um valor de frequência escrita. 
# 12. Há efeito de frequência escrita sobre tempo de reação?
# 13. Qual é o tamanho do efeito?
# 14. O efeito é estatisticamente significativo?
# 15. Qual é o valor de R quadrado?

mEnglish1 = lm(RTlexdec ~ WrittenFrequency, data = english)
summary(mEnglish1)


# OBS.:
# Efeito dado na escala da variavel Y, aqui em log
# Em milissegundos
exp(6.735931) - exp(6.735931 - 0.037010)

# R2 substancial (para apenas uma variável preditora): 
# 19% dos RTs explicados por frequência lexical


##############################
# Análise 3 - dados english
##############################

# 16. Plote tempo de reação (RTlexdec) em função da classe gramatical da palavra (WordCategory), 
#que tem 2 níveis: substantivo (N) e verbo (V). 
# Há indício de uma associação entre tempo de reação e classe gramatical?

ggplot(data = english, aes(x = WordCategory, y = RTlexdec)) + 
  geom_boxplot()

# Crie um modelo para estimar tempo de reação em função de classe gramatical. 
# 17. Há efeito de classe gramatical sobre tempo de reação?
# 18. Qual é o tamanho do efeito?
# 19. O efeito é estatisticamente significativo?
# 20. O modelo é adequado para explicar/prever o tempo de reação?

mEnglish2 = lm(RTlexdec ~ WordCategory, data = english)
summary(mEnglish2)

# OBS.:
# R2 baixíssimo, embora efeito seja significativo
# O que importa mais na SUA pesquisa: mostrar um efeito, ou mostrar um efeito substancial?


# ------------------------- FIM ---------------------------


