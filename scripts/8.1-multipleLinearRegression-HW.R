
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 8.1 - multiple linear regression Homework
# Last updated in Nov/2021

# =================================================

# Carregar pacotes
library(tidyverse)
library(languageR)
library(sjPlot)


# Carregue os dados 'english' do languageR em um tibble e selecione as 
# seguintes colunas: RTlexdec, Familiarity, AgeSubject, WordCategory,
# WrittenFrequency, FamilySize, CV, Voice.

data(english)
english = english %>% 
  as_tibble() %>% 
  select("RTlexdec", "Familiarity", "AgeSubject", "WordCategory",
         "WrittenFrequency", "FamilySize", "CV", "Voice")

en

# 1. Qual tipo de gráfico deve ser utilizado para cada variável preditora?

# 2. Crie um gráfico para cada variável preditora

# Variáveis numéricas contínuas:
# Familiarity
ggplot(english, aes(x = Familiarity, y = RTlexdec)) +
  geom_point() +
  geom_smooth(method = lm)

# WrittenFrequency
ggplot(english, aes(x = WrittenFrequency, y = RTlexdec)) +
  geom_point() +
  geom_smooth(method = lm)

# FamilySize
ggplot(english, aes(x = FamilySize, y = RTlexdec)) +
  geom_point() +
  geom_smooth(method = lm)

# Variáveis categóricas:
# AgeSubject
ggplot(english, aes(x = AgeSubject, y = RTlexdec)) +
  geom_boxplot()

# CV
ggplot(english, aes(x = CV, y = RTlexdec)) +
  geom_boxplot()

# Voice
ggplot(english, aes(x = Voice, y = RTlexdec)) +
  geom_boxplot()


# Ajuste um modelo de regressão linear explicando/prevendo o tempo de reação
# em função de todas as demais variáveis preditoras.
# 3. Qual é o valor do intercept (coeficiente linear)?
# 4. O que quer dizer esse valor?
# 5. O valor de p do intercept deu significativo? Por quê?
# 6. Quais variáveis preditoras apresentaram efeito significativo sobre o tempo de reação?
# 7. Quais são os valores de referência das variáveis categóricas?
# 8. Qual é o valor de tempo de reação (na escola logarítmica) que o modelo estima
# para um substantivo iniciado por consoante sonora de frequência, familiaridade e 
# tamanho iguais a zero de um participante velho?
modEnglish = lm(RTlexdec ~ ., english)
summary(modEnglish)

# 9. Qual é o valor de tempo de reação (na escola logarítmica) que o modelo estima
# para um verbo iniciado por vogal sonora de frequência, familiaridade e 
# tamanho iguais a zero de um participante novo? 
# (faça o cálculo com todas as casas decimais, e reporte utilizando 3 casas decimais)
6.900878 - 0.001309 + 0.013872 - 0.221721

# 10. Qual é o valor de tempo de reação em MILISSEGUNDOS que o modelo estima
# para um substantivo iniciado por consoante surda de frequência = 5, familiaridade = 4 e 
# tamanho = 1 de um participante novo? 
# (faça o cálculo com todas as casas decimais, e reporte sem nenhuma casa decimal)
exp(6.900878 - 0.005958 - (5 * 0.015661) - (4 * 0.034371) - 0.015068 - 0.221721)

# 11. Quanto por cento da variação no tempo de reação o modelo conseuqe explicar?
summary(modEnglish)


# --------------------------- FIM ------------------------

