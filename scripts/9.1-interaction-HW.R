
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 9.1 - Interaction Homework
# Last updated in Dec/2021

# =================================================

# Carregar pacotes

library(languageR)
library(tidyverse)
library(sjPlot)


# Carregue os dados 'english' do languageR em um tibble e selecione as 
# seguintes colunas: RTlexdec, Familiarity, AgeSubject, WordCategory,
# WrittenFrequency, FamilySize, CV, Voice.

data(english)
english = english %>% 
  as_tibble() %>% 
  select("RTlexdec", "Familiarity", "AgeSubject", "WordCategory",
         "WrittenFrequency", "FamilySize", "CV", "Voice")


# Ajuste um modelo de regressão linear (modelo 1) explicando/prevendo o tempo de reação
# em função de todas as demais variáveis preditoras.
# 1. Quais variáveis o modelo NÃO prevê como apresentando efeito significativo sobre o
# tempo de reação?
modEnglish1 = lm(RTlexdec ~ ., english)
summary(modEnglish1)

# Ajuste um novo modelo (modelo 2) verificando possíveis interações entre todas as variáveis
# 2. Há interação entre quais variáveis?
modEnglish2 = lm(RTlexdec ~ .^2, english)
summary(modEnglish2)

# Ajuste um novo modelo (3) mantendo apenas as interações significativas
# 3. Quais variáveis o novo modelo NÃO prevê como apresentado efeito significativo
# sobre o tempo de reação?
modEnglish3 = lm(RTlexdec ~ .
                 + Familiarity:WrittenFrequency + Familiarity:FamilySize
                 + WordCategory:CV + WordCategory:Voice
                 , english)
summary(modEnglish3)

# Ajuste um novo modelo (4) excluindo as variáveis que não se apresentaram significativas
# 4. Alguma variável deixou de ser significativa no novo modelo? Caso sim, qual/quais?
modEnglish4 = lm(RTlexdec ~ Familiarity + AgeSubject + WordCategory
                 + WrittenFrequency + FamilySize + Voice
                 + Familiarity:WrittenFrequency + Familiarity:FamilySize
                 + WordCategory:Voice
                 , english)
summary(modEnglish4)


# Ajuste um novo modelo (5) excluindo as variáveis que deixaram de ser significativas.
# 5. Alguma variável deixou de ser significativa no novo modelo? Caso sim, qual/quais?
modEnglish5 = lm(RTlexdec ~ Familiarity + AgeSubject 
                 + WrittenFrequency + FamilySize + Voice
                 + Familiarity:WrittenFrequency + Familiarity:FamilySize
                 , english)
summary(modEnglish5)

# Ajuste um novo modelo (6) excluindo as variáveis que deixaram de ser significativas.
# 6. Alguma variável deixou de ser significativa no novo modelo? Caso sim, qual/quais?
modEnglish6 = lm(RTlexdec ~ Familiarity + AgeSubject 
                 + WrittenFrequency + FamilySize
                 + Familiarity:WrittenFrequency + Familiarity:FamilySize
                 , english)
summary(modEnglish6)

# 7. De todos os modelos ajustados neste exercício, o modelo final é o que tem o maior
# valor de R quadrado?

# 8. Plote os gráficos das interações que permaneceram no modelo
plot_model(modEnglish6, type = "int")

# 9. Plote os efeitos marginais (valores previstos pelo modelo) de todas
# as variáveis preditoras de uma vez
plot_model(modEnglish6, type = "pred", terms = c("Familiarity", 
                                            "WrittenFrequency", 
                                            "AgeSubject",
                                            "FamilySize"))


# ------------------------- FIM ---------------------------


