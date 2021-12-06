
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 9 - Multiple Linear Regression - Interaction
# Last updated in Dec/2021

# =================================================

# Carregar pacotes

library(languageR)
library(tidyverse)
library(sjPlot)


###########################################
# REGRESSÃO LINEAR MÚLTIPLA (multifatorial)

# Interação

###########################################

# Careggar os dados'english' do languageR
data(english)
en = english %>% 
  as_tibble()

# Relembrando:
# O ponto de partida para esta parte é o modelo 5 do script anteior 
# (7-linearRegression1.R), que avaliar o papel de frequência escrita, 
# familiaridade e faixa etária no tempo de reação:

modEn5 = lm(RTlexdec ~ WrittenFrequency + Familiarity + AgeSubject, data = en)
summary(modEn5)

# Plotar os efeitos (com sjPlot)
plot_model(modEn5)

# Gerar uma tabela para reportar os resultados
tab_model(modEn5)



################# Interação ####################

# Há interação entre Familiaridade e frequência?

modEn6 = lm(RTlexdec ~ WrittenFrequency * Familiarity + AgeSubject, data = en)
summary(modEn6)

# Pelo coeficiente de interação, sim.
# Podemos plotar a interação (que também demonstra a interação)
plot_model(modEn6, type = "int")


#-------------------------- OBS ------------------------------------------
# Como se comportariam o coeficiente da interação e o gráfico
# da interação quando não há interação?
m = lm(RTlexdec ~ WrittenFrequency * AgeSubject, data = en)
summary(m)  # p>0.05

plot_model(m, type = "int") # lihas paralelas
#-------------------------- OBS ------------------------------------------


# Sendo assim, precisamos incluir a interação no modelo com essas 3
# variáveis previsoras, caso contrário erraríamos na interpretação
# dos efeitos:
tab_model(modEn5, modEn6)

# O modelo 6 é superior ao 5? Pelo R-squared sim, e pela Anova?
anova(modEn5, modEn6) 

# Decidindo trabalhar com o modelo 6, podemos gerar a tabela para reportar o resultado
tab_model(modEn6)

# E podemos plotar os efeitos dos coeficientes
plot_model(modEn6)

# Podemos também plotar os efeitos marginais (valores previstos pelo modelo),
# neste caso de tempo de reação em função de familiaridade
plot_model(modEn6, type = "pred", terms = "Familiarity")

## Este plot é o mesmo que 
ggplot(data = en, aes(x = Familiarity, y = RTlexdec)) +
  #geom_point(alpha = 0.2) +
  geom_smooth(method = lm) +
  labs(x = "Familiaridade com a palavra", y = "Tempo de reação")

# Mas com menos linhas o sjPlot consegue plotar os efeitos marginais de todas
# as variáveis preditoras de uma vez
plot_model(modEn6, type = "pred", terms = c("Familiarity", 
                                            "WrittenFrequency", 
                                            "AgeSubject"))


# ------------------------- FIM ---------------------------

