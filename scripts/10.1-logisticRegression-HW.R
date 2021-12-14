
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 9.1 - Logistic Regression Homework
# Last updated in Dec/2021

# =================================================

# Carregar pacotes

library(languageR)
library(tidyverse)
library(sjPlot)
library(arm)



# Carregue os dados vogaisPB e filtre os erros de medição de F1 
# (mantendo apenas valores abaixo de 900Hz)
pb = read_csv("data/vogaisPB.csv") %>% 
  filter(F1<900)
pb

# Objetivo: modelar a probabilidade da vogal ser /a/ dada a duration e o F1
# Para isso, precisamos criar um constrat coding
pb = pb %>% 
  mutate(a = as.factor(ifelse(vogal == "a", 1, 0)))
pb


# EDA (Exlporatory Data Analysis)
# Estatística Descritiva - média e DP de duração e F1 de /a/ vs demais
# 1. Qual é a média de duração da vogal /a/?
# 2. Qual é a média de duração das demais vogais?
# 3. Qual é a média de F1 da vogal /a/?
# 4. Qual é a média de F1 das demais vogais?
pb %>% 
  group_by(a) %>% 
  summarize(meanDur = mean(dur),
            sdDur = sd(dur),
            meanF1 = mean(F1),
            sdF1 = sd(F1)
  )


# Gráficos

# 5. Olhando para o gráfico da probabilidade da vogal ser /a/ em função da duração,
# aparenta haver efeito da duração sobre a probabilidade de ser /a/?
# Boxplot para duração
ggplot(data = pb, aes(x = dur, y = a)) +
  geom_boxplot() +
  stat_summary(color = "red") +
  labs(x = "Duração", y = "/a/") 

# 6. Olhando para o gráfico da probabilidade da vogal ser /a/ em função de F1,
# aparenta haver efeito de F1 sobre a probabilidade de ser /a/?
# Boxplot para F1
ggplot(data = pb, aes(x = F1, y = a)) +
  geom_boxplot() +
  stat_summary(color = "red") +
  labs(x = "F1", y = "/a/") 


# Modelo 1 (probabilidade de ser /a/ em função de duração e F1)
# 7. Qual é o valor do intercept (coeficiente linear) em log-odds?
# 8. O que quer dizer o intercept?
# 9. Qual é o valor do slope (coeficiente angular) para duração?
# 10. O que quer dizer o valor 0.05679 no output do modelo?
# 11. Qual coeficiente não foi significativo?
model1 = glm(a ~ dur + F1, data = pb, family = "binomial")
summary(model1)

# 12. Segundo este modelo, qual é a probabilidade (em porcentagem, sem casas decimais) 
# da vogal ser /a/ quando duração e F1 são iguais a zero?
# Intercept: log-odds de ser /a/ se dur=0 and F1=0
# Em probabilidade:
invlogit(-34.34118)

# dur: log-odds de ser /a/ diminui 0.02 para cada milissegundo 
# que aumenta em duração (não significativo)
# F1: log-odds de ser /a/ aumenta 0.057 para cada Hert
# de aumento em F1 (significativo)

# Modelo 2 (probabilidade de ser /a/ em função apenas de F1)
model2 = glm(a ~ F1, data = pb, family = "binomial")
summary(model2)


# 13. 14. 15. 16. Qual é a probabilidade da vogal ser /a/ (em porcentagem, sem casas decimais)
# se F1= 400, 600, 700 e 800 Hz?
invlogit((-35.36787) + 0.05508*400)
invlogit((-35.36787) + 0.05508*600)
invlogit((-35.36787) + 0.05508*700)
invlogit((-35.36787) + 0.05508*800)

# Ou:
new.pb = tibble(F1 = c(400, 600, 700, 800)) %>% 
  mutate(pred = predict(model2, new.pb, type = "response")) 
new.pb


# EXTRA:

# Plotar as probabilidades previstas
plot_model(model2, type = "pred", terms="F1 [all]")

## Potar manualmente em probabilidades
newF1 = tibble(F1 = seq(from = 500, to = 800, by = 25))

newF1 = newF1 %>% 
  mutate(pred = predict(model2, newF1, type = "response"))

ggplot(data = newF1, aes(x=F1, y=pred)) +
  geom_point() +
  geom_line() +
  labs(x = "F1", y="Predicted Probability")

## Potar manualmente em log-odds
newF1 = tibble(F1 = seq(from = 500, to = 800, by = 25))

newF1 = newF1 %>% 
  mutate(pred = predict(model2, newF1))

ggplot(data = newF1, aes(x=F1, y=pred)) +
  geom_point() +
  geom_line() +
  labs(x = "F1", y="Predicted Probability")


# ------------------------- FIM ---------------------------


