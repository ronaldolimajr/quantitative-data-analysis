
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 8 - Multiple Linear Regression
# Last updated in Nov/2021

# =================================================

# Carregar pacotes

library(languageR)
library(tidyverse)
library(sjPlot)


###########################################
# REGRESSÃO LINEAR MÚLTIPLA (multifatorial)
###########################################

# >>>>>>>>>>>>>>>> RELEMBRANDO REGRESSÃO LINEAR SIMPLES

# Careggar os dados'english' do languageR
data(english)
en = english %>% 
  as_tibble()

# Tempor de Reação ~ Familiaridade

## Gráfico
ggplot(data = en, aes(x = Familiarity, y = RTlexdec)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = lm) +
  labs(x = "Familiaridade com a palavra", y = "Tempo de reação")

## Modelo
modEn1 = lm(RTlexdec ~ Familiarity, data = en)
summary(modEn1)


# Tempor de Reação ~ Faixa Etária

## Gráfico
ggplot(data = en, aes(x = AgeSubject, y = RTlexdec)) +
  geom_boxplot() +
  stat_summary(color = "red")

## Modelo
modEn2 = lm(RTlexdec ~ AgeSubject, data = en)
summary(modEn2)


# >>>>>>>>>>>>>>>> REGRESSÃO LINEAR MÚLTIPLA


# Avaliar o papel de familiaridade E faixa etária no tempo de reação

## Gráfico
ggplot(data = en, aes(x = Familiarity, y = RTlexdec, color = AgeSubject)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = lm) +
  labs(x = "Familiaridade com a palavra", y = "Tempo de reação")

## Modelo
modEn3 = lm(RTlexdec ~ Familiarity + AgeSubject, data = en)
summary(modEn3)

# O modelo 3 é muito superior ao 1 e ao 2 comparando-se o r-squared
# mas podemos atestar sua superioridade significativa por meio de uma Anova
anova(modEn1, modEn3)
anova(modEn2, modEn3)

# Plotar os efeitos (com sjPlot)
plot_model(modEn3)

# Como reportar os dados de um modelo? Por meio de uma tabela como esta (sjPlot)
tab_model(modEn3)

# -------------------------------

# Avaliar o papel de frequência escrita E faixa etária no tempo de reação

## Gráfico
ggplot(data = en, aes(x = WrittenFrequency, y = RTlexdec, color = AgeSubject)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = lm) +
  labs(x = "Frequência escrita da palavra", y = "Tempo de reação")

## Modelo
modEn4 = lm(RTlexdec ~ WrittenFrequency + AgeSubject, data = en)
summary(modEn4)

# Plotar os efeitos (com sjPlot)
plot_model(modEn4)

# Gerar uma tabela (html) para reportar os resultados
tab_model(modEn4)

# O modelo 3 é superior ao 4? Pelo R-squared não:
tab_model(modEn3 , modEn4)

# pela Anova também não:
anova(modEn3, modEn4)

# Então por que não incluir as variáveis dos modelos 3 e 4 num mesmo modelo?
# Avaliar o papel de frequência escrita E familiaridade E faixa etária 
# no tempo de reação

## Modelo
modEn5 = lm(RTlexdec ~ WrittenFrequency + Familiarity + AgeSubject, data = en)
summary(modEn5)

# Plotar os efeitos (com sjPlot)
plot_model(modEn5)

# Gerar uma tabela para reportar os resultados
tab_model(modEn5)

# O modelo 5 é superior ao 4?
# Pelo R-squared sim, pela Anova também: 
anova(modEn4, modEn5)


# ------------------------------- 
# DADOS 'vogalE'
# ------------------------------- 


e = read_csv("data/vogalE.csv") %>% 
  select(F1.NORM, CONT.PREC, CONT.SEG, F1.SEG.NORM, AMOSTRA, SEXO) %>% 
  mutate_if(is.character, as.factor)

str(e)

levels(e$CONT.PREC)
levels(e$CONT.SEG)

# Inspecionar visualmente relação das variáveis preditoras com a 
# variável resposta
ggplot(data = e, 
       aes(x = fct_reorder(CONT.PREC, F1.NORM), 
           # troque 'CONT.PREC' pelas outras variáveis previsoras categóricas:
           # CONT.SEG, AMOSTRA, SEXO
           y=F1.NORM)) + 
  geom_boxplot()

ggplot(data = e, aes(x = F1.SEG.NORM, y=F1.NORM)) +
  geom_point() + 
  geom_smooth(method=lm)

# Retire os outliers e refaça as plotagens acima
e = e %>% 
  filter(F1.NORM < 550,
         F1.SEG.NORM < 550) 

# Em uma análise real, aqui seria o momento de analisar os dados de tendência central
# (pelo menos médias e desvios-padrão) e gerar tabelas para se familiarizar
# mais ainda com os dados


# Criar um modelo completo, com todas as variáveis previsoras
modE = lm(F1.NORM ~ AMOSTRA + SEXO + F1.SEG.NORM + 
                CONT.PREC + CONT.SEG, data = e)
summary(modE)


# -------------------- OBS.:


# Quanras / quais variáveis incluir/manter num modelo?

# CUIDADO com a prática de "star gazing" (McElreath)
N = 150
dsim = data.frame(
  x1 = rnorm(N), 
  x2 = rnorm(N), 
  x3 = rnorm(N), 
  x4 = rnorm(N), 
  x5 = rnorm(N), 
  x6 = rnorm(N), 
  x7 = rnorm(N), 
  x8 = rnorm(N), 
  x9 = rnorm(N), 
  x10 = rnorm(N),
  y = rnorm(N)
)
summary(lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, dsim))


# ------------------------- FIM ---------------------------


