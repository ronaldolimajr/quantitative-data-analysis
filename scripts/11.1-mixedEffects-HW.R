
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 11 - Mixed Effects HOMEWORK
# Last updated in Jan/2022

# =================================================

# Carregar pacotes
library(tidyverse)
library(languageR)
library(lme4)
library(sjPlot)

##########################################
# Data wrangling
##########################################

# Esta tarefa utiliza os dados "warlpiri" do pacote languageR.
# Trata-se de 347 observações de 21 falantes (8 crianças e 13 adultos) de Lajamanu 
# (norte da Austrália) descrevendo histórias de um livro de imagens
# na língua warlpiri. A variável resposta é uso do caso ergativo 
# (variável "CaseMarking") em função das variáveis:
# "AgeGroup" (adult/children)
# "WordOrder" (se o sujeito é inicial ou não)
# "AnimacyofSubject" (se o sujeito é animado ou inanimado)
# "OvertnessOfObject" (se o objeto é explícito ou não)
# "AnimacyOfObject" (se o objeto é animado ou inanimado)

# Carregar dados
data("warlpiri")
warl = warlpiri

glimpse(warl)
summary(warl)

# Fazer o contrast coding para modelar a probabilidade do uso do caso ergativo
warl = warl %>% 
  mutate(Ergative = ifelse(CaseMarking == "ergative", 1, 0))


##########################################
# Análise exploratória dos dados (EDA)
##########################################

# 1. Qual nível de cada variável preditora tem maior proporção de uso 
# do caso ergativo?

# Criar tabela de proporção de ergativo em relação a todas as variáveis:
props = warl %>% 
  group_by(AgeGroup, WordOrder, AnimacyOfSubject,
           OvertnessOfObject, AnimacyOfObject, CaseMarking) %>% 
  count() %>% 
  group_by(AgeGroup, WordOrder, AnimacyOfSubject,
           OvertnessOfObject, AnimacyOfObject) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(CaseMarking == "ergative")

# Gráficos das proporções de uso do caso ergativo para cada variável

# Faixa etária
ggplot(props, aes(x = AgeGroup, y = prop)) +
  stat_summary(geom = "bar") +
  theme_classic() +
  #coord_cartesian(ylim = c(0, 0.85)) +
  labs(x = "Faixa etária",
       y = "Uso do caso ergativo (proporção)") 

# Ordem das palavras
ggplot(props, aes(x = WordOrder, y = prop)) +
  stat_summary(geom = "bar") +
  theme_classic() +
  #coord_cartesian(ylim = c(0, 0.85)) +
  labs(x = "Ordem das palavras",
       y = "Uso do caso ergativo (proporção)") 
  
# Animacidade do sujeito
ggplot(props, aes(x = AnimacyOfSubject, y = prop)) +
  stat_summary(geom = "bar") +
  theme_classic() +
  #coord_cartesian(ylim = c(0, 0.85)) +
  labs(x = "Animacidade do sujeito",
       y = "Uso do caso ergativo (proporção)") 

# Objeto explícito ou não
ggplot(props, aes(x = OvertnessOfObject, y = prop)) +
  stat_summary(geom = "bar") +
  theme_classic() +
  #coord_cartesian(ylim = c(0, 0.85)) +
  labs(x = "Objeto explícito?",
       y = "Uso do caso ergativo (proporção)") 

# Animacidade do objeto
ggplot(props, aes(x = AnimacyOfObject, y = prop)) +
  stat_summary(geom = "bar") +
  theme_classic() +
  #coord_cartesian(ylim = c(0, 0.85)) +
  labs(x = "Animacidade do objeto",
       y = "Uso do caso ergativo (proporção)") 

##########################################
# Modelos
##########################################

# 2. Em um modelo de regressão logística modelando a probabilidade 
# de uso do caso ergativo em função das 5 variáveis preditoras SEM efeitos mistos
# (e sem interações), quais são as variáveis com efeito significativo?

# Modelo sem efeitos mistos
mod0 = glm(data = warl, CaseMarking ~ AgeGroup + WordOrder + AnimacyOfSubject +
              OvertnessOfObject + AnimacyOfObject, family = "binomial")
summary(mod0)

# Gráficos com previsões do modelo
plot_model(mod0, type = "pred")

# 3. Em um modelo como o do item 2, porém com intercepts variáveis para falantes, 
# quais são as variáveis com efeito significativo?
  
# Modelo com intercepts variáveis para falante
mod1 = glmer(data = warl, CaseMarking ~ AgeGroup + WordOrder + AnimacyOfSubject +
                      OvertnessOfObject + AnimacyOfObject
                      + (1 | Speaker) 
                    , family = "binomial")
summary(mod1)

tab_model(mod0, mod1)


# 4. Em um modelo verificando possíveis interações entre todos os pares 
# de variáveis preditoras, quais pares deram uma interação significativa?

# Modelo para conferir possíveis interações entre variáveis preditoras
mod2 = glmer(data = warl, CaseMarking ~ (AgeGroup + WordOrder + AnimacyOfSubject +
               OvertnessOfObject + AnimacyOfObject) ^ 2 
               + (AgeGroup | Speaker)
             , family = "binomial")
summary(mod2)


# 5. Em um modelo com as interações que deram  significativas 
# e com os intercepts variáveis para falantes, 
# quais variáveis preditoras deram significativas?

# 6. Qual foi o desvio-padrão dos intercepts variáveis? 

# Modelo mantendo a única interação que deu significativa
mod3 = glmer(data = warl, CaseMarking ~ AgeGroup + WordOrder + AnimacyOfSubject +
                                           OvertnessOfObject + AnimacyOfObject
             + AgeGroup:WordOrder
             + (1 | Speaker)
             , family = "binomial")
summary(mod3)

tab_model(mod1, mod3)


# 7. Qual das combinações abaixo tem a maior probabilidade de uso do caso ergativo?
  
# Gráficos dos valores previstos pelo modelo das duas variáveis significativas
plot_model(mod3, type = "pred", terms = c("AgeGroup", "AnimacyOfSubject"))


# 8. Qual é a direção da interação presente?

# Gráfico da interação
plot_model(mod3, type = "int")


# ------------------------------- FIM ----------------------------------

