
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 2 - Homework

# =================================================

# Carregar pacotes
library(tidyverse)

# Carregar dados
english = read_csv("data/english.csv")


# 1. Quais são as variáveis resposta e preditoras?
english

# 2. Quais variáveis são numéricas contínuas?

# 3. Quais são categóricas com 2 níveis?
english$WordCategory # não mostra tudo
levels(english$WordCategory) # foi lido como character

## Mudar a classe do que foi lido como character para factor
english = english %>% 
  mutate_if(is.character, as.factor)

## opção 1: LEVELS
levels(english$WordCategory)
levels(english$CV)
levels(english$AgeSubject)

## opção 2: STR (structure)
str(english)

## opção 3: SUMMARY
summary(english)


# 4. Há quantas observações nos dados?

# 5. Com qual classe o R interpreta AgeSubject na importação?

# 6. Qual função do tidyverse usamos para atribuir 
  # a classe “factor” para AgeSubject?

## opção 1: apenas para AgeSubject
english = english %>% 
  mutate(AgeSubject = as.factor(AgeSubject))

## opção 2: trocar de tudo o que foi lido como character para factor
english = english %>% 
  mutate_if(is.character, as.factor)

# 7. Quantas palavras foram utilizadas na pesquisa?
str(english)

# 8. Qual é a média do tempo de reação em ms 
    # para os participantes mais jovens? 
# 9. Qual é a mediana do tempo de reação em ms 
    # para os participantes mais velhos? 
english %>% 
  group_by(AgeSubject) %>% 
  summarize(meanRTms = mean(RTms),
            medianRTms = median(RTms)
            )

# 10. Qual é a moda do tempo de reação em log para  verbos? 
Mode = function(x) {
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

english %>% 
  group_by(WordCategory) %>% 
  summarize(modeRTlog = Mode(RTlog))


# 11. O valor 2.01 corresponde a 
    # (sd/se do tempo de reação em ms de jovens/velhos)
SE = function(x) {sd(x) / sqrt(length(x))}

english %>% 
  group_by(AgeSubject) %>% 
  summarize(sdRTms = sd(RTms),
            seRTms = SE(RTms)
            )


# 12. Os dados vão da direção da hipótese de que pessoas mais velhas 
    # são mais lentas no tempo de reação de decisões lexicais? 
english %>% 
  group_by(AgeSubject) %>% 
  summarize(meanRTms = mean(RTms),
            medianRTms = median(RTms),
            meanRTlog = mean(RTlog),
            medianRTlog = median(RTlog)
  )

ggplot(english, aes(x = AgeSubject, y = RTms)) +
  geom_boxplot()

ggplot(english, aes(x = AgeSubject, y = RTlog)) +
  geom_boxplot()

# 13. Os dados vão da direção da hipótese de que 
    # palavras mais frequentes levam a decisões mais rápidas? 
ggplot(english, aes(x = WrittenFrequency, y = RTlog)) +
  geom_point() +
  geom_smooth(method = "lm")

## ==>> Looking at both frequency and age at the same time:
ggplot(english, aes(x = WrittenFrequency, y = RTlog, 
                    color = AgeSubject)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")


# -------------------------- FIM ------------------------------------

