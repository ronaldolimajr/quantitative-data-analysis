
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 11 - Mixed Effects
# Last updated in Jan/2022

# =================================================

# Carregar pacotes

library(tidyverse)
library(languageR)
library(lme4)
library(sjPlot)


# Carregar dados
data(beginningReaders)
readers = beginningReaders

glimpse(readers)

#################################################################
# Investigar possível efeito do tomanho da palavra em letras 
# sobre tempo de reação
#################################################################

###### EDA ########

# Tendência geral
ggplot(readers, aes(y = LogRT, x = OrthLength)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm) +
  theme_classic() +
  labs(y = "Tempo de reação (log)", x = "Tamanho da palavra")

# Tendências individuais
ggplot(readers, aes(y = LogRT, x = OrthLength)) +
  #geom_point(alpha = 0.05) +
  geom_smooth(method = lm) +
  facet_wrap(~Subject) +
  theme_minimal() +
  labs(y = "Tempo de reação (log)", x = "Tamanho da palavra")



###### MODELOS ########


# -------> Modelo 1

# modelo SEM efeitos mistos
mod1 = lm(LogRT ~ OrthLength, data = readers)
summary(mod1)


# -------> Modelo 2

# modelo com INTERCEPTs aleatórios para sujeito
mod2 = lmer(LogRT ~ OrthLength + (1 | Subject), data = readers)
summary(mod2)

tab_model(mod1, mod2)

confint(mod2)


# -------> Modelo 3

# modelo com INTERCEPTs e SLOPEs aleatórios para sujeito (por OrthLength)
mod3 = lmer(LogRT ~ OrthLength + (OrthLength | Subject) , data = readers)
summary(mod3)

tab_model(mod2, mod3)


# -------> Modelo 4

# modelo com INTERCEPTs e SLOPEs aleatórios para sujeito (por OrthLength)
# e INTERCEPTS aleatórios para palavra
mod4 = lmer(LogRT ~ OrthLength + (1 | Word) + (OrthLength | Subject), 
            data = readers)
summary(mod3)

tab_model(mod3, mod4)


#################################################################
# Investigar possível efeito das demais variáveis
# sobre tempo de reação
#################################################################

###### EDA ########

# Trial
ggplot(readers, aes(y = LogRT, x = Trial)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm) +
  theme_classic() +
  labs(y = "Tempo de reação (log)", x = "Trial")

# LogFrequency
ggplot(readers, aes(y = LogRT, x = LogFrequency)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm) +
  theme_classic() +
  labs(y = "Tempo de reação (log)", x = "Frequência da palavra (log)")

# LogFamilySize
ggplot(readers, aes(y = LogRT, x = LogFamilySize)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm) +
  theme_classic() +
  labs(y = "Tempo de reação (log)", x = "Tamanho da família morfológica da palavra (log)")

# ReadingScore
ggplot(readers, aes(y = LogRT, x = ReadingScore)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm) +
  theme_classic() +
  labs(y = "Tempo de reação (log)", x = "Nota no teste de leitura")

# ProportionOfErrors
ggplot(readers, aes(y = LogRT, x = ProportionOfErrors)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm) +
  theme_classic() +
  labs(y = "Tempo de reação (log)", x = "Proporção de erros (por palavra)")


###### MODELOS ########

# -------> Modelo 0 
# Com todas as variáveis, sem efeitos mistos
m0 = lm(LogRT ~ OrthLength + Trial + 
            LogFrequency + LogFamilySize +
            ReadingScore + ProportionOfErrors,
          data = readers)
summary(m0)

# -------> Modelo 1
# Com todas as variáveis, intercepts aleatórios para palavra,
# e slopes aleatórios para sujeito em função do tamanho da palavra

# Podemos carregar o pacote lmerTest para ter valores de p no summary
library(lmerTest)

m1 = lmer(LogRT ~ OrthLength + Trial + 
            LogFrequency + LogFamilySize +
            ReadingScore + ProportionOfErrors +
            (1 | Word) + (OrthLength | Subject),
          data = readers)
summary(m1)

tab_model(m0, m1)

# -------> Modelo 2
# Com todas as variáveis, intercepts aleatórios para palavra,
# e slopes aleatórios para sujeito em função do tamanho da palavra
# e verificando possíveis interações
m2 = lmer(LogRT ~ (Trial + OrthLength + LogFrequency + LogFamilySize + 
                     ReadingScore + ProportionOfErrors) ^ 2 +
            (1|Word) + (OrthLength|Subject),
          data = readers)
summary(m2)


# -------> Modelo 3
# Mantendo interações significativas
m3 = lmer(LogRT ~ (Trial + OrthLength + LogFrequency + LogFamilySize +
                     ReadingScore + ProportionOfErrors) +
            Trial:OrthLength + Trial:ReadingScore + Trial:ProportionOfErrors +
            (1|Word) + (OrthLength|Subject),
          data = readers)
summary(m3)

plot_model(m3, type = "int")

tab_model(m1, m3)

plot_model(m3, type = "pred", terms = c("OrthLength",
                                        "LogFrequency",
                                        "ReadingScore"))

tab_model(m3)


# ------------------------------- FIM ----------------------------------



