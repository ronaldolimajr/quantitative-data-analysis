
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 10 - Logistic Regression
# Last updated in Dec/2021

# =================================================

# Carregar pacotes

library(tidyverse)
library(languageR)
library(arm)
library(sjPlot)


###########################################
# REGRESSÃO LOGÍSTICA
###########################################

# >>>>>>>>>>>>>>>> MODELO 1 - Dados 'english'

# Careggar dados
data(english)
english = english

# Explicar/Prever faixa etária em função do tempo de reação

# Contrast Coding
english = english %>% 
  dplyr::select(RTlexdec, AgeSubject) %>% 
  mutate(Old = as.factor(
    ifelse(AgeSubject == "old", 1, 0)
    ))

glimpse(english)

# Gráfico
ggplot(data = english, aes(y = Old, x = RTlexdec)) +
  geom_boxplot() +
  stat_summary(color = "blue") +
  theme_classic() +
  labs(y = "p(Old)", x = "Tempo de Reação")

# Estatística descritiva
english %>%
  group_by(Old) %>% 
  summarize(meanRT = mean(RTlexdec), 
            sdRT = sd(RTlexdec)
            )

# Modelo
model.eng = glm(Old ~ RTlexdec, data = english, family = binomial()) 
summary(model.eng) 
confint(model.eng) # inervalos de confiança dos coeficientes

# Tabela para reportar os resultados (do pacote sjPlot)
tab_model(model.eng, transform = NULL)

# Plot predicted probabilities
plot_model(model.eng, type = "pred", terms="RTlexdec [all]") +
  theme_classic()

# Prever probabilidades de 'Old' em tempos de reação específicos
# TR = 1
invlogit(-128.6308 + 19.6497)
# TR = 2
invlogit(-128.6308 + (2*19.6497))
# TR = 6,25
invlogit(-128.6308 + (6.25*19.6497))
# TR = 6,5
invlogit(-128.6308 + (6.5*19.6497))
# TR = 6,75
invlogit(-128.6308 + (6.75*19.6497))
# TR = 7
invlogit(-128.6308 + (7*19.6497))

# De maneira automática com predict() 
newRT = tibble(RTlexdec = seq(from = 6, to = 7, by = 0.25)) %>% 
  mutate(pred = predict(model.eng, newRT, type = "response")) %>% 
  mutate(pred = round(pred, digits = 4))



# >>>>>>>>>>>>>>>> MODELO 2 - Dados 'Labov'

# Carregar os dados e trocar o que for character para factor
labov = read_csv("data/LabovDS.csv") %>% 
  mutate_if(is.character, as.factor)

summary(labov)

# Excluir os casos de dúvida (d) em r
labov = labov %>% 
  filter(r != "d") %>% 
  droplevels()

summary(labov)

# Criar tabela de proporção de APAGAMENTO (r0) em relação a todas as variáveis:
props = labov %>% 
  group_by(store, emphasis, word, r) %>% 
  count() %>% 
  group_by(store, emphasis, word) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(r == "r0")

# Gráficos das proporções de apagamento para cada variável
ggplot(props, aes(x = store, y = prop)) +
  stat_summary(geom = "bar") +
  theme_classic() +
  labs(x = "Store",
       y = "Deletion of /r/ (proportion)") +
  coord_cartesian(ylim = c(0, 0.85))

ggplot(props, aes(x = word, y = prop)) +
  stat_summary(geom = "bar") +
  theme_classic() +
  labs(x = "Word",
       y = "Deletion of /r/ (proportion)") +
  coord_cartesian(ylim = c(0, 0.85))

ggplot(props, aes(x = emphasis, y = prop)) +
    stat_summary(geom = "bar") +
    theme_classic() +
    labs(x = "Emphasis",
         y = "Deletion of /r/ (proportion)") +
    coord_cartesian(ylim = c(0, 0.85))

# Contrast coding para modelarmos apagamento
labov = labov %>% 
  mutate(deletion = if_else(r == "r0", 1, 0))

# Modelo com apenas uma variável preditora (loja)
mLabov.store = glm(deletion ~ store,
                   data = labov,
                   family = "binomial")
summary(mLabov.store)

# Probabilidades de apagamento
invlogit(2.2285) # na Klein
invlogit(2.2285-1.7049) # na Macys
invlogit(2.2285-2.1385) # na Saks

# Ou:
predict(mLabov.store,
        newdata = tibble(store = c("Klein", "Macys", "Saks")), type = "response")

# Plotar as probabilidades previstas
plot_model(mLabov.store, type = "pred") 


# Modelo com duas variáveis preditoras (loja e palavra)
mLabov.store.word = glm(deletion ~ store + word,
                        data = labov,
                        family = "binomial")
summary(mLabov.store.word)


# Probabilidades previstas
pred.store.word = 
  tibble(store = rep(levels(labov$store), times = 2),
         word = rep(levels(labov$word), each = 3)) %>% 
  mutate(pred = predict(mLabov.store.word,
                        newdata = pred.store.word, 
                        type = "response"))
pred.store.word


# Plotar as probabilidades previstas
plot_model(mLabov.store.word, type = "pred", terms = c("store", "word")) +
  theme_light()



# Modelo com as 3 variáveis preditoras (loja, palavra e ênfase)
mLabov = glm(deletion ~ store + word + emphasis,
                        data = labov,
                        family = "binomial")
summary(mLabov)


# Probabilidades previstas
pred.labov = 
  tibble(store = rep(levels(labov$store), times = 4),
         word = rep(levels(labov$word), each = 6),
         emphasis = rep(levels(labov$emphasis), times = 2, each = 3)) %>% 
  mutate(pred = predict(mLabov,
                        newdata = pred.labov, 
                        type = "response"))
pred.labov


# Plotar as probabilidades previstas
plot_model(mLabov, type = "pred", terms = c("store", "emphasis", "word")) +
  theme_light()


# ------------------------- FIM ---------------------------


