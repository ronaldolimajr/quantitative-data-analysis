
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 2 - Descriptive statistics + histograms and boxplots
# Last updated in out/2021

# =================================================

# Carregar pacotes
library(tidyverse)

# Carregar dados e trocar a classe de "vogais" de caractere para fator
vogais = read_csv("data/vogaisPB.csv") %>% 
  mutate(vogal = as.factor(vogal))

vogais

# =================================================

# Inspecionar visualmente distribuição dos dados numéricos
ggplot(vogais, aes(x = dur)) +
  geom_histogram()

ggplot(vogais, aes(x = F1)) +
  geom_histogram()

# Filtrar o caso de erro de medição
vogais = vogais %>% 
  filter(F1 < 1000)

# Inspecionar por vogal (com boxplots)
ggplot(vogais, aes(x = vogal, y = dur)) +
  geom_boxplot()

ggplot(vogais, aes(x = vogal, y = F1)) +
  geom_boxplot()

# =================================================

# Média de duração e de F1 por vogal (organizados por duração)
vogais %>% 
  group_by(vogal) %>% 
  summarize(meanDur = mean(dur),
            meanF1 = mean(F1)
            ) %>% 
  arrange(meanDur)


# Média e mediana de duração e de F1 por vogal
vogais %>% 
  group_by(vogal) %>% 
  summarize(meanDur = mean(dur),
            medianDur = median(dur),
            meanF1 = mean(F1),
            medianF1 = median(F1)
            ) %>% 
  arrange(meanDur)

# Média, mediana e DP de duração e de F1 por vogal
vogais %>% 
  group_by(vogal) %>% 
  summarize(meanDur = mean(dur),
            medianDur = median(dur),
            SD.Dur = sd(dur), 
            meanF1 = mean(F1),
            medianF1 = median(F1),
            SD.F1 = sd(F1)
            ) %>% 
  arrange(meanDur)

# Média, mediana e moda de duração e de F1 por vogal

## Não há função no R para moda, mas podemos baixar um pacote que faça ou criar a função,
## neste caso, facilmente buscada no google:

Mode = function(x) {
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

vogais %>% 
  group_by(vogal) %>% 
  summarize(meanDur = mean(dur),
            medianDur = median(dur),
            modeDur = Mode(dur), 
            meanF1 = mean(F1),
            medianF1 = median(F1),
            modeF1 = Mode(F1)
            ) %>% 
  arrange(meanDur)


# Média e erro-padrão de duração e de F1 por vogal

## Lembrando da fórmula do erro-padrão:
## SE = sdDados / sqrt(n)

vogais %>% 
  group_by(vogal) %>% 
  summarize(meanDur = mean(dur),
            seDur = sd(dur) / sqrt(length(dur)),
            meanF1 = mean(F1),
            seF1 = sd(F1) / sqrt(length(F1))
            ) %>% 
  arrange(meanDur)

# Podemos criar uma fórmula pra erro-padrão
SE = function(x) {sd(x) / sqrt(length(x))}

vogais %>% 
  group_by(vogal) %>% 
  summarize(meanDur = mean(dur),
            seDur = SE(dur),
            meanF1 = mean(F1),
            seF1 = SE(F1)
            ) %>% 
  arrange(meanDur)

# Normalmente reportamos média e desvio-padrão
# Média e DP de duração e de F1 por vogal
vogais %>% 
  group_by(vogal) %>% 
  summarize(meanDur = mean(dur),
            SD.Dur = sd(dur), 
            meanF1 = mean(F1),
            SD.F1 = sd(F1)
            ) %>% 
  arrange(meanDur)

# Podemos salvar o resumo em um objeto do R (tibble)
vogais.summary = vogais %>% 
  group_by(vogal) %>% 
  summarize(meanDur = mean(dur),
            SD.Dur = sd(dur), 
            meanF1 = mean(F1),
            SD.F1 = sd(F1)
            ) %>% 
  arrange(meanDur)

vogais.summary

# Podemos salvar o tibble em arquivo csv
write_csv(vogais.summary, "data/vogaisPB-summary.csv")


# =================================================
# EXTRAS
# =================================================

# Há várias opções em pacotes para visualizar resumos descritivos
# Ex.:
install.packages("DT")
library(DT)

datatable(
  vogais %>% 
    group_by(vogal) %>% 
    summarize(meanDur = mean(dur),
              SD.Dur = sd(dur), 
              meanF1 = mean(F1),
              SD.F1 = sd(F1)
              )
  ) %>% 
  formatRound(columns=c("meanDur", "SD.Dur", 
                        "meanF1", "SD.F1"), 
              digits=2)


# Adicionando a média e erro-padrão a boxplots
ggplot(vogais, aes(x = vogal, y = dur)) +
  geom_boxplot() +
  stat_summary(color = "red")

ggplot(vogais, aes(x = vogal, y = F1)) +
  geom_boxplot() +
  stat_summary(shape = 8, color = "blue")

# Adicionando títulos a boxplots
ggplot(vogais, aes(x = vogal, y = dur)) +
  geom_boxplot() +
  stat_summary(color = "red") +
  labs(x = "vogal", y = "duração")

# Há relação entre duração e F1?
## SCATTERPLOT
ggplot(vogais, aes(x = dur, y = F1)) + 
  geom_point() + 
  stat_smooth(method = "lm")

# Melhorando o gráfico para fonólogos
ggplot(vogais, aes(x = dur, y = F1, label = vogal)) + 
  geom_text(size = 5, alpha = 0.5) + 
  stat_smooth(method = "lm") +
  scale_y_reverse() + 
  coord_cartesian(ylim = c(1000, 100)) + 
  theme_minimal()


# ===================== FIM ============================


