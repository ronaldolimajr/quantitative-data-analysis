
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 6.1 - correlation Homework
# Last updated in Nov/2021

# =================================================

# Carregar pacotes
library(tidyverse)
library(languageR)

# 1. Visualmente, há indício de uma correlação entre duração e F1 nos
# dados vogaisPB? (lembre-se de filtrar os erros de medição de F1, ficando
# apenas com valores abaixo de 800Hz)

# Ler os dados e filtrar os erros de medição de F1
vogais = read_csv("data/vogaisPB.csv") %>% 
  filter(F1 < 800)

# Visualizar os dados
ggplot(data = vogais, aes(x = dur, y = F1)) + 
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")

# 2. Os dados seguem uma distribuição normal?
shapiro.test(vogais$dur)
shapiro.test(vogais$F1)

# 3. Há uma correlação entre duração e F1?
# 4. Qual é a direção da correlação?
# 5. É uma correlação possível de ocorrer na população e ser replicada em outras amostras?
# 6. Quanto deu o rô de Spearman? (use 3 casas decimais)

# Rodar teste não paramétrico de correlação
cor.test(vogais$dur, vogais$F1, method = "spearman")

# 7. Quando daria um r de Pearson?
cor.test(vogais$dur, vogais$F1)


# ------------------------- FIM ---------------------------


