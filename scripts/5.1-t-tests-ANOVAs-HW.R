
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 5 - Homework correction (t-test and ANOVA)

# =================================================


# Carregar pacotes
library(tidyverse)
library(languageR)
library(car)

# 1. Sobre os dados "english", analisando os boxplots, parece haver 
# diferença no tempo de reação entre substantivos e verbos?

# Ler os dados "english" do pacote languageR
data(english)
english = english %>% 
  select(RTlexdec, Word, CV, WordCategory, AgeSubject) %>% 
  as_tibble()

english

# Visualizar os dados
ggplot(data = english, aes(x = WordCategory, y = RTlexdec)) + 
  geom_boxplot() + 
  stat_summary(color = "blue")

# 2. Qual é a média do tempo de reação para verbos?
# 3. E para substantivos?

# Checar médias
english %>% 
  group_by(WordCategory) %>% 
  summarize(MeanRT = mean(RTlexdec))

# 4. Pelo resultado do teste-t, a conclusão seria de que a diferença é
# estatisticamente significativa?

# Rodar t-test
# Two-tailed (bicaudal): old different from young
t.test(RTlexdec ~ WordCategory, english)

# One-tailed (unicaudal): old greater than young
t.test(RTlexdec ~ WordCategory, data = english, 
       alternative = "greater")

# Sobre os dados "vogaisPB", 
# 5. Pela inspeção visual dos boxplots, parece haver diferença de F1 entre as vogais?

# Carregar os dados "vogaisPB"
vogais = read_csv("data/vogaisPB.csv")

# Inspecionar boxplots das vogais
ggplot(data = vogais, aes(x = fct_reorder(vogal, F1), y = F1)) +
  geom_boxplot() +
  stat_summary(color = "red")

# Filtrar erros de medição de /i/
vogais = vogais %>% 
  filter(F1 < 800)

# 6. Quais são as vogais com menor e maior médias de F1?
# 7. Qual é a média de F1 da vogal /u/?

# Olhar os dados de F1 das vogaisPB por vogal, em ordem crescente
vogais %>% 
  group_by(vogal) %>% 
  summarize(meanF1 = mean(F1)) %>% 
  arrange(meanF1)

# 8. Há diferença entre todos os pares de vogais?
# 9. Qual é o par de vogais com a menor diferença?
# 10. E com a maior diferença?
# Rodar a ANOVA e ver o resumo 
anovaVogais = aov(F1 ~ vogal, data = vogais)
summary(anovaVogais)

# Teste pareado post-hoc
TukeyHSD(anovaVogais)

# 11. Os dados de duração das vogais seguem uma distribuição normal?
ggplot(vogais, aes(x = dur)) +
  geom_histogram(fill = "gray", color = "black") 

shapiro.test(vogais$dur)

# 12. Os dados de F1, seguem uma distribuição normal?
ggplot(vogais, aes(x = F1)) +
  geom_histogram(fill = "gray", color = "black") 

shapiro.test(vogais$F1)

# 13. Os dados de duração apresentam variação homogênea para as vogais?
leveneTest(dur ~ vogal, vogais)

# 14. Os dados de F1 apresentam variação homogênea para as vogais?
leveneTest(F1 ~ vogal, vogais)

