

# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 5 - t-tests and ANOVAs
# Last updated in oct/2021

# =================================================

# Carregar pacotes
library(tidyverse)
library(languageR)


##########################
# TESTE T
##########################

# -->> Variável preditora categórica (2 níveis) e variável resposta contínua

# A ideia é testar se duas médias são diferentes o suficiente

# Ler os dados "english" do pacote languageR
# Estes são os mesmos dados que usamos em aulas anteriores (english.csv)
data(english)
english = english %>% 
  select(RTlexdec, Word, CV, WordCategory, AgeSubject) %>% 
  as_tibble()

english

# ----------------- Ex. 1: Tempo de reação ~ Idade

# Visualizar os dados
ggplot(data = english, aes(x = AgeSubject, y = RTlexdec)) + 
  geom_boxplot() + 
  stat_summary(color = "blue") 

# Checar médias
english %>% 
  group_by(AgeSubject) %>% 
  summarize(MeanRT = mean(RTlexdec))

# Rodar t-test
# Two-tailed (bicaudal): old different from young
t.test(RTlexdec ~ AgeSubject, english)

# One-tailed (unicaudal): old greater than young
t.test(RTlexdec ~ AgeSubject, data = english, 
       alternative = "greater")


# ----------------- Ex. 2: Tempo de reação ~ CV

# Visualizar os dados
ggplot(data = english, aes(x = CV, y = RTlexdec)) + 
  geom_boxplot() + 
  stat_summary(color = "blue") 

# Checar médias
english %>% 
  group_by(CV) %>% 
  summarize(MeanRT = mean(RTlexdec))

# Rodar t-test
# Two-tailed (bicaudal): old different from young
t.test(RTlexdec ~ CV, english)

# One-tailed (unicaudal): old greater than young
t.test(RTlexdec ~ CV, data = english, 
       alternative = "greater")


# ----------------- Ex. 3: Tempo de reação ~ WordCategory

## -->> Tarefa de casa =)


# Obs.: são amostras independentes, se fossem dependentes (mesmo grupo
# em momentos diferentes), adicionar o argumento "paired = T"


##########################
# ANOVA
##########################

# -->> Variável preditora categórica (+ de 2 níveis) e variável resposta contínua

# Carregar os dados "vogaisPB"
vogais = read_csv("data/vogaisPB.csv")

# Olhar os dados de duração das vogaisPB por vogal, em ordem crescente
vogais %>% 
  group_by(vogal) %>% 
  summarize(meanDur = mean(dur)) %>% 
  arrange(meanDur)

# Inspecionar boxplots das vogais
ggplot(data = vogais, aes(x = fct_reorder(vogal, dur), y = dur)) +
  geom_boxplot() +
  stat_summary(color = "blue") 

# Rodar a ANOVA - salvamos em um objeto para ver o resumo depois
anovaVogais = aov(dur ~ vogal, data = vogais)
summary(anovaVogais)

# Conclusão: há diferença em pelo menos alguma comparação
# Para se saber onde tem diferença, é preciso conduzir algum tipo de teste pareado
# com correção de alfa, como o Tukey
TukeyHSD(anovaVogais)

# Quantos pares encontramos com diferença? 6
## PARA REFLETIR: quão informativo é isso? Não seria melhor conhecer o tamanho do efeito que 
## 'vogal' (juntamente com outras variáveis) tem na duração? É isso que modelos buscam fazer


# ANOVA vs. t-test: P-values vs. adjusted P-values
data(danish)
danish = danish %>% 
  select(Subject, Word, 
         Affix, LogRT) %>% 
  as_tibble()

danish

# Selecionar apenas alguns afixos
danish = danish %>% 
  filter(Affix %in% c("bar", "ende", 
                      "ede", "ere", "lig"))

# Visualizar afixos
ggplot(danish, aes(x = Affix, y = LogRT)) + 
  stat_summary()
#  geom_boxplot()

# Ordenar o gráfico
ggplot(data = danish, aes(x = reorder(Affix, LogRT), y = LogRT)) +
  stat_summary() 

# Rodamos uma anova nos 5 afixos:
danAnova = aov(LogRT ~ Affix, data = danish)
summary(danAnova)

# Post-hoc: observe ede-bar -> p = 0.06 -> n.s.
TukeyHSD(danAnova)

# Visualizando as múltiplas comparações:
plot(TukeyHSD(danAnova))

# Agora, imagine que desejamos apenas analisar ede e bar
# Neste caso, podemos rodar um t-test
edebar = danish %>% 
  filter(Affix %in% c("ede", "bar"))

t.test(LogRT ~ Affix, data = edebar)
# No teste t, p < 0.05

# Este foi um exemplo simples que mostra uma aparente contradição
# que resulta do ajuste de valor-p

# Ou seja, dependendo da nossa intenção a priori,
# nossa conclusão sobre ede-bar é categoricamente distinta
# Isso mostra mais um problema sobre a análise estatística
# baseada principalmente em p-values: isso simplifica demais
# a análise como um todo. Essa categorização/dicotomização
# entre "significativo" e "não significativo" é claramente
# simplista demais para aquilo que desejamos em uma análise.

# Quem conhce pouco de análises de dados pode cometer um erro
# Quem conhcece bastante pode fazer p-hacking


# Diferenças de F1 por vogal para os dados vogaisPB

## -->> Tarefa de casa =)


##########################
# Testes não paramétricos
##########################

# T-test and ANOVA assumptions:
# - Distribuição normal dos dados (variável resposta)
# - Homocedasticidade (homogeneidade de variância)

# Para verificar a normalidade da distribuição dos dados:

# 1. Histograma
ggplot(english, aes(x = RTlexdec)) +
  geom_histogram() 

ggplot(english, aes(x = RTlexdec)) +
  geom_histogram(fill = "gray", color = "black", bins = 50) 

# 2. Shapiro test
## H0: dados vêm de uma distribuição normal 
## p > 0.05 = teste paramétrico (t-test ou ANOVA)
## p < 0.05 = teste não paramétrico (Teste de Wilcoxon ou Kruskal-Wallis)
shapiro.test(english$RTlexdec)

# Para verificar a homocedasticidade:
# Levene's Test
## H0: Todas as variâncias são iguais
## p > 0.05 = teste paramétrico (t-test ou ANOVA)
## p < 0.05 = teste não paramétrico (Teste de Wilcoxon ou Kruskal-Wallis)
library(car)

leveneTest(RTlexdec ~ AgeSubject, english)
leveneTest(RTlexdec ~ CV, english)
leveneTest(RTlexdec ~ WordCategory, english)

# Teste de Wilcoxon
wilcox.test(RTlexdec ~ AgeSubject, english)
wilcox.test(RTlexdec ~ CV, english)

# Teste de Kruskal-wallis
# ex.:
kruskal.test(dur ~ vogal, data = vogais) 

# O teste pareado post-hoc não paramétricos é o 
# Wilcoxon Pairwise com correção de Bonferroni
pairwise.wilcox.test(vogais$F1, vogais$vogal, # a sintaxe muda, é preciso indicar as colunas
                     p.adjust.method="bonf")

# Obs.: são amostras independentes, se fossem dependentes (mesmo grupo
# em momentos diferentes), adicionar o argumento "paired = T"


# E os dados de duração e F1 dos dados vogaisPB, obedecem os dois requisitos?

## -->> Tarefa de casa =)


##########################
# Tamanho do efeito
##########################
library(effectsize)

# Cohen's d para testes t
## Ex.:
cohens_d(RTlexdec ~ AgeSubject, data = english)
#|d|<0.2 "negligible", |d|<0.5 "small", |d|<0.8 "medium", otherwise "large"


# Eta-squared para ANOVAs
## Ex.: 
eta_squared(anovaVogais)
# η2 = 0.01 small effect
# η2 = 0.06 medium effect
# η2 = 0.14 large effect

