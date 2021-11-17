
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 7 - Simple Linear Regression
# Last updated in Nov/2021

# =================================================

# Carregar pacotes

library(languageR)
library(tidyverse)
library(sjPlot)
library(effects)


###########################################
# REGRESSÃO LINEAR SIMPLES
###########################################

# Correlção (duas variáveis numéricas contínuas, em mesma quantidade de observações)

# Criar dois vetores numéricos e colocá-los num dataframe
idade = c(1, 2, 3, 4, 5, 5, 5, 6, 7, 8, 8, 9, 11, 12, 12)
altura = c(60, 65, 97, 98, 100, 105, 107, 105, 119, 122, 125, 132, 142, 147, 153)

cor = data.frame(idade, altura)

# Visualizar a distribuição
ggplot(data = cor, aes(x = idade, y = altura)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

# Teste de correlação de Pearson
cor.test(idade, altura)
cor.test(altura, idade) 
  ## mesmo resultado, pois o teste só verifica a co-ocorrência de variação

# r é sempre um número de -1 a +1. 
# -1 indica uma correlação negativa exata, quanto mais x, menos y; 
# +1 indica uma correlação positiva extava, quanto mais x, mais y;
# 0 indica ausência total de correlação.

## r = 0.97 indicando uma correlação de 97%
## Veja que o intervalo de confiança não cruza o zero, 
## pois há certeza de que a correlação é positiva,
## por isso o valor-p < 0.05

# ---------------------------------

# REGRESSÃO LINEAR

# Modelo de regressão para estimar a altura em função da idade
modAltura = lm(altura ~ idade, data = cor)
modAltura

# Para ver os valores estimados pelo modelo
modAltura$fitted.values

# Para ver os resíduos (diferença entre cada valor observado e cada valor estimado)
modAltura$residuals
  ## É o mesmo que 
altura - modAltura$fitted.values

# Ver informações completas do modelo:
summary(modAltura)

# Calcular os intervalos de confiança (95%) dos coeficientes:
confint.lm(modAltura)

# A função geom_smooth() do ggplot coloca por padrão uma banda cinza com 
# o intervalo de confiança (95%) dos coeficientes (retiramos o argumento 'se = F')
ggplot(data = cor, aes(x = idade, y = altura)) +
  geom_point() +
  geom_smooth(method = lm)


############################# Modelo com dados 'english'

# Carregar os dados "english" do pacote LanguageR
  ## não será possível usar o 'english.csv' que compartilhamos porque precisaremos
  ## de uma coluna que não incluímos nesse arquivo
library(languageR)
data(english)
en = english

# Plotar Tempo de Reação por Familiaridade (com a palavra)
ggplot(data = en, aes(x = Familiarity, y = RTlexdec)) +
  geom_point()
  
  ## Conegue enxergar uma possível relação?

## Adicionar  a linha de regressão
ggplot(data = en, aes(x = Familiarity, y = RTlexdec)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = lm) +
  labs(x = "Familiaridade com a palavra", y = "Tempo de reação")

# Estimar tempo de reação em função da familiaridade com a palavra
modEn = lm(RTlexdec ~ Familiarity, data = en)
summary(modEn)

## Familiaridade tem efeito em tempo de reação, com cada unidade de aumento
## em familiaridade diminuindo o tempo de reação em 0,06
## Com um grau de familiaridade = 0, o tempo de reação seria de 6,78
## Contudo, o r-squared mostra que apenas 20% da variação em tempo de reação
## pode ser explicado por familiaridade com a palavra

# Para calcular os intervalos de confiança (95%) dos coeficientes:
confint.lm(modEn)


############################# Modelo com dados 'covariáveis' da Livia Oushiro

# Carregar dados
cov = read_csv("Data sets/covariaveis.csv")

## covariaveis contém dados de 118 falantes paulistanos, com suas respectivas características
## sociais e proporções de emprego de variantes de seis variáveis sociolinguísticas: 
## (i) ditongação de /e/ nasal [~ej], como em 'fazenda' 
## (ii) realização de /r/ em coda como retroflexo
## (iii) apagamento de /r/ em coda 
## (iv) concordância nominal não padrão, como em 'os menino' 
## (v) concordância de 3PP não padrão, como 'eles foi' 
## (vi) concordância de 1PP não padrão, como 'nós vai' 


# Visualizar possível relação entre desvios de concordância verbal de 3PP e de concordância nominal
ggplot(data = cov, aes(x = CV3PP, y = CN)) + 
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  labs(x = "concordância 3a pessoa do plural", y = "concordância nominal")

# Modelo para prever erro de concordância nominal dado o erro de concordância verbal
  ## Neste caso poderia ser o inverso, pois um não necessariamente causa o outro
modCov = lm(CN ~ CV3PP, data = cov)
summary(modCov)

# Para calcular os intervalos de confiança (95%) dos coeficientes:
confint.lm(modCov)

# >>>>>>>>> Visualizar múltiplas correlações

# Há a função pairscor.fnc() do pacote languageR para visualizar uma série de
# correlações entre pares de variáveis de um mesmo dataframe 
# (para tomadas de deciões sobre variáveis a serem incluídas em um modelo).
# Podemos especificar que queremos a plotagem de um histograma de cada vetor com hist = T, 
# e uma linha de regressão com smooth = T. 

pairscor.fnc( ~ cov$EN + cov$RT + cov$R0 + cov$CN + cov$CV3PP + cov$CV1PP, hist = T, smooth = T)

# Na diagonal há os histogramas das distribuições das variantes 
# Na parte inferior há o r de Pearson e rho de Spearman, com os respectivos valores p 
# Na parte superior há os gráficos de dispersão com as linhas de regressão (suavizadas).


############################# Modelo com previsores categóricos (de 2 níveis)

# Carregar os dados 'english' do pacote LanguageR
data(english)
en = english

## Plotar idade dos participantes por Familiaridade com a palavra
ggplot(data = en, aes(x = AgeSubject, y = RTlexdec)) +
  geom_boxplot() 

# Estimar tempo de reação em função do grupo etário
modEnAge = lm(RTlexdec ~ AgeSubject, data = en)
summary(modEnAge)

# São exatamente os valores obtidos para as médias dos grupos com um teste-t
t.test(RTlexdec ~ AgeSubject, data = en)

# Para calcular os intervalos de confiança (95%) dos coeficientes:
confint.lm(modEnAge)

# Podemos plotar o efeito
  ## Com o pacote 'sjPlot'
plot_model(modEnAge)  #ficará mais informativo com mais variáveis previsoras
  ## Com o pacote 'effects'
plot(effect("AgeSubject", modEnAge), grid = T)


############################# Modelo com previsores categóricos (mais de 2 níveis)

# Carregar os dados "vogaisPB"
vogais = read_csv("Data sets/vogaisPB.csv")

# Plotar duração por vogal 
ggplot(data = vogais, aes(x = fct_reorder(vogal, dur), y = dur)) +
  geom_boxplot()

# Estimar duração em função da vogal
modVogais = lm(dur ~ vogal, data = vogais)
summary(modVogais)

# O valor do intercept é a duração estimada para vogal /a/, que é a primeira na ordem alfabética.
# Ser uma vogale, vogalE, vogali, etc. diminui/aumenta a duração conforme os coeficientes
# angulares apresentados, mas significativamente diferentes de zero apenas para i e u.

# Para calcular os intervalos de confiança (95%) dos coeficientes:
confint.lm(modVogais)
  ## Veja que os efeitos com p<0.05 são aqueles cujos ICs não cruzam o zero

# Podemos plotar o efeito (com o pacote 'sjPlot')
plot_model(modVogais)
  ## Veja que os efeitos com p<0.05 são aqueles cujas barras de erro não cruzam o zero

# Este modelo compara as durações das vogais em relação à vogal a, 
# Se quisermos comparar todos os pares, podemos utilizar o TukeyHSD de uma Anova do modelo
TukeyHSD(aov(modVogais))
  ## o resultado será idêntico ao Tukey da ANOVA dur ~ vogal



# ------------------------- FIM ---------------------------


