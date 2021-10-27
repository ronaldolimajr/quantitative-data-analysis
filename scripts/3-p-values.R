

# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 3 - p-values
# Last updated in out/2021

# =================================================

# Carregar pacotes
library(tidyverse)


##########################
# INTRODUÇÃO
##########################

# Simular uma população de 100 mil alunos com seus resultados em um teste
population = rbeta(100000, 5, 2)

# Distribuição das notas da POPULAÇÃO de alunos
summary(population)
sd(population)

hist(population)

# Vamos extrair 3 amostras dessa população, cada uma com 20 pessoas,
# simulando 3 turmas diferentes de alunos vindos da mesma população
sample1 = sample(x = population, size = 20)
sample2 = sample(x = population, size = 20)
sample3 = sample(x = population, size = 20)

# Criar um tibble (data frame) com os dados das 3 turmas simuladas (samples)
sample.data = tibble(class1 = sample1,
                     class2 = sample2,
                     class3 = sample3) %>% 
  gather("class1", "class2", "class3", key = class, value = test)

# Vamos conduzir um EDA (Exploratory Data Analysis) com os dados da turma
sample.data %>% 
  group_by(class) %>% 
  summarize(Test.mean = mean(test),
            Test.SD = sd(test))

ggplot(sample.data, aes(x = class, y = test)) +
  geom_boxplot() +
  stat_summary(color = "blue")

## -> Rode novamente o código a partir da linha 35 até a 53
## diversas vezes e veja como mudam os valores das 3 turmas

# LIÇÃO: como conhecemos a população (porque nós que criamos), sabemos
# que essas diferenças são aleatóreas, por causa do erro de amostragem
# (sampling error), mas quando não conhecemos a população (o que é o mais comum)
# e temos apenas amostras para inferir parâmetros da população, podemos facilmente
# inferir que essas diferenças são por efeito de alguma variável. Aí que entra
# a estatística inferencial. A grande pergunta é: qual é a probabilidade de se 
# encontrar diferenças desse tamanho (ou mais extremas) caso esses dados sejam da 
# mesma popualação?


# E se tivermos uma amostra maior? Vamos simular amostras de 3 turmas com 
# 50 alunos cada
sample4 = sample(x = population, size = 50)
sample5 = sample(x = population, size = 50)
sample6 = sample(x = population, size = 50)

# Criar um tibble (data frame) com os dados das 3 turmas simuladas (samples)
sample.data2 = tibble(class4 = sample4,
                     class5 = sample5,
                     class6 = sample6) %>% 
  gather("class4", "class5", "class6", key = class, value = test)

# Vamos conduzir um EDA (Exploratory Data Analysis) com os dados da turma
sample.data2 %>% 
  group_by(class) %>% 
  summarize(Test.mean = mean(test),
            Test.SD = sd(test))

ggplot(sample.data2, aes(x = class, y = test)) +
  geom_boxplot() +
  stat_summary(color = "blue")

# Teste aumentar a amostra para 100, 200, 1000 alunos e veja como 
# as diferenças entre turmas sem comportam. Qual é a conclusão
# sobre o tamanho da amostra?

##########################
# VALOR DE p
##########################

# Comecemos com noções básicas de probabilidade

## Qual é a probabilidade de caírem 3 caras em 3 jogadas
## onde a probabilidade de cair cara é 0.5?
dbinom(3, 3, 0.5)

## Qual é a probabilidade de caírem 0, 1, 2 e 3 caras em 3 jogadas
## onde a probabilidade de cair cara é 0.5?
dbinom(0:3, 3, 0.5)

barplot(dbinom(0:3, 3, 0.5), names.arg = dbinom(0:3, 3, 0.5))

## E de caírem 0, 1, 2, ..., 6 caras em 6 jogadas?
dbinom(0:6, 6, 0.5)

barplot(dbinom(0:6, 6, 0.5), names.arg = dbinom(0:6, 6, 0.5))

## e 12, 50 e 100 caras em 12, 50 e 100 jogadas?
barplot(dbinom(0:12, 12, 0.5))
barplot(dbinom(0:50, 50, 0.5))
barplot(dbinom(0:100, 100, 0.5))

# A soma de todas as probabilidade sempre será 1 (100%)
sum(dbinom(0:3, 3, 0.5))
sum(dbinom(0:100, 100, 0.5))
sum(dbinom(0:50, 50, 0.5))


## Com quantas aparições de cara ou coroa você diria que eu estou roubando?
## Pense num númeor e o guarde em uma variável x. 
## Ex:
x = 65

# Com 90 caras ou mais em 100 jogadas, a maioria das pessoas acharia que 
# a moeda é fraudulenta. Qual é a probabilidade de saírem 90 ou mais caras em 100
# jogadas em uma moeda não adulterada?
sum(dbinom(90:100, 100, 0.5))


# Essa probabilidade é tão pequena (0.00000000000000001531645) que certamente assumiríamos
# que se trata de umna moeda adulterada. Essa probabilidde que calculamos é o valor-p:
# a probabilidade da observação que encontramos ou mais extrema que ela sem nada alterando
# o mundo, ao acaso (i.e., dada a Hipótese Nula - H0).
# A partir de qual probabilidade consideramos o resultado pequeno o suficiente para
# descartarmos a H0? Esse valor é o nosso alfa, comumente/tradicionalmente 5% (0.05).

## A partir de quantas caras/coroas em 100 jogadas a probabilidade é < ou = a 0.05?
qbinom(0.05, 100, 0.5, lower.tail = F)
sum(dbinom(58:100, 100, 0.5))
sum(dbinom(59:100, 100, 0.5))

# Qual é a probabilidde de sairem a quantidade de caras que você estabeleceu em X ou mais?
sum(dbinom(x:100, 100, 0.5))
# Esse valor foi o seu alfa - foi maior ou menos (mais ou menos rígido) que o clássico 5%?


# Este é um teste unicaudal (one-tailed), pois parte-se de uma hipótese direcional
# Simula um jogador desconfiando do outro
# No caso de um juiz observando os dois jogadores, ele previamente deve desconfiar dos dois,
# isto é, se um tirar caras demais ou o outro tirar coroas demais, alguém está roubando.
# Nesse caso, parte-se de uma hipótese não-direcional e utilizamos um teste bicaudal (two-tailed)

# A partir de quantas caras OU coroas devemos desconfiar que alguém está roubando?
# Se a probabilidade de tirar X caras ou X coroas for menor que 5% com uma moeda justa,
# podemos começar a desconfiar. Ficamos, então, com 2.5% de probabilidade em cada cauda.
qbinom(0.05/2, 100, 0.5, lower.tail=FALSE)
sum(dbinom(c(0:40, 60:100), 100, 0.5))
sum(dbinom(c(0:39, 61:100), 100, 0.5))


##################################################
# VALOR DE p dos dados que simulamos no início
##################################################

# Para saber se há "diferença estatisticamente significativa"
# entre algum par das 3 turmas simuladas com 20 alunos
summary(aov(data = sample.data, test ~ class))

# Caso haja, para saber entre qual/quais pares a diferença é "estatisticamente significativa"
TukeyHSD(aov(data = sample.data, test ~ class))


# agora entre as 3 turmas simuladas com 50 alunos
summary(aov(data = sample.data2, test ~ class))

# Caso haja, para saber entre qual/quais pares a diferença é "estatisticamente significativa"
TukeyHSD(aov(data = sample.data2, test ~ class))

# -------------------------- FIM ------------------------------------
