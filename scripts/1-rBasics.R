
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 1 - R basics
# Last updated in out/2021

# =================================================

# Comece criando um R Project (File > New Project...) em uma pasta
# dedicada a este curso

# Comentários começam com # (são ignorados na hora de rodar o script)


#==============================
# Comandos básicos
#==============================

# R para cálculos matemáticos
5 + 5
8 / 3
2 ** 3
2^3 # mais de uma maneira para a mesma coisa
5 + (3 * 5 / 2)
sqrt(81) # sqrt() é uma função, que aceita um número como argumento


# Variáveis
myNumber = 5
myNumber = 5 + 1
another_Number = myNumber^2
another.number = another_Number

myWord = "linguística"
mySentence = "linguística e ciência de dados"

myNumber * 2
myNumber / another.number
myNumber + myWord

nchar(myWord) # número de caracteres


# VETORES = mais de um elemento na mesma variável

# Sequência de números
myNumbers = c(1, 2, 3, 4, 5)
my.numbers = 1:5
my_numbers = seq(from = 1, to = 5, by = 1)
# várias maneiras de atingirmos um mesmo resultado

myNumbers * 5
myNumbers + another.number

# Também podemos criar vetores com strings (character, ou chr)
languages = c("Brazilian Portuguese", "English", "Japanese")

# Podemos acessar diferentes itens em um vetor (slice notation)
languages[3]
languages[1]
languages[c(1, 3)]
languages[4] # Não há 4 itens: NA

str(myNumbers)
str(languages)

# Pense em vetores como "colunas em uma tabela de dados"
# Cada vetor tem apenas UM TIPO de dado (sem misturas)
# Números separados de palavras separadas de logicals etc.
# Exatamente o que teríamos em uma tabela pronta para análise


#==============================
# Importando os dados 
#=============================

# Normalmente carregamos uma planilha de dados já pronta
# e criamos uma variável no R para abrigar essa planilha (data frame, tibble)

# Cada coluna é um vetor, e representa uma variável
# Cada linha representa uma observação

# Importando uma planilha salva como um arquivo de texto delimitado por tabulação 
# (txt tab delimited)
vogais.txt = read.table("data/vogaisPB.txt", header = T)

# Importando uma planilha salva como um arquivo separado por vírgulas 
# (csv - comma separated values)
vogais.csv = read.csv("data/vogaisPB.csv") # no read.csv o argumento header já é default T

# Importando uma planilha de excel
# É preciso utilizar o pacote readxl
install.packages("readxl") # instala o pacote (necessário apenas uma vez)
library(readxl) # carrega o pacote
vogais.xlsx = read_excel("data/vogaisPB.xlsx")

# =================================================

# OBS: decimais com vírgula e csv com ponto e vírgula

# Frequentemente editores de planilhas de brasileiros estão configurados para separar
# os decimais por vígula e salvar arquivos csv com ponto e vírgula.
# Para o R os decimais são separados por ponto
# e os arquivos csv têm vírgulas como separadores. 
# Caso seu arquivo esteja como descrito para brasileiros:

# Adicione o argumento dec = "," em 'read.table()' para arquivos .txt
## Ex.: vogais.txt = read.table("data/vogaisPB.txt", header = T, dec = ",")

# Use a função 'read.csv2()' para arquivos .csv
## Ex.: vogais.csv = read.csv2("data/vogaisPB.csv") 

# A função read_excel faz a identificação e correção automaticamente

# =================================================

# Depois de importar os dados, pode-se visualizá-los ou ter um resumo de sua estrutura

vogais = vogais.csv

vogais             # Imprime todo o data frame (impraticável com data sets grandes)
View(vogais)       # Para ver o data frame como uma planilha (lento e trava com data sets grandes)
names(vogais)      # Para ver os nomes das colunas
head(vogais)       # Para ver as seis primeiras linhas do data frame
tail(vogais)       # Para ver as seis últimas linhas do data frame
str(vogais)        # Para ver a estrutura do data frame

summary(vogais)



#==============================
# O pacote tidyverse 
#=============================

install.packages("tidyverse") # Apenas uma vez
library(tidyverse) # carrega o pacote


# Importando dados via tidyverse
vogaisPB = read_csv("data/vogaisPB.csv")

# Outras opções:
vogaisPB = read_table("data/vogaisPB.txt") # não preicsa de header = T
# Para dados com , separando decimais e ; separando células:
## nomeDoObjeto = read_csv2("arquivo.csv") 
## nomeDoObjeto = read_table2("arquivo.txt") 


# Visualizar as 10 primeiras linhas
vogaisPB

# Criar subconjunto de dados filtrando linhas (observações): FILTER
vogais.a = vogaisPB %>% 
  filter(vogal == "a")

# Criar subconjunto com mais de um filtro
vogais.a.breves = vogaisPB %>% 
  filter(vogal == "a",
         dur < 100)

# Filtrar com mais de um parâmetro da mesma coluna
vogais.a.i.u = vogaisPB %>% 
  filter(vogal %in% c("a", "i", "u"))

# Filtrar com mais de um parâmetro da mesma coluna e mais de um filtro
vogais.a.i.u.filtered = vogaisPB %>% 
  filter(vogal %in% c("a", "i", "u"),
         dur < 100,
         F1 > 500
         ) 

# Selecionando colunas que queremos (ou não queremos): SELECT
vogais.dur = vogaisPB %>% 
  select(-F1)

vogais.dur1 = vogaisPB %>% 
  select(vogal, dur)

# Usando filter e select ao mesmo tempo
vogais.a.dur = vogaisPB %>% 
  filter(vogal == "a") %>% 
  select(-F1)


# Criar nova coluna: MUTATE
vogaisPB = vogaisPB %>% 
  mutate(dur.seg = dur / 1000)

# Também podemos utilizar mutate() para trocar classes de variáveis
str(vogaisPB)
summary(vogaisPB)

vogaisPB = vogaisPB %>% 
  mutate(vogal = as.factor(vogal))

vogaisPB
summary(vogaisPB)

# Podemos usar mutate.if para trocar todas de uma vez
vogaisPB = vogaisPB %>% 
  mutate_if(is.character, as.factor)

vogaisPB

# Podemos adicionar múltiplos passos ao mesmo tempo (incluindo a leitura do arquivo)
vogais.a.breve = read_csv("data/vogaisPB.csv") %>% 
  filter(vogal == "a",
         dur < 100) %>% 
  select(-F1) %>% 
  mutate(dur.seg = dur / 1000) %>% 
  mutate_if(is.character, as.factor)

vogais.a.breve


# Resumir padrões: SUMMARIZE
## qual a média de dur para cada vogal?
vogaisPB %>% 
  group_by(vogal) %>% 
  summarize(MeanDur = mean(dur))

## qual a média de dur e de F1 para cada vogal?
vogaisPB %>% 
  group_by(vogal) %>% 
  summarize(meanDur = mean(dur),
            meanF1 = mean(F1)
            )

## qual a média e o desvio-padrão de dur e de F1 para cada vogal?
vogaisPB %>% 
  group_by(vogal) %>% 
  summarize(meanDur = mean(dur),
            sdDur = sd(dur),
            meanF1 = mean(F1),
            sdF1 = sd(F1)
            )

# Ordenar os resultados: ARRANGE
## por média da duração
vogaisPB %>% 
  group_by(vogal) %>% 
  summarize(meanDur = mean(dur),
            sdDur = sd(dur),
            meanF1 = mean(F1),
            sdF1 = sd(F1)
            ) %>% 
  arrange(meanDur)

# Podemos salvar esse output em um tibble
vogais.summary = vogaisPB %>% 
  group_by(vogal) %>% 
  summarize(meanDur = mean(dur),
            sdDur = sd(dur),
            meanF1 = mean(F1),
            sdF1 = sd(F1)
            ) %>% 
  arrange(meanDur)

vogais.summary


# ===================== FIM ============================
