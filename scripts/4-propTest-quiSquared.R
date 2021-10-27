

# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 4 - proportions test and qui-squared test
# Last updated in oct/2021

# =================================================

# Carregar pacotes
library(tidyverse)


##########################
# TESTE DE PROPORÇÃO
##########################

# -->> Para avaliar frequências/proporções em uma variável nominal

# Simular jogadas de moeda
# 8 caras e 2 coroas
coin1 = c(rep("head", times = 8),
         rep("tail", times = 2))

# Visualizar tabela de frequências
table(coin1)

# Visualizar tabela de proporções
prop.table(table(coin1))

## Salvar tabela de proporções (para gráfico)
prop1 = data.frame(prop.table(table(coin1)))

# Visualizar proporções em gráfico
ggplot(prop1, aes(x = coin1, y = Freq)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  labs(y = "Proporção", x = "Resultado")

# Rodar o teste de proporções
## H1: A moeda é adulterada
## H0: A moeda é justa
## --> Hipótese não direcional -> teste bicaudal
prop.test(table(coin1))

## H1: A moeda é adultareda para cair mais caras
## H0: A moeda é justa
## --> Hipótese direcional -> teste unicaudal
prop.test(table(coin1), alternative = "greater") # p = p/2 do teste bicaudal


# Simular jogadas de moeda
# 80 caras e 20 coroas
coin2 = c(rep("head", times = 80),
          rep("tail", times = 20))

# Visualizar tabela de frequências
table(coin2)

# Visualizar e salvar tabela de proporções
prop.table(table(coin2))
prop2 = data.frame(prop.table(table(coin2)))

# Visualizar proporções em gráfico
ggplot(prop2, aes(x = coin2, y = Freq)) +
  geom_col()

# Rodar o teste de proporções
prop.test(table(coin2))


# Simular jogadas de moeda
# 800 caras e 200 coroas
coin3 = c(rep("head", times = 800),
          rep("tail", times = 200))

# Rodar o teste de proporções
prop.test(table(coin3))


##########################
# TESTE DE QUI-QUADRADO
##########################

# -->> Variável preditora nominal e variável resposta nominal

# Carregar o arquivo "LabovDS" num objeto "labov"
labov = read_csv("data/LabovDS.csv")
str(labov)

labov = labov %>% 
  mutate_if(is.character, as.factor)
summary(labov)

# Excluir os casos de dúvida (d) em r
labov = labov %>% 
  filter(r != "d") %>% 
  droplevels()
summary(labov)

# Obs: se fossem todas essas observaçes de /r/ de uma mesma loja, com mesma
# ênfase e da mesma palavra, e quiséssemos apenas testar a probabilidade
# de 499 apagamentos contra 231 produções de /r/, o que faríamos?
# Um teste de proporções: 
prop.test(table(labov$r))

# Mas aqui queremos testar a proporção de /r/ em relação às demais
# variáveis preditoras (loja, ênfase e palavra)
# Para isso vamos logo criar uma tabela da proporção de APAGAMENTO 
# (fenômeno de interesse) em relação a todas as variáveis:
props = labov %>% 
  group_by(store, emphasis, word, r) %>% 
  count() %>% 
  group_by(store, emphasis, word) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(r == "r0")

# PERGUNTA 1:
# Efeito da palavra (posição do /r/) na taxa de apagamento
props %>% 
  group_by(word) %>% 
  summarize(prop = mean(prop))

## plot
ggplot(props, aes(x = word, y = prop)) +
  stat_summary(geom = "bar")

## optional arguments for plot
ggplot(props, aes(x = word, y = prop)) +
  stat_summary(geom = "bar") +
  theme_classic() +
  labs(x = "Word",
       y = "Deletion of /r/ (proportion)") +
  coord_cartesian(ylim = c(0, 1)) 

# Teste de qui-quadrado
## H1: a palavra influencia a taxa de apagamento do /r/
## H0: não há efeito de palavra na taxa de apagamento do /r/
chisq.test(table(labov$word, labov$r))


# PERGUNTA 2:
# Efeito da loja na taxa de apagamento
props %>% 
  group_by(store) %>% 
  summarize(prop = mean(prop))

## plot
ggplot(props, aes(x = store, y = prop)) +
  stat_summary(geom = "bar") 

## optional arguments for plot
ggplot(props, aes(x = store, y = prop)) +
  stat_summary(geom = "bar") +
  theme_classic() +
  labs(x = "Store",
       y = "Deletion of /r/ (proportion)") +
  coord_cartesian(ylim = c(0, 1)) 

# Teste de qui-quadrado
## H1: a palavra influencia a taxa de apagamento do /r/
## H0: não há efeito de palavra na taxa de apagamento do /r/
chisq.test(table(labov$store, labov$r))

# Mas entre quais das lojas há diferença?
# Se houver entre as duas menores (Macys x Saks), haverá nas outras
labov.noKlein = labov %>% 
  filter(store != "Klein") %>% 
  droplevels()
summary(labov.noKlein)

chisq.test(table(labov.noKlein$store, labov.noKlein$r))

# PERGUNTA 3:
# Efeito da ênfase na taxa de apagamento

## -->> Tarefa de casa =)

# -------------------------- FIM ------------------------------------


