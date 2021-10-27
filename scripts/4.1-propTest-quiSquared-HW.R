
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 4.1 - proportions test and qui-squared test
# Correção da tarefa de casa

# Last updated in oct/2021

# =================================================


# Dica: em vez de carregar tudo que já tínhamos carregado no script anterior
# (pacotes e dados), criar todos os objetos, e
# fazer todas as modificações (excluir casos de dúvida, por exemplo),
# podemos simplesmente utilizar a função source() para iniciar este script com 
# todo o script anterior rodado:
source("scripts/4-propTest-quiSquared.R")

# 1. Há ocorrência de mais apagamentos em qual nível de ênfase?
table(labov$emphasis, labov$r)

props %>%  
  group_by(emphasis) %>% 
  summarize(prop = mean(prop))

#2. Qual é a porcentagem de ocorrência de apagamento na fala casual 
# e na fala enfática, respectivamente?

# 3. Qual é a H1 e a H0 no caso da ênfase?
## H1: a palavra influencia a taxa de apagamento do /r/
## H0: não há efeito de palavra na taxa de apagamento do /r/

# 4. Os dados favorecem rejeitar a H0?
# 5. Qual é o valor da estatística de qui-quadrado para os dados?
# 6. Qual é o valor de p?

# Teste de qui-quadrado
chisq.test(table(labov$emphasis, labov$r))

# -------------------------- FIM ------------------------------------

