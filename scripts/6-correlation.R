
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 6 - correlation
# Last updated in Nov/2021

# =================================================

# Carregar pacotes
library(tidyverse)
library(languageR)


##########################
# CORRELAÇÃO
##########################

# -->> Variável preditora e variável resposta contínuas

# A ideia é ver o quanto as duas variáveis co-ocorrem

# A partir de hoje é preciso ler os dados "english" diretamente do pacote languageR
# pois utilizaremos uma variável (Familiarity), que não está na versão que compartilhei
data(english)
english = english %>% 
  select(RTlexdec, Word, Familiarity, AgeSubject) %>% # selecionamos só algumas colunas para trabalhar
  as_tibble()

english

# Veremos a correlação entre tempo de reação e familiaridade com a palavra

# Visualizar os dados
ggplot(data = english, aes(x = Familiarity, y = RTlexdec)) + 
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  theme_light()

# Teste de correlação
cor.test(english$RTlexdec, english$Familiarity)

# r = (+-) 1 --> correlação perfeita, todos os pontos co-ocorrem
# r = (+-) 0.75 --> correlação forte
# r = (+-) 0.5 --> correlação moderada
# r = (+-) 0.25 --> correlação fraca
# r = 0 --> sem correlação alguma, nenhum ponto co-ocorre

# O teste paramétrico de correlação (correlação de Pearson) pressupõe uma
# distribuição normal dos dados
shapiro.test(english$RTlexdec)
shapiro.test(english$Familiarity)

# Como os nossos dados não seguem uma distribuição normal, deveríamos utilizar
# a versão não paramétrica, a correlação de Spearman
cor.test(english$RTlexdec, english$Familiarity, method = "spearman")


###### ATENÇÃO!!! ######

# Mantra da Estatística: CORRELAÇÃO NÃO É SINÔNIMO DE MOTIVAÇÃO!
# (Correlation does not mean causation)
# http://www.tylervigen.com/spurious-correlations


# ------------------------- FIM ---------------------------

