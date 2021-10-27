
# Universidade Federal do Ceará
# Programa de Pós-Graduação em Linguística
# Análise Quantitativa de Dados em Linguística

# Prof. Dr. Ronaldo Lima Jr.
## ronaldojr@letras.ufc.br
## ronaldolimajr.github.io

# SCRIPT 3.1 - used to create plots for slides presentation on p-values
# Last updated in out/2021

# =================================================

# Carregar pacotes
library(tidyverse)
library(RColorBrewer)

# Criar tibble e histograma com os valores da população
pop = tibble(id = 1:100000, score = population)

ggplot(pop, aes(x = score)) +
  geom_histogram(color = "chocolate", fill = "chocolate1", alpha = 0.7) +
  theme_minimal()

ggplot(pop, aes(x = score)) +
  geom_histogram(fill = "white") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

# Criar tibbles com as distribuições binomiais e os plots com ggplot
## 3
binom3 = tibble(heads = 0:3,
                probability = dbinom(0:3, 3, 0.5)) 

ggplot(binom3, aes(x = heads, y = probability)) +
  geom_col() +
  theme_minimal() 

## 6
binom6 = tibble(heads = 0:6,
                probability = dbinom(0:6, 6, 0.5)) 

ggplot(binom6, aes(x = heads, y = probability)) +
  geom_col() +
  theme_minimal() 

## 12
binom12 = tibble(heads = 0:12,
                probability = dbinom(0:12, 12, 0.5)) 

ggplot(binom12, aes(x = heads, y = probability)) +
  geom_col() +
  theme_minimal() 

## 50
binom50 = tibble(heads = 0:50,
                probability = dbinom(0:50, 50, 0.5)) 

ggplot(binom50, aes(x = heads, y = probability)) +
  geom_col() +
  theme_minimal() 

## 100
binom100 = tibble(heads = 0:100,
                  probability = dbinom(0:100, 100, 0.5)) 

ggplot(binom100, aes(x = heads, y = probability)) +
  geom_col() +
  theme_minimal() 

## Plot de binom100 com linha em 90 caras
ggplot(binom100, aes(x = heads, y = probability)) +
  geom_col() +
  geom_vline(xintercept = 90, color = "blue", linetype = "dashed") +
  #geom_rect(aes(xmin = 90, xmax = 100, ymin = 0, ymax = 0.08),
  #          fill = "cornflowerblue") +
  theme_minimal() 

## Binom100 separando p < 0.05 (em 59 caras)
binom100.59 = binom100 %>% 
  mutate(condition = if_else(heads > 58, "yes", "no"))

ggplot(binom100.59, aes(x = heads, y = probability, fill = condition)) +
  geom_col() +
  scale_fill_brewer(direction = -1, palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "none")

## Binom100 separando p < 0.05 bicaudal (em 39 e 61)
binom100.39.61 = binom100 %>% 
  mutate(condition = case_when(heads > 60 ~ "yes", 
                               heads < 40 ~ "yes",
                               TRUE ~ "no"))

ggplot(binom100.39.61, aes(x = heads, y = probability, fill = condition)) +
  geom_col() +
  scale_fill_brewer(direction = -1, palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "none")

# -------------------------- FIM ------------------------------------