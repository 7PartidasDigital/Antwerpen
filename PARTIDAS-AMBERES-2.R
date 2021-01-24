# Cargamos las librerías
library(tidyverse)
library(tidytext) 
library(tm)
library(topicmodels)


# Lee el texto de las Siete Partidas
partidas <- read_tsv("https://tinyurl.com/SPAntwerpen-1")

# Divide (tokeniza) en palabras por Título
por_titulo_palabras <- partidas %>%
  group_by(partida, titulo) %>%
  unite(partida_titulo, partida, titulo) %>%
  unite(texto, rubrica, texto, sep = " ") %>%
  unnest_tokens(palabra, texto) %>%
  ungroup()

# Mira como queda ahora
por_titulo_palabras

# Echa una ojeada a las palabras más frecuentes. En otros análisis pueden ser útiles
# pero para hacer un modelado de tópicos de nada nos sirven las llamadas palabras
# vacías

por_titulo_palabras %>%
  count(palabra, sort = T)

por_titulo_palabras %>%
  count(partida_titulo, palabra, sort = T)

# Elimina palabras vacías

vacias <- read_tsv("https://tinyurl.com/SPAntwerpen-2")

palabra_conteo <- por_titulo_palabras %>%   
  anti_join(vacias)

palabra_conteo %>%   
  count(partida_titulo, palabra, sort = TRUE)


# Elimina las palabras vacias especiales
especiales <- tibble(palabra = c("cosa", "cosas", "deue", "deuen", "dezimos",
                                 "dezir", "fazen", "fazer", "ley", "manera",
                                 "ome", "omes", "puede", "pueden", "razon",
                                 "dar", "dado", "tenudo", "seria", "parte",
                                 "partes", "fecho", "fecha"))

palabra_conteo <- palabra_conteo %>%   
  anti_join(especiales)


# Y ahora podemos saber cuántas palabras de "valor" hay en cada partida
# y cuál es su frecuencia

palabra_conteo <- palabra_conteo %>%   
  count(partida_titulo, palabra, sort = TRUE)

# En este momento, este dataframe está ordenado, con un término por documento por fila. 
# Sin embargo, el paquete TOPICMODELS requiere un DocumentTermMatrix (del paquete TM). 
# Se logra la adaptación a DocumentTermMatrix con cast_dtm de tidytext:

partidas_dtm <- palabra_conteo %>%
  cast_dtm(partida_titulo, palabra, n)

# Ahora estás listo para usar el paquete topicmodels y crear un modelo LDA 
# con varios tópicos. Lo vas hacer con uno por cada PARTIDA

partidas_lda <- LDA(partidas_dtm, k = 7, control = list(seed = 1234)) # Ojo al valor de k


# (En este caso, sabemos que hay 7 temas porque hay 7 libros, 
# en la práctica es posible que tengamos que probar algunos valores diferentes para k).
# Ahora tidytext da la opción de volver a un análisis ordenado, 
# utilizando las voces ordenadas.

partidas_lda_td <- tidy(partidas_lda, matrix = "beta")

# Para cada combinación, el modelo decide la probabilidad de que ese término se genere a partir de ese tema.
# Ptop_n para ver los N términos principales dentro de cada tema:

terminos_frecuentes <- partidas_lda_td %>%
  group_by(topic) %>%
  top_n(25, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terminos_frecuentes

# El modelo se presta a una visualización:

theme_set(theme_bw())


terminos_frecuentes %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

##############
###### STOP HERE
