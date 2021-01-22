# Carga las librerías necesarias
library(tidyverse)
library(tidytext) 
library(tm)
library(topicmodels)

# Lee el texto de las Siete Partidas
partidas <- read_tsv("https://tinyurl.com/SPAntwerpen-1")

# Divide por capítulos. Solo válido para aquellos textos en los que los capítulos
# estén marcados con el término CAPÍTULO. No importa si está en mayúscula, minúscula
# o si está correctamente acentuado.


# Divide (tokeniza) en palabras por Título
por_partida_palabras <- partidas %>%
  group_by(partida) %>%
  unite(texto, rubrica, texto, sep = " ") %>%
  unnest_tokens(palabra, texto) %>%
  ungroup()
# Mira como queda ahora
por_partida_palabras

por_partida_palabras %>%
  count(partida, palabra, sort = T)

# Cargamos un fichero con palabras vacías específicas para el castellano medieval

vacias <- read_tsv("https://tinyurl.com/SPAntwerpen-2")

# Elimina palabras vacías
palabra_conteo <- por_partida_palabras %>%   
  anti_join(vacias) %>%   
  count(partida, palabra, sort = TRUE)

# Y ahora vemos el comienzo de lo que queda

palabra_conteo


# Creamos una tabla con las palabras vacías especiales
especiales <- tibble(palabra = c("cosa", "cosas", "deue", "deuen", "dezimos",
                                 "dezir", "fazen", "fazer", "ley", "manera",
                                 "ome", "omes", "puede", "pueden", "razon",
                                 "dar", "dado", "tenudo", "seria"))
# Las eliminamos
palabra_conteo <- palabra_conteo %>%   
  anti_join(especiales)

# Y ahora podemos saber cuántas palabras de "valor" hay en cada partida
# y cuál es su frecuencia

palabra_conteo

# En este momento, este dataframe está ordenado,
# con un término por documento por fila. 
# Sin embargo, el paquete TOPICMODELS requiere un
# DocumentTermMatrix (del paquete TM). 
# Se logra la adaptación a DocumentTermMatrix con cast_dtm de tidytext:

partidas_dtm <- palabra_conteo %>%
  cast_dtm(partida, palabra, n)

# partidas_dtm

# Ahora estás listo para usar el paquete topicmodels y crear un modelo LDA 
# con varios tópicos. Lo vas hacer con uno por cada Partida
# Por eso el valor de k = 7

partidas_lda <- LDA(partidas_dtm, k = 7, control = list(seed = 1234)) # Ojo al valor de k

# partidas_lda

# (En este caso, sabemos que hay 7 temas porque hay 7 libros, 
# en la práctica es posible que tengamos que probar algunos valores diferentes para k).
# Ahora tidytext da la opción de volver a un análisis ordenado, 
# utilizando las voces ordenadas.

partidas_lda_td <- tidy(partidas_lda, matrix = "beta")
partidas_lda_td %>% print(n = 21)


# Para cada combinación, el modelo decide la probabilidad de que ese término
# se genere a partir de ese tópico
# top_n N términos principales dentro de cada tópico

terminos_frecuentes <- partidas_lda_td %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terminos_frecuentes %>% print(n = Inf)

# El modelo se presta a una visualización:

theme_set(theme_bw())


terminos_frecuentes %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

