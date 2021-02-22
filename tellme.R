# Instala las librerías
install.packages("tidyverse")
install.packages("tidytext") 
install.packages("tm")
install.packages("topicmodels")

# Esta acción solo la tienes que hacer la primera vez
# que uses RStudio. En sesiones sucesivas o con scripts
# sucesivos que usen estas liberías basta con que cargues
# las librerías.

# Carga las librerías necesarias
# Se hace siempre
library(tidyverse)
library(tidytext) 
library(tm)
library(topicmodels)

# Lee el texto de Cuéntame…
cuentame <- read_tsv("https://raw.githubusercontent.com/7PartidasDigital/Antwerpen/main/cuentame.tsv")

# Divide por temporadas
# Divide (tokeniza) en palabras por temporada
por_temporada_palabras <- cuentame %>%
  group_by(temporada) %>%
  unnest_tokens(palabra, texto) %>%
  ungroup()


por_temporada_palabras %>%
  count(temporada, palabra, sort = T)

# Cargamos un fichero con palabras vacías específicas para el castellano medieval

vacias <- read_tsv("https://tinyurl.com/7PartidasVacias")

# Elimina palabras vacías
palabra_conteo <- por_temporada_palabras %>%   
  anti_join(vacias) %>%   
  count(temporada, palabra, sort = TRUE)

# Y ahora vemos el comienzo de lo que queda

palabra_conteo


# Creamos una tabla con las palabras POR DEFINIR
especiales <- tibble(palabra = c(""))
# Las eliminamos
palabra_conteo <- palabra_conteo %>%   
  anti_join(especiales)
# Y ahora podemos saber cuántas palabras de "valor" hay en cada temporada
# y cuál es su frecuencia
palabra_conteo



# En este momento, este dataframe está ordenado,
# con un término por documento por fila. 
# Sin embargo, el paquete TOPICMODELS requiere un
# DocumentTermMatrix (del paquete TM). 
# Se logra la adaptación a DocumentTermMatrix con cast_dtm de tidytext:

temporadas_dtm <- palabra_conteo %>%
  cast_dtm(temporada, palabra, n)

# temporadas_dtm

# Ahora estás listo para usar el paquete topicmodels y crear un modelo LDA 
# con varios tópicos. Lo vas hacer con uno por cada temporada
# Por eso el valor de k = 10

temporadas_lda <- LDA(temporadas_dtm, k = 21, control = list(seed = 1234)) # Ojo al valor de k

# temporadas_lda

# (En este caso, sabemos que hay 7 temas porque hay 7 libros, 
# en la práctica es posible que tengamos que probar algunos valores diferentes para k).
# Ahora tidytext da la opción de volver a un análisis ordenado, 
# utilizando las voces ordenadas.

temporadas_lda_td <- tidy(temporadas_lda, matrix = "beta")
temporadas_lda_td


# Para cada combinación, el modelo decide la probabilidad de que ese término
# se genere a partir de ese tópico
# top_n N términos principales dentro de cada tópico

terminos_frecuentes <- temporadas_lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
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







cuentame <- read_tsv("https://raw.githubusercontent.com/7PartidasDigital/Antwerpen/main/cuentame.tsv")

palabras_temporada <- cuentame %>% 
  unnest_tokens(palabra, texto) %>% 
  count(temporada, palabra, sort = TRUE)

total_palabras <- palabras_temporada %>%
  group_by(temporada)

total_palabras <- palabras_temporada %>%
  group_by(temporada) %>%
  summarize(total = sum(n))

palabras_temporada <- left_join(palabras_temporada,
                               total_palabras) %>%
  bind_tf_idf(palabra,
              temporada,
              n) %>%
  group_by(temporada) %>%
  top_n(5, tf_idf) %>%
  ungroup()

# Listado
palabras_temporada %>%
  count(temporada, palabra, tf_idf) %>%
print(n = 50)

ggplot(palabras_temporada,
       aes(reorder(palabra,
                   tf_idf),
           tf_idf,
           fill = temporada)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap( ~ temporada, ncol = 3, scales = "free") +
  coord_flip()



# CARACTERÍSTICA POR EPISODIO

cuentame <- read_tsv("https://raw.githubusercontent.com/7PartidasDigital/Antwerpen/main/cuentame.tsv")


#####
palabras_episodio <- cuentame %>% 
  unnest_tokens(palabra, texto) %>% 
  count(episodio, palabra, sort = TRUE)

total_palabras <- palabras_episodio %>%
  group_by(episodio)

total_palabras <- palabras_episodio %>%
  group_by(episodio) %>%
  summarize(total = sum(n))

palabras_episodio <- left_join(palabras_episodio,
                               total_palabras) %>%
  bind_tf_idf(palabra,
              episodio,
              n) %>%
  group_by(episodio) %>%
  top_n(5, tf_idf) %>%
  ungroup()


palabras_episodio %>%
  count(episodio, palabra, tf_idf) #%>%
  print(n = 50)



ggplot(palabras_episodio,
       aes(reorder(palabra,
                   tf_idf),
           tf_idf,
           fill = episodio)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap( ~ episodio, ncol = 10, scales = "free") +
  coord_flip()

#####

