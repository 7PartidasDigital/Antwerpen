library(tidyverse)
library(tidytext) 
library(tm)
library(topicmodels)
library(scales)

# Carga la lista de stopwords españolas
vacias <- read_tsv("https://tinyurl.com//7PartidasVacias")
# Los textos los cargas desde un repositorio externo
ruta <- "https://tinyurl.com/AmberesEpisodios/"
titulos <- c("Trafalgar",
             "La Corte de Carlos IV",
             "El 19 de Marzo y el 2 de Mayo",
             "Bailén",
             "Napoleón en Chamartín",
             "Zaragoza",
             "Gerona",
             "Cádiz",
             "Juan Martín El Empecinado",
             "La Batalla de los Arapiles")
ficheros <- c("01_EN-01-01-Trafalgar.txt",
              "02_EN-01-02-La_Corte_de_Carlos_IV.txt",
              "03_EN-01-03-El_19_de_Marzo_y_el_2_de_Mayo.txt",
              "04_EN-01-04-Bailen.txt",
              "05_EN-01-05-Napoleon_en_Chamartin.txt",
              "06_EN-01-06-Zaragoza.txt",
              "07_EN-01-07-Gerona.txt",
              "08_EN-01-08-Cadiz.txt",
              "09_EN-01-09-Juan_Martin_El_Empecinado.txt",
              "10_EN-01-10-La_Batalla_de_los_Arapiles.txt")
episodios <- tibble(texto = character(),
                  titulo = character(),
                  pagina = numeric())
# Lee los texto y los divide en "páginas"
for (j in 1:length(ficheros)){
  texto.entrada <- read_lines(paste(ruta,
                                    ficheros[j],
                                    sep = ""),
                              locale = default_locale())
  texto.todo <- paste(texto.entrada, collapse = " ")
  por.palabras <- strsplit(texto.todo, " ")
  texto.palabras <- por.palabras[[1]]
  trozos <- split(texto.palabras,
                  ceiling(seq_along(texto.palabras)/375))
  for (i in 1:length(trozos)){
    fragmento <- trozos[i]
    fragmento.unido <- tibble(texto = paste(unlist(fragmento),
                                            collapse = " "),
                              titulo = titulos[j],
                              pagina = i)
    episodios <- bind_rows(episodios, fragmento.unido)
  }
}
# Borra todas los objetos que no serán necesarios
rm(ficheros, titulos, trozos, fragmento, ruta, fragmento.unido, texto.entrada, texto.palabras, texto.todo, por.palabras, i, j)
# Divide (tokeniza) en palabras por capítulo
por_pagina_palabras <- episodios %>%   
  unite(titulo_pagina, titulo, pagina) %>%                 
  unnest_tokens(palabra, texto)
# Elimina palabras vacías
palabra_conteo <- por_pagina_palabras %>%   
  anti_join(vacias) %>%   
  count(titulo_pagina, palabra, sort = TRUE) %>%   
  ungroup()

# Quizá sea necesario crear una lista de palabras que no quieras
# que considere porque no informan nada (los verbos dicendi podrían
# ser un caso claro, pero no lo podrás saber hasta que no ejecutes
# todo el script). Cómo hacerlo y cuándo ejecutarlo lo tienes en
# los otros scripts sobre las Partidas.

paginas_dtm <- palabra_conteo %>%
  cast_dtm(titulo_pagina, palabra, n)

# Calcula los tópicos. k es la clave. Hay 10 porque hay diez episodios
# pero puede haber muchos más. Juegas con ese valor para establecer el
# mejor valor

paginas_lda <- LDA(paginas_dtm, k = 10, control = list(seed = 1234))
paginas_lda_td <- tidy(paginas_lda, matrix = "beta")

# top_n(10 ) Si se cambia el 10 por otro número, esas serán las
# palabras que imprima en el gráfico. Puedes moddificarlo a tu
# gusto
terminos_frecuentes <- paginas_lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
terminos_frecuentes %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(caption = "7PartidasDigital 2021") +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
