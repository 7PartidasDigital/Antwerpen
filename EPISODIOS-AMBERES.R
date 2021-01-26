# Estas cinco instrucciones solo tienes que ejecutarlas la
# primera vez que uses RStudio. Puede llevarte algo
# tiempo

install.packages("tidyverse")
install.packages("tidytext")
install.packages("tm")
install.packages("topicmodels")
install.packages("scales")


# Carga las librerías

library(tidyverse)
library(tidytext) 
library(tm)
library(topicmodels)
library(scales)

# Carga la lista de stopwords españolas
vacias <- read_tsv("https://tinyurl.com//7PartidasVacias")

# Los textos los cargas desde un repositorio externo esta es la ruta
ruta <- "https://tinyurl.com/AmberesGaldos/"

# Aquí tienes los títulos de los Episodios que vas a analizar
titulos <- c("Trafalgar",
             "La Corte de Carlos IV",
             "El 19 de Marzo y el 2 de Mayo",
             "Bailén",
             "Napoleón en Chamartín",
             "Zaragoza",
             "Gerona",
             "Cádiz",
             "Juan Martín El Empecinado",
             "La Batalla de los Arapiles",
             "El Equipaje del Rey José",
             "Memorias de un cortesano de 1815",
             "La Segunda Casaca",
             "El Grande Oriente",
             "7 de Julio",
             "Los Cien Mil Hijos de San Luis",
             "El Terror de 1824",
             "Un voluntario realista",
             "Los Apostólicos",
             "Un faccioso más y algunos frailes menos",
             "Zumalacárregui",
             "Mendizábal",
             "De Oñate a La Granja",
             "Luchana",
             "La campaña del Maestrazgo",
             "La estafeta romántica",
             "Vergara",
             "Montes de Oca",
             "Los Ayacuchos",
             "Bodas reales",
             "Las tormentas del 48",
             "Narvaez",
             "Los duendes de la camarilla",
             "La Revolucion de Julio",
             "O'Donnell",
             "Aita Tettauen",
             "Carlos VI en La Rápita",
             "La vuelta al mundo en La Numancia",
             "Prim",
             "La de los tristes destinos",
             "España sin Rey",
             "España trágica",
             "Amadeo I",
             "La Primera República",
             "De Cartago a Sagunto",
             "Cánovas")

# Estos son los nombres de los ficheros de cada uno de los
# Episodios en el repositorio de 7PartidasDigital
ficheros <- c("01_EN-01-01-Trafalgar.txt",
              "02_EN-01-02-La_Corte_de_Carlos_IV.txt",
              "03_EN-01-03-El_19_de_Marzo_y_el_2_de_Mayo.txt",
              "04_EN-01-04-Bailen.txt",
              "05_EN-01-05-Napoleon_en_Chamartin.txt",
              "06_EN-01-06-Zaragoza.txt",
              "07_EN-01-07-Gerona.txt",
              "08_EN-01-08-Cadiz.txt",
              "09_EN-01-09-Juan_Martin_El_Empecinado.txt",
              "10_EN-01-10-La_Batalla_de_los_Arapiles.txt",
              "11_EN-02-01-El_Equipaje_del_Rey_Jose.txt",
              "12_EN-02-02-Memorias_de_un_cortesano_de_1815.txt",
              "13_EN-02-03-La_Segunda_Casaca.txt",
              "14_EN-02-04-El_Grande_Oriente.txt",
              "15_EN-02-05-7_de_Julio.txt",
              "16_EN-02-06-Los_Cien_Mil_Hijos_de_San_Luis.txt",
              "17_EN-02-07-El_Terror_de_1824.txt",
              "18_EN-02-08-Un_voluntario_realista.txt",
              "19_EN-02-09-Los_Apostolicos.txt",
              "20_EN-02-10-Un_faccioso_mas_y_algunos_frailes_menos.txt",
              "21_EN-03-01-Zumalacarregui.txt",
              "22_EN-03-02-Mendizabal.txt",
              "23_EN-03-03-De_Onate_a_La_Granja.txt",
              "24_EN-03-04-Luchana.txt",
              "25_EN-03-05-La_campana_del_Maestrazgo.txt",
              "26_EN-03-06-La_estafeta_romantica.txt",
              "27_EN-03-07-Vergara.txt",
              "28_EN-03-08-Montes_de_Oca.txt",
              "29_EN-03-09-Los_Ayacuchos.txt",
              "30_EN-03-10-Bodas_reales.txt",
              "31_EN-04-01-Las_tormentas_del_48.txt",
              "32_EN-04-02-Narvaez.txt",
              "33_EN-04-03-Los_duendes_de_la_camarilla.txt",
              "34_EN-04-04-La_Revolucion_de_Julio.txt",
              "35_EN-04-05-ODonnell.txt",
              "36_EN-04-06-Aita_Tettauen.txt",
              "37_EN-04-07-Carlos_VI_en_la_Rapita.txt",
              "38_EN-04-08-La_vuelta_al_mundo_en_La_Numancia.txt",
              "39_EN-04-09-Prim.txt",
              "40_EN-04-10-La_de_los_tristes_destinos.txt",
              "41_EN-05-01-Espana_sin_Rey.txt",
              "42_EN-05-02-Espana_tragica.txt",
              "43_EN-05-03-Amadeo_I.txt",
              "44_EN-05-04-La_Primera_Republica.txt",
              "45_EN-05-05-De_Cartago_a_Sagunto.txt",
              "46_EN-05-06-Canovas.txt")
episodios <- tibble(texto = character(),
                  titulo = character(),
                  pagina = numeric())

# Lee los texto y los divide en "páginas".
# Podríamos dividirlos capítulos, tal y como hace Silge
# con las cuatro novelas que analiza

# Ten paciencia, este proceso le llevará un ratito.
# Los 46 EN contienen 3 276 648 palabras-token (gráficas) y
# 91291 palabras-tipo. Es un buen montón. Ten encuenta, también,
#que lo hemos dividido en 8767 páginas de 375 palabras cada una,
# aunque puedes hacerlas más largas si cambias el 375 que hay en
# la línea 146.

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

# Si quieres echarle una ojeda la tabla, haz clic en palabra-conteo.
# Se abrirá una nueva ventana y podrás recorrerla.

paginas_dtm <- palabra_conteo %>%
  cast_dtm(titulo_pagina, palabra, n)

# Calcula los tópicos. k es la clave. He utilizado 20, pera nada
# impide que no puedas usar más o menos.
# Juega con ese valor para establecer el mejor número de tópicos
# Recuerda que lo importante lo complicado es etiquetar los tópicos,
# que solo te aparecerán como un número

paginas_lda <- LDA(paginas_dtm, k = 20, control = list(seed = 1234))
paginas_lda_td <- tidy(paginas_lda, matrix = "beta")

# top_n(10 ) Si se cambia el 10 por otro número, esas serán las
# palabras que imprima en el gráfico. Puedes modificarlo a tu
# gusto y según tus necesidades
terminos_frecuentes <- paginas_lda_td %>%
  group_by(topic) %>%
  top_n(25, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
terminos_frecuentes %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(caption = "7PartidasDigital 2021") +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
