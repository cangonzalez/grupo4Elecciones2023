# para usar R desde VSCode
install.packages("languageserver")

# instalando todos los paquetes de tidyverse
# The tidyverse is a collection of open source packages for the R programming language 
# introduced by Hadley Wickham and his team that "share an underlying design philosophy, 
# grammar, and data structures" of tidy data.
install.packages("tidyverse")
# instalando duckdb
install.packages("duckdb")

#libreria
install.packages("ggplot2")
library(ggplot2)

#libreria 2
install.packages("arrow")
library(arrow)

#libreria de suma
library(dplyr)
library("duckdb")
con <- duckdb::dbConnect(duckdb())

#libreria plotly
install.packages("plotly")
library(plotly)

#libreria
library(patchwork)


#abrir los archivos 
setwd("/Users/candelariagonzalez/Desktop/R_Buenos_aires")

#cargo los datos 
#Cargar datos desde un archivo Parquet usando DuckDB
datos <- dbGetQuery(con, "SELECT * FROM read_parquet('Buenos_Aires.parquet');")
datos

print(names(datos))
#verifico estrutura
str(datos)
-------------------------------------------------------------------------------------

#gráficos 

#gráfico 1

colores_agrupacion <- c("LA LIBERTAD AVANZA" = "purple", 
                          "FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD" = "red",
                          "HACEMOS POR NUESTRO PAIS" = "blue",
                          "JUNTOS POR EL CAMBIO"= "yellow",
                          "UNION POR LA PATRIA"= "lightblue")

#agrupa
datos_resumen <- datos %>%
  group_by(eleccion_tipo, agrupacion_nombre) %>%
  summarise(total_votos = sum(cantidad_votos))

# gráfico 1
ggplot(datos_resumen, aes(x = eleccion_tipo, y = total_votos, fill = agrupacion_nombre)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total de Votos por Agrupación política en cada Tipo de Elección",
       x = "Tipo de Elección",
       y = "Total de Votos") +
  theme_minimal() +
  scale_fill_manual(values = colores_agrupacion) +  # Asigna colores por agrupación
  geom_text(aes(label = scales::comma(total_votos)), position = position_dodge(width = 0.9), vjust = -0.5) +  # Etiquetas con total de votos
  theme(legend.position = "top") +  # Mueve la leyenda a la parte superior
  scale_y_continuous(labels = scales::comma_format())

----------------------------------------
#gráafico 2
# Agrupa 
  #gráafico 2
  # Agrupa los datos por tipo de elección y agrupación, y calcula el total de votos
datos_totales <- datos %>%
  group_by(eleccion_tipo, agrupacion_nombre) %>%
  summarise(total_votos = sum(cantidad_votos))

# Filtro
datos_paso <- datos_totales %>%
  filter(eleccion_tipo == "PASO" & agrupacion_nombre %in% c("LA LIBERTAD AVANZA", "UNION POR LA PATRIA"))

# Gráfico 1 - PASO
grafico_barras_paso <- ggplot(datos_paso, aes(x = total_votos, y = agrupacion_nombre, fill = agrupacion_nombre)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::comma(total_votos), group = agrupacion_nombre), 
            position = position_dodge(width = 0.9), 
            hjust = 1.5, 
            size = 3) +  
  labs(title = "Votos entre La libertad avanza y Union por la patria en las PASO",
       x = "Cantidad de Votos",
       y = "Agrupación") +
  scale_fill_manual(values = c("LA LIBERTAD AVANZA" = "purple", "UNION POR LA PATRIA" = "lightblue")) +  
  theme_minimal() +
  scale_x_continuous(labels = scales::comma, limits = c(0, 5000000))  
----------------------------------------
#gráfico 3
#quería ver los datos
datos_general <- datos_totales %>%
  filter(eleccion_tipo == "GENERAL" & agrupacion_nombre %in% c("LA LIBERTAD AVANZA", "UNION POR LA PATRIA")) %>%
  group_by(agrupacion_nombre) %>%
  summarise(total_votos = sum(total_votos))

print(datos_general)
-----
  
# Agrupa 
datos_totales <- datos %>%
  group_by(eleccion_tipo, agrupacion_nombre) %>%
  summarise(total_votos = sum(cantidad_votos))

# Filtra 
datos_general <- datos_totales %>%
  filter(eleccion_tipo == "GENERAL" & agrupacion_nombre %in% c("LA LIBERTAD AVANZA", "UNION POR LA PATRIA")) %>%
  group_by(agrupacion_nombre) %>%
  summarise(total_votos = sum(total_votos))

# Imprime los datos antes de crear el gráfico quería verlos
#print(datos_general)


# Gráfico 2
grafico_barras_general <- ggplot(datos_general, aes(x = total_votos, y = agrupacion_nombre, fill = agrupacion_nombre)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::comma(total_votos), group = agrupacion_nombre), 
            position = position_dodge(width = 0.9), 
            hjust = 1.5, 
            size = 3) +  
  labs(title = "Votos entre La libertad avanza y Union por la patria en la GENERAL",
       x = "Cantidad de Votos",
       y = "Agrupación") +
  scale_fill_manual(values = c("LA LIBERTAD AVANZA" = "purple", "UNION POR LA PATRIA" = "lightblue")) +  
  theme_minimal() +
  scale_x_continuous(labels = scales::comma, limits = c(0, 5000000))  

grafico_barras_general
----------------------------------------
#grafico 3
  

#agrupar
datos_totales <- datos %>%
  group_by(eleccion_tipo, agrupacion_nombre) %>%
  summarise(total_votos = sum(cantidad_votos))

datos_segunda_vuelta <- datos_totales %>%
  filter(eleccion_tipo == "SEGUNDA VUELTA" & agrupacion_nombre %in% c("LA LIBERTAD AVANZA", "UNION POR LA PATRIA")) %>%
  group_by(agrupacion_nombre) %>%
  summarise(total_votos = sum(total_votos))

# Gráfico 3 
grafico_barras_segunda_vuelta <- ggplot(datos_segunda_vuelta, aes(x = total_votos, y = agrupacion_nombre, fill = agrupacion_nombre)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::comma(total_votos), group = agrupacion_nombre), 
            position = position_dodge(width = 0.9), 
            hjust = 1.5, 
            size = 3) +  
  labs(title = "Votos entre La libertad avanza y Union por la patria en el Balotaje",
       x = "Cantidad de Votos",
       y = "Agrupación") +
  scale_fill_manual(values = c("LA LIBERTAD AVANZA" = "purple", "UNION POR LA PATRIA" = "lightblue")) +  
  theme_minimal() +
  scale_x_continuous(labels = scales::comma, limits = c(0, 5000000))  

grafico_barras_segunda_vuelta
  
-----------------------------------------
  
#poner en un solo panel los tres gráficos 
graficos_combinados <- grafico_barras_paso / grafico_barras_general / grafico_barras_segunda_vuelta

graficos_combinados
-----------------------------------------
#gráfico 
  

colores_agrupacion <- c("LA LIBERTAD AVANZA" = "purple", 
                          "UNION POR LA PATRIA"= "lightblue")
# Filtra
datos_interactivos_filtrados <- datos_interactivos %>%
  filter(agrupacion_nombre %in% c("LA LIBERTAD AVANZA", "UNION POR LA PATRIA"))

# Ajusta el orden 
datos_interactivos_filtrados$eleccion_tipo <- factor(datos_interactivos_filtrados$eleccion_tipo, levels = c("PASO", "GENERAL", "SEGUNDA VUELTA"))

datos_agrupados <- datos_interactivos_filtrados %>%
  group_by(agrupacion_nombre, eleccion_tipo) %>%
  summarise(total_votos = sum(total_votos))

datos_agrupados$eleccion_tipo <- factor(datos_agrupados$eleccion_tipo, levels = c("PASO", "GENERAL", "SEGUNDA VUELTA"))

# Crea el gráfico interactivo utilizando plotly
grafico_interactivo <- plot_ly(datos_agrupados, 
                               x = ~eleccion_tipo, 
                               y = ~total_votos,  
                               color = ~agrupacion_nombre, 
                               colors = colores_agrupacion[c("LA LIBERTAD AVANZA", "UNION POR LA PATRIA")],
                               type = 'scatter',
                               mode = 'lines+markers',
                               text = ~paste("Agrupación: ", agrupacion_nombre, "<br>Total de votos: ", scales::comma(total_votos)),
                               hoverinfo = "text+name",  # Muestra la información en el tooltip
                               group = ~agrupacion_nombre) %>%
  layout(title = "Evolución de Votos entre La libertad Avanza y Union por la Patria en cada elección",
         xaxis = list(title = "Tipo de Elección"),
         yaxis = list(title = "Total de Votos", range = c(1000000, 5000000)),  # Establece el rango del eje y
         hovermode = "closest",
         showlegend = TRUE)  # Mostrar leyenda

# Visualiza el gráfico interactivo
grafico_interactivo





--------------------------------------------------------------------------------------------------------------
#estos son otros gráficos que no use 
# Verifica los valores únicos en agrupacion_nombre en tus datos
valores_unicos <- unique(datos$agrupacion_nombre)

# Verifica si hay valores únicos que no estén presentes en la escala de colores
valores_faltantes <- setdiff(valores_unicos, names(colores_agrupacion))


colores_agrupacion <- c(colores_agrupacion, setNames(rep("gray", length(valores_faltantes)), valores_faltantes))


datos_totales <- datos %>%
  group_by(eleccion_tipo, agrupacion_nombre) %>%
  summarise(total_votos = sum(cantidad_votos))


grafico_interactivo1 <- plot_ly(datos_totales, 
                               x = ~eleccion_tipo, 
                               y = ~total_votos, 
                               color = ~agrupacion_nombre, 
                               colors = colores_agrupacion,
                               type = 'bar',
                               text = ~paste("Agrupación: ", agrupacion_nombre, "<br>Total de votos: ", scales::comma(total_votos))) %>%
  layout(title = "Total de Votos por Agrupación y Tipo de Elección",
         xaxis = list(title = "Tipo de Elección"),
         yaxis = list(title = "Total de Votos"),
         hovermode = "closest")


grafico_interactivo



------------------------------
# gráfico de torta que verifica que estan bien los datos del balotage 
datos_segunda_vuelta <- datos %>% filter(eleccion_tipo == "SEGUNDA VUELTA")

datos_segunda_vuelta <- datos_segunda_vuelta %>%
  group_by(agrupacion_nombre) %>%
  summarise(total_votos = sum(cantidad_votos)) %>%
  mutate(porcentaje = total_votos / sum(total_votos) * 100)

grafico_torta <- ggplot(datos_segunda_vuelta, aes(x = "", y = porcentaje, fill = agrupacion_nombre)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribución de votos de La libertad Avanza y Union por la Patria  en el Balotage") +
  theme_void() +
  scale_fill_manual(values = colores_agrupacion) +  # Asigna colores por agrupación
  geom_text(aes(label = sprintf("%.1f%%\n%d votos", porcentaje, total_votos)), 
            position = position_stack(vjust = 0.5),
            size = 4,
            color = "black")

print(grafico_torta)