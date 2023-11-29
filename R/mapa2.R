#ABRIR GRÁFICOS DE GEOJSON 
#no me funcionan no se porque 
install.packages("sf")
library(sf)
# Carga las bibliotecas
library(sf)
library(dbplyr)
library(dplyr)
library(ggplot2)
#unir
install.packages("patchwork")
install.packages("leaflet")
# Carga la librería
library(patchwork)
library(leaflet)

install.packages("arrow")
library(arrow)

#abrir los archivos 
setwd("/Users/candelariagonzalez/Desktop/R_Buenos_aires")# Lee el archivo GeoJSON

datos_geojson <- st_read("/Users/candelariagonzalez/Desktop/R_Buenos_aires/circuitos-electorales.geojson")

# Consulta los datos de las elecciones desde el archivo Parquet
datos_elecciones <- read_parquet('Buenos_Aires.parquet')


# Muestra la estructura de los datos de las elecciones
str(datos_elecciones)
str(datos_geojson)

columnas_geojson <- names(datos_geojson)
print(columnas_geojson)

datos_combinados <- merge(datos_geojson, datos_elecciones, by.x = "municipio_nombre", by.y = "seccion_nombre")

print(names(datos_combinados))
str(datos_combinados)
print(summary(datos_combinados))
print(head(datos_combinados))

-------------------------------------
  #datos
  #Encuentra la agrupación que recibió más votos en cada municipio
agrupacion_max_votos <- datos_paso %>%
  group_by(municipio_nombre) %>%
  slice(which.max(cantidad_votos)) %>%
  ungroup()

print(agrupacion_max_votos[, c("municipio_nombre", "agrupacion_nombre", "cantidad_votos")])


# Filtra la información para el municipio de La Matanza
matanza_info <- agrupacion_max_votos %>% filter(municipio_nombre == "La Matanza")

print(matanza_info[, c("municipio_nombre", "agrupacion_nombre", "cantidad_votos")])

# Imprime las primeras filas de ambas tablas para comparar
print(head(datos_combinados[, c("municipio_nombre", "agrupacion_nombre")]))
print(head(agrupacion_max_votos[, c("municipio_nombre", "agrupacion_nombre", "cantidad_votos")]))
----------------

datos_sf <- st_as_sf(datos_combinados)

datos_sf_general <- datos_sf %>% 
  filter(eleccion_tipo == "GENERAL") %>%
  group_by(municipio_nombre, agrupacion_nombre) %>%
  summarise(total_votos = sum(cantidad_votos))

# Crear el gráfico de mapa para el total de votos en cada municipio con intervalos personalizados
ggplot(datos_sf_general) +
  geom_sf(aes(fill = total_votos), color = "white", lwd = 0.1) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", guide = "legend", 
                      name = "Total de Votos", breaks = seq(0, 5000000, by = 100000), labels = scales::comma) +
  labs(title = "Mapa del Total de Votos por Municipio en la general", subtitle = "Elecciones") +
  theme_minimal() +
  theme(legend.position = "bottom") 
---------------
  
# a partir de aca no   
  
  
  
datos_sf <- st_as_sf(datos_combinados)
datos_sf_general <- datos_sf[datos_sf$eleccion_tipo == "GENERAL", ]

# Definir los intervalos 
intervalos <- seq(0, 600000, by = 2000)


ggplot(datos_sf_general) +
  geom_sf(aes(fill = cantidad_votos), color = "white", lwd = 0.1) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", guide = "legend", 
                      name = "Cantidad de Votos", breaks = intervalos) +
  labs(title = "Mapa de Cantidad de Votos (Elección GENERAL)", subtitle = "Elecciones") +
  theme_minimal()
#trate de que funcionaran pero no pude, me encantaria 
  library(sf)
library(ggplot2)
library(dplyr)

# Filtrar los datos para la eleccion_tipo "segunda vuelta"
datos_segunda_vuelta <- datos_combinados[datos_combinados$eleccion_tipo == "SEGUNDA VUELTA", ]

# Calcular el total de votos por municipio y agrupación
votos_por_municipio <- datos_segunda_vuelta %>%
  group_by(municipio_nombre, agrupacion_nombre) %>%
  summarize(total_votos = sum(cantidad_votos))

# Encontrar la agrupación con el mayor total de votos por municipio
max_votos_por_municipio <- votos_por_municipio %>%
  group_by(municipio_nombre) %>%
  slice(which.max(total_votos))

mapa_colores <- datos_segunda_vuelta %>%
  filter(municipio_nombre %in% max_votos_por_municipio$municipio_nombre)


mapa_colores_sf <- st_as_sf(mapa_colores)

# Crear el gráfico de mapa
ggplot(mapa_colores_sf) +
  geom_sf(aes(fill = agrupacion_nombre)) +
  scale_fill_manual(values = c("La Libertad Avanza" = "violet", "Unión por la Patria" = "skyblue")) +
  labs(title = "Resultados de la Segunda Vuelta",
       subtitle = "Coloreado por la agrupación con mayor total de votos",
       caption = "Fuente: Datos electorales") +
  theme_minimal()
-----------------


-------------------------
  library(viridis)
install.packages( "viridis")

# Define los colores de las agrupaciones
colores_agrupacion <- c(
  "LA LIBERTAD AVANZA" = "purple", 
  "FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD" = "red",
  "HACEMOS POR NUESTRO PAIS" = "blue",
  "JUNTOS POR EL CAMBIO" = "yellow",
  "UNION POR LA PATRIA" = "lightblue"
)

# Crea el gráfico de mapa para elecciones PASO con escala continua
mapa_paso <- ggplot() +
  geom_sf(data = datos_combinados_paso, aes(fill = cantidad_votos, color = agrupacion_nombre), size = 0.2) +
  scale_fill_viridis_c(
    name = "Cantidad de Votos",
    labels = scales::comma_format(scale = 1),  # Desactiva la notación científica y agrega comas
    breaks = seq(0, 100000, by = 10000),
    limits = c(0, 100000),
    option = "A",
    direction = -1
  ) +
  scale_color_manual(values = colores_agrupacion) +  # Color de la agrupación
  labs(title = "Agrupación con Mayor Votación por Municipio (PASO)",
       caption = "Fuente: Tus Datos") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_sf(data = st_union(datos_combinados_paso), fill = NA, color = "black")  

# Crea el gráfico de mapa para elecciones generales con escala continua
mapa_general <- ggplot() +
  geom_sf(data = datos_combinados_general, aes(fill = cantidad_votos, color = agrupacion_nombre), size = 0.2) +
  scale_fill_viridis_c(
    name = "Cantidad de Votos",
    labels = scales::comma_format(scale = 1),  # Desactiva la notación científica y agrega comas
    breaks = seq(0, 100000, by = 10000),
    limits = c(0, 100000),
    option = "A",
    direction = -1
  ) +
  scale_color_manual(values = colores_agrupacion) +  
  labs(title = "Agrupación con Mayor Votación por Municipio (General)",
       caption = "Fuente: Tus Datos") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_sf(data = st_union(datos_combinados_general), fill = NA, color = "black") 


mapa_paso +  mapa_general

