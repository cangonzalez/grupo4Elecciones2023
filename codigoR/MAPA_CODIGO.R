#GRÁFICOS CON MAPAS 
#ABRIR GRÁFICOS DE GEOJSON 
install.packages("sf")
library(sf)
# Carga las bibliotecas
library(sf)
library(dbplyr)
library(dplyr)
library(ggplot2)
#unir
install.packages("patchwork")
# Carga la librería
library(patchwork)

# Establece el directorio de trabajo
setwd("/Users/candelariagonzalez/Desktop/R_elecciones")

# Lee el archivo GeoJSON
datos_geojson <- st_read("/Users/candelariagonzalez/Desktop/R_elecciones/circuitos-electorales.geojson")

# Consulta los datos de las elecciones desde el archivo Parquet
datos_elecciones <- read_parquet('Buenos_Aires.parquet')

# Muestra la estructura de los datos de las elecciones
str(datos_elecciones)
str(datos_geojson)

columnas_geojson <- names(datos_geojson)
print(columnas_geojson)

# Combina los datos geoespaciales con los datos de las elecciones
# Combina los datos geoespaciales con los datos de las elecciones
datos_combinados <- merge(datos_geojson, datos_elecciones, by.x = "municipio_nombre", by.y = "seccion_nombre")

print(names(datos_combinados))
str(datos_combinados)

# Crea un gráfico de mapa
ggplot(datos_combinados) +
  geom_sf(aes(fill = cantidad_votos), color = "white", lwd = 0.2) +
  scale_fill_viridis_c() +
  labs(title = "Cantidad de votos por circuito",
       fill = "Cantidad de votos") +
  theme_minimal()




# Define colores para cada agrupación
colores_agrupacion <- c("LA LIBERTAD AVANZA" = "purple", 
                        "FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD" = "red",
                        "HACEMOS POR NUESTRO PAIS" = "blue",
                        "JUNTOS POR EL CAMBIO" = "yellow",
                        "UNION POR LA PATRIA" = "lightblue")

# Filtrar por elección tipo "PASO"
datos_paso <- datos_combinados[datos_combinados$eleccion_tipo == "PASO", ]
# Filtra los datos para la agrupación "La Libertad Avanza" y el tipo de elección "PASO"
datos_filtrados <- subset(datos_combinados, agrupacion_nombre == "LA LIBERTAD AVANZA" & eleccion_tipo == "PASO")

# Crea un objeto sf con los datos filtrados
sf_datos <- st_as_sf(datos_filtrados)

# Crea el gráfico de mapa con leyenda en el lado derecho y sin notación científica
ggplot(sf_datos) +
  geom_sf(aes(fill = cantidad_votos), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    name = "Cantidad de Votos",
    limits = c(0, 100000),
    breaks = seq(0, 100000, by = 10000),
    labels = scales::comma_format(scale = 1)  # Desactiva la notación científica
  ) +
  labs(title = "Mapa de La Libertad Avanza en Elecciones PASO",
       caption = "Fuente: Tus Datos") +
  theme_minimal() +
  theme(legend.position = "right",  # Posiciona la leyenda en el lado derecho
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12))


-------------------------------------------
  # Instala la librería si aún no la tienes instalada
  # install.packages("dplyr")
  
  # Instala la librería si aún no la tienes instalada
  # install.packages("dplyr")
  
# Carga la librería  library(dplyr)

  # Instala la librería si aún no la tienes instalada
  # install.packages("dplyr")
  
# Carga la librería
library(dplyr)

# Define los colores de las agrupaciones
colores_agrupacion <- c(
  "LA LIBERTAD AVANZA" = "purple", 
  "FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD" = "red",
  "HACEMOS POR NUESTRO PAIS" = "blue",
  "JUNTOS POR EL CAMBIO" = "yellow",
  "UNION POR LA PATRIA" = "lightblue"
)

# Filtra por el tipo de elección "PASO"
datos_combinados_paso <- datos_combinados %>%
  filter(eleccion_tipo == "PASO")

# Encuentra la agrupación que más votó en cada municipio
agrupacion_max_votos <- datos_combinados_paso %>%
  group_by(municipio_nombre) %>%
  filter(cantidad_votos == max(cantidad_votos)) %>%
  ungroup()

# Crea el gráfico de mapa
ggplot() +
  geom_sf(data = datos_combinados_paso, aes(fill = agrupacion_nombre), color = "white", size = 0.2) +
  scale_fill_manual(values = colores_agrupacion) +
  labs(title = "Agrupación con Mayor Votación por Municipio (PASO)",
       caption = "Fuente: Tus Datos") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_sf(data = st_union(datos_combinados_paso), fill = NA, color = "black")  # Límite del mapa completo


-----------------------------------------
  # Instala la librería si aún no la tienes instalada
  # install.packages("dplyr")
  
# Carga la librería
library(dplyr)

# Define los colores de las agrupaciones
colores_agrupacion <- c(
  "LA LIBERTAD AVANZA" = "purple", 
  "FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD" = "red",
  "HACEMOS POR NUESTRO PAIS" = "blue",
  "JUNTOS POR EL CAMBIO" = "yellow",
  "UNION POR LA PATRIA" = "lightblue"
)

--------------------------------


# Crea el gráfico de mapa para elecciones PASO
mapa_paso <- ggplot() +
  geom_sf(data = datos_combinados_paso, aes(fill = agrupacion_nombre), color = "white", size = 0.2) +
  scale_fill_manual(values = colores_agrupacion) +
  labs(title = "Agrupación con Mayor Votación por Municipio (PASO)",
       caption = "Fuente: Tus Datos") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_sf(data = st_union(datos_combinados_paso), fill = NA, color = "black")  # Límite del mapa completo

# Crea el gráfico de mapa para elecciones generales
mapa_general <- ggplot() +
  geom_sf(data = datos_combinados_general, aes(fill = agrupacion_nombre), color = "white", size = 0.2) +
  scale_fill_manual(values = colores_agrupacion) +
  labs(title = "Agrupación con Mayor Votación por Municipio (General)",
       caption = "Fuente: Tus Datos") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_sf(data = st_union(datos_combinados_general), fill = NA, color = "black")  # Límite del mapa completo

# Combina los mapas
mapa_paso + mapa_general


--------------------------------
#MAPA SEGUNDA VUELTA 

# Define los colores de las agrupaciones
colores_agrupacion <- c(
  "LA LIBERTAD AVANZA" = "purple", 
  "UNION POR LA PATRIA" = "lightblue"
)

# Filtra por el tipo de elección "SEGUNDA VUELTA" y las agrupaciones específicas
datos_combinados_segunda_vuelta <- datos_combinados %>%
  filter(eleccion_tipo == "SEGUNDA VUELTA" & agrupacion_nombre %in% c("LA LIBERTAD AVANZA", "UNION POR LA PATRIA"))

# Encuentra la agrupación que más votó en cada municipio
agrupacion_max_votos_segunda_vuelta <- datos_combinados_segunda_vuelta %>%
  group_by(municipio_nombre) %>%
  filter(cantidad_votos == max(cantidad_votos)) %>%
  ungroup()

# Crea el gráfico de mapa para la segunda vuelta
ggplot() +
  geom_sf(data = datos_combinados_segunda_vuelta, aes(fill = agrupacion_nombre), color = "white", size = 0.2) +
  scale_fill_manual(values = colores_agrupacion) +
  labs(title = "Agrupación con Mayor Votación por Municipio (Segunda Vuelta)",
       caption = "Fuente: Tus Datos") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_sf(data = st_union(datos_combinados_segunda_vuelta), fill = NA, color = "black")  # Límite del mapa completo

---------------------------------------------------------------
  # Carga la librería
library(patchwork)

# Crea el gráfico de mapa para elecciones PASO
mapa_paso <- ggplot() +
  geom_sf(data = datos_combinados_paso, aes(fill = agrupacion_nombre), color = "white", size = 0.2) +
  scale_fill_manual(values = colores_agrupacion) +
  labs(title = "Agrupación con Mayor Votación por Municipio (PASO)",
       caption = "Fuente: Tus Datos") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_sf(data = st_union(datos_combinados_paso), fill = NA, color = "black")  # Límite del mapa completo

# Crea el gráfico de mapa para elecciones generales
mapa_general <- ggplot() +
  geom_sf(data = datos_combinados_general, aes(fill = agrupacion_nombre), color = "white", size = 0.2) +
  scale_fill_manual(values = colores_agrupacion) +
  labs(title = "Agrupación con Mayor Votación por Municipio (General)",
       caption = "Fuente: Tus Datos") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_sf(data = st_union(datos_combinados_general), fill = NA, color = "black")  # Límite del mapa completo

# Combina los mapas
mapa_paso + mapa_general


---------------------------------------------------------------
  # Filtra los datos para la agrupación "Frente de Izquierda y de Trabajadores - Unidad" y el tipo de elección "PASO"
datos_filtrados <- subset(datos_combinados, agrupacion_nombre == "FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD" & eleccion_tipo == "PASO")

# Crea un objeto sf con los datos filtrados
sf_datos <- st_as_sf(datos_filtrados)

# Crea el gráfico de mapa con leyenda en el lado derecho y sin notación científica
ggplot(sf_datos) +
  geom_sf(aes(fill = cantidad_votos), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    name = "Cantidad de Votos",
    limits = c(0, 100000),
    breaks = seq(0, 100000, by = 10000),
    labels = scales::comma_format(scale = 1)  # Desactiva la notación científica
  ) +
  labs(title = "Mapa de Frente de Izquierda y de Trabajadores - Unidad en Elecciones PASO",
       caption = "Fuente: Tus Datos") +
  theme_minimal() +
  theme(legend.position = "right",  # Posiciona la leyenda en el lado derecho
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12))

  
  
---------------------------------------------------------------
  # Crea el gráfico de mapa para elecciones PASO con escala continua
  # Crea el gráfico de mapa para elecciones PASO con escala continua
mapa_paso <- ggplot() +
  geom_sf(data = datos_combinados_paso, aes(fill = cantidad_votos), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    name = "Cantidad de Votos",
    labels = scales::comma_format(scale = 1),  # Desactiva la notación científica y agrega comas
    breaks = seq(0, 100000, by = 10000),
    limits = c(0, 100000),
    option = "A",
    direction = -1
  ) +
  labs(title = "Agrupación con Mayor Votación por Municipio (PASO)",
       caption = "Fuente: Tus Datos") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_sf(data = st_union(datos_combinados_paso), fill = NA, color = "black")  # Límite del mapa completo

# Crea el gráfico de mapa para elecciones generales con escala continua
mapa_general <- ggplot() +
  geom_sf(data = datos_combinados_general, aes(fill = cantidad_votos), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    name = "Cantidad de Votos",
    labels = scales::comma_format(scale = 1),  # Desactiva la notación científica y agrega comas
    breaks = seq(0, 100000, by = 10000),
    limits = c(0, 100000),
    option = "A",
    direction = -1
  ) +
  labs(title = "Agrupación con Mayor Votación por Municipio (General)",
       caption = "Fuente: Tus Datos") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_sf(data = st_union(datos_combinados_general), fill = NA, color = "black")  # Límite del mapa completo

# Combina los mapas
mapa_paso + mapa_general
---------------------------------------------------------------  
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
  geom_sf(data = st_union(datos_combinados_paso), fill = NA, color = "black")  # Límite del mapa completo

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
  scale_color_manual(values = colores_agrupacion) +  # Color de la agrupación
  labs(title = "Agrupación con Mayor Votación por Municipio (General)",
       caption = "Fuente: Tus Datos") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_sf(data = st_union(datos_combinados_general), fill = NA, color = "black")  # Límite del mapa completo

# Combina los mapas
mapa_paso +  mapa_general
  
  
---------------------------------------------------------------
#no funciona este código 


  
  # Carga las librerías
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(patchwork)


# Define los colores de las agrupaciones
colores_agrupacion <- c(
  "LA LIBERTAD AVANZA" = "purple", 
  "FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD" = "red",
  "HACEMOS POR NUESTRO PAIS" = "blue",
  "JUNTOS POR EL CAMBIO" = "yellow",
  "UNION POR LA PATRIA" = "lightblue"
)
# Crear un objeto sf con los datos filtrados para elecciones PASO
sf_datos_paso <- st_as_sf(datos_combinados %>% filter(eleccion_tipo == "PASO"))

# Crear un objeto sf con los datos filtrados para elecciones generales
sf_datos_general <- st_as_sf(datos_combinados %>% filter(eleccion_tipo == "general"))

# Encuentra la agrupación que más votó en cada municipio para elecciones PASO
agrupacion_max_votos_paso <- sf_datos_paso %>%
  group_by(municipio_nombre) %>%
  filter(cantidad_votos == max(cantidad_votos)) %>%
  ungroup()

# Encuentra la agrupación que más votó en cada municipio para elecciones generales
agrupacion_max_votos_general <- sf_datos_general %>%
  group_by(municipio_nombre) %>%
  filter(cantidad_votos == max(cantidad_votos)) %>%
  ungroup()

# Crea el gráfico de mapa para elecciones PASO
mapa_paso <- ggplot(sf_datos_paso) +
  geom_sf(aes(fill = agrupacion_nombre), color = "white", size = 0.2) +
  scale_fill_manual(values = colores_agrupacion) +
  labs(title = "Agrupación con Mayor Votación por Municipio (PASO)",
       caption = "Fuente: Tus Datos") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_sf(data = st_union(sf_datos_paso), fill = NA, color = "black")  +  # Límite del mapa completo
  coord_sf(default_crs = NULL)  # Ajuste aquí

# Crea el gráfico de mapa para elecciones generales
mapa_general <- ggplot(sf_datos_general) +
  geom_sf(aes(fill = agrupacion_nombre), color = "white", size = 0.2) +
  scale_fill_manual(values = colores_agrupacion) +
  labs(title = "Agrupación con Mayor Votación por Municipio (General)",
       caption = "Fuente: Tus Datos") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_sf(data = st_union(sf_datos_general), fill = NA, color = "black") +  # Límite del mapa completo
  coord_sf(default_crs = NULL)  # Ajuste aquí

# Personaliza el diseño y la apariencia
mapa_paso + mapa_general