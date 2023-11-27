#TENES QUE CORRER TODO!!!!!!



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

datos <- dbGetQuery(con, "SELECT 1+1")
datos

#establecer ruta de trabajo 

setwd("/Users/candelariagonzalez/Desktop/R_elecciones")
datos <- dbGetQuery(con, "SELECT count(*) FROM read_parquet('Buenos_Aires.parquet');")
datos

#cargo los datos 
# Cargar datos desde un archivo Parquet usando DuckDB
datos <- dbGetQuery(con, "SELECT * FROM read_parquet('Buenos_Aires.parquet');")
datos

print(names(datos))
#verifico estrutura
str(datos)




# Ajustar el tamaño global de los gráficos
options(repr.plot.width = 20, repr.plot.height = 12)

# Filtrar solo por eleccion_tipo = "PASO"
datos_paso <- datos[datos$eleccion_tipo == "PASO", ]

# Sumar la cantidad de votos por sección provincial, agrupación y tipo de elección
datos_sumados <- datos_paso %>%
  group_by(seccionprovincial_nombre, agrupacion_nombre, eleccion_tipo) %>%
  summarise(total_votos = sum(cantidad_votos))


# Crear el gráfico con paneles separados por agrupación y colores personalizados
p_paso <- ggplot(data = datos_sumados, aes(x = seccionprovincial_nombre, y = total_votos, fill = agrupacion_nombre)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::comma(total_votos)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2, color = "black") +
  labs(title = "Cantidad de Votos por Sección Provincial y Agrupación (Solo PASO)",
       x = "Sección Provincial",
       y = "Cantidad de Votos",
       fill = "") +  # Establecer el título de la leyenda de fill como una cadena vacía
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(vjust = 1.5),  # Ajusta la posición vertical del título de la leyenda
        legend.text = element_text(size = 8),
        strip.text.y = element_blank()) +  # Oculta las etiquetas del eje y en los paneles
  facet_grid(agrupacion_nombre ~ ., scales = "free_y") +  # Filtra por agrupación y muestra en filas
  scale_y_continuous(labels = scales::comma, limits = c(0, 5000000), 
                     breaks = seq(0, 5000000, by = 1000000)) +
  scale_fill_manual(values = colores_agrupacion)

print(p_paso)



# Filtrar solo por eleccion_tipo = "GENERAL"
datos_general <- datos[datos$eleccion_tipo == "GENERAL", ]

# Sumar la cantidad de votos por sección provincial, agrupación y tipo de elección
datos_sumados_general <- datos_general %>%
  group_by(seccionprovincial_nombre, agrupacion_nombre, eleccion_tipo) %>%
  summarise(total_votos = sum(cantidad_votos))


# Crear el gráfico con paneles separados por agrupación y colores personalizados para "GENERAL"
p_general <- ggplot(data = datos_sumados_general, aes(x = seccionprovincial_nombre, y = total_votos, fill = agrupacion_nombre)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::comma(total_votos)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5, color = "black") +
  labs(title = "Cantidad de Votos por Sección Provincial y Agrupación (Solo GENERAL)",
       x = "Sección Provincial",
       y = "Cantidad de Votos",
       fill = "") +  # Establecer el título de la leyenda de fill como una cadena vacía
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(vjust = 1.5),  # Ajusta la posición vertical del título de la leyenda
        legend.text = element_text(size = 8),
        strip.text.y = element_blank()) +  # Oculta las etiquetas del eje y en los paneles
  facet_grid(agrupacion_nombre ~ ., scales = "free_y") +  # Filtra por agrupación y muestra en filas
  scale_y_continuous(labels = scales::comma, limits = c(0, 5000000), 
                     breaks = seq(0, 5000000, by = 1000000)) +
  scale_fill_manual(values = colores_agrupacion)

print(p_general)



# Filtrar solo por eleccion_tipo = "SEGUNDA VUELTA"
datos_segunda_vuelta <- datos[datos$eleccion_tipo == "SEGUNDA VUELTA", ]

# Sumar la cantidad de votos por sección provincial, agrupación y tipo de elección (SEGUNDA VUELTA)
datos_sumados_segunda_vuelta <- datos_segunda_vuelta %>%
  group_by(seccionprovincial_nombre, agrupacion_nombre, eleccion_tipo) %>%
  summarise(total_votos = sum(cantidad_votos))

# Crear el gráfico con paneles separados por agrupación y colores personalizados para "SEGUNDA VUELTA"
p_segunda_vuelta <- ggplot(data = datos_sumados_segunda_vuelta, aes(x = seccionprovincial_nombre, y = total_votos, fill = agrupacion_nombre)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::comma(total_votos)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5, color = "black") +
  labs(title = "Cantidad de Votos por Sección Provincial y Agrupación (Solo SEGUNDA VUELTA)",
       x = "Sección Provincial",
       y = "Cantidad de Votos",
       fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(vjust = 1.5),
        legend.text = element_text(size = 8),
        strip.text.y = element_blank()) +
  facet_grid(agrupacion_nombre ~ ., scales = "free_y") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 5000000), 
                     breaks = seq(0, 5000000, by = 1000000)) +
  scale_fill_manual(values = colores_agrupacion)

print(p_segunda_vuelta)

----------------------
install.packages("gridExtra") 
library(gridExtra)
#unir los tres gráficos en una misma ventana pero se ve mal 
grid.arrange(p_paso, p_general, p_segunda_vuelta, ncol = 3)

--------------------------------------------------
  # Combina los datos de las tres elecciones en un solo conjunto de datos
datos_combinados <- rbind(
    mutate(datos_paso, tipo_eleccion = "PASO"),
    mutate(datos_general, tipo_eleccion = "GENERAL"),
    mutate(datos_segunda_vuelta, tipo_eleccion = "SEGUNDA VUELTA")
  )

# Sumar la cantidad de votos por sección provincial, agrupación y tipo de elección
datos_combinados_sumados <- datos_combinados %>%
  group_by(seccionprovincial_nombre, agrupacion_nombre, tipo_eleccion) %>%
  summarise(total_votos = sum(cantidad_votos))

# Calcular los porcentajes
datos_combinados_sumados <- datos_combinados_sumados %>%
  group_by(seccionprovincial_nombre, tipo_eleccion) %>%
  mutate(porcentaje = total_votos / sum(total_votos) * 100)

# Ajustar el tamaño global de los gráficos
options(repr.plot.width = 30, repr.plot.height = 6)

# Crear el gráfico combinado con porcentajes dentro de las barras y orientación horizontal
p_combinado <- ggplot(data = datos_combinados_sumados, aes(x = porcentaje, y = seccionprovincial_nombre, fill = agrupacion_nombre)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(porcentaje/100, accuracy = 0.1)),
            position = position_stack(vjust = 0.5), size = 2.5, color = "black") +
  labs(title = "Porcentaje de Votos por Sección Provincial, Agrupación y Tipo de Elección",
       x = "Porcentaje de Votos",
       y = "Sección Provincial",
       fill = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1),  # Ajusta el texto del eje y a horizontal
        legend.title = element_text(vjust = 1.5),
        legend.text = element_text(size = 6),  # Ajustar el tamaño de la letra de la leyenda
        strip.text = element_blank(),  # Oculta las etiquetas del eje y en los paneles
        legend.position = "bottom",  # Coloca la leyenda debajo del gráfico
        legend.box = "horizontal") +  # Muestra la leyenda en una caja horizontal
  facet_wrap(~ tipo_eleccion, scales = "free_y") +
  scale_x_continuous(labels = scales::percent_format(scale = 1, accuracy = 5), limits = c(0, 100)) +  # Ajusta el eje x a porcentajes con incrementos del 5%
  scale_fill_manual(values = colores_agrupacion)

print(p_combinado)


---------------
  
  
#ACA EMPIEZAN LOS GRÄFICOS 
#parte dos solo se va a ver el el de las paso y generales 
  # Definir colores para cada agrupación
colores_agrupacion <- c("LA LIBERTAD AVANZA" = "purple", 
                          "FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD" = "red",
                          "HACEMOS POR NUESTRO PAIS" = "blue",
                          "JUNTOS POR EL CAMBIO"= "yellow",
                          "UNION POR LA PATRIA"= "lightblue")  
  
  # Combina los datos de las elecciones "GENERAL" y "PASO"
datos_combinados <- rbind(
    mutate(datos_paso, tipo_eleccion = "PASO"),
    mutate(datos_general, tipo_eleccion = "GENERAL")
  )

#Sumar la cantidad de votos por sección provincial, agrupación y tipo de elección
datos_combinados_sumados <- datos_combinados %>%
  group_by(seccionprovincial_nombre, agrupacion_nombre, tipo_eleccion) %>%
  summarise(total_votos = sum(cantidad_votos))

# Calcular los porcentajes
datos_combinados_sumados <- datos_combinados_sumados %>%
  group_by(seccionprovincial_nombre, tipo_eleccion) %>%
  mutate(porcentaje = total_votos / sum(total_votos) * 100)

# Ajustar el tamaño global de los gráficos
options(repr.plot.width = 30, repr.plot.height = 8)

# Crear el gráfico combinado con porcentajes dentro de las barras y orientación horizontal
p_combinado <- ggplot(data = datos_combinados_sumados, aes(x = porcentaje, y = seccionprovincial_nombre, fill = agrupacion_nombre)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(porcentaje/100, accuracy = 0.1)),
            position = position_stack(vjust = 0.5), size = 2.5, color = "black") +
  labs(title = "Porcentaje de Votos por Sección Provincial, Agrupación y Tipo de Elección",
       x = "PASO                              Porcentaje de Votos                            GENERAL",
       y = "Sección Provincial",
       fill = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1),  # Ajusta el texto del eje y a horizontal
        legend.title = element_text(vjust = 1.5),
        legend.text = element_text(size = 6),  # Ajustar el tamaño de la letra de la leyenda
        strip.text = element_blank(),  # Oculta las etiquetas del eje y en los paneles
        legend.position = "bottom",  # Coloca la leyenda debajo del gráfico
        legend.box = "horizontal") +  # Muestra la leyenda en una caja horizontal
  facet_wrap(~ tipo_eleccion, scales = "free_y") +
  scale_x_continuous(labels = scales::percent_format(scale = 1, accuracy = 5), limits = c(0, 100)) +  # Ajusta el eje x a porcentajes con incrementos del 5%
  scale_fill_manual(values = colores_agrupacion)

print(p_combinado)



-------------------------------------
  # Filtrar por eleccion_tipo = "PASO" y agrupaciones de interés
datos_filtrados_paso <- datos %>%
  filter(eleccion_tipo == "PASO" & agrupacion_nombre %in% c("LA LIBERTAD AVANZA", "UNION POR LA PATRIA"))

# Ajustar el tamaño global del gráfico
options(repr.plot.width = 10, repr.plot.height = 8)

# Sumar la cantidad de votos por agrupación y tipo de elección
datos_sumados_paso <- datos_filtrados_paso %>%
  group_by(agrupacion_nombre, eleccion_tipo) %>%
  summarise(total_votos = sum(cantidad_votos))

# Calcular porcentajes
datos_sumados_paso$porcentaje <- datos_sumados_paso$total_votos / sum(datos_sumados_paso$total_votos) * 100

# Crear gráfico de torta
paso_plot1 <- ggplot(datos_sumados_paso, aes(x = "", y = total_votos, fill = agrupacion_nombre)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), position = position_stack(vjust = 0.5)) +
  coord_polar("y") +
  ggtitle("Votos para PASO por Agrupación") +
  scale_fill_manual(values = colores_agrupacion) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(0, 4), "cm"))

# Mostrar el gráfico
print(paso_plot1)
-------------------------------
  # Filtrar por eleccion_tipo = "GENERAL" y agrupaciones de interés
datos_filtrados_general <- datos %>%
  filter(eleccion_tipo == "GENERAL" & agrupacion_nombre %in% c("LA LIBERTAD AVANZA", "UNION POR LA PATRIA"))

# Ajustar el tamaño global del gráfico
options(repr.plot.width = 10, repr.plot.height = 8)

# Sumar la cantidad de votos por agrupación y tipo de elección
datos_sumados_general <- datos_filtrados_general %>%
  group_by(agrupacion_nombre, eleccion_tipo) %>%
  summarise(total_votos = sum(cantidad_votos))

# Calcular porcentajes
datos_sumados_general$porcentaje <- datos_sumados_general$total_votos / sum(datos_sumados_general$total_votos) * 100

# Crear gráfico de torta
general_plot1 <- ggplot(datos_sumados_general, aes(x = "", y = total_votos, fill = agrupacion_nombre)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), position = position_stack(vjust = 0.5)) +
  coord_polar("y") +
  ggtitle("Votos para ELECCIÓN GENERAL por Agrupación") +
  scale_fill_manual(values = colores_agrupacion) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(0, 4), "cm"))

# Mostrar el gráfico
print(general_plot1)
------------------------
  # Filtrar por eleccion_tipo = "BALOTAGE" sin especificar agrupaciones de interés
  
datos_filtrados_balotage <- datos %>%
  filter(eleccion_tipo == "SEGUNDA VUELTA")

# Ajustar el tamaño global del gráfico
options(repr.plot.width = 8, repr.plot.height = 8)

# Seleccionar las agrupaciones de interés
agrupaciones_interes <- c("LA LIBERTAD AVANZA", "UNION POR LA PATRIA")

# Filtrar datos solo para las agrupaciones de interés
datos_filtrados_balotage <- datos_filtrados_balotage %>%
  filter(agrupacion_nombre %in% agrupaciones_interes)

# Sumar la cantidad de votos por agrupación
datos_sumados_balotage <- datos_filtrados_balotage %>%
  group_by(agrupacion_nombre) %>%
  summarise(total_votos = sum(cantidad_votos))

# Calcular porcentajes
datos_sumados_balotage$porcentaje <- datos_sumados_balotage$total_votos / sum(datos_sumados_balotage$total_votos) * 100

# Definir colores específicos para las agrupaciones de interés
colores_agrupacion <- c("LA LIBERTAD AVANZA" = "purple", 
                        "UNION POR LA PATRIA" = "lightblue")

# Create the plot
balotage_pie_plot1 <- ggplot(datos_sumados_balotage, aes(x = "", y = total_votos, fill = agrupacion_nombre)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), position = position_stack(vjust = 0.5)) +
  coord_polar("y") +
  ggtitle("Votos para BALOTAGE por Agrupación") +
  scale_fill_manual(values = colores_agrupacion) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(0, 4), "cm"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "bottom") +  # Set legend position to bottom
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))  # Arrange legend items vertically

# Show the plot
print(balotage_pie_plot1)

------------------------------
#PONER LOS TRES EN UN SOLO PANEL 
# Combinar los tres gráficos en una sola fila con tres paneles
grid.arrange(paso_plot1, general_plot1, balotage_pie_plot1, ncol = 3)

--------------------------------
  # Filtrar por eleccion_tipo = "PASO" sin especificar agrupaciones de interés
  # Definir colores para cada agrupación
colores_agrupacion <- c("LA LIBERTAD AVANZA" = "purple", 
                          "FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD" = "red",
                          "HACEMOS POR NUESTRO PAIS" = "blue",
                          "JUNTOS POR EL CAMBIO"= "yellow",
                          "UNION POR LA PATRIA"= "lightblue")
datos_filtrados_paso <- datos %>%
  filter(eleccion_tipo == "PASO")

# Ajustar el tamaño global del gráfico
options(repr.plot.width = 8, repr.plot.height = 8)

# Sumar la cantidad de votos por agrupación y tipo de elección
datos_sumados_paso <- datos_filtrados_paso %>%
  group_by(agrupacion_nombre, eleccion_tipo) %>%
  summarise(total_votos = sum(cantidad_votos))

# Calcular porcentajes
datos_sumados_paso$porcentaje <- datos_sumados_paso$total_votos / sum(datos_sumados_paso$total_votos) * 100

# Create the plot
paso_plot <- ggplot(datos_sumados_paso, aes(x = "", y = total_votos, fill = agrupacion_nombre)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), position = position_stack(vjust = 0.5), size=3) +
  coord_polar("y") +
  ggtitle("Votos para PASO por Agrupación") +
  scale_fill_manual(values = colores_agrupacion) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(0, 4), "cm"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "bottom") +  # Set legend position to bottom
  guides(fill = guide_legend(nrow = length(unique(datos_sumados_paso$agrupacion_nombre))))  # Stack legend items vertically

# Show the plot
print(paso_plot)
-------------------------------
  # Definir colores para cada agrupación
colores_agrupacion <- c("LA LIBERTAD AVANZA" = "purple", 
                          "FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD" = "red",
                          "HACEMOS POR NUESTRO PAIS" = "blue",
                          "JUNTOS POR EL CAMBIO"= "yellow",
                          "UNION POR LA PATRIA"= "lightblue")

# Filtrar por eleccion_tipo = "GENERAL" sin especificar agrupaciones de interés
datos_filtrados_general <- datos %>%
  filter(eleccion_tipo == "GENERAL")

# Ajustar el tamaño global del gráfico
options(repr.plot.width = 8, repr.plot.height = 8)

# Sumar la cantidad de votos por agrupación y tipo de elección
datos_sumados_general <- datos_filtrados_general %>%
  group_by(agrupacion_nombre, eleccion_tipo) %>%
  summarise(total_votos = sum(cantidad_votos))

# Calcular porcentajes
datos_sumados_general$porcentaje <- datos_sumados_general$total_votos / sum(datos_sumados_general$total_votos) * 100

# Create the plot
general_plot <- ggplot(datos_sumados_general, aes(x = "", y = total_votos, fill = agrupacion_nombre)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3) +  # Set the text size to 3 (you can adjust as needed)
  coord_polar("y") +
  ggtitle("Votos para ELECCIÓN GENERAL por Agrupación") +
  scale_fill_manual(values = colores_agrupacion) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(0, 4), "cm"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "bottom") +  # Set legend position to bottom
  guides(fill = guide_legend(nrow = length(unique(datos_sumados_general$agrupacion_nombre))))  # Stack legend items vertically

# Show the plot
print(general_plot)


-------------------------------
# Arrange the plots with equal widths
grid.arrange(paso_plot, general_plot, balotage_pie_plot1, ncol = 3, widths = c(1, 1, 1))

--------------------------------
#tratar de hacer un gráfico interactivo
install.packages("plotly")
library(plotly)

# Filtrar por eleccion_tipo = "PASO"
datos_filtrados_paso <- datos %>%
  filter(eleccion_tipo == "PASO" & agrupacion_nombre %in% c("LA LIBERTAD AVANZA", "UNION POR LA PATRIA"))

# Sumar la cantidad de votos por agrupación y tipo de elección
datos_sumados_paso <- datos_filtrados_paso %>%
  group_by(agrupacion_nombre, eleccion_tipo) %>%
  summarise(total_votos = sum(cantidad_votos))

# Calcular porcentajes
datos_sumados_paso$porcentaje <- datos_sumados_paso$total_votos / sum(datos_sumados_paso$total_votos) * 100

# Crear gráfico interactivo
grafico_interactivo <- plot_ly(
  data = datos_sumados_paso,
  labels = ~agrupacion_nombre,
  values = ~total_votos,
  type = "pie",
  text = ~paste(agrupacion_nombre, round(porcentaje, 1), "%"),
  hoverinfo = "text"
) %>%
  layout(
    title = "Votos para PASO por Agrupación",
    showlegend = TRUE
  )

# Mostrar el gráfico interactivo
grafico_interactivo

--------------------------
  
#seguir viendo porque no me gusta como funciona porque no se quedna bien los colores 
  
install.packages("shiny")
library(shiny)

library(scales)


# Crear la aplicación Shiny
ui <- fluidPage(
  titlePanel("Gráfico de Torta Interactivo"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tipo_eleccion", "Tipo de Elección", choices = unique(datos$eleccion_tipo)),
      selectInput("agrupaciones", "Agrupaciones", multiple = TRUE, choices = unique(datos$agrupacion_nombre))
    ),
    mainPanel(
      plotlyOutput("grafico_interactivo")
    )
  )
)

server <- function(input, output) {
  # Filtrar y resumir datos según las selecciones del usuario
  datos_filtrados <- reactive({
    datos %>%
      filter(eleccion_tipo == input$tipo_eleccion & agrupacion_nombre %in% input$agrupaciones) %>%
      group_by(agrupacion_nombre) %>%
      summarise(total_votos = sum(cantidad_votos))
  })
  
  # Crear el gráfico interactivo
  output$grafico_interactivo <- renderPlotly({
    datos_resumidos <- datos_filtrados()
    
    # Calcular porcentajes
    datos_resumidos$porcentaje <- datos_resumidos$total_votos / sum(datos_resumidos$total_votos)
    
    plot_ly(
      data = datos_resumidos,
      labels = ~agrupacion_nombre,
      values = ~total_votos,
      type = "pie",
      text = ~format(percent(porcentaje, accuracy = 0.1), nsmall = 1),
      hoverinfo = "text",
      marker = list(colors = colores_agrupacion)
    ) %>%
      layout(
        title = paste("Votos para", input$tipo_eleccion, "por Agrupación"),
        showlegend = TRUE
      )
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
--------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------
