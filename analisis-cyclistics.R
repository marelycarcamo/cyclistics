#-------------------------------------------------------------------------------------------------------------------
#   Proceso de Análisis y Visualización de Datos Cyclistics
#---------------------------------------------------------------------------------------------------------------------
# CArgar librerías necesarias

library(ggplot2)

install.packages('scales')
library(scales)


# Importar datos limpios

data_clean <- read_csv(file.path(ruta_datos, '2024-tripdata-clean.csv'))
glimpse(data_clean)
head(data_clean)


#  Análisis descriptivo de tiempo_recorrido (todas las cifras en segundos)
summary(data_clean$tiempo_recorrido)


# Análisis comparativo entre clientes y suscriptores
aggregate(data_clean$tiempo_recorrido ~ data_clean$tipo_cliente, FUN = mean)
aggregate(data_clean$tiempo_recorrido ~ data_clean$tipo_cliente, FUN = median)
aggregate(data_clean$tiempo_recorrido ~ data_clean$tipo_cliente, FUN = max)
aggregate(data_clean$tiempo_recorrido ~ data_clean$tipo_cliente, FUN = min)


# Definir dias de la semana y meses del año
data_clean$mes <- ordered(data_clean$mes, levels=c('enero','febrero','marzo','abril','mayo','junio','julio','agosto','septiembre','octubre','noviembre','diciembre'))
data_clean$dia_semana <- ordered(data_clean$dia_semana, levels=c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado","domingo"))


# Días de la semana ordenados (lunes a domingo)
dias_semana_ordenados <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")

# Meses del año ordenados (enero a diciembre)
meses_ordenados <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
                     "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")


# Definir una paleta de colores específica para los tipos de clientes
colores_clientes <- c("suscriptor" = "#1b9e77", "cliente" = "#5F02D9")






# Ver el tiempo promedio de viaje por día para clientes frente a suscriptor
aggregate(data_clean$tiempo_recorrido ~ data_clean$tipo_cliente + data_clean$dia_semana, FUN = mean)


#-Visualizaciones----------------------------------------------------------------------------------------------------------------------------------



#---------------------------------------------------------------------------------------------------------------------------------------------------------
# Visualizaciones de Analisis por tipo de usuario en el MES


# Verificar los valores únicos en la columna 'tipo_cliente'
unique(data_clean$tipo_cliente)

# Resumir datos por mes
data_resumen_mes <- data_clean %>%
  mutate(mes = factor(mes, levels = meses_ordenados)) %>%
  group_by(tipo_cliente, mes) %>%
  summarise(
    cantidad_recorridos = n(),
    tiempo_promedio = mean(tiempo_recorrido, na.rm = TRUE),
    .groups = "drop"
  )

# Visualización: Gráfico de área por tendencias mensuales de recorridos
  ggplot(data_resumen_mes,aes(x = mes, y = cantidad_recorridos, group = tipo_cliente, fill = tipo_cliente)) +
  geom_area(position = "stack") +
  scale_fill_manual(values = colores_clientes) + # Aplicar la paleta de colores específica
  scale_y_continuous(labels = comma) + # Cambia el formato del eje Y
  labs(title = "Tendencias Mensuales de Recorridos por Tipo de Cliente",
       subtitle = "Cyclistics, Chicago 2024", # Subtítulo
       x = "Meses",
       y = "Cantidad de Recorridos") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Alinea el título a la derecha
    plot.subtitle = element_text(hjust = 0.5), # Alinea el subtítulo a la derecha
    axis.text.x = element_text(margin = margin(t = 30, b=5)), # Añadir margen superior a las etiquetas del eje X 
    axis.text.y = element_text(margin = margin(r = 30, l=5)), # Añadir margen derecho a las etiquetas del eje Y
    plot.margin = margin(t = 20, b = 20, l = 20, r = 20) # Añadir margen superior para subir el título y subtítulo
  ) +
  annotate("text", x = Inf, y = -Inf, label = "Fuente: Cyclistic", hjust = 1.1, vjust = -0.5, size = 3)


  # Visualización: Gráfico de área tiempo promedio recorrido
  ggplot(data_resumen_mes,aes(x = mes, y = tiempo_promedio, group = tipo_cliente, fill = tipo_cliente)) +
    geom_area(position = "stack") +
    scale_fill_manual(values = colores_clientes) + # Aplicar la paleta de colores específica
    scale_y_continuous(labels = comma) + # Cambia el formato del eje Y
    labs(title = "Tiempo Promedio Mensual Recorrido por Tipo de Cliente",
         subtitle = "Cyclistics, Chicago 2024", # Subtítulo
         x = "Meses",
         y = "Tiempo promedio (min)") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),  # Alinea el título a la derecha
      plot.subtitle = element_text(hjust = 0.5), # Alinea el subtítulo a la derecha
      axis.text.x = element_text(margin = margin(t = 30, b=5)), # Añadir margen superior a las etiquetas del eje X 
      axis.text.y = element_text(margin = margin(r = 30, l=5)), # Añadir margen derecho a las etiquetas del eje Y
      plot.margin = margin(t = 20, b = 20, l = 20, r = 20) # Añadir margen superior para subir el título y subtítulo
    ) +
    annotate("text", x = Inf, y = -Inf, label = "Fuente: Cyclistic", hjust = 1.1, vjust = -0.5, size = 3)
  
  




#--------------------------------------------------------------------------------


# Resumir datos para dias de la semana
data_resumen_semana <- data_clean %>%
  mutate(dia_semana = factor(dia_semana, levels = dias_semana_ordenados),
         hora_inicio = hour(inicio_recorrido),
         hora_final = hour(final_recorrido)) %>%
  group_by(tipo_cliente, dia_semana) %>%
  summarise(
    cantidad_recorridos = n(),
    tiempo_promedio = mean(tiempo_recorrido, na.rm = TRUE),
    .groups = "drop"
  ) 

# Visualización: Número de recorridos
ggplot(data_resumen_semana, aes(x = dia_semana, y = cantidad_recorridos, fill = tipo_cliente)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = colores_clientes) + # Aplicar la paleta de colores específica
  scale_y_continuous(labels = comma) + # Cambia el formato del eje Y
  labs(
    title = "Número de Recorridos Diarios por Tipo de Cliente",
    subtitle = "Cyclistics, Chicago 2024",
    x = "Día de la Semana", y = "Número de Recorridos"
  ) +
  theme_minimal() +
  theme( 
    plot.title = element_text(hjust = 0.5), # Centra el título 
    plot.subtitle = element_text(hjust = 0.5), # Centra el subtítulo
    axis.text.x = element_text(margin = margin(t = 30, b=5)), # Añadir margen superior a las etiquetas del eje X 
    axis.text.y = element_text(margin = margin(r = 30, l=5)), # Añadir margen derecho a las etiquetas del eje Y
    plot.margin = margin(t = 20, b = 20, l = 20, r = 20) # Añadir margen superior para subir el título y subtítulo
    ) +
  annotate("text", x = Inf, y = -Inf, label = "Fuente: Cyclistic", hjust = 1.1, vjust = -0.5, size = 3)



# Visualización: Tiempo promedio  de recorridos por tipo de cliente en dias de la semana
  ggplot(data_resumen_semana, aes(x = dia_semana, y = tiempo_promedio, fill = tipo_cliente)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = colores_clientes) + # Aplicar la paleta de colores específica
  scale_y_continuous(labels = comma) + # Cambia el formato del eje Y
  labs(
    title = "Tiempo Promedio de Recorridos Durante la Semana por Tipo de Cliente",
    subtitle = "Cyclistics, Chicago 2024",
    x = "Día de la Semana", y = "Tiempo Promedio (min)"
  ) +
  theme_minimal() +
  theme( 
    plot.title = element_text(hjust = 0.5), # Centra el título 
    plot.subtitle = element_text(hjust = 0.5), # Centra el subtítulo
    axis.text.x = element_text(margin = margin(t = 30, b=5)), # Añadir margen superior a las etiquetas del eje X 
    axis.text.y = element_text(margin = margin(r = 30, l=5)), # Añadir margen derecho a las etiquetas del eje Y
  ) +
  annotate("text", x = Inf, y = -Inf, label = "Fuente: Cyclistic", hjust = 1.1, vjust = -0.5, size = 3)





