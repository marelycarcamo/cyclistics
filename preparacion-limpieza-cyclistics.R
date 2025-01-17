# --------------------------------------------------------------------------------------------------------------------------------------------------

# Instalar y cargar librería 
if (!requireNamespace("tidyverse", quietly = TRUE)) 
  install.packages("tidyverse") 
library(tidyverse) 

# Función para construir rutas 
construir_ruta <- function(...){
  return(file.path(getwd(), ...))
} 

# Obtener la ruta del proyecto y la ruta de datos 
ruta_datos <- construir_ruta("data") 
ruta_datos_de_origen <- construir_ruta("data", "data-meses")


# Cargar y mostrar la lista de archivos CSV
archivos_csv <- list.files(path = ruta_datos_de_origen, pattern = "\\.csv$", full.names = FALSE, recursive = TRUE)
print(archivos_csv)


# Iterar sobre los archivos CSV para visualizar las columnas
for (archivo in archivos_csv) { 
  tryCatch({
    datos <- read.csv(file.path(ruta_datos_de_origen, archivo)) 
    print(paste('NOMBRE ARCHIVO: ', archivo))
    print(colnames(datos))
    str(datos)
    cat("\n")
  }, error = function(e) {
    message("Error leyendo archivo: ", archivo, "\n", e)
  })
}

# Leer y combinar todos los archivos en un único dataframe
dataframe_combinado <- archivos_csv %>%
  map_df(~ read_csv(file.path(ruta_datos_de_origen, .))) # Combina automáticamente todos los datos
str(dataframe_combinado)
head(dataframe_combinado)

# Exportar dataframe a CSV
write.csv(dataframe_combinado, file.path(ruta_datos, "2024-tripdata.csv"), row.names = FALSE)

# --------------------------------------------------------------------------------------------------------------------------------------------------------
# LIMPIEZA DE DATOS
# -------------------------------------------------------------------------------------------------------------------------------------------------------

# Instalar y cargar paquete lubridate
install.packages('lubridate')
library(lubridate)

# Importar nueva data
data <- read_csv(file.path(ruta_datos, '2024-tripdata.csv'))
head(data)
summary(data)
str(data)
glimpse(data)


# Eliminar columnas innecesarias para el proceso de análisis
data <- data %>% 
  select(-c('start_lat', 'start_lng', 'end_lat', 'end_lng'))
glimpse(data)


# Renombramos las columnas
tripdata_rename <- rename(data, 
                          id_recorrido = ride_id,
                          tipo_bicicleta = rideable_type,
                          inicio_recorrido = started_at,
                          final_recorrido = ended_at,
                          estacion_inicial = start_station_name,
                          estacion_final = end_station_name,
                          id_estacion_inicial = start_station_id,
                          id_estacion_final = end_station_id,
                          tipo_cliente = member_casual)
glimpse(tripdata_rename)



# Eliminar registros duplicados
tripdata_rename <- tripdata_rename %>%  distinct() 


# Verificar tipos de usuarios existentes y tipos de bicicletas
tripdata_rename %>% 
  group_by(tipo_bicicleta,tipo_cliente) %>%
  summarize(n())


# Crear tres nuevas columnas con el mes correspondiente y el tiempo de recorrido
# Cambiar valores
tripdata_new <- tripdata_rename %>% 
  mutate(tiempo_recorrido = as.numeric(difftime(final_recorrido, inicio_recorrido, units = 'mins')),
         mes = format(as.Date(inicio_recorrido), "%B"),
         dia_semana = weekdays(as.Date(inicio_recorrido)),
         tipo_cliente = recode(tipo_cliente,
                               "member" = 'suscriptor',
                               "casual" = 'cliente'),
         tipo_bicicleta = recode(tipo_bicicleta,
                                 'classic_bike' = 'bicicleta clasica',
                                 'electric_bike' = 'bicicleta electrica',
                                 'electric_scooter' = 'scooter electrico'))
glimpse(tripdata_new)
head(tripdata_new)



# Contar valores menores a 0 en la columna tiempo_recorrido
valores_negativos <- sum(tripdata_new$tiempo_recorrido < 0, na.rm = TRUE)
print(paste("Valores menores a 0 en tiempo_recorrido:", valores_negativos))


# Filtrar recorrido mayor a cero 
tripdata_new <- tripdata_new %>% 
  filter(tiempo_recorrido > 0)



# Tratar valores nulos, reemplazando por "Desconocido"---------------------------------------------------------------------------------------------------

# Mostrar cantidad de valores nulos por columna y porcentajes
colSums(is.na(tripdata_new))
porcentaje_nulos <- sapply(tripdata_new[, c("id_estacion_inicial", "estacion_inicial", "id_estacion_final", "estacion_final")], function(x) mean(is.na(x)) * 100)
print(porcentaje_nulos)

# Reemplazar valores NA
tripdata_new <- tripdata_new %>%
  replace_na(list(
    estacion_inicial = "Desconocido",
    id_estacion_inicial = "Desconocido",
    estacion_final = "Desconocido",
    id_estacion_final = "Desconocido"
  ))

#--Formatear valores tipo caracter------------------------------------------------------------------------------------------------------------------------------------------------------


# Formatear valores tipo caracter 
# Eliminar espacios en blanco y formatear las columnas de tipo carácter 


# Formatear las columnas de tipo caracter
tripdata_new <- tripdata_new %>%
  mutate(
    across(where(is.character), ~ str_trim(.)),         # Eliminar espacios en blanco
    estacion_inicial = str_to_title(estacion_inicial),  # Inicial mayúscula
    estacion_final = str_to_title(estacion_final)     # Igual para final
  )
head(tripdata_new)

                
#---Manejo de columnas en conflicto-----------------------------------------------------------------------------------------------------------------------

# Agrupar por id_estacion_inicial y estacion_inicial y contar las ocurrencias 
datos_conflictos_inicial <- tripdata_new %>%
  group_by(id_estacion_inicial, estacion_inicial) %>%
  summarize(conteo = n(), .groups = "drop") %>%
  ungroup() %>% group_by(id_estacion_inicial) %>%
  filter(n() > 1) 
print(datos_conflictos_inicial)


# Agrupar por id_estacion_final y estacion_final  contar las ocurrencias 
datos_conflictos_final <- tripdata_new %>%
  group_by(id_estacion_final, estacion_final) %>%
  summarize(conteo = n(), .groups = "drop") %>%
  ungroup() %>% group_by(id_estacion_final) %>%
  filter(n() > 1)
print(datos_conflictos_final)



# Eliminar columnas en conflictos para garantizar la consistencia de datos
tripdata_new <- tripdata_new %>% 
  select(-c('id_estacion_inicial','estacion_inicial','id_estacion_final','estacion_final'))
glimpse(tripdata_new)


#---Exportar dataframe limpio ------------------------------------------------------------------------------------------------------------------------------------------------------


# Exportar datos limpios a CSV
write.csv(tripdata_new, file.path(ruta_datos, "2024-tripdata-clean.csv"), row.names = FALSE)
