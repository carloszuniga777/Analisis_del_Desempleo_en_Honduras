#install.packages("readxl")     # Para leer archivos excel
#install.packages("tidyverse")  # Para manipular los datos y graficar 
#install.packages("ggrepel")    # Para etiquetas que no se traslapan 

library(readxl)           # Para leer archivos excel
library(tidyverse)        # Para manipular los datos y graficar
library(ggrepel)          # Para etiquetas que no se traslapan 

#--------------------------------------
# 1. Carga inicial 

# Carga del dataset de empleo segmentado por edad
empleo_edad <- read_excel("~/Programacion/Data_Analytics_Portfolio/Analisis_Desempleo_en_Honduras/dataset/Empleo_Honduras_2012_a_2023_por_Edad_y_Genero.xlsx",
                          sheet = "Empleo Edad 2012 - 2025",
                          skip = 1)


# Carga del dataset de empleo segmentado por genero
empleo_genero <- read_excel("~/Programacion/Data_Analytics_Portfolio/Analisis_Desempleo_en_Honduras/dataset/Empleo_Honduras_2012_a_2023_por_Edad_y_Genero.xlsx",
                            sheet = "Empleo Genero 2012 - 2025",
                            skip = 1)


#-------------------------------------------------------------------------
# 2. Analisis Exploratorio (EDA)
#--------------------------------------------------------------------------


# 2.1 Verificar

# Verificar la estructura de la tabla
glimpse(empleo_genero)
glimpse(empleo_edad)

# Verificar los tipos de datos
str(empleo_genero)
str(empleo_edad)

# Verificar valores faltantes
colSums(is.na(empleo_genero))  # No encontro valores faltantes o nulos
colSums(is.na(empleo_edad))    # No encontro valores faltantes o nulos


# Verificar duplicados
duplicated(empleo_genero) %>% sum() # No encontro duplicados
duplicated(empleo_edad) %>% sum()   # No encontro duplicados



#2.2 Se Analiza estadisticamente los datasets, esto nos permite:
#
# - Ver rangos (minimos, maximos) para ver si los valores tienen sentido
# - Ver tendencias centrales (media, mediana) para ver si hay sesgo o valores extremos
# - Ver la distribucion general para ver si hay dispersión o concentración 
# - Ver los tipos de datos para ver si las columnas están bien clasificadas

summary(empleo_genero) 
summary(empleo_edad) 

#--------------------------------------------------
# 2.3 Identificar Outliers y sesgo


# Funcion para detectar Outlier's 

detectar_outliers <- function(data, columna) {
  
  Q1 <- quantile(data[[columna]], 0.25)  # Primer Cuartil Q1        
  Q3 <- quantile(data[[columna]], 0.75)  # Tercer Cuartil Q3
  IQR_val <- Q3 - Q1                     # Rango Intercuartílico
  
  # Aplicando la regla de Tukey:
  # Límite Inferior: Q1 - 1.5 * IQR: Todo lo que sea menor a esto es un valor "inusualmente bajo".
  # Límite Superior: Q3 + 1.5 * IQR. Todo lo que sea mayor a esto es un valor "inusualmente alto".
  
  
  data %>%
    filter(.data[[columna]] < (Q1 - 1.5 * IQR_val) | 
             .data[[columna]] > (Q3 + 1.5 * IQR_val))
}






# Función para preparar y graficar un dataset completo

graficar_boxplot_unificado <- function(data, titulo) {
  
  # Pivot
  data_long <- data %>%
    select( Ocupados, 
            Desocupados, 
            Inactivos, 
           `Población Económicamente Activa (PEA)`, 
           `Población en Edad de Trabajar (PET)`) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")
  
  
  # Grafica
  ggplot(data_long, aes(x = Variable, y = Valor, fill = Variable)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 16) +
    labs(title = titulo, x = "", y = "Cantidad de Personas") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
}







# --- ANALISIS EMPLEO GÉNERO ---

graficar_boxplot_unificado(empleo_genero, "Distribución de Variables: Empleo por Género")

# Analisis del grafico:
# 
# 1. DISTRIBUCIÓN Y SESGO: 
#    - La mayoría de las variables (Ocupados, Desocupados, PEA) son simétricas, lo que facilita el uso de promedios.
#
#    - Inactivos presenta sesgo positivo (cola hacia arriba) debido a que la mediana está muy abajo en la caja. 
#      Esto indica que la mayoría de los datos tienen valores bajos de inactividad, 
#      pero hay unos pocos valores más altos que "estiran" la caja hacia arriba.
#
#     - PET sesgo negativo (mediana alta) debido a que la mediana está más cerca de la parte superior de la caja.
#       Eso se traduce que El 25% de los datos que están justo por encima del promedio están "apretados" 
#       en un rango de valores pequeño. En cambio, el 25% que está justo por debajo tiene que cubrir más espacio.      
#
#    - IMPACTO DEL SESGO: El promedio no representa bien la realidad; se "estira" hacia los valores extremos.
#    - SOLUCIÓN: Usar la MEDIANA en lugar del promedio para reportes de tendencia central.
#
# 2. OUTLIERS: 
#    - No se detectaron valores atípicos. Los datos son consistentes y confiables para análisis directos.






# --- ANALISIS EMPLEO EDAD ---

graficar_boxplot_unificado(empleo_edad, "Distribución de Variables: Empleo por Edad")

# Analisis del grafico:
# 
# 1. DISTRIBUCIÓN Y SESGO:
#    - Existe un SESGO POSITIVO marcado en Desocupados e Inactivos. La mayoría de los grupos de edad tienen 
#      valores bajos, pero unos pocos grupos "disparan" las cifras hacia arriba.
#
# 2. OUTLIERS (Puntos Rojos):
#    - Detectados en Desocupados e Inactivos.
#    - IMPACTO: Estos valores extremos inflan los totales y pueden dar una falsa sensación de crisis o 
#      inactividad generalizada cuando solo ocurre en nichos específicos.
#    - SOLUCIÓN: Investigar si son errores de captura o grupos demográficos específicos (ej. jóvenes). 
#      Para modelos predictivos, se recomienda aplicar logaritmos o Winsorization (limitar valores extremos).


# Grafico de boxplot para ver a detalle Desocupados
# boxplot(empleo_edad$Desocupados, main = "Outliers en población Desocupado", col = "lightgray")


# Detalle de outliers de Desocupados
detectar_outliers(empleo_edad, "Desocupados")   # Existen valores atipicos

# Detalle de outliers de Inactivos 
detectar_outliers(empleo_edad, "Inactivos")   # Existen valores atipicos


#---------------------------------------------
# 2.4 Analizar la calidad y la consistencia

  
# Validar si PEA = Ocupados + Desocupados y si PET = Ocupados + Desocupados + Inactivos
  
# Empleo Edad

empleo_edad_test <- empleo_edad %>% 
  mutate(
          
         check_pea = Ocupados + Desocupados,                  # Calculamos la PEA teórica
         check_pet = Ocupados + Desocupados + Inactivos,      # Calculamos la PET teórica
         
         # Verificamos si la diferencia es significativa (mayor a 0.01), 
         # Si es mayor a 0.01 sigifica que existe diferencia (FALSE)
         # Si es menor a 0.01 significa que no existe diferencia (TRUE)
         is_consistent_pea = abs(check_pea - `Población Económicamente Activa (PEA)`) < 0.01,
         is_consistent_pet = abs(check_pet - `Población en Edad de Trabajar (PET)`) < 0.01
         ) 


# No hay diferencias PEA: TRUE  
print("Validación PEA:")
table(empleo_edad_test$is_consistent_pea)

# No hay diferencia PET: TRUE
print("Validación PET:")
table(empleo_edad_test$is_consistent_pet)






# Empleo Genero

empleo_genero_test <- empleo_genero %>% 
  mutate(
    
    check_pea = Ocupados + Desocupados,                  # Calculamos la PEA teórica
    check_pet = Ocupados + Desocupados + Inactivos,      # Calculamos la PET teórica
    
    # Verificamos si la diferencia es significativa (mayor a 0.01), 
    # Si es mayor a 0.01 sigifica que existe diferencia (FALSE)
    # Si es menor a 0.01 significa que no existe diferencia (TRUE)
    is_consistent_pea = abs(check_pea - `Población Económicamente Activa (PEA)`) < 0.01,
    is_consistent_pet = abs(check_pet - `Población en Edad de Trabajar (PET)`) < 0.01
  ) 

# No hay diferencias: TRUE
print("Validación PEA:")
table(empleo_genero_test$is_consistent_pea)


# No hay diferencia PET: TRUE
print("Validación PET:")
table(empleo_genero_test$is_consistent_pet)





#--------------------------------------------
#     3. Limpieza y Transformaciones
#--------------------------------------------


# 3.1. Limpiando todo el dataset de espacios raros

empleo_edad <- empleo_edad %>% 
  mutate(across(where(is.character), str_squish))   # str_squish: Quita los espacios de los extremos y convierte cualquier doble espacio interno en uno solo



#3.2. Estandarizacion de los grupos de edad 

empleo_edad <- empleo_edad %>% 
  mutate(`Grupos_edad` = case_when (
    
    # ---- Supergrupo: Menores de 15 años ---
    `Grupos de edad` %in% c("De 10 a 11 años",
                            "De 12 a 14 años") ~ "Menores de 15 años",
    
    # --- Supergrupo: 15 a 29 años ---
    `Grupos de edad` %in% c("De 15 a 18 años", 
                            "De 19 a 24 años", 
                            "De 25 a 29 años") ~ "Jóvenes (15-29 años)",
    
    # --- Supergrupo: 30 a 44 años ---
    `Grupos de edad` %in% c("De 30 a 34 años", "De 35 a 39 años", 
                            "De 40 a 44 años", "De 30 a 35 años", "De 36 a 44 años") ~ "Adultos Joven (30-44 años)",  
    
    # --- Supergrupo: 45 a 59 años ---
    `Grupos de edad` %in% c("De 45 a 49 años", "De 50 a 54 años", 
                            "De 55 a 59 años", "De 45 a 59 años") ~ "Adultos Mayores  (45-59 años)",                    
    
    # --- Supergrupo: 60 años o más ---
    `Grupos de edad` %in% c("De 60 a 64 años", "De 65 a 69 años", 
                            "De 70 a 74 años", "De 75 a 79 años",                    
                            "De 60 años y más", "De 65 años y más") ~ "Adultos en Edad de Retiro (+60 años)", 
    
    # --- Si algo no coincide, lo marcamos para revisar ------
    TRUE ~ "Revisar/Otros"
    ))




# 3.3. Dataset Resumen Edad: 
# Se crea un nuevo Dataset para calcular los valores de Ocupado, Desocupado, Inactivo, Inactivo, PEA, PET
# de cada supergrupo Grupos_edad y calculamos las tasas

empleo_edad_resumen <- empleo_edad %>% 
  group_by(Año, Grupos_edad) %>% 
summarise(
  Ocupados = sum(Ocupados, na.rm = TRUE),
  Desocupados = sum(Desocupados, na.rm = TRUE),
  Inactivos = sum(Inactivos, na.rm = TRUE),
  PEA = sum(`Población Económicamente Activa (PEA)`,na.rm = TRUE),
  PET = sum(`Población en Edad de Trabajar (PET)`, na.rm = TRUE),
  .groups = 'drop'
) %>% 
  # 2. Calculamos las tasas:  Tasa de empleo, actividad, ocupación e inactividad
  mutate( tasa_desempleo = round((Desocupados / PEA) * 100, 2), 
          tasa_actividad = round((PEA / PET) * 100, 2), 
          tasa_ocupacion = round((Ocupados / PET) * 100, 2)
  )







# 3.4. Calcular tasas del dataset empleo_genero: Tasa de empleo, actividad y ocupación 

empleo_genero<- empleo_genero %>% 
  mutate( tasa_desempleo = round((Desocupados / `Población Económicamente Activa (PEA)`) * 100, 2), 
          tasa_actividad = round(( `Población Económicamente Activa (PEA)`/ `Población en Edad de Trabajar (PET)`) * 100, 2), 
          tasa_ocupacion = round((Ocupados / `Población en Edad de Trabajar (PET)`) * 100, 2)
  )

#---------------------------------------------
#   4. Visualizaciones 
#---------------------------------------------

# 4.1.  PET vs PET  

# Datos para el grafico de barras:
#    Primero se agrupan los datos por año y sumar las poblaciones y 
#    luego se pivotean los datos (Convertir de formato ancho a largo)

empleo_honduras_pet_vs_pea <-  empleo_edad_resumen %>% 
  group_by(Año) %>% 
  summarise(
    PET = sum(PET, na.rm = TRUE),
    PEA = sum(PEA, na.rm = TRUE)
  ) %>% 
  pivot_longer(cols = c(PET, PEA), names_to = "Categoria", values_to = "Valor") %>% 
  mutate(Categoria = factor(Categoria, levels = c("PET", "PEA")))      # Forzamos el orden para que PET aparezca primero (izquierda)



# Datos para el grafico de linea:
empleo_tasa_actividad_anual <- empleo_edad_resumen %>%
  group_by(Año) %>%
  summarise(
    PET = sum(PET, na.rm = TRUE),
    PEA = sum(PEA, na.rm = TRUE)
  ) %>%
  mutate(tasa_actividad = (PEA / PET) * 100)


# Configuración del eje secundario
# Necesitamos un factor de escala para que el 100% llegue al tope del eje Y (aprox 8M)
# Escala: 8,000,000 / 100 = 80,000
escala_factor <- 80000




# Construcción del Gráfico
ggplot() +
  
  # 1. GRAFICO BARRAS: PET y PEA
  geom_col(data = empleo_honduras_pet_vs_pea, aes(x = factor(Año), y = Valor, fill = Categoria), 
           position = position_dodge(width = 0.8), width = 0.7) +
  
  
  # 2. ETIQUETAS DE BARRAS: (Números internos)
  geom_text(data = empleo_honduras_pet_vs_pea, 
            aes(x = factor(Año), y = Valor, label = scales::comma(Valor)),
            position = position_dodge(width = 0.8),
            angle = 90, hjust = 1.2, color = "white", size = 3, fontface = "bold") +
  
  
  # 3. GRAFICO LÍNEA: Tasa de Actividad (Multiplicada por el factor para que sea visible)
  geom_line(data = empleo_tasa_actividad_anual, aes(x = factor(Año), y = tasa_actividad * escala_factor, group = 1),
            color = "#F39C12", size = 1.2) +
  
  
  # 4. PUNTOS Y ETIQUETAS DE LA TASA
  
  # Puntos
  geom_point(data = empleo_tasa_actividad_anual, 
             aes(x = factor(Año), y = tasa_actividad * escala_factor), 
             color = "#F39C12", size = 3) +
  
  # Etiquetas
  geom_text(data = empleo_tasa_actividad_anual %>% filter(Año %in% c(2012, 2016, 2017, 2021, 2025)),
            aes(x = factor(Año), y = (tasa_actividad * escala_factor) + 200000,
                                      label = paste0(round(tasa_actividad, 1), "%")),
              #color = "#D35400",
              fontface = "bold", 
              size = 3,
              vjust = 2.5,           # Valor positivo empuja el texto hacia ABAJO del punto
     ) +
  
  
  # 5. ESCALAS
  scale_fill_manual(values = c("PET" = "#A9A9A9", "PEA" = "#5DADE2"),
                    labels = c("PET" = "Población en Edad de Trabajar", "PEA" = "Población Activa")) +
  
  # Doble eje: El primario en formato normal, el secundario transformado de vuelta a %
  scale_y_continuous(
    name = "Población",
    labels = scales::comma,
    sec.axis = sec_axis(~ . / escala_factor, name = "Tasa de Actividad (%)", 
                        labels = function(b) paste0(b, "%"))
  ) +
  
  # TEXTOS
    labs(title = "Población apta para trabajar (PET) vs quienes son económicamente activos (PEA) \n en Honduras",
         subtitle = "Comparativo PET vs PEA y porcentaje de participación (2012-2025)",
         caption = "Fuente: Instituto Nacional de Estadística (INE)",
         x = "Año",
         fill = "Categoría Poblacional",
         ) +
  
  
  # TEMA PROFESIONAL
  theme_minimal() +                                  # Aplica un estilo limpio con fondo blanco y rejillas sutiles
  theme(
    
    # -----------FORMATO A LOS TITULOS ---------------------
    # Centra el título (hjust = 0.5), lo pone en negrita y añade margen inferior (b = 20)
    # para que no choque visualmente con las barras del gráfico.
    
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "darkblue"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "#555555", face = "italic", hjust = 1),
    
    
    # --- ETIQUETAS DE LOS EJES (X e Y) ---
    
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),
    
    axis.title.x = element_text(face = "bold", size = 14, color = "darkgrey"),
    
    axis.title.y = element_text(face = "bold", size = 14, color = "darkgrey"),
    axis.title.y.right = element_text(color = "#D35400", face = "bold", size = 12),
    
    # --- CONFIGURACIÓN DE LA LEYENDA ---
    legend.position = "bottom",                      # Posiciona la leyenda en la parte inferior para maximizar el área de visualización.
    legend.box = "horizontal",
    
    # -------- Limpieza visual ------------
    panel.grid.major.x = element_blank(),             # Elimina las líneas verticales de la cuadrícula para un look más moderno y enfocado en las barras.
    panel.grid.minor = element_blank()  
  )






#-------------------------------------------------------------------------------
# 4.2. Dinámica laboral en Honduras


# Primero se agrupan los datos y se suman las poblaciones, y se vuelve a calcular las tasas. Luego se hace pivot
empleo_honduras <-  empleo_genero %>% 
  group_by(Año) %>% 
  summarise(
    Ocupados = sum(Ocupados, na.rm = TRUE),
    Desocupados = sum(Desocupados, na.rm = TRUE),
    Inactivos = sum(Inactivos, na.rm = TRUE),
    PET = sum(`Población en Edad de Trabajar (PET)`, na.rm = TRUE),
    PEA = sum(`Población Económicamente Activa (PEA)`, na.rm = TRUE)
  ) %>% 
  mutate(
    tasa_desempleo = round((Desocupados / PEA) * 100, 2), 
    tasa_actividad = round(( PEA/ PET) * 100, 2), 
    tasa_ocupacion = round((Ocupados / PET) * 100, 2)
  ) %>% 
  pivot_longer(
    cols = c(tasa_desempleo, tasa_actividad, tasa_ocupacion),
    names_to = "Categoria",
    values_to = "Valor"
  ) %>%
  select(Año, Categoria, Valor)  






colores_honduras <- c(
  "tasa_actividad" = "#E6A100",  # Gris azulado (Contexto/Potencial)
  "tasa_ocupacion" = "#00509D",  # Azul fuerte (Éxito/Empleo real)
  "tasa_desempleo" = "#D32F2F"   # Rojo (Alerta/Punto de dolor)
)


# Grafico de lineas de la dinamica laboral en Honduras

ggplot(empleo_honduras, aes(x=Año, y=Valor, colour = Categoria)) +

  # 1. GEOMETRÍAS: Líneas de tendencia y puntos de medición
  geom_line(size = 1.2, alpha = 0.8) +                      # Dibuja las líneas de tendencia
  geom_point(size = 2) +                                    # Marca los puntos exactos de medición



  
  # 2. ETIQUETAS DE DATOS:  Etiquetas de datos inteligentes (Solo para el último año y picos para no saturar)
  
  # --- Etiquetas para Tasa de Actividad (ARRIBA)
  geom_text_repel(
    data = empleo_honduras %>% filter(Año %in% c(2012, 2020, 2025), Categoria == "tasa_actividad"),
    aes(label = paste0(round(Valor, 1), "%")),
    nudge_y = 5,           # Empuja hacia arriba
    direction = "y",
    fontface = "bold", size = 3.5, segment.color = NA, show.legend = FALSE
  ) +

  # --- Etiquetas para Población Ocupada (ABAJO)
  geom_text_repel(
    data = empleo_honduras %>% filter(Año %in% c(2012, 2020, 2025), Categoria == "tasa_ocupacion"),
    aes(label = paste0(round(Valor, 1), "%")),
    nudge_y = -5,          # Empuja hacia abajo
    direction = "y",
    fontface = "bold", size = 3.5, segment.color = NA, show.legend = FALSE
  ) +

  # --- Etiquetas para Tasa de Desempleo (ARRIBA)
  geom_text_repel(
    data = empleo_honduras %>% filter(Año %in% c(2012, 2020, 2025), Categoria == "tasa_desempleo"),
    aes(label = paste0(round(Valor, 1), "%")),
    nudge_y = 3,
    direction = "y",
    fontface = "bold", size = 3.5, segment.color = NA, show.legend = FALSE
  ) +


  # Etiquetas para Tasa de Actividad (ARRIBA)
  # geom_text(
  #   data = empleo_honduras %>% filter(Año %in% c(2012, 2020, 2025), Categoria == "tasa_actividad"),
  #   aes(label = paste0(round(Valor, 1), "%")),
  #   vjust = -1.5,          # Valor negativo empuja el texto hacia ARRIBA del punto
  #   fontface = "bold", size = 3.5, show.legend = FALSE
  # ) +
  #
  # # Etiquetas para Población Ocupada (ABAJO)
  # geom_text(
  #   data = empleo_honduras %>% filter(Año %in% c(2012, 2020, 2025), Categoria == "tasa_ocupacion"),
  #   aes(label = paste0(round(Valor, 1), "%")),
  #   vjust = 2.5,           # Valor positivo empuja el texto hacia ABAJO del punto
  #   fontface = "bold", size = 3.5, show.legend = FALSE
  # ) +
  #
  # # Etiquetas para Tasa de Desempleo (ARRIBA)
  # geom_text(
  #   data = empleo_honduras %>% filter(Año %in% c(2012, 2020, 2025), Categoria == "tasa_desempleo"),
  #   aes(label = paste0(round(Valor, 1), "%")),
  #   vjust = -1.2,
  #   fontface = "bold", size = 3.5, show.legend = FALSE
  # ) +

  
  
  # 3. TEXTOS Y TÍTULOS

  labs(
    title = "Dinámica laboral en Honduras",
    subtitle = "Evolución de la Participación, Ocupación y Desempleo (2012 - 2025)",
    caption = "Fuente: Instituto Nacional de Estadística (INE)",
    y = "Tasas (%)",
    x = "Año de la Encuesta",
    color = "Indicador Laboral" # Título de la leyenda
  ) +



  # 4. Escalas y etiquetas

  scale_color_manual(                                                                 # Se personalizan los colores de cada linea y los nombres de la leyenda
    values = colores_honduras,
    labels = c(
      "tasa_actividad" = "Tasa de Actividad",
      "tasa_ocupacion" = "Población Ocupada",
      "tasa_desempleo" = "Tasa de Desempleo"
    )
  ) +

  scale_x_continuous(breaks = unique(empleo_honduras$Año)) +                   # Todos los años en el eje X

  scale_y_continuous(labels = function(x) paste0(x, "%"),                     # Añade el símbolo %
                     limits = c(0, 100),                                      # Limites de rango del eje Y
                     breaks = seq(0, 100, by = 10)                            # Guías cada 10% para mejorar lectura
  ) +





  # 5. Tema profesional de BI
  theme_minimal(base_size = 14) +
  theme(
    # --- CENTRAR Y DAR TAMAÑO A TÍTULOS ---
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "darkblue"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "#555555", face = "italic", hjust = 1),

    # --- ETIQUETAS DE LOS EJES (X e Y) ---
    axis.title.x = element_text(face = "bold", size = 14, color = "darkgrey"),
    axis.title.y = element_text(face = "bold", size = 14, color = "darkgrey"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),   # Inclinamos los años para que no choquen
    axis.text.y = element_text(size = 10, color = "black"),

    # --- CONFIGURACIÓN DE LA LEYENDA ---
    legend.position = "top",                                                         # Coloca la leyenda arriba
    legend.title = element_text(face = "bold", size =11),
    legend.text = element_text(size = 11),
    legend.background = element_rect(fill = "white", color = "lightgrey"),

    # Limpieza visual
    panel.grid.minor = element_blank()
  )










#-------------------------------------------------------------------------------
# 4.3. Estructura del mercado por Edad


# PREPARACIÓN: Aseguramos que los datos estén ordenados por año
# Esto es vital para que las líneas de trayectoria sigan el orden cronológico
empleo_edad_pivot <- empleo_edad_resumen %>%
  select(Año, Grupos_edad, tasa_actividad, tasa_desempleo) %>%
  pivot_longer(
    cols = c(tasa_actividad, tasa_desempleo), 
    names_to = "Indicador", 
    values_to = "Valor"
  ) 


# DEFINICIÓN DE COLORES 
colores_kpi <- c(
  "tasa_actividad" = "#2E7D32", # Verde bosque
  "tasa_desempleo" = "#C62828"  # Rojo intenso
)



# CONSTRUCCIÓN DEL GRÁFICO: Grafico de lineas
ggplot(empleo_edad_pivot, aes(x = Año, y = Valor, color = Indicador)) +
  
  # 1. GEOMETRÍAS: Líneas de tendencia y puntos de medición
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  
  
  # 2.Separación de paneles
  facet_wrap(~Grupos_edad, ncol = 2) + 
  
  
  
  # 3. ETIQUETAS DE DATOS:  Etiquetas de datos inteligentes (Solo para el último año y picos para no saturar)
  geom_text(data = empleo_edad_pivot %>% filter(Año %in% c(2012, 2020, 2025)),
            aes(label = paste0(round(Valor, 1), "%")),
            vjust = -1, size = 3, fontface = "bold", show.legend = FALSE) +
  
  
  
  # 4. TÍTULOS Y TEXTOS
  labs(
    title = "ANÁLISIS ESTRUCTURAL DEL MERCADO LABORAL POR EDAD",
    subtitle = "Honduras 2012-2025: Comparativa entre Participación Activa y Desempleo Abierto",
    x = "Año de la Encuesta",
    y = "Tasa Porcentual (%)",
    color = "Indicador",
    caption = "Fuente: INE Honduras"
  ) +
  
  
  
  # 5. CONFIGURACIÓN DE ESCALAS
  
  # Color de las lineas y renombrar las etiquetas de la leyenda
  scale_color_manual(
    values = colores_kpi,
    labels = c("tasa_actividad" = "Tasa de Actividad", "tasa_desempleo" = "Tasa de Desempleo")
  ) +
  
  # Escala del eje y 
  scale_y_continuous(limits = c(0, 100),                                        # Establece los limites de 0 a 100 
                     labels = function(x) paste0(x, "%")                        # Coloca signo % a cada dato del eje Y  
                     ) +            
  
  # Escala del eje x
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2025)) +                     # Valores del eje x
  
  
  
  
  # 5. TEMA PROFESIONAL DE ALTA LECTURA
  theme_minimal(base_size = 14) +
  theme(
    
    # --------- Fondo de los títulos de panel --------------
    strip.background = element_rect(fill = "#263238"), # Fondo oscuro para los títulos de cada panel
    strip.text = element_text(color = "white", face = "bold"),
    
    #-------- Espacio entre paneles ------------
    panel.spacing = unit(1.5, "lines"), 
    
    # --- CENTRAR Y DAR TAMAÑO A TÍTULOS ---
    plot.title = element_text(face = "bold", size = 16, color = "#1A237E"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "#555555", face = "italic", hjust = 1),
    

    # --- ETIQUETAS DE LOS EJES (X e Y) ---
    axis.title.x = element_text(face = "bold", size = 14, color = "darkgrey"),
    axis.title.y = element_text(face = "bold", size = 14, color = "darkgrey"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),   # Inclinamos los años para que no choquen
    axis.text.y = element_text(size = 10, color = "black"),
    
        
    # --- CONFIGURACIÓN DE LA LEYENDA ---
    legend.position = "top",
    
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 11),
    legend.background = element_rect(fill = "white", color = "lightgrey"),
    
    
    # ---- Limpieza visual -----------
    panel.grid.minor = element_blank()
  )





#-----------------------------------------------------------------------------------
# 4.4. BRECHA DE GENERO Y DINAMICA LABORAL


# Convirtiendo de formato ancho a largo
empleo_genero_pivot <- empleo_genero %>%
  select(Año, Genero, tasa_desempleo, tasa_actividad) %>%
  pivot_longer(cols = c(tasa_desempleo, tasa_actividad), 
               names_to = "Indicador", 
               values_to = "Valor")

# Colores 
colores_genero <- c(
  "tasa_actividad" = "#2E7D32", # Verde (Participación)
  "tasa_desempleo" = "#C62828"  # Rojo (Desempleo)
)



# CONSTRUCCIÓN DEL GRÁFICO: Grafico de lineas
ggplot(empleo_genero_pivot, aes(x = Año, y = Valor, color = Indicador, group = Indicador)) +

  # 1. GEOMETRÍAS: Líneas de tendencia y puntos de medición
  geom_line(size = 1.3, alpha = 0.8) +                      # Dibuja las líneas de tendencia
  geom_point(size = 2.5) +                                  # Marca los puntos exactos de medición

  # 2.Separación de paneles
  facet_wrap(~Genero, ncol = 2) + 

  # 2. ETIQUETAS DE DATOS
  
  # 3. Etiquetas de datos inteligentes (Solo para el último año y picos para no saturar)
  geom_text(data = empleo_genero_pivot %>% filter(Año %in% c(2012, 2020, 2025)),
            aes(label = paste0(round(Valor, 1), "%")),
            vjust = -1.2, size = 3.5, fontface = "bold", show.legend = FALSE) +
  
  
  # geom_label(aes(label = paste0(round(tasa_desempleo, 1), "%")),  # Se redondea a un decimal para evitar etiquetas muy largas
  #            vjust = -0.5,                                         # Desplaza el texto hacia arriba del punto
  #            size = 3.2,                                           # Tamaño de la fuente del valor
  #            fontface = "bold",                                    # Resalta el valor en negrita
  #            color = "black",                                      # Texto en negro para contraste
  # 
  #            #Estilo del fondo de las etiquetas
  #            fill = "#E5E7E9",                                     # Gris claro profesional
  #            alpha = 0.7,                                          # Transparencia
  #            label.size = 0,                                       # Quita el borde del recuadro
  #            #
  #            show.legend = FALSE) +                                # Evita que aparezca una 'a' en la leyenda
  # 


  # 3. TEXTOS Y TÍTULOS
  
  labs(
    title = "BRECHA DE GÉNERO Y DINÁMICA LABORAL",
    subtitle = "Comparativa de Tasa de Actividad vs. Desempleo por Género (2012-2025)",
    caption = "Fuente: INE Honduras",
    y = "Porcentaje (%)",
    x = "Año",
    color = "Métrica"
  ) +
  

  # 4. Escalas y etiquetas
  
  scale_color_manual(
    values = colores_genero,
    labels = c("tasa_actividad" = "Tasa de Actividad", "tasa_desempleo" = "Tasa de Desempleo")
  ) +
  
  # Escala del eje x
  scale_x_continuous(breaks = c(2012, 2015, 2018, 2020, 2022, 2025)) +      
  
  # Escala del eje y                                                                        
  scale_y_continuous(limits = c(0, 105),                                    # Establece los limites de 0 a 100 + 5
                     breaks = seq(0, 100, 20),                              # La secuencia de los datos del eje Y es de 20 en 20
                     labels = function(x) paste0(x, "%")                    # Coloca signo % a cada dato del eje Y  
                     ) +
  
  

  # 5. Tema profesional de BI
  theme_minimal(base_size = 15) +
  theme(
    
    # --------- Fondo de los títulos de panel --------------
    strip.background = element_rect(fill = "#ECEFF1", color = NA), 
    strip.text = element_text(face = "bold", color = "#263238", size = 14),
    
    # --------- Más espacio entre los dos gráficos ------
    panel.spacing = unit(2, "lines"),                              
    

    # --- CENTRAR Y DAR TAMAÑO A TÍTULOS ---
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "darkblue"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "#555555", face = "italic", hjust = 1),

    # --- ETIQUETAS DE LOS EJES (X e Y) ---
    axis.title.x = element_text(face = "bold", size = 14, color = "darkgrey"),
    axis.title.y = element_text(face = "bold", size = 14, color = "darkgrey"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),   # Inclinamos los años para que no choquen
    axis.text.y = element_text(size = 10, color = "black"),

    # --- CONFIGURACIÓN DE LA LEYENDA ---
    legend.position = "top",                                                         # Coloca la leyenda arriba
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 11),
    legend.background = element_rect(fill = "white", color = "lightgrey"),

    # Limpieza visual
    panel.grid.minor = element_blank()
  )















