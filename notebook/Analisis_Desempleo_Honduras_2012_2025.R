#install.packages("readxl")    # Para leer archivos excel
#install.packages("ggrepel")


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
# 2.3 Identificar Outliers 


# 2.3.1 Crear funcion para detectar Outlier's 

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





# 2.3.2 Analizando el Dataset empleo_genero

# 2.3.2.1 Analizando: Ocupados        

# Identificando los valores atipicos
detectar_outliers(empleo_genero, "Ocupados")


# Graficando por medio de Boxplot para identificar los valores atipicos

boxplot(empleo_genero$Ocupados, main = "Outliers en población Ocupados", col = "lightgray")

# Analisis del grafico:
#
# - Distribución: Es simétrica. La línea negra gruesa (mediana) está casi en el centro de la caja.
#
# - Concentración: La "caja" es relativamente grande, lo que indica que hay una dispersión moderada pero constante en el número de personas ocupadas.
#
# - Simetría: Los "bigotes" tienen una longitud similar, lo que sugiere que no hay un sesgo marcado hacia valores muy altos o muy bajos.


# Conclusion: No se encontraron valores atipicos



# 2.3.2.2 Analizando: Desocupados 

# Identificando los valores atipicos
detectar_outliers(empleo_genero, "Desocupados")


# Graficando por medio de Boxplot para identificar los valores atipicos
boxplot(empleo_genero$Desocupados, main = "Outliers en población Desocupados", col = "lightgray")

# Analisis del grafico:
#
# Simetría: Al igual que en ocupados, la mediana está bastante centrada.
#
# Escala: Nota que la escala es mucho menor (llega hasta 200,000 aprox.), comparada con los millones de la población inactiva u ocupada.
#
# Rango: El rango es bastante compacto, lo que significa que el desempleo se ha mantenido en niveles similares


# Conclusion: No se encontraron valores atipicos






# 2.3.2.3 Analizando: Inactivos 

# Identificando los valores atipicos
detectar_outliers(empleo_genero, "Inactivos")

# Graficando por medio de Boxplot para identificar los valores atipicos
boxplot(empleo_genero$Inactivos, main = "Outliers en población Inactiva", col = "lightgray")

# Analisis del grafico:
#
# Sesgo Positivo: La mediana está muy abajo en la caja. Esto indica que la mayoría de los datos tienen 
# valores bajos de inactividad, pero hay unos pocos valores más altos que "estiran" la caja hacia arriba.
#
# Dispersión: El 25% de los datos superiores (la parte de arriba de la caja) 
# está mucho más disperso que el 25% inferior.

# Conclusion: No se encontraron valores atipicos, pero si Sesgo positivo






# 2.3.2.4 Analizando: PEA 

# Identificando los valores atipicos
detectar_outliers(empleo_genero, "Población Económicamente Activa (PEA)")


# Graficando por medio de Boxplot para identificar los valores atipicos
boxplot(empleo_genero$`Población Económicamente Activa (PEA)`, main = "Outliers en PEA", col = "lightgray")

# Analisis del grafico:
#
# - Ausencia de Outliers: No se observan puntos o círculos por encima del bigote superior ni por debajo del inferior. 
# Todos los registros de la PEA caen dentro del rango esperado según la regla $1.5 x IQR$.
#
# - Distribución y Mediana: La línea negra (mediana) está ligeramente desplazada hacia la parte inferior 
# de la caja. Esto indica que el 50% de los datos tienden a agruparse en valores más cercanos 
# al límite inferior, mientras que el 50% superior tiene una dispersión 
# un poco mayor (la parte superior de la caja es más alta).
#
# - Rango de Datos: La PEA se mueve principalmente entre un poco menos de 1,500,000 y cerca de 2,500,000. 
# La "caja" (donde está el 50% central de tus datos) es bastante compacta, lo que sugiere una estabilidad 
# relativa en la población activa durante el periodo analizado.

# Conclusion: No se encontraron valores atipicos






# 2.3.2.5 Analizando: PET 

# Identificando los valores atipicos
detectar_outliers(empleo_genero, "Población en Edad de Trabajar (PET)")


# Graficando por medio de Boxplot para identificar los valores atipicos
boxplot(empleo_genero$`Población en Edad de Trabajar (PET)`, main = "Outliers en PET", col = "lightgray")


# - Sesgo Negativo (Hacia arriba): A diferencia de la PEA, en la PET vemos que la mediana está más cerca de 
# la parte superior de la caja.
#
# - Interpretación: 
#          - La línea negra (mediana) está más arriba del centro exacto de la caja.
#
#           - Esto significa que el espacio entre la mediana y la parte superior de la caja es más pequeño 
#             que el espacio entre la mediana y la parte inferior.
#
#           - Traducción: El 25% de los datos que están justo por encima del promedio están "apretados" 
#              en un rango de valores pequeño. En cambio, el 25% que está justo por debajo tiene que cubrir más espacio.

# - Escala: Es, naturalmente, la variable con los valores más altos (llegando a los 4 millones), 
# ya que incluye tanto a la PEA como a los Inactivos.
#
# Conclusion: No se encontraron valores atipicos, pero si sesgo Negativo


# Conclusion final
# 
# Tras realizar el análisis exploratorio mediante diagramas de caja (boxplots) 
# y aplicar la regla del Rango Intercuartílico (IQR), 
# se determinó que las variables de Ocupados, Desocupados, Inactivos, PEA y PET 
# no presentan valores atípicos. Los datos muestran distribuciones contenidas 
# dentro de los límites esperados, lo que permite trabajar con los promedios 
# y sumatorias de la muestra sin riesgo de distorsiones por valores extremos.
# Ademas, las variables inactivo y PET presentaron sesgo



#--------------------------------------


# 2.3.3 Analizando el Dataset empleo_edad

# 2.3.3.1 Analizando: Ocupados        

# Identificando los valores atipicos
detectar_outliers(empleo_edad, "Ocupados")


# Graficando por medio de Boxplot para identificar los valores atipicos
boxplot(empleo_edad$Ocupados, main = "Outliers en población Ocupados", col = "lightgray")

# Analisis del grafico:
#
# - Outliers: No se observan valores atipicos
# 
# - La Mediana: La línea gruesa negra dentro de la caja gris indica el valor central. 
#   Se observa que está ligeramente desplazada hacia la parte inferior de la caja, 
#   lo que sugiere una leve asimetría positiva (sesgo a la derecha), 
#   indicando que hay una mayor concentración de datos en los valores bajos 
#   y una dispersión un poco mayor hacia los valores altos.
#
# - Rango	Los datos fluyen desde cerca de 0 hasta aproximadamente 800,000 (basado en el límite superior del bigote).




# 2.3.3.2 Analizando: Desocupados 

# Identificando los valores atipicos
detectar_outliers(empleo_edad, "Desocupados")   # Existen valores atipicos



# Graficando por medio de Boxplot para identificar los valores atipicos
boxplot(empleo_edad$Desocupados, main = "Outliers en población Desocupados", col = "lightgray")

# Analisis del grafico
#
# - La Mediana (línea gruesa central): Se ubica aproximadamente en los 20,000. 
# Esto indica que el 50% de las observaciones tienen una población de desocupados 
# igual o menor a este valor.
#
# - Caja (Rango Intercuartílico): La caja gris representa el 50% central 
# de los datos. El límite inferior (Q1) está cerca de los 5,000 y el superior 
# (Q3) ronda los 35,000.
#
# - Asimetría: La caja es relativamente compacta en comparación con la 
# longitud del "bigote" superior y los outliers. 
# Esto sugiere una asimetría positiva (sesgada a la derecha); 
# es decir, hay una mayor concentración de datos en valores bajos, 
# pero una "cola" larga hacia valores muy altos.
#
# Conclusion:
# El gráfico revela una distribución desigual. La mayoría de las unidades 
# analizadas tienen una población de desocupados relativamente baja 
# (entre 0 y 40,000), pero existen casos extremos que triplican o cuadruplican 
# la mediana.







# 2.3.3.3 Analizando: Inactivos        

# Identificando los valores atipicos
detectar_outliers(empleo_edad, "Inactivos")   # Existen valores atipicos


# Graficando por medio de Boxplot para identificar los valores atipicos
boxplot(empleo_edad$Inactivos, main = "Outliers en población Inactiva", col = "lightgray")

# Analisis del grafico
#
# - Mediana (línea gruesa central): Se encuentra aproximadamente en el valor 
# 2 x 10^5 (200,000). Esto indica que el 50% de los datos analizados están 
# por debajo de este valor.
#
# - Sesgo: La mediana está más cerca de la parte inferior de la caja que 
# de la superior. Esto indica un sesgo positivo (a la derecha), 
# lo que significa que hay una mayor concentración de datos en valores bajos, 
# pero con una cola extendida hacia valores más altos.
#
# - Outliers: Se observan al menos cuatro valores atípicos por encima del "bigote" superior.
# Estas son regiones, periodos o grupos donde la población inactiva 
# es excepcionalmente alta en comparación con el resto del conjunto de datos.




# 2.3.3.4 Analizando: PEA        

# Identificando los valores atipicos
detectar_outliers(empleo_edad, "Población Económicamente Activa (PEA)")   


# Graficando por medio de Boxplot para identificar los valores atipicos
boxplot(empleo_edad$`Población Económicamente Activa (PEA)`, main = "Outliers en población PEA", col = "lightgray")

# No se observa valores atipicos


# 2.3.3.5 Analizando: PET  

# Identificando los valores atipicos
detectar_outliers(empleo_edad, "Población en Edad de Trabajar (PET)")   


# Graficando por medio de Boxplot para identificar los valores atipicos
boxplot(empleo_edad$`Población en Edad de Trabajar (PET)`, main = "Outliers en población PET", col = "lightgray")


# No se observa valores atipicos




# Conclusion final
# 
# Tras realizar el análisis exploratorio mediante diagramas de caja (boxplots) 
# y aplicar la regla del Rango Intercuartílico (IQR), 
# se determinó que las variables de Ocupados, PEA y PET no presentan valores atípicos. 
# Los datos muestran distribuciones contenidas dentro de los límites esperados, 
# lo que permite trabajar con los promedios y sumatorias de la muestra sin riesgo de distorsiones por valores extremos.
# 
# Por el contrario, las variables de Desocupados e Inactivos exhiben valores atípicos 
# y un sesgo positivo marcado. Esto implica que el uso de promedios simples en estas 
# categorías podría conducir a conclusiones erróneas al estar influenciados por 
# casos extremos. Para mitigar este riesgo, el análisis de estas variables 
# se debe complementar con el uso de medianas y técnicas de normalización, 
# asegurando que las interpretaciones reflejen con precisión la estructura 
# del mercado laboral sin sesgos informativos.

#---------------------------------------------
# 2.4 Analizar la calidad y la consistencia
  
# 2.5 Validar si PEA = Ocupados + Desocupados y si PET = Ocupados + Desocupados + Inactivos
  
# 2.5.1 Empleo Edad

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






# 2.5.2 Empleo Genero

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



# 4.1 BRECHA DE EDAD

# 4.1.1. PREPARACIÓN: Aseguramos que los datos estén ordenados por año
# Esto es vital para que las líneas de trayectoria sigan el orden cronológico
empleo_edad_pivot <- empleo_edad_resumen %>%
  select(Año, Grupos_edad, tasa_actividad, tasa_desempleo) %>%
  pivot_longer(
    cols = c(tasa_actividad, tasa_desempleo), 
    names_to = "Indicador", 
    values_to = "Valor"
  )


# 4.1.2. DEFINICIÓN DE COLORES 
colores_kpi <- c(
  "tasa_actividad" = "#2E7D32", # Verde bosque
  "tasa_desempleo" = "#C62828"  # Rojo intenso
)



# 3. CONSTRUCCIÓN DEL GRÁFICO
ggplot(empleo_edad_pivot, aes(x = Año, y = Valor, color = Indicador)) +
  
  # Líneas de tendencia y puntos de medición
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  
  # FACETAS: Creamos un mini-gráfico para cada grupo de edad
  # scales = "free_y" permitiría que cada uno tenga su escala, 
  # pero para BI Senior es mejor mantener la misma escala para comparar magnitudes reales.
  facet_wrap(~Grupos_edad, ncol = 2) + 
  
  # ETIQUETAS DE DATOS: Solo en años clave para no ensuciar el gráfico
  geom_text(data = empleo_edad_pivot %>% filter(Año %in% c(2012, 2020, 2025)),
            aes(label = paste0(round(Valor, 1), "%")),
            vjust = -1, size = 3, fontface = "bold", show.legend = FALSE) +
  
  # TÍTULOS Y TEXTOS
  labs(
    title = "ANÁLISIS ESTRUCTURAL DEL MERCADO LABORAL POR EDAD",
    subtitle = "Honduras 2012-2025: Comparativa entre Participación Activa y Desempleo Abierto",
    x = "Año de la Encuesta",
    y = "Tasa Porcentual (%)",
    color = "Indicador",
    caption = "Fuente: INE Honduras | La brecha entre líneas representa la población ocupada dentro de la PEA."
  ) +
  
  # CONFIGURACIÓN DE ESCALAS
  scale_color_manual(
    values = colores_kpi,
    labels = c("tasa_actividad" = "Tasa de Actividad (PEA)", "tasa_desempleo" = "Tasa de Desempleo")
  ) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2025)) +
  
  # TEMA PROFESIONAL DE ALTA LECTURA
  theme_minimal(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#263238"), # Fondo oscuro para los títulos de cada panel
    strip.text = element_text(color = "white", face = "bold"),
    plot.title = element_text(face = "bold", size = 16, color = "#1A237E"),
    panel.spacing = unit(1.5, "lines"), # Espacio entre paneles
    legend.position = "top",
    panel.grid.minor = element_blank()
  )








#--------------------

# 4.2 BRECHA DE GENERO Y DINAMICA LABORAL

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



ggplot(empleo_genero_pivot, aes(x = Año, y = Valor, color = Indicador, group = Indicador)) +

  # 1. GEOMETRÍAS: Líneas y Puntos
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
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "#555555", face = "italic", hjust = 1),

    # --- ETIQUETAS DE LOS EJES (X e Y) ---
    axis.title.x = element_text(face = "bold", size = 14, color = "darkgrey"),
    axis.title.y = element_text(face = "bold", size = 14, color = "darkgrey"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),   # Inclinamos los años para que no choquen
    axis.text.y = element_text(size = 10, color = "black"),

    # --- CONFIGURACIÓN DE LA LEYENDA ---
    legend.position = "top",                                                         # Coloca la leyenda arriba
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "white", color = "lightgrey"),

    # Limpieza visual
    panel.grid.minor = element_blank()
  )





# 4.2. Metricas Empleo en Honduras 


# 4.2.1 PET vs PET  


# Primero se agrupan los datos por año y sumar las poblaciones y 
# luego se pivotean los datos (Convertir de formato ancho a largo)

empleo_honduras_pet_vs_pea <-  empleo_edad_resumen %>% 
  group_by(Año) %>% 
  summarise(
    PET = sum(PET, na.rm = TRUE),
    PEA = sum(PEA, na.rm = TRUE)
  ) %>% 
  pivot_longer(cols = c(PET, PEA), names_to = "Categoria", values_to = "Valor") %>% 
  mutate(Categoria = factor(Categoria, levels = c("PET", "PEA")))      # Forzamos el orden para que PET aparezca primero (izquierda)




# Grafica Barras comparativa entre PEA y PET

ggplot(empleo_honduras_pet_vs_pea, aes(x = factor(Año), y = Valor, fill = Categoria)) +
  
  # 1. Dibujar las barras
  geom_col(position = position_dodge(width = 0.9)) +    # position_dodge(width = 0.9) separa las barras de PET y PEA una al lado de la otra.
  
  # 2. ETIQUETAS: Agrega las etiquetas de datos (números) dentro de las barras.
  geom_text(aes(label = scales::comma(Valor)), 
            position = position_dodge(width = 0.9),     # Alinea el texto con el centro de cada barra                       
            vjust = 0.5,                                # Centrado vertical respecto al punto de anclaje
            hjust = 1.2,                                # Empuja el texto hacia el interior (abajo) de la barra
            size = 3.5,                                 # Tamaño de la fuente de la etiqueta  
            color = "white",                            # Color blanco para contraste sobre gris/azul
            angle = 90,                                 # Rotación de 90 grados para lectura vertical
            fontface = "bold") +                        # Resalta el número en negrita
  
  
  # 3. Define colores personalizados y nombres exactos para la leyenda.
  scale_fill_manual(values = c("PET" = "#A9A9A9",                # Gris para PET
                               "PEA" = "#5DADE2"),               # Azul para PEA
                    labels = c("PET" = "Población en Edad de Trabajar (PET)", 
                               "PEA" = "Población Económicamente Activa (PEA)")) +
  
  
  # 4. Escalas y etiquetas
  # Formatea el eje Y con comas y da un 10% de espacio extra arriba
  # para evitar que las barras o etiquetas toquen el borde del área de trazado.
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +
  
 
  
  # 5. TEXTOS Y TÍTULOS
  # Define el título (usando \n para salto de línea), etiquetas de ejes y quita el título de la leyenda.
  
  labs(title = "Gráfico comparativo entre la población apta para trabajar\ny de quienes son económicamente activos en Honduras",
       subtitle = "Honduras: Periodo 2012 - 2025",
       caption = "Fuente: Instituto Nacional de Estadística (INE)",
       x = "Año",
       y = "Población",
       fill = "") +
  
  
  
  # 6. Tema profesional de BI
  theme_minimal() +                                  # Aplica un estilo limpio con fondo blanco y rejillas sutiles
  theme(
    
    # -----------FORMATO A LOS TITULOS ---------------------
    # Centra el título (hjust = 0.5), lo pone en negrita y añade margen inferior (b = 20) 
    # para que no choque visualmente con las barras del gráfico.
    
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "darkblue"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "#555555", face = "italic", hjust = 1),
    
    
    # --- ETIQUETAS DE LOS EJES (X e Y) ---
    axis.title.x = element_text(face = "bold", size = 14, color = "darkgrey"),
    axis.title.y = element_text(face = "bold", size = 14, color = "darkgrey"),
    
    # --- CONFIGURACIÓN DE LA LEYENDA ---
    legend.position = "bottom",                      # Posiciona la leyenda en la parte inferior para maximizar el área de visualización.
    
    
    # -------- Limpieza visual ------------
    panel.grid.major.x = element_blank()             # Elimina las líneas verticales de la cuadrícula para un look más moderno y enfocado en las barras.
  )







# 4.2.2. Dinámica laboral en Honduras


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
  
  # 1. GEOMETRÍAS: Líneas y Puntos
  geom_line(size = 1.2, alpha = 0.8) +                      # Dibuja las líneas de tendencia
  geom_point(size = 2) +                                    # Marca los puntos exactos de medición
  
  

  
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
      "tasa_actividad" = "Participación Activa",
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
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "white", color = "lightgrey"),
    
    # Limpieza visual
    panel.grid.minor = element_blank()
  )











