library(haven)
library(dplyr)
library(tidyr)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(geobr)
library(geodata)
library(spdep)
library(viridis)
library(ineq)
library(stringi)


###############################
# IMPORTAMOS LOS DATOS
###############################

datos <- read_sav("C:/Users/elvis/Downloads/ENA_2014_2024/datos.sav", encoding = "latin1", col_select = c("ANIO","CCDD","NOMBREDD","CCPP","NOMBREPV","CCDI","NOMBREDI","CCCP","NOMBRECP","P229G_NOM","P229G_COD","P229H_CANT_ENT","P229H_CANT_DEC","P229H_UM","P229H_UM_COD","P229H_EQUIV","P229I_1_CANT_ENT","P229I_1_CANT_DEC","P229I_PREC_ENT","P229I_PREC_DEC","P229I_VAL","P229I_2_ENT","P229I_2_DEC","P229I_3_ENT","P229I_3_DEC"))

# conversion de variables a numericas

datos <- datos %>%
  mutate(
    P229H_CANT_ENT = as.numeric(P229H_CANT_ENT),
    P229H_CANT_DEC = as.numeric(P229H_CANT_DEC),
    P229I_1_CANT_ENT = as.numeric(P229I_1_CANT_ENT),
    P229I_1_CANT_DEC = as.numeric(P229I_1_CANT_DEC),
    P229I_PREC_ENT = as.numeric(P229I_PREC_ENT),
    P229I_PREC_DEC = as.numeric(P229I_PREC_DEC),
    P229I_2_ENT = as.numeric(P229I_2_ENT),
    P229I_2_DEC = as.numeric(P229I_2_DEC),
    P229I_3_ENT = as.numeric(P229I_3_ENT),
    P229I_3_DEC = as.numeric(P229I_3_DEC)
  )



###########################
# LIMPIESA DE DATOS 
##########################

# verificamos que tan grabe es...
summary(datos$P229H_CANT_ENT)
# ver cuantos Na EXISTEN

sum(is.na(datos$P229H_CANT_ENT))

# en cada variable 

colSums(is.na(datos))

#porcontaje de variables NA

colMeans(is.na(datos)) * 100


# se observo que entre el 98 a 99 porciento de los datos tienen valores NA, por lo mismo se ara un filtrado de datos.

# FILTRAR SOLO DATOS ÚTILES
datos_limpios <- datos %>%
  filter(!is.na(P229H_CANT_ENT))
dim(datos_limpios)   # cuantos datos quedaron

# tenemos un total de 74080 datos listos para el analisis.

# CREACION DE VARIABLES EN LA BASE LIMPIA

datos_limpios <- datos_limpios %>%
  mutate(
    PRODUCCION = P229H_CANT_ENT + coalesce(P229H_CANT_DEC, 0)/100,   # Producción total
    VENTA_CANT = P229I_1_CANT_ENT + coalesce(P229I_1_CANT_DEC, 0)/100, # Venta
    PRECIO = P229I_PREC_ENT + coalesce(P229I_PREC_DEC, 0)/100,       # Precio
    ANIMALES_CANT = P229I_2_ENT + coalesce(P229I_2_DEC, 0)/100,      # Alimento animales
    OTROS_CANT = P229I_3_ENT + coalesce(P229I_3_DEC, 0)/100          # Otros
  )

datos_limpios %>%
  select(PRODUCCION, VENTA_CANT, PRECIO, ANIMALES_CANT, OTROS_CANT) %>%
  head()


#verificamos 
colSums(is.na(datos_limpios))

#Análisis de venta
datos_venta <- datos_limpios %>%
  filter(!is.na(P229I_PREC_ENT))

dim(datos_venta)   # ver cuantos quedan

#################################### 
# Se observa que una gran proporción de la producción no se destina a la venta, lo que evidencia un alto nivel de autoconsumo en las unidades agropecuarias.”
# Solo ~9.5% (7050 de 74080) de la producción se vende
####################################


summary(datos_limpios$PRODUCCION)
# se observa como la mediana es 15 , pero el maximo valor es de 720000.0,  siendo un aparente valor outlayer, que afecta ala mediana 
# por lo mismo veremos que valores tienen valores grandes

datos_limpios %>%
  arrange(desc(PRODUCCION)) %>%
  select(PRODUCCION, P229G_NOM, NOMBREDD) %>%
  head(10)

#para un mejor entendimiento veremos la distribucion de los valores

hist(datos_limpios$PRODUCCION)
boxplot(datos_limpios$PRODUCCION)

#podemos ver como fuertemente esta sesgado ala derecha 

quantile(datos_limpios$PRODUCCION, 0.99, na.rm = TRUE)

#APLICAREMOS RECORTE (WINSORIZACION)
datos_limpios <- datos_limpios %>%
  mutate(
    PRODUCCION_LIM = ifelse(PRODUCCION > 5000, 5000, PRODUCCION)
  )

# VERIFICAR
summary(datos_limpios$PRODUCCION_LIM)

# COMPARAR
summary(datos_limpios$PRODUCCION)
summary(datos_limpios$PRODUCCION_LIM)

# Se identificaron valores extremos en la producción, los cuales generaban una sobreestimación del promedio. Por ello, se aplicó un recorte al percentil 99, logrando una distribución más representativa de la realidad



##########################################
#   ANALISIS 
########################################## 

#filtro ppr año

datos_limpios %>%
  group_by(ANIO) %>%
  summarise(total_registros = n())

# grafico


datos_limpios%>%
  group_by(ANIO) %>%
  summarise(total_registros = n()) %>%
  ggplot(aes(x = ANIO, y = total_registros)) +
  geom_col() +
  labs(
    title = "Cantidad de registros por año",
    x = "Año",
    y = "Número de registros"
  )

# ¿Qué departamentos tienen mayor producción?

# MAPA DEL PERU

peru_dep <- geodata::gadm(country = "PER", level = 1, path = tempdir())
names(peru_dep)
plot(peru_dep)

# FILTRAR AÑO
datos_2015 <- datos_limpios %>%
  filter(ANIO == 2015)


# AGRUPAR POR DEPARTAMENTO
datos_dep_2015 <- datos_2015 %>%
  group_by(NOMBREDD) %>%
  summarise(
    produccion_total = sum(PRODUCCION_LIM, na.rm = TRUE)
  ) %>%
  arrange(desc(produccion_total))
head(datos_dep_2015)

# ARREGLAR NOMBRES
datos_dep_2015 <- datos_dep_2015 %>%
  mutate(NOMBREDD = toupper(NOMBREDD))

peru_dep$NAME_1 <- toupper(peru_dep$NAME_1)

# UNIR DATOS
peru_dep_sf <- st_as_sf(peru_dep)
mapa_final <- peru_dep_sf %>%
  left_join(datos_dep_2015, by = c("NAME_1" = "NOMBREDD"))

# grafico 
ggplot(mapa_final) +
  geom_sf(aes(fill = produccion_total)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(
    title = "Producción por departamento (2015)",
    fill = "Producción"
  ) +
  theme_minimal()

# ranquin de departamentos 

datos_dep_2015 %>%
  arrange(desc(produccion_total)) %>%
  head(10)


ggplot(mapa_final) +
  geom_sf(aes(fill = produccion_total), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "C",
    trans = "sqrt",   # 🔥 mejora visual cuando hay sesgo
    na.value = "grey90"
  ) +
  labs(
    title = "Producción agropecuaria por departamento (Perú, 2015)",
    fill = "Producción"
  ) +
  theme_minimal()


#########################################################
# RELACIÓN PRODUCCIÓN vs VENTA
#########################################################

# Gráfico de dispersión
ggplot(datos_rel, aes(x = PRODUCCION, y = VENTA_CANT)) +
  geom_point(alpha = 0.3, color = "blue") +  # puntos semitransparentes
  scale_x_log10() +  # escala log para mejorar visualización (no altera datos)
  labs(
    title = "Relación entre Producción y Venta",
    x = "Producción",
    y = "Cantidad Vendida"
  ) +
  theme_minimal()

# Interpretación esperada:
# - Muchos puntos con venta baja → autoconsumo
# - No toda la producción se vende


#########################################################
# RELACIÓN PRODUCCIÓN vs PRECIO
#########################################################

ggplot(datos_rel, aes(x = PRODUCCION, y = PRECIO)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  scale_x_log10() +
  labs(
    title = "Relación entre Producción y Precio",
    x = "Producción",
    y = "Precio"
  ) +
  theme_minimal()

# Interpretación esperada:
# - Alta dispersión
# - No hay relación lineal clara


#########################################################
# RELACIÓN PRECIO vs VENTA
#########################################################

ggplot(datos_rel, aes(x = PRECIO, y = VENTA_CANT)) +
  geom_point(alpha = 0.3, color = "purple") +
  labs(
    title = "Relación entre Precio y Venta",
    x = "Precio",
    y = "Cantidad Vendida"
  ) +
  theme_minimal()

# Interpretación esperada:
# - No se observa patrón claro
# - Precio no determina totalmente la venta


#########################################################
# PROPORCIÓN DE PRODUCCIÓN DESTINADA A LA VENTA
#########################################################

datos_rel <- datos_rel %>%
  filter(ratio_venta <= 1)
summary(datos_rel$ratio_venta)

# Crear variable ratio (proporción)
datos_rel <- datos_rel %>%
  mutate(
    ratio_venta = VENTA_CANT / PRODUCCION
  )

# Revisar valores
summary(datos_rel$ratio_venta)

# Histograma de proporción
ggplot(datos_rel, aes(x = ratio_venta)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(
    title = "Proporción de Producción destinada a la Venta",
    x = "Proporción",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Interpretación:
# - Valores cercanos a 0 → autoconsumo
# - Valores cercanos a 1 → producción orientada al mercado


#########################################################
# DESTINO TOTAL DE LA PRODUCCIÓN
#########################################################

# Suma total por tipo de uso
destino <- datos_rel %>%
  summarise(
    venta = sum(VENTA_CANT, na.rm = TRUE),
    animales = sum(ANIMALES_CANT, na.rm = TRUE),
    otros = sum(OTROS_CANT, na.rm = TRUE)
  )

destino

# Convertir a formato largo para gráfico
destino_long <- destino %>%
  pivot_longer(cols = everything(),
               names_to = "tipo",
               values_to = "cantidad")

# Gráfico de barras
ggplot(destino_long, aes(x = tipo, y = cantidad, fill = tipo)) +
  geom_col() +
  labs(
    title = "Destino de la Producción",
    x = "Tipo de uso",
    y = "Cantidad total"
  ) +
  theme_minimal()

# Interpretación:
# - Permite ver cuánto se vende vs autoconsumo


#########################################################
# PARTICIPACIÓN PORCENTUAL
#########################################################

destino_long <- destino_long %>%
  mutate(
    porcentaje = cantidad / sum(cantidad) * 100
  )

destino_long


#####################################################
#  ANALISIS ESPACIAL 
###############################################

#datos por departamento 
datos_dep <- datos_limpios %>%
  group_by(NOMBREDD) %>%
  summarise(
    produccion = sum(PRODUCCION_LIM, na.rm = TRUE),
    precio = mean(PRECIO, na.rm = TRUE),
    venta = sum(VENTA_CANT, na.rm = TRUE)
  )
# Unir con el mapa 

peru_dep_sf <- st_as_sf(peru_dep)

mapa_final <- peru_dep_sf %>%
  left_join(datos_dep, by = c("NAME_1" = "NOMBREDD"))
# MAPA COROPLÉTICO

ggplot(mapa_final) +
  geom_sf(aes(fill = produccion), color = "white") +
  scale_fill_viridis_c(trans = "sqrt") +
  labs(
    title = "Producción por departamento de todos los años",
    fill = "Producción"
  ) +
  theme_minimal()

# analisis de ventas y consumo 

# Agrupar todos los años por departamento
datos_dep_uso <- datos_limpios %>%
  group_by(NOMBREDD) %>%
  summarise(
    produccion = sum(PRODUCCION, na.rm = TRUE),
    venta = sum(VENTA_CANT, na.rm = TRUE),
    consumo = sum(ANIMALES_CANT + OTROS_CANT, na.rm = TRUE)
  ) %>%
  mutate(
    ratio_venta = venta / produccion,
    ratio_consumo = consumo / produccion
  )
datos_dep_uso <- datos_dep_uso %>%
  filter(ratio_venta <= 1)

# unir con mapa
peru_dep_sf <- st_as_sf(peru_dep)

mapa_uso <- peru_dep_sf %>%
  left_join(datos_dep_uso, by = c("NAME_1" = "NOMBREDD"))

#MAPA: orientación a la venta
ggplot(mapa_uso) +
  geom_sf(aes(fill = ratio_venta), color = "white") +
  scale_fill_viridis_c(option = "C") +
  labs(
    title = "Orientación a la venta por departamento",
    fill = "Ratio venta"
  ) +
  theme_minimal()
#MAPA: orientación al consumo
ggplot(mapa_uso) +
  geom_sf(aes(fill = ratio_consumo), color = "white") +
  scale_fill_viridis_c(option = "B") +
  labs(
    title = "Orientación al consumo por departamento",
    fill = "Ratio consumo"
  ) +
  theme_minimal()
# comparacion
datos_dep_uso %>%
  arrange(desc(ratio_venta)) %>%
  head(5)

datos_dep_uso %>%
  arrange(ratio_venta) %>%
  head(5)

################
#funciol análisis espacial del destino de la producción evidencia una clara diferenciación regional en el Perú. Los departamentos de la costa, como Lima, Ica y Arequipa, presentan una mayor orientación al mercado, destinando una proporción significativa de su producción a la venta.
#En contraste, los departamentos de la selva, como Loreto, Madre de Dios y Ucayali, muestran una fuerte orientación al autoconsumo, con niveles mínimos de comercialización.
#Este patrón sugiere la influencia de factores geográficos, económicos y de acceso a mercados en la estructura productiva de las regiones.
################

#########################################################
# 📊 1. EFICIENCIA COMERCIAL
#########################################################

datos_dep_uso <- datos_dep_uso %>%
  mutate(
    eficiencia = venta / produccion
  )

# Ranking
datos_dep_uso %>%
  arrange(desc(eficiencia)) %>%
  select(NOMBREDD, eficiencia)

#########################################################
# 💰 VALOR ECONÓMICO CORRECTO
#########################################################

library(dplyr)
library(stringi)
library(ggplot2)

# 1. LIMPIAR NOMBRES (ANTES DE TODO)
datos_limpios <- datos_limpios %>%
  mutate(NOMBREDD = stri_trans_general(NOMBREDD, "Latin-ASCII"))

mapa_uso <- mapa_uso %>%
  mutate(NAME_1 = stri_trans_general(NAME_1, "Latin-ASCII"))

# 2. CREAR VALOR A NIVEL INDIVIDUAL
datos_limpios <- datos_limpios %>%
  mutate(
    valor_individual = PRODUCCION * PRECIO
  )

# 3. AGREGAR POR DEPARTAMENTO
datos_dep_valor <- datos_limpios %>%
  group_by(NOMBREDD) %>%
  summarise(
    valor = sum(valor_individual, na.rm = TRUE)
  )

# 4. UNIR AL MAPA (SOLO UNA VEZ)
mapa_uso <- mapa_uso %>%
  left_join(datos_dep_valor, by = c("NAME_1" = "NOMBREDD"))

# 5. VERIFICAR
names(mapa_uso)

# 6. MAPA
ggplot(mapa_uso) +
  geom_sf(aes(fill = valor), color = "white") +
  scale_fill_viridis_c() +
  labs(title = "Valor económico de la producción") +
  theme_minimal()

