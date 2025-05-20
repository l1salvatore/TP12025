library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(reshape2)

datos <- read_excel("/home/lsalvatore/Documents/FACULTAD/ProbabilidadYEstadistica/Prob_Datos_LP.xlsx", col_names = FALSE, skip=3)

datos <- datos |>
      select(   # Seleccionar las columnas que quiero conservar
             "...1", "...2","...3", "...5", "...6", "...13", "...24", "...25", "...26", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...50" 
         )

colnames(datos) <- c("OrdenInicial", # Cuantitativa Discreta
                      "Provincia", # Cualitativa Nominal
                      "Barrio", # Cualitativa Nominal
                      "TiempoDeResidenciaEnAños", # Cuantitativa Continua
                      "CantidadIntegrantesVivienda", #Cuantitativa Discreta
                      "CantidadDeDormitorios", #Cuantitativa Discreta
                      "FormaObtencionAgua", # Cualitativa Nominal
                      "ConsumeAguaEmbotellada", # Cualitativa Nominal Dicotómica
                      "PresionAgua", # Cualitativa Ordinal
                     
                       #TipoDeCalefaccion -> Cualitativa de respuesta múltiple
                      "PoseeGasNaturalParaCocina", # Cualitativa Dicotómica
                      "PoseeGarrafaParaCocina", # Cualitativa Dicotómica
                      "ElectricidadParaCocina", # Cualitativa Dicotómica
                      "PoseeLeñaCarbonParaCocina",# Cualitativa Dicotómica
                      "NoTieneParaCocina",# Cualitativa Dicotómica
                     
                       #TipoDeCocina -> Cualitativa de respuesta múltiple
                      "PoseeGasNaturalParaCalefaccion", # Cualitativa Dicotómica
                      "PoseeGarrafaParaCalefaccion", # Cualitativa Dicotómica
                      "ElectricidadParaCalefaccion", # Cualitativa Dicotómica
                      "PoseeLeñaCarbonParaCalefaccion",# Cualitativa Dicotómica
                      "NoTieneParaCalefaccion", # Cualitativa Dicotómica
                      "NoNecesitaCalefaccionar",# Cualitativa Dicotómica
                     
                      "TipoConexionElectrica" # Cualitativa Nominal
                     )
datos <- data.frame(datos)
# Transformación
datos_base <- datos |>
    mutate(
      PoseeGasNaturalParaCocina = ifelse(PoseeGasNaturalParaCocina == 'Gas natural (red de gas)' & !is.na(PoseeGasNaturalParaCocina), 1, 0),
      PoseeGarrafaParaCocina = ifelse(PoseeGarrafaParaCocina == 'Gas natural (red de gas)'& !is.na(PoseeGarrafaParaCocina), 1, 0),
      ElectricidadParaCocina = ifelse(ElectricidadParaCocina == 'Electricidad'& !is.na(ElectricidadParaCocina), 1, 0),
      PoseeLeñaCarbonParaCocina = ifelse(PoseeLeñaCarbonParaCocina == 'Leña/Carbón' & !is.na(PoseeLeñaCarbonParaCocina), 1, 0),
      NoTieneParaCocina = ifelse(NoTieneParaCocina == 'No tengo para cocinar en mi vivienda' & !is.na(NoTieneParaCocina), 1, 0),
      PoseeGasNaturalParaCalefaccion = ifelse(PoseeGasNaturalParaCalefaccion == 'Gas natural (red de gas)' & !is.na(PoseeGasNaturalParaCalefaccion), 1, 0),
      PoseeGarrafaParaCalefaccion = ifelse(PoseeGarrafaParaCalefaccion == 'Gas envasado (garrafa)'& !is.na(PoseeGarrafaParaCalefaccion), 1, 0),
      ElectricidadParaCalefaccion = ifelse(ElectricidadParaCalefaccion == 'Electricidad'& !is.na(ElectricidadParaCalefaccion), 1, 0),
      PoseeLeñaCarbonParaCalefaccion = ifelse(PoseeLeñaCarbonParaCalefaccion == 'Leña/Carbón' & !is.na(PoseeLeñaCarbonParaCalefaccion), 1, 0),
      NoTieneParaCalefaccion = ifelse(NoTieneParaCalefaccion == 'No tengo para calefaccionar mi vivienda' & !is.na(NoTieneParaCalefaccion), 1, 0),
      NoNecesitaCalefaccionar = ifelse(NoNecesitaCalefaccionar == 'No necesito calefaccionar mi vivienda en ninguna época del año' & !is.na(NoNecesitaCalefaccionar), 1, 0),
      # Recodifico las etiquetas de una variable categórica
      FormaObtencionAgua = recode(FormaObtencionAgua, "No sabe" = "No sabe",
                       "A través de una conexión con medidor a la red pública" = "Conexión con medidor en red",
                       "A través de una conexión sin medidor, es decir “informalmente”, sea a través de una conexión directa a la red pública o a través de una conexión indirecta a través de un vecinx “informalmente”" = "Conexión a la red sin medidor, informalmente",
                       "A través de un camión cisterna" = "Camión cisterna",
                       "No poseo agua dentro de la vivienda y/o tengo que acarrear desde fuera del terreno en que se ubica mi vivienda"  = "No posee agua, consume agua externa",
                       "A través de un pozo" = "Agua de pozo",
                       "Conexión a un tanque comunitario" = "Tanque comunitario"),
      TipoConexionElectrica = recode(TipoConexionElectrica, "Conexión a través de un medidor a la red eléctrica" = "Con medidor en red",
                                  "Conexión sin medidor a una red eléctrica (“informal”)" = "Sin medidor, informalmente",
                                  "Conexión a través de un medidor comunitario a la red eléctrica" = "Con medidor comunitario",
                                  "No posee conexión a la red eléctrica en la vivienda" = "No tiene acceso a la red eléctrica")
      
       )
datos_base <- datos_base %>%
  filter(Provincia == "Río Negro" | Provincia == "Santa Cruz")
datos_base$PresionAgua <- factor(as.character(datos_base$PresionAgua), 
                                 levels = c("Muy débil", "Débil", "Buena"), 
                                 ordered = TRUE)


# --- GRAFICO 0 ------ #Cantidad de viviendas por provincia, qué estamos analizando
df_tabla <- as.data.frame(table(datos_base$Provincia, datos_base$Barrio))
colnames(df_tabla)<- c("Provincia", "Barrio", "Viviendas")
df_tabla <- subset(df_tabla, Viviendas != 0)
print(df_tabla)

# --- GRAFICO 1 ------ #Tiempo de residencia en años
# Armamos un boxplot, para observar rapidamente un panorama donde se concentra el tiempo de residencia
hist_data <- hist(datos_base$TiempoDeResidenciaEnAños,
     main = "Tiempo de Residencia en Años",
     xlab = "Tiempo de Residencia en Años",
     ylab = "Cantidad de Viviendas",
     col = "lightblue",
     breaks = seq(0, max(datos_base$TiempoDeResidenciaEnAños, na.rm = TRUE) + 5, by = 5),
     border = "white")
lines(hist_data$mids, hist_data$counts, type = "b", col = "blue", lwd = 2, pch = 16)
summary(datos_base$TiempoDeResidenciaEnAños)
sd(datos_base$TiempoDeResidenciaEnAños)
# --- GRAFICO 1.1 ------ #Tiempo de residencia en años por provincia
ggplot(datos_base, aes(x = paste(Barrio, Provincia, sep = " - "), y = TiempoDeResidenciaEnAños)) +
  geom_boxplot(fill = "green") +
  labs(
    title = "Tiempo de residencia por barrio popular",
    x = "Barrio",
    y = "Tiempo de Residencia"
  ) +
  scale_y_continuous(breaks = seq(0, max(datos_base$TiempoDeResidenciaEnAños, na.rm = TRUE), by = 5)) +
  theme_minimal()
summary((datos_base %>% filter(Provincia == 'Río Negro'))$TiempoDeResidenciaEnAños)
summary((datos_base %>% filter(Provincia == 'Santa Cruz'))$TiempoDeResidenciaEnAños)


# ---- GRAFICO 2 ----

ggplot(datos_base, aes(x = factor(CantidadIntegrantesVivienda))) +
  geom_bar(fill = "red", color = "black", width = 0.05) +
  geom_text(
    aes(label = after_stat(count)),  # Usar las frecuencias calculadas
    stat = "count", 
    vjust = -0.5, 
    size = 2
  ) +
  labs(
    title = "Cantidad de viviendas por número de integrantes",
    x = "Número de integrantes",
    y = "Cantidad de viviendas"
  ) +
  theme_minimal()

sd(datos_base$CantidadIntegrantesVivienda)
summary(datos_base$CantidadIntegrantesVivienda)


# ---- GRAFICO 2.1 ----

ggplot(datos_base, aes(x = as.factor(CantidadDeDormitorios), y = CantidadIntegrantesVivienda)) +
  geom_boxplot(fill = "red") +
  labs(
    title = "Relación entre integrantes y cantidad de ambientes usados como dormitorio",
    x = "Cantidad de dormitorios",
    y = "Número de integrantes"
  ) +
  scale_y_continuous(breaks = seq(0, max(datos_base$CantidadIntegrantesVivienda, na.rm = TRUE), by = 1)) +
  theme_minimal()

summary(datos_base$CantidadDeDormitorios)
#SECCION AGUA 
#Forma de obtención de agua
datos_base$FormaObtencionAgua <- factor(datos_base$FormaObtencionAgua, levels = names(sort(table(datos_base$FormaObtencionAgua), decreasing = TRUE)))
datos_base$PresionAgua <- factor(datos_base$PresionAgua, levels = names(sort(table(datos_base$PresionAgua), decreasing = FALSE)))

# --- GRAFICO 3 ------
ggplot(data.frame(datos_base), aes(x = FormaObtencionAgua)) +
  geom_bar(fill = "blue") +
  labs(
    title = "Forma de Obtención del Agua",
    x = "",
    y = "Cantidad de hogares"
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, max(table(datos_base$FormaObtencionAgua)), by = 10)) +
  coord_flip()
# Tabla de frecuencias
freqFormaObtencionAgua <- table(datos_base$FormaObtencionAgua)
round(max(freqFormaObtencionAgua)/sum(freqFormaObtencionAgua) * 100, 2)

#-----GRAFICO 3.1: Agua Potable -----
# Calcular porcentajes por cada FormaObtencionAgua
datos_plot <- datos_base %>%
  group_by(FormaObtencionAgua, ConsumeAguaEmbotellada) %>%
  summarise(Frecuencia = n(), .groups = "drop") %>%
  group_by(FormaObtencionAgua) %>%
  mutate(Prop = Frecuencia / sum(Frecuencia),
         Porcentaje = paste0(round(Prop * 100), "%")) %>%
  ungroup()

# Gráfico de torta con etiquetas de porcentaje
ggplot(datos_plot, aes(x = "", y = Prop, fill = ConsumeAguaEmbotellada)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = Porcentaje),
            position = position_stack(vjust = 0.5), size = 4) +
  coord_polar("y", start = 0) +
  facet_wrap(~ FormaObtencionAgua) +
  labs(title = "Distribución del Consumo de Agua Embotellada",
       fill = "¿Consume Agua Embotellada?") +
  theme_void() +
  theme(strip.text = element_text(face = "bold"))

# --- GRAFICO 4 ------

ggplot(datos_base, aes(x = PresionAgua)) +
  geom_bar(fill = "lightgreen")  +
  geom_text(
    aes(label = after_stat(count)),  # Usar las frecuencias calculadas
    stat = "count", 
    vjust = -0.5, 
    size = 2
  ) +
  labs(title = "Presion del Agua",
       x = "",
       y = "Cantidad de hogares") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(table(datos_base$PresionAgua)), by = 5))

freqPresionAgua <- table(datos_base$PresionAgua)
suma_debil <- freqPresionAgua["Muy débil"] + freqPresionAgua["Débil"]
porcentaje_debil <- round(suma_debil / sum(freqPresionAgua) * 100, 2)
porcentaje_debil
#SECCION CALEFACCION Y COCINA
calefaccion <- data.frame(
  GasNatural = datos_base$PoseeGasNaturalParaCalefaccion,
  Garrafa = datos_base$PoseeGarrafaParaCalefaccion,
  Electricidad = datos_base$ElectricidadParaCalefaccion,
  "Leña Carbon" = datos_base$PoseeLeñaCarbonParaCalefaccion,
  "No Tiene" = datos_base$NoTieneParaCalefaccion,
  "No Necesita" = datos_base$NoNecesitaCalefaccionar
)
calefaccionfreq <- colSums(calefaccion)

# Convertir los datos de frecuencia en un data frame
calefaccion_df <- data.frame(
  Tipo = names(calefaccionfreq),
  Frecuencia = calefaccionfreq
)


# --- GRAFICO 5: Porcentaje de hogares por método de calefacción------
ggplot(calefaccion_df, aes(x = reorder(Tipo, -Frecuencia), y = Frecuencia, fill = Tipo)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de Hogares de acuerdo a los Métodos de Calefacción", 
       x = "Tipo", 
       y = "Frecuencia") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = Frecuencia), vjust = -0.5, color = "black")


cocina <- data.frame(
  GasNatural = datos_base$PoseeGasNaturalParaCocina,
  Garrafa = datos_base$PoseeGarrafaParaCocina,
  Electricidad = datos_base$ElectricidadParaCocina,
  "Leña Carbon" = datos_base$PoseeLeñaCarbonParaCocina,
  "No Tiene" = datos_base$NoTieneParaCocina
)
cocinafreq <- colSums(cocina)

cocina_df <- data.frame(
  Tipo = names(cocinafreq),
  Frecuencia = cocinafreq
)

# --- GRAFICO 6: Porcentaje de hogares por método de cocina------
ggplot(cocina_df, aes(x =  reorder(Tipo, -Frecuencia), y = Frecuencia, fill = Tipo)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de Hogares de acuerdo a los Métodos de Cocina", x = "Tipo", y = "Frecuencia") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = Frecuencia), vjust = -0.5, color = "black")



calefaccion$ID <- 1:nrow(calefaccion)  # Agregar ID para fusionar correctamente
cocina$ID <- 1:nrow(cocina)
 
# Unir tablas por ID
datos_relacion <- merge(calefaccion, cocina, by = "ID", suffixes = c("_Calefaccion", "_Cocina"))

#-----GRAFICO 7.1: Gas Natural -----
tabla_contingencia <- table(
  calefaccion = factor(datos_relacion$GasNatural_Calefaccion, levels = c(0,1), labels = c("No", "Sí")),
  cocina = factor(datos_relacion$GasNatural_Cocina, levels = c(0,1), labels = c("No", "Sí"))
)
print('GAS NATURAL')
print(tabla_contingencia)

#-----GRAFICO 7.2: Garrafa -----
tabla_contingencia <- table(
  calefaccion = factor(datos_relacion$Garrafa_Calefaccion, levels = c(0,1), labels = c("No", "Sí")),
  cocina =  factor(datos_relacion$Garrafa_Cocina, levels = c(0,1), labels = c("No", "Sí"))
)
print('GARRAFA')
print(tabla_contingencia)

#-----GRAFICO 7.3: Electricidad -----
tabla_contingencia <- table(
  calefaccion = factor(datos_relacion$Electricidad_Calefaccion, levels = c(0,1), labels = c("No", "Sí")),
  cocina = factor(datos_relacion$Electricidad_Cocina, levels = c(0,1), labels = c("No", "Sí"))
)
print('ELECTRICIDAD')
print(tabla_contingencia)

#-----GRAFICO 7.4: LeñaCarbon -----
tabla_contingencia <- table(
  calefaccion = factor(datos_relacion$Leña.Carbon_Calefaccion, levels = c(0,1), labels = c("No", "Sí")),
  cocina = factor(datos_relacion$Leña.Carbon_Cocina, levels = c(0,1), labels = c("No", "Sí"))
)
print('LEÑA/CARBON')
print(tabla_contingencia)

# --- GRAFICO 8 ------
datos_base$TipoConexionElectrica <- factor(datos_base$TipoConexionElectrica, levels = names(sort(table(datos_base$TipoConexionElectrica), decreasing = TRUE)))
ggplot(data.frame(datos_base), aes(x = TipoConexionElectrica)) +
  geom_bar(fill = "orange") +
  geom_text(
    aes(label = after_stat(count)),  # Usar las frecuencias calculadas
    stat = "count", 
    vjust = -0.5, 
    size = 4
  ) +
  labs(
    title = "Cantidad de hogares de acuerdo al suministro de energía eléctrica",
    x = "Tipo de conexión eléctrica",
    y = "Cantidad de hogares"
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, max(table(datos_base$TipoConexionElectrica)), by = 10))