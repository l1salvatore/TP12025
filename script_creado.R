library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(reshape2)

datos <- read_excel("/home/lsalvatore/Documents/FACULTAD/ProbabilidadYEstadistica/Prob_Datos_LP.xlsx", col_names = FALSE, skip=3)

datos <- datos |>
      select(   # Seleccionar las columnas que quiero conservar
             "...1", "...2","...5", "...24", "...25", "...26", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...50" 
         )

colnames(datos) <- c("OrdenInicial", # Cuantitativa Discreta
                      "Provincia", # Cualitativa Nominal
                      "TiempoDeResidenciaEnAños", # Cuantitativa Continua
                      "FormaObtencionAgua", # Cualitativa Nominal
                      "AguaPotable", # Cualitativa Nominal Dicotómica
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
datos_base <- datos |>
    mutate(
      AguaPotable = ifelse(AguaPotable == 'No' & !is.na(AguaPotable), 'Si', 'No'),
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
  filter(Provincia == "CABA" | Provincia == "Mendoza" | Provincia == "Río Negro" | Provincia == "Santa Cruz" | Provincia == "Jujuy" | Provincia == "Córdoba" | Provincia == 'Misiones')


datos_base$PresionAgua <- factor(datos_base$PresionAgua, 
                                 levels = c("Muy débil", "Débil", "Buena"), 
                                 ordered = TRUE)
#Tiempo de residencia en años

# Armamos un boxplot, para observar rapidamente un panorama donde se concentra el tiempo de residencia
boxplot(datos_base$TiempoDeResidenciaEnAños, main = "Tiempo de Residencia en Años", ylab = "Tiempo de Residencia en Años", col = "lightblue")
summary(datos_base$TiempoDeResidenciaEnAños)

#SECCION AGUA 
#Forma de obtención de agua
agua <- datos_base
agua$FormaObtencionAgua <- factor(agua$FormaObtencionAgua, levels = names(sort(table(datos_base$FormaObtencionAgua), decreasing = TRUE)))

# --- GRAFICO 1 ------
ggplot(data.frame(agua), aes(x = FormaObtencionAgua)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Forma de Obtención del Agua",
       x = "",
       y = "Cantidad de hogares") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(table(agua$FormaObtencionAgua)), by = 50))
#El agua que consigue, es potable?

# --- GRAFICO 2 ------
pie(table(agua$AguaPotable), col = c("salmon", "lightblue"), # Cambia los colores según las categorías
    main = "El agua es potable?")

# --- GRAFICO 3 ------
pie(table(agua$AguaPotable), col = c("salmon", "lightblue"), # Cambia los colores según las categorías
    main = "El agua es potable?")

#SECCION CALEFACCION Y COCINA
calefaccion <- data.frame(
  GasNatural = datos_base$PoseeGasNaturalParaCalefaccion,
  Garrafa = datos_base$PoseeGarrafaParaCalefaccion,
  Electricidad = datos_base$ElectricidadParaCalefaccion,
  "Leña Carbon" = datos_base$PoseeLeñaCarbonParaCalefaccion,
  "No Tiene" = datos_base$NoTieneParaCalefaccion,
  "No Necesita" = datos_base$NoNecesitaCalefaccionar
)
calefaccionfreq <- colSums(calefaccion[, -1])

#barplot(calefaccionfreq , col = "lightblue",
#        main = "Modos de Calefaccion en los hogares",
#        xlab = "", ylab = "Cantidad de Hogares",
#        names.arg = names(calefaccionfreq))


cocina <- data.frame(
  GasNatural = datos_base$PoseeGasNaturalParaCocina,
  Garrafa = datos_base$PoseeGarrafaParaCocina,
  Electricidad = datos_base$ElectricidadParaCocina,
  "Leña Carbon" = datos_base$PoseeLeñaCarbonParaCocina,
  "No Tiene" = datos_base$NoTieneParaCocina
)
cocinafreq <- colSums(cocina[, -1])
#barplot(cocinafreq , col = "lightblue",
#        main = "Modos de cocina en los hogares",
#        xlab = "", ylab = "Cantidad de Hogares",
#        names.arg = names(cocinafreq))

calefaccion$ID <- 1:nrow(calefaccion)  # Agregar ID para fusionar correctamente
cocina$ID <- 1:nrow(cocina)
 
# Unir tablas por ID
datos_relacion <- merge(calefaccion, cocina, by = "ID", suffixes = c("_Calefaccion", "_Cocina"))

#-----GRAFICO 3: Gas Natural -----
tabla_contingencia <- table(
  calefaccion = factor(datos_relacion$GasNatural_Calefaccion, levels = c(0,1), labels = c("No", "Sí")),
  cocina = factor(datos_relacion$GasNatural_Cocina, levels = c(0,1), labels = c("No", "Sí"))
)
print(tabla_contingencia)

#-----GRAFICO 3: Garrafa -----
tabla_contingencia <- table(
  calefaccion = factor(datos_relacion$Garrafa_Calefaccion, levels = c(0,1), labels = c("No", "Sí")),
  cocina =  factor(datos_relacion$Garrafa_Cocina, levels = c(0,1), labels = c("No", "Sí"))
)

print(tabla_contingencia)

#-----GRAFICO 3: Electricidad -----
tabla_contingencia <- table(
  calefaccion = factor(datos_relacion$Electricidad_Calefaccion, levels = c(0,1), labels = c("No", "Sí")),
  cocina = factor(datos_relacion$Electricidad_Cocina, levels = c(0,1), labels = c("No", "Sí"))
)

print(tabla_contingencia)

#-----GRAFICO 3: LeñaCarbon -----
tabla_contingencia <- table(
  calefaccion = factor(datos_relacion$Leña.Carbon_Calefaccion, levels = c(0,1), labels = c("No", "Sí")),
  cocina = factor(datos_relacion$Leña.Carbon_Cocina, levels = c(0,1), labels = c("No", "Sí"))
)

print(tabla_contingencia)

#-----GRAFICO 3: No Tiene -----
tabla_contingencia <- table(
  calefaccion = factor(datos_relacion$No.Tiene_Calefaccion, levels = c(0,1), labels = c("No", "Sí")),
  cocina = factor(datos_relacion$No.Tiene_Cocina, levels = c(0,1), labels = c("No", "Sí"))
)
print(tabla_contingencia)

