# =============================================================================
# LIMPIEZA DE BASE: Acciones colectivas y enfrentamientos por día (nivel provincial)
# Periodo: dic-2022 a may-2024
# Autora: Geraldy Rojas López
# =============================================================================
# Pasos:
#   0. Configuración y carga
#   1. Nombres de columnas limpios (snake_case, sin tildes)
#   2. Tipos de datos (fechas como Date)
#   3. Espacios y codificación de texto
#   4. Valores faltantes disfrazados ("-", "", "1.0", ...)
#   5. Unificación de categorías escritas de distintas formas
#   6. Variable nueva: categoría agrupada de hecho
#   7. Duplicados (identificación y tratamiento)
#   8. Variables sin información (constantes / casi vacías)
#   9. Tratamiento final de NA
#  10. Verificación y exportación
# =============================================================================

# ---- 0. Configuración y carga ------------------------------------------------
# install.packages(c("readxl", "dplyr", "stringr", "stringi", "writexl"))
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(stringi)
  library(writexl)
})

# ¿Eliminar filas repetidas? Ver explicación en el paso 7.
# FALSE (recomendado) = se conservan y se numeran como eventos distintos.
ELIMINAR_DUPLICADOS <- FALSE

# Se busca el archivo por patrón porque la tilde de "día" puede venir con
# otra codificación Unicode y fallar si se escribe el nombre a mano.
archivo <- list.files(pattern = "^Acciones.*\\.xlsx$")[1]
base_original <- read_excel(archivo, sheet = "Hoja 1")
diccionario   <- read_excel(archivo, sheet = "Diccionario", col_names = FALSE,
                            .name_repair = "minimal")
names(diccionario) <- c("variable", "descripcion")   # la 1ra fila NO era encabezado

cat("Base original:", nrow(base_original), "filas x", ncol(base_original), "columnas\n")
cat("NA por columna (original):\n"); print(colSums(is.na(base_original)))

bd <- base_original

# ---- 1. Nombres de columnas --------------------------------------------------
limpiar_nombres <- function(x) {
  x |>
    stri_trans_general("Latin-ASCII") |>   # Región -> Region
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_remove_all("^_|_$")
}
names(bd) <- limpiar_nombres(names(bd))
# fecha, region, provincia, distrito, hecho, detalle, fecha_de_actualizacion

# ---- 2. Tipos de datos -------------------------------------------------------
# Vienen como fecha-hora (POSIXct) aunque no tienen hora: se pasan a Date.
bd <- bd |>
  mutate(fecha                  = as.Date(fecha),
         fecha_de_actualizacion = as.Date(fecha_de_actualizacion))

# ---- 3. Espacios y codificación ----------------------------------------------
# - stri_trans_nfc(): unifica la codificación de las tildes (evita que
#   "Puno" y "Puno" con tilde compuesta se vean iguales pero sean distintos).
# - str_squish(): quita espacios al inicio/fin y espacios dobles.
bd <- bd |>
  mutate(across(where(is.character), \(x) str_squish(stri_trans_nfc(x))))

# ---- 4. Faltantes disfrazados ------------------------------------------------
# "-" o "" significan "sin dato" -> se convierten en NA real.
bd <- bd |>
  mutate(across(where(is.character), \(x) if_else(x %in% c("-", "", "NA", "N/A"), NA_character_, x)))

# En "detalle", los valores "1.0", "2.0", "3.0"... NO son descripciones:
# son un contador de eventos repetidos el mismo día y lugar. Se guardan en
# una columna numérica y se borran de "detalle".
bd <- bd |>
  mutate(n_evento_detalle = if_else(str_detect(detalle, "^[0-9]+(\\.0)?$"),
                                    suppressWarnings(as.integer(as.numeric(detalle))), NA_integer_),
         detalle = if_else(!is.na(n_evento_detalle), NA_character_, detalle))

# ---- 5. Unificación de categorías --------------------------------------------
# 5a. Mayúsculas en nombres propios: "De", "Del", "Y" van en minúscula
#     ("Cercado De Lima" -> "Cercado de Lima", "Madre De Dios" -> "Madre de Dios").
minusculas_conectores <- function(x) {
  str_replace_all(x, "(?<=\\s)(De|Del|La|Las|Los|Y|E)(?=\\s)", \(m) str_to_lower(m))
}
bd <- bd |>
  mutate(across(c(region, provincia, distrito), minusculas_conectores))

# 5b. Correcciones puntuales de ortografía (forma oficial INEI/Ubigeo)
bd <- bd |>
  mutate(
    provincia = recode(provincia,
                       "Paucar del Sara Sara" = "Páucar del Sara Sara"),
    distrito  = recode(distrito,
                       "Acora"                      = "Ácora",
                       "Ayavirí"                    = "Ayaviri",
                       "Desagüadero"                = "Desaguadero",
                       "Juanjui"                    = "Juanjuí",
                       "Wánchaq"                    = "Wanchaq",
                       "Santa Anita District, Lima" = "Santa Anita",
                       "Lurigancho - Chosica"       = "Lurigancho")
  )

# 5c. Región: la base mezcla "Lima" y "Lima Metropolitana" como región para la
#     MISMA provincia (Lima Metropolitana). Se usa el departamento oficial
#     "Lima"; la distinción Lima Metropolitana sigue en la columna provincia.
bd <- bd |>
  mutate(region = if_else(region == "Lima Metropolitana", "Lima", region))

# 5d. Tipo de hecho: mayúsculas inconsistentes y variantes de una misma acción
bd <- bd |>
  mutate(
    hecho = str_to_sentence(hecho),                       # "PAro" -> "Paro"
    hecho = str_replace(hecho, "\\bpnp\\b", "PNP"),       # sigla en mayúscula
    hecho = str_replace(hecho, "\\blima\\b", "Lima"),
    hecho = case_when(
      hecho == "Lavado de bandera"                ~ "Lavado de banderas",
      str_detect(hecho, "^Intervención artística") ~ "Intervención artística",
      TRUE ~ hecho
    )
  )

# ---- 6. Categoría agrupada de hecho ------------------------------------------
# 'hecho' tiene ~25 valores, muchos con 1-5 casos. Para análisis se crea una
# categoría más amplia (se conserva 'hecho' con el detalle original).
bd <- bd |>
  mutate(hecho_categoria = case_when(
    str_detect(hecho, "^Paro|^Paralización")                     ~ "Paro",
    str_detect(hecho, "^Movilización|^Marcha|^Pasacalle")        ~ "Movilización / marcha",
    str_detect(hecho, "^Enfrentamiento|^Hecho de violencia")     ~ "Violencia / enfrentamiento",
    str_detect(hecho, paste0("^Concentración|^Plantón|^Vigilia|^Cacerolazo|",
                             "^Banderolazo|^Romería|^Lavado|^Intervención|^Impedimento"))
                                                                 ~ "Concentración / acto simbólico",
    TRUE                                                         ~ "Otro"
  ))

# ---- 7. Duplicados -----------------------------------------------------------
# Hay filas idénticas en todas las columnas. Al revisarlas, coinciden con los
# contadores 1.0, 2.0, ... de 'detalle' (paso 4): son VARIOS eventos del mismo
# tipo, el mismo día y en el mismo lugar (p. ej. 4 movilizaciones en Cercado de
# Lima el 15/12/2022). Si se borran, se pierden eventos reales y se subestima
# la conflictividad. Por eso, por defecto se CONSERVAN y se numeran.
vars_clave <- c("fecha", "region", "provincia", "distrito", "hecho", "detalle")

n_duplicados <- sum(duplicated(bd[vars_clave]))
cat("\nFilas repetidas (mismo día, lugar y hecho):", n_duplicados, "\n")

bd <- bd |>
  group_by(across(all_of(vars_clave))) |>
  mutate(n_evento         = row_number(),   # 1, 2, 3... dentro del grupo
         eventos_mismo_dia = n()) |>        # cuántos hubo en total ese día/lugar
  ungroup()

if (ELIMINAR_DUPLICADOS) {
  bd <- bd |> filter(n_evento == 1)
  cat("Se eliminaron", n_duplicados, "filas repetidas.\n")
}

# ---- 8. Variables sin información --------------------------------------------
# fecha_de_actualizacion: el mismo valor en todas las filas -> no aporta.
# n_evento_detalle: ya está contenido en n_evento -> redundante.
fecha_corte <- unique(bd$fecha_de_actualizacion)
cat("Fecha de actualización (única):", format(fecha_corte), "-> se elimina la columna\n")
bd <- bd |> select(-fecha_de_actualizacion, -n_evento_detalle)

# ---- 9. Tratamiento final de NA ----------------------------------------------
# Ninguna fila se elimina por NA: el hecho sí ocurrió, solo falta el detalle
# geográfico o descriptivo. Se reemplaza el NA por una categoría explícita.
#  - provincia / distrito: el evento se registró a nivel regional/provincial.
#  - detalle: ~96 % vacío; la mayoría de hechos no tiene descripción adicional.
bd <- bd |>
  mutate(
    provincia = coalesce(provincia, "No especificado"),
    distrito  = coalesce(distrito,  "No especificado"),
    detalle   = coalesce(detalle,   "Sin detalle")
  )

# ---- 10. Verificación y exportación ------------------------------------------
bd <- bd |>
  arrange(fecha, region, provincia, distrito, hecho, n_evento) |>
  mutate(id = row_number(), .before = 1) |>
  select(id, fecha, region, provincia, distrito, hecho, hecho_categoria,
         detalle, n_evento, eventos_mismo_dia)

cat("\n================ VERIFICACIÓN ================\n")
cat("Filas:", nrow(bd), " | Columnas:", ncol(bd), "\n")
cat("NA totales:", sum(is.na(bd)), "\n")
cat("Rango de fechas:", format(min(bd$fecha)), "a", format(max(bd$fecha)), "\n")
cat("Regiones:", n_distinct(bd$region), " | Provincias:", n_distinct(bd$provincia),
    " | Distritos:", n_distinct(bd$distrito), " | Tipos de hecho:", n_distinct(bd$hecho), "\n")

# Comprobación: ya no deben quedar variantes que solo difieren en tildes/mayúsculas
clave <- function(x) stri_trans_general(str_to_lower(x), "Latin-ASCII")
for (v in c("region", "provincia", "distrito", "hecho")) {
  vals <- unique(bd[[v]])
  stopifnot("Quedan variantes de escritura" = !anyDuplicated(clave(vals)))
}
cat("OK: no quedan variantes de escritura en region/provincia/distrito/hecho\n")

cat("\nEventos por categoría:\n"); print(table(bd$hecho_categoria))

# Diccionario actualizado
diccionario_limpio <- tibble::tribble(
  ~variable,           ~descripcion,
  "id",                "Identificador único de la fila (evento)",
  "fecha",             "Fecha en que se registra la acción colectiva o enfrentamiento",
  "region",            "Departamento donde ocurre (Lima incluye Lima Metropolitana)",
  "provincia",         "Provincia donde ocurre ('No especificado' si no se registró)",
  "distrito",          "Distrito donde ocurre ('No especificado' si no se registró)",
  "hecho",             "Tipo de acción colectiva o enfrentamiento (estandarizado)",
  "hecho_categoria",   "Agrupación del hecho: Movilización/marcha, Concentración/acto simbólico, Paro, Violencia/enfrentamiento",
  "detalle",           "Detalle del hecho ('Sin detalle' si no se registró)",
  "n_evento",          "Número del evento entre los del mismo día, lugar y tipo (1, 2, 3...)",
  "eventos_mismo_dia", "Total de eventos del mismo tipo ese día en ese lugar"
)

write.csv(bd, "acciones_colectivas_limpia.csv", row.names = FALSE, fileEncoding = "UTF-8")
write_xlsx(list(datos = bd, diccionario = diccionario_limpio),
           "acciones_colectivas_limpia.xlsx")
cat("\nArchivos guardados: acciones_colectivas_limpia.csv / .xlsx\n")
