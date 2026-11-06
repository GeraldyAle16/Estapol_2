# =============================================================================
# DURACIÓN DE LAS PROTESTAS POR PROVINCIA (en días)
# Usa la base limpia: acciones_colectivas_limpia.xlsx
# =============================================================================
# Idea: la unidad deja de ser el EVENTO y pasa a ser el DÍA.
#   1. provincia-día : una fila por provincia y día con protesta
#   2. episodios     : rachas de días consecutivos con protesta
#   3. resumen       : indicadores de duración por provincia
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(writexl)
})

# Días sin protesta que se toleran dentro de un mismo episodio.
# 1 = solo días consecutivos (10, 11, 12 ene -> 1 episodio; 10 y 12 ene -> 2).
# 2 = se permite 1 día de pausa (10 y 12 ene -> mismo episodio), etc.
SALTO_MAXIMO <- 1

bd <- read_excel("acciones_colectivas_limpia.xlsx", sheet = "datos") |>
  mutate(fecha = as.Date(fecha))

# Eventos sin provincia: cuando solo se registra la región, el evento suele
# referirse a la capital regional (p. ej. región Puno -> provincia Puno).
# Se les asigna la provincia capital y se marcan con provincia_imputada = 1.
capitales <- c(
  "Amazonas" = "Chachapoyas",     "Áncash"        = "Huaraz",
  "Apurímac" = "Abancay",         "Arequipa"      = "Arequipa",
  "Ayacucho" = "Huamanga",        "Cajamarca"     = "Cajamarca",
  "Callao"   = "Callao",          "Cusco"         = "Cusco",
  "Huancavelica" = "Huancavelica","Huánuco"       = "Huánuco",
  "Ica"      = "Ica",             "Junín"         = "Huancayo",
  "La Libertad" = "Trujillo",     "Lambayeque"    = "Chiclayo",
  "Lima"     = "Lima Metropolitana", "Loreto"     = "Maynas",
  "Madre de Dios" = "Tambopata",  "Moquegua"      = "Mariscal Nieto",
  "Pasco"    = "Pasco",           "Piura"         = "Piura",
  "Puno"     = "Puno",            "San Martín"    = "Moyobamba",
  "Tacna"    = "Tacna",           "Tumbes"        = "Tumbes",
  "Ucayali"  = "Coronel Portillo"
)
stopifnot("Hay regiones sin capital definida" = all(unique(bd$region) %in% names(capitales)))

bd <- bd |>
  mutate(provincia_imputada = as.integer(provincia == "No especificado"),
         provincia = if_else(provincia_imputada == 1, unname(capitales[region]), provincia))

sin_prov <- sum(bd$provincia_imputada)
cat("Eventos con provincia asignada a la capital regional:", sin_prov, "\n")
print(as.data.frame(bd |> filter(provincia_imputada == 1) |> count(region, provincia)))

# ---- 1. Base provincia-día ---------------------------------------------------
# Se agrupa por región + provincia (por si dos regiones tuvieran una provincia
# con el mismo nombre).
prov_dia <- bd |>
  group_by(region, provincia, fecha) |>
  summarise(
    n_eventos       = n(),
    n_imputados     = sum(provincia_imputada),
    tipos_de_hecho  = paste(sort(unique(hecho)), collapse = "; "),
    .groups = "drop"
  ) |>
  arrange(region, provincia, fecha)

# ---- 2. Episodios (rachas de días consecutivos) -----------------------------
# Se compara cada día con el anterior de la misma provincia: si la diferencia
# supera SALTO_MAXIMO, empieza un episodio nuevo. cumsum() numera los episodios.
prov_dia <- prov_dia |>
  group_by(region, provincia) |>
  mutate(
    dias_desde_anterior = as.integer(fecha - lag(fecha)),
    nuevo_episodio      = is.na(dias_desde_anterior) | dias_desde_anterior > SALTO_MAXIMO,
    episodio            = cumsum(nuevo_episodio)
  ) |>
  ungroup()

episodios <- prov_dia |>
  group_by(region, provincia, episodio) |>
  summarise(
    inicio            = min(fecha),
    fin               = max(fecha),
    duracion_dias     = as.integer(fin - inicio) + 1,   # incluye inicio y fin
    dias_con_protesta = n(),                            # = duración si SALTO_MAXIMO = 1
    n_eventos         = sum(n_eventos),
    .groups = "drop"
  )

# ---- 3. Resumen por provincia ------------------------------------------------
resumen <- episodios |>
  group_by(region, provincia) |>
  summarise(
    dias_con_protesta       = sum(dias_con_protesta),
    n_episodios             = n(),
    episodio_mas_largo_dias = max(duracion_dias),
    inicio_episodio_largo   = inicio[which.max(duracion_dias)],
    fin_episodio_largo      = fin[which.max(duracion_dias)],
    primera_protesta        = min(inicio),
    ultima_protesta         = max(fin),
    .groups = "drop"
  ) |>
  arrange(desc(dias_con_protesta))

cat("\nProvincias analizadas:", nrow(resumen), "\n")
cat("Top 10 provincias por días con protesta:\n")
print(as.data.frame(head(resumen, 10)))

# ---- Exportación ------------------------------------------------------------
diccionario <- tibble::tribble(
  ~hoja,       ~variable,                 ~descripcion,
  "resumen",   "dias_con_protesta",       "Número de días distintos con al menos un evento",
  "resumen",   "n_episodios",             "Número de rachas de días consecutivos con protesta",
  "resumen",   "episodio_mas_largo_dias", "Duración (días) de la racha más larga",
  "resumen",   "inicio_episodio_largo",   "Fecha de inicio del episodio más largo",
  "resumen",   "fin_episodio_largo",      "Fecha de fin del episodio más largo",
  "resumen",   "primera_protesta",        "Primer día con protesta en la provincia",
  "resumen",   "ultima_protesta",         "Último día con protesta en la provincia",
  "episodios", "duracion_dias",           "Días entre inicio y fin del episodio (ambos incluidos)",
  "episodios", "dias_con_protesta",       "Días con eventos dentro del episodio",
  "provincia_dia", "n_eventos",           "Eventos ocurridos ese día en esa provincia",
  "provincia_dia", "episodio",            "Número de episodio al que pertenece el día",
  "provincia_dia", "n_imputados",         "Eventos de ese día cuya provincia se asignó a la capital regional",
  "nota",      "SALTO_MAXIMO",            paste("Días consecutivos usados para formar episodios:", SALTO_MAXIMO),
  "nota",      "imputados",               paste(sin_prov, "eventos sin provincia se asignaron a la provincia capital de su región")
)

write_xlsx(list(resumen_provincia = resumen,
                episodios         = episodios,
                provincia_dia     = prov_dia |> select(-nuevo_episodio),
                diccionario       = diccionario),
           "duracion_protestas_provincia.xlsx")
cat("\nArchivo guardado: duracion_protestas_provincia.xlsx\n")
