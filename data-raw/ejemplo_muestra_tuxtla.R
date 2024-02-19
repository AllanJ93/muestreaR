library(dplyr)
library(sf)
library(leaflet)
library(RColorBrewer)
library(ggmap)
# library(aelectoral2)
library(muestreaR)
library(tidyr)

# library(aelectoral2)

devtools::load_all(path = "../aelectoral2/")
1


# Compu de Allan
wd <- "H:/Shared drives/Morant Consultores/Insumos/INE"

# Compu de Emilio
# wd <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/"
# Insumos -----------------------------------------------------------------

bd <- Electoral$new(eleccion = "pm_21", entidad = "chis", extranjero = F)
bd$agregar_bd("gb_18")
bd$partido(eleccion = "gb_18")
bd$partido(eleccion = "pm_21")
#LISTA NOMINAL INE

ln_re_sexo <- readxl::read_excel(glue::glue("{wd}/Lista Nominal/DatosAbiertos-derfe-pdln_edms_re_20230921.xlsx"))
ln_re_sexo <- ln_re_sexo %>% set_names(gsub(pattern = "\r\n",replacement = " ",x = names(ln_re_sexo)))
ln_chis <- ln_re_sexo %>% filter(`CLAVE ENTIDAD` == 7, SECCION != 0, `NOMBRE MUNICIPIO` == "TUXTLA GUTIERREZ")
# SHAPE FILES INE

mun_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2022/07 CHIAPAS/MUNICIPIO.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

murb_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2022/07 CHIAPAS/MANCHA_URBANA.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

loc_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2022/07 CHIAPAS/LOCALIDAD.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf() #%>%
# mutate(SECCION = glue("{str_pad(ENTIDAD, width = 2, pad = '0')}_{str_pad(SECCION, width = 4, pad = '0')}"))
# loc_shp <- loc_shp %>% mutate(LOCALIDAD = paste(MUNICIPIO, LOCALIDAD))
sec_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2022/07 CHIAPAS/SECCION.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

mza_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2022/07 CHIAPAS/MANZANA.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf() #%>%
# mutate(SECCION = glue("{str_pad(ENTIDAD, width = 2, pad = '0')}_{str_pad(SECCION, width = 4, pad = '0')}"))
# mza_shp <- mza_shp %>% mutate(LOCALIDAD = paste(MUNICIPIO, LOCALIDAD))
dl_shp <- rgdal::readOGR(dsn=glue::glue("{wd}/SHP/2022/07 CHIAPAS/DISTRITO_LOCAL.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

df_shp <- rgdal::readOGR(dsn=glue::glue("{wd}/SHP/2022/07 CHIAPAS/DISTRITO_FEDERAL.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf() |>
  rename(DISTRITO_F = DISTRITO)

base_ine <- read.table(glue::glue("{wd}/Lista Nominal/UT_23_00109_pdln_edmslm_sexo_13012023.txt"),
                       sep = ",",
                       header = TRUE) %>% as_tibble() %>%
  rename(DISTRITO_F = DISTRITO) |>
  filter(ENTIDAD == 7, MUNICIPIO == 102)

mzaSHP <- mza_shp %>%
  filter(MUNICIPIO == 102) |>
  semi_join(
    ln_chis, join_by(SECCION)
  )

# CORRECCIONES Y FILTROS --------------------------------------------------

no_shp <- base_ine %>%
  anti_join(mzaSHP, by = c("ENTIDAD", "DISTRITO_F", "MUNICIPIO", "SECCION", "LOCALIDAD"))

no_shp %>% count(DISTRITO_F, sort = T)

ln_loc <- base_ine %>% count(ENTIDAD, DISTRITO_F, MUNICIPIO, LOCALIDAD, wt = LISTA, name = "LISTA")
locSHP <- loc_shp #%>% left_join(ln_loc)

# algunas localidades se quedan sin lista nominal

# Corrección LISTA NOMINAL ------------------------------------------------

auxi <- ln_chis %>%
  select(SECCION, contains("LISTA")) %>%
  rename(LISTA = `LISTA NOMINAL`, LISTA_HOMBRES = `LISTA HOMBRES`, LISTA_MUJERES = `LISTA MUJERES`) %>%
  select(-contains(c("NO BINARIO", "NOBINARIO"))) |>
  tidyr::pivot_longer(-c(SECCION, LISTA, LISTA_HOMBRES, LISTA_MUJERES)) %>%
  mutate(sexo = if_else(grepl("HOMBRES",name), "HOMBRES", "MUJERES")) %>%
  left_join(
    base_ine %>% group_by(SECCION) %>% summarise(across(contains("LISTA"), ~sum(.x, na.rm = T),.names = "{.col}_nuevo"))
  ) %>%
  group_by(SECCION, sexo) %>%
  mutate(pct = value/sum(value)) %>%
  ungroup %>%
  mutate(value_nuevo = if_else(sexo == "HOMBRES", round(pct*LISTA_HOMBRES_nuevo), round(pct*LISTA_MUJERES_nuevo)),
         value_nuevo = if_else(is.na(value_nuevo), value, value_nuevo)) %>%
  select(SECCION, name, value_nuevo, sexo) %>%
  group_by(SECCION, sexo) %>%
  mutate(LISTA = sum(value_nuevo)) %>%
  ungroup

# proviene de la ln por manzana
ln_sexo <- auxi %>%
  select(-name,-value_nuevo) %>%
  filter(sexo != "NO BINARIO") |>
  distinct(SECCION,sexo,LISTA) %>%
  tidyr::pivot_wider(names_from = c("sexo"), values_from = c("LISTA")) %>%
  set_names(c("SECCION", "LISTA_HOMBRES", "LISTA_MUJERES")) %>%
  mutate(`LISTA NOMINAL` = LISTA_HOMBRES + LISTA_MUJERES)

#proviene de la ln por sección
ln_edad_sexo <- auxi %>%
  select(-sexo, -LISTA) %>%
  filter(!grepl("NO_BINARIO|NOBINARIO", name)) |>
  tidyr::pivot_wider(names_from = c("name"), values_from = c("value_nuevo"))

ln_chis2 <- ln_sexo %>% select(-contains("LISTA_")) %>%
  left_join(ln_edad_sexo)

# Regiones ----------------------------------------------------------------

regiones <- sec_shp |> filter(MUNICIPIO == 102) |> as_tibble() %>% split(.$DISTRITO) |> map(~.x$SECCION) #|> append(c("1737", "1743")))
# Población ---------------------------------------------------------------
# devtools::load_all(path = "~/Documents/Git/muestreaR")
devtools::load_all()
rm(diseño_chiapas)
chiapas <- PoblacionINE$new(nombre = "Chiapas",
                            ln = ln_chis2,
                            electoral = bd$bd_partido$pm_21 %>%
                              mutate(seccion = as.character(as.numeric(gsub("07_","",seccion)))) |>
                              semi_join(ln_chis2 |> mutate(SECCION = as.character(SECCION)), join_by(seccion == SECCION)),
                            shp_mza = mzaSHP,# |> relocate(DISTRITO_F,.before = DISTRITO_L),
                            shp_loc = locSHP |> filter(MUNICIPIO == 102),
                            shp_mun = mun_shp |> filter(MUNICIPIO == 102))

chiapas$marco_muestral$lista_nominal |> sum()
ln_chis$`LISTA NOMINAL` |> sum()



shp_chiapas <- CartografiaINE$new(df_shp, dl_shp, mun_shp |>
                                    filter(MUNICIPIO == 102), loc_shp |>
                                    filter(MUNICIPIO == 102), sec_shp |> filter(MUNICIPIO == 102) ,
                                  mzaSHP)


# diseño muestral ---------------------------------------------------------


diseño_chiapas <- DiseñoINE$new(poblacion=chiapas,
                                n=700,
                                n_0=5,
                                variable_poblacional="lista_nominal",
                                unidad_muestreo="Manzanas y localidades rurales puntuales",
                                id_unidad_muestreo="id",
                                llave_muestreo="Man")



# write_rds(shp_chiapas,"Insumos/shp.rda")

# agregar regiones --------------------------------------------------------

#esta funcion matchea  una lista  con las secciones correspondientes a los estratos que aqui se llaman regiones
#seria mejor usar un tibble con la estratificacion
chiapas$regiones(id = "SECCION",
                 regiones = regiones)


# se agregan los estratos que acaban de ser agregados a la informacion del INE en el objeto chiapas
diseño_chiapas$agregar_nivel("region",
                             tipo="strata",
                             descripcion= "Distritos federales 2022 y Tuxtla en una región. Más lugares con y sin diálogo",
                             llave="region")

 (uno <- shp_chiapas$graficar_mapa(bd = diseño_chiapas$poblacion$marco_muestral, nivel = "MUNICIPIO"))

# # Secciones ---------------------------------------------------------------
#
diseño_chiapas$agregar_nivel("SECCION",
                             tipo="cluster",
                             descripcion= "Secciones electorales",
                             llave="SECCION")

# Plan de muestra ---------------------------------------------------------
# 700 encuestas
# 5 encuestas por manzana
# 2 manzanas por sección
# entonces 10 encuestas por sección
# entonces son 70 secciones



#### Definicion del nivel 1
# diseño_chiapas$plan_muestra(nivel=1, criterio = "peso", unidades_nivel = 25)

diseño_chiapas$plan_muestra(nivel=1, criterio = "peso", unidades_nivel = 70)

diseño_chiapas$n_i$strata_1 %>% summarise(sum(m_1))
diseño_chiapas$n_i$strata_1 %>% summarise(sum(n_1))

#### Definicion del nivel 2
# diseño_chiapas$plan_muestra %>% debug

# 70 cluster dentro del marco seleccionados por peso
#diseño_chiapas$plan_muestra(nivel=1, criterio = "peso", unidades_nivel = 70)

# diseño_chiapas$n_i$strata_1 %>% summarise(sum(m_1))
# diseño_chiapas$n_i$strata_1 %>% summarise(sum(n_1))


# diseño_chiapas$plan_muestra(nivel=2,criterio = "uniforme", unidades_nivel = 100)
diseño_chiapas$plan_muestra(nivel=2)


# FPC ---------------------------------------------------------------------
# debug(calcular_fpc)
diseño_chiapas$fpc(nivel = 2)
# diseño_chiapas$fpc %>% undebug
diseño_chiapas$fpc(nivel = 0)

# Muestra -----------------------------------------------------------------

diseño_chiapas$extraer_muestra(nivel = 1)


diseño_chiapas$extraer_muestra(nivel = 2)

diseño_chiapas$muestra %>%
  pluck("MZA") %>%
  inner_join(diseño_chiapas$n_i$cluster_0) %>%
  summarise(sum(n_0))

diseño_chiapas$muestra %>%
  pluck("MZA") %>%
  count(strata_1, cluster_2) %>% count(strata_1)
# diseño_chiapas$muestra %>% pluck("MZA") %>% count(cluster_2, cluster_3) %>% count(cluster_2)
# diseño_chiapas$muestra %>% pluck("MZA") %>% count(cluster_3, cluster_0) %>% count(cluster_3)

uno %>% shp_chiapas$graficar_mapa(bd = diseño_chiapas$muestra, nivel = "SECCION")

# Cuotas ------------------------------------------------------------------
diseño_chiapas$calcular_cuotas(ajustar = T)

# Pruebas de la muestra ---------------------------------------------------
ja <- diseño_chiapas$revisar_muestra(prop_vars = bd$bd_partido$pm_21 %>% select(matches("morena|pvem|pri|pan|prd")) %>% names,
                                     var_extra = bd$bd_partido$pm_21 %>% select(matches("morena|pvem|pri|pan|prd")) %>% names)
ja[[3]]
ja[[3]][[2]] %>% ggplot(aes(x = deff)) + geom_histogram()


# Estadísticas ------------------------------------------------------------
leaflet(mun_shp |> filter(MUNICIPIO == 102)) %>% addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fill = F, weight = 1) %>%
  shp_chiapas$graficar_mapa(bd = diseño_chiapas$muestra, nivel = "SECCION")

diseño_chiapas$poblacion$marco_muestral %>% semi_join(
  diseño_chiapas$muestra$SECCION
) %>% count(region, MUNICIPIO, SECCION,sort = T) %>%
  count(region,MUNICIPIO, sort = T) %>%
  left_join(mun_shp %>% as_tibble() %>% select(MUNICIPIO,NOMBRE))

# prop rural urbana - MZA
diseño_chiapas$muestra$MZA |>
  unnest(data) |>
  count(TIPO) |>
  mutate(pct = n/sum(n),muestra = T) |>
  bind_rows(
    diseño_chiapas$poblacion$marco_muestral |>
      count(TIPO) |>
      mutate(pct = n/sum(n),muestra = F)
  ) |>
  ggplot(aes(x = muestra, y = pct)) +
  geom_point() +
  geom_line(aes(group = TIPO))+
  geom_label(aes(label = scales::percent(pct))) +
  facet_wrap(~TIPO)

# prop rural urbana - MZA
diseño_chiapas$muestra$MZA |>
  unnest(data) |>
  distinct(SECCION) |>
  left_join(
    sec_shp |> as_tibble() |> select(SECCION,DISTRITO,TIPO)
  ) |>
  mutate(muestra = T) |>
  bind_rows(
    sec_shp |> as_tibble() |> select(SECCION,TIPO) |>
      mutate(muestra = F)
  ) |>
  # mutate(TIPO = if_else(TIPO == "3", "2", TIPO)) |>
  count(muestra, TIPO) |>
  mutate(pct = n/sum(n), .by = muestra) |>
  ggplot(aes(x = muestra, y = pct)) +
  geom_point() +
  geom_line(aes(group = TIPO)) +
  facet_wrap(~TIPO)

diseño_chiapas$cuotas |> count(rango,sexo, wt = n)
# Mapas -------------------------------------------------------------------
library(ggmap)
library(usethis)
diseño_chiapas$exportar(shp_chiapas, carpeta = "Insumos", zoom = 16)
