# muestra veracruz 19 feb 2024

library(dplyr)
library(sf)
library(leaflet)
library(RColorBrewer)
library(ggmap)
# library(aelectoral2)
library(muestreaR)
library(tidyr)

devtools::load_all(path = "../aelectoral2/")
1
# Insumos -----------------------------------------------------------------

#insumos INE
wd <- "H:/Shared drives/Morant Consultores/Insumos/INE"
bd <- Electoral$new(eleccion = "pm_21", entidad = "ver", extranjero = F)
bd$agregar_bd("gb_18")
bd$partido(eleccion = "gb_18")
bd$partido(eleccion = "pm_21")


ln_re_sexo <- readxl::read_excel(glue::glue("{wd}/Lista Nominal/DatosAbiertos-derfe-pdln_edms_re_20230921.xlsx"))
ln_re_sexo <- ln_re_sexo %>% set_names(gsub(pattern = "\r\n",replacement = " ",x = names(ln_re_sexo)))
ln_ver <- ln_re_sexo %>% filter(`CLAVE ENTIDAD` == 30, SECCION != 0)


# SHAPE FILES INE

mun_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2023/30 VERACRUZ/MUNICIPIO.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

murb_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2023/30 VERACRUZ/MANCHA_URBANA.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

loc_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2023/30 VERACRUZ/LOCALIDAD.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()


sec_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2023/30 VERACRUZ/SECCION.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

mza_shp <- rgdal::readOGR(dsn= glue::glue("{wd}/SHP/2023/30 VERACRUZ/MANZANA.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf() #%>%

dl_shp <- rgdal::readOGR(dsn=glue::glue("{wd}/SHP/2023/30 VERACRUZ/DISTRITO_LOCAL.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf()

df_shp <- rgdal::readOGR(dsn=glue::glue("{wd}/SHP/2023/30 VERACRUZ/DISTRITO_FEDERAL.shp"),encoding = "CP1252") %>%
  sp::spTransform(sp::CRS("+init=epsg:4326")) %>% sf::st_as_sf() |>
  rename(DISTRITO_F = DISTRITO)

base_ine <- read.table(glue::glue("{wd}/Lista Nominal/UT_23_00109_pdln_edmslm_sexo_13012023.txt"),
                       sep = ",",
                       header = TRUE) %>% as_tibble() %>%
  rename(DISTRITO_F = DISTRITO) |>
  filter(ENTIDAD == 30)

mzaSHP <- mza_shp %>%
  semi_join(
    ln_ver, join_by(SECCION)
  )


 ln_ver %>% anti_join(mza_shp %>% as_tibble() ,by= "SECCION")


 # CORRECCIONES Y FILTROS --------------------------------------------------

 no_shp <- base_ine %>%
   anti_join(mzaSHP, by = c("ENTIDAD", "DISTRITO_F", "MUNICIPIO", "SECCION", "LOCALIDAD"))

 no_shp %>% count(DISTRITO_F, sort = T)

 ln_loc <- base_ine %>% count(ENTIDAD, DISTRITO_F, MUNICIPIO, LOCALIDAD, wt = LISTA, name = "LISTA")
 locSHP <- loc_shp


 # Corrección LISTA NOMINAL ------------------------------------------------


 auxi <- ln_ver %>%
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

 ln_ver2 <- ln_sexo %>% select(-contains("LISTA_")) %>%
   left_join(ln_edad_sexo)



#Regiones------------------------------------------------------------------
 set.seed(1107)

 regiones <- ln_ver2 |>
   distinct(SECCION) %>%
   mutate(SECCION = as.character(as.numeric(SECCION))) %>%
   mutate(region = sample(1:2, nrow(.), replace = T)) |>
   split(~region) |>
   map(~.x |> pull(SECCION))

# Población ---------------------------------------------------------------
#devtools::load_all(path = "~/Documents/Git/muestreaR")
marcoMuestral_Veracruz <- PoblacionINE$new(nombre = "Veracruz",
                            ln = ln_ver2,
                            electoral = bd$bd_partido$pm_21 %>%
                              mutate(seccion = as.character(as.numeric(gsub("30_","",seccion)))) |>
                              semi_join(ln_ver2|> mutate(SECCION = as.character(SECCION)), join_by(seccion == SECCION)),
                            shp_mza = mzaSHP,# |> relocate(DISTRITO_F,.before = DISTRITO_L),
                            shp_loc = locSHP,
                            shp_mun = mun_shp)

marcoMuestral_Veracruz$marco_muestral$lista_nominal |> sum()
ln_chis$`LISTA NOMINAL` |> sum()



shp_veracruz <- CartografiaINE$new(df_shp,
                                   dl_shp,
                                   mun_shp,
                                   loc_shp ,
                                   sec_shp,
                                  mzaSHP)

# diseño muestral ---------------------------------------------------------





diseno_veracruz <- DiseñoINE$new(poblacion=marcoMuestral_Veracruz,
                                n=800,
                                n_0=6,
                                variable_poblacional="lista_nominal",
                                unidad_muestreo="Manzanas y localidades rurales puntuales",
                                id_unidad_muestreo="id",
                                llave_muestreo="Man")


#Estratos ------------------------------------------------------------------
marcoMuestral_Veracruz$regiones(id = "SECCION",
                 regiones = regiones)


# se agregan los estratos que acaban de ser agregados a la informacion del INE en el objeto chiapas
diseno_veracruz$agregar_nivel("region",
                             tipo="strata",
                             descripcion= "Estratos asignados",
                             llave="region")

(uno <- shp_veracruz$graficar_mapa(bd = diseno_veracruz$poblacion$marco_muestral, nivel = "MUNICIPIO"))


# # Secciones ---------------------------------------------------------------
#
diseno_veracruz$agregar_nivel("SECCION",
                             tipo="cluster",
                             descripcion= "Secciones electorales",
                             llave="SECCION")


# Plan de muestra ---------------------------------------------------------
# 800 encuestas
# 6 encuestas por manzana
# 2 manzanas por sección
# 134  manzanas a visitar

#### Definicion del nivel 1

diseno_veracruz$plan_muestra(nivel=1, criterio = "peso", unidades_nivel = 65)

diseno_veracruz$n_i$strata_1 %>% summarise(sum(m_1))
diseno_veracruz$n_i$strata_1 %>% summarise(sum(n_1))

#### Definicion del nivel 2
diseno_veracruz$plan_muestra(nivel=2)




# FPC ---------------------------------------------------------------------
diseno_veracruz$fpc(nivel = 2)
diseno_veracruz$fpc(nivel = 0)

# Muestra -----------------------------------------------------------------

diseno_veracruz$extraer_muestra(nivel = 1)
diseno_veracruz$extraer_muestra(nivel = 2)

diseno_veracruz$muestra %>%
  pluck("MZA") %>%
  inner_join(diseno_veracruz$n_i$cluster_0) %>%
  summarise(sum(n_0))

diseno_veracruz$muestra %>%
  pluck("MZA") %>%
  count(strata_1, cluster_2) %>% count(strata_1)


uno %>% shp_veracruz$graficar_mapa(bd = diseno_veracruz$muestra, nivel = "SECCION")



diseno_veracruz$calcular_cuotas(ajustar = T)

# Pruebas de la muestra ---------------------------------------------------
ja <- diseno_veracruz$revisar_muestra(prop_vars = bd$bd_partido$pm_21 %>% select(matches("morena|pvem|pri|pan|prd")) %>% names,
                                     var_extra = bd$bd_partido$pm_21 %>% select(matches("morena|pvem|pri|pan|prd")) %>% names)
ja[[3]]
ja[[3]][[2]] %>% ggplot(aes(x = deff)) + geom_histogram()

# Estadísticas ------------------------------------------------------------
leaflet(mun_shp ) %>% addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fill = F, weight = 1) %>%
  shp_veracruz$graficar_mapa(bd = diseno_veracruz$muestra, nivel = "SECCION")


diseno_veracruz$poblacion$marco_muestral %>% semi_join(
  diseno_veracruz$muestra$SECCION
) %>% count(region, MUNICIPIO, SECCION,sort = T) %>%
  count(region,MUNICIPIO, sort = T) %>%
  left_join(mun_shp %>% as_tibble() %>% select(MUNICIPIO,NOMBRE))

# prop rural urbana - MZA
diseno_veracruz$muestra$MZA |>
  unnest(data) |>
  count(TIPO) |>
  mutate(pct = n/sum(n),muestra = T) |>
  bind_rows(
    diseno_veracruz$poblacion$marco_muestral |>
      count(TIPO) |>
      mutate(pct = n/sum(n),muestra = F)
  ) |>
  ggplot(aes(x = muestra, y = pct)) +
  geom_point() +
  geom_line(aes(group = TIPO))+
  geom_label(aes(label = scales::percent(pct))) +
  facet_wrap(~TIPO)

# prop rural urbana - MZA
diseno_veracruz$muestra$MZA |>
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
  count(muestra, TIPO) |>
  mutate(pct = n/sum(n), .by = muestra) |>
  ggplot(aes(x = muestra, y = pct)) +
  geom_point() +
  geom_line(aes(group = TIPO)) +
  facet_wrap(~TIPO)

diseno_veracruz$cuotas |> count(rango,sexo, wt = n)
# Mapas -------------------------------------------------------------------
library(ggmap)
library(usethis)

ggmap::register_google(key = "AIzaSyDLKgUN2iLUX7zLxyLN44O5E4T62IRogNE")

diseno_veracruz$exportar(shp_veracruz, carpeta = "data-raw/Insumos_veracruz/", zoom = 16)
