library(tidyverse)
library(glue)
library(sf)
library(rgdal)
library(sp)
library(rgdal)
devtools::load_all()

# Crear marco muestral ----------------------------------------------------

# wd <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INEGI/Censo 2020"
#
# wd_murb <- list.files(glue("{wd}/Población"), full.names = T)
# wd_loc <- list.files(paste(list.files(glue("{wd}/Localidad"),full.names = T), "conjunto_de_datos",sep = "/"), full.names = T)
# wd_shp <- list.files(paste(list.files(glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "ar.shp")
# wd_shp_loc <- list.files(paste(list.files(glue("{wd}/AGEB"), full.names = T)[1:32],"conjunto_de_datos", sep = "/"), full.names = T,pattern = "[[:digit:]]l.shp")
# orden <- substr(wd_loc,nchar(wd_loc)-13,nchar(wd_loc)-12) %>% order
#
#
# qro <- crear_mm(mza = wd_murb[22],loc = wd_loc[orden][22], ageb_shp = wd_shp[22], loc_shp = wd_shp_loc[22])
#
# usethis::use_data(qro, overwrite = T)
# Región ------------------------------------------------------------------
#cambiar a qro

region_anterior <- list(
  `Sierra Gorda` = c("Arroyo Seco",
    "Jalpan de Serra",
    "Landa de Matamoros",
    "Pinal de Amoles",
    "San Joaquín",
    "Cadereyta de Montes",
    "Colón",
    "Peñamiller",
    "Tolimán"),
  `Los Valles Centrales` = c(
    "Ezequiel Montes",
    "El Marqués",
    "Tequisquiapan",
    "Pedro Escobedo",
    "San Juan del Río",
    "Amealco de Bonfil",
    "Huimilpan"
  ),
  `El Bajío Queretano` = c(
    "Querétaro",
    "Corregidora"
  )
)


marco <- regiones(qro, id = "NOM_MUN", regiones = region_anterior)

# Análisis ----------------------------------------------------------------

# Cuántas personas mayores de 18 años hay en Querétaro?

marco %>%
  summarise(resultado=sum(P_18YMAS, na.rm=T)) %>%
  pull()

# Porcentaje de población total:


marco %>%
  summarise(porcentaje=sum(P_18YMAS, na.rm=T)/sum(POBTOT, na.rm=T)) %>%
  pull()



# Unidad de muestreo
nrow(qro)


# Población rural y urbana - total y mayores de 18

marco %>%
  group_by(AMBITO) %>%
  summarise(pobtot=sum(POBTOT, na.rm=T))

marco %>%
  group_by(AMBITO) %>%
  summarise(pobtot=sum(P_18YMAS, na.rm=T))

# Población total de estratos (pob tot por municipio, pob tot de manzanas, pob tot de amanzanadas)

# Total pobalción por municipio
marco %>%
  group_by(MUN,NOM_MUN, AMBITO) %>%
  summarise(pobtot=sum(POBTOT, na.rm=T)) %>%
  spread(AMBITO, pobtot)

# total población manzanas

marco %>%
  group_by(AMBITO) %>%
  summarise(pobtot=sum(POBTOT, na.rm=T)) %>%
  spread(AMBITO, pobtot)

# concentración de municipios por ambito

# total localidades no amanzanadas

marco %>%
  filter(MZA==" NA") %>%
  group_by(LOC) %>%
  summarise(pobtot=sum(POBTOT, na.rm=T)) %>%
  ungroup()


# resumen de variable del censo

marco %>%
  select(c(VPH_AGUADV,VPH_SNBIEN, POCUPADA, VPH_C_ELEC)) %>%
  summary()




# graficar porcentajes de variable del censo

## density

marco %>%
  mutate(
         porc= POCUPADA/POBTOT) %>%
  arrange(desc(porc)) %>%
  ggplot() +
  geom_density(aes(x=porc, color=NOM_MUN)) +
  facet_wrap(~region)


marco %>%
  mutate(
    porc= POCUPADA/POBTOT) %>%
  arrange(desc(porc)) %>%
  ggplot() +
  geom_density(aes(x=porc, color=AMBITO))

## boxplot

marco %>%
  mutate(
    porc= VPH_AGUADV/TVIVPARHAB) %>%
  arrange(desc(porc)) %>%
  ggplot() +
  geom_boxplot(aes(y=porc, color=AMBITO))


# calculo nivel municipal
marco %>%
  select(MUN:MZA, VPH_AGUADV, TVIVPARHAB) %>%
  group_by(MUN) %>%
  summarise(VPH_AGUADV=sum(VPH_AGUADV, na.rm=T),
            TVIVPARHAB=sum(TVIVPARHAB, na.rm=T),
            porc=VPH_AGUADV/TVIVPARHAB) %>%
  ungroup() %>%
  arrange(desc(porc))

# Información muestral ----------------------------------------------------
ja <- empaquetar(bd = marco,
                 grupo = c("region","NOM_MUN","MZA"),
                 tipo = c("strata","id","id"),
                 n = c(NA_integer_, 5,130),
                 peso_tamaño = POBTOT,
                 criterio_n = "peso")
ja %>% count(region, NOM_MUN,fpc_2) %>% count(region,wt = fpc_2, sort = T) %>% summarise(sum(n))
ja %>% count(region, NOM_MUN,NOM_LOC,fpc_3) %>% count(region,NOM_MUN,wt = fpc_3,sort = T) %>% summarise(sum(n))
ja %>% count(region, NOM_MUN,NOM_LOC,AGEB,fpc_4) %>% count(region,NOM_MUN,NOM_LOC,wt = fpc_4,sort = T)%>% summarise(sum(n))

ja %>% distinct(region,fpc_2,fpc_3,fpc_4) %>%
  summarise(across(fpc_2:fpc_4, list(minimo = min, maximo = max),
                   .names = "{.col}.{.fn}")) %>%
  pivot_longer(everything()) %>% separate(name,into=c("col","stat"),sep = "\\.") %>%
  pivot_wider(names_from = stat, values_from = value)


niveles <- c("region","NOM_MUN","NOM_LOC")
t_niveles <- c("strata","id","id")

agrupar <- c()
n_sortear <- c(3,3,6)

aux <- ja
for(i in seq_along(t_niveles)){
  agrupar <- c(agrupar, niveles[i])

  if(t_niveles[i] != "strata"){
    aux <- aux %>% group_by(across(all_of(agrupar))) %>% nest %>% ungroup(niveles[i]) %>%
      sample_n(n_sortear[i]) %>% unnest(data) %>% ungroup
  }
}



