library(tidyverse)
diseño <- readr::read_rds("auditoria/data/diseño_qro.rda")
shp <- readr::read_rds("auditoria/data/shp_qro.rda")
t_nivel <- "AULR"
id <- 1687

nivel <- diseño$niveles %>% filter(variable == "AULR") %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
nivel_anterior <- diseño$niveles %>% filter(nivel == diseño$niveles %>%
                                              filter(variable == "AULR") %>% pull(nivel) -1) %>%
  transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)

muestra <- diseño$muestra[[t_nivel]]

subcluster <- muestra %>% filter(!!rlang::sym(nivel) == id) %>% pull(nivel_anterior)

#nueva muestra
nuevo <- diseño$poblacion$marco_muestral %>%
  filter(!!rlang::sym(nivel_anterior) == subcluster) %>%
  muestreaR:::agrupar_nivel(readr::parse_number(nivel)) %>%
  anti_join(muestra) %>%
  mutate(total = sum(POBTOT)) %>%
  group_by(total, .add = T) %>%
  nest %>%
  ungroup %>%
  slice_sample(weight_by = total)

manzanas <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) == id) %>% nrow

nuevas_manzanas <- nuevo %>% unnest(data) %>% slice_sample(n = manzanas) %>%
  group_by(across(strata_1:total),cluster_0) %>% nest()
#recalcular n_i$cluster_0
cl0_quitar <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) == id) %>% pull(cluster_0)
enc_0 <- diseño$n_i$cluster_0 %>% filter(cluster_0 %in% cl0_quitar) %>% pull(n_0)
diseño$n_i$cluster_0 <- diseño$n_i$cluster_0 %>% filter(!cluster_0 %in% nuevas_manzanas$cluster_0) %>%
  bind_rows(
    diseño$n_i$cluster_0 %>% filter(cluster_0 %in% nuevas_manzanas$cluster_0) %>% mutate(n_0 = enc_0)
  ) %>% arrange(cluster_0)
#sustituir muestra
diseño$muestra[[t_nivel]] <- diseño$muestra[[t_nivel]] %>% filter(!!rlang::sym(nivel) != id) %>% bind_rows(nuevo)
diseño$muestra$MZA <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) != id) %>% bind_rows(nuevas_manzanas)

#recalcular cuotas

diseño$cuotas <- muestreaR:::cuotas(diseño)
undebug(muestreaR:::google_maps)
library(ggmap)
ggmap::ggmap(ggmap::get_map())
muestreaR:::google_maps(diseño, shp = shp$shp, zoom = 15)

diseño %>% readr::write_rds("auditoria/data/diseño_qro2.rda")

# sustitucion de 428 ------------------------------------------------------


library(tidyverse)
diseño <- readr::read_rds("auditoria/data/diseño_qro2_1.rda")
shp <- readr::read_rds("auditoria/data/shp_qro.rda")
t_nivel <- "AULR"
id <- 428

nivel <- diseño$niveles %>% filter(variable == "AULR") %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
nivel_anterior <- diseño$niveles %>% filter(nivel == diseño$niveles %>%
                                              filter(variable == "AULR") %>% pull(nivel) -1) %>%
  transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)

muestra <- diseño$muestra[[t_nivel]]

subcluster <- muestra %>% filter(!!rlang::sym(nivel) == id) %>% pull(nivel_anterior)

#nueva muestra
nuevo <- diseño$poblacion$marco_muestral %>%
  filter(!!rlang::sym(nivel_anterior) == subcluster) %>%
  muestreaR:::agrupar_nivel(readr::parse_number(nivel)) %>%
  anti_join(muestra) %>%
  mutate(total = sum(POBTOT)) %>%
  group_by(total, .add = T) %>%
  nest %>%
  ungroup %>%
  slice_sample(weight_by = total)

manzanas <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) == id) %>% nrow

nuevas_manzanas <- nuevo %>% unnest(data) %>% slice_sample(n = manzanas) %>%
  group_by(across(strata_1:total),cluster_0) %>% nest()
#recalcular n_i$cluster_0
cl0_quitar <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) == id) %>% pull(cluster_0)
enc_0 <- diseño$n_i$cluster_0 %>% filter(cluster_0 %in% cl0_quitar) %>% pull(n_0)
diseño$n_i$cluster_0 <- diseño$n_i$cluster_0 %>% filter(!cluster_0 %in% nuevas_manzanas$cluster_0) %>%
  bind_rows(
    diseño$n_i$cluster_0 %>% filter(cluster_0 %in% nuevas_manzanas$cluster_0) %>% mutate(n_0 = enc_0)
  ) %>% arrange(cluster_0)
#sustituir muestra
diseño$muestra[[t_nivel]] <- diseño$muestra[[t_nivel]] %>% filter(!!rlang::sym(nivel) != id) %>% bind_rows(nuevo)
diseño$muestra$MZA <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) != id) %>% bind_rows(nuevas_manzanas)

#recalcular cuotas

aux_cuotas <- muestreaR:::cuotas(diseño)

diseño$cuotas <- diseño$cuotas %>% semi_join(aux_cuotas, by = "cluster_3") %>%
  bind_rows(aux_cuotas %>% anti_join(diseño$cuotas, by = "cluster_3")) %>% arrange(cluster_3)

library(ggmap)
ggmap::ggmap(ggmap::get_map())
muestreaR:::google_maps(diseño, shp = shp$shp, zoom = 15)

diseño %>% readr::write_rds("auditoria/data/diseño_qro3.rda")

# sustitucion de 1251 ------------------------------------------------------


library(tidyverse)
diseño <- readr::read_rds("auditoria/data/diseño_qro3.rda")
shp <- readr::read_rds("auditoria/data/shp_qro.rda")
t_nivel <- "AULR"
id <- 1251

nivel <- diseño$niveles %>% filter(variable == "AULR") %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
nivel_anterior <- diseño$niveles %>% filter(nivel == diseño$niveles %>%
                                              filter(variable == "AULR") %>% pull(nivel) -1) %>%
  transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)

muestra <- diseño$muestra[[t_nivel]]

subcluster <- muestra %>% filter(!!rlang::sym(nivel) == id) %>% pull(nivel_anterior)

#nueva muestra
nuevo <- diseño$poblacion$marco_muestral %>%
  filter(!!rlang::sym(nivel_anterior) == subcluster) %>%
  muestreaR:::agrupar_nivel(readr::parse_number(nivel)) %>%
  anti_join(muestra) %>%
  mutate(total = sum(POBTOT)) %>%
  group_by(total, .add = T) %>%
  nest %>%
  ungroup %>%
  slice_sample(weight_by = total)
nuevo$data %>% pluck(1,"NOM_MUN")
manzanas <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) == id) %>% nrow

nuevas_manzanas <- nuevo %>% unnest(data) %>% slice_sample(n = manzanas) %>%
  group_by(across(strata_1:total),cluster_0) %>% nest()
#recalcular n_i$cluster_0
cl0_quitar <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) == id) %>% pull(cluster_0)
enc_0 <- diseño$n_i$cluster_0 %>% filter(cluster_0 %in% cl0_quitar) %>% pull(n_0)
diseño$n_i$cluster_0 <- diseño$n_i$cluster_0 %>% filter(!cluster_0 %in% nuevas_manzanas$cluster_0) %>%
  bind_rows(
    diseño$n_i$cluster_0 %>% filter(cluster_0 %in% nuevas_manzanas$cluster_0) %>% mutate(n_0 = enc_0)
  ) %>% arrange(cluster_0)
#sustituir muestra
diseño$muestra[[t_nivel]] <- diseño$muestra[[t_nivel]] %>% filter(!!rlang::sym(nivel) != id) %>% bind_rows(nuevo)
diseño$muestra$MZA <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) != id) %>% bind_rows(nuevas_manzanas)

#recalcular cuotas

aux_cuotas <- muestreaR:::cuotas(diseño)

diseño$cuotas <- diseño$cuotas %>% semi_join(aux_cuotas, by = "cluster_3") %>%
  bind_rows(aux_cuotas %>% anti_join(diseño$cuotas, by = "cluster_3")) %>% arrange(cluster_3)

library(ggmap)
ggmap::ggmap(ggmap::get_map())
muestreaR:::google_maps(diseño, shp = shp$shp, zoom = 15)

diseño %>% readr::write_rds("auditoria/data/diseño_qro4.rda")

# sustitucion de 1251 ------------------------------------------------------


library(tidyverse)
diseño <- readr::read_rds("auditoria/data/diseño_qro4.rda")
shp <- readr::read_rds("auditoria/data/shp_qro.rda")
t_nivel <- "AULR"
id <- 1053

nivel <- diseño$niveles %>% filter(variable == "AULR") %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
nivel_anterior <- diseño$niveles %>% filter(nivel == diseño$niveles %>%
                                              filter(variable == "AULR") %>% pull(nivel) -1) %>%
  transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)

muestra <- diseño$muestra[[t_nivel]]

subcluster <- muestra %>% filter(!!rlang::sym(nivel) == id) %>% pull(nivel_anterior)

#nueva muestra
nuevo <- diseño$poblacion$marco_muestral %>%
  filter(!!rlang::sym(nivel_anterior) == subcluster) %>%
  muestreaR:::agrupar_nivel(readr::parse_number(nivel)) %>%
  anti_join(muestra) %>%
  mutate(total = sum(POBTOT)) %>%
  group_by(total, .add = T) %>%
  nest %>%
  ungroup %>%
  slice_sample(weight_by = total)
nuevo$data %>% pluck(1,"NOM_MUN")
manzanas <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) == id) %>% nrow

nuevas_manzanas <- nuevo %>% unnest(data) %>% slice_sample(n = manzanas) %>%
  group_by(across(strata_1:total),cluster_0) %>% nest()
#recalcular n_i$cluster_0
cl0_quitar <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) == id) %>% pull(cluster_0)
enc_0 <- diseño$n_i$cluster_0 %>% filter(cluster_0 %in% cl0_quitar) %>% pull(n_0)
diseño$n_i$cluster_0 <- diseño$n_i$cluster_0 %>% filter(!cluster_0 %in% nuevas_manzanas$cluster_0) %>%
  bind_rows(
    diseño$n_i$cluster_0 %>% filter(cluster_0 %in% nuevas_manzanas$cluster_0) %>% mutate(n_0 = enc_0)
  ) %>% arrange(cluster_0)
#sustituir muestra
diseño$muestra[[t_nivel]] <- diseño$muestra[[t_nivel]] %>% filter(!!rlang::sym(nivel) != id) %>% bind_rows(nuevo)
diseño$muestra$MZA <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) != id) %>% bind_rows(nuevas_manzanas)

#recalcular cuotas

aux_cuotas <- muestreaR:::cuotas(diseño)

diseño$cuotas <- diseño$cuotas %>% semi_join(aux_cuotas, by = "cluster_3") %>%
  bind_rows(aux_cuotas %>% anti_join(diseño$cuotas, by = "cluster_3")) %>% arrange(cluster_3)

library(ggmap)
# ggmap::ggmap(ggmap::get_map())
muestreaR:::google_maps(diseño, shp = shp$shp, zoom = 15)

diseño %>% readr::write_rds("auditoria/data/diseño_qro5.rda")
