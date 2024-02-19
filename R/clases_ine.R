
# Diseño ------------------------------------------------------------------
#' @export
DiseñoINE <- R6::R6Class("Diseño",
                         public =list(
                           poblacion=NULL,
                           ultimo_nivel=-1,
                           n=NULL,
                           n_0=NULL,
                           n_i=list(),
                           variable_poblacional=NULL,
                           niveles=tibble::tibble(nivel=NULL,
                                                  tipo=NULL,
                                                  descripcion=NULL,
                                                  llave=NULL,
                                                  aprobado=NULL,
                                                  unidades=NULL,
                                                  plan_muestra=NULL),
                           muestra = NULL,
                           cuotas = NULL,
                           n_sustitucion = 0,
                           dir.exportar = NULL,
                           sobre_muestra = NULL,
                           initialize = function(poblacion,
                                                 n,
                                                 n_0,
                                                 variable_poblacional,
                                                 unidad_muestreo,
                                                 id_unidad_muestreo,
                                                 llave_muestreo){
                             self$poblacion=poblacion
                             self$n=n
                             private$unidad_muestreo=unidad_muestreo
                             self$n_0=n_0
                             self$variable_poblacional=variable_poblacional
                             self$niveles=self$agregar_nivel(variable=id_unidad_muestreo,
                                                             tipo="cluster",
                                                             descripcion = unidad_muestreo,
                                                             llave = llave_muestreo)
                             self$n_i <- self$plan_muestra(nivel = self$ultimo_nivel)
                           },
                           print = function(){
                             mensaje <- cat(
                               glue::glue(
                                 "Diseño que representa a la población de {self$poblacion$nombre} con una muestra de tamaño {scales::comma(self$n)} cuya unidad de muestreo es {private$unidad_muestreo}.

                               Se realizarán {self$n_0} entrevistas por unidad mínima.

                               Para efectos del diseño muestral se utilizará {self$variable_poblacional} del censo 2020 del INEGI para cuantificar el tamaño de la población."))
                             return(mensaje)
                           },
                           agregar_nivel=function(variable,
                                                  tipo,
                                                  descripcion,
                                                  llave){
                             if(!tipo %in% c("strata", "cluster")) stop("Tipo debe ser igual a cluster o strata")
                             # Al último nivel le agregamos uno
                             self$ultimo_nivel <- self$ultimo_nivel+1
                             # Modificar el marco muestral
                             self$poblacion$marco_muestral <- self$poblacion$marco_muestral %>%
                               {if(self$ultimo_nivel!=0) agrupar_nivel(., nivel=self$ultimo_nivel)
                                 else .
                               } %>%
                               group_by(!!sym(variable), add=T) %>%
                               mutate("{tipo}_{self$ultimo_nivel}":= cur_group_id()) %>%
                               ungroup()
                             # Modificar sel niveles
                             self$niveles <- self$niveles %>%
                               add_row(.data = tibble(nivel=self$ultimo_nivel,
                                                      variable=variable,
                                                      tipo=tipo,
                                                      descripcion=descripcion,
                                                      llave=llave,
                                                      unidades=NA_integer_,
                                                      aprobado=F,
                                                      plan_muestra=F))

                             return(self$niveles)
                           },
                           eliminar_nivel=function(nivel){
                             aux <- nivel
                             self$niveles <- self$niveles %>%
                               filter(nivel<aux)
                             self$ultimo_nivel <- nivel-1
                             self$poblacion$marco_muestral <- self$poblacion$marco_muestral %>%
                               select(-matches(glue::glue("(cluster|strata)_[{nivel}-9]")))
                             self$n_i <- self$n_i[-grep(glue::glue("(cluster|strata)_[{nivel}-9]"),
                                                        names(self$n_i))]
                           },
                           plan_muestra =function(nivel, criterio, unidades_nivel, manual){
                             nivel_l <- nivel
                             if(nivel_l==0){
                               # Se asigna el nivel 0
                               res <- self$poblacion$marco_muestral %>%
                                 group_by(cluster_0) %>%
                                 summarise(m_0=self$n_0,
                                           n_0=self$n_0)
                               # Se le agrega el total de unidades al nivel
                               self$niveles <- self$niveles %>%
                                 mutate(unidades=if_else(nivel==0,
                                                         ceiling(self$n/self$n_0),
                                                         as.numeric(unidades)))
                               # Se etiqueta en la lista
                               res <- list(cluster_0=res)
                             }
                             else{
                               # Cuando es el último nivel
                               if(nivel_l==self$ultimo_nivel){
                                 res <- asignar_m(self,
                                                  unidades_nivel = ) %>%
                                   left_join(asignar_n(self))
                                 res <- purrr::set_names(list(res), glue::glue("{self$niveles %>%
                                                     filter(nivel==nivel_l) %>%
                                                     pull(tipo)}_{nivel_l}"))
                               }
                               # Si no es nivel=0 ni es nivel=ultimo_nivel
                               else{
                                 # Si no es el último nivel
                                 # Primero se asigna m
                                 # Después se asigna n
                                 if(criterio != "manual"){
                                   res <- asignar_m(diseño = self,
                                                    criterio = criterio,
                                                    unidades_nivel = unidades_nivel, manual = manual) %>%
                                     left_join(asignar_n(self))
                                 } else{

                                   res <- asignar_m(diseño = self,
                                                    criterio = criterio,
                                                    unidades_nivel = unidades_nivel, manual = manual) %>%
                                     mutate(!!rlang::sym(glue::glue("n_{nivel_l}")) := !!rlang::sym(glue::glue("m_{nivel_l}"))/sum(!!rlang::sym(glue::glue("m_{nivel_l}"))) * self$n)
                                 }

                                 # Se etiqueta
                                 res <- purrr::set_names(list(res), glue::glue("{self$niveles %>%
                                                     filter(nivel==nivel_l) %>%
                                                     pull(tipo)}_{nivel_l}"))
                               }
                             }
                             self$niveles <- self$niveles %>%
                               mutate(plan_muestra=(nivel<=nivel_l))
                             self$n_i <- c(self$n_i, res)
                             return(res)
                           },
                           sobremuestra = function(nivel, unidad, unidad_elegida, total_m, total_n){
                             #modificar n_i
                             #modificar niveles
                             #tablita de sobremuestra para indicar en extraer muestra

                             tipo_nivel <- self$n_i %>% names() %>% grep(glue::glue("strata|cluster_{nivel}"),. ,value = T)

                             estratos <- self$poblacion$marco_muestral %>%
                               filter(!!rlang::sym(unidad) == !! unidad_elegida) %>%
                               distinct(!!rlang::sym(tipo_nivel))

                             if(nivel == 1){
                               modif <- self$n_i %>% pluck(tipo_nivel) %>% semi_join(estratos)

                               self$n_i[[tipo_nivel]] <-
                                 self$n_i %>% pluck(tipo_nivel) %>% anti_join(
                                   estratos
                                 ) %>% bind_rows(

                                   modif %>%
                                     mutate(pct_m = m_1/sum(m_1),
                                            tot_m = sum(m_1),
                                            faltan_m = !!total_m-tot_m,
                                            nuevo_m = round(faltan_m*pct_m),
                                            pct_n = n_1/sum(n_1),
                                            tot_n = sum(n_1),
                                            faltan_n = !!total_n-tot_n,
                                            nuevo_n = faltan_n*pct_n
                                     ) %>%
                                     transmute(strata_1, m_1 = m_1 + nuevo_m, n_1 = n_1 + nuevo_n)
                                 ) %>% arrange(!!rlang::sym(tipo_nivel))

                               self$n <- self$n_i[[tipo_nivel]] %>% summarise(sum(n_1)) %>% pull(1) %>% round()

                               self$niveles <- self$niveles %>%
                                 mutate(unidades = case_when(nivel == (!!nivel +1)~(self$n_i %>% pluck(tipo_nivel) %>%
                                                                                      summarise(sum(m_1)) %>% pull(1)),
                                                             nivel == 0 ~ self$n/self$n_0,
                                                             T~unidades))


                               self$sobre_muestra <- self$sobre_muestra %>%
                                 bind_rows(
                                   modif %>%
                                     rename_with(.fn = ~glue("{.x}_vieja"), .cols = matches("m_|n_")) %>%
                                     mutate(!!rlang::sym(unidad) := !!unidad_elegida, m_sm = round(total_m*m_1_vieja/sum(m_1_vieja)), n_sm = total_n*n_1_vieja/sum(n_1_vieja))
                                 )
                             } else{
                               stop("sobremuestra no programada para este nivel de profundidad")
                             }
                           },
                           fpc = function(nivel){
                             self$poblacion$marco_muestral <- calcular_fpc(self, nivel = nivel)
                           },
                           extraer_muestra = function(nivel){
                             m <- muestrear(self, nivel = nivel)
                             aux <- m %>% purrr::pluck(length(m))
                             if(nivel == self$ultimo_nivel){
                               #actualizar fpc_0
                               if(self$niveles %>% filter(nivel == 0) %>% pull(unidades) != nrow(aux)){
                                 ajuste <- ((self$niveles %>% filter(nivel == 0) %>% pull(unidades)) - nrow(aux))*self$n_0

                                 nuevo <- self$n_i$cluster_0 %>% semi_join(aux) %>% sample_n(size = abs(ajuste), replace = T) %>%
                                   mutate(sumar = sign(ajuste)) %>% group_by(cluster_0) %>%
                                   summarise(m_0 = unique(m_0), n_0 = unique(n_0),
                                             sumar = sum(sumar)) %>% mutate(n_0 = n_0 + sumar) %>% select(-sumar)
                                 self$n_i$cluster_0 <- self$n_i$cluster_0 %>% anti_join(nuevo, by = "cluster_0") %>% bind_rows(nuevo) %>%
                                   arrange(cluster_0)
                               }
                             }
                             self$muestra <- m
                           },
                           calcular_cuotas = function(ajustar = T){
                             self$cuotas <- cuotas_ine(self, ajustar = ajustar)
                           },
                           revisar_muestra = function(prop_vars, var_extra){
                             a <- llaves(self)

                             b <- plan(self)

                             c <- revision_ine(self = self,prop_vars = prop_vars, var_extra = var_extra)

                             return(list(a,b,c))

                           },
                           exportar = function(shp, zoom = 16, carpeta = "Insumos"){
                             self$dir.exportar <- carpeta
                             if(!file.exists(carpeta)) dir.create(carpeta)
                             if(!file.exists(glue::glue("{carpeta}/Mapas"))) dir.create(glue::glue("{carpeta}/Mapas"))

                             shp$crear_mapas(diseño = self, zoom = zoom, dir = glue::glue("{carpeta}/Mapas"))
                             self$cuotas %>% readr::write_excel_csv(glue::glue("{carpeta}/cuotas.csv"))
                             readr::write_rds(self, glue::glue("{carpeta}/diseño.rda"))
                             shp %>% readr::write_rds(glue::glue("{carpeta}/shp.rda"))
                           },
                           sustituir_muestra = function(shp, id, zoom = 16, ajustar_cuotas = T){
                             self <- sustituir_muestra_ine(self, shp, id, zoom, dir = glue::glue("{self$dir.exportar}/Mapas"), ajustar_cuotas = ajustar_cuotas)

                             if(!file.exists(glue::glue("{self$dir.exportar}/Mapas/Eliminadas"))) dir.create(glue::glue("{self$dir.exportar}/Mapas/Eliminadas"))

                             file.rename(glue::glue("{self$dir.exportar}/Mapas/{id}.png"),
                                         glue::glue("{self$dir.exportar}/Mapas/Eliminadas/{id}.png"))

                             self$n_sustitucion <- self$n_sustitucion + 1
                             readr::write_rds(self, glue::glue("{self$dir.exportar}/diseño{self$n_sustitucion}.rda"))
                             self$cuotas %>% readr::write_excel_csv(glue::glue("{self$dir.exportar}/cuotas{self$n_sustitucion}.csv"))
                           }
                         ),
                         private = list(
                           unidad_muestreo=NULL
                         )
)


# Población ---------------------------------------------------------------


#' @export
PoblacionINE <- R6::R6Class("Poblacion",
                            public = list(
                              nombre = NULL,
                              marco_muestral=NULL,
                              informacion_electoral = NULL,
                              initialize=function(nombre,
                                                  ln,
                                                  electoral,
                                                  shp_mza,
                                                  shp_loc,
                                                  shp_mun){
                                self$nombre = nombre

                                self$marco_muestral = muestreaR::crear_mm_ine(ln = ln,
                                                                              shp_mza = shp_mza,
                                                                              shp_loc = shp_loc,
                                                                              shp_mun = shp_mun)

                                self$informacion_electoral <- electoral %>% mutate(seccion = as.character(seccion)) %>%
                                  left_join(self$marco_muestral %>% group_by(SECCION) %>%
                                              summarise(across(c(lista_nominal, contains("LN22_")), ~sum(.x))),
                                            by = c("seccion" = "SECCION"))

                              },
                              calcular_poblacion=function(na.rm=T){
                                res= sum(self$marco_muestral[,self$variable_poblacion],na.rm = na.rm)
                                return(res)
                              },
                              regiones = function(id, regiones){
                                self$marco_muestral<- regiones(self$marco_muestral,
                                                               id = id,
                                                               regiones = regiones)
                              }
                            )
)

#' @export

CartografiaINE <- R6::R6Class("Cartografia",
                              public = list(
                                shp = NULL,
                                initialize = function(
    df_shp = NULL,
    dl_shp = NULL,
    mun_shp = NULL,
    loc_shp = NULL,
    secc_shp = NULL,
    mza_shp = NULL
                                ){
                                  self$shp <- crear_shp_ine(df_shp, dl_shp, mun_shp, loc_shp, secc_shp,mza_shp)
                                },
    graficar_mapa = function(lflt = NULL, bd, nivel){
      nivel_p <- if(nivel == "MANZANA"){
        "MZA"
      } else nivel
      graficar_mapa_muestra_ine(lflt = lflt,
                                muestra = if(!is.data.frame(bd)) bd %>% purrr::pluck(nivel_p) %>% tidyr::unnest(data) else bd,
                                shp = self$shp,
                                nivel = nivel)
    },
    crear_mapas = function(diseño, zoom, dir){
      google_maps_ine(diseño, shp = self$shp, zoom = zoom, dir = dir)
    }
                              ))
