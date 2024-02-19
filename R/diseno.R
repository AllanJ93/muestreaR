

#' Title
#'
#' @param bd
#' @param grupo
#' @param tipo
#' @param i
#'
#' @return
#' @export
#' @import dplyr ggplot2
#' @examples
agregar_nivel <- function(bd, grupo, tipo, i){
  bd %>% group_by({{grupo}}, .add = T) %>%
    mutate(!!glue::glue("{tipo}_{i}"):= cur_group_id()) %>% ungroup
}

#' Title
#'
#' @param bd
#'
#' @return
#' @export
#'
#' @examples

calcular_fpc <- function(diseño, nivel = 1){
  nombres <- names(diseño$poblacion$marco_muestral)
  nivel_anterior <- diseño$niveles %>% filter(nivel == (!!nivel - 1))
  nivel_principal <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel}"),
                          value = T )
  if(length(nivel_principal)!=1) stop("El nivel seleccionado no se encuestra en el marco muestral")
  # if(nivel_principal=="cluster_0") stop("Hay que arreglar esto")
  # if(nivel == diseño$ultimo_nivel) nivel_secundario <- "cluster_0"
  # else{
  #   nivel_secundario <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel+1}"),
  #                            value = T )
  #   if(length(nivel_secundario)==0) {
  #     warning("El nivel posterior no se encuentra en el marco muestral, se utiliza en cambio el último nivel")
  #     nivel_secundario <- "cluster_0"
  #   }
  # }

  aux <- if(nivel == 0){
    nivel_principal <- diseño$niveles %>% filter(nivel == max(nivel)) %>% pull(nivel)
    diseño$poblacion$marco_muestral %>% agrupar_nivel(nivel_principal) %>%
      mutate(manzanas = n()) %>%
      left_join(diseño$n_i %>% .[[grep(nombres,pattern = glue::glue("(strata|cluster)_{diseño$ultimo_nivel}"),
                                       value = T )]]) %>%
      mutate(
        fpc_0= sampling::inclusionprobabilities(n = unique(!!sym(glue::glue("m_{diseño$ultimo_nivel}"))), a = manzanas)
      ) %>% distinct(fpc_0) %>% ungroup
  } else{

    diseño$poblacion$marco_muestral %>% agrupar_nivel(readr::parse_number(nivel_principal)) %>%
      summarise(total = sum(!!sym(diseño$variable_poblacional),na.rm = T)) %>%
      left_join(diseño$n_i %>% .[[glue::glue("{nivel_anterior$tipo}_{nivel_anterior$nivel}")]]) %>%
      mutate(
        !!rlang::sym(glue::glue("fpc_{nivel}")) := sampling::inclusionprobabilities(n = unique(!!sym(glue::glue("m_{nivel-1}"))), a = total)
      ) %>% ungroup %>% select(-total,-all_of(c(glue::glue("{c('m','n')}_{nivel-1}"))))
  }




  res <- diseño$poblacion$marco_muestral %>% left_join(aux)
  return(res)
}


#' Title
#'
#' @param bd
#' @param id
#' @param regiones
#'
#' @return
#' @export
#'
#' @examples
regiones <- function(bd, id, regiones){
  aux <- regiones %>% tibble::enframe(name = "region", value = id) %>% tidyr::unnest(all_of(id))
  faltan <- bd %>% anti_join(aux) %>% distinct(!!sym(id)) %>% pull(1) %>% paste(collapse = ", ")
  if(faltan != ""){
    warning(glue::glue("No se ha clasificado los siguientes {id}: {faltan}. \n Favor de agregarlos a la lista regiones"))
  }

  error <- aux %>% anti_join(bd) %>% distinct(!!sym(id)) %>% pull(1) %>% paste(collapse = ", ")
  if(error != ""){
    warning(glue::glue("Los siguientes {id} no existen en la base de datos: {error}. \n Favor de rectificar la lista de regiones"))
  }

  bd %>% left_join(
    aux
  ) %>% select(region, everything())
}

nivel <- function(aux, nivel, grupo, tipo, n, peso_tamaño, criterio_n){
  aux <- etiquetar(aux, grupo, tipo, nivel)

  nivel <- aux %>% ungroup %>% select(last_col()) %>% names
  if(!grepl("strata",nivel)){
    nivel_anterior <- aux %>% ungroup %>% select(last_col()-1) %>% names
    if(grepl("fpc",nivel_anterior)) nivel_anterior <- aux %>% ungroup() %>% select(last_col()-2) %>% names
    base_n <- criterio_N(base = aux %>% ungroup, variable_estrato = !!sym(nivel_anterior),
                         variable_estudio = {{peso_tamaño}},num = n, tipo = criterio_n)

  } else{
    base_n <- NULL
  }

  aux <- aux %>% calcular_fpc(n_grupo = base_n$n, peso_tamaño = {{peso_tamaño}})
  return(aux)
}
#' #' Title
#' #'
#' #' @param bd
#' #' @param grupo
#' #' @param tipo
#' #' @param peso
#' #' @param metodo
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' empaquetar <- function(bd, grupo, tipo, n, peso_tamaño, criterio_n = "peso"){
#'   aux <- bd
#'   for(i in seq_along(tipo)){
#'
#'     aux <- nivel(aux, i, grupo[i], tipo[i], n[i], peso_tamaño, criterio_n = "peso")
#'
#'   }
#'
#'   return(aux %>% ungroup)
#' }
#'

#' #' Title
#' #'
#' #' @param base
#' #' @param variable_estrato
#' #' @param variable_estudio
#' #' @param estimador
#' #' @param n_k
#' #' @param base_n
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' calcular_varianza_estratificada <- function(base, variable_estrato, variable_estudio,estimador=NULL, n_k=NULL, base_n=NULL) {
#'   if(is.null(estimador)) {
#'
#'     bd <- base %>%
#'       group_by({{variable_estrato}}) %>%
#'       summarise(varianza=var({{variable_estudio}}, na.rm=T)) %>%
#'       summarise(suma_varianza=sum(varianza, na.rm=T)) %>%
#'       pull(suma_varianza)
#'
#'   }
#'
#'   else {
#'
#'     if(estimador=="t") {
#'       bd <- base %>%
#'         filter(!is.na({{variable_estudio}})) %>%
#'         group_by({{variable_estrato}}) %>%
#'         summarise(varianza=var({{variable_estudio}}, na.rm=T),
#'                   N=n()) %>%
#'         left_join(base_n) %>%
#'         mutate(
#'           varianza=((varianza*(N^2))/n)*(1-n/N)) %>%
#'         summarise(suma_varianza=sum(varianza, na.rm=T)) %>%
#'         pull(suma_varianza)
#'
#'     }
#'
#'   }
#'   return(bd)
#'
#'
#' }
#'
#'

#' #' Title
#' #'
#' #' @param base
#' #' @param n
#' #' @param variable_estudio
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' calcular_varianza_mas <- function(base, n, variable_estudio) {
#'   bd <- base %>%
#'     filter(!is.na({{variable_estudio}})) %>%
#'     summarise(N=n(),
#'               varianza=var({{variable_estudio}}, na.rm=T)) %>%
#'     mutate(varianza_t=((N^2)*(1-n/N)*varianza)/n) %>%
#'     pull(varianza_t)
#'
#'   return(bd)
#'
#' }
#'


#' #' Title
#' #'
#' #' @param base
#' #' @param variable_estrato
#' #' @param variable_estudio
#' #' @param num
#' #' @param tipo
#' #' @param variable_peso
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #'
#' criterio_N <- function(base,
#'                        nivel,
#'                        variable_estudio=NULL,
#'                        num,
#'                        criterio = "unidades",
#'                        ultimo_nivel=F) {
#'   nombres <- names(base)
#'   nivel_principal <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel}"),
#'                           value = T )
#'   if(length(nivel_principal)!=1) stop("El nivel seleccionado no se encuestra en el marco muestral")
#'   if(nivel_principal=="cluster_0") stop("Hay que arreglar esto")
#'   if(ultimo_nivel) nivel_secundario <- "cluster_0"
#'   else{
#'     nivel_secundario <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel+1}"),
#'                              value = T )
#'     if(length(nivel_secundario)==0) {
#'       warning("El nivel posterior no se encuentra en el marco muestral, se utiliza en cambio el último nivel")
#'       nivel_secundario <- "cluster_0"
#'     }
#'   }
#'   base <- base %>%
#'     agrupar_nivel(nivel=nivel)
#'   if(criterio == "unidades"){
#'     res <- base %>%
#'       filter(!is.na(!!sym(variable_estudio))) %>%
#'       summarise(N=n_distinct(!!sym(nivel_secundario))) %>%
#'       mutate(n = ceiling(num*N/sum(N))) %>% select(-N)
#'   }
#'
#'   if(criterio == "peso"){
#'
#'     res <- base %>%
#'       filter(!is.na(!!sym(variable_estudio))) %>%
#'       summarise(N=sum(!!sym(variable_estudio))) %>%
#'       mutate(n = min(c(round(num*N/sum(N))),N)) %>% select(-N)
#'   }
#'
#'   if(criterio == "uniforme"){
#'     res <- base %>%
#'       summarise(N=n_distinct(!!sym(nivel_secundario))) %>%
#'       mutate(n = min(c(num, N)))
#'   }
#'
#'   return(res)
#' }
#
# criterio_m <- function(base,    nivel,
#                        criterio = "unidades",
#                        variable_tamaño=NULL,
#                        num,
#                        ultimo_nivel=F) {
#
#   nombres <- names(base)
#   nivel_principal <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel}"),
#                           value = T )
#   if(length(nivel_principal)!=1) stop("El nivel seleccionado no se encuestra en el marco muestral")
#   if(nivel_principal=="cluster_0") stop("Hay que arreglar esto")
#   if(ultimo_nivel) nivel_secundario <- "cluster_0"
#   else{
#     nivel_secundario <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel+1}"),
#                              value = T )
#     if(length(nivel_secundario)==0) {
#       warning("El nivel posterior no se encuentra en el marco muestral, se utiliza en cambio el último nivel")
#       nivel_secundario <- "cluster_0"
#     }
#   }
#   base <- base %>%
#     agrupar_nivel(nivel=nivel)
#   if(criterio == "unidades"){
#     res <- base %>%
#       filter(!is.na(!!variable_tamaño)) %>%
#       summarise(N=n_distinct(!!sym(nivel_secundario))) %>%
#       mutate(n = ceiling(m_i*N/sum(N))) %>%
#       select(-N)
#   }
#
#   if(criterio == "peso"){
#
#     res <- base %>%
#       filter(!is.na(!!variable_estudio)) %>%
#       summarise(N=sum(!!variable_estudio)) %>%
#       mutate(n = min(c(round(num*N/sum(N))),N)) %>%
#       select(-N)
#   }
#
#   if(criterio == "uniforme"){
#     res <- base %>%
#       summarise(N=n_distinct(!!sym(nivel_secundario))) %>%
#       mutate(n = min(c(num, N)))
#   }
#
#   return(res)
# }
#
# criterio_n <- function(plan_muestreo,
#                        nivel,
#                        variable_tamaño,
#                        marco_muestral,
#                        n,
#                        m_i,
#                        n_0) {
#   if(nivel==0){
#     res <- marco_muestral %>%
#       group_by(cluster_0) %>%
#       summarise(n_0=n_0) %>%
#       mutate(m_0=ceiling(n/n_0))
#     res <- list(cluster_0=res)
#   }
#   if(nivel!=ultimo_nivel){
#     if(nivel==1){
#       clase <- "strata"
#       print("Clase fija, cuidado")
#       if(clase=="strata"){
#         ve <- "POBTOT"
#         print("Un monton de cosas fijas! CUIDADO")
#         res_2 <- criterio_N(base = marco_muestral,
#                             nivel = nivel,
#                             variable_estudio = sym(ve),
#                             num = m_i, criterio = "unidades",
#                             ultimo_nivel = F) %>%
#           rename("m_{nivel}":=n)
#         # Para n
#         res <- marco_muestral %>%
#           agrupar_nivel(nivel=nivel) %>%
#           summarise(nn:=sum({{variable_tamaño}})) %>%
#           mutate("n_{nivel}":=round(n*nn/sum(nn))) %>%
#           select(-nn)
#         res <- inner_join(res, res_2)
#
#       }
#
# Para m

#   nombre <- grep(pattern = glue::glue("(cluster|strata)_{nivel}"),
#                  x = names(res),value = T)
#   # Falta la m=peso, uniforme, unidades
#
#   res <- set_names(list(res), nombre)
# }
# if(nivel>1){
#
#   pp <- plan_muestreo[grep(pattern = glue::glue("(cluster|strata)_{nivel-1}"),
#                            names(plan_muestreo))]
#   nivel_secundario <- plan_muestreo[grep(pattern = glue::glue("(cluster|strata)_{nivel+1}"),
#                                          names(plan_muestreo))]
#   if(length(nivel_secundario)==0) nivel_secundario="cluster_0"
#   marco_agrupado <- agrupar_nivel(marco_muestral, nivel = nivel)
#   marco_agrupado <- marco_agrupado %>% nest()%>%
#     left_join(pp[[1]])
# res2 <- map(marco_agrupado$data,
#             ~criterio_N(base = .x,
#                         nivel = nivel,
#                         variable_estudio = sym(ve),
#                         num = 5,
#                         criterio = "unidades",
#                         ultimo_nivel = F) %>%
#               rename("m_{nivel}":=n))
#       res_2 <- criterio_N(base = marco_muestral,
#                           nivel = nivel,
#                           variable_estudio = sym(ve),
#                           num = 5,
#                           criterio = "uniforme",
#                           ultimo_nivel = F) %>%
#         rename("m_{nivel}":=n) %>% select(-N)
#
#       res <- marco_muestral %>%
#         agrupar_nivel(nivel=nivel) %>%
#         summarise() %>%
#         left_join(pp[[1]]) %>%
#         mutate("n_{nivel}":=round(!!sym(glue::glue("n_{nivel-1}"))/
#                                     !!sym(glue::glue("m_{nivel-1}")))) %>%
#         agrupar_nivel(nivel = nivel) %>%
#         select(matches(glue::glue("n_{nivel}")))
#       res <- inner_join(res, res_2)
#
#       nombre <- grep(pattern = glue::glue("(cluster|strata)_{nivel}"),
#                      x = names(res),value = T)
#       # Falta la m=peso, uniforme, unidades
#
#       res <- set_names(list(res), nombre)
#
#
#     }
#   }
#   else{
#     plan_muestreo["cluster_0"]
#     res <- marco_muestral %>%
#       agrupar_nivel(nivel)
#     mutate()
#     res <- list(cluster_0=res)
#
#   }
#
#
#   return(c(plan_muestreo,res))
# }
#

asignar_m <- function(diseño, criterio, unidades_nivel, manual){
  # Se elige la unidad de muestreo
  un_muestreo <- diseño$niveles %>% filter(!plan_muestra) %>% slice_min(n=1, order_by = nivel)
  # Se cuentan las unidades secundarias de muestreo
  # Sirve para saber si hay suficientes unidades secundarias
  aux <- diseño$poblacion$marco_muestral %>%
    agrupar_nivel(un_muestreo$nivel) %>% {
      if(un_muestreo$nivel==diseño$ultimo_nivel){
        summarise(.,n=n_distinct(cluster_0))
      }
      else
        summarise(.,across(matches(glue::glue("(strata|cluster)_{un_muestreo$nivel+1}")),
                           ~n_distinct(.x),
                           .names = "n"))
    }
  if(un_muestreo$nivel==diseño$ultimo_nivel){
    warning("Por ser el último nivel el criterio de repartición está determinado")
    res <- diseño$poblacion$marco_muestral %>%
      agrupar_nivel(un_muestreo$nivel) %>%
      summarise() %>%
      left_join(diseño$n_i %>% last()) %>%
      # Aquí se fuerza al número de manzanas determinada
      mutate("m_{un_muestreo$nivel}":=round((!!sym(glue::glue("n_{un_muestreo$nivel-1}"))/
                                               !!sym(glue::glue("m_{un_muestreo$nivel-1}")) )/
                                              diseño$n_0)) %>%
      select(-all_of(c(glue::glue("n_{un_muestreo$nivel-1}"),
                       glue::glue("m_{un_muestreo$nivel-1}"))))
  }
  else{
    if(criterio=="uniforme"){
      res <- diseño$poblacion$marco_muestral %>%
        agrupar_nivel(un_muestreo$nivel) %>%
        summarise() %>%
        mutate("m_{un_muestreo$nivel}":= round(unidades_nivel/un_muestreo$unidades)) %>%
        select(starts_with("strata_"),
               starts_with("cluster_"),
               glue::glue("m_{un_muestreo$nivel}"))
    }
    if(criterio == "manual"){
      res <- diseño$poblacion$marco_muestral %>%
        agrupar_nivel(un_muestreo$nivel) %>%
        summarise() %>% mutate("m_{un_muestreo$nivel}":= manual)
    }
    if(criterio=="unidades"){
      res <- aux %>%
        mutate("m_{un_muestreo$nivel}":= repartir_cociente(unidades_nivel,unidades_nivel*n/sum(n)))
    }
    if(criterio=="peso"){
      # Se utiliza la variable poblacional de tamaño para repartir las unidades de nivel
      # según el peso. Hasta aquí no es relativo al grupo anterior, es una repartición global.
      res <- diseño$poblacion$marco_muestral %>%
        agrupar_nivel(un_muestreo$nivel) %>%
        summarise(n=sum(!!sym(diseño$variable_poblacional))) %>%
        # agrupar_nivel(un_muestreo$nivel) %>%
        mutate("m_{un_muestreo$nivel}":= repartir_cociente(unidades_nivel,unidades_nivel*n/sum(n))) %>%
        # mutate("m_{un_muestreo$nivel}":= round(unidades_nivel/n_groups(.)*n/sum(n))) %>%
        select(-n)
    }

  }
  # Se elije el mínimo entre las unidades previstas en el plan de muestreo y las posibles
  res <- res %>% left_join(aux) %>%
    mutate("m_{un_muestreo$nivel}":= if_else(!!sym(glue::glue("m_{un_muestreo$nivel}")) < n,
                                             !!sym(glue::glue("m_{un_muestreo$nivel}")), as.numeric(n))) %>%
    select(-n)
  # Se asigna el número total de unidades de muestreo
  # Cuando es strata se asigna así mismo y al siguiente
  if(un_muestreo$tipo=="strata"){
    diseño$niveles <-  diseño$niveles %>%
      # El número de estratos. Sirve para el nivel 1, hay que checar para otros niveles
      mutate(unidades=if_else(nivel==un_muestreo$nivel,
                              as.numeric(nrow(res)), unidades))
  }
  # Siempre se asigna al siguiente

  diseño$niveles <- diseño$niveles %>%
    mutate(unidades=if_else(nivel==un_muestreo$nivel+1,
                            if_else(un_muestreo$nivel==1,res %>%
                                      ungroup() %>%
                                      summarise(n=sum(!!sym(glue::glue("m_{un_muestreo$nivel}")))) %>%
                                      pull(n),
                                    res %>%
                                      ungroup() %>%
                                      summarise(n=mean(!!sym(glue::glue("m_{un_muestreo$nivel}")))) %>%
                                      pull(n) * un_muestreo$unidades),
                            unidades))
  return(res)
}

asignar_n <- function(diseño){
  un_muestreo <- diseño$niveles %>%
    filter(!plan_muestra) %>%
    slice_min(n=1, order_by = nivel)
  if(un_muestreo$tipo=="strata"){
    res <- diseño$poblacion$marco_muestral %>%
      agrupar_nivel(un_muestreo$nivel) %>%
      summarise(n=sum(!!sym(diseño$variable_poblacional))) %>%
      mutate(n=n/sum(n))
    if(un_muestreo$nivel==1) res <- res %>% mutate(.,n_1=diseño$n*n) %>% select(-n)
    else{
      anterior <- diseño$niveles %>% filter(nivel==un_muestreo$nivel-1)
      diseño$n_i[[glue::glue("{anterior$tipo}_{anterior$nivel}")]] %>%
        left_join(res)
    }

  }
  if(un_muestreo$tipo=="cluster"){
    anterior <- diseño$niveles %>% filter(nivel==un_muestreo$nivel-1)
    res <- diseño$poblacion$marco_muestral %>%
      agrupar_nivel(un_muestreo$nivel) %>%
      summarise()
    res <- res %>% left_join(diseño$n_i[[glue::glue("{anterior$tipo}_{anterior$nivel}")]] %>%
                               mutate("n_{un_muestreo$nivel}":=!!sym(glue::glue("n_{anterior$nivel}"))/!!sym(glue::glue("m_{anterior$nivel}"))) %>%
                               select(starts_with("cluster"),
                                      starts_with("strata"),
                                      glue::glue("n_{un_muestreo$nivel}")))
  }
  return(res)
}



#' Title
#'
#' @param base
#' @param nivel
#' @param variable_estudio
#' @param bd_n
#'
#' @return
#' @export
#'
#' @examples
muestrear <- function(diseño, nivel){

  nombres <- names(diseño$poblacion$marco_muestral)
  nivel_principal <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel}"),
                          value = T )
  if(length(nivel_principal)!=1) stop("El nivel seleccionado no se encuestra en el marco muestral")
  if(nivel_principal=="cluster_0") stop("Hay que arreglar esto")
  if(nivel == diseño$ultimo_nivel) nivel_secundario <- "cluster_0"
  else{
    nivel_secundario <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel+1}"),
                             value = T ) %>% readr::parse_number()
    if(length(nivel_secundario)==0) {
      warning("El nivel posterior no se encuentra en el marco muestral, se utiliza en cambio el último nivel")
      nivel_secundario <- "cluster_0"
    }
  }

  bd <- if(is.null(diseño$muestra)) diseño$poblacion$marco_muestral else diseño$muestra %>% purrr::pluck(length(diseño$muestra)) %>% tidyr::unnest(data)
  muestra  <- bd %>%
    agrupar_nivel(nivel_secundario) %>%
    mutate(total = sum(!!sym(diseño$variable_poblacional),na.rm = T)) %>%
    group_by(total, .add = T) %>%
    tidyr::nest() %>%
    ungroup() %>%
    # semi_join(
    #   diseño$n_i %>% .[[nivel_principal]] %>% filter(!!rlang::sym(glue::glue("m_{nivel}")) >0)
    # ) %>%
    split(.[[nivel_principal]]) %>% purrr::map_df(~{
      n_nivel <- diseño$n_i %>% .[[nivel_principal]] %>%
        filter(!!sym(nivel_principal) == unique(.x[[nivel_principal]])) %>% pull(glue::glue("m_{nivel}"))

      if(nivel_secundario == "cluster_0") sorteado <- .x %>% slice_sample(n = n_nivel) else{
        if(is.null(diseño$sobre_muestra)){
          no_necesita_sm <- T
        } else{
          no_necesita_sm <- diseño$sobre_muestra %>% semi_join(.x) %>% nrow() == 0
        }

        if(no_necesita_sm){
          sorteado <-  .x %>% slice_sample(weight_by = total,n = n_nivel)
        } else{
          n_nivel_original <- diseño$sobre_muestra %>% semi_join(.x) %>% pull(m_1_vieja)
          sorteo_normal <- .x %>% slice_sample(weight_by = total,n = n_nivel_original)

          ya <- sorteo_normal %>% semi_join(
            diseño$poblacion$marco_muestral %>% semi_join(diseño$sobre_muestra)
          )

          n_nivel_sm <- diseño$sobre_muestra %>% semi_join(.x) %>% pull(m_sm) -nrow(ya)

          sorteo_sm <- .x %>% semi_join(
            diseño$poblacion$marco_muestral %>% semi_join(diseño$sobre_muestra)
          ) %>%
            anti_join(sorteo_normal)

          if(nrow(sorteo_sm) > n_nivel_sm) {
           sorteo_sm <- sorteo_sm %>%
             slice_sample(weight_by = total,n = n_nivel_sm)
          }

          sorteado <- bind_rows(sorteo_normal, sorteo_sm)

        }

      }


      return(sorteado)
    })

  nombre <- diseño$niveles %>% filter(nivel == !!nivel+1) %>% pull(variable)
  if(length(nombre) == 0) nombre <- "MZA"
  res <- diseño$muestra %>% append(list(muestra) %>% purrr::set_names(nombre))
  return(res)

}

#' Title
#'
#' @param dise
#'
#' @return
#' @export
#'
#' @examples
#'
cuotas <- function(diseño){
  u_nivel <- diseño$niveles %>% filter(nivel == diseño$ultimo_nivel)
  u_cluster <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
  muestra <- diseño$muestra %>% purrr::pluck(length(diseño$muestra))
  bd <- diseño$poblacion$marco_muestral %>%
    semi_join(muestra %>% distinct(!!rlang::sym(u_cluster)))

  ent <- muestra %>% left_join(diseño$n_i$cluster_0) %>% count(!!rlang::sym(u_cluster), wt = n_0, name = "entrevistas")
  cuotas <- bd %>% transmute(Municipio = NOM_MUN,
                             Localidad = NOM_LOC,
                             !!rlang::sym(u_cluster),
                             P_18A24_F,
                             P_18A24_M,
                             P_25A59_F = P_18YMAS_F - P_18A24_F - P_60YMAS_F,
                             P_25A59_M = P_18YMAS_M - P_18A24_M - P_60YMAS_M,
                             P_60YMAS_F,P_60YMAS_M) %>%
    group_by(Municipio, Localidad,!!rlang::sym(u_cluster)) %>%
    summarise(across(everything(),.fns = sum, na.rm = T),.groups =  "drop") %>%
    tidyr::pivot_longer(c(-Municipio,-Localidad,-!!sym(u_cluster)), names_to = "edad", values_to = "cantidad") %>%
    tidyr::separate(edad, c("basura","rango","sexo")) %>% select(-basura) %>%
    group_by(!!rlang::sym(u_cluster)) %>% mutate(pct = cantidad/sum(cantidad)) %>% ungroup %>%
    left_join(
      ent
    ) %>% mutate(n =  round(pct*entrevistas))

  revision <- cuotas %>% count(!!rlang::sym(u_cluster), wt = n)

  c <- revision %>% left_join(ent) %>% mutate(diff = entrevistas - n) %>%
    select(a = 1,b = 4) %>%
    purrr::pmap_df(function(a, b){
      aux <- cuotas %>% filter(!!rlang::sym(u_cluster) == !! a)
      ya <- abs(b)
      while(ya != 0){
        alea <- min(nrow(aux), ya)
        aleatorio <- sample(x = seq_len(nrow(aux)), size = alea)
        aux[aleatorio, "n"] <- aux[aleatorio, "n"] + sign(b)
        ya <- ya - alea
      }

      return (aux)
    })

  cool <- c %>% count(!!rlang::sym(u_cluster), wt = n) %>% left_join(ent) %>% filter(n != entrevistas)
  if(nrow(cool) == 0 & nrow(c) == nrow(cuotas)) print("Exacto")
  return(c %>% select(-cantidad,-pct,-entrevistas))
}

#' Title
#'
#' @param dise
#'
#' @return
#' @export
#'
#' @examples
cuotas_ine <- function(diseño, ajustar){
  u_nivel <- diseño$niveles %>% filter(nivel == diseño$ultimo_nivel)
  u_cluster <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
  muestra <- diseño$muestra %>% purrr::pluck(length(diseño$muestra))
  bd <- diseño$poblacion$marco_muestral %>%
    semi_join(muestra %>% distinct(!!rlang::sym(u_cluster)))

  ent <- muestra %>% left_join(diseño$n_i$cluster_0) %>% count(!!rlang::sym(u_cluster), wt = n_0, name = "entrevistas")

  cuotas <- bd %>% select(Municipio = NOMBRE_MUN,
                          Seccion = SECCION,
                          !!rlang::sym(u_cluster),
                          contains("LN22")) %>%
    group_by(Municipio, Seccion,!!rlang::sym(u_cluster)) %>%
    summarise(across(everything(),.fns = sum, na.rm = T),.groups =  "drop") %>%
    tidyr::pivot_longer(c(-Municipio, -Seccion,-!!sym(u_cluster)), names_to = "edad", values_to = "cantidad") %>%
    tidyr::separate(edad, c("basura","rango","sexo")) %>% select(-basura) %>%
    group_by(!!rlang::sym(u_cluster)) %>% mutate(pct = cantidad/sum(cantidad)) %>% ungroup %>%
    left_join(
      ent
    ) %>% mutate(n =  round(pct*entrevistas))

  if(ajustar){
    revision <- cuotas %>% count(!!rlang::sym(u_cluster), wt = n)

    ci <- revision %>% left_join(ent) %>% mutate(diff = entrevistas - n) %>%
      select(a = 1,b = 4) %>%
      purrr::pmap_df(function(a, b){
        aux <- cuotas %>% filter(!!rlang::sym(u_cluster) == !! a)
        ya <- abs(b)
        while(ya != 0){
          alea <- min(nrow(aux), ya)
          aleatorio <- sample(x = seq_len(nrow(aux)), size = alea)
          aux[aleatorio, "n"] <- aux[aleatorio, "n"] + sign(b)
          ya <- ya - alea
        }

        return (aux)
      })

    cool <- ci %>% count(!!rlang::sym(u_cluster), wt = n) %>% left_join(ent) %>% filter(n != entrevistas)
    if(nrow(cool) == 0 & nrow(ci) == nrow(cuotas)) print("Exacto")
    ci <- ci %>% select(-cantidad,-pct,-entrevistas,-Seccion)
  } else{
    ci <- cuotas
  }

  return(ci)
}
