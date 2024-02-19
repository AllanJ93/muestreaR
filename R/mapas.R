#' Title
#'
#' @param bd
#' @param shp
#' @param nivel
#' @param variable
#'
#' @return
#' @export
#' @import leaflet
#' @examples
graficar_mapa_poblacion <- function(bd, shp, nivel, variable){
  aux <- shp %>% purrr::pluck(nivel) %>%
    inner_join(
      bd %>% count(across(all_of(nivel)), wt = across(all_of(variable)))
    )

  pal <- leaflet::colorNumeric(palette = "Reds", domain = aux$n)

  aux <- aux %>% mutate(color = pal(n))

  leaflet::leaflet() %>%
    leaflet::addProviderTiles("CartoDB.DarkMatter") %>%
    leaflet::addPolygons(data = aux %>% filter(sf::st_geometry_type(.) != "POINT"),
                         fillColor = ~color, fillOpacity = .7, weight = 1,stroke = T, color = "black") %>%
    leaflet::addCircleMarkers(data = aux %>% filter(sf::st_geometry_type(.) == "POINT"),
                              radius = 1, color = ~color, fillOpacity = 1,stroke = T,opacity = 1, , weight = 1) %>%
    leaflet::addLegend(data = aux, pal = pal, values = ~n, title = variable)


}

#' Title
#'
#' @param lflt
#' @param muestra
#' @param shp
#' @param nivel
#'
#' @return
#' @export
#'
#' @examples
graficar_mapa_muestra <- function(lflt = NULL, muestra, shp, nivel){
  pal <- if(nivel == "MUN"){
    colorFactor(topo.colors(n_distinct(muestra$strata_1)), domain = unique(muestra$strata_1))
  } else{
    colorFactor(c("orange","red"),c("LOC","AGEB"))
  }

  mapa <- if(is.null(lflt)){
    shp %>% purrr::pluck(nivel) %>%
      left_join(muestra %>% distinct(MUN,strata_1)) %>%
      group_by(strata_1) %>% summarise(n()) %>%
      sf::st_buffer(dist = 0) %>%
      leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = ~pal(strata_1), opacity = 1, fill = F) %>%
      addLegend(pal = pal, values = ~strata_1, position = "bottomleft")
  } else{
    if(nivel == "MUN"){
      lflt %>% addPolygons(data = shp %>% purrr::pluck(nivel) %>% inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T)),
                           color = ~pal(strata_1), fillOpacity = 1, label = ~glue::glue("Municipio: {NOM_MUN}"))
    } else{
      mapear <- shp %>% purrr::pluck(nivel) %>% inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T)) %>%
        tidyr::separate(!!sym(nivel),c("CVEGEO","nivel","tipo"))

      if(!"Nombre" %in% names(mapear)){
        mapear <- mapear %>% mutate(Nombre = "")
      }
      lflt %>% addPolygons(data = mapear %>%
                             filter(sf::st_geometry_type(.) != "POINT"), stroke = T, color = "black",
                           fillColor = ~pal(nivel), fillOpacity = 1,weight = 1, opacity = 1,
                           popup = ~glue::glue("Tipo: {nivel} <br>
                                                Ámbito: {tipo} <br>
                                                CVEGEO: {CVEGEO}
                                               "))  %>%
        addCircleMarkers(data = mapear %>% filter(sf::st_geometry_type(.) == "POINT"),
                         radius = 3, color = "red", weight = 1,
                         clusterOptions = markerClusterOptions(),
                         popup = ~glue::glue("Nombre: {Nombre} <br>
                                              Tipo: {nivel} <br>
                                              Ámbito: {tipo} <br>
                                              CVEGEO: {CVEGEO}
                                               ")
        ) %>% addLegend(data = mapear, pal = pal, values = ~nivel)
    }

  }

  return(mapa)
}

#' Title
#'
#' @param lflt
#' @param muestra
#' @param shp
#' @param nivel
#'
#' @return
#' @export
#'
#' @examples
graficar_mapa_muestra_ine <- function(lflt = NULL, muestra, shp, nivel){
  pal <- if(nivel == "MUNICIPIO"){
    colorFactor(topo.colors(n_distinct(muestra$strata_1)), domain = unique(muestra$strata_1))
  } else{
    colorFactor(c("orange","red"),c("LOCALIDAD","SECCION"))
  }

  mapa <- if(is.null(lflt)){
    shp %>% purrr::pluck(nivel) %>%
      left_join(muestra %>% distinct(MUNICIPIO,strata_1)) %>%
      group_by(strata_1) %>% summarise(n()) %>%
      sf::st_buffer(dist = 0) %>%
      leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = ~pal(strata_1), opacity = 1, fill = F) %>%
      addLegend(pal = pal, values = ~strata_1, position = "bottomleft")
  } else{
    if(nivel == "MUNICIPIO"){
      lflt %>% addPolygons(data = shp %>% purrr::pluck(nivel) %>% inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T)),
                           fillColor = ~pal(strata_1), color = "black", opacity = 1, weight = 1, fillOpacity = 1, label = ~glue::glue("Municipio: {NOMBRE_MUN}"))
    } else{
      if(nivel == "MANZANA"){
        mapear <- shp %>% purrr::pluck(nivel) %>% inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T))

        lflt %>%
          addCircleMarkers(data = mapear %>% filter(sf::st_geometry_type(.) == "POINT"),
                              label = ~glue::glue("Localidad: {MANZANA}"), opacity = 1, fillOpacity = 1,
                              fillColor = "#f72585", color = "black", weight = 1) %>%
          addLegend(position = "bottomright", colors = "#f72585", labels = "Localidades rurales")

      } else{
        mapear <- shp %>% purrr::pluck(nivel) %>% inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T))

        nivel <- mapear %>% as_tibble %>% select(contains("cluster")) %>% names %>% parse_number %>% max

        popup_cluster <- paste0("cluster_",nivel,": ", as_tibble(mapear)[[paste("cluster",nivel,sep = "_")]])
        popup_mun <- paste("Municipio: ", mapear$NOMBRE_MUN)
        lflt %>% addPolygons(data = mapear,
                             stroke = T, color = "black",
                             fillColor = ~pal(nivel), fillOpacity = .2,weight = 1, opacity = 1,
                             # popup = ~glue::glue("Sección: {SECCION}")
                             popup = paste(popup_mun, popup_cluster, sep = "<br>")
        ) %>%
          addLegend(data = mapear, pal = pal, values = ~nivel, position = "bottomright")
      }
    }
  }

  return(mapa)
}

#' Title
#'
#' @param dise
#' @param shp
#' @param zoom
#'
#' @return
#' @export
#'
#' @examples
google_maps <- function(diseño, shp, zoom, dir = "Mapas"){

  u_nivel <- diseño$niveles %>% filter(nivel == diseño$ultimo_nivel)
  u_cluster <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
  bd <- diseño$muestra %>% purrr::pluck(length(diseño$muestra)) %>% tidyr::unnest(data)

  cluster <- bd %>% distinct(!!rlang::sym(u_cluster)) %>% pull(1)
  ya <- list.files(path=dir) %>% gsub('^.*_\\s*|\\s*.png.*$', '', .)
  cluster <- cluster[!cluster %in% ya]
  # agebs <- agebs %>% mutate(CVE_AGEB = paste0(22,CVE_MUN,CVE_LOC,CVE_AGEB))
  shp_mapa <- shp %>% purrr::pluck(u_nivel %>% pull(variable)) %>% inner_join(bd)
  man_shp <- shp %>% purrr::pluck("MZA") %>% inner_join(bd)


  for(i in cluster){
    aux_s <- diseño$cuotas %>% filter(!!rlang::sym(u_cluster) == i)
    s <- aux_s %>%
      mutate(n = glue::glue("{n} entrevistas")) %>%
      tidyr::pivot_wider(names_from = c("rango", "sexo"),values_from = "n") %>% select(-1) %>%
      mutate(Total = glue::glue("{sum(aux_s$n)} entrevistas")) %>% relocate(Total,.before = 1)
    cuotas <- paste(s %>% names(), s, sep = ": ") %>% paste(collapse = "\n")
    man <- man_shp %>% filter(!!rlang::sym(u_cluster) == i)
    aux_mapeo <- shp_mapa %>% filter(!!rlang::sym(u_cluster) == i)
    caja <- aux_mapeo %>% sf::st_union() %>% sf::st_centroid() %>% sf::st_coordinates() %>% as.numeric()
    nc_map <- ggmap::get_map(location = caja, maptype = "roadmap",
                             source = "google",force = T, zoom = zoom)
    Google <- ggmap::ggmap(nc_map)
    # Google
    g <- Google +
      geom_sf(data = aux_mapeo,
              inherit.aes = F, alpha = 0, color = "blue") +
      geom_sf(data = man,
              inherit.aes = F, alpha = 0, color = "red") +
      # scale_x_continuous(limits = c(caja[1], caja[3])) + scale_y_continuous(limits = c(caja[2],caja[4])) +
      guides(fill = "none") +
      theme_minimal() +
      ggtitle(glue::glue("Municipio: {unique(aux_mapeo$NOM_MUN)} \n Localidad: {unique(aux_mapeo$NOM_LOC)}  \n {u_cluster}: {i}")) +
      labs(subtitle =  cuotas) +
      theme(plot.title = element_text(hjust = 1), plot.subtitle = element_text(size = 10, hjust = 0))

    ggsave(g, filename= sprintf("%s.png", i),
           path=dir,width = 11,height = 8.5,units = "in",dpi = "print")
  }
  beepr::beep()

}

google_maps_ine <- function(diseño, shp, zoom, dir = "Mapas"){

  u_nivel <- diseño$niveles %>% filter(nivel == diseño$ultimo_nivel)
  u_cluster <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
  bd <- diseño$muestra %>% purrr::pluck(length(diseño$muestra)) %>% tidyr::unnest(data)

  cluster <- bd %>% distinct(!!rlang::sym(u_cluster)) %>% pull(1)
  ya <- list.files(path=dir) %>% gsub('^.*_\\s*|\\s*.png.*$', '', .)
  cluster <- cluster[!cluster %in% ya]
  # agebs <- agebs %>% mutate(CVE_AGEB = paste0(22,CVE_MUN,CVE_LOC,CVE_AGEB))
  shp_mapa <- shp %>% purrr::pluck(u_nivel %>% pull(variable)) %>% inner_join(bd)
  man_shp <- shp %>% purrr::pluck("MANZANA") %>% inner_join(bd)


  for(i in cluster){
    aux_s <- diseño$cuotas %>% filter(!!rlang::sym(u_cluster) == i)
    s <- aux_s %>%
      mutate(n = glue::glue("{n} entrevistas")) %>%
      tidyr::pivot_wider(names_from = c("rango", "sexo"),values_from = "n") %>% select(-1) %>%
      mutate(Total = glue::glue("{sum(aux_s$n)} entrevistas")) %>% relocate(Total,.before = 1)
    cuotas <- paste(s %>% names(), s, sep = ": ") %>% paste(collapse = "\n")
    man <- man_shp %>% filter(!!rlang::sym(u_cluster) == i)
    aux_mapeo <- shp_mapa %>% filter(!!rlang::sym(u_cluster) == i)
    caja <- aux_mapeo %>% st_make_valid() %>% sf::st_union() %>% sf::st_centroid() %>% sf::st_coordinates() %>% as.numeric()
    nc_map <- ggmap::get_map(location = caja, maptype = "roadmap",
                             source = "google",force = T, zoom = zoom)
    Google <- ggmap::ggmap(nc_map)
    # Google
    puntos <- man %>% filter(sf::st_geometry_type(.) == "POINT")
    man <- man %>% filter(sf::st_geometry_type(.) != "POINT")
    g <- Google +
      geom_sf(data = aux_mapeo,
              inherit.aes = F, alpha = 0, color = "blue") +
      geom_sf(data = man,
              inherit.aes = F, alpha = 0, color = "red") +
      geom_sf(data = puntos,
              inherit.aes = F, alpha = 1, color = "red") +
      geom_sf_label(data = puntos, color = "red",
                    inherit.aes = F, aes(label = MANZANA), hjust = "inward",
                   vjust = "inward", size = 2) +
      # scale_x_continuous(limits = c(caja[1], caja[3])) + scale_y_continuous(limits = c(caja[2],caja[4])) +
      guides(fill = "none") +
      theme_minimal() +
      ggtitle(glue::glue("Municipio: {unique(aux_mapeo$NOMBRE_MUN)}  \n {u_cluster}: {i}")) +
      labs(subtitle =  cuotas) +
      theme(plot.title = element_text(hjust = 1), plot.subtitle = element_text(size = 10, hjust = 0))

    ggsave(g, filename= sprintf("%s.png", i),
           path=dir,width = 11,height = 8.5,units = "in",dpi = "print", bg = "white")
  }
  beepr::beep()

}
