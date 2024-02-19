#' Title
#'
#' @param var
#' @param tama
#' @param bandera
#'
#' @return
#' @export
#'
#' @examples
formato <- function(var, tamaño, bandera = 0){

  readr::parse_character(
    formatC({{var}}, width = tamaño, flag = bandera)
  )
}

#' Title
#'
#' @param a
#' @param b
#' @param cd
#'
#' @return
#' @export
#'
#' @examples
crear_mm <- function(mza,
                     loc,
                     loc_shp,
                     lpr_shp){
  # print(mza)
  # poblacion <- read_csv(mza,na = "*")
  poblacion <- mza
  parseN <- poblacion %>% select(POBTOT:last_col()) %>% select(where(is.character)) %>% names
  poblacion <- poblacion %>% mutate(across(all_of(parseN), ~readr::parse_double(.x,na = c("","NA","*","N/A"))))

  poblacion <- poblacion %>% mutate(MUN = formato(MUN, tamaño = 3),
                                    LOC = formato(LOC, tamaño = 4))

  murb <- poblacion %>% filter(!grepl("Total ",NOM_LOC)) %>% mutate(
    ENTIDAD = formato(ENTIDAD, tamaño = 2),
    MUN = formato(MUN, tamaño = 3),
    LOC = formato(LOC, tamaño = 4)
  )

  # loc_shp <- readOGR(dsn=loc_shp,encoding = "CP1252") %>% spTransform(CRS("+init=epsg:4326")) %>% st_as_sf()
  murb <- murb %>% left_join(loc_shp %>% as_tibble() %>%
                               transmute(ENTIDAD = formato(CVE_ENT, tamaño = 2),
                                         MUN =formato(CVE_MUN, tamaño = 3),
                                         LOC = formato(CVE_LOC, tamaño = 4),
                                         AMBITO))
  murb <- murb %>% mutate(tipo_localidad = "Localidad amanzanada",
                          ARLU = if_else(AMBITO == "Urbana", glue::glue("{ENTIDAD}{MUN}{LOC}-LOC-{AMBITO}"), glue::glue("{ENTIDAD}{MUN}{AGEB}-AGEB-{AMBITO}")),
                          AULR = if_else(AMBITO == "Urbana", glue::glue("{ENTIDAD}{MUN}{LOC}{AGEB}-AGEB-{AMBITO}"), glue::glue("{ENTIDAD}{MUN}{LOC}{AGEB}800-LOC-{AMBITO}")),
                          # ARLU = if_else(AMBITO == "Urbana", "LU", "AR"),
                          # AULR = if_else(AMBITO == "Urbana", "AU", "LR"),
                          tipo = "Localidad amanzanada")



  # localidad <- read_csv(loc,na = "*")
  localidad <- loc
  # ageb_shp <- st_read(ageb_shp)%>%
  #   st_transform(4326)

  loc_no_murb <- localidad %>% filter(!grepl("Total de|Localidades",NOM_LOC)) %>%
    mutate(
      ENTIDAD = formato(ENTIDAD, tamaño = 2),
      MUN =formato(MUN, tamaño = 3),
      LOC = formato(LOC, tamaño = 4)) %>%
    anti_join(murb %>% distinct(MUN, LOC)) %>%
    mutate(AMBITO = "Rural", tipo_localidad = "Localidad puntual")

  parseN2 <- loc_no_murb %>% select(POBTOT:VPH_SINTIC) %>% select(where(is.character)) %>% names
  loc_no_murb <- loc_no_murb %>%
    select(-(LONGITUD:ALTITUD)) %>%
    mutate(
      # LONGITUD=map_dbl(LONGITUD,~as.numeric(char2dms(.x,"°","'"))),
      # LATITUD =map_dbl(LATITUD,~as.numeric(char2dms(.x,"°","'"))),
      # ALTITUD = as.numeric(ALTITUD),
      across(all_of(parseN2), ~readr::parse_double(.x, na = c("","NA","*","N/A")))
    )

  # loc <- loc_no_murb %>% st_as_sf(coords = c("LONGITUD","LATITUD"), crs = 4326) %>%
  #   sf::st_join(ageb_shp %>% mutate(valid = sf::st_is_valid(geometry)) %>% filter(valid)) %>% as_tibble %>%
  #   select(MUN,LOC,AGEB = CVE_AGEB)

  loc_no_murb <- loc_no_murb %>% left_join(
    lpr_shp %>%
      tibble %>%
      transmute(ENTIDAD = formato(CVE_ENT,2),
                MUN = formato(CVE_MUN, 3),
                LOC = formato(CVE_LOC,4),
                AGEB = formato(CVE_AGEB,4),
                MZA = formato(CVE_MZA,3)
      )
  )

  loc_no_murb <- loc_no_murb %>% mutate(ARLU = glue::glue("{ENTIDAD}{MUN}{AGEB}-AGEB-Rural"),
                                        AULR = glue::glue("{ENTIDAD}{MUN}{LOC}{AGEB}800-LOC-Rural"),
                                        # ARLU = "AR",
                                        # AULR = "LR",
                                        tipo = "Localidad puntual rural")


  final <- murb %>%
    bind_rows(
      loc_no_murb
    ) %>% mutate(ENTIDAD = formato(ENTIDAD, 2),
                 MZA = formato(MZA, tamaño = 3),
                 MUN = paste0(ENTIDAD, MUN),
                 LOC = paste0(MUN, LOC),
                 AGEBR = if_else(tipo == "Localidad puntual rural",paste0(MUN, AGEB),paste0(LOC, AGEB)),
                 AGEB = paste0(LOC, AGEB),
                 MZA = paste0(AGEB, MZA)) %>%
    tibble::rownames_to_column("id")


  # yo <- yo %>% summarise(sum(POBTOT)) %>% pull(1)
  # urb <- localidad %>% slice(1) %>% pull(POBTOT)
  # list(
  #   identical(yo,urb),
  #   yo-urb,
  #   .x
  # )

  return(final)
}


#' Title
#'
#' @param mun
#' @param locU
#' @param agebR
#' @param agebU
#' @param locR
#' @param mza
#'
#' @return
#' @export
#'
#' @examples
crear_shp <- function(mun, locU, agebR, agebU, locR, mza){
  mun <- mun %>% transmute(MUN = CVEGEO, NOM_MUN = NOMGEO)

  locU <- locU %>% transmute(ARLU = glue::glue("{CVEGEO}-LOC-{AMBITO}"), NOM_LOC = NOMGEO, AMBITO = AMBITO)

  agebR <- agebR %>% transmute(ARLU = glue::glue("{CVEGEO}-AGEB-Rural"))

  agebU <- agebU %>% transmute(AULR = glue::glue("{CVEGEO}-AGEB-Urbana"))

  locR <-   locR %>% transmute(AULR = glue::glue("{CVEGEO}-LOC-Rural"), Nombre = NOMGEO)

  mza <- mza %>% transmute(MZA = CVEGEO, TIPOMZA)

  return(list(MUN = mun,
              ARLU = bind_rows(locU %>%
                                 filter(AMBITO == "Urbana"), agebR),
              AULR = bind_rows(agebU,locR),
              AGEB = agebU,
              MZA = mza
  )
  )
}


#' Title
#'
#' @param ln
#' @param shp_mza
#' @param shp_loc
#' @param shp_mun
#'
#' @return
#' @export
#'
#' @examples
#'
crear_mm_ine <- function(ln, shp_mza, shp_loc, shp_mun){
  shp_mza <- shp_mza %>% filter(st_is_valid(.), STATUS == 1)
  aux <- st_join(shp_loc, shp_mza %>% select(MANZANA))
  shp_lpr <- aux %>% filter(is.na(MANZANA)) %>% select(-MANZANA)

  if("LISTA" %in% colnames(shp_lpr)){
    shp_lpr <- shp_lpr %>% relocate(LISTA, .before = LOCALIDAD)
  }

  shp_lpr <- shp_lpr %>% as_tibble %>% rename(MANZANA = NOMBRE) %>%
    mutate(across(c(ENTIDAD:LOCALIDAD,-contains("LISTA")), ~as.character(.x))) %>%
    select(ENTIDAD:MANZANA) %>% mutate(TIPO = "rural")

  shp_mun <- shp_mun %>% as_tibble %>% rename(NOMBRE_MUN = NOMBRE) %>%
    mutate(across(ENTIDAD:MUNICIPIO, ~as.character(.x))) %>% select(ENTIDAD:NOMBRE_MUN)

  if("LISTA" %in% colnames(shp_mza)){
    shp_mza <- shp_mza %>% relocate(LISTA, .before = MANZANA)
  }

  shp_mza <- shp_mza %>% as_tibble %>% select(ENTIDAD:MANZANA) %>%
    mutate(across(c(ENTIDAD:MANZANA,-contains("LISTA")), ~as.character(.x))) %>% mutate(TIPO = "urbana")

  shp_mza <- shp_mza %>% bind_rows(shp_lpr) %>% arrange(as.numeric(SECCION))

  ln <- ln %>% select(SECCION, contains("LISTA_")) %>% pivot_longer(-SECCION, names_to = "sector",
                                                                    values_to = "n") %>%
    mutate(sector = gsub(pattern = "_18_",replacement =  "_18_18_",x =  sector),
           sector = gsub(pattern = "_19_",replacement =  "_19_19_",x =  sector),
           sector = gsub(pattern = "_Y_",replacement =  "_",x =  sector),
    ) %>%
    separate(sector, into = c("lista","ini","fin","sexo")) %>%
    # mutate(across(ini:fin, parse_number)) %>%
    mutate(fin = as.numeric(fin),
           fin = if_else(is.na(fin),200,fin),
           rango = cut(as.numeric(fin), c(17,24,39,59,Inf),
                       labels = paste0("LN22_",c("18A24","25A39","40A59","60YMAS")))) %>%
    count(SECCION,rango,sexo, wt = n) %>% mutate(sexo = if_else(sexo == "HOMBRES","M","F")) %>%
    unite(rango_sexo, rango:sexo) %>%
    pivot_wider(SECCION,names_from = rango_sexo,values_from = n) %>%
    left_join(ln %>% select(SECCION, `LISTA NOMINAL`)) %>%
    mutate(SECCION = as.character(SECCION)) %>% rename(lista_nominal = `LISTA NOMINAL`)

  ln_mza <- ln %>%
    left_join(
      shp_mza %>% count(SECCION, name = "n_mza")
    ) %>% mutate(across(2:lista_nominal, ~.x/n_mza)) %>% select(-n_mza)

  # ln_mza <- ln %>% #filter(SECCION == 3) %>%
  #   left_join(
  #     shp_mza %>% count(SECCION,SECCION2, name = "n_mza") %>%
  #       count(SECCION2), by = c("SECCION" = "SECCION2")
  #   ) %>% mutate(across(2:lista_nominal, ~.x/n)) %>% select(-n) %>%
  #   left_join(
  #     shp_mza %>% distinct(SECCION, SECCION2), by = c("SECCION" = "SECCION2")
  #   ) %>% select(-SECCION) %>% relocate(SECCION = SECCION.y) %>%
  #   left_join(
  #     shp_mza %>% count(SECCION, name = "n_mza")
  #   ) %>% mutate(across(2:lista_nominal, ~.x/n_mza)) %>% select(-n_mza) %>%
  #   filter(!is.na(SECCION))

  mza <- shp_mza %>% left_join(shp_mun) %>%
    left_join(ln_mza, by = "SECCION") %>%
    mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x))) %>%
    rownames_to_column(var = "id")

  if("LISTA" %in% colnames(mza)){
    mza <- mza %>%
      mutate(lista_nominal = if_else(is.na(LISTA) | LISTA == 0, lista_nominal, LISTA)) %>%
      filter(lista_nominal > 0) |>
      select(-LISTA)

    pct_general <- mza |> pivot_longer(starts_with("LN22")) |>
      filter(value != 0) |>
      count(name, wt = value) |>
      mutate(pct = n/sum(n)) |>
      select(-n)

    mza <- mza |>
      pivot_longer(starts_with("LN22")) |>
      left_join(pct_general, join_by(name)) |>
      mutate(value = if_else(value == 0, lista_nominal*pct, value)) |>
      select(-pct) |>
      pivot_wider(names_from = name, values_from = value)

  }

  return(mza)
}



#' Title
#'
#' @param df
#' @param dl
#' @param mun
#' @param loc
#' @param secc
#' @param mza
#'
#' @return
#' @export
#'
#' @examples
crear_shp_ine <- function(df, dl, mun, loc, secc, mza){
  df <- df %>% transmute(across(c(ENTIDAD,DISTRITO_F), ~as.character(.x))) %>% st_make_valid()

  dl <- dl %>% transmute(across(c(ENTIDAD,DISTRITO_L), ~as.character(.x))) %>% st_make_valid()

  mun <- mun %>% rename(NOMBRE_MUN = NOMBRE) %>%
    mutate(across(ENTIDAD:MUNICIPIO, ~as.character(.x))) %>% select(ENTIDAD:NOMBRE_MUN) %>% st_make_valid()

  loc <- loc %>% rename(MANZANA = NOMBRE) %>%
    mutate(across(ENTIDAD:LOCALIDAD, ~as.character(.x))) %>%
    select(ENTIDAD:MANZANA) %>% st_make_valid()

  secc <- secc %>% rename(DISTRITO_F = DISTRITO) %>%
    mutate(across(ENTIDAD:SECCION, ~as.character(.x))) %>%
    select(ENTIDAD:SECCION) %>% st_make_valid()

  mza <- mza %>% filter(st_is_valid(.), STATUS == 1) %>% select(ENTIDAD:MANZANA) %>%
    mutate(across(ENTIDAD:MANZANA, ~as.character(.x))) %>% st_make_valid()

  mza <- bind_rows(mza, loc) %>% arrange(as.numeric(SECCION))

  return(list(
    DISTRITO_F = df,
    DISTRITO_L = dl,
    MUNICIPIO = mun,
    SECCION = secc,
    MANZANA = mza
  ))
}
