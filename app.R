# app.R — Geolocalitzador (VERSIÓ SIMPLE: NOMÉS MODE "process")
# arranged by AI

# =====================
# Llibreries
# =====================
library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(DT)
library(osrm)
library(sf)
library(readxl)
library(writexl)
library(tidyr)
library(lubridate)
library(rlang)
library(httr2)
library(tidygeocoder)
library(bslib)
library(waiter)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# =====================
# UI
# =====================
ui <- bslib::page_sidebar(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = bslib::font_google("Inter"),
    "font-size-root" = "0.8125rem"  # ~13px
  ),
  title = "Geolocalitzador d'Itinerari de Viatge",
  
  # -------- SIDEBAR: càrrega + paràmetres + desar --------
  sidebar = bslib::sidebar(
    column(6, actionButton("show_format", "Format input")),
    column(6, actionButton("show_info", "Informació")),
    title = "",
    open = "desktop",
    width = 290,
    
    # Càrrega
    h5("Des de disc local (.xlsx)"),
    fileInput("proc_trip_file", NULL, accept = c(".xlsx", ".xls")),
    
    h5("Des de carpeta 'www/'"),
    fluidRow(
      column(12, actionButton("proc_refresh_www", "Actualitza", class = "btn-secondary w-100 mb-2")),
      column(12, selectInput("proc_trip_www", "", choices = c("(cap)")))
    ),
    actionButton("proc_load_www", "Carrega l'arxiu seleccionat", class = "btn-primary w-100"),
    tags$hr(),
    
    # Paràmetres
    h4("Paràmetres de l'itinerari"),
    checkboxInput("ver_todas", "Mostra totes les etapes", value = FALSE),
    checkboxInput("show_hotels_on_map", "Mostrar hotels al mapa", value = TRUE),
    checkboxInput("allow_edit_process", "Permetre edició", value = FALSE),
    
    h4("Detall de l'etapa"),
    selectInput(
      "filtro_tipo", "Filtra per tipus:",
      choices = c("Itinerari", "Vols", "Cotxe", "Tren", "Vaixell", "Visita"),
      selected = "Itinerari"
    ),
    
    tags$hr(),
    # Desar
    h4("Desar canvis"),
    textInput("www_filename_proc", "Nom del fitxer (sense extensió)", value = "itinerari_visualitzar"),
    checkboxInput("overwrite_www_proc", "Sobreescriure si ja existeix", value = FALSE),
    actionButton("save_places_proc", "Desa en memòria i a www/", class = "btn-warning w-100"),
    
    br(), br(),
    helpText("Astúcia: marca «Permetre edició» per poder canviar taules.")
  ),
  
  waiter::useWaiter(),
  
  # -------- CONTINGUT PRINCIPAL --------
  h3("Detalls de l'etapa"),
  fluidRow(
    column(6,
           uiOutput("detalle_etapa"),
           tags$hr(),
           h4("Hotel"),
           uiOutput("hoteles_panel")
    ),
    column(6,
           tags$h4("Llocs d'interès"),
           uiOutput("interes_panel"),
           uiOutput("ruta_panel")
    )
  ),
  
  bslib::navset_tab(
    bslib::nav_panel(
      "Mapa",
      bslib::layout_columns(
        col_widths = c(4,8),
        bslib::card(
          bslib::card_header(""),
          airDatepickerInput("dia", "Data:", Sys.Date(), inline = TRUE, language = "es", autoClose = TRUE)
        ),
        leafletOutput("mapa", height = "420px")
      )
    ),
    
    # Taules (només si es permet l'edició)
    conditionalPanel(
      "input.allow_edit_process",
      bslib::navset_tab(
        bslib::nav_panel("Llocs",  DTOutput("places_table")),
        bslib::nav_panel("Etapes", DTOutput("stages_table")),
        bslib::nav_panel("Hotels", DTOutput("hotels_table")),
        bslib::nav_panel("Interès", DTOutput("interes_table"))
      )
    )
  ),
  
  # Estils
  tags$head(tags$style(HTML('
    .form-control, .selectize-input, .btn { font-size: 16px; }
    .btn { padding: 10px 14px; }
    @media (max-width: 576px) {
      .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter {
        float: none; text-align: left;
      }
    }
    .stage-card{border:1px solid #e6e6e6;border-radius:12px;padding:10px 12px;margin-bottom:8px;background:#fff;box-shadow:0 1px 2px rgba(0,0,0,.04)}
  ')))
)

# =====================
# SERVER
# =====================
server <- function(input, output, session) {
  
  # ---------- Utilitats simples ----------
  coerce_like <- function(value, template) {
    if (is.numeric(template))      suppressWarnings(as.numeric(value))
    else if (inherits(template, "Date")) as.Date(value)
    else if (is.logical(template)) as.logical(value)
    else                            as.character(value)
  }
  
  list_www_xlsx <- function() {
    if (!dir.exists("www")) return(character(0))
    sort(list.files("www", pattern = "\\.xlsx?$", full.names = FALSE))
  }
  
  col_first_existing <- function(df, candidates, default = NA_character_) {
    for (nm in candidates) if (nm %in% names(df)) return(df[[nm]])
    rep(default, nrow(df))
  }
  
  .pick_col <- function(df, candidates) {
    nms <- tolower(names(df)); cand <- tolower(candidates)
    idx <- match(cand, nms); idx <- idx[!is.na(idx)]
    if (length(idx)) idx[1] else NA_integer_
  }
  
  # ---------- Estat: dataset actiu ----------
  trip_data <- reactiveVal(NULL)
  current_td <- reactive(trip_data())
  
  # ---------- Lectura Excel i normalització ----------
  .sheet_aliases <- list(
    places = c("llocs","lugares","places"),
    stages = c("etapes","etapas","stages"),
    hotels = c("hotels","hoteles"),
    interes = c("interes","interés","interests","interest","interesos")
  )
  
  read_trip_data <- function(path) {
    all <- readxl::excel_sheets(path)
    
    find_sheet <- function(aliases) {
      s_low <- tolower(all)
      for (al in aliases) {
        hit <- which(s_low == al)
        if (length(hit)) return(all[hit[1]])
      }
      NA_character_
    }
    
    sh_places  <- find_sheet(.sheet_aliases$places)
    sh_stages  <- find_sheet(.sheet_aliases$stages)
    sh_hotels  <- find_sheet(.sheet_aliases$hotels)
    sh_interes <- find_sheet(.sheet_aliases$interes)
    stopifnot(!is.na(sh_places), !is.na(sh_stages))
    
    # --- PLACES ---
    places <- readxl::read_excel(path, sheet = sh_places)
    names(places) <- tolower(trimws(names(places)))
    # sinònims essencials
    if (!"place_id" %in% names(places)) names(places)[names(places) %in% c("lloc_id","lugar_id")] <- "place_id"
    if (!"city"     %in% names(places)) names(places)[names(places) %in% c("ciutat","ciudad")] <- "city"
    if (!"country"  %in% names(places)) names(places)[names(places) %in% c("pais","país")] <- "country"
    if (!"wikipedia"%in% names(places)) places$wikipedia <- NA_character_
    places$lat <- suppressWarnings(as.numeric(places$lat))
    places$lon <- suppressWarnings(as.numeric(places$lon))
    # canònic per a la resta del codi
    places <- places |> dplyr::rename(ciudad = city, pais = country)
    
    # --- STAGES ---
    stages <- readxl::read_excel(path, sheet = sh_stages)
    names(stages) <- tolower(trimws(names(stages)))
    rn <- function(alts, to) if (any(alts %in% names(stages))) names(stages)[match(alts[alts %in% names(stages)][1], names(stages))] <<- to
    rn(c("etapa","stage"), "etapa")
    rn(c("de_id","from_id"), "from_id")
    rn(c("a_id","to_id"),   "to_id")
    rn(c("data","fecha","date"), "date")
    rn(c("descripció","descripción","description"), "description")
    rn(c("medi","medio","mode","modo"), "medi")
    rn(c("ruta","route","trayecto","trajeto","route_desc","ruta_text"), "ruta")
    if (!"ruta" %in% names(stages)) stages$ruta <- NA_character_
    
    # --- HOTELS (opcional) ---
    hotels <- if (!is.na(sh_hotels)) {
      h <- readxl::read_excel(path, sheet = sh_hotels)
      names(h) <- tolower(trimws(names(h)))
      if (!"place_id"   %in% names(h)) names(h)[names(h) %in% c("lloc_id","lugar_id")] <- "place_id"
      if (!"hotel_name" %in% names(h)) names(h)[names(h) %in% c("hotel_nom","hotel_nombre","name")] <- "hotel_name"
      if (!"hotel_link" %in% names(h)) names(h)[names(h) %in% c("link","url")] <- "hotel_link"
      if (!"details"    %in% names(h)) names(h)[names(h) %in% c("detalls","detalles","notes","notas")] <- "details"
      if (!"hotel_lat"  %in% names(h)) h$hotel_lat <- NA_real_
      if (!"hotel_lon"  %in% names(h)) h$hotel_lon <- NA_real_
      h$hotel_lat <- suppressWarnings(as.numeric(h$hotel_lat))
      h$hotel_lon <- suppressWarnings(as.numeric(h$hotel_lon))
      # afegeix ciutat/pais des de places
      h <- dplyr::left_join(h, places |> dplyr::select(place_id, ciudad, pais), by = "place_id")
      h
    } else NULL
    
    # --- INTERES (opcional) ---
    interes <- if (!is.na(sh_interes)) {
      inter <- readxl::read_excel(path, sheet = sh_interes)
      names(inter) <- tolower(trimws(names(inter)))
      if (!"descriptor"%in% names(inter)) names(inter)[names(inter) %in% c("name","title")] <- "descriptor"
      if (!"ciudad"    %in% names(inter)) names(inter)[names(inter) %in% c("city","ciutat")] <- "ciudad"
      if (!"pais"      %in% names(inter)) names(inter)[names(inter) %in% c("country","país")] <- "pais"
      if (!"tipus"     %in% names(inter)) names(inter)[names(inter) %in% c("tipo","type")] <- "tipus"
      if (!"observacio"%in% names(inter)) names(inter)[names(inter) %in% c("observació","observacion","observación","details","note","notes")] <- "observacio"
      if (!"link"      %in% names(inter)) names(inter)[names(inter) %in% c("enllaç","enlace","url")] <- "link"
      if (!"lat"       %in% names(inter)) inter$lat <- NA_real_
      if (!"lon"       %in% names(inter)) inter$lon <- NA_real_
      inter$lat <- suppressWarnings(as.numeric(inter$lat))
      inter$lon <- suppressWarnings(as.numeric(inter$lon))
      inter
    } else NULL
    
    list(places = places, stages = stages, hotels = hotels, interes = interes)
  }
  
  # ---------- Càrrega: local i www ----------
  observeEvent(input$proc_trip_file, {
    req(input$proc_trip_file$datapath)
    td <- read_trip_data(input$proc_trip_file$datapath)
    trip_data(td)
    showNotification("✅ Itinerari carregat", type = "message")
  })
  
  observe({
    files <- list_www_xlsx()
    updateSelectInput(session, "proc_trip_www",
                      choices = c("(cap)", files),
                      selected = if (length(files)) files[1] else "(cap)"
    )
  })
  
  observeEvent(TRUE, {
    files <- list_www_xlsx()
    if (length(files)) {
      path <- file.path("www", files[1])
      td <- try(read_trip_data(path), silent = TRUE)
      if (!inherits(td, "try-error")) {
        trip_data(td)
        showNotification(paste0("✅ Carregat per defecte: ", files[1]), type = "message")
      } else {
        showNotification("❌ Error llegint l'arxiu per defecte de www/.", type = "error")
      }
    }
  }, once = TRUE, ignoreInit = FALSE)
  
  observeEvent(input$proc_refresh_www, {
    files <- list_www_xlsx()
    updateSelectInput(session, "proc_trip_www",
                      choices = c("(cap)", files),
                      selected = if (length(files)) files[1] else "(cap)"
    )
  })
  
  observeEvent(input$proc_load_www, {
    if (is.null(input$proc_trip_www) || input$proc_trip_www == "(cap)") {
      showNotification("📂 Selecciona un arxiu de www/.", type = "warning"); return()
    }
    path <- file.path("www", input$proc_trip_www)
    td <- try(read_trip_data(path), silent = TRUE)
    if (inherits(td, "try-error")) {
      showNotification("❌ Error llegint l'arxiu.", type = "error"); return()
    }
    trip_data(td)
    showNotification(paste0("✅ Carregat: ", input$proc_trip_www), type = "message")
  })
  
  # ---------- Complements: Wikipedia + Geocoding (helpers) ----------
  wikipedia_missing <- function(places_df, lang = "es") {
    if (!"wikipedia" %in% names(places_df)) places_df$wikipedia <- NA_character_
    needs_wiki <- is.na(places_df$wikipedia) | places_df$wikipedia == ""
    if (!any(needs_wiki) || !"ciudad" %in% names(places_df)) return(places_df)
    qvec <- places_df$ciudad[needs_wiki]
    fetch_wiki <- function(q) {
      url <- paste0("https://", lang, ".wikipedia.org/w/api.php")
      req <- httr2::request(url) |>
        httr2::req_url_query(action = "opensearch", search = q, limit = 1, namespace = 0, format = "json") |>
        httr2::req_user_agent("itinerario-app/1.0 (contact: you@example.com)")
      resp <- try(httr2::req_perform(req), silent = TRUE)
      if (inherits(resp, "try-error")) return(NA_character_)
      js <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
      if (inherits(js, "try-error")) return(NA_character_)
      if (length(js) >= 4 && length(js[[4]]) >= 1) return(js[[4]][1])
      NA_character_
    }
    places_df$wikipedia[needs_wiki] <- vapply(qvec, fetch_wiki, FUN.VALUE = character(1))
    places_df
  }
  
  geocode_one <- function(query) {
    out <- try(geocode_OSM(query, as.data.frame = TRUE), silent = TRUE)
    if (inherits(out, "try-error") || is.null(out)) return(c(NA_real_, NA_real_))
    if (inherits(out, "sf")) {
      coords <- sf::st_coordinates(out)
      return(c(coords[1, "X"], coords[1, "Y"]))
    }
    if (all(c("lon","lat") %in% names(out))) return(c(out$lon[1], out$lat[1]))
    if (all(c("coords.x1","coords.x2") %in% names(out))) return(c(out$coords.x1[1], out$coords.x2[1]))
    if (all(c("x","y") %in% names(out))) return(c(out$x[1], out$y[1]))
    c(NA_real_, NA_real_)
  }
  
  geocode_missing <- function(df, default_country = NULL) {
    if (!all(c("ciudad","lat","lon") %in% names(df))) return(df)
    if (!"wikipedia" %in% names(df)) df$wikipedia <- NA_character_
    need_idx <- which(is.na(df$lat) | is.na(df$lon))
    if (!length(need_idx)) return(df)
    
    withProgress(message = "Geocodificant llocs…", value = 0, {
      clean_city <- function(x) {
        x <- trimws(as.character(x))
        x <- sub(" -.*$", "", x); x <- sub(":.*$", "", x)
        x <- sub("/.*$", "", x); x <- sub(",.*$", "", x); x
      }
      ciudad_clean <- vapply(df$ciudad, clean_city, FUN.VALUE = "")
      pais_orig <- if ("pais" %in% names(df)) as.character(df$pais) else rep(NA_character_, nrow(df))
      pais_final <- ifelse(is.na(pais_orig) | pais_orig == "", default_country, pais_orig)
      q_full <- ifelse(!is.na(pais_final) & nzchar(pais_final), paste(ciudad_clean, pais_final, sep = ", "), ciudad_clean)
      
      queries <- tibble::tibble(query = q_full[need_idx])
      res_bulk <- try(geo(queries, address = query, method = "osm", lat = latitude, long = longitude, limit = 1, verbose = FALSE), silent = TRUE)
      if (!inherits(res_bulk, "try-error") && is.data.frame(res_bulk) && nrow(res_bulk) == length(need_idx)) {
        df$lat[need_idx] <- res_bulk$latitude
        df$lon[need_idx] <- res_bulk$longitude
      }
      still_idx <- which(is.na(df$lat) | is.na(df$lon))
      if (length(still_idx)) {
        for (i in still_idx) {
          coords <- geocode_one(q_full[i])
          df$lon[i] <- coords[1]; df$lat[i] <- coords[2]
          incProgress(1/length(still_idx))
          Sys.sleep(0.1)
        }
      }
    })
    df$lat <- suppressWarnings(as.numeric(df$lat))
    df$lon <- suppressWarnings(as.numeric(df$lon))
    df
  }
  
  fill_missing_hotel_coords <- function(h, places_df = NULL) {
    if (is.null(h) || !nrow(h)) return(h)
    for (nm in c("hotel_lat","hotel_lon")) if (!nm %in% names(h)) h[[nm]] <- NA_real_
    need_idx <- which(is.na(h$hotel_lat) | is.na(h$hotel_lon))
    if (!length(need_idx)) return(h)
    
    withProgress(message = "Geolocalitzant hotels…", value = 0, {
      city_by_id <- country_by_id <- NULL
      if (!is.null(places_df) && all(c("place_id","ciudad","pais") %in% names(places_df))) {
        city_by_id <- setNames(as.character(places_df$ciudad), places_df$place_id)
        country_by_id <- setNames(as.character(places_df$pais), places_df$place_id)
      }
      for (i in need_idx) {
        name <- if ("hotel_name" %in% names(h)) as.character(h$hotel_name[i]) else NA_character_
        pid  <- if ("place_id"   %in% names(h)) as.character(h$place_id[i])   else NA_character_
        parts <- list(name)
        if (!is.null(city_by_id) && !is.na(pid) && nzchar(pid)) parts <- c(parts, city_by_id[[pid]], country_by_id[[pid]])
        q <- paste(na.omit(parts), collapse = ", ")
        coords <- geocode_one(q)
        h$hotel_lon[i] <- coords[1]; h$hotel_lat[i] <- coords[2]
        incProgress(1/length(need_idx))
        Sys.sleep(0.1)
      }
    })
    h$hotel_lat <- as.numeric(h$hotel_lat)
    h$hotel_lon <- as.numeric(h$hotel_lon)
    h
  }
  
  geocode_interes_missing <- function(h) {
    if (is.null(h) || !nrow(h)) return(h)
    for (nm in c("descriptor","ciutat","pais","lat","lon")) if (!nm %in% names(h)) h[[nm]] <- NA
    need_idx <- which(is.na(h$lat) | is.na(h$lon))
    if (!length(need_idx)) return(h)
    
    withProgress(message = "Geolocalitzant punts d'interès…", value = 0, {
      for (i in need_idx) {
        q <- paste(na.omit(c(h$descriptor[i], h$ciutat[i], h$pais[i])), collapse = ", ")
        coords <- geocode_one(q)
        h$lon[i] <- coords[1]; h$lat[i] <- coords[2]
        incProgress(1/length(need_idx))
        Sys.sleep(0.1)
      }
    })
    h$lat <- as.numeric(h$lat); h$lon <- as.numeric(h$lon)
    h
  }
  
  interes_link_missing <- function(inter_df, lang = "es") {
    h <- inter_df
    if (is.null(h) || !nrow(h)) return(h)
    if (!"link" %in% names(h)) h$link <- NA_character_
    need <- is.na(h$link) | h$link == ""
    if (!any(need)) return(h)
    fetch_wiki <- function(q) {
      url <- paste0("https://", lang, ".wikipedia.org/w/api.php")
      req <- httr2::request(url) |>
        httr2::req_url_query(action = "opensearch", search = q, limit = 1, namespace = 0, format = "json") |>
        httr2::req_user_agent("itinerario-app/1.0 (contact: you@example.com)")
      resp <- try(httr2::req_perform(req), silent = TRUE)
      if (inherits(resp, "try-error")) return(NA_character_)
      js <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
      if (inherits(js, "try-error")) return(NA_character_)
      if (length(js) >= 4 && length(js[[4]]) >= 1) return(js[[4]][1])
      NA_character_
    }
    qvec <- paste(na.omit(h$descriptor[need]), ifelse(is.na(h$ciudad[need]), "", h$ciudad[need]), ifelse(is.na(h$pais[need]), "", h$pais[need]))
    h$link[need] <- vapply(qvec, fetch_wiki, FUN.VALUE = character(1))
    h
  }
  
  # ---------- Parsing de dates i expansió ----------
  .norm_months <- function(s) {
    if (is.na(s)) return(NA_character_)
    x <- tolower(trimws(gsub("\\s+", " ", as.character(s))))
    map <- c(
      "gener"="jan","gen"="jan","febrer"="feb","feb"="feb","març"="mar","mar"="mar",
      "abril"="apr","abr"="apr","maig"="may","mai"="may","juny"="jun","jun"="jun",
      "juliol"="jul","jul"="jul","agost"="aug","ago"="aug","setembre"="sep","set"="sep",
      "octubre"="oct","oct"="oct","novembre"="nov","nov"="nov","desembre"="dec","des"="dec",
      "enero"="jan","ene"="jan","febrero"="feb","marzo"="mar","abril"="apr","abr"="apr",
      "mayo"="may","junio"="jun","julio"="jul","agosto"="aug","septiembre"="sep","setiembre"="sep",
      "octubre"="oct","noviembre"="nov","diciembre"="dec","dic"="dec"
    )
    parts <- strsplit(x, " ")[[1]]
    parts <- vapply(parts, function(p) if (!is.na(map[p])) map[p] else p, character(1))
    paste(parts, collapse = " ")
  }
  
  .parse_single_date <- function(x, year = 2025) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))
    x0 <- as.character(x); x0 <- trimws(gsub("\\s+", " ", x0))
    num <- suppressWarnings(as.numeric(x0))
    if (!is.na(num) && num > 30000) return(as.Date("1899-12-30") + num) # Excel
    if (grepl("^\\d{4}$", x0)) return(NA)
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x0)) return(suppressWarnings(as.Date(x0)))
    x1 <- .norm_months(x0)
    d <- suppressWarnings(lubridate::parse_date_time(x1, orders = c("d b Y","d B Y","d b","d B","dmy","mdy","ymd","Ymd","dmY","mdY")))
    if (!is.na(d)) {
      if (is.na(lubridate::year(d)) || lubridate::year(d) < 1900) d <- lubridate::update(d, year = year)
      return(as.Date(d))
    }
    x2 <- gsub("/","-", x1, fixed = TRUE)
    d2 <- suppressWarnings(lubridate::parse_date_time(x2, orders = c("d-b-Y","d-b","dmy","mdy","ymd","Ymd","dmY","mdY")))
    if (!is.na(d2)) {
      if (is.na(lubridate::year(d2)) || lubridate::year(d2) < 1900) d2 <- lubridate::update(d2, year = year)
      return(as.Date(d2))
    }
    NA
  }
  
  .expand_fecha <- function(x, year = 2025) {
    if (is.null(x) || is.na(x) || !nzchar(as.character(x))) return(as.Date(NA))
    s <- as.character(x)
    s <- trimws(gsub("\\s+", " ", s))
    s <- gsub("\u2013","-", s); s <- gsub("\u2014","-", s)
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", s)) return(.parse_single_date(s, year))
    if (grepl("\\s-\\s", s)) {
      parts <- strsplit(s, "\\s-\\s")[[1]]
      if (length(parts) == 2) {
        d1 <- .parse_single_date(parts[1], year); d2 <- .parse_single_date(parts[2], year)
        if (!is.na(d1) && !is.na(d2)) return(if (d1 <= d2) seq(d1, d2, by = "day") else d1)
        return(as.Date(NA))
      }
    }
    .parse_single_date(s, year)
  }
  
  # ---------- Etapes expandides per dia ----------
  etapas_exp <- reactive({
    td <- current_td(); req(td)
    places <- td$places; stages <- td$stages
    
    # normalitza modo
    if ("medi" %in% names(stages)) names(stages)[names(stages) == "medi"] <- "modo"
    stages$modo <- tolower(trimws(as.character(stages$modo)))
    stages$modo <- dplyr::case_when(
      stages$modo %in% c("vol","flight","avion","avió","plane","vuelo") ~ "vol",
      stages$modo %in% c("cotxe","car","coche","auto","automovil","automóvil","drive","road") ~ "cotxe",
      stages$modo %in% c("tren","train","ferrocarril","rail","railway") ~ "tren",
      stages$modo %in% c("vaixell","barco","ferry","ship","boat") ~ "vaixell",
      TRUE ~ "cotxe"
    )
    same_place <- stages$from_id == stages$to_id
    stages$modo[same_place] <- "visita"
    
    stages %>%
      mutate(dia_list = lapply(date, .expand_fecha, year = 2025)) %>%
      tidyr::unnest(dplyr::all_of("dia_list"), names_repair = "minimal") %>%
      rename(dia = dia_list) %>%
      left_join(places %>% select(place_id, ciudad_from = ciudad, lat_from = lat, lon_from = lon), by = c("from_id" = "place_id")) %>%
      left_join(places %>% select(place_id, ciudad_to   = ciudad, lat_to   = lat, lon_to   = lon, wikipedia), by = c("to_id"   = "place_id")) %>%
      arrange(etapa, dia)
  })
  
  # Calendari segons rang
  set_calendar_to_min <- function() {
    ee <- etapas_exp()
    if (is.null(ee) || !nrow(ee) || !"dia" %in% names(ee) || all(is.na(ee$dia))) return()
    min_day <- as.Date(min(ee$dia, na.rm = TRUE))
    max_day <- as.Date(max(ee$dia, na.rm = TRUE))
    shinyWidgets::updateAirDateInput(
      session, inputId = "dia", value = min_day,
      options = list(minDate = min_day, maxDate = max_day)
    )
  }
  observeEvent(etapas_exp(), { set_calendar_to_min() }, ignoreInit = FALSE)
  
  etapas_by_day <- reactive({ req(etapas_exp()) ; if (is.null(input$dia) || is.na(input$dia)) return(etapas_exp()[0, ]) ; dplyr::filter(etapas_exp(), dia == as.Date(input$dia)) })
  ver_todas_eff   <- reactive(isTRUE(input$ver_todas))
  show_hotels_eff <- reactive(isTRUE(input$show_hotels_on_map))
  etapas_chosen   <- reactive({ if (isTRUE(ver_todas_eff())) etapas_exp() else etapas_by_day() })
  
  iconos_etapas <- reactive({
    e <- etapas_chosen(); if (!nrow(e)) return(e[0, ])
    dplyr::transmute(
      e,
      ciudad = ciudad_to,
      tipo = dplyr::case_when(
        modo == "vol"    ~ "Vols",
        modo == "cotxe"  ~ "Cotxe",
        modo == "tren"   ~ "Tren",
        modo == "vaixell"~ "Vaixell",
        modo == "visita" ~ "Visita",
        TRUE              ~ "Itinerari"
      ),
      lat = lat_to, lon = lon_to
    ) %>% dplyr::distinct()
  })
  
  etapas_filtradas <- reactive({
    df <- iconos_etapas(); if (!nrow(df)) return(df)
    if (is.null(input$filtro_tipo) || input$filtro_tipo == "Itinerari") {
      dplyr::filter(df, tipo %in% c("Vols","Cotxe","Tren","Vaixell","Visita"))
    } else {
      dplyr::filter(df, tipo == input$filtro_tipo)
    }
  })
  
  iconos <- reactive({
    df <- etapas_filtradas()
    if (!nrow(df)) return(awesomeIcons(icon = "map-marker", iconColor = "white", library = "fa", markerColor = "cadetblue"))
    the_icon <- ifelse(df$tipo == "Vols", "plane",
                       ifelse(df$tipo == "Cotxe", "truck",
                              ifelse(df$tipo == "Tren", "train",
                                     ifelse(df$tipo == "Vaixell", "ship",
                                            ifelse(df$tipo == "Visita","flag","map-marker")))))
    the_color <- ifelse(df$tipo == "Vols", "blue",
                        ifelse(df$tipo == "Cotxe", "red",
                               ifelse(df$tipo == "Tren", "green",
                                      ifelse(df$tipo == "Vaixell", "purple",
                                             ifelse(df$tipo == "Visita", "darkgreen", "cadetblue")))))
    awesomeIcons(icon = the_icon, iconColor = "white", library = "fa", markerColor = the_color)
  })
  
  interes_icons <- function(tipus_vec) {
    icon_name <- ifelse(tipus_vec %in% c("paisatge","natura","parc","platja"), "tree",
                        ifelse(tipus_vec %in% c("monument","historia","història","centre"), "university",
                               ifelse(tipus_vec %in% c("museu","cultura","art"), "university",
                                      ifelse(tipus_vec %in% c("mirador","panoràmica"), "binoculars",
                                             ifelse(tipus_vec %in% c("gastronomia","mercat"), "cutlery",
                                                    ifelse(tipus_vec %in% c("foto","fotografia","viewpoint"), "camera", "star"))))))
    color <- ifelse(tipus_vec %in% c("paisatge","natura","parc","platja"), "green",
                    ifelse(tipus_vec %in% c("monument","historia","història","centre"), "blue",
                           ifelse(tipus_vec %in% c("museu","cultura","art"), "purple",
                                  ifelse(tipus_vec %in% c("mirador","panoràmica"), "darkgreen",
                                         ifelse(tipus_vec %in% c("gastronomia","mercat"), "red", "orange")))))
    leaflet::awesomeIcons(icon = icon_name, iconColor = "white", library = "fa", markerColor = color)
  }
  
  interes_markers <- reactive({
    td <- current_td(); req(td)
    inter <- td$interes
    if (is.null(inter) || !nrow(inter)) return((data.frame())[0,])
    for (nm in c("descriptor","lat","lon","ciudad","pais","observacio","tipus","link")) if (!nm %in% names(inter)) inter[[nm]] <- NA
    inter %>%
      dplyr::filter(!is.na(lat), !is.na(lon)) %>%
      dplyr::mutate(
        tipus = ifelse(is.na(tipus) | !nzchar(tipus), "altres", tolower(trimws(tipus))),
        popup_html = sprintf(
          "<b>%s</b><br/><i>%s, %s</i>%s%s",
          htmltools::htmlEscape(descriptor %||% ""),
          htmltools::htmlEscape(ciudad %||% ""),
          htmltools::htmlEscape(pais %||% ""),
          ifelse(is.na(observacio) | !nzchar(observacio), "", paste0("<br/>", htmltools::htmlEscape(observacio))),
          ifelse(is.na(link) | !nzchar(link), "", paste0("<br/><a href='", htmltools::htmlEscape(link), "' target='_blank'>Enllaç</a>"))
        )
      )
  })
  
  # ---------- Rutes (OSRM) ----------
  options(osrm.server = "https://router.project-osrm.org/", osrm.profile = "car")
  
  to_sf_point <- function(df) {
    if (!nrow(df)) return(NULL)
    if (any(!stats::complete.cases(df$lon, df$lat))) return(NULL)
    sf::st_as_sf(df, coords = c("lon","lat"), crs = 4326)
  }
  
  route_sf <- function(src_df, dst_df) {
    if (!requireNamespace("osrm", quietly = TRUE)) return(NULL)
    if (any(!stats::complete.cases(src_df$lon, src_df$lat))) return(NULL)
    if (any(!stats::complete.cases(dst_df$lon, dst_df$lat))) return(NULL)
    src_sf <- to_sf_point(src_df); if (is.null(src_sf)) return(NULL)
    dst_sf <- to_sf_point(dst_df); if (is.null(dst_sf)) return(NULL)
    r <- tryCatch(osrm::osrmRoute(src = src_sf, dst = dst_sf), error = function(e) NULL)
    if (is.null(r)) return(NULL)
    if (inherits(r, "sf")) return(r)
    if (is.data.frame(r) && "geometry" %in% names(r)) return(sf::st_as_sf(r, wkt = "geometry", crs = 4326))
    NULL
  }
  
  rutas_carretera <- reactive({
    e <- etapas_chosen(); if (is.null(e) || !nrow(e)) return(list())
    cotxe <- dplyr::filter(e, modo == "cotxe", from_id != to_id)
    if (!nrow(cotxe)) return(list())
    cotxe <- dplyr::filter(cotxe, stats::complete.cases(lon_from, lat_from, lon_to, lat_to))
    if (!nrow(cotxe)) return(list())
    cotxe <- dplyr::distinct(cotxe, lon_from, lat_from, lon_to, lat_to, .keep_all = TRUE)
    cotxe <- head(cotxe, 20)
    lapply(seq_len(nrow(cotxe)), function(i){
      src <- data.frame(id = paste0("src", i), lon = cotxe$lon_from[i], lat = cotxe$lat_from[i])
      dst <- data.frame(id = paste0("dst", i), lon = cotxe$lon_to[i],   lat = cotxe$lat_to[i])
      route_sf(src, dst)
    })
  })
  
  # ---------- Hotels: join + assignació a etapes ----------
  hoteles_por_ciudad <- reactive({ td <- current_td(); req(td); if (is.null(td$hotels)) return(NULL); dplyr::left_join(td$hotels, td$places, by = "place_id") })
  
  hotels_join <- reactive({
    td <- current_td(); req(td)
    if (is.null(td$hotels) || !nrow(td$hotels)) return(td$hotels)
    h <- td$hotels; p <- td$places
    if (!"place_id" %in% names(h)) names(h)[names(h) %in% c("lloc_id","lugar_id")] <- "place_id"
    h$place_id <- as.character(h$place_id)
    if (!"hotel_lat" %in% names(h)) h$hotel_lat <- NA_real_
    if (!"hotel_lon" %in% names(h)) h$hotel_lon <- NA_real_
    h$hotel_lat <- suppressWarnings(as.numeric(h$hotel_lat))
    h$hotel_lon <- suppressWarnings(as.numeric(h$hotel_lon))
    if (!"ciudad" %in% names(p) && "city" %in% names(p)) p$ciudad <- p$city
    if (!"pais"   %in% names(p) && "country" %in% names(p)) p$pais   <- p$country
    p$place_id <- as.character(p$place_id)
    hj <- dplyr::left_join(h, p[, c("place_id","ciudad","pais")], by = "place_id")
    for (nm in c("ciudad","pais")) if (!nm %in% names(hj)) hj[[nm]] <- NA_character_
    hj
  })
  
  assign_hotels_to_stages <- function(e, hj) {
    if (is.null(e) || !nrow(e)) return(e[0, ])
    e2 <- e; e2$row_id <- seq_len(nrow(e2))
    e2$from_id <- as.character(e2$from_id); e2$to_id <- as.character(e2$to_id)
    if (is.null(hj) || !nrow(hj)) { e2$assigned_place_id <- NA_character_; e2$assign_source <- "none"; return(e2) }
    hj2 <- hj; hj2$place_id <- as.character(hj2$place_id)
    has_to   <- e2$to_id   %in% hj2$place_id
    has_from <- e2$from_id %in% hj2$place_id
    e2$assigned_place_id <- ifelse(has_to, e2$to_id, ifelse(has_from, e2$from_id, NA_character_))
    e2$assign_source     <- ifelse(has_to, "to_id", ifelse(has_from, "from_id", "none"))
    dplyr::left_join(e2, hj2, by = c("assigned_place_id" = "place_id"))
  }
  
  assigned_hotels <- reactive({
    e <- if (isTRUE(ver_todas_eff())) etapas_exp() else etapas_by_day()
    hj <- hotels_join()
    assign_hotels_to_stages(e, hj)
  })
  
  hotel_icons <- reactive({ leaflet::awesomeIcons(icon = "bed", iconColor = "white", library = "fa", markerColor = "orange") })
  
  hotels_markers <- reactive({
    ah <- tryCatch(assigned_hotels(), error = function(e) NULL)
    if (is.null(ah) || !nrow(ah)) return((data.frame())[0, ])
    if ("assign_source" %in% names(ah)) ah <- dplyr::filter(ah, assign_source != "none")
    ah <- dplyr::filter(ah, !is.na(hotel_lat), !is.na(hotel_lon))
    if (!nrow(ah)) return(ah[0, ])
    city_col <- if ("ciudad" %in% names(ah)) "ciudad" else if ("city" %in% names(ah)) "city" else if ("ciudad_to" %in% names(ah)) "ciudad_to" else if ("ciudad_from"%in% names(ah)) "ciudad_from" else NULL
    country_col <- if ("pais" %in% names(ah)) "pais" else if ("country" %in% names(ah)) "country" else NULL
    city_val <- if (!is.null(city_col)) ah[[city_col]] else rep(NA_character_, nrow(ah))
    country_val <- if (!is.null(country_col)) ah[[country_col]] else rep(NA_character_, nrow(ah))
    mk_popup <- function(i, with_link = TRUE) {
      nom <- htmltools::htmlEscape(ah$hotel_name[i] %||% "(Hotel)")
      ciu <- htmltools::htmlEscape(city_val[i] %||% "")
      pai <- htmltools::htmlEscape(country_val[i] %||% "")
      sep <- if (nzchar(ciu) && nzchar(pai)) ", " else ""
      lloc <- paste0("<i>", ciu, sep, pai, "</i>")
      lk <- ah$hotel_link[i]
      if (with_link && !is.na(lk) && nzchar(lk)) paste0("<b>", nom, "</b><br>", "<a href='", htmltools::htmlEscape(lk), "' target='_blank'>Web de l'hotel</a><br>", lloc)
      else paste0("<b>", nom, "</b><br>", lloc)
    }
    ah$popup_html <- vapply(seq_len(nrow(ah)), function(i) mk_popup(i, with_link = TRUE), character(1))
    ah
  })
  
  # ---------- Renders ----------
  output$ruta_panel <- renderUI({
    e <- etapas_chosen(); if (!nrow(e)) return(NULL)
    ruta_vec <- col_first_existing(e, c("ruta","route","trayecto","trajeto"))
    idx <- which(!is.na(ruta_vec) & nzchar(trimws(ruta_vec)))
    if (!length(idx)) return(NULL)
    e$ruta_ <- ruta_vec
    df <- e[idx, , drop = FALSE] %>% dplyr::arrange(dia, etapa)
    blocs <- lapply(split(df, df$etapa), function(dfe){
      cap <- sprintf("Etapa %s · %s → %s", as.character(dfe$etapa[1]), dfe$ciudad_from[1], dfe$ciudad_to[1])
      dia_txt <- if (!is.null(dfe$dia[1]) && !is.na(dfe$dia[1])) format(dfe$dia[1], "%d %b %Y") else as.character(dfe$date[1])
      textos <- unique(na.omit(trimws(dfe$ruta_)))
      tags$div(class = "stage-card",
               tags$div(tags$b(cap), tags$span(style="float:right;", dia_txt)),
               lapply(textos, function(tx) tags$p(htmltools::htmlEscape(tx)))
      )
    })
    do.call(tagList, blocs)
  })
  
  output$mapa <- renderLeaflet({
    req(etapas_chosen())
    e_all <- etapas_chosen()
    
    df_markers <- e_all %>%
      dplyr::transmute(
        description = description,
        wikipedia = wikipedia,
        ciudad = ciudad_to,
        tipo = dplyr::case_when(
          modo == "vol"    ~ "Vols",
          modo == "cotxe"  ~ "Cotxe",
          modo == "tren"   ~ "Tren",
          modo == "vaixell"~ "Vaixell",
          modo == "visita" ~ "Visita",
          TRUE              ~ "Itinerari"
        ),
        lat = lat_to, lon = lon_to
      ) %>% dplyr::distinct() %>% dplyr::filter(stats::complete.cases(lon, lat))
    
    m <- leaflet() %>% addProviderTiles(providers$OpenStreetMap)
    
    # Línies rectes (vol/tren/vaixell) evitant from==to
    add_mode_lines <- function(m, df, color) {
      if (!nrow(df)) return(m)
      m %>% addPolylines(
        lng = unlist(mapply(c, df$lon_from, df$lon_to, SIMPLIFY = FALSE)),
        lat = unlist(mapply(c, df$lat_from, df$lat_to, SIMPLIFY = FALSE)),
        color = color, weight = 3, opacity = 0.7
      )
    }
    v <- dplyr::filter(e_all, modo == "vol",    from_id != to_id) %>% dplyr::filter(stats::complete.cases(lon_from, lat_from, lon_to, lat_to))
    t <- dplyr::filter(e_all, modo == "tren",   from_id != to_id) %>% dplyr::filter(stats::complete.cases(lon_from, lat_from, lon_to, lat_to))
    s <- dplyr::filter(e_all, modo == "vaixell",from_id != to_id) %>% dplyr::filter(stats::complete.cases(lon_from, lat_from, lon_to, lat_to))
    m <- add_mode_lines(m, v, "blue")
    m <- add_mode_lines(m, t, "green")
    m <- add_mode_lines(m, s, "purple")
    
    # Rutes OSRM (cotxe)
    rlist <- rutas_carretera()
    if (length(rlist)) for (tramo in rlist) if (!is.null(tramo) && inherits(tramo, "sf")) {
      m <- m %>% addPolylines(data = tramo, color = "black", weight = 4, opacity = 0.9, dashArray = "5,10")
    }
    
    # Marcadors de destinacions
    if (nrow(df_markers)) {
      m <- m %>% addAwesomeMarkers(
        data = df_markers, lng = ~lon, lat = ~lat, icon = iconos(), label = ~ciudad,
        popup = ~paste0(
          "<b>", htmltools::htmlEscape(description), "</b><br/>",
          ifelse(!is.na(wikipedia) & nzchar(wikipedia),
                 paste0("<a href='", htmltools::htmlEscape(wikipedia), "' target='_blank' rel='noopener noreferrer'>Veure a Wikipedia</a>"),
                 "")
        )
      )
    }
    
    # Visites (from==to)
    vis <- dplyr::filter(e_all, modo == "visita") %>%
      dplyr::transmute(lon = lon_to, lat = lat_to) %>%
      dplyr::filter(stats::complete.cases(lon, lat)) %>% dplyr::distinct()
    if (nrow(vis)) {
      m <- m %>% addCircleMarkers(data = vis, lng = ~lon, lat = ~lat, radius = 10, stroke = TRUE, weight = 2, fillOpacity = 0.25, color = "darkgreen")
    }
    
    # Overlays: Hotels + Interès
    overlay_groups <- character(0)
    if (isTRUE(show_hotels_eff())) {
      hm <- hotels_markers()
      if (!is.null(hm) && nrow(hm)) {
        m <- m %>% addAwesomeMarkers(
          data = hm, lng = ~hotel_lon, lat = ~hotel_lat, icon = hotel_icons(), label = ~hotel_name,
          popup = ~popup_html, group = "Hotels", clusterOptions = markerClusterOptions()
        )
        overlay_groups <- c(overlay_groups, "Hotels")
      }
    }
    im <- interes_markers()
    if (!is.null(im) && nrow(im)) {
      m <- m %>% addAwesomeMarkers(
        data = im, lng = ~lon, lat = ~lat, icon = interes_icons(im$tipus), label = ~descriptor,
        popup = ~paste0(
          "<b>", descriptor, "</b><br/>", "<i>", ciudad, ", ", pais, "</i><br/>",
          ifelse(!is.na(observacio) & nzchar(observacio), paste0("<p>", htmltools::htmlEscape(observacio), "</p>"), ""),
          ifelse(!is.na(link) & nzchar(link), paste0("<a href='", htmltools::htmlEscape(link), "' target='_blank'>Enllaç</a>"), "")
        ),
        group = "Interès", clusterOptions = markerClusterOptions()
      )
      overlay_groups <- c(overlay_groups, "Interès")
    }
    if (length(overlay_groups)) {
      m <- m %>% addLayersControl(overlayGroups = overlay_groups, options = layersControlOptions(collapsed = TRUE))
    }
    
    # Auto-zoom
    coords <- rbind(data.frame(lon = e_all$lon_from, lat = e_all$lat_from), data.frame(lon = e_all$lon_to, lat = e_all$lat_to))
    coords <- coords[stats::complete.cases(coords), , drop = FALSE]
    if (!nrow(coords)) {
      m <- m %>% setView(lng = 0, lat = 20, zoom = 2)
    } else {
      coords <- unique(coords)
      if (nrow(coords) == 1) {
        m <- m %>% setView(lng = coords$lon[1], lat = coords$lat[1], zoom = 12)
      } else {
        lon_min <- min(coords$lon); lon_max <- max(coords$lon)
        lat_min <- min(coords$lat); lat_max <- max(coords$lat)
        eps <- 1e-4
        if ((abs(lon_max - lon_min) < eps) && (abs(lat_max - lat_min) < eps)) {
          m <- m %>% setView(lng = mean(c(lon_min, lon_max)), lat = mean(c(lat_min, lat_max)), zoom = 12)
        } else {
          m <- m %>% fitBounds(lng1 = lon_min, lat1 = lat_min, lng2 = lon_max, lat2 = lat_max)
        }
      }
    }
    m %>% leaflet.extras::addSearchOSM()
  })
  
  # Avisos coord
  observeEvent(etapas_exp(), {
    td <- current_td(); req(td)
    missing <- td$places |> dplyr::filter(is.na(lat) | is.na(lon)) |> dplyr::pull(ciudad)
    if (length(missing)) {
      showNotification(paste0("⚠️ Hi ha llocs sense coordenades: ", paste(missing, collapse = ", "), ". Edita-les a l'Excel d'origen si cal."), type = "warning", duration = 6)
    }
  })
  
  # ---------- Panells informatius ----------
  output$detalle_etapa <- renderUI({
    e <- etapas_chosen()
    if (!nrow(e)) return(HTML("<i>No hi ha cap etapa per al filtre seleccionat.</i>"))
    if (isTRUE(ver_todas_eff())) {
      ciutats <- paste(unique(e$ciudad_to), collapse = " → ")
      HTML(paste0("<h4>Itinerari complet</h4>", "<b>Etapes:</b> ", length(unique(e$etapa)), "<br>", "<b>Ciutats destí:</b> ", ciutats))
    } else {
      desc_vec <- col_first_existing(e, c("description","descripció","descripción"))
      mode_label <- dplyr::case_when(
        e$modo == "vol"    ~ "Vol",
        e$modo == "cotxe"  ~ "Cotxe",
        e$modo == "tren"   ~ "Tren",
        e$modo == "vaixell"~ "Vaixell",
        e$modo == "visita" ~ "Visita",
        TRUE                ~ "Itinerari"
      )
      items <- lapply(seq_len(nrow(e)), function(i){
        tags$div(class = "stage-card",
                 tags$div(class = "stage-head",
                          tags$b(sprintf("Etapa %s · %s", as.character(e$etapa[i]), mode_label[i])),
                          tags$span(style = "float:right;", format(e$dia[i], "%d %b %Y"))
                 ),
                 tags$div(tags$b("Origen: "), e$ciudad_from[i], " ", HTML("&rarr; "), tags$b("Destí: "), e$ciudad_to[i]),
                 { d <- desc_vec[i]; if (!is.na(d) && nzchar(d)) tags$div(tags$b("Descripció: "), htmltools::htmlEscape(d)) }
        )
      })
      tags$div(tags$h4(sprintf("Etapes del %s", format(e$dia[1], "%d %b %Y"))), do.call(tagList, items))
    }
  })
  
  output$hoteles_panel <- renderUI({
    ah <- assigned_hotels()
    if (is.null(ah) || !nrow(ah)) {
      miss <- if (isTRUE(ver_todas_eff())) "Sense hotels per a l'itinerari." else "Sense hotels assignats per a aquest dia."
      return(HTML(paste0("<i>", miss, "</i>")))
    }
    ah <- ah %>% dplyr::arrange(dia, etapa, row_id, ciudad_to, ciudad_from)
    cards <- lapply(split(ah, ah$row_id), function(df){
      src <- unique(df$assign_source)[1]
      cap <- sprintf("Etapa %s · %s → %s", as.character(df$etapa[1]), df$ciudad_from[1], df$ciudad_to[1])
      dia_txt <- if (!is.null(df$dia[1]) && !is.na(df$dia[1])) format(df$dia[1], "%d %b %Y") else as.character(df$date[1])
      if (src == "none") {
        tags$div(class="stage-card",
                 tags$div(tags$b(cap), tags$span(style="float:right;", dia_txt)),
                 tags$div(HTML("<i>Etapa de trànsit: cap hotel assignat (ni a origen ni a destí).</i>"))
        )
      } else {
        ciutat_label <- if (src == "to_id") df$ciudad_to[1] else df$ciudad_from[1]
        items <- lapply(which(!is.na(df$hotel_name)), function(i){
          nm <- df$hotel_name[i] %||% "(Hotel sense nom)"
          lk <- df$hotel_link[i]
          if (!is.na(lk) && nzchar(lk)) tags$li(tags$a(href = lk, target = "_blank", nm)) else tags$li(nm)
        })
        if (!length(items)) items <- list(tags$li(HTML("<i>Hotel sense enllaç o coordenades.</i>")))
        tags$div(class="stage-card",
                 tags$div(tags$b(cap), tags$span(style="float:right;", dia_txt)),
                 tags$div(tags$small(if (src=="to_id") "Hotels a destí" else "Hotels a origen"), " — ", ciutat_label),
                 tags$ul(items)
        )
      }
    })
    do.call(tagList, cards)
  })
  
  observeEvent(assigned_hotels(), {
    ah <- assigned_hotels(); if (is.null(ah) || !nrow(ah)) return()
    tr <- ah %>% dplyr::group_by(row_id) %>% dplyr::summarise(
      transit = all(assign_source == "none"), etapa = dplyr::first(etapa), dia = dplyr::first(dia),
      from = dplyr::first(ciudad_from), to = dplyr::first(ciudad_to), .groups = "drop"
    ) %>% dplyr::filter(transit)
    if (nrow(tr)) {
      msg <- paste0("⚠️ Etapes de trànsit sense hotel: ", paste(sprintf("#%s (%s: %s → %s)", tr$etapa, ifelse(is.na(tr$dia), "—", format(tr$dia, "%d %b")), tr$from, tr$to), collapse = "; "))
      showNotification(msg, type = "warning", duration = 8)
    }
  }, ignoreInit = FALSE)
  
  output$interes_panel <- renderUI({
    td <- current_td(); req(td)
    df <- td$interes
    if (is.null(df) || !nrow(df)) return(tags$div(class = "stage-card", HTML("<i>Sense llocs d'interès.</i>")))
    e <- etapas_chosen(); if (!nrow(e)) return(NULL)
    city <- e$ciudad_to[1]
    df_city <- df %>% dplyr::filter(ciudad == city)
    if (!nrow(df_city)) return(tags$div(class = "stage-card", HTML("<i>Sense llocs d'interès per a aquesta ciutat.</i>")))
    tags$div(class = "stage-card",
             tags$h5(paste0("Llocs per visitar a · ", city)),
             tags$ul(lapply(seq_len(nrow(df_city)), function(i){
               label <- df_city$descriptor[i] %||% "(sense descriptor)"
               link  <- df_city$link[i]
               if (!is.na(link) && nzchar(link)) tags$li(tags$a(href = link, target = "_blank", label)) else tags$li(label)
             }))
    )
  })
  
  # ---------- Taules editables ----------
  # Helper per registrar observadors d'edició (evita repetició)
  register_edit <- function(input_id, slot_name, table_proxy_id) {
    observeEvent(input[[paste0(input_id, "_cell_edit")]], {
      info <- input[[paste0(input_id, "_cell_edit")]]
      td <- trip_data(); req(td)
      colname <- names(td[[slot_name]])[info$col]
      td[[slot_name]][info$row, colname] <- coerce_like(info$value, td[[slot_name]][[colname]])
      trip_data(td)
      DT::replaceData(DT::dataTableProxy(table_proxy_id), td[[slot_name]], resetPaging = FALSE, rownames = FALSE)
    })
  }
  register_edit("places_table",  "places",  "places_table")
  register_edit("stages_table",  "stages",  "stages_table")
  register_edit("interes_table", "interes", "interes_table")
  register_edit("hotels_table",  "hotels",  "hotels_table")
  
  output$places_table  <- DT::renderDT({ td <- current_td(); req(td); df <- td$places;  if (is.null(df)) df <- data.frame(); DT::datatable(df, editable = isTRUE(input$allow_edit_process), options = list(pageLength = 8)) })
  output$stages_table  <- DT::renderDT({ td <- current_td(); req(td); df <- td$stages;  if (is.null(df)) df <- data.frame(); DT::datatable(df, editable = isTRUE(input$allow_edit_process), options = list(pageLength = 8)) })
  output$hotels_table  <- DT::renderDT({ td <- current_td(); req(td); df <- td$hotels;  if (is.null(df)) df <- data.frame(); DT::datatable(df, editable = isTRUE(input$allow_edit_process), options = list(pageLength = 8)) })
  output$interes_table <- DT::renderDT({ td <- current_td(); req(td); df <- td$interes; if (is.null(df)) df <- data.frame(); DT::datatable(df, editable = isTRUE(input$allow_edit_process), options = list(pageLength = 8)) })
  
  # ---------- Escriure a www/ ----------
  write_trip_to_www <- function(td, base_name, overwrite = FALSE, select_after = TRUE) {
    # Normalitza tipus
    if ("lat" %in% names(td$places)) td$places$lat <- suppressWarnings(as.numeric(td$places$lat))
    if ("lon" %in% names(td$places)) td$places$lon <- suppressWarnings(as.numeric(td$places$lon))
    if (!is.null(td$hotels) && nrow(td$hotels)) {
      if ("hotel_lat" %in% names(td$hotels)) td$hotels$hotel_lat <- suppressWarnings(as.numeric(td$hotels$hotel_lat))
      if ("hotel_lon" %in% names(td$hotels)) td$hotels$hotel_lon <- suppressWarnings(as.numeric(td$hotels$hotel_lon))
    }
    
    # places (canònic)
    places <- td$places
    if (!"wikipedia" %in% names(places)) places$wikipedia <- NA_character_
    if (!"ciudad" %in% names(places) && "city" %in% names(places)) places$ciudad <- places$city
    if (!"pais"   %in% names(places) && "country" %in% names(places)) places$pais   <- places$country
    places_out <- places %>% dplyr::rename(city = ciudad, country = pais) %>% dplyr::select(place_id, city, country, lat, lon, wikipedia)
    
    # stages (canònic)
    st <- td$stages
    desc_vec <- col_first_existing(st, c("description","descripció","descripción"))
    medi_vec <- col_first_existing(st, c("medi","modo"))
    ruta_vec <- col_first_existing(st, c("ruta","route","trayecto","trajeto"))
    for (nm in c("etapa","from_id","to_id","date")) if (!nm %in% names(st)) st[[nm]] <- NA
    stages_out <- st %>% dplyr::mutate(description = desc_vec, medi = medi_vec, ruta = ruta_vec) %>% dplyr::select(etapa, from_id, to_id, date, description, medi, ruta)
    
    # hotels (opcional)
    hotels_out <- NULL
    if (!is.null(td$hotels) && nrow(td$hotels)) {
      h <- td$hotels
      for (nm in c("hotel_name","hotel_link","details","hotel_lat","hotel_lon")) if (!nm %in% names(h)) h[[nm]] <- if (grepl("_lat$|_lon$", nm)) NA_real_ else NA_character_
      h$hotel_lat <- suppressWarnings(as.numeric(h$hotel_lat))
      h$hotel_lon <- suppressWarnings(as.numeric(h$hotel_lon))
      hotels_out <- h %>% dplyr::select(place_id, hotel_name, hotel_link, details, hotel_lat, hotel_lon)
    }
    
    # interes (opcional)
    interes_out <- NULL
    if (!is.null(td$interes) && nrow(td$interes)) {
      inter <- td$interes
      for (nm in c("descriptor","ciudad","pais","lat","lon","tipus","link","observacio")) if (!nm %in% names(inter)) inter[[nm]] <- NA
      inter$lat <- suppressWarnings(as.numeric(inter$lat))
      inter$lon <- suppressWarnings(as.numeric(inter$lon))
      interes_out <- inter %>% dplyr::select(descriptor, ciudad, pais, lat, lon, tipus, link, observacio)
    }
    
    # Escriure
    dir.create("www", showWarnings = FALSE, recursive = TRUE)
    base_name <- trimws(base_name %||% "")
    if (!nzchar(base_name)) base_name <- paste0("itinerari_", Sys.Date())
    base_name <- gsub("[^[:alnum:]_\\x2D ]+", "_", base_name)
    fname <- paste0(base_name, ".xlsx")
    save_path <- file.path("www", fname)
    
    if (file.exists(save_path) && !isTRUE(overwrite)) {
      showNotification(sprintf("❗ L'arxiu '%s' ja existeix a www/. Marca 'Sobreescriure' o tria un altre nom.", fname), type = "warning", duration = 6)
      return(invisible(FALSE))
    }
    
    ok <- TRUE; err_msg <- NULL
    tryCatch({
      writexl::write_xlsx(Filter(Negate(is.null), list(
        places = places_out, stages = stages_out, hotels = hotels_out, interes = interes_out
      )), path = save_path)
    }, error = function(e) { ok <<- FALSE; err_msg <<- e$message })
    
    if (ok) {
      showNotification(sprintf("✅ Fitxer desat a www/: %s", fname), type = "message", duration = 6)
      if (isTRUE(select_after)) updateSelectInput(session, "proc_trip_www", choices = c("(cap)", list_www_xlsx()), selected = fname)
    } else {
      showNotification(paste0("⚠️ No s’ha pogut escriure a www/: ", err_msg), type = "error", duration = 10)
    }
    invisible(ok)
  }
  
  observeEvent(input$save_places_proc, {
    if (!isTRUE(input$allow_edit_process)) { showNotification("Activa «Permetre edició» per desar.", type = "warning"); return() }
    td <- current_td(); if (is.null(td)) { showNotification("No hi ha dades carregades per desar.", type = "warning"); return() }
    trip_data(td) # guarda en memòria
    showNotification("💾 Canvis guardats en memòria.", type = "message")
    write_trip_to_www(td = td, base_name = input$www_filename_proc, overwrite = isTRUE(input$overwrite_www_proc), select_after = TRUE)
  })
  
  # ---------- Modals: format i info ----------
  observeEvent(input$show_format, {
    showModal(modalDialog(
      title = "Format esperat de l’Excel (multilingüe)", size = "l", easyClose = TRUE, footer = modalButton("Tancar"),
      HTML(paste0(
        "<b>Document excel amb 4 fulls:</b><br>",
        "<ul>",
        "<li><b>Places</b>: <i>llocs</i> / <i>lugares</i> / <i>places</i></li> (obligatori)",
        "<li><b>Etapes</b>: <i>etapes</i> / <i>etapas</i> / <i>stages</i></li> (obligatori)",
        "<li><b>Hotels</b>: <i>hotels</i> / <i>hoteles</i></li> (opcional)",
        "<li><b>Interes</b>: <i>interes</i> / <i>interés</i> / <i>interests</i> / <i>interest</i> / <i>interesos</i></li> (opcional)",
        "</ul>",
        "<b>Noms de les columnes</b> (sinònims ca/es/en admesos).",
        "<br><br><b>Places</b>: place_id | city | country | lat | lon | wikipedia",
        "<br><br><b>Stages</b>: etapa | from_id | to_id | date | description | medi | ruta",
        "<br><b>Hotels</b>: place_id | hotel_name | hotel_link | details | hotel_lat | hotel_lon",
        "<br><b>Interes</b>: descriptor | lat | lon | city | country | details/observacio | type/tipus | link",
        "<br><br>Valors de <i>medi</i>: vol/avió/avion/flight/plane, cotxe/coche/car, tren/train, vaixell/barco/ferry/ship/boat.",
        "<br>Valors de <i>tipus</i>: art/cultura/foto/fotografia/gastronomia/història/mercat/mirador/monument/museu/natura/paisatge/panoràmica/parc."
      ))
    ))
  })
  
  observeEvent(input$show_info, {
    showModal(modalDialog(
      title = "Informació rellevant dels països a visitar", size = "l", easyClose = TRUE, footer = modalButton("Tancar"),
      HTML(paste0(
        "<h4><strong>ARGENTINA</strong></h4>",
        "<ul>",
        "<li><strong>DOCUMENTACIÓ:</strong> UE: passaport, bitllet anada i tornada, i mitjans econòmics. Estades &lt;90 dies sense visat. Declara aliments peribles (multa ~100 USD).</li>",
        "<li><strong>HORA LOCAL:</strong> GMT −3 (sense canvi d’hora).</li>",
        "<li><strong>IDIOMA:</strong> castellà; anglès en àrees turístiques.</li>",
        "<li><strong>INTERNET:</strong> WIFI estesa; opcional SIM local (Movistar, Claro, Tuenti) o eSIM.</li>",
        "<li><strong>ENDOLLS:</strong> tipus C i I.</li>",
        "<li><strong>MONEDA:</strong> peso argentí (ARS). Comprova el canvi abans de viatjar.</li>",
        "</ul>",
        "<h4><strong>XILE</strong></h4>",
        "<ul>",
        "<li><strong>DOCUMENTACIÓ:</strong> UE: sense visat &lt;90 dies. Targeta de turisme a l’entrada. Declara aliments peribles (multa ~100 USD).</li>",
        "<li><strong>HORA LOCAL:</strong> GMT −4 (continental; zones especials poden variar).</li>",
        "<li><strong>IDIOMA:</strong> castellà; anglès en punts turístics.</li>",
        "<li><strong>INTERNET:</strong> WIFI estesa; SIM (Movistar, Entel) o eSIM.</li>",
        "<li><strong>ENDOLLS:</strong> tipus C. Tensió 220 V.</li>",
        "<li><strong>MONEDA:</strong> peso xilè (CLP). Comprova el canvi abans de sortir.</li>",
        "</ul>"
      ))
    ))
  })
}

shinyApp(ui, server)

