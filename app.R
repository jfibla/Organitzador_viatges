# app.R — Geolocator (SIMPLE VERSION: ONLY "process" MODE)
# Multilingual EN/CA/ES with live switching
# Editing and geolocalization

# =====================
# Libraries
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
library(tmaptools)   # <- needed for geocode_OSM
library(bslib)
library(waiter)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# =====================
# i18n dictionary + helpers
# =====================
i18n <- list(
  en = list(
    title = "Travel Itinerary Geolocator",
    btn_format = "Input format",
    btn_info = "Information",
    from_disk = "From local disk (.xlsx)",
    from_www  = "From 'www/' folder",
    refresh   = "Refresh",
    load_sel  = "Load selected file",
    it_params = "Itinerary parameters",
    show_all  = "Show all stages",
    show_hotels_map = "Show hotels on the map",
    allow_edit = "Allow edit",
    return_to_visor = "Return to travel visor",
    stage_detail = "Stage details",
    filter_by = "Filter by type:",
    types = c("Itinerary","Flights","Car","Train","Boat","Visit"),
    save_changes = "Edit files",
    file_name = "File name (without extension)",
    overwrite = "Overwrite if it already exists",
    save_to_www = "Save to memory and www/",
    tip_edit = "Tip: tick «Allow editing» to be able to modify tables.",
    map = "Map",
    date = "Date:",
    places_tab = "Places",
    stages_tab = "Stages",
    hotels_tab = "Hotels",
    interest_tab = "Interest",
    hotel = "Hotel",
    places_interest = "Places of interest",
    itinerary_full = "Full itinerary",
    stages_label = "Stages",
    dest_cities = "Destination cities",
    stage = "Stage",
    origin = "Origin",
    destination = "Destination",
    description = "Description",
    no_stage_for_filter = "No stages for the selected filter.",
    no_hotels_itinerary = "No hotels for the itinerary.",
    no_hotels_day = "No hotels assigned for this day.",
    no_interest = "No places of interest.",
    no_interest_city = "No places of interest for this city.",
    places_to_visit_in = "Places to visit in · ",
    hotel_website = "Hotel website",
    unnamed_hotel = "Unnamed hotel",
    hotel_no_link_coords = "Hotel without link or coordinates.",
    transit_no_hotel = "Transit stage: no hotel assigned (neither origin nor destination).",
    route_block_title = "Stage %s · %s → %s",
    see_wikipedia = "View on Wikipedia",
    notice_loaded = "✅ Itinerary loaded",
    notice_loaded_default = "✅ Loaded by default: ",
    warn_pick_www = "📂 Select a file from www/.",
    err_read_default = "❌ Error reading default file from www/.",
    err_read_file = "❌ Error reading the file.",
    warn_missing_coords = "⚠️ Some places have no coordinates: ",
    warn_edit_to_save = "Enable «Allow editing» to save.",
    warn_no_data_to_save = "No data loaded to save.",
    saved_memory = "💾 Changes saved in memory.",
    saved_www = "✅ File saved to www/: ",
    warn_exists = "❗ File already exists in www/. Tick 'Overwrite' or choose another name.",
    err_write_www = "⚠️ Could not write to www/: ",
    fmt_modal_title = "Expected Excel format (multilingual)",
    close = "Close",
    fmt_sheets = "<b>Excel document with 4 sheets:</b><br>
<ul>
<li><b>Places</b>: <i>llocs</i> / <i>lugares</i> / <i>places</i></li> (mandatory)
<li><b>Stages</b>: <i>etapes</i> / <i>etapas</i> / <i>stages</i></li> (mandatory)
<li><b>Hotels</b>: <i>hotels</i> / <i>hoteles</i></li> (optional)
<li><b>Interest</b>: <i>interes</i> / <i>interés</i> / <i>interests</i> / <i>interest</i> / <i>interesos</i></li> (optional)
</ul>
<b>Column names</b> (accepted synonyms ca/es/en).<br><br>
<b>Places</b>: place_id | city | country | lat | lon | wikipedia
<br><br><b>Stages</b>: etapa | from_id | to_id | date | description | medi | ruta
<br><b>Hotels</b>: place_id | hotel_name | hotel_link | details | hotel_lat | hotel_lon
<br><b>Interest</b>: descriptor | lat | lon | city | country | details/observacio | type/tipus | link
<br><br>Values for <i>medi</i>: flight/plane/vol/avió/avion, car/cotxe/coche, train/tren, boat/ferry/vaixell.
<br>Values for <i>type</i>: art/culture/photo/photography/gastronomy/history/market/viewpoint/monument/museum/nature/landscape/panorama/park.",
    info_modal_title = "Relevant information about countries to visit",
    ar_title = "ARGENTINA",
    cl_title = "CHILE",
    docu = "DOCUMENTATION",
    local_time = "LOCAL TIME",
    language = "LANGUAGE",
    internet = "INTERNET",
    plugs = "PLUGS",
    currency = "CURRENCY",
    none = "(none)",
    
    # Editing tools (EN)
    ed_tools_title   = "Editing tools",
    ed_auto_complete = "Auto-complete: coords + Wikipedia + hotels + interest",
    ed_geo_fill_sel  = "Geocode selected places",
    ed_manual_title  = "Manual coordinates",
    ed_place_pick    = "Place to update:",
    ed_lat           = "Lat",
    ed_lon           = "Lon",
    ed_use_map_click = "Use last map click",
    ed_apply_manual  = "Apply coordinates",
    ed_wiki_title    = "Wikipedia links",
    ed_wiki_fill_sel = "Fill Wikipedia (selected)",
    ed_working       = "Working… this may take a few seconds",
    ed_auto_progress = "Autocompleting",
    ed_geo_places    = "Geocoding places…",
    ed_wiki_places   = "Fetching Wikipedia links…",
    ed_geo_hotels    = "Geocoding hotels…",
    ed_geo_interest  = "Geocoding points of interest…",
    ed_wiki_interest = "Fetching POI links…",
    done             = "Done ✅",
    ed_done          = "✅ Autocomplete finished. Remaining coords: {geo} | Remaining Wikipedia: {wiki}",
    ed_geo_sel_done  = "✅ Coordinates filled for selected places.",
    ed_wiki_sel_done = "✅ Wikipedia filled for selected places.",
    ed_manual_done   = "📍 Coordinates updated."
  ),
  es = list(
    title = "Geolocalizador de Itinerario de Viaje",
    btn_format = "Formato de entrada",
    btn_info = "Información",
    from_disk = "Desde disco local (.xlsx)",
    from_www  = "Desde carpeta 'www/'",
    refresh   = "Actualizar",
    load_sel  = "Cargar archivo seleccionado",
    it_params = "Parámetros del itinerario",
    show_all  = "Mostrar todas las etapas",
    show_hotels_map = "Mostrar hoteles en el mapa",
    allow_edit = "Permitir edición",
    return_to_visor = "Volver al visor de viaje",
    stage_detail = "Detalles de la etapa",
    filter_by = "Filtrar por tipo:",
    types = c("Itinerario","Vuelos","Coche","Tren","Barco","Visita"),
    save_changes = "Editar archivos",
    file_name = "Nombre de archivo (sin extensión)",
    overwrite = "Sobrescribir si ya existe",
    save_to_www = "Guardar en memoria y www/",
    tip_edit = "Consejo: marca «Permitir edición» para poder modificar tablas.",
    map = "Mapa",
    date = "Fecha:",
    places_tab = "Lugares",
    stages_tab = "Etapas",
    hotels_tab = "Hoteles",
    interest_tab = "Interés",
    hotel = "Hotel",
    places_interest = "Lugares de interés",
    itinerary_full = "Itinerario completo",
    stages_label = "Etapas",
    dest_cities = "Ciudades destino",
    stage = "Etapa",
    origin = "Origen",
    destination = "Destino",
    description = "Descripción",
    no_stage_for_filter = "No hay etapas para el filtro seleccionado.",
    no_hotels_itinerary = "Sin hoteles para el itinerario.",
    no_hotels_day = "Sin hoteles asignados para este día.",
    no_interest = "Sin lugares de interés.",
    no_interest_city = "Sin lugares de interés para esta ciudad.",
    places_to_visit_in = "Lugares para visitar en · ",
    hotel_website = "Web del hotel",
    unnamed_hotel = "Hotel sin nombre",
    hotel_no_link_coords = "Hotel sin enlace o coordenadas.",
    transit_no_hotel = "Etapa de tránsito: sin hotel asignado (ni en origen ni en destino).",
    route_block_title = "Etapa %s · %s → %s",
    see_wikipedia = "Ver en Wikipedia",
    notice_loaded = "✅ Itinerario cargado",
    notice_loaded_default = "✅ Cargado por defecto: ",
    warn_pick_www = "📂 Selecciona un archivo de www/.",
    err_read_default = "❌ Error leyendo el archivo por defecto de www/.",
    err_read_file = "❌ Error leyendo el archivo.",
    warn_missing_coords = "⚠️ Hay lugares sin coordenadas: ",
    warn_edit_to_save = "Activa «Permitir edición» para guardar.",
    warn_no_data_to_save = "No hay datos cargados para guardar.",
    saved_memory = "💾 Cambios guardados en memoria.",
    saved_www = "✅ Archivo guardado en www/: ",
    warn_exists = "❗ El archivo ya existe en www/. Marca 'Sobrescribir' o elige otro nombre.",
    err_write_www = "⚠️ No se pudo escribir en www/: ",
    fmt_modal_title = "Formato esperado del Excel (multilingüe)",
    close = "Cerrar",
    fmt_sheets = "<b>Documento Excel con 4 hojas:</b><br>
<ul>
<li><b>Places</b>: <i>llocs</i> / <i>lugares</i> / <i>places</i></li> (obligatoria)
<li><b>Stages</b>: <i>etapes</i> / <i>etapas</i> / <i>stages</i></li> (obligatoria)
<li><b>Hotels</b>: <i>hotels</i> / <i>hoteles</i></li> (opcional)
<li><b>Interest</b>: <i>interes</i> / <i>interés</i> / <i>interests</i> / <i>interest</i> / <i>interesos</i></li> (opcional)
</ul>
<b>Nombres de columnas</b> (se aceptan sinónimos en ca/es/en).<br><br>
<b>Places</b>: place_id | city | country | lat | lon | wikipedia
<br><br><b>Stages</b>: etapa | from_id | to_id | date | description | medi | ruta
<br><b>Hotels</b>: place_id | hotel_name | hotel_link | details | hotel_lat | hotel_lon
<br><b>Interest</b>: descriptor | lat | lon | city | country | details/observacio | type/tipus | link
<br><br>Valores de <i>medi</i>: vuelo/avión/flight/plane/vol/avió/avion, coche/carro/cotxe/car, tren/train, barco/ferry/vaixell/ship/boat.
<br>Valores de <i>type</i>: arte/cultura/foto/fotografía/gastronomía/historia/mercado/mirador/monumento/museo/naturaleza/paisaje/panorama/parque.",
    info_modal_title = "Información relevante de los países a visitar",
    ar_title = "ARGENTINA",
    cl_title = "CHILE",
    docu = "DOCUMENTACIÓN",
    local_time = "HORA LOCAL",
    language = "IDIOMA",
    internet = "INTERNET",
    plugs = "ENCHUFES",
    currency = "MONEDA",
    none = "(ninguno)",
    
    # Editing tools (ES)
    ed_tools_title   = "Herramientas de edición",
    ed_auto_complete = "Autocompletar: coords + Wikipedia + hoteles + interés",
    ed_geo_fill_sel  = "Geocodificar lugares seleccionados",
    ed_manual_title  = "Coordenadas manuales",
    ed_place_pick    = "Lugar a actualizar:",
    ed_lat           = "Lat",
    ed_lon           = "Lon",
    ed_use_map_click = "Usar último clic en el mapa",
    ed_apply_manual  = "Aplicar coordenadas",
    ed_wiki_title    = "Enlaces de Wikipedia",
    ed_wiki_fill_sel = "Completar Wikipedia (seleccionados)",
    ed_working       = "Trabajando… puede tardar unos segundos",
    ed_auto_progress = "Autocompletando",
    ed_geo_places    = "Geocodificando lugares…",
    ed_wiki_places   = "Buscando enlaces de Wikipedia…",
    ed_geo_hotels    = "Geocodificando hoteles…",
    ed_geo_interest  = "Geocodificando puntos de interés…",
    ed_wiki_interest = "Buscando enlaces de interés…",
    done             = "Hecho ✅",
    ed_done          = "✅ Autocompletado finalizado. Coordenadas pendientes: {geo} | Wikipedia pendiente: {wiki}",
    ed_geo_sel_done  = "✅ Coordenadas completadas para los lugares seleccionados.",
    ed_wiki_sel_done = "✅ Wikipedia completada para los lugares seleccionados.",
    ed_manual_done   = "📍 Coordenadas actualizadas."
  ),
  ca = list(
    title = "Geolocalitzador d'Itinerari de Viatge",
    btn_format = "Format d'entrada",
    btn_info = "Informació",
    from_disk = "Des de disc local (.xlsx)",
    from_www  = "Des de la carpeta 'www/'",
    refresh   = "Actualitza",
    load_sel  = "Carrega l'arxiu seleccionat",
    it_params = "Paràmetres de l'itinerari",
    show_all  = "Mostra totes les etapes",
    show_hotels_map = "Mostra els hotels al mapa",
    allow_edit = "Permetre edició",
    return_to_visor = "Tornar al visor de viatge",
    stage_detail = "Detalls de l'etapa",
    filter_by = "Filtra per tipus:",
    types = c("Itinerari","Vols","Cotxe","Tren","Vaixell","Visita"),
    save_changes = "Editar arxius",
    file_name = "Nom del fitxer (sense extensió)",
    overwrite = "Sobreescriure si ja existeix",
    save_to_www = "Desa en memòria i a www/",
    tip_edit = "Astúcia: marca «Permetre edició» per poder canviar taules.",
    map = "Mapa",
    date = "Data:",
    places_tab = "Llocs",
    stages_tab = "Etapes",
    hotels_tab = "Hotels",
    interest_tab = "Interès",
    hotel = "Hotel",
    places_interest = "Llocs d'interès",
    itinerary_full = "Itinerari complet",
    stages_label = "Etapes",
    dest_cities = "Ciutats destí",
    stage = "Etapa",
    origin = "Origen",
    destination = "Destí",
    description = "Descripció",
    no_stage_for_filter = "No hi ha cap etapa per al filtre seleccionat.",
    no_hotels_itinerary = "Sense hotels per a l'itinerari.",
    no_hotels_day = "Sense hotels assignats per a aquest dia.",
    no_interest = "Sense llocs d'interès.",
    no_interest_city = "Sense llocs d'interès per a aquesta ciutat.",
    places_to_visit_in = "Llocs per visitar a · ",
    hotel_website = "Web de l'hotel",
    unnamed_hotel = "Hotel sense nom",
    hotel_no_link_coords = "Hotel sense enllaç o coordenades.",
    transit_no_hotel = "Etapa de trànsit: cap hotel assignat (ni a origen ni a destí).",
    route_block_title = "Etapa %s · %s → %s",
    see_wikipedia = "Veure a Viquipèdia",
    notice_loaded = "✅ Itinerari carregat",
    notice_loaded_default = "✅ Carregat per defecte: ",
    warn_pick_www = "📂 Selecciona un arxiu de www/.",
    err_read_default = "❌ Error llegint l'arxiu per defecte de www/.",
    err_read_file = "❌ Error llegint l'arxiu.",
    warn_missing_coords = "⚠️ Hi ha llocs sense coordenades: ",
    warn_edit_to_save = "Activa «Permetre edició» per desar.",
    warn_no_data_to_save = "No hi ha dades carregades per desar.",
    saved_memory = "💾 Canvis guardats en memòria.",
    saved_www = "✅ Fitxer desat a www/: ",
    warn_exists = "❗ L'arxiu ja existeix a www/. Marca 'Sobreescriure' o tria un altre nom.",
    err_write_www = "⚠️ No s’ha pogut escriure a www/: ",
    fmt_modal_title = "Format esperat del Excel (multilingüe)",
    close = "Tancar",
    fmt_sheets = "<b>Document Excel amb 4 fulls:</b><br>
<ul>
<li><b>Places</b>: <i>llocs</i> / <i>lugares</i> / <i>places</i></li> (obligatori)
<li><b>Stages</b>: <i>etapes</i> / <i>etapas</i> / <i>stages</i></li> (obligatori)
<li><b>Hotels</b>: <i>hotels</i> / <i>hoteles</i></li> (opcional)
<li><b>Interest</b>: <i>interes</i> / <i>interés</i> / <i>interests</i> / <i>interest</i> / <i>interesos</i></li> (opcional)
</ul>
<b>Noms de les columnes</b> (admet sinònims en ca/es/en).<br><br>
<b>Places</b>: place_id | city | country | lat | lon | wikipedia
<br><br><b>Stages</b>: etapa | from_id | to_id | date | description | medi | ruta
<br><b>Hotels</b>: place_id | hotel_name | hotel_link | details | hotel_lat | hotel_lon
<br><b>Interest</b>: descriptor | lat | lon | city | country | details/observacio | type/tipus | link
<br><br>Valors de <i>medi</i>: vol/avió/avion/flight/plane, cotxe/coche/car, tren/train, vaixell/barco/ferry/ship/boat.
<br>Valors de <i>type</i>: art/cultura/foto/fotografia/gastronomia/història/mercat/mirador/monument/museu/natura/paisatge/panoràmica/parc.",
    info_modal_title = "Informació rellevant dels països a visitar",
    ar_title = "ARGENTINA",
    cl_title = "XILE",
    docu = "DOCUMENTACIÓ",
    local_time = "HORA LOCAL",
    language = "IDIOMA",
    internet = "INTERNET",
    plugs = "ENDOLLS",
    currency = "MONEDA",
    none = "(cap)",
    
    # Editing tools (CA)
    ed_tools_title   = "Eines d'edició",
    ed_auto_complete = "Autocompletar: coords + Viquipèdia + hotels + interès",
    ed_geo_fill_sel  = "Geocodificar llocs seleccionats",
    ed_manual_title  = "Coordenades manuals",
    ed_place_pick    = "Lloc a actualitzar:",
    ed_lat           = "Lat",
    ed_lon           = "Lon",
    ed_use_map_click = "Usar l’últim clic al mapa",
    ed_apply_manual  = "Aplicar coordenades",
    ed_wiki_title    = "Enllaços Viquipèdia",
    ed_wiki_fill_sel = "Completar Viquipèdia (seleccionats)",
    ed_working       = "Treballant… pot trigar uns segons",
    ed_auto_progress = "Autocompletant",
    ed_geo_places    = "Geocodificant llocs…",
    ed_wiki_places   = "Cercant enllaços de Viquipèdia…",
    ed_geo_hotels    = "Geolocalitzant hotels…",
    ed_geo_interest  = "Geolocalitzant punts d'interès…",
    ed_wiki_interest = "Cercant enllaços d’interès…",
    done             = "Fet ✅",
    ed_done          = "✅ Autocompletat finalitzat. Coordenades pendents: {geo} | Viquipèdies pendents: {wiki}",
    ed_geo_sel_done  = "✅ Coordenades completades per als llocs seleccionats.",
    ed_wiki_sel_done = "✅ Viquipèdia completada per als llocs seleccionats.",
    ed_manual_done   = "📍 Coordenades actualitzades."
  )
)

.lang_default <- "en"
tr <- function(key, lang = .lang_default, data = NULL) {
  val <- tryCatch(i18n[[lang]][[key]], error = function(e) NULL)
  if (is.null(val)) return(key)
  if (!is.null(data) && is.list(data)) {
    for (nm in names(data)) {
      val <- gsub(paste0("\\{", nm, "\\}"), as.character(data[[nm]]), val, perl = TRUE)
    }
  }
  val
}

# =====================
# UI
# =====================
ui <- bslib::page_sidebar(
  tags$style(HTML("
  /* Reduce sidebar font size */
  .sidebar, .bslib-sidebar, .sidebar .form-group, .sidebar label, .sidebar input, .sidebar select {
    font-size: 1rem;   /* smaller text */
  }

  /* Optional: adjust headings in sidebar */
  .sidebar h4, .bslib-sidebar h4 {
    font-size: 1.2rem;      /* was ~1.25rem */
    font-weight: 600;
  }

  .sidebar h5 {
    font-size: 1.1rem;
    font-weight: 500;
  }

  /* Make buttons text a bit smaller too */
  .sidebar .btn {
    font-size: 1rem;
    padding: 4px 8px;
  }
")),
  
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = bslib::font_google("Inter"),
    "font-size-root" = "0.8125rem"  # ~13px
  ),
  title = tr("title", .lang_default),
  # -------- SIDEBAR: language + load + params + save --------
  sidebar = bslib::sidebar(
    # Language switcher
    div(class = "d-flex gap-2 mb-3",
        actionButton("lang_en", "EN", class = "btn-outline-secondary btn-sm"),
        actionButton("lang_ca", "CA", class = "btn-outline-secondary btn-sm"),
        actionButton("lang_es", "ES", class = "btn-outline-secondary btn-sm")
    ),
    uiOutput("edit_tools"),
    uiOutput("sidebar_text_blocks"),
    br(), br()
  ),
  
  waiter::useWaiter(),
  
  # -------- MAIN CONTENT --------
  h3(textOutput("stage_details_title")),
  fluidRow(
    column(6,
           uiOutput("detalle_etapa"),
           tags$hr(),
           h4(textOutput("hotel_title")),
           uiOutput("hoteles_panel")
    ),
    column(6,
           tags$h4(textOutput("poi_title")),
           uiOutput("interes_panel"),
           uiOutput("ruta_panel")
    )
  ),
  
  bslib::navset_tab(
    id = "main_tabs",
    bslib::nav_panel(
      title = textOutput("tab_map_title"),
      value = "map",
      bslib::layout_columns(
        col_widths = c(4,8),
        bslib::card(
          bslib::card_header(""),
          uiOutput("date_picker_ui")
        ),
        leafletOutput("mapa", height = "420px")
      )
    ),
    bslib::nav_panel(
      title = textOutput("tab_tables_title"),
      value = "tables",
      conditionalPanel(
        "input.allow_edit_process",
        bslib::navset_tab(
          id = "tables_tabs",
          bslib::nav_panel(title = textOutput("tab_places_title"),   value = "places",   DTOutput("places_table")),
          bslib::nav_panel(title = textOutput("tab_stages_title"),   value = "stages",   DTOutput("stages_table")),
          bslib::nav_panel(title = textOutput("tab_hotels_title"),   value = "hotels",   DTOutput("hotels_table")),
          bslib::nav_panel(title = textOutput("tab_interest_title"), value = "interest", DTOutput("interes_table"))
        )
      ),
      conditionalPanel(
        "!input.allow_edit_process",
        div(class = "p-3", em(textOutput("tip_edit_text")))
      )
    )
  ),
  
  # Styles
  tags$head(
    tags$style(HTML('
      .form-control, .selectize-input, .btn { font-size: 16px; }
      .btn { padding: 10px 14px; }
      @media (max-width: 576px) {
        .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter {
          float: none; text-align: left;
        }
      }
      .stage-card{border:1px solid #e6e6e6;border-radius:12px;padding:10px 12px;margin-bottom:8px;background:#fff;box-shadow:0 1px 2px rgba(0,0,0,.04)}
    ')),
    tags$script("Shiny.addCustomMessageHandler('update-title', function(t){ document.title = t; });")
  )
)

# =====================
# SERVER
# =====================
server <- function(input, output, session) {
  # Preserve map view (zoom/center)
  view_state <- reactiveValues(center = NULL, zoom = NULL)
  observe({
    b <- input$mapa_bounds
    if (!is.null(b)) {
      view_state$center <- c(
        lng = as.numeric((b$west + b$east)/2),
        lat = as.numeric((b$south + b$north)/2)
      )
    }
  })
  observe({ if (!is.null(input$mapa_zoom)) view_state$zoom <- as.numeric(input$mapa_zoom) })
  
  # Language reactive
  lang <- reactiveVal("en")
  observeEvent(input$lang_en, lang("en"))
  observeEvent(input$lang_ca, lang("ca"))
  observeEvent(input$lang_es, lang("es"))
  observe({ session$sendCustomMessage("update-title", tr("title", lang())) })
  
  # Date range helper
  dates_range <- reactive({
    ee <- etapas_exp()
    if (is.null(ee) || !nrow(ee) || !"dia" %in% names(ee) || all(is.na(ee$dia))) return(NULL)
    list(min = as.Date(min(ee$dia, na.rm = TRUE)),
         max = as.Date(max(ee$dia, na.rm = TRUE)))
  })
  
  # Keep selected day stable
  current_day <- reactiveVal(NULL)
  observeEvent(etapas_exp(), {
    rng <- dates_range(); if (is.null(rng)) return()
    sel <- current_day()
    if (is.null(sel) || is.na(sel) || sel < rng$min || sel > rng$max) current_day(rng$min)
    shinyWidgets::updateAirDateInput(session, "dia", value = current_day(),
                                     options = list(minDate = rng$min, maxDate = rng$max))
  })
  observeEvent(input$dia, { if (!is.null(input$dia) && !is.na(input$dia)) current_day(as.Date(input$dia)) })
  
  # Utilities
  coerce_like <- function(value, template) {
    if (is.numeric(template))      suppressWarnings(as.numeric(value))
    else if (inherits(template, "Date")) as.Date(value)
    else if (is.logical(template)) as.logical(value)
    else                            as.character(value)
  }
  list_www_xlsx <- function() { if (!dir.exists("www")) character(0) else sort(list.files("www", pattern="\\.xlsx?$", full.names = FALSE)) }
  col_first_existing <- function(df, candidates, default = NA_character_) {
    for (nm in candidates) if (nm %in% names(df)) return(df[[nm]])
    rep(default, nrow(df))
  }
  .pick_col <- function(df, candidates) {
    nms <- tolower(names(df)); cand <- tolower(candidates)
    idx <- match(cand, nms); idx <- idx[!is.na(idx)]
    if (length(idx)) idx[1] else NA_integer_
  }
  
  # Active dataset
  trip_data <- reactiveVal(NULL)
  current_td <- reactive(trip_data())
  
  # Excel read + normalization
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
      for (al in aliases) { hit <- which(s_low == al); if (length(hit)) return(all[hit[1]]) }
      NA_character_
    }
    sh_places  <- find_sheet(.sheet_aliases$places)
    sh_stages  <- find_sheet(.sheet_aliases$stages)
    sh_hotels  <- find_sheet(.sheet_aliases$hotels)
    sh_interes <- find_sheet(.sheet_aliases$interes)
    stopifnot(!is.na(sh_places), !is.na(sh_stages))
    
    places <- readxl::read_excel(path, sheet = sh_places)
    names(places) <- tolower(trimws(names(places)))
    if (!"place_id" %in% names(places)) names(places)[names(places) %in% c("lloc_id","lugar_id")] <- "place_id"
    if (!"city"     %in% names(places)) names(places)[names(places) %in% c("ciutat","ciudad")] <- "city"
    if (!"country"  %in% names(places)) names(places)[names(places) %in% c("pais","país")] <- "country"
    if (!"wikipedia"%in% names(places)) places$wikipedia <- NA_character_
    places$lat <- suppressWarnings(as.numeric(places$lat))
    places$lon <- suppressWarnings(as.numeric(places$lon))
    places <- places |> dplyr::rename(ciudad = city, pais = country)
    
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
      h <- dplyr::left_join(h, places |> dplyr::select(place_id, ciudad, pais), by = "place_id")
      h
    } else NULL
    
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
  
  # Sidebar (localized)
  output$sidebar_text_blocks_xxx <- renderUI({
    l <- lang()
    tagList(
      fluidRow(
        column(6, actionButton("show_format", tr("btn_format", l))),
        column(6, actionButton("show_info",   tr("btn_info",   l)))
      ),
      h5(tr("from_disk", l)),
      fileInput("proc_trip_file", NULL, accept = c(".xlsx", ".xls")),
      h5(tr("from_www", l)),
      fluidRow(
        column(12, actionButton("proc_refresh_www", tr("refresh", l), class = "btn-secondary w-100 mb-2")),
        column(12, selectInput("proc_trip_www", "", choices = c(tr("none", l))))
      ),
      actionButton("proc_load_www", tr("load_sel", l), class = "btn-primary w-100"),
      tags$hr(),
      
      # --- this block will reappear only when not editing ---
      uiOutput("itinerary_controls"),
      
      # --- always visible ---
      checkboxInput("allow_edit_process", tr("allow_edit", l), value = FALSE),
      
      tags$hr(),
      h4(tr("save_changes", l)),
      textInput("www_filename_proc", tr("file_name", l), value = "itinerary_view"),
      checkboxInput("overwrite_www_proc", tr("overwrite", l), value = FALSE),
      actionButton("save_places_proc", tr("save_to_www", l), class = "btn-warning w-100"),
      br(), br(),
      helpText(tr("tip_edit", l))
    )
  })
  
  output$sidebar_text_blocks <- renderUI({
    l <- lang()
    allow <- isTRUE(input$allow_edit_process)
    
    # --- Dynamic label depending on state ---
    label_text <- if (allow) {
      tr("return_to_visor", l)   # new key in your i18n dict
    } else {
      tr("allow_edit", l)
    }
    
    # Always include the toggle first
    base_toggle <- checkboxInput(
      "allow_edit_process",
      label_text,
      value = allow
    )
    
    if (allow) {
      # EDITING MODE: only show toggle + save controls
      tagList(
        base_toggle,
        tags$hr(),
        h4(tr("save_changes", l)),
        textInput(
          "www_filename_proc",
          tr("file_name", l),
          value = input$www_filename_proc %||% "itinerary_view"
        ),
        checkboxInput(
          "overwrite_www_proc",
          tr("overwrite", l),
          value = isTRUE(input$overwrite_www_proc)
        ),
        actionButton("save_places_proc", tr("save_to_www", l), class = "btn-warning w-100"),
        br(), br(),
        helpText(tr("tip_edit", l))
      )
      
    } else {
      # NORMAL MODE: full panel
      tagList(
        base_toggle,
        fluidRow(
          column(6, actionButton("show_format", tr("btn_format", l))),
          column(6, actionButton("show_info",   tr("btn_info",   l)))
        ),
        h5(tr("from_disk", l)),
        fileInput("proc_trip_file", NULL, accept = c(".xlsx", ".xls")),
        h5(tr("from_www", l)),
        fluidRow(
          column(12, actionButton("proc_refresh_www", tr("refresh", l),
                                  class = "btn-secondary w-100 mb-2")),
          column(12, selectInput("proc_trip_www", "", choices = c(tr("none", l))))
        ),
        actionButton("proc_load_www", tr("load_sel", l), class = "btn-primary w-100"),
        tags$hr(),
        uiOutput("itinerary_controls"),
        tags$hr(),
        h4(tr("save_changes", l)),
        textInput(
          "www_filename_proc",
          tr("file_name", l),
          value = input$www_filename_proc %||% "itinerary_view"
        ),
        checkboxInput(
          "overwrite_www_proc",
          tr("overwrite", l),
          value = isTRUE(input$overwrite_www_proc)
        ),
        actionButton("save_places_proc", tr("save_to_www", l), class = "btn-warning w-100"),
        br(), br(),
        helpText(tr("tip_edit", l))
      )
    }
  })
  
  output$itinerary_controls <- renderUI({
    l <- lang()
    # Hide the controls entirely while editing
    if (isTRUE(input$allow_edit_process)) return(NULL)
    
    # Preserve current values if they exist, else use defaults
    show_all_val   <- isTRUE(input$ver_todas)
    hotels_map_val <- if (is.null(input$show_hotels_on_map)) TRUE else isTRUE(input$show_hotels_on_map)
    filtro_choices <- tr("types", l)
    filtro_sel     <- input$filtro_tipo %||% filtro_choices[1]
    
    tagList(
      h4(tr("it_params", l)),
      checkboxInput("ver_todas", tr("show_all", l), value = show_all_val),
      checkboxInput("show_hotels_on_map", tr("show_hotels_map", l), value = hotels_map_val),
      h4(tr("stage_detail", l)),
      selectInput("filtro_tipo", tr("filter_by", l),
                  choices = filtro_choices,
                  selected = filtro_sel)
    )
  })
  
  output$sidebar_text_blocks_xxx <- renderUI({
    l <- lang()
    # When editing is ON: show a *minimal* panel so you can toggle it off and still save/filter
    if (isTRUE(input$allow_edit_process)) {
      return(tagList(
        h4(tr("it_params", l)),
        checkboxInput("allow_edit_process", tr("allow_edit", l), value = TRUE),  # <-- keep this visible!
        checkboxInput("ver_todas", tr("show_all", l), value = input$ver_todas %||% FALSE),
        checkboxInput("show_hotels_on_map", tr("show_hotels_map", l), value = input$show_hotels_on_map %||% TRUE),
        
        h4(tr("stage_detail", l)),
        selectInput("filtro_tipo", tr("filter_by", l),
                    choices = tr("types", l),
                    selected = input$filtro_tipo %||% tr("types", l)[1]),
        
        tags$hr(),
        h4(tr("save_changes", l)),
        textInput("www_filename_proc", tr("file_name", l), value = input$www_filename_proc %||% "itinerary_view"),
        checkboxInput("overwrite_www_proc", tr("overwrite", l), value = isTRUE(input$overwrite_www_proc)),
        actionButton("save_places_proc", tr("save_to_www", l), class = "btn-warning w-100"),
        
        br(), helpText(tr("tip_edit", l))
      ))
    }
    
    # When editing is OFF: show the full utilities block (upload, www picker, info/format, etc.)
    tagList(
      fluidRow(
        column(6, actionButton("show_format", tr("btn_format", l))),
        column(6, actionButton("show_info",   tr("btn_info",   l)))
      ),
      h5(tr("from_disk", l)),
      fileInput("proc_trip_file", NULL, accept = c(".xlsx", ".xls")),
      
      h5(tr("from_www", l)),
      fluidRow(
        column(12, actionButton("proc_refresh_www", tr("refresh", l), class = "btn-secondary w-100 mb-2")),
        column(12, selectInput("proc_trip_www", "", choices = c(tr("none", l))))
      ),
      actionButton("proc_load_www", tr("load_sel", l), class = "btn-primary w-100"),
      
      tags$hr(),
    #  h4(tr("it_params", l)),
    #  checkboxInput("ver_todas", tr("show_all", l), value = input$ver_todas %||% FALSE),
    #  checkboxInput("show_hotels_on_map", tr("show_hotels_map", l), value = input$show_hotels_on_map %||% TRUE),
    #  checkboxInput("allow_edit_process", tr("allow_edit", l), value = FALSE),
    #  
    #  h4(tr("stage_detail", l)),
    #  selectInput("filtro_tipo", tr("filter_by", l),
    #              choices = tr("types", l),
    #              selected = input$filtro_tipo %||% tr("types", l)[1]),
    h4(tr("it_params", l)),
    
    # --- Only visible when NOT editing ---
    conditionalPanel(
      condition = "!input.allow_edit_process",
      checkboxInput("ver_todas", tr("show_all", l), value = FALSE),
      checkboxInput("show_hotels_on_map", tr("show_hotels_map", l), value = TRUE),
      h4(tr("stage_detail", l)),
      selectInput("filtro_tipo", tr("filter_by", l),
                  choices = tr("types", l),
                  selected = tr("types", l)[1])
    ),
    
    # --- Always visible ---
    checkboxInput("allow_edit_process", tr("allow_edit", l), value = FALSE),
    
      tags$hr(),
      h4(tr("save_changes", l)),
      textInput("www_filename_proc", tr("file_name", l), value = input$www_filename_proc %||% "itinerary_view"),
      checkboxInput("overwrite_www_proc", tr("overwrite", l), value = isTRUE(input$overwrite_www_proc)),
      actionButton("save_places_proc", tr("save_to_www", l), class = "btn-warning w-100"),
      
      br(), br(),
      helpText(tr("tip_edit", l))
    )
  })
  
  output$tip_edit_text <- renderText(tr("tip_edit", lang()))
  
  # Load: local and www
  observeEvent(input$proc_trip_file, {
    req(input$proc_trip_file$datapath)
    td <- read_trip_data(input$proc_trip_file$datapath)
    trip_data(td)
    showNotification(tr("notice_loaded", lang()), type = "message")
  })
  observe({
    l <- lang()
    files <- list_www_xlsx()
    updateSelectInput(session, "proc_trip_www",
                      choices = c(tr("none", l), files),
                      selected = if (length(files)) files[1] else tr("none", l))
  })
  observeEvent(TRUE, {
    files <- list_www_xlsx()
    if (length(files)) {
      path <- file.path("www", files[1])
      td <- try(read_trip_data(path), silent = TRUE)
      if (!inherits(td, "try-error")) {
        trip_data(td)
        showNotification(paste0(tr("notice_loaded_default", lang()), files[1]), type = "message")
      } else {
        showNotification(tr("err_read_default", lang()), type = "error")
      }
    }
  }, once = TRUE, ignoreInit = FALSE)
  observeEvent(input$proc_refresh_www, {
    l <- lang()
    files <- list_www_xlsx()
    updateSelectInput(session, "proc_trip_www",
                      choices = c(tr("none", l), files),
                      selected = if (length(files)) files[1] else tr("none", l))
  })
  observeEvent(input$proc_load_www, {
    l <- lang()
    if (is.null(input$proc_trip_www) || input$proc_trip_www == tr("none", l)) {
      showNotification(tr("warn_pick_www", l), type = "warning"); return()
    }
    path <- file.path("www", input$proc_trip_www)
    td <- try(read_trip_data(path), silent = TRUE)
    if (inherits(td, "try-error")) {
      showNotification(tr("err_read_file", l), type = "error"); return()
    }
    trip_data(td)
    showNotification(paste0("✅ ", tr("load_sel", l), ": ", input$proc_trip_www), type = "message")
  })
  
  # --- Wikipedia + Geocode helpers ---
  wikipedia_missing <- function(places_df, lang_api = "es") {
    if (!"wikipedia" %in% names(places_df)) places_df$wikipedia <- NA_character_
    needs_wiki <- is.na(places_df$wikipedia) | places_df$wikipedia == ""
    if (!any(needs_wiki) || !"ciudad" %in% names(places_df)) return(places_df)
    qvec <- places_df$ciudad[needs_wiki]
    fetch_wiki <- function(q) {
      url <- paste0("https://", lang_api, ".wikipedia.org/w/api.php")
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
    out <- try(tmaptools::geocode_OSM(query, as.data.frame = TRUE), silent = TRUE)
    if (inherits(out, "try-error") || is.null(out)) return(c(NA_real_, NA_real_))
    if (inherits(out, "sf")) {
      coords <- sf::st_coordinates(out); return(c(coords[1,"X"], coords[1,"Y"]))
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
    withProgress(message = tr("ed_geo_places", lang()), value = 0, {
      clean_city <- function(x) {
        x <- trimws(as.character(x))
        x <- sub(" -.*$", "", x); x <- sub(":.*$", "", x)
        x <- sub("/.*$", "", x);  x <- sub(",.*$", "", x); x
      }
      ciudad_clean <- vapply(df$ciudad, clean_city, FUN.VALUE = "")
      pais_orig <- if ("pais" %in% names(df)) as.character(df$pais) else rep(NA_character_, nrow(df))
      pais_final <- ifelse(is.na(pais_orig) | pais_orig=="", default_country, pais_orig)
      q_full <- ifelse(!is.na(pais_final) & nzchar(pais_final), paste(ciudad_clean, pais_final, sep = ", "), ciudad_clean)
      queries <- tibble::tibble(query = q_full[need_idx])
      res_bulk <- try(geo(queries, address = query, method = "osm", lat = latitude, long = longitude, limit = 1, verbose = FALSE), silent = TRUE)
      if (!inherits(res_bulk, "try-error") && is.data.frame(res_bulk) && nrow(res_bulk) == length(need_idx)) {
        df$lat[need_idx] <- res_bulk$latitude; df$lon[need_idx] <- res_bulk$longitude
      }
      still_idx <- which(is.na(df$lat) | is.na(df$lon))
      if (length(still_idx)) {
        for (i in still_idx) {
          coords <- geocode_one(q_full[i]); df$lon[i] <- coords[1]; df$lat[i] <- coords[2]
          incProgress(1/length(still_idx)); Sys.sleep(0.05)
        }
      }
    })
    df$lat <- suppressWarnings(as.numeric(df$lat)); df$lon <- suppressWarnings(as.numeric(df$lon)); df
  }
  fill_missing_hotel_coords <- function(h, places_df = NULL) {
    if (is.null(h) || !nrow(h)) return(h)
    for (nm in c("hotel_lat","hotel_lon")) if (!nm %in% names(h)) h[[nm]] <- NA_real_
    need_idx <- which(is.na(h$hotel_lat) | is.na(h$hotel_lon))
    if (!length(need_idx)) return(h)
    withProgress(message = tr("ed_geo_hotels", lang()), value = 0, {
      city_by_id <- country_by_id <- NULL
      if (!is.null(places_df) && all(c("place_id","ciudad","pais") %in% names(places_df))) {
        city_by_id    <- setNames(as.character(places_df$ciudad), places_df$place_id)
        country_by_id <- setNames(as.character(places_df$pais),   places_df$place_id)
      }
      for (i in need_idx) {
        name <- if ("hotel_name" %in% names(h)) as.character(h$hotel_name[i]) else NA_character_
        pid  <- if ("place_id"   %in% names(h)) as.character(h$place_id[i])   else NA_character_
        parts <- list(name)
        if (!is.null(city_by_id) && !is.na(pid) && nzchar(pid)) parts <- c(parts, city_by_id[[pid]], country_by_id[[pid]])
        q <- paste(na.omit(parts), collapse = ", ")
        coords <- geocode_one(q); h$hotel_lon[i] <- coords[1]; h$hotel_lat[i] <- coords[2]
        incProgress(1/length(need_idx)); Sys.sleep(0.05)
      }
    })
    h$hotel_lat <- as.numeric(h$hotel_lat); h$hotel_lon <- as.numeric(h$hotel_lon); h
  }
  geocode_interes_missing <- function(h) {
    if (is.null(h) || !nrow(h)) return(h)
    for (nm in c("descriptor","ciutat","pais","lat","lon")) if (!nm %in% names(h)) h[[nm]] <- NA
    need_idx <- which(is.na(h$lat) | is.na(h$lon))
    if (!length(need_idx)) return(h)
    withProgress(message = tr("ed_geo_interest", lang()), value = 0, {
      for (i in need_idx) {
        q <- paste(na.omit(c(h$descriptor[i], h$ciutat[i], h$pais[i])), collapse = ", ")
        coords <- geocode_one(q); h$lon[i] <- coords[1]; h$lat[i] <- coords[2]
        incProgress(1/length(need_idx)); Sys.sleep(0.05)
      }
    })
    h$lat <- as.numeric(h$lat); h$lon <- as.numeric(h$lon); h
  }
  interes_link_missing <- function(inter_df, lang_api = "es") {
    h <- inter_df
    if (is.null(h) || !nrow(h)) return(h)
    if (!"link" %in% names(h)) h$link <- NA_character_
    need <- is.na(h$link) | h$link == ""
    if (!any(need)) return(h)
    fetch_wiki <- function(q) {
      url <- paste0("https://", lang_api, ".wikipedia.org/w/api.php")
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
    qvec <- paste(na.omit(h$descriptor[need]),
                  ifelse(is.na(h$ciudad[need]), "", h$ciudad[need]),
                  ifelse(is.na(h$pais[need]), "", h$pais[need]))
    h$link[need] <- vapply(qvec, fetch_wiki, FUN.VALUE = character(1))
    h
  }
  
  # Dates parse + expand
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
    if (!is.na(num) && num > 30000) return(as.Date("1899-12-30") + num)
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
  
  # Expanded stages per day
  etapas_exp <- reactive({
    td <- current_td(); req(td)
    places <- td$places; stages <- td$stages
    
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
  
  # Datepicker language mapping
  datepicker_lang <- reactive({
    switch(lang(),
           "en" = "en",
           "es" = "es",
           "ca" = "es",
           "en")
  })
  output$date_picker_ui <- renderUI({
    rng <- dates_range()
    value_day <- current_day() %||% (if (!is.null(rng)) rng$min else Sys.Date())
    min_opt   <- if (!is.null(rng)) rng$min else NULL
    max_opt   <- if (!is.null(rng)) rng$max else NULL
    
    airDatepickerInput(
      inputId   = "dia",
      label     = tr("date", lang()),
      value     = value_day,
      inline    = TRUE,
      language  = datepicker_lang(),
      autoClose = TRUE,
      minDate   = min_opt,
      maxDate   = max_opt
    )
  })
  
  etapas_by_day   <- reactive({ req(etapas_exp()); if (is.null(input$dia) || is.na(input$dia)) return(etapas_exp()[0, ]); dplyr::filter(etapas_exp(), dia == as.Date(input$dia)) })
  ver_todas_eff   <- reactive(isTRUE(input$ver_todas))
  show_hotels_eff <- reactive(isTRUE(input$show_hotels_on_map))
  etapas_chosen   <- reactive({ if (isTRUE(ver_todas_eff())) etapas_exp() else etapas_by_day() })
  
  iconos_etapas <- reactive({
    l <- lang()
    e <- etapas_chosen(); if (!nrow(e)) return(e[0, ])
    type_labels <- tr("types", l)
    dplyr::transmute(
      e,
      ciudad = ciudad_to,
      tipo = dplyr::case_when(
        modo == "vol"     ~ type_labels[2],
        modo == "cotxe"   ~ type_labels[3],
        modo == "tren"    ~ type_labels[4],
        modo == "vaixell" ~ type_labels[5],
        modo == "visita"  ~ type_labels[6],
        TRUE               ~ type_labels[1]
      ),
      lat = lat_to, lon = lon_to
    ) %>% dplyr::distinct()
  })
  etapas_filtradas <- reactive({
    df <- iconos_etapas(); if (!nrow(df)) return(df)
    if (is.null(input$filtro_tipo) || input$filtro_tipo == tr("types", lang())[1]) {
      dplyr::filter(df, tipo %in% tr("types", lang())[2:6])
    } else {
      dplyr::filter(df, tipo == input$filtro_tipo)
    }
  })
  iconos <- reactive({
    df <- etapas_filtradas()
    if (!nrow(df)) return(awesomeIcons(icon = "map-marker", iconColor = "white", library = "fa", markerColor = "cadetblue"))
    the_icon <- ifelse(df$tipo == tr("types", lang())[2], "plane",
                       ifelse(df$tipo == tr("types", lang())[3], "truck",
                              ifelse(df$tipo == tr("types", lang())[4], "train",
                                     ifelse(df$tipo == tr("types", lang())[5], "ship",
                                            ifelse(df$tipo == tr("types", lang())[6],"flag","map-marker")))))
    the_color <- ifelse(df$tipo == tr("types", lang())[2], "blue",
                        ifelse(df$tipo == tr("types", lang())[3], "red",
                               ifelse(df$tipo == tr("types", lang())[4], "green",
                                      ifelse(df$tipo == tr("types", lang())[5], "purple",
                                             ifelse(df$tipo == tr("types", lang())[6], "darkgreen", "cadetblue")))))
    awesomeIcons(icon = the_icon, iconColor = "white", library = "fa", markerColor = the_color)
  })
  interes_icons <- function(tipus_vec) {
    icon_name <- ifelse(tipus_vec %in% c("paisatge","natura","parc","platja","landscape","nature","park","beach"), "tree",
                        ifelse(tipus_vec %in% c("monument","historia","història","centre","history","center","centre","monumento"), "university",
                               ifelse(tipus_vec %in% c("museu","cultura","art","museum","culture","arte"), "university",
                                      ifelse(tipus_vec %in% c("mirador","panoràmica","viewpoint","panorama"), "binoculars",
                                             ifelse(tipus_vec %in% c("gastronomia","mercat","market","food"), "cutlery",
                                                    ifelse(tipus_vec %in% c("foto","fotografia","viewpoint","photo","photography"), "camera", "star"))))))
    color <- ifelse(tipus_vec %in% c("paisatge","natura","parc","platja","landscape","nature","park","beach"), "green",
                    ifelse(tipus_vec %in% c("monument","historia","història","centre","history","center","centre"), "blue",
                           ifelse(tipus_vec %in% c("museu","cultura","art","museum","culture","arte"), "purple",
                                  ifelse(tipus_vec %in% c("mirador","panoràmica","viewpoint","panorama"), "darkgreen",
                                         ifelse(tipus_vec %in% c("gastronomia","mercat","market","food"), "red", "orange")))))
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
          ifelse(is.na(link) | !nzchar(link), "", paste0("<br/><a href='", htmltools::htmlEscape(link), "' target='_blank'>", tr("see_wikipedia", lang()), "</a>"))
        )
      )
  })
  
  # Routes (OSRM)
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
  
  # Hotels: join + assign to stages
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
      nom <- htmltools::htmlEscape(ah$hotel_name[i] %||% tr("unnamed_hotel", lang()))
      ciu <- htmltools::htmlEscape(city_val[i] %||% "")
      pai <- htmltools::htmlEscape(country_val[i] %||% "")
      sep <- if (nzchar(ciu) && nzchar(pai)) ", " else ""
      lloc <- paste0("<i>", ciu, sep, pai, "</i>")
      lk <- ah$hotel_link[i]
      if (with_link && !is.na(lk) && nzchar(lk)) paste0("<b>", nom, "</b><br>", "<a href='", htmltools::htmlEscape(lk), "' target='_blank'>", tr("hotel_website", lang()), "</a><br>", lloc)
      else paste0("<b>", nom, "</b><br>", lloc)
    }
    ah$popup_html <- vapply(seq_len(nrow(ah)), function(i) mk_popup(i, with_link = TRUE), character(1))
    ah
  })
  
  # Map
  output$mapa <- renderLeaflet({ leaflet() %>% addProviderTiles(providers$OpenStreetMap) })
  redraw_map <- function(do_zoom = FALSE) {
    e_all <- tryCatch(etapas_chosen(), error = function(e) NULL)
    if (is.null(e_all) || !nrow(e_all)) return(invisible())
    l <- lang()
    type_labels <- tr("types", l)
    df_markers <- e_all %>%
      dplyr::transmute(
        description = description,
        wikipedia   = wikipedia,
        ciudad      = ciudad_to,
        tipo = dplyr::case_when(
          modo == "vol"     ~ type_labels[2],
          modo == "cotxe"   ~ type_labels[3],
          modo == "tren"    ~ type_labels[4],
          modo == "vaixell" ~ type_labels[5],
          modo == "visita"  ~ type_labels[6],
          TRUE              ~ type_labels[1]
        ),
        lat = lat_to, lon = lon_to
      ) %>% dplyr::distinct() %>% dplyr::filter(stats::complete.cases(lon, lat))
    v <- dplyr::filter(e_all, modo == "vol",     from_id != to_id) %>% dplyr::filter(stats::complete.cases(lon_from, lat_from, lon_to, lat_to))
    t <- dplyr::filter(e_all, modo == "tren",    from_id != to_id) %>% dplyr::filter(stats::complete.cases(lon_from, lat_from, lon_to, lat_to))
    s <- dplyr::filter(e_all, modo == "vaixell", from_id != to_id) %>% dplyr::filter(stats::complete.cases(lon_from, lat_from, lon_to, lat_to))
    rlist <- tryCatch(rutas_carretera(), error = function(e) list())
    hm    <- tryCatch(hotels_markers(),  error = function(e) (data.frame())[0, ])
    im    <- tryCatch(interes_markers(), error = function(e) (data.frame())[0, ])
    proxy <- leafletProxy("mapa")
    proxy %>% clearGroup("Flights") %>% clearGroup("Trains") %>% clearGroup("Boats") %>%
      clearGroup("Drive") %>% clearGroup("Destinations") %>% clearGroup("Visits") %>%
      clearGroup("Hotels") %>% clearGroup("Interest")
    add_mode_lines <- function(df, color, group) {
      if (!nrow(df)) return(invisible())
      proxy %>% addPolylines(
        lng = unlist(mapply(c, df$lon_from, df$lon_to, SIMPLIFY = FALSE)),
        lat = unlist(mapply(c, df$lat_from, df$lat_to, SIMPLIFY = FALSE)),
        color = color, weight = 3, opacity = 0.7, group = group
      )
    }
    add_mode_lines(v, "blue",   "Flights")
    add_mode_lines(t, "green",  "Trains")
    add_mode_lines(s, "purple", "Boats")
    if (length(rlist)) for (tramo in rlist) if (!is.null(tramo) && inherits(tramo, "sf")) {
      proxy %>% addPolylines(data = tramo, color = "black", weight = 4, opacity = 0.9, dashArray = "5,10", group = "Drive")
    }
    if (nrow(df_markers)) {
      proxy %>% addAwesomeMarkers(
        data = df_markers, lng = ~lon, lat = ~lat, icon = iconos(), label = ~ciudad,
        popup = ~paste0(
          "<b>", htmltools::htmlEscape(description), "</b><br/>",
          ifelse(!is.na(wikipedia) & nzchar(wikipedia),
                 paste0("<a href='", htmltools::htmlEscape(wikipedia), "' target='_blank' rel='noopener noreferrer'>",
                        tr("see_wikipedia", l), "</a>"),
                 "")
        ),
        group = "Destinations"
      )
    }
    vis <- dplyr::filter(e_all, modo == "visita") %>%
      dplyr::transmute(lon = lon_to, lat = lat_to) %>%
      dplyr::filter(stats::complete.cases(lon, lat)) %>% dplyr::distinct()
    if (nrow(vis)) {
      proxy %>% addCircleMarkers(data = vis, lng = ~lon, lat = ~lat, radius = 10, stroke = TRUE, weight = 2,
                                 fillOpacity = 0.25, color = "darkgreen", group = "Visits")
    }
    if (!is.null(hm) && nrow(hm)) {
      proxy %>% addAwesomeMarkers(
        data = hm, lng = ~hotel_lon, lat = ~hotel_lat, icon = hotel_icons(), label = ~hotel_name,
        popup = ~popup_html, group = "Hotels", clusterOptions = markerClusterOptions()
      )
    }
    if (!is.null(im) && nrow(im)) {
      proxy %>% addAwesomeMarkers(
        data = im, lng = ~lon, lat = ~lat, icon = interes_icons(im$tipus), label = ~descriptor,
        popup = ~popup_html, group = "Interest", clusterOptions = markerClusterOptions()
      )
    }
    proxy %>% addLayersControl(
      overlayGroups = c("Flights","Trains","Boats","Drive","Destinations","Visits","Hotels","Interest"),
      options = layersControlOptions(collapsed = TRUE)
    )
    if (isTRUE(do_zoom)) {
      coords <- rbind(data.frame(lon = e_all$lon_from, lat = e_all$lat_from),
                      data.frame(lon = e_all$lon_to,   lat = e_all$lat_to))
      coords <- coords[stats::complete.cases(coords), , drop = FALSE]
      if (!nrow(coords)) {
        proxy %>% setView(lng = 0, lat = 20, zoom = 2)
      } else {
        coords <- unique(coords)
        if (nrow(coords) == 1) {
          proxy %>% setView(lng = coords$lon[1], lat = coords$lat[1], zoom = 12)
        } else {
          lon_min <- min(coords$lon); lon_max <- max(coords$lon)
          lat_min <- min(coords$lat); lat_max <- max(coords$lat)
          eps <- 1e-4
          if ((abs(lon_max - lon_min) < eps) && (abs(lat_max - lat_min) < eps)) {
            proxy %>% setView(lng = mean(c(lon_min, lon_max)), lat = mean(c(lat_min, lat_max)), zoom = 12)
          } else {
            proxy %>% fitBounds(lng1 = lon_min, lat1 = lat_min, lng2 = lon_max, lat2 = lat_max)
          }
        }
      }
    }
  }
  observeEvent(etapas_chosen(), { redraw_map(do_zoom = TRUE) })
  observeEvent(list(show_hotels_eff(), interes_markers()), { redraw_map(do_zoom = FALSE) })
  observeEvent(lang(), {
    rng <- dates_range(); if (is.null(rng)) return()
    sel <- current_day()
    if (is.null(sel) || is.na(sel) || sel < rng$min || sel > rng$max) current_day(rng$min)
    shinyWidgets::updateAirDateInput(session, "dia", value = current_day(),
                                     options = list(minDate = rng$min, maxDate = rng$max))
    redraw_map(FALSE)
  })
  
  # Missing coords notice
  observeEvent(etapas_exp(), {
    td <- current_td(); req(td)
    missing <- td$places |> dplyr::filter(is.na(lat) | is.na(lon)) |> dplyr::pull(ciudad)
    if (length(missing)) {
      showNotification(paste0(tr("warn_missing_coords", lang()), paste(missing, collapse = ", "), "."), type = "warning", duration = 6)
    }
  })
  
  # Titles
  output$stage_details_title <- renderText(tr("stage_detail", lang()))
  output$tab_map_title      <- renderText(tr("map", lang()))
  output$tab_tables_title   <- renderText(tr("save_changes", lang())) # or "Tables"
  output$tab_places_title   <- renderText(tr("places_tab", lang()))
  output$tab_stages_title   <- renderText(tr("stages_tab", lang()))
  output$tab_hotels_title   <- renderText(tr("hotels_tab", lang()))
  output$tab_interest_title <- renderText(tr("interest_tab", lang()))
  output$hotel_title        <- renderText(tr("hotel", lang()))
  output$poi_title          <- renderText(tr("places_interest", lang()))
  
  # ---------- Panels ----------
  output$detalle_etapa <- renderUI({
    l <- lang()
    e <- etapas_chosen()
    if (!nrow(e)) return(HTML(paste0("<i>", tr("no_stage_for_filter", l), "</i>")))
    if (isTRUE(ver_todas_eff())) {
      ciutats <- paste(unique(e$ciudad_to), collapse = " → ")
      HTML(paste0("<h4>", tr("itinerary_full", l), "</h4>",
                  "<b>", tr("stages_label", l), ":</b> ", length(unique(e$etapa)),
                  "<br>", "<b>", tr("dest_cities", l), ":</b> ", ciutats))
    } else {
      desc_vec <- col_first_existing(e, c("description","descripció","descripción"))
      type_labels <- tr("types", l)
      mode_label <- dplyr::case_when(
        e$modo == "vol"    ~ type_labels[2],
        e$modo == "cotxe"  ~ type_labels[3],
        e$modo == "tren"   ~ type_labels[4],
        e$modo == "vaixell"~ type_labels[5],
        e$modo == "visita" ~ type_labels[6],
        TRUE                ~ type_labels[1]
      )
      items <- lapply(seq_len(nrow(e)), function(i){
        tags$div(class = "stage-card",
                 tags$div(class = "stage-head",
                          tags$b(sprintf("%s %s · %s", tr("stage", l), as.character(e$etapa[i]), mode_label[i])),
                          tags$span(style = "float:right;", format(e$dia[i], "%d %b %Y"))
                 ),
                 tags$div(tags$b(paste0(tr("origin", l), ": ")), e$ciudad_from[i], " ", HTML("&rarr; "), tags$b(paste0(tr("destination", l), ": ")), e$ciudad_to[i]),
                 { d <- desc_vec[i]; if (!is.na(d) && nzchar(d)) tags$div(tags$b(paste0(tr("description", l), ": ")), htmltools::htmlEscape(d)) }
        )
      })
      tags$div(tags$h4(sprintf("%s %s", tr("stage_detail", l), format(e$dia[1], "%d %b %Y"))), do.call(tagList, items))
    }
  })
  
  output$hoteles_panel <- renderUI({
    l <- lang()
    ah <- assigned_hotels()
    if (is.null(ah) || !nrow(ah)) {
      miss <- if (isTRUE(ver_todas_eff())) tr("no_hotels_itinerary", l) else tr("no_hotels_day", l)
      return(HTML(paste0("<i>", miss, "</i>")))
    }
    ah <- ah %>% dplyr::arrange(dia, etapa, row_id, ciudad_to, ciudad_from)
    cards <- lapply(split(ah, ah$row_id), function(df){
      src <- unique(df$assign_source)[1]
      cap <- sprintf(tr("route_block_title", l), as.character(df$etapa[1]), df$ciudad_from[1], df$ciudad_to[1])
      dia_txt <- if (!is.null(df$dia[1]) && !is.na(df$dia[1])) format(df$dia[1], "%d %b %Y") else as.character(df$date[1])
      if (src == "none") {
        tags$div(class="stage-card",
                 tags$div(tags$b(cap), tags$span(style="float:right;", dia_txt)),
                 tags$div(HTML(paste0("<i>", tr("transit_no_hotel", l), "</i>")))
        )
      } else {
        ciutat_label <- if (src == "to_id") df$ciudad_to[1] else df$ciudad_from[1]
        items <- lapply(which(!is.na(df$hotel_name)), function(i){
          nm <- df$hotel_name[i] %||% tr("unnamed_hotel", l)
          lk <- df$hotel_link[i]
          if (!is.na(lk) && nzchar(lk)) tags$li(tags$a(href = lk, target = "_blank", nm)) else tags$li(nm)
        })
        if (!length(items)) items <- list(tags$li(HTML(paste0("<i>", tr("hotel_no_link_coords", l), "</i>"))))
        tags$div(class="stage-card",
                 tags$div(tags$b(cap), tags$span(style="float:right;", dia_txt)),
                 tags$div(tags$small(if (src=="to_id") tr("destination", l) else tr("origin", l)), " — ", ciutat_label),
                 tags$ul(items)
        )
      }
    })
    do.call(tagList, cards)
  })
  
  observeEvent(assigned_hotels(), {
    l <- lang()
    ah <- assigned_hotels(); if (is.null(ah) || !nrow(ah)) return()
    trns <- ah %>% dplyr::group_by(row_id) %>% dplyr::summarise(
      transit = all(assign_source == "none"), etapa = dplyr::first(etapa), dia = dplyr::first(dia),
      from = dplyr::first(ciudad_from), to = dplyr::first(ciudad_to), .groups = "drop"
    ) %>% dplyr::filter(transit)
    if (nrow(trns)) {
      # keep short, multilingual core already in cards; optional popup omitted to reduce noise
    }
  }, ignoreInit = FALSE)
  
  output$interes_panel <- renderUI({
    l <- lang()
    td <- current_td(); req(td)
    df <- td$interes
    if (is.null(df) || !nrow(df)) return(tags$div(class = "stage-card", HTML(paste0("<i>", tr("no_interest", l), "</i>"))))
    e <- etapas_chosen(); if (!nrow(e)) return(NULL)
    city <- e$ciudad_to[1]
    df_city <- df %>% dplyr::filter(ciudad == city)
    if (!nrow(df_city)) return(tags$div(class = "stage-card", HTML(paste0("<i>", tr("no_interest_city", l), "</i>"))))
    tags$div(class = "stage-card",
             tags$h5(paste0(tr("places_to_visit_in", l), city)),
             tags$ul(lapply(seq_len(nrow(df_city)), function(i){
               label <- df_city$descriptor[i] %||% "(—)"
               link  <- df_city$link[i]
               if (!is.na(link) && nzchar(link)) tags$li(tags$a(href = link, target = "_blank", label)) else tags$li(label)
             }))
    )
  })
  
  # ---------- Editable tables ----------
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
  
  output$places_table  <- DT::renderDT({ td <- current_td(); req(td); df <- td$places;  if (is.null(df)) df <- data.frame(); DT::datatable(df, selection = if (isTRUE(input$allow_edit_process)) "multiple" else "none"); DT::datatable(df, editable = isTRUE(input$allow_edit_process), options = list(pageLength = 8)) })
  output$stages_table  <- DT::renderDT({ td <- current_td(); req(td); df <- td$stages;  if (is.null(df)) df <- data.frame(); DT::datatable(df, selection = if (isTRUE(input$allow_edit_process)) "multiple" else "none"); DT::datatable(df, editable = isTRUE(input$allow_edit_process), options = list(pageLength = 8)) })
  output$hotels_table  <- DT::renderDT({ td <- current_td(); req(td); df <- td$hotels;  if (is.null(df)) df <- data.frame(); DT::datatable(df, selection = if (isTRUE(input$allow_edit_process)) "multiple" else "none"); DT::datatable(df, editable = isTRUE(input$allow_edit_process), options = list(pageLength = 8)) })
  output$interes_table <- DT::renderDT({ td <- current_td(); req(td); df <- td$interes; if (is.null(df)) df <- data.frame(); DT::datatable(df, selection = if (isTRUE(input$allow_edit_process)) "multiple" else "none"); DT::datatable(df, editable = isTRUE(input$allow_edit_process), options = list(pageLength = 8)) })
  
  # ---------- Write to www/ ----------
  write_trip_to_www <- function(td, base_name, overwrite = FALSE, select_after = TRUE) {
    if ("lat" %in% names(td$places)) td$places$lat <- suppressWarnings(as.numeric(td$places$lat))
    if ("lon" %in% names(td$places)) td$places$lon <- suppressWarnings(as.numeric(td$places$lon))
    if (!is.null(td$hotels) && nrow(td$hotels)) {
      if ("hotel_lat" %in% names(td$hotels)) td$hotels$hotel_lat <- suppressWarnings(as.numeric(td$hotels$hotel_lat))
      if ("hotel_lon" %in% names(td$hotels)) td$hotels$hotel_lon <- suppressWarnings(as.numeric(td$hotels$hotel_lon))
    }
    
    places <- td$places
    if (!"wikipedia" %in% names(places)) places$wikipedia <- NA_character_
    if (!"ciudad" %in% names(places) && "city" %in% names(places)) places$ciudad <- places$city
    if (!"pais"   %in% names(places) && "country" %in% names(places)) places$pais   <- places$country
    places_out <- places %>% dplyr::rename(city = ciudad, country = pais) %>% dplyr::select(place_id, city, country, lat, lon, wikipedia)
    
    st <- td$stages
    desc_vec <- col_first_existing(st, c("description","descripció","descripción"))
    medi_vec <- col_first_existing(st, c("medi","modo"))
    ruta_vec <- col_first_existing(st, c("ruta","route","trayecto","trajeto"))
    for (nm in c("etapa","from_id","to_id","date")) if (!nm %in% names(st)) st[[nm]] <- NA
    stages_out <- st %>% dplyr::mutate(description = desc_vec, medi = medi_vec, ruta = ruta_vec) %>% dplyr::select(etapa, from_id, to_id, date, description, medi, ruta)
    
    hotels_out <- NULL
    if (!is.null(td$hotels) && nrow(td$hotels)) {
      h <- td$hotels
      for (nm in c("hotel_name","hotel_link","details","hotel_lat","hotel_lon")) if (!nm %in% names(h)) h[[nm]] <- if (grepl("_lat$|_lon$", nm)) NA_real_ else NA_character_
      h$hotel_lat <- suppressWarnings(as.numeric(h$hotel_lat))
      h$hotel_lon <- suppressWarnings(as.numeric(h$hotel_lon))
      hotels_out <- h %>% dplyr::select(place_id, hotel_name, hotel_link, details, hotel_lat, hotel_lon)
    }
    
    interes_out <- NULL
    if (!is.null(td$interes) && nrow(td$interes)) {
      inter <- td$interes
      for (nm in c("descriptor","ciudad","pais","lat","lon","tipus","link","observacio")) if (!nm %in% names(inter)) inter[[nm]] <- NA
      inter$lat <- suppressWarnings(as.numeric(inter$lat))
      inter$lon <- suppressWarnings(as.numeric(inter$lon))
      interes_out <- inter %>% dplyr::select(descriptor, ciudad, pais, lat, lon, tipus, link, observacio)
    }
    
    dir.create("www", showWarnings = FALSE, recursive = TRUE)
    base_name <- trimws(base_name %||% "")
    if (!nzchar(base_name)) base_name <- paste0("itinerary_", Sys.Date())
    base_name <- gsub("[^[:alnum:]_\\x2D ]+", "_", base_name)
    fname <- paste0(base_name, ".xlsx")
    save_path <- file.path("www", fname)
    
    if (file.exists(save_path) && !isTRUE(overwrite)) {
      showNotification(tr("warn_exists", lang()), type = "warning", duration = 6)
      return(invisible(FALSE))
    }
    
    ok <- TRUE; err_msg <- NULL
    tryCatch({
      writexl::write_xlsx(Filter(Negate(is.null), list(
        places = places_out, stages = stages_out, hotels = hotels_out, interes = interes_out
      )), path = save_path)
    }, error = function(e) { ok <<- FALSE; err_msg <<- e$message })
    
    if (ok) {
      showNotification(paste0(tr("saved_www", lang()), fname), type = "message", duration = 6)
      files <- list_www_xlsx()
      updateSelectInput(session, "proc_trip_www",
                        choices = c(tr("none", lang()), files),
                        selected = fname)
    } else {
      showNotification(paste0(tr("err_write_www", lang()), err_msg), type = "error", duration = 10)
    }
    invisible(ok)
  }
  
  observeEvent(input$save_places_proc, {
    l <- lang()
    if (!isTRUE(input$allow_edit_process)) { showNotification(tr("warn_edit_to_save", l), type = "warning"); return() }
    td <- current_td(); if (is.null(td)) { showNotification(tr("warn_no_data_to_save", l), type = "warning"); return() }
    trip_data(td) # save in memory
    showNotification(tr("saved_memory", l), type = "message")
    write_trip_to_www(td = td, base_name = input$www_filename_proc, overwrite = isTRUE(input$overwrite_www_proc), select_after = TRUE)
  })
  
  # ---------- Modals: format & info ----------
  observeEvent(input$show_format, {
    l <- lang()
    showModal(modalDialog(
      title = tr("fmt_modal_title", l), size = "l", easyClose = TRUE, footer = modalButton(tr("close", l)),
      HTML( i18n[[l]][["fmt_sheets"]] %||% i18n[["en"]][["fmt_sheets"]] )
    ))
  })
  
  observeEvent(input$show_info, {
    l <- lang()
    showModal(modalDialog(
      title = tr("info_modal_title", l), size = "l", easyClose = TRUE, footer = modalButton(tr("close", l)),
      HTML(paste0(
        "<h4><strong>", tr("ar_title", l), "</strong></h4>",
        "<ul>",
        "<li><strong>", tr("docu", l), ":</strong> UE: passport, return ticket, and funds. Stay < 90 days without visa. Declare perishable food.</li>",
        "<li><strong>", tr("local_time", l), ":</strong> GMT −3.</li>",
        "<li><strong>", tr("language", l), ":</strong> Spanish; English in tourist areas.</li>",
        "<li><strong>", tr("internet", l), ":</strong> Widespread Wi-Fi; local SIM/eSIM available.</li>",
        "<li><strong>", tr("plugs", l), ":</strong> Type C & I.</li>",
        "<li><strong>", tr("currency", l), ":</strong> ARS. Check rates before travel.</li>",
        "</ul>",
        "<h4><strong>", tr("cl_title", l), "</strong></h4>",
        "<ul>",
        "<li><strong>", tr("docu", l), ":</strong> UE: no visa < 90 days. Declare perishable food.</li>",
        "<li><strong>", tr("local_time", l), ":</strong> GMT −4 (continental).</li>",
        "<li><strong>", tr("language", l), ":</strong> Spanish; English in tourist hubs.</li>",
        "<li><strong>", tr("internet", l), ":</strong> Widespread Wi-Fi; local SIM/eSIM available.</li>",
        "<li><strong>", tr("plugs", l), ":</strong> Type C. 220 V.</li>",
        "<li><strong>", tr("currency", l), ":</strong> CLP. Check rates before travel.</li>",
        "</ul>"
      ))
    ))
  })
  
  # When user enables editing, jump to the Tables tab and default to Places subtab
  observeEvent(input$allow_edit_process, {
    if (isTRUE(input$allow_edit_process)) {
      bslib::nav_select("main_tabs", "tables")
      bslib::nav_select("tables_tabs", "places")
    } else {
      bslib::nav_select("main_tabs", "map")
    }
  }, ignoreInit = TRUE)
  
  output$edit_tools <- renderUI({
    if (!isTRUE(input$allow_edit_process)) return(NULL)
    
    # Language helpers (labels)
    lab <- function(key) tr(key, lang())
    
    tagList(
      tags$hr(),
      h4(lab("ed_tools_title")),
      # 1) One-click autocomplete (coords + Wikipedia + hotels + interest)
      actionButton("auto_complete", lab("ed_auto_complete"), class = "btn btn-primary w-100 mb-2"),
      
      # 2) Geocode selected places
      actionButton("geo_fill_sel", lab("ed_geo_fill_sel"), class = "btn btn-secondary w-100 mb-3"),
      
      # 3) Manual edit block
      h5(lab("ed_manual_title")),
      selectInput("place_pick", lab("ed_place_pick"), choices = NULL),
      fluidRow(
        column(6, textInput("lat_manual", lab("ed_lat"), "")),
        column(6, textInput("lon_manual", lab("ed_lon"), ""))
      ),
      div(class="d-grid gap-2",
          actionButton("use_map_click",  lab("ed_use_map_click"),  class = "btn btn-outline-secondary"),
          actionButton("apply_manual_coords", lab("ed_apply_manual"), class = "btn btn-outline-primary")
      ),
      tags$hr(),
      h5(lab("ed_wiki_title")),
      actionButton("wiki_fill_sel", lab("ed_wiki_fill_sel"), class = "btn btn-secondary w-100")
    )
  })
  
  # ---- Wikipedia URL fill (opensearch) ----
  wikipedia_missing <- function(places_df, lang = "es") {
    if (!"wikipedia" %in% names(places_df)) places_df$wikipedia <- NA_character_
    needs_wiki <- is.na(places_df$wikipedia) | places_df$wikipedia == ""
    if (!any(needs_wiki) || !"ciudad" %in% names(places_df)) return(places_df)
    qvec <- places_df$ciudad[needs_wiki]
    fetch_wiki <- function(q) {
      url <- paste0("https://", lang, ".wikipedia.org/w/api.php")
      req <- httr2::request(url) |>
        httr2::req_url_query(action="opensearch", search=q, limit=1, namespace=0, format="json") |>
        httr2::req_user_agent("itinerario-app/1.0 (contact: you@example.com)")
      resp <- try(httr2::req_perform(req), silent=TRUE)
      if (inherits(resp,"try-error")) return(NA_character_)
      js <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent=TRUE)
      if (inherits(js,"try-error")) return(NA_character_)
      if (length(js) >= 4 && length(js[[4]]) >= 1) return(js[[4]][1])
      NA_character_
    }
    places_df$wikipedia[needs_wiki] <- vapply(qvec, fetch_wiki, FUN.VALUE = character(1))
    places_df
  }
  
  # ---- Robust single-query OSM geocode (works with different return shapes) ----
  geocode_one <- function(query) {
    out <- try(geocode_OSM(query, as.data.frame = TRUE), silent = TRUE)
    if (inherits(out, "try-error") || is.null(out)) return(c(NA_real_, NA_real_))
    if (inherits(out, "sf")) {
      coords <- sf::st_coordinates(out)
      return(c(coords[1,"X"], coords[1,"Y"]))
    }
    if (all(c("lon","lat") %in% names(out))) return(c(out$lon[1], out$lat[1]))
    if (all(c("coords.x1","coords.x2") %in% names(out))) return(c(out$coords.x1[1], out$coords.x2[1]))
    if (all(c("x","y") %in% names(out))) return(c(out$x[1], out$y[1]))
    c(NA_real_, NA_real_)
  }
  
  # ---- Bulk-first geocode for places (fallback to single queries) ----
  geocode_missing <- function(df, default_country = NULL) {
    if (!all(c("ciudad","lat","lon") %in% names(df))) return(df)
    if (!"wikipedia" %in% names(df)) df$wikipedia <- NA_character_
    need_idx <- which(is.na(df$lat) | is.na(df$lon))
    if (!length(need_idx)) return(df)
    
    withProgress(message = "Geocodificant llocs…", value = 0, {
      clean_city <- function(x) {
        x <- trimws(as.character(x))
        x <- sub(" -.*$", "", x); x <- sub(":.*$", "", x)
        x <- sub("/.*$", "", x);  x <- sub(",.*$", "", x); x
      }
      ciudad_clean <- vapply(df$ciudad, clean_city, FUN.VALUE = "")
      pais_orig <- if ("pais" %in% names(df)) as.character(df$pais) else rep(NA_character_, nrow(df))
      pais_final <- ifelse(is.na(pais_orig) | pais_orig=="", default_country, pais_orig)
      q_full <- ifelse(!is.na(pais_final) & nzchar(pais_final),
                       paste(ciudad_clean, pais_final, sep = ", "), ciudad_clean)
      
      # bulk via tidygeocoder
      queries <- tibble::tibble(query = q_full[need_idx])
      res_bulk <- try(
        geo(queries, address = query, method = "osm", lat = latitude, long = longitude, limit = 1, verbose = FALSE),
        silent = TRUE
      )
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
        city_by_id    <- setNames(as.character(places_df$ciudad), places_df$place_id)
        country_by_id <- setNames(as.character(places_df$pais),   places_df$place_id)
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
        httr2::req_url_query(action="opensearch", search=q, limit=1, namespace=0, format="json") |>
        httr2::req_user_agent("itinerario-app/1.0 (contact: you@example.com)")
      resp <- try(httr2::req_perform(req), silent=TRUE)
      if (inherits(resp,"try-error")) return(NA_character_)
      js <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent=TRUE)
      if (inherits(js,"try-error")) return(NA_character_)
      if (length(js) >= 4 && length(js[[4]]) >= 1) return(js[[4]][1])
      NA_character_
    }
    qvec <- paste(na.omit(h$descriptor[need]),
                  ifelse(is.na(h$ciudad[need]), "", h$ciudad[need]),
                  ifelse(is.na(h$pais[need]),   "", h$pais[need]))
    h$link[need] <- vapply(qvec, fetch_wiki, FUN.VALUE = character(1))
    h
  }
  
  # Keep a reference to the active dataset setters/getters (your app may already have trip_data() etc.)
  get_td <- reactive(trip_data())
  set_td  <- function(td) trip_data(td)
  
  # Update the city picker whenever data changes (and editing is allowed)
  observe({
    if (!isTRUE(input$allow_edit_process)) return()
    td <- get_td(); req(td)
    updateSelectInput(session, "place_pick", choices = td$places$ciudad)
  })
  
  # Helper to read selected rows from Places DT
  get_selected_places <- function() {
    idx <- input$places_table_rows_selected
    if (is.null(idx) || !length(idx)) return(NULL)
    td <- get_td()
    list(idx = idx, df = td$places[idx, , drop = FALSE])
  }
  
  # Map click → stash last click (for manual coords)
  last_click <- reactiveVal(NULL)
  observeEvent(input$mapa_click, {
    last_click(c(lat = input$mapa_click$lat, lon = input$mapa_click$lng))
  })
  observeEvent(input$use_map_click, {
    req(isTRUE(input$allow_edit_process))
    lc <- last_click(); req(lc)
    updateTextInput(session, "lat_manual", value = as.character(lc["lat"]))
    updateTextInput(session, "lon_manual", value = as.character(lc["lon"]))
  })
  
  # 4.a One-click autocomplete (coords + wiki + hotels + interest)
  observeEvent(input$auto_complete, {
    req(isTRUE(input$allow_edit_process))
    td <- get_td(); req(td)
    
    shinybusy::show_modal_spinner(spin="fading-circle", color="#007BFF",
                                  text = tr("ed_working", lang()))
    on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
    
    withProgress(message = tr("ed_auto_progress", lang()), value = 0, {
      incProgress(0.2, detail = tr("ed_geo_places", lang()))
      places1 <- geocode_missing(td$places, default_country = NULL)
      
      incProgress(0.5, detail = tr("ed_wiki_places", lang()))
      # pick wiki language from UI language, but fallback CA->ES
      wiki_lang <- switch(lang(), "en"="en","es"="es","ca"="es","en")
      places2 <- wikipedia_missing(places1, lang = wiki_lang)
      
      hotels_out <- td$hotels
      if (!is.null(hotels_out) && nrow(hotels_out)) {
        incProgress(0.75, detail = tr("ed_geo_hotels", lang()))
        for (nm in c("hotel_lat","hotel_lon","hotel_link"))
          if (!nm %in% names(hotels_out)) hotels_out[[nm]] <- if (nm %in% c("hotel_lat","hotel_lon")) NA_real_ else NA_character_
        hotels_out <- fill_missing_hotel_coords(hotels_out, places2)
      }
      
      inter_out <- td$interes
      if (!is.null(inter_out) && nrow(inter_out)) {
        incProgress(0.9, detail = tr("ed_geo_interest", lang()))
        inter_out <- geocode_interes_missing(inter_out)
        incProgress(0.95, detail = tr("ed_wiki_interest", lang()))
        inter_out <- interes_link_missing(inter_out, lang = wiki_lang)
      }
      
      set_td(modifyList(td, list(places = places2, hotels = hotels_out, interes = inter_out)))
      incProgress(1, detail = tr("done", lang()))
    })
    
    # Refresh Places & Hotels & Interest tables if they’re visible
    isolate({
      td2 <- get_td()
      DT::replaceData(DT::dataTableProxy("places_table"),  td2$places, resetPaging = FALSE)
      if (!is.null(td2$hotels))
        DT::replaceData(DT::dataTableProxy("hotels_table"), td2$hotels, resetPaging = FALSE)
      if (!is.null(td2$interes))
        DT::replaceData(DT::dataTableProxy("interes_table"), td2$interes, resetPaging = FALSE)
    })
    
    # Repaint map layers (you already have redraw_map(); otherwise call leafletProxy logic)
    redraw_map(do_zoom = FALSE)
    
    n_geo_after  <- sum(is.na(get_td()$places$lat) | is.na(get_td()$places$lon))
    n_wiki_after <- sum(is.na(get_td()$places$wikipedia) | get_td()$places$wikipedia == "")
    showNotification(tr("ed_done", lang(), list(geo = n_geo_after, wiki = n_wiki_after)), type = "message", duration = 6)
  })
  
  # 4.b Geocode only selected places
  observeEvent(input$geo_fill_sel, {
    req(isTRUE(input$allow_edit_process))
    sel <- get_selected_places(); req(sel)
    td <- get_td()
    upd <- geocode_missing(sel$df, default_country = NULL)
    td$places[sel$idx, ] <- upd
    set_td(td)
    DT::replaceData(DT::dataTableProxy("places_table"), td$places, resetPaging = FALSE)
    redraw_map(FALSE)
    showNotification(tr("ed_geo_sel_done", lang()), type = "message")
  })
  
  # 4.c Fill Wikipedia for selected places
  observeEvent(input$wiki_fill_sel, {
    req(isTRUE(input$allow_edit_process))
    sel <- get_selected_places(); req(sel)
    td <- get_td()
    wiki_lang <- switch(lang(), "en"="en","es"="es","ca"="es","en")
    upd <- wikipedia_missing(sel$df, lang = wiki_lang)
    td$places[sel$idx, ] <- upd
    set_td(td)
    DT::replaceData(DT::dataTableProxy("places_table"), td$places, resetPaging = FALSE)
    showNotification(tr("ed_wiki_sel_done", lang()), type = "message")
  })
  
  # 4.d Apply manual lat/lon to selected city from the dropdown
  observeEvent(input$apply_manual_coords, {
    req(isTRUE(input$allow_edit_process))
    td <- get_td(); req(td, input$place_pick, nzchar(input$lat_manual), nzchar(input$lon_manual))
    i <- which(td$places$ciudad == input$place_pick); req(length(i) == 1)
    td$places$lat[i] <- suppressWarnings(as.numeric(input$lat_manual))
    td$places$lon[i] <- suppressWarnings(as.numeric(input$lon_manual))
    set_td(td)
    DT::replaceData(DT::dataTableProxy("places_table"), td$places, resetPaging = FALSE)
    redraw_map(FALSE)
    showNotification(tr("ed_manual_done", lang()), type = "message")
  })
  
}

shinyApp(ui, server)

