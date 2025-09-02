# app.R — Geolocator (SIMPLE VERSION: ONLY "process" MODE)
# Multilingual EN/CA/ES with live switching
# Editing and geolocalization adjusted

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
library(tmaptools)   # geocode_OSM
library(bslib)
library(waiter)
library(shinybusy)
library(osmdata)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# =====================
# i18n dictionary + helpers
# =====================
i18n <- list(
  en = list(
    title = "Travel Itinerary Geolocator",
    btn_format = "Input format",
    btn_info = "Information",
    from_disk = "Load travel file (.xlsx)",
    from_www  = "Load example file",
    refresh   = "Select example file",
    load_sel  = "Load selected file",
    it_params = "Itinerary parameters",
    show_all  = "Show all stages",
    show_hotels_map = "Show hotels on the map",
    allow_edit = "Allow edit",
    return_to_visor = "Return to travel visor",
    stage_detail = "Stage details",
    filter_by = "Filter by type:",
    types = c("Itinerary","Flights","Car","Train","Boat","Visit"),
    save_changes = "Files to edit",
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
    warn_no_info = "No data available on country visited",
    fmt_sheets = "<b>Excel document with 5 sheets:</b><br>
<ul>
<li><b>Places</b>: <i>places</i> (mandatory)
<li><b>Stages</b>: <i>stages</i> (mandatory)
<li><b>Hotels</b>: <i>hotels</i> (optional)
<li><b>Interest</b>: <i>interest</i> (optional)
<li><b>Info</b>: <i>info</i> (optional)
</ul>
<b>Column names</b><br><br>
<b>Places</b>: place_id | city | country | lat | lon | wikipedia
<br><b>Stages</b>: stage | from_id | to_id | date | description | transport | comments
<br><b>Hotels</b>: place_id | hotel_name | hotel_link | details | hotel_lat | hotel_lon
<br><b>Interest</b>: descriptor | lat | lon | city | country | details | type | link
<br><b>info</b>: country | documentation | local_time | language | internet | plugs | currency
<br><br><b>Values for <i>transport</i></b>: flight/plane, car, train, boat/ferry.
<br><b>Values for <i>type</i></b>: art/culture/photo/photography/gastronomy/history/market/viewpoint
    <br>/monument/museum/nature/landscape/panorama/park.",
    info_modal_title = "Relevant information about countries to visit",
  #  ar_title = "ARGENTINA",
  #  cl_title = "CHILE",
    docu = "DOCUMENTATION",
    local_time = "LOCAL TIME",
    language = "LANGUAGE",
    internet = "INTERNET",
    plugs = "PLUGS",
    currency = "CURRENCY",
    none = "(none)",
    download_excel = "Download Excel",
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
    from_disk = "Cargar archivo del viaje (.xlsx)",
    from_www  = "Cargar archivo de ejemplo",
    refresh   = "Selecciona archivo de ejemplo",
    load_sel  = "Cargar archivo seleccionado",
    it_params = "Parámetros del itinerario",
    show_all  = "Mostrar todas las etapas",
    show_hotels_map = "Mostrar hoteles en el mapa",
    allow_edit = "Permitir edición",
    return_to_visor = "Volver al visor de viaje",
    stage_detail = "Detalles de la etapa",
    filter_by = "Filtrar por tipo:",
    types = c("Itinerario","Vuelos","Coche","Tren","Barco","Visita"),
    save_changes = "Archivos para editar",
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
    warn_no_info = "No hay datos disponibles sobre los países visitados",
    fmt_sheets = "<b>Documento Excel con 5 hojas:</b><br>
<ul>
<li><b>Places</b>: <i>lugares</i> (obligatoria)
<li><b>Stages</b>: <i>etapas</i> (obligatoria)
<li><b>Hotels</b>: <i>hoteles</i> (opcional)
<li><b>Interest</b>: <i>interes</i> (opcional)
<li><b>Info</b>: <i>info</i> (opcional)
</ul>
<b>Nombres de columnas</b><br><br>
<b>Places</b>: place_id | ciudad | pais | lat | lon | wikipedia
<br><b>Stages</b>: etapa | de_id | a_id | fecha | descripción | transporte | comentarios
<br><b>Hotels</b>: lugar_id | hotel_nombre | hotel_enlace | detalles | hotel_lat | hotel_lon
<br><b>Interest</b>: descriptor | lat | lon | ciudad | pais | observación | tipo | enlace
<br><b>info</b>: país | documentación | hora_local | idiomas | internet | conectores | moneda
<br><br><b>Valores de <i>transporte</i></b>: vuelo/avión, coche/carro, tren, barco/ferry.
<br><b>Valores de <i>tipo</i></b>: arte/cultura/foto/fotografía/gastronomía/historia/mercado/mirador
    <br>/monumento/museo/naturaleza/paisaje/panorama/parque.",
    info_modal_title = "Información relevante de los países a visitar",
  #  ar_title = "ARGENTINA",
  #  cl_title = "CHILE",
    docu = "DOCUMENTACIÓN",
    local_time = "HORA LOCAL",
    language = "IDIOMA",
    internet = "INTERNET",
    plugs = "ENCHUFES",
    currency = "MONEDA",
    none = "(ninguno)",
    download_excel = "Descargar Excel",
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
    from_disk = "Carregar arxiu del viatge (.xlsx)",
    from_www  = "Carregar arxiu d'exemple",
    refresh   = "Selecciona arxiu d'exemple",
    load_sel  = "Carrega l'arxiu seleccionat",
    it_params = "Paràmetres de l'itinerari",
    show_all  = "Mostra totes les etapes",
    show_hotels_map = "Mostra els hotels al mapa",
    allow_edit = "Permetre edició",
    return_to_visor = "Tornar al visor de viatge",
    stage_detail = "Detalls de l'etapa",
    filter_by = "Filtra per tipus:",
    types = c("Itinerari","Vols","Cotxe","Tren","Vaixell","Visita"),
    save_changes = "Arxius per editar",
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
    warn_no_info = "No hi ha dades disponibles sobre els països visitats",
    fmt_sheets = "<b>Document Excel amb 5 fulls:</b><br>
<ul>
<li><b>Places</b>: <i>llocs</i> (obligatoria)
<li><b>Stages</b>: <i>etapes</i> (obligatoria)
<li><b>Hotels</b>: <i>hotels</i> (opcional)
<li><b>Interest</b>: <i>interes</i> (opcional)
<li><b>Info</b>: <i>info</i> (opcional)
</ul>
<b>Noms de les columnes</b><br><br>
<b>Places</b>: place_id | ciutat | païs | lat | lon | wikipedia
<br><b>Stages</b>: etapa | de_id | a_id | data | descripció | transport | comentaris
<br><b>Hotels</b>: lloc_id | hotel_nom | hotel_enllaç | detalls | hotel_lat | hotel_lon
<br><b>Interest</b>: descriptor | lat | lon | ciutat | païs | observació | tipus | enllaç
<br><b>info</b>: païs | documentació | hora_local | idiomes | internet | conectors | moneda
<br><br><b>Valors de <i>transport</i></b>: vol/avió, cotxe, tren, vaixell/ferry.
<br><b>Valors de <i>tipus</i></b>: art/cultura/foto/fotografia/gastronomia/història/mercat/mirador
    <br>/monument/museu/natura/paisatge/panoràmica/parc.",
    info_modal_title = "Informació rellevant dels països a visitar",
   # ar_title = "ARGENTINA",
   # cl_title = "XILE",
    docu = "DOCUMENTACIÓ",
    local_time = "HORA LOCAL",
    language = "IDIOMA",
    internet = "INTERNET",
    plugs = "ENDOLLS",
    currency = "MONEDA",
    none = "(cap)",
    download_excel = "Descarregar Excel",
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
  .sidebar, .bslib-sidebar, .sidebar .form-group, .sidebar label, .sidebar input, .sidebar select {
    font-size: 1rem;
  }
  .sidebar h4, .bslib-sidebar h4 { font-size: 1.2rem; font-weight: 600; }
  .sidebar h5 { font-size: 1.1rem; font-weight: 500; }
  .sidebar .btn { font-size: 1rem; padding: 4px 8px; }
")),
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = bslib::font_google("Inter"),
    "font-size-root" = "0.8125rem"  # ~13px
  ),
  title = tr("title", .lang_default),
  
  sidebar = bslib::sidebar(
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
          bslib::nav_panel(title = textOutput("tab_interest_title"), value = "interest", DTOutput("interes_table")),
          bslib::nav_panel(title = textOutput("tab_info_title"),     value = "info",     DTOutput("info_table"))  # <- NOVA
        )
      ),
      conditionalPanel(
        "!input.allow_edit_process",
        div(class = "p-3", em(textOutput("tip_edit_text")))
      )
    )
  ),
  
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
  
  # ---- Timeout configuration ----
  options(timeout = 10)  # base R
  options(osmdata_timeout = 6)  # internal HTTP request timeout for osmdata

  # ---- Set fast Overpass mirror (optional) ----
  try(osmdata::set_overpass_url("https://overpass.kumi.systems/api/interpreter"), silent = TRUE)
  
  # ---- Define empty trip data structure ----
  empty_td <- function() {
    list(
      places = tibble::tibble(
        place_id = character(), ciudad = character(), pais = character(),
        lat = numeric(), lon = numeric(), wikipedia = character()
      ),
      stages = tibble::tibble(
        etapa = character(), from_id = character(), to_id = character(),
        date = as.character(character()), description = character(),
        medi = character(), ruta = character(), modo = character()
      ),
      hotels = tibble::tibble(
        place_id = character(), hotel_name = character(), hotel_link = character(),
        details = character(), hotel_lat = numeric(), hotel_lon = numeric(),
        ciudad = character(), pais = character()
      ),
      interes = tibble::tibble(
        descriptor = character(), lat = numeric(), lon = numeric(),
        ciudad = character(), pais = character(),
        observacio = character(), tipus = character(), link = character()
      ),
      info = tibble::tibble(      # formato interno “tidy” que ya consume tu modal
        country = character(), field = character(),
        en = character(), es = character(), ca = character()
      )
    )
  }
  
  # ---- Reactive trip data ----
  trip_data <- reactiveVal(empty_td())
  current_td <- reactive(trip_data())
  
  # Convert INFO table to wide format with automatic language priority (en > es > ca)
  info_to_wide <- function(df) {
    if (is.null(df) || !nrow(df)) return(NULL)
    nms <- tolower(trimws(names(df)))
    names(df) <- nms
    
    # If tidy format: country | field | en/es/ca
    if (all(c("country","field") %in% nms) && any(c("en","es","ca") %in% nms)) {
      value <- dplyr::coalesce(df$en, df$es, df$ca)  # preference: en → es → ca
      tmp <- tibble::tibble(
        country = df$country,
        field   = toupper(trimws(as.character(df$field))),
        value   = value
      )
      wide <- tidyr::pivot_wider(tmp, names_from = field, values_from = value)
    } else {
      # Already wide format: harmonize column names
      names(df)[grepl("^documen", names(df))]   <- "documentation"
      names(df)[grepl("^local[_ ]?tim", names(df))] <- "local_time"
      names(df)[grepl("^language", names(df))]  <- "language"
      names(df)[grepl("^internet", names(df))]  <- "internet"
      names(df)[grepl("^plugs", names(df))]     <- "plugs"
      names(df)[grepl("^currency", names(df))]  <- "currency"
      wide <- df
    }
    
    # Final column order and headers
    need <- c("country","DOCUMENTATION","LOCAL_TIME","LANGUAGE","INTERNET","PLUGS","CURRENCY")
    names(wide) <- ifelse(tolower(names(wide)) == "country", "country", toupper(names(wide)))
    for (nm in need) if (!nm %in% names(wide)) wide[[nm]] <- NA_character_
    wide[, need]
  }
  
  info_to_wide_lang <- function(df, lcode = "en") {
    # Returns the INFO table in wide format using the language column specified by lcode
    if (is.null(df) || !nrow(df)) {
      return(data.frame(
        country = character(),
        DOCUMENTATION = character(), LOCAL_TIME = character(), LANGUAGE = character(),
        INTERNET = character(), PLUGS = character(), CURRENCY = character()
      ))
    }
    nms <- tolower(trimws(names(df)))
    if (all(c("country","field") %in% nms) && any(c("en","es","ca") %in% nms)) {
      col <- if (lcode %in% c("en","es","ca")) lcode else "en"
      tmp <- tibble::tibble(
        country = df$country,
        field   = toupper(trimws(as.character(df$field))),
        value   = as.character(df[[col]])
      )
      wide <- tidyr::pivot_wider(tmp, names_from = field, values_from = value)
    } else {
      wide <- df
    }
    names(wide) <- ifelse(tolower(names(wide)) == "country", "country", toupper(names(wide)))
    need <- c("country","DOCUMENTATION","LOCAL_TIME","LANGUAGE","INTERNET","PLUGS","CURRENCY")
    for (nm in need) if (!nm %in% names(wide)) wide[[nm]] <- NA_character_
    wide[, need]
  }
  
  
  build_trip_xlsx <- function(td, lang_code = "en") {
    # --- places ---
    places <- td$places
    if (!"wikipedia" %in% names(places)) places$wikipedia <- NA_character_
    if (!"ciudad" %in% names(places) && "city" %in% names(places)) places$ciudad <- places$city
    if (!"pais"   %in% names(places) && "country" %in% names(places)) places$pais   <- places$country
    places$lat <- suppressWarnings(as.numeric(places$lat))
    places$lon <- suppressWarnings(as.numeric(places$lon))
    places_out <- places |>
      dplyr::rename(city = ciudad, country = pais) |>
      dplyr::select(place_id, city, country, lat, lon, wikipedia)
    
    # --- stages ---
    st <- td$stages
    desc_vec <- col_first_existing(st, c("description","descripció","descripción"))
    medi_vec <- col_first_existing(st, c("medi","modo"))
    ruta_vec <- col_first_existing(st, c("ruta","route","trayecto","trajeto","route_desc","ruta_text"))
    for (nm in c("etapa","from_id","to_id","date")) if (!nm %in% names(st)) st[[nm]] <- NA
    stages_out <- st |>
      dplyr::mutate(description = desc_vec, medi = medi_vec, ruta = ruta_vec) |>
      dplyr::select(etapa, from_id, to_id, date, description, medi, ruta)
    
    # --- hotels ---
    hotels_out <- NULL
    if (!is.null(td$hotels) && nrow(td$hotels)) {
      h <- td$hotels
      for (nm in c("hotel_name","hotel_link","details","hotel_lat","hotel_lon"))
        if (!nm %in% names(h)) h[[nm]] <- if (grepl("_lat$|_lon$", nm)) NA_real_ else NA_character_
      h$hotel_lat <- suppressWarnings(as.numeric(h$hotel_lat))
      h$hotel_lon <- suppressWarnings(as.numeric(h$hotel_lon))
      hotels_out <- h |> dplyr::select(place_id, hotel_name, hotel_link, details, hotel_lat, hotel_lon)
    }
    
    # --- interest ---
    interes_out <- NULL
    if (!is.null(td$interes) && nrow(td$interes)) {
      inter <- td$interes
      for (nm in c("descriptor","ciudad","pais","lat","lon","tipus","link","observacio"))
        if (!nm %in% names(inter)) inter[[nm]] <- NA
      inter$lat <- suppressWarnings(as.numeric(inter$lat))
      inter$lon <- suppressWarnings(as.numeric(inter$lon))
      interes_out <- inter |> dplyr::select(descriptor, ciudad, pais, lat, lon, tipus, link, observacio)
    }
    
    # --- info (force wide) ---
    info_out <- NULL
    if (!is.null(td$info) && nrow(td$info)) info_out <- info_to_wide(td$info)
    
    Filter(Negate(is.null), list(
      places  = places_out,
      stages  = stages_out,
      hotels  = hotels_out,
      interes = interes_out,
      info    = info_out   # wide
    ))
  }
  

  # === download handler ===
output$download_itinerary_proc <- downloadHandler(
  filename = function() {
    base <- input$www_filename_proc %||% "itinerary_view"
    base <- gsub("[^[:alnum:]_\\x2D ]+", "_", trimws(base))
    paste0(base, ".xlsx")
  },
  content = function(file) {
    td <- current_td(); if (is.null(td)) stop("No data loaded")
    # construye payload y BLINDA info en ancho
    payload <- build_trip_xlsx(td)
    if (!is.null(td$info) && nrow(td$info)) payload$info <- info_to_wide(td$info)
    writexl::write_xlsx(payload, path = file)
  }
)
  
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
  
  
  # Excel read + normalization
  SHEET_ALIASES <- list(
    places  = c("places","lugares","llocs"),
    stages  = c("stages","etapas","etapes"),
    hotels  = c("hotels","hoteles","hotels"),
    interest= c("interest","interes","interès","interesos"),
    info    = c("info")
  )
  
  # Maps of SYNONYMS -> INTERNAL CANONICAL NAME
  # Internally I keep these names:
  # places:  place_id, ciudad, pais, lat, lon, wikipedia
  # stages:  etapa, from_id, to_id, date, description, medi, ruta
  # hotels:  place_id, hotel_name, hotel_link, details, hotel_lat, hotel_lon
  # interest: descriptor, lat, lon, ciudad, pais, observacio, tipus, link
  COL_ALIASES <- list(
    places = list(
      place_id = c("place_id","lloc_id","lugar_id"),
      ciudad   = c("ciudad","city","ciutat"),
      pais     = c("pais","país","country"),
      lat      = c("lat","latitude"),
      lon      = c("lon","long","lng","longitude"),
      wikipedia= c("wikipedia","wiki")
    ),
    stages = list(
      etapa       = c("etapa","stage"),
      from_id     = c("from_id","de_id"),
      to_id       = c("to_id","a_id"),
      date        = c("date","fecha","data"),
      description = c("description","descripción","descripció","descripcion"),
      medi        = c("medi","medio","mode","modo","transport"),  # <- transport -> medi
      ruta        = c("ruta","route","trayecto","trajeto","route_desc","ruta_text","comments","comentarios","comentaris")  # <- comments -> ruta
    ),
    hotels = list(
      place_id   = c("place_id","lloc_id","lugar_id"),
      hotel_name = c("hotel_name","hotel_nom","hotel_nombre","name"),
      hotel_link = c("hotel_link","hotel_enlace","hotel_enllaç","link","url"),
      details    = c("details","detalles","detalls","notes","notas"),
      hotel_lat  = c("hotel_lat","lat","latitude"),
      hotel_lon  = c("hotel_lon","lon","long","lng","longitude")
    ),
    interest = list(
      descriptor = c("descriptor","name","title","títol","titulo"),
      lat        = c("lat","latitude"),
      lon        = c("lon","long","lng","longitude"),
      ciudad     = c("ciudad","city","ciutat"),
      pais       = c("pais","país","country"),
      observacio = c("observacio","observación","observacion","observació","details","detail","note","notes","observación/notes"),
      tipus      = c("tipus","tipo","type"),
      link       = c("link","enlace","enllaç","url")
    ),
    info = list(
      country       = c("country","pais","país"),
      documentation = c("documentation","documentacion","documentación","documentacio","documentació"),
      local_time    = c("local_time","local time","hora_local","hora local"),
      language      = c("language","idioma","llengua"),
      internet      = c("internet"),
      plugs         = c("plugs","enchufes","endolls"),
      currency      = c("currency","moneda")
    )
  )
  
  # Find the first existing column name and rename it to the canonical one
  rename_cols_canonical <- function(df, alias_map) {
    if (is.null(df) || !ncol(df)) return(df)
    nms <- tolower(trimws(names(df)))
    names(df) <- nms
    for (canon in names(alias_map)) {
      alts <- tolower(alias_map[[canon]])
      hit  <- which(nms %in% alts)
      if (length(hit)) {
        names(df)[hit[1]] <- canon
        nms[hit[1]] <- canon
      }
    }
    df
  }
  
  # Find a sheet by its alias
  find_sheet_by_alias <- function(all_sheets, aliases) {
    s_low <- tolower(all_sheets)
    for (al in aliases) {
      idx <- which(s_low == al)
      if (length(idx)) return(all_sheets[idx[1]])
    }
    NA_character_
  }
  
  read_trip_data <- function(path) {
    all <- readxl::excel_sheets(path)
    
    sh_places  <- find_sheet_by_alias(all, SHEET_ALIASES$places)
    sh_stages  <- find_sheet_by_alias(all, SHEET_ALIASES$stages)
    sh_hotels  <- find_sheet_by_alias(all, SHEET_ALIASES$hotels)
    sh_interest<- find_sheet_by_alias(all, SHEET_ALIASES$interest)
    sh_info    <- find_sheet_by_alias(all, SHEET_ALIASES$info)
    
    if (is.na(sh_places) || is.na(sh_stages)) {
      stop("Excel must include mandatory sheets: Places & Stages (any of: EN/ES/CA names).")
    }
    
    # ---- PLACES ----
    places <- readxl::read_excel(path, sheet = sh_places)
    places <- rename_cols_canonical(places, COL_ALIASES$places)
    # Complete required/frequent columns
    for (nm in c("place_id","ciudad","pais","lat","lon","wikipedia")) {
      if (!nm %in% names(places)) places[[nm]] <- if (nm %in% c("lat","lon")) NA_real_ else NA_character_
    }
    places$lat <- suppressWarnings(as.numeric(places$lat))
    places$lon <- suppressWarnings(as.numeric(places$lon))
    
    # ---- STAGES ----
    stages <- readxl::read_excel(path, sheet = sh_stages)
    stages <- rename_cols_canonical(stages, COL_ALIASES$stages)
    for (nm in c("etapa","from_id","to_id","date","description","medi","ruta")) {
      if (!nm %in% names(stages)) stages[[nm]] <- NA
    }
    
    if (!is.data.frame(stages)) {
      if (inherits(stages, "Date") || is.atomic(stages)) {
        stages <- tibble::tibble(date = stages)
      } else {
        stages <- tibble::as_tibble(stages)
      }
    } else {
      stages <- tibble::as_tibble(stages)
    }
    names(stages) <- make.unique(names(stages), sep = "__dup")
    for (nm in c("etapa","from_id","to_id","date","description","medi","ruta","modo"))
      if (!nm %in% names(stages)) stages[[nm]] <- NA
    # Resolve medium/mode without duplicates:
    if ("medi" %in% names(stages) && "modo" %in% names(stages)) {
      stages$modo <- dplyr::coalesce(as.character(stages$modo), as.character(stages$medi))
      stages$medi <- NULL
    } else if ("medi" %in% names(stages)) {
      names(stages)[names(stages) == "medi"] <- "modo"
    }
    
    
    # ---- HOTELS (optional) ----
    hotels <- NULL
    if (!is.na(sh_hotels)) {
      hotels <- readxl::read_excel(path, sheet = sh_hotels)
      hotels <- rename_cols_canonical(hotels, COL_ALIASES$hotels)
      for (nm in c("place_id","hotel_name","hotel_link","details","hotel_lat","hotel_lon")) {
        if (!nm %in% names(hotels)) hotels[[nm]] <- if (nm %in% c("hotel_lat","hotel_lon")) NA_real_ else NA_character_
      }
      hotels$hotel_lat <- suppressWarnings(as.numeric(hotels$hotel_lat))
      hotels$hotel_lon <- suppressWarnings(as.numeric(hotels$hotel_lon))
      # Add city/country via places (useful for popups)
      hotels <- dplyr::left_join(hotels, places[, c("place_id","ciudad","pais")], by = "place_id")
    }
    
    # ---- INTEREST (optional) ----
    interes <- NULL
    if (!is.na(sh_interest)) {
      interes <- readxl::read_excel(path, sheet = sh_interest)
      interes <- rename_cols_canonical(interes, COL_ALIASES$interest)
      for (nm in c("descriptor","lat","lon","ciudad","pais","observacio","tipus","link")) {
        if (!nm %in% names(interes)) interes[[nm]] <- NA
      }
      interes$lat <- suppressWarnings(as.numeric(interes$lat))
      interes$lon <- suppressWarnings(as.numeric(interes$lon))
    }
    
    info_df <- NULL
    if (!is.na(sh_info)) {
      raw <- readxl::read_excel(path, sheet = sh_info)
      names(raw) <- tolower(trimws(names(raw)))
      
      has_wide <- all(c("documentation","local_time","language","internet","plugs","currency") %in% names(raw))
      has_tidy <- all(c("country","field") %in% names(raw)) && any(c("en","es","ca") %in% names(raw))
      
      if (has_tidy) {
        # already in tidy -> normalize columns
        for (lng in c("en","es","ca")) if (!lng %in% names(raw)) raw[[lng]] <- NA_character_
        raw$field <- toupper(trimws(as.character(raw$field)))
        info_df <- raw[, c("country","field","en","es","ca")]
      } else if (has_wide) {
        # width -> a tidy (memory)
        tmp <- tidyr::pivot_longer(
          raw,
          cols = c(documentation, local_time, language, internet, plugs, currency),
          names_to = "field", values_to = "en"
        )
        tmp$field   <- toupper(tmp$field)
        tmp$es <- tmp$en; tmp$ca <- tmp$en  # fallback
        info_df <- tmp[, c("country","field","en","es","ca")]
      }
    }
    return(list(
      places  = places,
      stages  = stages,
      hotels  = hotels,
      interes = interes,
      info    = info_df
    ))
    
  }
  
  warn_if_no_info <- function(td) {
    if (is.null(td$info) || !nrow(td$info)) {
      showNotification(tr("warn_no_info", lang()), type = "warning", duration = 6)
    }
  }
  
  
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
        # REMOVE: checkboxInput("overwrite_www_proc", ...),
        # REMOVE: actionButton("save_places_proc", ...),
        downloadButton("download_itinerary_proc", tr("download_excel", l), class = "btn btn-warning w-100"),
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
        # h5(tr("from_www", l)),
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
        # REMOVE: checkboxInput("overwrite_www_proc", ...),
        # REMOVE: actionButton("save_places_proc", ...),
        downloadButton("download_itinerary_proc", tr("download_excel", l), class = "btn btn-warning w-100"),
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
  
  output$tip_edit_text <- renderText(tr("tip_edit", lang()))
  
  # Load: local and www
  observeEvent(input$proc_trip_file, {
    req(input$proc_trip_file$datapath)
    td <- read_trip_data(input$proc_trip_file$datapath)
    trip_data(td)
    showNotification(tr("notice_loaded", lang()), type = "message")
    warn_if_no_info(td)
    if (is.null(td$info) || !nrow(td$info)) {
      showNotification(tr("warn_no_info", lang()), type = "warning")
    }
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
        warn_if_no_info(td)
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
    warn_if_no_info(td)
    if (is.null(td$info) || !nrow(td$info)) {
      showNotification(tr("warn_no_info", lang()), type = "warning")
    }
  })
  
  # --- Wikipedia + Geocode helpers ---
  wikipedia_missing <- function(places_df, lang = "es") {
    if (!"wikipedia" %in% names(places_df)) places_df$wikipedia <- NA_character_
    needs_wiki <- is.na(places_df$wikipedia) | places_df$wikipedia == ""
    if (!any(needs_wiki) || !"ciudad" %in% names(places_df)) return(places_df)
    
    # Best query: city + country if available (disambiguate)
    qvec <- if ("pais" %in% names(places_df)) {
      paste(places_df$ciudad[needs_wiki], places_df$pais[needs_wiki], sep = ", ")
    } else {
      places_df$ciudad[needs_wiki]
    }
    
    places_df$wikipedia[needs_wiki] <- vapply(
      qvec,
      function(q) fetch_wiki(q, lang = lang),
      FUN.VALUE = character(1)
    )
    places_df
  }
  
  
  # --- NEW: geocoder with strict timeout (Nominative) ---
  geo_nominatim <- function(query, tmo = 4) {
    if (is.null(query) || !nzchar(query)) return(c(NA_real_, NA_real_))
    
    req <- httr2::request("https://nominatim.openstreetmap.org/search") |>
      httr2::req_url_query(q = query, format = "jsonv2", limit = 1, addressdetails = 0) |>
      httr2::req_user_agent("geoitinr/1.0 (contact: you@example.com)") |>
      httr2::req_timeout(tmo) |>
      httr2::req_retry(max_tries = 2)
    
    resp <- try(httr2::req_perform(req), silent = TRUE)
    if (inherits(resp, "try-error")) return(c(NA_real_, NA_real_))
    
    # IMPORTANT: don't simplify -> stable list of lists
    js <- try(httr2::resp_body_json(resp, simplifyVector = FALSE), silent = TRUE)
    if (inherits(js, "try-error") || is.null(js) || length(js) < 1) return(c(NA_real_, NA_real_))
    
    # Typical case: list of objects (each object is a list with "lon"/"lat")
    rec <- js[[1]]
    
    # 1) If it came as a list: access with [[ ]]
    if (is.list(rec)) {
      lon_chr <- rec[["lon"]]; lat_chr <- rec[["lat"]]
      lon <- suppressWarnings(as.numeric(if (length(lon_chr)) lon_chr[[1]] else NA))
      lat <- suppressWarnings(as.numeric(if (length(lat_chr)) lat_chr[[1]] else NA))
      return(c(lon %||% NA_real_, lat %||% NA_real_))
    }
    
    # 2) Fallback if httr2 returned a data.frame despite the simplifyVector = FALSE
    if (is.data.frame(js) && nrow(js) >= 1 && all(c("lon","lat") %in% names(js))) {
      lon <- suppressWarnings(as.numeric(js$lon[1])); 
      lat <- suppressWarnings(as.numeric(js$lat[1]))
      return(c(lon %||% NA_real_, lat %||% NA_real_))
    }
    
    # 3) Last attempt: if rec was an atomic vector with numbers
    if (is.atomic(rec) && !is.null(names(rec)) && all(c("lon","lat") %in% names(rec))) {
      lon <- suppressWarnings(as.numeric(rec[["lon"]]))
      lat <- suppressWarnings(as.numeric(rec[["lat"]]))
      return(c(lon %||% NA_real_, lat %||% NA_real_))
    }
    
    c(NA_real_, NA_real_)
  }
  
  
  geocode_one <- function(query) {
    # first: Nominatim with timeout
    coords <- geo_nominatim(query, tmo = 4)
    if (!is.na(coords[1]) && !is.na(coords[2])) return(coords)
    # very short fallback (just in case)
    out <- try(tmaptools::geocode_OSM(query, as.data.frame = TRUE), silent = TRUE)
    if (!inherits(out, "try-error") && !is.null(out)) {
      if (all(c("lon","lat") %in% names(out)))    return(c(out$lon[1], out$lat[1]))
      if (all(c("coords.x1","coords.x2") %in% names(out))) return(c(out$coords.x1[1], out$coords.x2[1]))
      if (all(c("x","y") %in% names(out))) return(c(out$x[1], out$y[1]))
    }
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
      q_full <- ifelse(!is.na(pais_final) & nzchar(pais_final),
                       paste(ciudad_clean, pais_final, sep = ", "), ciudad_clean)
      
      n <- length(need_idx)
      for (k in seq_along(need_idx)) {
        i <- need_idx[k]
        coords <- geo_nominatim(q_full[i], tmo = 4)
        df$lon[i] <- coords[1]; df$lat[i] <- coords[2]
        incProgress(1/n)
        Sys.sleep(0.05)
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
  
  # --- POI: geocode using 'city' (not 'city') and with small alias of cities ---
  geocode_interes_missing <- function(h) {
    if (is.null(h) || !nrow(h)) return(h)
    for (nm in c("descriptor","ciudad","pais","lat","lon")) if (!nm %in% names(h)) h[[nm]] <- NA
    need_idx <- which(is.na(h$lat) | is.na(h$lon))
    if (!length(need_idx)) return(h)
    
    withProgress(message = tr("ed_geo_interest", lang()), value = 0, {
      norm_city <- function(x) {
        x <- trimws(as.character(x))
        if (!nzchar(x)) return(x)
      }
      for (i in need_idx) {
        city <- norm_city(h$ciudad[i])
        q  <- paste(na.omit(c(h$descriptor[i], city, h$pais[i])), collapse = ", ")
        cr <- geocode_one(q)
        # fallback: only descriptor + city
        if (is.na(cr[1]) || is.na(cr[2])) {
          q2 <- paste(na.omit(c(h$descriptor[i], city)), collapse = ", ")
          cr <- geocode_one(q2)
        }
        h$lon[i] <- cr[1]; h$lat[i] <- cr[2]
        incProgress(1/length(need_idx)); Sys.sleep(0.05)
      }
    })
    h$lat <- as.numeric(h$lat); h$lon <- as.numeric(h$lon); h
  }
  
  # --- Hotels: try to complete hotel_link (Wikipedia opensearch 'number + city + hotel') ---
  # HOTELS: Wikipedia only; OSM disabled by default
  hotel_link_missing <- function(h, places_df = NULL, wiki_lang = "es",
                                 per_item_timeout = 2, total_budget_s = 12,
                                 use_osm_fallback = TRUE) {
    if (is.null(h) || !nrow(h)) return(h)
    if (!"hotel_link" %in% names(h)) h$hotel_link <- NA_character_
    need <- which(is.na(h$hotel_link) | h$hotel_link == "")
    if (!length(need)) return(h)
    
    city_by_id <- if (!is.null(places_df) && all(c("place_id","ciudad","pais") %in% names(places_df))) {
      list(city  = setNames(as.character(places_df$ciudad), places_df$place_id),
           country = setNames(as.character(places_df$pais),   places_df$place_id))
    } else NULL
    
    seen <- new.env(parent = emptyenv())  # <— cache local
    deadline <- Sys.time() + total_budget_s
    for (i in need) {
      if (Sys.time() > deadline) break
      nm  <- as.character(h$hotel_name[i] %||% "")
      pid <- as.character(h$place_id[i] %||% "")
      city <- if (!is.null(city_by_id)) city_by_id$city[[pid]] else NA_character_
      ctry <- if (!is.null(city_by_id)) city_by_id$country[[pid]] else NA_character_
      lat <- suppressWarnings(as.numeric(h$hotel_lat[i]))
      lon <- suppressWarnings(as.numeric(h$hotel_lon[i]))
      
      key <- paste(norm_str(nm), norm_str(city), round(lat,4), round(lon,4), sep="|")
      if (exists(key, envir = seen, inherits = FALSE)) {
        h$hotel_link[i] <- get(key, envir = seen); next
      }
      
      url <- best_wiki_link(nm, city = city, country = ctry,
                            lat = lat, lon = lon, lang = wiki_lang,
                            type = "hotel", per_item_timeout = per_item_timeout,
                            budget_left = as.numeric(difftime(deadline, Sys.time(), units="secs")))
      
      if ((is.na(url) || !nzchar(url)) && isTRUE(use_osm_fallback) &&
          !is.na(lat) && !is.na(lon)) {
        url <- try(find_osm_url_near(lat, lon, name = nm, restrict_hotels = TRUE,
                                     radius_m = 150, timeout_s = per_item_timeout),
                   silent = TRUE)
        if (inherits(url, "try-error")) url <- NA_character_
      }
      
      if (!is.na(url) && nzchar(url)) h$hotel_link[i] <- url
      assign(key, h$hotel_link[i] %||% NA_character_, envir = seen)
      Sys.sleep(0.03)
    }
    h
  }
  
  interes_link_missing <- function(inter_df, wiki_lang = "es",
                                   per_item_timeout = 3, total_budget_s = 18,
                                   use_osm_fallback = TRUE) {
    h <- inter_df
    if (is.null(h) || !nrow(h)) return(h)
    if (!"link" %in% names(h)) h$link <- NA_character_
    need <- which(is.na(h$link) | h$link == "")
    if (!length(need)) return(h)
    
    seen <- new.env(parent = emptyenv())  # <— cache local
    deadline <- Sys.time() + total_budget_s
    for (i in need) {
      if (Sys.time() > deadline) break
      nm   <- as.character(h$descriptor[i] %||% "")
      city <- as.character(h$ciudad[i]    %||% "")
      ctry <- as.character(h$pais[i]      %||% "")
      lat  <- suppressWarnings(as.numeric(h$lat[i]))
      lon  <- suppressWarnings(as.numeric(h$lon[i]))
      
      key <- paste(norm_str(nm), norm_str(city), round(lat,4), round(lon,4), sep="|")
      if (exists(key, envir = seen, inherits = FALSE)) {
        h$link[i] <- get(key, envir = seen); next
      }
      
      url <- best_wiki_link(nm, city = city, country = ctry,
                            lat = lat, lon = lon, lang = wiki_lang,
                            type = "poi", per_item_timeout = per_item_timeout,
                            budget_left = as.numeric(difftime(deadline, Sys.time(), units="secs")))
      
      if ((is.na(url) || !nzchar(url)) && isTRUE(use_osm_fallback) &&
          !is.na(lat) && !is.na(lon)) {
        url <- try(find_osm_url_near(lat, lon, name = nm, restrict_hotels = FALSE,
                                     radius_m = 180, timeout_s = per_item_timeout),
                   silent = TRUE)
        if (inherits(url, "try-error")) url <- NA_character_
      }
      
      if (!is.na(url) && nzchar(url)) h$link[i] <- url
      assign(key, h$link[i] %||% NA_character_, envir = seen)
      Sys.sleep(0.03)
    }
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
    
    places <- td$places
    stages <- td$stages
    
    # --- 1) Very defensive standardization of 'stages' ---
    if (is.null(stages)) stages <- tibble::tibble()
    
    if (!is.data.frame(stages)) {
      # It came as a vector (date included) -> we convert it into a tibble with a 'date' column
      if (inherits(stages, "Date") || is.atomic(stages)) {
        stages <- tibble::tibble(date = stages)
      } else {
        stages <- tibble::as_tibble(stages)
      }
    } else {
      stages <- tibble::as_tibble(stages)
    }
    
    # Ensure unique numbers so dplyr doesn't fail
    names(stages) <- make.unique(names(stages), sep = "__dup")
    
    # Ensure minimum columns
    req_cols <- c("etapa","from_id","to_id","date","description","medi","ruta","modo")
    for (nm in req_cols) if (!nm %in% names(stages)) stages[[nm]] <- NA
    
    # Resolve medium/mode conflict without duplicating numbers
    if ("medi" %in% names(stages) && "modo" %in% names(stages)) {
      stages$modo <- dplyr::coalesce(
        as.character(stages$modo),
        as.character(stages$medi)
      )
      stages$medi <- NULL
    } else if ("medi" %in% names(stages) && !"modo" %in% names(stages)) {
      names(stages)[names(stages) == "medi"] <- "modo"
    }
    if (!"modo" %in% names(stages)) stages$modo <- NA_character_
    
    # If empty, return tibble with column 'day' and 0 rows
    if (!nrow(stages)) {
      out <- dplyr::mutate(stages, dia = as.Date(character()))
      return(out[0, ])
    }
    
    # --- 2) Normalized mode of transport ---
    stages$modo <- tolower(trimws(as.character(stages$modo)))
    stages$modo <- dplyr::case_when(
      stages$modo %in% c("vol","flight","avion","avió","plane","vuelo") ~ "vol",
      stages$modo %in% c("cotxe","car","coche","auto","automovil","automóvil","drive","road") ~ "cotxe",
      stages$modo %in% c("tren","train","ferrocarril","rail","railway") ~ "tren",
      stages$modo %in% c("vaixell","barco","ferry","ship","boat") ~ "vaixell",
      TRUE ~ "cotxe"
    )
    same_place <- (as.character(stages$from_id) == as.character(stages$to_id))
    stages$modo[same_place %in% TRUE] <- "visita"
    
    # --- 3) Expand date and enrich ---
    stages %>%
      dplyr::mutate(dia_list = lapply(date, .expand_fecha, year = 2025)) %>%
      tidyr::unnest(cols = c(dia_list), names_repair = "minimal") %>%
      dplyr::rename(dia = dia_list) %>%
      dplyr::left_join(
        places %>% dplyr::select(place_id, ciudad_from = ciudad, lat_from = lat, lon_from = lon),
        by = c("from_id" = "place_id")
      ) %>%
      dplyr::left_join(
        places %>% dplyr::select(place_id, ciudad_to = ciudad, lat_to = lat, lon_to = lon, wikipedia),
        by = c("to_id" = "place_id")
      ) %>%
      dplyr::arrange(etapa, dia)
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
  output$tab_info_title <- renderText(tr("btn_info", lang()))
  
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
  register_edit("info_table", "info", "info_table")
  output$places_table  <- DT::renderDT({ td <- current_td(); req(td);  df <- td$places;  if (is.null(df)) df <- data.frame();  DT::datatable(df,selection =  if (isTRUE(input$allow_edit_process)) "multiple" else "none", editable  = isTRUE(input$allow_edit_process),options   = list(pageLength = 8))})
  output$stages_table  <- DT::renderDT({ td <- current_td(); req(td);  df <- td$stages;  if (is.null(df)) df <- data.frame();  DT::datatable(df,selection =  if (isTRUE(input$allow_edit_process)) "multiple" else "none", editable  = isTRUE(input$allow_edit_process),options   = list(pageLength = 8))})
  output$hotels_table  <- DT::renderDT({ td <- current_td(); req(td);  df <- td$hotels;  if (is.null(df)) df <- data.frame();  DT::datatable(df,selection =  if (isTRUE(input$allow_edit_process)) "multiple" else "none", editable  = isTRUE(input$allow_edit_process),options   = list(pageLength = 8))})
  output$interes_table  <- DT::renderDT({ td <- current_td(); req(td);  df <- td$interes;  if (is.null(df)) df <- data.frame(); DT::datatable(df,selection =  if (isTRUE(input$allow_edit_process)) "multiple" else "none", editable  = isTRUE(input$allow_edit_process),options   = list(pageLength = 8))})
#  output$info_table  <- DT::renderDT({ td <- current_td(); req(td);  df <- td$info;  if (is.null(df)) df <- data.frame(); DT::datatable(df,selection =  if (isTRUE(input$allow_edit_process)) "multiple" else "none", editable  = isTRUE(input$allow_edit_process),options   = list(pageLength = 8))})
  output$info_table <- DT::renderDT({td <- current_td(); req(td)
    wide <- info_to_wide_lang(td$info, lang())
    DT::datatable(
      wide,
      selection = if (isTRUE(input$allow_edit_process)) "multiple" else "none",
      editable  = if (isTRUE(input$allow_edit_process)) list(target="cell", disable=list(columns=c(0))) else FALSE,
      options   = list(pageLength = 8)
    )
  })
  
  observeEvent(input$info_table_cell_edit, {
    info <- input$info_table_cell_edit
    td <- trip_data(); req(td)
    
    # Reconstruct the wide table as seen on the screen (to map row/column)
    wide <- info_to_wide_lang(td$info, lang())
    # Column edited
    colname <- names(wide)[info$col]
    # Si intentan editar 'country' (col 1), ignoramos
    if (identical(colname, "country")) return()
    
    # Country (row)
    cty   <- wide$country[info$row]
    field <- toupper(colname)  # DOCUMENTATION, LOCAL_TIME, ...
    
    # Language column to be updated in the tidy
    lcode <- switch(lang(), "es"="es", "ca"="ca", "en"="en", "en")
    
    # Make sure the tidy row (country + field) exists
    idx <- which(toupper(td$info$field) == field & td$info$country == cty)
    if (!length(idx)) {
      td$info <- rbind(td$info, data.frame(
        country = cty, field = field,
        en = NA_character_, es = NA_character_, ca = NA_character_,
        stringsAsFactors = FALSE
      ))
      idx <- nrow(td$info)
    }
    
    # Applies the new value to the active language
    td$info[idx, lcode] <- as.character(info$value)
    
    # Save in memory and refresh the wide view
    trip_data(td)
    DT::replaceData(DT::dataTableProxy("info_table"),
                    info_to_wide_lang(td$info, lang()),
                    resetPaging = FALSE, rownames = FALSE)
  })
  
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
    
    info_out <- NULL
    if (!is.null(td$info) && nrow(td$info)) info_out <- info_to_wide(td$info)
    
    dir.create("www", showWarnings = FALSE, recursive = TRUE)
    base_name <- gsub("[^[:alnum:]_\\x2D ]+", "_", trimws(base_name %||% paste0("itinerary_", Sys.Date())))
    save_path <- file.path("www", paste0(base_name, ".xlsx"))
    
    if (file.exists(save_path) && !isTRUE(overwrite)) {
      showNotification(tr("warn_exists", lang()), type = "warning", duration = 6); return(invisible(FALSE))
    }
    
    ok <- TRUE; err_msg <- NULL
    tryCatch({
      writexl::write_xlsx(
        Filter(Negate(is.null), list(
          places  = places_out,
          stages  = stages_out,
          hotels  = hotels_out,
          interes = interes_out,
          info    = info_out
        )),
        path = save_path
      )
    }, error = function(e) { ok <<- FALSE; err_msg <<- e$message })
    
    if (ok) {
      showNotification(paste0(tr("saved_www", lang()), basename(save_path)), type = "message", duration = 6)
      files <- list_www_xlsx()
      updateSelectInput(session, "proc_trip_www",
                        choices = c(tr("none", lang()), files),
                        selected = basename(save_path)
      )
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
    td <- current_td()
    
    make_block <- function(country, tbl, lcode) {
      # guard + pick language column
      if (is.null(tbl) || !nrow(tbl)) return(NULL)
      col <- switch(lcode, "es"="es","ca"="ca","en"="en","en")
      if (!col %in% names(tbl)) return(NULL)
      
      # normalize for matching
      fld  <- toupper(trimws(as.character(tbl$field)))
      ctry <- trimws(as.character(tbl$country))
      
      pick <- function(field_name) {
        idx <- which(fld == toupper(field_name) & ctry == country)
        if (!length(idx)) return(NULL)
        vals <- tbl[idx, col, drop = TRUE]
        vals <- vals[!is.na(vals) & nzchar(as.character(vals))]
        if (!length(vals)) return(NULL)
        as.character(vals[1])
      }
      
      fields <- c("DOCUMENTATION","LOCAL_TIME","LANGUAGE","INTERNET","PLUGS","CURRENCY")
      items <- lapply(fields, function(f) {
        val <- pick(f)
        if (!is.null(val)) paste0("<li><strong>", f, ":</strong> ",
                                  htmltools::htmlEscape(val), "</li>")
      })
      items <- Filter(Negate(is.null), items)
      if (!length(items)) return(NULL)
      
      paste0("<h4><strong>", htmltools::htmlEscape(country),
             "</strong></h4><ul>", paste(items, collapse=""), "</ul>")
    }
    
    html_body <- NULL
    if (!is.null(td$info) && nrow(td$info)) {
      # Render all countries present
      countries <- unique(td$info$country)
      blocks <- lapply(countries, function(cty) make_block(cty, td$info, l))
      blocks <- Filter(Negate(is.null), blocks)
      if (length(blocks)) html_body <- paste(blocks, collapse = "")
    }
    
    showModal(modalDialog(
      title = tr("info_modal_title", l),
      size = "l", easyClose = TRUE, footer = modalButton(tr("close", l)),
      HTML(html_body)
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
    shinybusy::show_modal_spinner(spin="fading-circle", color="#007BFF", text = tr("ed_working", lang()))
    on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
    
    withProgress(message = tr("ed_auto_progress", lang()), value = 0, {
      incProgress(0.2, detail = tr("ed_geo_places", lang()))
      places1 <- geocode_missing(td$places, default_country = NULL)
      
      incProgress(0.5, detail = tr("ed_wiki_places", lang()))
      wiki_lang <- switch(lang(), "en"="en","es"="es","ca"="es","en")
      places2 <- wikipedia_missing(places1, lang = wiki_lang)
      
      hotels_out <- td$hotels
      if (!is.null(hotels_out) && nrow(hotels_out)) {
        hotels_out <- fill_missing_hotel_coords(hotels_out, places2)
        hotels_out <- hotel_link_missing(hotels_out, places_df = places2, wiki_lang = wiki_lang,
                                         use_osm_fallback = F)
      }
      
      inter_out <- td$interes
      if (!is.null(inter_out) && nrow(inter_out)) {
        inter_out <- geocode_interes_missing(inter_out)
        inter_out <- interes_link_missing(inter_out, wiki_lang = wiki_lang,
                                          use_osm_fallback = F)
      }
      
      set_td(modifyList(td, list(places = places2, hotels = hotels_out, interes = inter_out)))
      incProgress(1, detail = tr("done", lang()))
    })
    
    isolate({
      td2 <- get_td()
      DT::replaceData(DT::dataTableProxy("places_table"),  td2$places, resetPaging = FALSE)
      if (!is.null(td2$hotels))  DT::replaceData(DT::dataTableProxy("hotels_table"),  td2$hotels,  resetPaging = FALSE)
      if (!is.null(td2$interes)) DT::replaceData(DT::dataTableProxy("interes_table"), td2$interes, resetPaging = FALSE)
    })
    redraw_map(FALSE)
    
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
  
 # Helpers OSM (bbox + extracción de URL) 
  fetch_wiki <- function(q, lang = "es", tmo = 3) {
    if (is.null(q) || !nzchar(q)) return(NA_character_)
    url <- paste0("https://", lang, ".wikipedia.org/w/api.php")
    req <- httr2::request(url) |>
      httr2::req_url_query(action = "opensearch", search = q, limit = 1, namespace = 0, format = "json") |>
      httr2::req_user_agent("geoitinr/1.0 (contact: you@example.com)") |>
      httr2::req_timeout(tmo) |>
      httr2::req_retry(max_tries = 2)
    resp <- try(httr2::req_perform(req), silent = TRUE)
    if (inherits(resp, "try-error")) return(NA_character_)
    js <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
    if (inherits(js, "try-error")) return(NA_character_)
    if (length(js) >= 4 && length(js[[4]]) >= 1) return(js[[4]][1])
    NA_character_
  }
  
  bbox_around <- function(lat, lon, radius_m = 250) {
    dlat <- radius_m / 111320; dlon <- radius_m / (111320 * cos(lat * pi/180))
    c(lon - dlon, lat - dlat, lon + dlon, lat + dlat)
  }
  first_nonempty <- function(...) {
    xx <- unlist(list(...)); xx <- xx[!is.na(xx) & nzchar(as.character(xx))]
    if (length(xx)) as.character(xx[1]) else NA_character_
  }
  osm_pick_url <- function(attrs) {
    nm <- names(attrs)
    w   <- if ("website"         %in% nm) attrs[["website"]]         else NA
    cw  <- if ("contact:website" %in% nm) attrs[["contact:website"]] else NA
    url <- if ("url"             %in% nm) attrs[["url"]]             else NA
    cu  <- if ("contact:url"     %in% nm) attrs[["contact:url"]]     else NA
    out <- first_nonempty(w, cw, url, cu)
    if (!is.na(out)) return(out)
    wp  <- if ("wikipedia" %in% nm) attrs[["wikipedia"]] else NA
    if (!is.na(wp) && nzchar(wp) && grepl(":", wp, fixed = TRUE)) {
      parts <- strsplit(wp, ":", fixed = TRUE)[[1]]
      return(paste0("https://", parts[1], ".wikipedia.org/wiki/",
                    utils::URLencode(parts[2], reserved = TRUE)))
    }
    wd  <- if ("wikidata" %in% nm) attrs[["wikidata"]] else NA
    if (!is.na(wd) && nzchar(wd)) return(paste0("https://www.wikidata.org/wiki/", wd))
    NA_character_
  }
  collect_osm_candidates <- function(res, lat, lon) {
    layers <- list(res$osm_points, res$osm_polygons, res$osm_multipolygons, res$osm_lines)
    layers <- Filter(function(x) !is.null(x) && nrow(x) > 0, layers)
    if (!length(layers)) return(NULL)
    out <- list()
    ref_pt <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
    for (x in layers) {
      g <- sf::st_geometry(x); if (length(g) == 0) next
      ctr <- suppressWarnings(sf::st_centroid(g))
      d   <- try(as.numeric(sf::st_distance(ctr, ref_pt)), silent = TRUE)
      if (inherits(d, "try-error") || length(d) != nrow(x)) d <- rep(Inf, nrow(x))
      attrs <- sf::st_drop_geometry(x); attrs$..dist <- d
      out[[length(out) + 1]] <- attrs
    }
    if (!length(out)) return(NULL)
    cand <- do.call(rbind, out)
    cand <- cand[order(cand$..dist), , drop = FALSE]
    head(cand, 80)
  }
  find_osm_url_near <- function(lat, lon, name = NULL, restrict_hotels = FALSE,
                                radius_m = 250, timeout_s = 6) {
    if (is.na(lat) || is.na(lon)) return(NA_character_)
    bb <- bbox_around(lat, lon, radius_m)
    try_layer <- function(qobj) {
      res <- try(osmdata::osmdata_sf(qobj), silent = TRUE)
      if (inherits(res, "try-error")) return(NULL)
      collect_osm_candidates(res, lat, lon)
    }
    q <- osmdata::opq(bbox = bb, timeout = timeout_s)
    cand <- NULL
    if (!is.null(name) && nzchar(name)) {
      cand <- try_layer(osmdata::add_osm_feature(q, key = "name", value = name, value_exact = FALSE, match_case = FALSE))
    }
    if (is.null(cand) && isTRUE(restrict_hotels)) {
      cand <- try_layer(osmdata::add_osm_feature(q, key = "tourism", value = c("hotel","hostel","guest_house"), value_exact = TRUE))
      if (is.null(cand)) cand <- try_layer(osmdata::add_osm_feature(q, key = "amenity", value = c("hotel"), value_exact = TRUE))
    }
    if (is.null(cand) && !isTRUE(restrict_hotels)) {
      keys <- list(
        list("tourism", c("museum","gallery","attraction")),
        list("amenity", c("place_of_worship")),
        list("historic", c("monument","heritage","yes")),
        list("building", c("cathedral","church"))
      )
      for (kv in keys) {
        cand <- try_layer(osmdata::add_osm_feature(q, key = kv[[1]], value = kv[[2]], value_exact = TRUE))
        if (!is.null(cand)) break
      }
    }
    if (is.null(cand) || !nrow(cand)) return(NA_character_)
    for (i in seq_len(nrow(cand))) {
      url <- osm_pick_url(as.list(cand[i, , drop = FALSE]))
      if (!is.na(url) && nzchar(url)) return(url)
    }
    NA_character_
  }
  
  # ---------------- Wikipedia/Wikidata helpers ----------------
  norm_str <- function(s) {
    s <- tolower(trimws(as.character(s %||% "")))
    s <- iconv(s, to="ASCII//TRANSLIT")           # quita acentos
    s <- gsub("[^[:alnum:] ]+", " ", s)
    s <- gsub("\\s+", " ", s)
    trimws(s)
  }
  sim_ratio <- function(a,b) {
    a <- norm_str(a); b <- norm_str(b)
    if (!nzchar(a) || !nzchar(b)) return(0)
    ad <- utils::adist(a,b, ignore.case = TRUE)
    1 - (ad / max(nchar(a), nchar(b)))
  }
  
  # Minimal useful aliases (you can expand)
  expand_aliases <- function(name, type = c("poi","hotel"), city = NULL) {
    type <- match.arg(type)
    base <- unique(c(name))
    n <- norm_str(name); cty <- norm_str(city)
    unique(base)
  }
  
  wiki_url_build <- function(lang, title) {
    if (!nzchar(title)) return(NA_character_)
    paste0("https://", lang, ".wikipedia.org/wiki/",
           utils::URLencode(title, reserved = TRUE))
  }
  
  wiki_opensearch_urls <- function(lang, query, limit = 3, tmo = 3) {
    if (!nzchar(query)) return(character(0))
    url <- paste0("https://", lang, ".wikipedia.org/w/api.php")
    req <- httr2::request(url) |>
      httr2::req_url_query(action="opensearch", search=query, limit=limit,
                           namespace=0, format="json") |>
      httr2::req_timeout(tmo) |>
      httr2::req_retry(max_tries = 2)
    resp <- try(httr2::req_perform(req), silent = TRUE)
    if (inherits(resp, "try-error")) return(character(0))
    js <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
    if (inherits(js, "try-error") || length(js) < 4) return(character(0))
    as.character(js[[4]] %||% character(0))
  }
  
  wiki_geosearch <- function(lang, lat, lon, radius = 150, limit = 15, tmo = 3) {
    if (is.na(lat) || is.na(lon)) return(data.frame())
    url <- paste0("https://", lang, ".wikipedia.org/w/api.php")
    req <- httr2::request(url) |>
      httr2::req_url_query(
        action="query", list="geosearch",
        gscoord=paste0(lat,"|",lon),
        gsradius=radius, gslimit=limit, format="json"
      ) |>
      httr2::req_timeout(tmo) |>
      httr2::req_retry(max_tries = 2)
    resp <- try(httr2::req_perform(req), silent = TRUE)
    if (inherits(resp, "try-error")) return(data.frame())
    js <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
    if (inherits(js, "try-error")) return(data.frame())
    items <- try(js$query$geosearch, silent = TRUE)
    if (inherits(items, "try-error") || is.null(items) || !nrow(as.data.frame(items))) return(data.frame())
    df <- as.data.frame(items)
    df$url <- vapply(df$title, function(t) wiki_url_build(lang, t), character(1))
    df
  }
  
  wikidata_search_qids <- function(query, language = "en", limit = 5, tmo = 3) {
    if (!nzchar(query)) return(character(0))
    url <- "https://www.wikidata.org/w/api.php"
    req <- httr2::request(url) |>
      httr2::req_url_query(action="wbsearchentities", format="json",
                           language=language, search=query, type="item", limit=limit) |>
      httr2::req_timeout(tmo) |>
      httr2::req_retry(max_tries = 2)
    resp <- try(httr2::req_perform(req), silent = TRUE)
    if (inherits(resp, "try-error")) return(character(0))
    js <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
    if (inherits(js, "try-error")) return(character(0))
    hits <- js$search
    if (is.null(hits) || !length(hits)) return(character(0))
    unique(vapply(hits, function(x) as.character(x$id %||% ""), character(1)))
  }
  
  wikidata_sitelink_url <- function(qid, langs = c("es","en","it","ca"), tmo = 3) {
    if (is.null(qid) || !nzchar(qid)) return(NA_character_)
    url <- "https://www.wikidata.org/w/api.php"
    req <- httr2::request(url) |>
      httr2::req_url_query(action="wbgetentities", format="json",
                           ids=qid, props="sitelinks/urls") |>
      httr2::req_timeout(tmo) |>
      httr2::req_retry(max_tries = 2)
    resp <- try(httr2::req_perform(req), silent = TRUE)
    if (inherits(resp, "try-error")) return(NA_character_)
    js <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
    if (inherits(js, "try-error")) return(NA_character_)
    ent <- js$entities[[qid]]
    if (is.null(ent) || is.null(ent$sitelinks)) return(NA_character_)
    for (lg in langs) {
      key <- paste0(lg, "wiki")
      if (!is.null(ent$sitelinks[[key]]$url)) return(ent$sitelinks[[key]]$url)
    }
    # if not in those languages, returns the first one available
    sl <- ent$sitelinks
    if (length(sl)) return(as.character(sl[[1]]$url %||% NA_character_))
    NA_character_
  }
  
  # --- Normalization/alias/distance helpers ---
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  
  norm_str <- function(x){
    x <- tolower(trimws(as.character(x)))
    x <- iconv(x, to = "ASCII//TRANSLIT")
    gsub("[^a-z0-9]+"," ", x)
  }
  
  city_aliases <- function(city){
    if (is.null(city) || !nzchar(city)) return(character(0))
    cty <- tolower(trimws(as.character(city)))
    unique(c(cty, norm_str(cty)))
  }
  
  haversine_km <- function(lat1, lon1, lat2, lon2){
    if (any(is.na(c(lat1,lon1,lat2,lon2)))) return(Inf)
    R <- 6371
    dlat <- (lat2-lat1)*pi/180
    dlon <- (lon2-lon1)*pi/180
    a <- sin(dlat/2)^2 + cos(lat1*pi/180)*cos(lat2*pi/180)*sin(dlon/2)^2
    R*2*atan2(sqrt(a), sqrt(1-a))
  }
  
  # --- Safe wrappers for Wikipedia searches ---
  safe_geosearch <- function(lat, lon, lang="es", radius=250, limit=12, tmo=4){
    if (is.na(lat) || is.na(lon)) return(list())
    url <- paste0("https://", lang, ".wikipedia.org/w/api.php")
    req <- httr2::request(url) |>
      httr2::req_url_query(
        action="query", list="geosearch", format="json",
        gscoord=paste0(lat, "|", lon), gsradius=radius, gslimit=limit
      ) |>
      httr2::req_timeout(tmo) |>
      httr2::req_retry(max_tries = 2)
    resp <- try(httr2::req_perform(req), silent=TRUE)
    if (inherits(resp,"try-error")) return(list())
    js <- try(httr2::resp_body_json(resp, simplifyVector = FALSE), silent=TRUE)
    if (inherits(js,"try-error")) return(list())
    gs <- try(js$query$geosearch, silent=TRUE)
    if (inherits(gs,"try-error") || is.null(gs)) return(list())
    # Devuelve siempre lista de listas {pageid,title}
    out <- lapply(gs, function(x){
      list(pageid = suppressWarnings(as.integer(x$pageid %||% NA)),
           title  = as.character(x$title %||% ""))
    })
    # Filter candidates without title
    Filter(function(x) is.list(x) && nzchar(x$title %||% ""), out)
  }
  
  safe_opensearch <- function(q, lang="es", limit=10, tmo=4){
    if (is.null(q) || !nzchar(q)) return(list())
    url <- paste0("https://", lang, ".wikipedia.org/w/api.php")
    req <- httr2::request(url) |>
      httr2::req_url_query(action="opensearch", search=q, limit=limit, namespace=0, format="json") |>
      httr2::req_timeout(tmo) |>
      httr2::req_retry(max_tries = 2)
    resp <- try(httr2::req_perform(req), silent=TRUE)
    if (inherits(resp,"try-error")) return(list())
    js <- try(httr2::resp_body_json(resp, simplifyVector = FALSE), silent=TRUE)
    if (inherits(js,"try-error") || length(js) < 4) return(list())
    titles <- try(unlist(js[[2]]), silent=TRUE)
    urls   <- try(unlist(js[[4]]), silent=TRUE)
    if (inherits(titles,"try-error") || length(titles)==0) return(list())
    if (inherits(urls,"try-error")) urls <- character(length(titles))
    out <- lapply(seq_along(titles), function(i){
      list(pageid = NA_integer_, title = as.character(titles[i] %||% ""),
           url    = as.character(urls[i]   %||% ""))
    })
    Filter(function(x) is.list(x) && nzchar(x$title %||% ""), out)
  }
  
  # --- Page metadata (coords + categories + disambiguation) ---
  wiki_page_meta <- function(pageids, lang="es"){
    pageids <- unique(na.omit(as.integer(pageids)))
    if (!length(pageids)) return(list())
    url <- paste0("https://", lang, ".wikipedia.org/w/api.php")
    req <- httr2::request(url) |>
      httr2::req_url_query(
        action="query", format="json",
        prop="pageprops|coordinates|categories",
        ppprop="disambiguation|wikibase_item",
        cllimit=50, clshow="!hidden",
        pageids=paste(pageids, collapse="|")
      ) |>
      httr2::req_user_agent("geoitinr/1.0") |>
      httr2::req_timeout(5) |>
      httr2::req_retry(max_tries = 2)
    resp <- try(httr2::req_perform(req), silent=TRUE)
    if (inherits(resp,"try-error")) return(list())
    js <- try(httr2::resp_body_json(resp, simplifyVector = FALSE), silent=TRUE)
    if (inherits(js,"try-error")) return(list())
    pages <- try(js$query$pages, silent=TRUE)
    if (inherits(pages,"try-error") || is.null(pages)) return(list())
    
    # 'pages' is a list named by pageid; convert it into a simple list
    lst <- unname(pages)
    out <- lapply(lst, function(p){
      # p can be either a list or a data frame; we handle both.
      get1 <- function(obj, nm) { tryCatch(obj[[nm]], error=function(e) NULL) }
      title <- as.character(get1(p, "title") %||% "")
      pp    <- get1(p, "pageprops")
      is_dis <- FALSE
      if (is.list(pp)) is_dis <- !is.null(pp$disambiguation)
      
      # coords can be a list of lists or a data.frame
      lat <- lon <- NA_real_
      coords <- get1(p, "coordinates")
      if (!is.null(coords)) {
        if (is.data.frame(coords)) {
          lat <- suppressWarnings(as.numeric(coords$lat[1] %||% NA))
          lon <- suppressWarnings(as.numeric(coords$lon[1] %||% NA))
        } else if (is.list(coords) && length(coords)) {
          c1 <- coords[[1]]
          lat <- suppressWarnings(as.numeric((c1$lat) %||% NA))
          lon <- suppressWarnings(as.numeric((c1$lon) %||% NA))
        }
      }
      
      cats <- character(0)
      cl <- get1(p, "categories")
      if (!is.null(cl)) {
        if (is.data.frame(cl)) cats <- as.character(cl$title)
        else if (is.list(cl)) cats <- vapply(cl, function(x) as.character(x$title %||% ""), "")
        cats <- cats[nzchar(cats)]
      }
      
      pid <- suppressWarnings(as.integer(p$pageid %||% NA))
      list(pageid = pid, title = title, is_disambig = is_dis,
           lat = lat, lon = lon, categories = cats)
    })
    
    # indexed by pageid for fast lookup
    names(out) <- vapply(out, function(x) as.character(x$pageid %||% NA), "")
    out
  }
  
  # --- Ranking with city/country/distance --
  rank_candidates <- function(cands, meta, city=NULL, country=NULL, ref_lat=NA_real_, ref_lon=NA_real_){
    if (!length(cands)) return(NULL)
    city_toks <- city_aliases(city)
    sc <- vapply(seq_along(cands), function(i){
      cand <- cands[[i]]
      # save: cand must be listed with $title/$pageid
      if (!is.list(cand)) return(-1e6)
      ti <- as.character(cand$title %||% "")
      if (!nzchar(ti)) return(-1e6)
      pid <- suppressWarnings(as.integer(cand$pageid %||% NA))
      m <- if (!is.na(pid) && length(meta) && as.character(pid) %in% names(meta)) meta[[as.character(pid)]] else NULL
      
      s <- 0
      # Penalizes disambiguation
      if (is.list(m) && isTRUE(m$is_disambig)) s <- s - 100
      
      # Title contains city/alias
      tt <- norm_str(ti)
      if (length(city_toks) && any(nchar(city_toks) > 0) &&
          any(vapply(city_toks, function(tok) grepl(paste0("\\b",tok,"\\b"), tt), logical(1))))
        s <- s + 6
      
      # Title contains country
      if (!is.null(country) && nzchar(country)) {
        if (grepl(paste0("\\b", norm_str(country), "\\b"), tt)) s <- s + 2
      }
      
      # Categories with the city
      if (is.list(m) && length(m$categories)) {
        cats_norm <- norm_str(paste(m$categories, collapse=" "))
        if (length(city_toks) &&
            any(vapply(city_toks, function(tok) grepl(paste0("\\b",tok,"\\b"), cats_norm), logical(1))))
          s <- s + 5
      }
      
      # Distance to ref (if there are page coords)
      if (is.list(m) && !is.na(m$lat) && !is.na(ref_lat)) {
        dk <- haversine_km(ref_lat, ref_lon, m$lat, m$lon)
        if (is.finite(dk)) {
          if (dk <= 0.25)      s <- s + 10
          else if (dk <= 0.6)  s <- s + 6
          else if (dk <= 1.5)  s <- s + 3
          else if (dk <= 5)    s <- s + 1
          else                 s <- s - 6
        }
      }
      s
    }, numeric(1))
    ord <- order(sc, decreasing = TRUE, na.last = TRUE)
    list(order = ord, score = sc)
  }
  
  # --- BEST: use geosearch + opensearch + meta + ranking robust ---
  best_wiki_link <- function(name, city = NULL, country = NULL,
                             lat = NA_real_, lon = NA_real_,
                             lang = "es", type = c("poi","hotel"),
                             per_item_timeout = 4, budget_left = 8){
    
    type <- match.arg(type)
    if (is.null(name) || !nzchar(name)) return(NA_character_)
    
    # 1) Geosearch (if coords)
    cand_geo  <- safe_geosearch(lat, lon, lang = lang, radius = 250, limit = 12, tmo = per_item_timeout)
    
    # 2) Text (name + city + country)
    qtext <- paste(name, city, country)
    cand_text <- safe_opensearch(qtext, lang = lang, limit = 10, tmo = per_item_timeout)
    
    #3) Join candidates and filter non-list objects
    all_cand <- c(cand_geo, cand_text)
    all_cand <- Filter(function(x) is.list(x) && nzchar(as.character(x$title %||% "")), all_cand)
    if (!length(all_cand)) return(NA_character_)
    
    # 4) Deduplicate by normalized title
    titles_norm <- vapply(all_cand, function(x) norm_str(x$title %||% ""), "")
    keep <- !duplicated(titles_norm)
    all_cand <- all_cand[keep]
    all_cand <- head(all_cand, 12)
    
    # 5) Meta by pageid (those who have it)
    ids <- suppressWarnings(as.integer(na.omit(vapply(all_cand, function(x) x$pageid %||% NA_integer_, integer(1)))))
    meta <- wiki_page_meta(ids, lang = lang)
    
    # 6) Ranking
    rk <- rank_candidates(all_cand, meta, city = city, country = country, ref_lat = lat, ref_lon = lon)
    if (is.null(rk)) return(NA_character_)
    best <- all_cand[[ rk$order[1] ]]
    
    # 7) final Link
    if (!is.null(best$url) && nzchar(best$url)) {
      return(best$url)
    } else if (nzchar(best$title)) {
      return(paste0("https://", lang, ".wikipedia.org/wiki/",
                    utils::URLencode(gsub(" ", "_", best$title), reserved=TRUE)))
    }
    NA_character_
  }
  
  
  # ---- High-level cache to avoid repeating identical searches ----
  .cache_best_link <- new.env(parent = emptyenv())
  
  best_wiki_link <- local({
    .orig <- best_wiki_link  ## save the current version
    
    function(name, city = NULL, country = NULL,
             lat = NA_real_, lon = NA_real_,
             lang = "es", type = c("poi","hotel"),
             per_item_timeout = 2, budget_left = 6) {
      
      type <- match.arg(type)
      key <- paste(
        norm_str(name), norm_str(city), norm_str(country),
        round(lat, 4), round(lon, 4), lang, type,
        sep = "|"
      )
      
      if (exists(key, envir = .cache_best_link, inherits = FALSE)) {
        return(get(key, envir = .cache_best_link))
      }
      
      url <- .orig(name, city, country, lat, lon, lang, type,
                   per_item_timeout, budget_left)
      
      assign(key, url, envir = .cache_best_link)
      url
    }
  })
  
  
}

shinyApp(ui, server)

