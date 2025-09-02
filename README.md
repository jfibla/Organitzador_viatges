<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>GeoItinR — Travel Itinerary Geolocator (Shiny)</title>
</head>
<body>
  <h1>GeoItinR — Travel Itinerary Geolocator</h1>
  <p>
    <strong>GeoItinR</strong> is an R/Shiny app that turns a simple Excel workbook into an
    interactive, geocoded <em>travel itinerary</em>. It supports multilingual inputs
    (English/Catalan/Spanish), draws routes and destinations on a Leaflet map, and includes
    editing tools to complete missing coordinates and Wikipedia links for cities, hotels and
    places of interest.
  </p>

  <h2>Why it&apos;s useful</h2>
  <ul>
    <li>Start from a human-friendly Excel file (no API keys required).</li>
    <li>Instant interactive map (Leaflet) with flights, trains, boats, driving routes and visits.</li>
    <li>Automatic geocoding (Nominatim/OSM) and Wikipedia linking with rate‑limit friendly timeouts.</li>
    <li>Multilingual UI (EN/CA/ES) with one-click switching.</li>
    <li>Defensive parsing of dates and ranges (e.g., <code>12 Feb – 14 Feb</code> → three days).</li>
    <li>Data cleaning: accepts sheet/column names in EN/ES/CA and normalizes them internally.</li>
    <li>Built-in editors: fill missing coords, set manual coords from a map click, and autofill Wikipedia links.</li>
    <li>Export back to Excel with the harmonized structure.</li>
  </ul>

  <h2>Getting started</h2>
  <h3>Requirements</h3>
  <ul>
    <li>R &ge; 4.2 and <code>shiny</code>.</li>
    <li>Suggested packages (install if missing): <code>shiny</code>, <code>shinyWidgets</code>, <code>leaflet</code>, <code>leaflet.extras</code>, <code>dplyr</code>, <code>DT</code>, <code>osrm</code>, <code>sf</code>, <code>readxl</code>, <code>writexl</code>, <code>tidyr</code>, <code>lubridate</code>, <code>rlang</code>, <code>httr2</code>, <code>tidygeocoder</code>, <code>tmaptools</code>, <code>bslib</code>, <code>waiter</code>, <code>shinybusy</code>, <code>osmdata</code>.</li>
  </ul>
  <h3>Install packages (once)</h3>
  <pre><code># in R
pkgs &lt;- c(
  "shiny","shinyWidgets","leaflet","leaflet.extras","dplyr","DT","osrm","sf",
  "readxl","writexl","tidyr","lubridate","rlang","httr2","tidygeocoder","tmaptools",
  "bslib","waiter","shinybusy","osmdata"
)
install.packages(setdiff(pkgs, rownames(installed.packages())))
</code></pre>
  <h3>Run</h3>
  <pre><code># in R, from the folder that contains app.R
shiny::runApp("app.R")
</code></pre>

  <h2>Excel input schema</h2>
  <p>The app expects 2 mandatory sheets and up to 3 optional ones. Sheet and column names can be in EN/ES/CA; they are normalized internally.</p>

  <h3>Sheets</h3>
  <ul>
    <li><strong>Places</strong> (mandatory)</li>
    <li><strong>Stages</strong> (mandatory)</li>
    <li><strong>Hotels</strong> (optional)</li>
    <li><strong>Interest</strong> (optional)</li>
    <li><strong>Info</strong> (optional)</li>
  </ul>

  <h3>Columns by sheet (canonical names)</h3>
  <ul>
    <li><strong>Places</strong>: <code>place_id</code> | <code>city</code> | <code>country</code> | <code>lat</code> | <code>lon</code> | <code>wikipedia</code></li>
    <li><strong>Stages</strong>: <code>stage</code> | <code>from_id</code> | <code>to_id</code> | <code>date</code> | <code>description</code> | <code>transport</code> | <code>comments</code></li>
    <li><strong>Hotels</strong>: <code>place_id</code> | <code>hotel_name</code> | <code>hotel_link</code> | <code>details</code> | <code>hotel_lat</code> | <code>hotel_lon</code></li>
    <li><strong>Interest</strong>: <code>descriptor</code> | <code>lat</code> | <code>lon</code> | <code>city</code> | <code>country</code> | <code>details</code> | <code>type</code> | <code>link</code></li>
    <li><strong>Info</strong> (either tidy or wide):
      <ul>
        <li><em>Tidy</em>: <code>country</code> | <code>field</code> | <code>en</code> | <code>es</code> | <code>ca</code></li>
        <li><em>Wide</em>: <code>country</code> | <code>documentation</code> | <code>local_time</code> | <code>language</code> | <code>internet</code> | <code>plugs</code> | <code>currency</code></li>
      </ul>
    </li>
  </ul>

  <h3>Accepted values</h3>
  <ul>
    <li><strong>transport</strong>: <code>flight/plane</code>, <code>car</code>, <code>train</code>, <code>boat/ferry</code>. Same-<code>from_id</code>/<code>to_id</code> rows are treated as a <em>visit</em>.</li>
    <li><strong>type</strong> (Interest): examples include <code>art</code>, <code>culture</code>, <code>photo</code>, <code>gastronomy</code>, <code>history</code>, <code>market</code>, <code>viewpoint</code>, <code>monument</code>, <code>museum</code>, <code>nature</code>, <code>landscape</code>, <code>park</code>.</li>
  </ul>

  <details>
    <summary><strong>Column name synonyms (EN/ES/CA)</strong> — click to expand</summary>
    <p>Examples the app understands and normalizes:</p>
    <ul>
      <li><strong>city</strong> ⇢ <code>city</code>, <code>ciudad</code>, <code>ciutat</code></li>
      <li><strong>country</strong> ⇢ <code>country</code>, <code>pais</code>, <code>país</code></li>
      <li><strong>transport</strong> ⇢ <code>transport</code>, <code>modo</code>, <code>medi</code>, <code>medio</code></li>
      <li><strong>comments / route</strong> ⇢ <code>comments</code>, <code>ruta</code>, <code>trayecto</code>, <code>route_desc</code></li>
      <li><strong>interest type</strong> ⇢ <code>type</code>, <code>tipo</code>, <code>tipus</code></li>
      <li><strong>info fields</strong> ⇢ <code>documentation</code>, <code>local_time</code>, <code>language</code>, <code>internet</code>, <code>plugs</code>, <code>currency</code></li>
    </ul>
    <p>The app also accepts multiple sheet names (e.g., <em>Places/Lugares/Llocs</em>, <em>Stages/Etapas/Etapes</em>, etc.).</p>
  </details>

  <h2>How to use</h2>
  <ol>
    <li>Open the app and choose a UI language (EN/CA/ES).</li>
    <li>Load your Excel: either upload a file or pick one from the <code>www/</code> folder.</li>
    <li>Select a day with the inline date picker (or tick “Show all stages”).</li>
    <li>Filter markers by type (Itinerary / Flights / Car / Train / Boat / Visit).</li>
    <li>Toggle “Show hotels on the map” to display hotel markers and clusters.</li>
    <li>Explore details:
      <ul>
        <li><strong>Stage details</strong> cards for the selected day or full itinerary.</li>
        <li><strong>Hotels</strong> per stage, with website links when available.</li>
        <li><strong>Places of interest</strong> for the current destination city.</li>
        <li><strong>Info</strong> modal by country: documentation, local time, language, internet, plugs, currency.</li>
      </ul>
    </li>
    <li>Export: <strong>Download Excel</strong> with harmonized sheets.</li>
  </ol>

  <h2>Editing tools</h2>
  <p>Enable <em>Allow edit</em> to unlock the editor sidebar and tables:</p>
  <ul>
    <li><strong>Autocomplete</strong>: one-click completion for missing <em>coords + Wikipedia + hotels + interest</em>.</li>
    <li><strong>Geocode selected places</strong>: only fill rows you select in the <em>Places</em> table.</li>
    <li><strong>Manual coordinates</strong>:
      <ul>
        <li>Pick a place from the dropdown.</li>
        <li>Click the map to capture the last coordinates, or type them.</li>
        <li>Apply to update the table.</li>
      </ul>
    </li>
    <li><strong>Wikipedia (selected)</strong>: fill links just for selected places.</li>
  </ul>
  <p>All data tables are editable (DataTables), with type-safe coercion and immediate in-memory updates.</p>

  <h2>Map &amp; routing</h2>
  <ul>
    <li><strong>Base map</strong>: OpenStreetMap via Leaflet.</li>
    <li><strong>Markers</strong>: destinations (by transport type), visits (same origin/destination), hotels, and interest points.</li>
    <li><strong>Layers control</strong>: toggle Flights, Trains, Boats, Drive, Destinations, Visits, Hotels, Interest.</li>
    <li><strong>Routes</strong>: driving polylines via public OSRM (<code>car</code> profile). Flights/Trains/Boats are drawn as straight lines.</li>
    <li><strong>Clustering</strong>: hotel and interest markers use cluster options when appropriate.</li>
  </ul>

  <h2>Geocoding &amp; Wikipedia</h2>
  <ul>
    <li><strong>Geocoding</strong>: primary Nominatim (OpenStreetMap) with short timeouts and retries; fallback to <code>tmaptools::geocode_OSM</code> when needed.</li>
    <li><strong>Wikipedia</strong>: opensearch + geosearch + metadata ranking to pick the most relevant page near the coordinates, with optional OSM URL fallback for POIs.</li>
    <li><strong>Hotels</strong>: tries to infer hotel website/Wikipedia using hotel name, city and coordinates.</li>
  </ul>

  <h2>Data flow &amp; robustness</h2>
  <ul>
    <li>Loads the first <code>.xlsx</code> from <code>www/</code> on start (if present).</li>
    <li>Conservative timeouts for network calls (<em>httr2</em>) to keep the UI responsive.</li>
    <li>Defensive handling of sheet/column variants and empty tables.</li>
    <li>Dates are expanded into daily rows to power the day picker and stage cards.</li>
    <li>Hotel assignment prefers <code>to_id</code> (destination) and falls back to <code>from_id</code> when needed.</li>
  </ul>

  <h2>Folder layout</h2>
  <ul>
    <li><code>app.R</code> — the Shiny app.</li>
    <li><code>www/</code> — optional example/input Excel files (read on load and selectable in the UI).</li>
  </ul>

  <h2>Privacy, terms &amp; rate limits</h2>
  <ul>
    <li>This app queries public services: Nominatim (OSM), Wikipedia/Wikidata and OSRM. Please respect their terms of use and rate limits.</li>
    <li>Consider editing the user agent string in the code to include your contact email when doing many requests.</li>
    <li>No analytics or tracking is included.</li>
  </ul>

  <h2>Troubleshooting</h2>
  <ul>
    <li><strong>No markers?</strong> Check that <em>Places</em> has valid <code>lat</code>/<code>lon</code>, or enable edit and run <em>Autocomplete</em>.</li>
    <li><strong>Interest list empty for a city?</strong> Ensure its rows have coordinates or run <em>Autocomplete</em>.</li>
    <li><strong>Routes not shown?</strong> OSRM can be temporarily unavailable; try again later or reduce the number of car legs.</li>
    <li><strong>Excel not recognized?</strong> Verify mandatory sheets exist (Places, Stages) and that column names map to the canonical ones listed above.</li>
  </ul>

### Hosted demo

You can try an interactive instance of **GeoItinR** here:

**(https://pyrprs.shinyapps.io/trip_app/)**

This server is meant for functionality testing and demos. Performance and availability may vary depending on server load. The demo uses the app’s default upload cap (configured in the code) and includes example datasets to explore the workflow quickly.

---


  <h2>Acknowledgements</h2>
  <p>Built with R, Shiny, Leaflet, OpenStreetMap/Nominatim, OSRM, and Wikipedia/Wikidata. Thanks to their communities.</p>

<h2>📄 User guide</h2>
<br>- 🇬🇧 [Download User Guide (EN)](https://github.com/jfibla/Organitzador_viatges/raw/main/Travel_Itinerary_App_UserGuide_EN.pdf)
<br>-  <img width="20" height="20" alt="image" src="https://github.com/user-attachments/assets/0bbcc085-e250-4190-b7fa-45d1642cf4b2" />
  [Descarregar Guia d'Usuari (CA)](https://github.com/jfibla/Organitzador_viatges/raw/main/Travel_Itinerary_App_UserGuide_CA.pdf)
<br>- 🇪🇸 [Descargar Guía de Usuario (ES)](https://github.com/jfibla/Organitzador_viatges/raw/main/Travel_Itinerary_App_UserGuide_ES.pdf)


  <h2>License</h2>
  <p>This project is licensed under the [CC BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0) license.</p>
</body>
</html>
