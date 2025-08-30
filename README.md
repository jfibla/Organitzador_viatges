[![License: CC BY-NC 4.0](https://img.shields.io/badge/License-CC_BY--NC_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc/4.0/)

<h1>🗺️ GeoIntR: Travel Itinerary Geolocator</h1>
<p>Multilingual <strong>Shiny</strong> application to visualize and optionally edit travel itineraries from an <strong>Excel file</strong>.<br>
It displays trip stages (flights, car, train, boat, and visits) on an interactive map, overlays hotels and points of interest, supports filtering by day or type, and allows <strong>editing in table form</strong> with <strong>saving</strong> to a normalized Excel file in <code>www/</code>.</p>

<hr>

<h2>✨ Languages available:</h2>
<ul> <strong>English</strong> (EN), <strong>Catalan</strong> (CA), <strong>Spanish</strong> (ES). Change language anytime using the buttons at the top right of the app.
</ul>
<h2>✨ Key Features</h2>
<ul>
  <li><strong>Excel reader</strong> with 2–4 sheets: <code>Places</code> and <code>Stages</code> (required), <code>Hotels</code> and <code>Interes</code> (optional).</li>
  <li><strong>Interactive map</strong> with markers and connections:
    <ul>
      <li>flights / train / boat as straight lines</li>
      <li><strong>car routes via OSRM</strong> (road routing service)</li>
    </ul>
  </li>
  <li><strong>Calendar</strong> to filter by day; toggle to see <strong>all stages</strong>.</li>
  <li><strong>Overlay layers</strong> for Hotels and Points of Interest, with popups and clustering.</li>
  <li><strong>Optional editing</strong> of tables (<code>Places</code>, <code>Stages</code>, <code>Hotels</code>, <code>Interes</code>) and <strong>export</strong> to <code>www/</code> in canonical format.</li>
  <li><strong>Autocompletion</strong> of missing coordinates (geocoding) and <strong>Wikipedia links</strong> when missing.</li>
  <li><strong>Dual-mode sidebar</strong>:
    <ul>
      <li><strong>Normal mode (visor):</strong> file upload, info buttons, itinerary filters.</li>
      <li><strong>Edit mode:</strong> simplified panel for editing + save controls. The toggle button label changes from <em>Allow edit</em> to <em>Return to travel visor</em> when editing is enabled.</li>
    </ul>
  </li>
</ul>

<hr>

<h2>🧰 Requirements</h2>
<ul>
  <li><strong>R</strong> (preferably via <strong>RStudio</strong>).</li>
  <li>Packages: <code>shiny</code>, <code>shinyWidgets</code>, <code>leaflet</code>, <code>leaflet.extras</code>, <code>dplyr</code>, <code>DT</code>, <code>osrm</code>, <code>sf</code>, <code>readxl</code>, <code>writexl</code>, <code>tidyr</code>, <code>lubridate</code>, <code>rlang</code>, <code>httr2</code>, <code>tidygeocoder</code>, <code>bslib</code>, <code>waiter</code>.</li>
  <li>One <code>app.R</code> file and a <strong><code>www/</code></strong> folder (place your <code>.xlsx</code> there if desired).</li>
</ul>

<p>Install packages (example):</p>
<pre><code class="language-r">install.packages(c(
  "shiny","shinyWidgets","leaflet","leaflet.extras","dplyr","DT",
  "osrm","sf","readxl","writexl","tidyr","lubridate","rlang",
  "httr2","tidygeocoder","bslib","waiter"
))
</code></pre>

<p>Run the app:</p>
<pre><code class="language-r">shiny::runApp()
</code></pre>

<hr>

<h2>🧾 Excel Structure</h2>

<h3>Accepted sheets</h3>
<ul>
  <li><strong>Places</strong> (required) — synonyms: <code>llocs</code>, <code>lugares</code>, <code>places</code>.</li>
  <li><strong>Stages</strong> (required) — synonyms: <code>etapes</code>, <code>etapas</code>, <code>stages</code>.</li>
  <li><strong>Hotels</strong> (optional) — synonyms: <code>hotels</code>, <code>hoteles</code>.</li>
  <li><strong>Interes</strong> (optional) — synonyms: <code>interes</code>, <code>interés</code>, <code>interests</code>, <code>interest</code>, <code>interesos</code>.</li>
</ul>

<h3>Recommended columns</h3>

<p><strong>Places</strong></p>
<ul>
  <li><code>place_id</code> — unique identifier.</li>
  <li><code>city</code> — city (synonyms: <code>ciudad</code>, <code>ciutat</code>).</li>
  <li><code>country</code> — country (synonyms: <code>pais</code>, <code>país</code>).</li>
  <li><code>lat</code>, <code>lon</code> — coordinates (autocompleted if missing).</li>
  <li><code>wikipedia</code> — optional link.</li>
</ul>

<p><strong>Stages</strong></p>
<ul>
  <li><code>etapa</code> — stage number or code.</li>
  <li><code>from_id</code> — origin <code>place_id</code>.</li>
  <li><code>to_id</code> — destination <code>place_id</code>.</li>
  <li><code>date</code> — date or range (e.g., <code>12 Feb – 14 Feb</code>).</li>
  <li><code>description</code> — optional.</li>
  <li><code>medi</code> — transport type: <em>vol</em> (flight), <em>cotxe</em> (car), <em>tren</em>, <em>vaixell</em> (boat). If <code>from_id == to_id</code>, treated as a <strong>visit</strong>.</li>
  <li><code>ruta</code> — optional notes (synonyms: <code>route</code>, <code>trayecto</code>, <code>trajeto</code>).</li>
</ul>

<p><strong>Hotels</strong></p>
<ul>
  <li><code>place_id</code> — linked city.</li>
  <li><code>hotel_name</code>, <code>hotel_link</code>, <code>details</code>.</li>
  <li><code>hotel_lat</code>, <code>hotel_lon</code> — coordinates (autocompleted if missing).</li>
</ul>

<p><strong>Interes</strong></p>
<ul>
  <li><code>descriptor</code> — point of interest name.</li>
  <li><code>city</code>, <code>country</code>, <code>lat</code>, <code>lon</code>.</li>
  <li><code>tipus</code> (<em>type</em>): e.g., museum, monument, nature, gastronomy.</li>
  <li><code>link</code> — optional.</li>
  <li><code>observacio</code> — notes/details (optional).</li>
</ul>

<h3>Dates supported</h3>
<ul>
  <li><code>YYYY-MM-DD</code></li>
  <li><code>12 Feb</code>, <code>12 Feb 2025</code>, <code>12 Feb – 14 Feb</code></li>
  <li>Missing year defaults to <strong>2025</strong> (configurable in code).</li>
</ul>

<hr>

<h2>🚀 Basic Workflow</h2>
<ol>
  <li><strong>Load Excel</strong>
    <ul>
      <li>From disk: sidebar &rarr; <em>From local disk (.xlsx)</em>.</li>
      <li>From <code>www/</code>: click <em>Refresh</em>, pick from the list, and <em>Load</em>.</li>
    </ul>
  </li>
  <li><strong>Select date</strong> in the calendar (tab <em>Map</em>).
    <ul>
      <li>Toggle <em>Show all stages</em> to see the full itinerary.</li>
    </ul>
  </li>
  <li><strong>Filter by type</strong>: Itinerary / Flights / Car / Train / Boat / Visit.</li>
  <li><strong>Explore the map</strong>:
    <ul>
      <li>Flights/Train/Boat: straight lines.</li>
      <li>Car: OSRM road routes (requires Internet).</li>
      <li>Hotels and Interest: additional layers with clustered markers.</li>
    </ul>
  </li>
  <li><strong>View panels</strong>: Stage details, hotels, and city-specific points of interest.</li>
  <li><strong>(Optional) Edit</strong>:
    <ul>
      <li>Tick <em>Allow edit</em> &rarr; label changes to <em>Return to travel visor</em>.</li>
      <li>Sidebar simplifies to editing tools and save controls; upload and info controls are hidden in edit mode.</li>
      <li>Tables (Places/Stages/Hotels/Interes) become editable.</li>
    </ul>
  </li>
  <li><strong>Save</strong>:
    <ul>
      <li>Enter filename, tick <em>Overwrite</em> if needed, then click <em>Save</em>.</li>
      <li>Output Excel is written in <code>www/</code> with normalized sheets/columns.</li>
    </ul>
  </li>
</ol>

<hr>

<h2>🗺️ Map &amp; Icons</h2>
<ul>
  <li><strong>Flights</strong> ✈️ blue line, airplane marker.</li>
  <li><strong>Car</strong> 🚗 black dashed OSRM route, truck marker.</li>
  <li><strong>Train</strong> 🚆 green line, train marker.</li>
  <li><strong>Boat</strong> ⛴️ purple line, ship marker.</li>
  <li><strong>Visit</strong> 🏁 green circle + flag marker (stay in same city).</li>
  <li><strong>Hotels</strong> 🛏️ bed icon.</li>
  <li><strong>Interest</strong> ⭐ themed icons (tree, university, binoculars, cutlery, camera, etc.).</li>
</ul>
<p>Auto-zoom adjusts to fit all visible itinerary points.</p>

<hr>

<h2>✅ Data Best Practices</h2>
<ul>
  <li>Ensure <code>from_id</code> and <code>to_id</code> exist in <strong>Places</strong> as <code>place_id</code>.</li>
  <li>Fill <code>lat</code>/<code>lon</code> to avoid slow geocoding.</li>
  <li>Use consistent modes (<em>vol</em>, <em>cotxe</em>, <em>tren</em>, <em>vaixell</em>).</li>
  <li>Use clean date formats; for ranges: <code>12 Feb – 14 Feb</code>.</li>
  <li>Synonyms across Catalan/Spanish/English are supported; consistency is recommended.</li>
</ul>

<hr>

<h2>🌐 Internet Dependencies</h2>
<ul>
  <li><strong>Map tiles</strong> (OpenStreetMap).</li>
  <li><strong>OSRM</strong> for car routes.</li>
  <li><strong>Geocoding</strong> for missing coordinates.</li>
  <li><strong>Wikipedia API</strong> for missing links.</li>
</ul>
<p>Without Internet, the app runs but these features may be limited.</p>

<hr>

<h2>🛠️ Troubleshooting</h2>
<ul>
  <li><strong>&ldquo;Places without coordinates&rdquo;</strong>: add <code>lat</code>/<code>lon</code> or use Autocomplete.</li>
  <li>Car routes not showing: check coordinates and OSRM availability.</li>
  <li>Hotels missing markers: fill <code>hotel_lat</code>/<code>hotel_lon</code> or provide enough info for geocoding.</li>
  <li>Cannot save: file exists in <code>www/</code>; tick <em>Overwrite</em> or rename.</li>
  <li>Empty calendar: no stages defined for the selected date.</li>
</ul>

<hr>

<h2>🔒 Privacy</h2>
<ul>
  <li>Excel remains local.</li>
  <li>External requests only happen for maps, routing, geocoding, and Wikipedia.</li>
  <li>Avoid sensitive data in Hotels/Interes.</li>
</ul>

<hr>

<h2>🙌 Credits</h2>
<ul>
  <li><strong>OpenStreetMap</strong> for basemaps.</li>
  <li><strong>OSRM</strong> for routing.</li>
  <li><strong>R/Shiny community</strong> and package developers.</li>
  <li>Assistants: ChatGPT, Copilot.</li>
</ul>

<hr>

<h2>📄 Copyright</h2>
<p>Copyright (c) 2025 — JFP</p>

<hr>

<h2>📬 Support</h2>
<p>If you encounter issues or have suggestions, open an <strong>issue</strong> in the repository or contact the maintainer.</p>
<hr>

<h2>📄 User guide</h2>
<br>- 🇬🇧 [Download User Guide (EN)](https://github.com/jfibla/Organitzador_viatges/raw/main/Travel_Itinerary_App_UserGuide_EN.pdf)
<br>-  <img width="20" height="20" alt="image" src="https://github.com/user-attachments/assets/0bbcc085-e250-4190-b7fa-45d1642cf4b2" />
  [Descarregar Guia d'Usuari (CA)](https://github.com/jfibla/Organitzador_viatges/raw/main/Travel_Itinerary_App_UserGuide_CA.pdf)
<br>- 🇪🇸 [Descargar Guía de Usuario (ES)](https://github.com/jfibla/Organitzador_viatges/main/Travel_Itinerary_App_UserGuide_ES.pdf)

## License

This project is licensed under the [CC BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0) license.


