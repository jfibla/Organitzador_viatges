# Geolocalitzador d’Itinerari de Viatge (mode “process”)

Aplicació **Shiny** per visualitzar itineraris de viatge des d’un fitxer **Excel**. Mostra etapes (vols, cotxe, tren, vaixell i visites) sobre un mapa, afegeix hotels i punts d’interès, permet filtres per dia/tipus i, opcionalment, edició en taula i **desat** a un nou Excel normalitzat a `www/`.

---

## ✨ Funcionalitats clau

- **Lectura d’Excel** amb 2–4 fulls: `Places` i `Stages` (obligatoris), `Hotels` i `Interes` (opcionals).
- **Mapa interactiu** amb marcadors i línies: vols/tren/vaixell (línia recta) i **rutes de carretera OSRM** per a cotxe.
- **Calendari** per filtrar per dia; commutador per veure **totes les etapes**.
- **Capes d’Hotels i Interès** amb *popups* i agrupació per clústers.
- **Edició opcional** de taules (Places, Stages, Hotels, Interes) i **exportació** a `www/` en format canònic.
- **Autocompletat** opcional d’enllaços a Wikipedia i **geocodificació** quan falten coordenades (requereix Internet).

---

## 🧰 Requisits

- **R** (i preferiblement **RStudio**).
- Paquets: `shiny`, `shinyWidgets`, `leaflet`, `leaflet.extras`, `dplyr`, `DT`, `osrm`, `sf`, `readxl`, `writexl`, `tidyr`, `lubridate`, `rlang`, `httr2`, `tidygeocoder`, `bslib`, `waiter`.
- Un fitxer `app.R` i una carpeta **`www/`** (on posaràs els teus `.xlsx`).

Instal·lació de paquets (exemple):

```r
install.packages(c(
  "shiny","shinyWidgets","leaflet","leaflet.extras","dplyr","DT",
  "osrm","sf","readxl","writexl","tidyr","lubridate","rlang",
  "httr2","tidygeocoder","bslib","waiter"
))
```

Arrencada de l’app:

```r
shiny::runApp()
```

---

## 🧾 Estructura de l’Excel

### Fulls acceptats

- **Places** *(obligatori)* — sinònims: `llocs`, `lugares`, `places`.
- **Stages** *(obligatori)* — sinònims: `etapes`, `etapas`, `stages`.
- **Hotels** *(opcional)* — sinònims: `hotels`, `hoteles`.
- **Interes** *(opcional)* — sinònims: `interes`, `interés`, `interests`, `interest`, `interesos`.

> Pots usar qualsevol d’aquests noms en català/castellà/anglès; l’app els identifica automàticament.

### Columnes recomanades

**Places**
- `place_id` *(o `lloc_id`/`lugar_id`)* — identificador únic del lloc/ciutat.
- `city` *(o `ciutat`/`ciudad`)* — ciutat.
- `country` *(o `pais`/`país`)* — país.
- `lat`, `lon` — coordenades (si falten, l’app pot geocodificar).
- `wikipedia` *(opcional)* — URL.

**Stages (Etapes)**
- `etapa` *(o `stage`)* — número o codi d’etapa.
- `from_id` *(o `de_id`)* — `place_id` d’origen.
- `to_id` — `place_id` de destí.
- `date` *(o `data`/`fecha`)* — data o rang (p. ex. `12 feb - 14 feb`).
- `description` *(opcional; `descripció`/`descripción`)*.
- `medi` *(o `mode`/`modo`/`medio`)* — *vol*, *cotxe*, *tren*, *vaixell*. Si `from_id == to_id`, es considera **visita**.
- `ruta` *(opcional; `route`/`trayecto`/`trajeto`)* — anotacions de l’etapa.

**Hotels (opcional)**
- `place_id` — ciutat associada.
- `hotel_name`, `hotel_link`, `details` *(sinònims equivalents admesos)*.
- `hotel_lat`, `hotel_lon` — coordenades (si falten, l’app pot geocodificar).

**Interes (opcional)**
- `descriptor` — nom del punt d’interès.
- `city`/`ciudad`/`ciutat`, `country`/`pais`/`país`.
- `lat`, `lon` — coordenades (si falten, geocodificació opcional).
- `tipus` *(o `type`/`tipo`)* — categoria (*museu*, *monument*, *natura*…).
- `link` *(opcional)*, `observacio`/`details`/`notes` *(opcional)*.

### Dates admeses

- `YYYY-MM-DD` o formats com `12 feb`, `12 feb 2025`, `12 feb - 14 feb`.
- Si manca l’any, s’assumeix l’any del viatge configurat al codi (per defecte **2025**).

---

## 🚀 Flux d’ús bàsic

1. **Carrega l’Excel**
   - **Des de disc:** a la barra lateral, “Des de disc local (.xlsx)”.
   - **Des de `www/`:** clica **Actualitza**, tria de la llista i prem **Carrega**.
2. **Selecciona la data** al calendari de la pestanya **Mapa**.
   - Activa **“Mostra totes les etapes”** per veure l’itinerari complet.
3. **Filtra per tipus**: Itinerari / Vols / Cotxe / Tren / Vaixell / Visita.
4. **Explora el mapa**: marcadors per destinació i línies per trajectes.
   - **Cotxe**: prova de traçat per **OSRM** (pot requerir Internet i pot no estar disponible sempre).
   - **Hotels** i **Interès**: capes activables (amb *popups* i clústers).
5. **Consulta els panells**
   - **Detalls de l’etapa**: orígens/destins del dia, mode, descripcions.
   - **Hotel**: llistes d’hotels associats a l’etapa (prioritza destí; si no, origen).
   - **Llocs d’interès**: per la ciutat del dia, amb enllaços si n’hi ha.
   - **Ruta**: mostra el text d’anotacions si has omplert la columna `ruta`.
6. **(Opcional) Edita**
   - Marca **“Permetre edició”** per canviar cel·les a Places/Stages/Hotels/Interes.
7. **Desa a Excel**
   - A “Desar canvis”, posa nom (sense extensió), indica si vols **sobreescriure** i prem **“Desa en memòria i a www/”**.
   - Es genera un Excel **normalitzat** a `www/` amb fulls i columnes canòniques.

---

## 🗺️ Detalls del mapa i icones

- **Vols** ✈️ — línia recta, icona d’avió.
- **Trens** 🚆 — línia recta, icona de tren.
- **Vaixells** ⛴️ — línia recta, icona de vaixell.
- **Cotxe** 🚗 — ruta OSRM (línia discontínua negra), icona de vehicle.
- **Visita** 🏁 — cercles verds i icona de bandera per estades a la mateixa ciutat.
- **Hotels** 🛏️ — icona de llit; capa “Hotels”.
- **Interès** ⭐ — icones temàtiques (arbre, universitat, binoculars, coberts, càmera, etc.).

> **Auto-zoom**: el mapa encaixa tots els punts de l’itinerari visible.

---

## ✅ Bones pràctiques de dades

- **IDs consistents**: `from_id`/`to_id` han d’existir a **Places** com a `place_id`.
- **Coordenades**: omple `lat`/`lon` (o `hotel_lat`/`hotel_lon`) per evitar geocodificacions lentes.
- **Modes normals**: usa *vol*, *cotxe*, *tren*, *vaixell* (o sinònims admesos). Si `from_id == to_id`, el mode es considera **visita**.
- **Dates netes**: evita formats amb abreviatures inusuals; per a rangs usa `12 feb - 14 feb`.
- **Sinònims**: pots barrejar idiomes (ca/es/en), però mantén coherència dins de cada full.

---

## 🧩 Dependències d’Internet

- **Mapes** (OpenStreetMap/tiles) i **cerca** al mapa.
- **OSRM** per a rutes de carretera (cotxe).
- **Geocodificació** quan falten coordenades.
- **Wikipedia** per completar enllaços (opcional).

Sense Internet, l’app obrirà però aquestes funcions poden quedar limitades.

---

## 🛠️ Problemes freqüents

- **“Hi ha llocs sense coordenades”**: afegeix `lat`/`lon` a **Places** o deixa que la geocodificació treballi (cal Internet).
- **Rutes de cotxe no surten**: comprova coordenades a orígens/destins i disponibilitat del servei OSRM.
- **Hotels sense marcador**: cal `hotel_lat`/`hotel_lon` o bé prou context per geocodificar l’hotel.
- **No deixa desar**: si el nom ja existeix a `www/`, activa **Sobreescriure** o canvia el nom.
- **Calendari fora de rang**: revisa el camp `date` de **Stages**; pot no haver-hi cap etapa aquell dia.

---

## 🔒 Privadesa

- L’Excel es manté **local**. Les crides externes (mapes, OSRM, geocodificació, Wikipedia) només s’usen si actives/visualitzes funcionalitats que les requereixen.
- Evita dades sensibles. Per a **Hotels** i **Interes**, guarda només el necessari (nom, enllaç, ciutat, país, coordenades).

---

## 🙌 Agraïments

- **OpenStreetMap** per les capes de mapa.
- **OSRM** per al càlcul de rutes.
- Comunitat **R/Shiny** i ecosistema de paquets utilitzats.
- ChatGPT
- Microsoft Copilot

---

## 📄 Copyright (c) 2025 — JFP

---

## 📬 Suport

Si trobes cap problema o vols proposar millores, obre un **issue** al teu repositori o contacta amb l’equip mantenidor.

