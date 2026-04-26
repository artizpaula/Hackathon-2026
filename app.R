# FlyMood — Mood-Driven Travel App

Sys.setenv(GROQ_API_KEY       = "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")
Sys.setenv(SKYSCANNER_API_KEY = "sh782613596881417389290430162312")

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)
library(stringr)
library(httr)
library(jsonlite)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# =============================================================================
# QUIZ QUESTIONS
# =============================================================================

quiz_preguntas <- data.frame(
  id = 1:5,
  pregunta = c(
    "What brought you to think about travelling?",
    "What have you been missing recently?",
    "How are you feeling about money this month?",
    "When you close your eyes and imagine yourself finally free, how long does that feeling last?",
    "What does your body need most right now?"
  ),
  opciones = c(
    "I need to escape,I want to celebrate,I don't know I just need to get out,I want to try something new",
    "Space and silence just for me,Laughs and memories to share with people",
    "I can afford it I want to have fun,I have some money saved for this,Just the necessary I don't want to go all out,I'm in a budget but I need to get out",
    "A few days — just enough to breathe,A full week — enough to actually disconnect,Two weeks or more — I need to truly disappear",
    "To slow down and do nothing for a while,To feel alive — movement, Adventure, Energy,To connect — food, People, Streets, Stories,To lose myself in beauty — art, History, Wonder,To dance, Stay up late and forget about everything"
  ),
  stringsAsFactors = FALSE
)

# =============================================================================
# AIRPORTS
# =============================================================================

aeropuertos_mundo <- list(
  Spain = data.frame(
    ciudad = c("Madrid","Barcelona","Valencia","Seville","Bilbao","Málaga","Palma de Mallorca","Alicante","Granada","Santiago","Ibiza","Las Palmas"),
    codigo = c("MAD","BCN","VLC","SVQ","BIO","AGP","PMI","ALC","GRX","SCQ","IBZ","LPA"),
    lat = c(40.4168,41.3851,39.4699,37.3891,43.2630,36.7213,39.5696,38.3452,37.1773,42.8782,38.9088,28.1300),
    lon = c(-3.7038,2.1734,-0.3763,-5.9845,-2.9350,-4.4214,2.6502,-0.4830,-3.5986,-8.5448,1.4328,-15.4300),
    stringsAsFactors = FALSE),
  France = data.frame(
    ciudad = c("Paris CDG","Paris Orly","Nice","Lyon","Marseille","Toulouse","Bordeaux"),
    codigo = c("CDG","ORY","NCE","LYS","MRS","TLS","BOD"),
    lat = c(49.0097,48.7253,43.6653,45.7264,43.4390,43.6351,44.8283),
    lon = c(2.5479,2.3594,7.2150,5.0901,5.2211,1.3678,-0.7156),
    stringsAsFactors = FALSE),
  Italy = data.frame(
    ciudad = c("Rome FCO","Milan MXP","Venice","Naples","Bologna","Florence","Palermo"),
    codigo = c("FCO","MXP","VCE","NAP","BLQ","FLR","PMO"),
    lat = c(41.8003,45.6306,45.5053,40.8860,44.5354,43.8100,38.1759),
    lon = c(12.2389,8.7281,12.3519,14.2914,11.2887,11.2020,13.0910),
    stringsAsFactors = FALSE),
  Germany = data.frame(
    ciudad = c("Frankfurt","Munich","Berlin","Hamburg","Düsseldorf","Cologne","Stuttgart"),
    codigo = c("FRA","MUC","BER","HAM","DUS","CGN","STR"),
    lat = c(50.0379,48.3538,52.3667,53.6304,51.2800,50.8659,48.6899),
    lon = c(8.5622,11.7861,13.5033,9.9882,6.7578,7.1427,9.2220),
    stringsAsFactors = FALSE),
  UK = data.frame(
    ciudad = c("London Heathrow","London Gatwick","London Stansted","Manchester","Edinburgh","Birmingham","Glasgow"),
    codigo = c("LHR","LGW","STN","MAN","EDI","BHX","GLA"),
    lat = c(51.4700,51.1537,51.8860,53.3537,55.9500,52.4539,55.8719),
    lon = c(-0.4543,-0.1821,0.2389,-2.2750,-3.3725,-1.7480,-4.4330),
    stringsAsFactors = FALSE),
  USA = data.frame(
    ciudad = c("New York JFK","New York Newark","Los Angeles","Chicago","Miami","San Francisco","Dallas","Atlanta","Denver","Seattle","Boston","Las Vegas"),
    codigo = c("JFK","EWR","LAX","ORD","MIA","SFO","DFW","ATL","DEN","SEA","BOS","LAS"),
    lat = c(40.6413,40.6895,33.9416,41.9742,25.7932,37.6213,32.8998,33.6407,39.8494,47.4502,42.3656,36.0840),
    lon = c(-73.7781,-74.1745,-118.4085,-87.9073,-80.2906,-122.3790,-97.0403,-84.4277,-104.6738,-122.3088,-71.0096,-115.1537),
    stringsAsFactors = FALSE),
  Portugal = data.frame(
    ciudad = c("Lisbon","Porto","Faro","Madeira","Azores"),
    codigo = c("LIS","OPO","FAO","FNC","PDL"),
    lat = c(38.7742,41.2355,37.0145,32.6942,37.7412),
    lon = c(-9.1342,-8.6780,-7.9660,-16.7720,-25.6979),
    stringsAsFactors = FALSE),
  Netherlands = data.frame(
    ciudad = c("Amsterdam","Eindhoven","Rotterdam"),
    codigo = c("AMS","EIN","RTM"),
    lat = c(52.3105,51.4501,51.9569),
    lon = c(4.7683,5.3750,4.4393),
    stringsAsFactors = FALSE),
  UAE = data.frame(
    ciudad = c("Dubai","Abu Dhabi","Sharjah"),
    codigo = c("DXB","AUH","SHJ"),
    lat = c(25.2532,24.4330,25.3286),
    lon = c(55.3657,54.6511,55.5136),
    stringsAsFactors = FALSE),
  Japan = data.frame(
    ciudad = c("Tokyo Narita","Tokyo Haneda","Osaka","Fukuoka","Sapporo"),
    codigo = c("NRT","HND","KIX","FUK","CTS"),
    lat = c(35.7647,35.5494,34.4347,33.5858,42.7752),
    lon = c(140.3864,139.7798,135.2440,130.4511,141.6922),
    stringsAsFactors = FALSE),
  Singapore = data.frame(
    ciudad = c("Singapore Changi"),
    codigo = c("SIN"),
    lat = c(1.3644),
    lon = c(103.9915),
    stringsAsFactors = FALSE),
  Thailand = data.frame(
    ciudad = c("Bangkok Suvarnabhumi","Bangkok Don Mueang","Phuket","Chiang Mai"),
    codigo = c("BKK","DMK","HKT","CNX"),
    lat = c(13.6900,13.9126,8.1132,18.7669),
    lon = c(100.7501,100.6067,98.3169,98.9626),
    stringsAsFactors = FALSE),
  Australia = data.frame(
    ciudad = c("Sydney","Melbourne","Brisbane","Perth","Adelaide"),
    codigo = c("SYD","MEL","BNE","PER","ADL"),
    lat = c(-33.9399,-37.6731,-27.3842,-31.9405,-34.9452),
    lon = c(151.1753,144.8438,153.1175,115.9670,138.5308),
    stringsAsFactors = FALSE),
  Canada = data.frame(
    ciudad = c("Toronto Pearson","Vancouver","Montreal","Calgary"),
    codigo = c("YYZ","YVR","YUL","YYC"),
    lat = c(43.6777,49.1940,45.4706,51.1314),
    lon = c(-79.6248,-123.1838,-73.7408,-114.0123),
    stringsAsFactors = FALSE),
  Morocco = data.frame(
    ciudad = c("Casablanca","Marrakech","Fez","Rabat","Tangier"),
    codigo = c("CMN","RAK","FEZ","RBA","TNG"),
    lat = c(33.3675,31.6069,33.9275,34.0509,35.7269),
    lon = c(-7.5897,-8.0363,-4.9776,-6.7518,-5.9169),
    stringsAsFactors = FALSE),
  Turkey = data.frame(
    ciudad = c("Istanbul IST","Istanbul SAW","Ankara","Antalya","Izmir"),
    codigo = c("IST","SAW","ESB","AYT","ADB"),
    lat = c(41.2753,40.8986,40.1281,36.8987,38.2924),
    lon = c(28.7519,29.3092,32.9951,30.7997,27.1570),
    stringsAsFactors = FALSE),
  Greece = data.frame(
    ciudad = c("Athens","Thessaloniki","Heraklion","Rhodes","Santorini","Mykonos"),
    codigo = c("ATH","SKG","HER","RHO","JTR","JMK"),
    lat = c(37.9364,40.5197,35.3397,36.4054,36.3995,37.4355),
    lon = c(23.9445,22.9709,25.1793,28.0862,25.4799,25.3481),
    stringsAsFactors = FALSE),
  Switzerland = data.frame(
    ciudad = c("Zurich","Geneva","Basel"),
    codigo = c("ZRH","GVA","BSL"),
    lat = c(47.4647,46.2384,47.5900),
    lon = c(8.5492,6.1090,7.5291),
    stringsAsFactors = FALSE),
  Austria = data.frame(
    ciudad = c("Vienna","Salzburg","Innsbruck"),
    codigo = c("VIE","SZG","INN"),
    lat = c(48.1103,47.7947,47.2602),
    lon = c(16.5697,13.0034,11.3443),
    stringsAsFactors = FALSE),
  Belgium = data.frame(
    ciudad = c("Brussels","Charleroi","Antwerp"),
    codigo = c("BRU","CRL","ANR"),
    lat = c(50.9014,50.4642,51.1894),
    lon = c(4.4844,4.4697,4.4603),
    stringsAsFactors = FALSE),
  India = data.frame(
    ciudad = c("Delhi","Mumbai","Bangalore","Chennai","Goa"),
    codigo = c("DEL","BOM","BLR","MAA","GOI"),
    lat = c(28.5562,19.0896,13.1979,12.9941,15.3809),
    lon = c(77.1000,72.8679,77.7063,80.1709,73.8314),
    stringsAsFactors = FALSE),
  Egypt = data.frame(
    ciudad = c("Cairo","Sharm El Sheikh","Hurghada"),
    codigo = c("CAI","SSH","HRG"),
    lat = c(30.1219,27.9773,27.1783),
    lon = c(31.4056,34.3950,33.7994),
    stringsAsFactors = FALSE),
  SouthAfrica = data.frame(
    ciudad = c("Johannesburg","Cape Town","Durban"),
    codigo = c("JNB","CPT","DUR"),
    lat = c(-26.1367,-33.9648,-29.6144),
    lon = c(28.2460,18.6017,30.9502),
    stringsAsFactors = FALSE)
)

paises_disponibles <- c(sort(names(aeropuertos_mundo)), "Others")

# =============================================================================
# MOOD → DESTINATION MAPPING
# Cada combinación de respuestas del quiz tiene destinos prioritarios distintos.
# Los destinos se pasan a la API de Skyscanner EN ESE ORDEN, de modo que
# los primeros son los más relevantes emocionalmente para el usuario.
# =============================================================================

get_mood_destinations <- function(user_profile) {
  
  interests  <- tolower(user_profile$interests   %||% "")
  why        <- tolower(user_profile$why_travel  %||% "")
  missing    <- tolower(user_profile$missing     %||% "")
  
  # ── Nightlife / Party ────────────────────────────────────────────────────
  if (grepl("dance|stay up late|forget about everything", interests)) {
    return(c("IBZ","BER","AMS","DUB","CDG","BCN","LIS","ATH","BUD","PRG",
             "MXP","NAP","WAW","LGW","BRU","VIE","MAN","LHR","CPH","ARN"))
  }
  
  # ── Nature / Outdoors / Breathe ───────────────────────────────────────────
  if (grepl("breathe|mountain|ocean|open air|nature", interests)) {
    return(c("KEF","TRD","OSL","BGO","FAO","FNC","PDL","ALC","PMI","AGP",
             "HER","SCQ","OPO","CPH","ARN","GVA","INN","SZG","ZRH","EDI"))
  }
  
  # ── Rest / Slow down ─────────────────────────────────────────────────────
  if (grepl("slow down|do nothing|rest", interests)) {
    return(c("FNC","PDL","FAO","LIS","OPO","PMI","ALC","NCE","GVA","ATH",
             "JTR","RHO","HER","MRS","TLS","BOD","SCQ","LPA","SZG","KEF"))
  }
  
  # ── Culture / Art / History ───────────────────────────────────────────────
  if (grepl("beauty|art|history|wonder|culture", interests)) {
    return(c("FCO","ATH","VIE","IST","CDG","FLR","VCE","PRG","BUD","LIS",
             "MXP","BRU","ZRH","GVA","EDI","WAW","HEL","ARN","MRS","NAP"))
  }
  
  # ── Connection / Food / People ────────────────────────────────────────────
  if (grepl("connect|food|people|streets|stories", interests)) {
    return(c("FCO","CDG","IST","ATH","LIS","AMS","MXP","NAP","MRS","BRU",
             "BCN","RAK","DUB","HAM","STN","BER","LHR","TLS","VCE","FLR"))
  }
  
  # ── Celebrate / Adventure / Energy ───────────────────────────────────────
  if (grepl("alive|movement|adventure|energy|celebrat", paste(interests, why))) {
    return(c("DXB","BKK","HKT","RAK","IST","ATH","DUB","AMS","BER","IBZ",
             "LAS","MIA","NAP","LIS","CDG","AYT","HER","JMK","MAN","LHR"))
  }
  
  # ── Escape / default ─────────────────────────────────────────────────────
  return(c("LIS","FCO","BER","ATH","DUB","RAK","IST","CDG","AMS","PRG",
           "BCN","MXP","BRU","VIE","BUD","CPH","ARN","WAW","HEL","KEF"))
}

# =============================================================================
# SKYSCANNER BROWSE QUOTES — PRECIOS REALES VIA RAPIDAPI
# Usa el endpoint oficial de Browse Quotes con la clave de la app.
# Devuelve precio, aerolínea y URL de reserva exactos de Skyscanner.
# =============================================================================

# Metadatos de destinos para enriquecer la respuesta de la API
dest_meta <- list(
  LIS = list(destino="Lisbon",      pais="Portugal",        lat=38.7223, lon=-9.1393),
  FCO = list(destino="Rome",        pais="Italy",           lat=41.9028, lon=12.4964),
  BER = list(destino="Berlin",      pais="Germany",         lat=52.5200, lon=13.4050),
  ATH = list(destino="Athens",      pais="Greece",          lat=37.9838, lon=23.7275),
  DUB = list(destino="Dublin",      pais="Ireland",         lat=53.3498, lon=-6.2603),
  RAK = list(destino="Marrakech",   pais="Morocco",         lat=31.6295, lon=-7.9811),
  IST = list(destino="Istanbul",    pais="Turkey",          lat=41.0082, lon=28.9784),
  CDG = list(destino="Paris",       pais="France",          lat=48.8566, lon=2.3522),
  AMS = list(destino="Amsterdam",   pais="Netherlands",     lat=52.3676, lon=4.9041),
  PRG = list(destino="Prague",      pais="Czech Republic",  lat=50.0755, lon=14.4378),
  BCN = list(destino="Barcelona",   pais="Spain",           lat=41.3851, lon=2.1734),
  MXP = list(destino="Milan",       pais="Italy",           lat=45.4642, lon=9.1900),
  BRU = list(destino="Brussels",    pais="Belgium",         lat=50.8503, lon=4.3517),
  VIE = list(destino="Vienna",      pais="Austria",         lat=48.2082, lon=16.3738),
  BUD = list(destino="Budapest",    pais="Hungary",         lat=47.4979, lon=19.0402),
  CPH = list(destino="Copenhagen",  pais="Denmark",         lat=55.6761, lon=12.5683),
  ARN = list(destino="Stockholm",   pais="Sweden",          lat=59.3293, lon=18.0686),
  WAW = list(destino="Warsaw",      pais="Poland",          lat=52.2297, lon=21.0122),
  HEL = list(destino="Helsinki",    pais="Finland",         lat=60.1699, lon=24.9384),
  KEF = list(destino="Reykjavik",   pais="Iceland",         lat=64.1466, lon=-21.9426),
  IBZ = list(destino="Ibiza",       pais="Spain",           lat=38.9088, lon=1.4328),
  DXB = list(destino="Dubai",       pais="UAE",             lat=25.2048, lon=55.2708),
  BKK = list(destino="Bangkok",     pais="Thailand",        lat=13.7563, lon=100.5018),
  HKT = list(destino="Phuket",      pais="Thailand",        lat=7.8804,  lon=98.3923),
  AYT = list(destino="Antalya",     pais="Turkey",          lat=36.8969, lon=30.7133),
  FNC = list(destino="Madeira",     pais="Portugal",        lat=32.6669, lon=-16.9241),
  PDL = list(destino="Azores",      pais="Portugal",        lat=37.7412, lon=-25.6979),
  FAO = list(destino="Faro",        pais="Portugal",        lat=37.0194, lon=-7.9660),
  OPO = list(destino="Porto",       pais="Portugal",        lat=41.1579, lon=-8.6291),
  PMI = list(destino="Palma",       pais="Spain",           lat=39.5696, lon=2.6502),
  ALC = list(destino="Alicante",    pais="Spain",           lat=38.3452, lon=-0.4830),
  AGP = list(destino="Malaga",      pais="Spain",           lat=36.7213, lon=-4.4214),
  NCE = list(destino="Nice",        pais="France",          lat=43.7102, lon=7.2620),
  JTR = list(destino="Santorini",   pais="Greece",          lat=36.3995, lon=25.4799),
  JMK = list(destino="Mykonos",     pais="Greece",          lat=37.4355, lon=25.3481),
  HER = list(destino="Heraklion",   pais="Greece",          lat=35.3397, lon=25.1793),
  RHO = list(destino="Rhodes",      pais="Greece",          lat=36.4054, lon=28.0862),
  GVA = list(destino="Geneva",      pais="Switzerland",     lat=46.2044, lon=6.1432),
  ZRH = list(destino="Zurich",      pais="Switzerland",     lat=47.3769, lon=8.5417),
  INN = list(destino="Innsbruck",   pais="Austria",         lat=47.2692, lon=11.4041),
  SZG = list(destino="Salzburg",    pais="Austria",         lat=47.7981, lon=13.0462),
  EDI = list(destino="Edinburgh",   pais="UK",              lat=55.9533, lon=-3.1883),
  FCR = list(destino="Florence",    pais="Italy",           lat=43.7696, lon=11.2558),
  FLR = list(destino="Florence",    pais="Italy",           lat=43.8100, lon=11.2020),
  VCE = list(destino="Venice",      pais="Italy",           lat=45.4408, lon=12.3155),
  NAP = list(destino="Naples",      pais="Italy",           lat=40.8522, lon=14.2681),
  MRS = list(destino="Marseille",   pais="France",          lat=43.2965, lon=5.3698),
  TLS = list(destino="Toulouse",    pais="France",          lat=43.6047, lon=1.4442),
  BOD = list(destino="Bordeaux",    pais="France",          lat=44.8378, lon=-0.5792),
  SCQ = list(destino="Santiago",    pais="Spain",           lat=42.8782, lon=-8.5448),
  LPA = list(destino="Gran Canaria",pais="Spain",           lat=28.1300, lon=-15.4300),
  MAN = list(destino="Manchester",  pais="UK",              lat=53.4808, lon=-2.2426),
  LHR = list(destino="London",      pais="UK",              lat=51.5074, lon=-0.1278),
  LGW = list(destino="London Gatwick",pais="UK",            lat=51.1537, lon=-0.1821),
  STN = list(destino="London Stansted",pais="UK",           lat=51.8860, lon=0.2389),
  HAM = list(destino="Hamburg",     pais="Germany",         lat=53.5753, lon=10.0153),
  MIA = list(destino="Miami",       pais="USA",             lat=25.7617, lon=-80.1918),
  LAS = list(destino="Las Vegas",   pais="USA",             lat=36.1699, lon=-115.1398),
  BGO = list(destino="Bergen",      pais="Norway",          lat=60.3913, lon=5.3221),
  TRD = list(destino="Trondheim",   pais="Norway",          lat=63.4308, lon=10.3951),
  OSL = list(destino="Oslo",        pais="Norway",          lat=59.9139, lon=10.7522),
  WAW = list(destino="Warsaw",      pais="Poland",          lat=52.2297, lon=21.0122)
)

# Aerolíneas predominantes por ruta (para el fallback)
route_airlines <- list(
  IBZ=c("Vueling","Ryanair","EasyJet"),   BER=c("Ryanair","EasyJet","Lufthansa"),
  AMS=c("KLM","Transavia","Vueling"),      DUB=c("Ryanair","Aer Lingus"),
  CDG=c("Air France","Vueling","Iberia"),  LIS=c("TAP","Vueling","Ryanair"),
  FCO=c("Vueling","Iberia","ITA"),         ATH=c("Aegean","Ryanair","Vueling"),
  BUD=c("Wizz Air","Ryanair"),             PRG=c("Ryanair","Vueling","Czech Airlines"),
  VIE=c("Austrian","Vueling","Ryanair"),   IST=c("Turkish","Vueling","Pegasus"),
  RAK=c("Ryanair","Air Arabia","Vueling"), MXP=c("EasyJet","Ryanair","Vueling"),
  BRU=c("Brussels Airlines","Ryanair"),    CPH=c("SAS","Vueling","Norwegian"),
  ARN=c("SAS","Norwegian","Ryanair"),      WAW=c("LOT","Wizz Air","Ryanair"),
  HEL=c("Finnair","Norwegian"),            KEF=c("Icelandair","EasyJet"),
  DXB=c("Emirates","flydubai","Vueling"),  BKK=c("Thai","Qatar","Etihad"),
  HKT=c("Thai","AirAsia","Qatar"),         PMI=c("Vueling","Ryanair","TUI"),
  FNC=c("TAP","Ryanair","Vueling"),        FAO=c("Ryanair","Vueling","TAP"),
  OPO=c("TAP","Ryanair","EasyJet"),        JTR=c("Aegean","Ryanair"),
  JMK=c("Aegean","Olympic Air"),           GVA=c("EasyJet","Swiss","Vueling"),
  ZRH=c("Swiss","EasyJet","Vueling"),      NCE=c("EasyJet","Air France","Vueling"),
  HER=c("Ryanair","EasyJet","Aegean"),     AGP=c("Ryanair","EasyJet","Vueling"),
  ALC=c("Ryanair","EasyJet","Vueling"),    LPA=c("Ryanair","Vueling","Binter"),
  EDI=c("Ryanair","EasyJet","Vueling"),    AYT=c("Pegasus","Turkish","Corendon")
)

get_airline_for_dest <- function(code) {
  airlines <- route_airlines[[code]]
  if (!is.null(airlines) && length(airlines) > 0) return(airlines[1])
  return("Low-cost carrier")
}

# =============================================================================
# SKYSCANNER API — LLAMADA REAL (RapidAPI Browse Quotes)
# =============================================================================

fetch_skyscanner_quote <- function(origin, dest_code, outbound_date, sky_key) {
  url <- sprintf(
    "https://skyscanner-skyscanner-flight-search-v1.p.rapidapi.com/apiservices/browsequotes/v1.0/ES/EUR/en-US/%s-sky/%s-sky/%s",
    origin, dest_code, outbound_date
  )
  resp <- tryCatch(
    httr::GET(
      url,
      httr::add_headers(
        "X-RapidAPI-Key"  = sky_key,
        "X-RapidAPI-Host" = "skyscanner-skyscanner-flight-search-v1.p.rapidapi.com"
      ),
      httr::timeout(6)
    ),
    error = function(e) NULL
  )
  if (is.null(resp) || resp$status_code != 200) return(NULL)
  
  tryCatch({
    data <- jsonlite::fromJSON(
      httr::content(resp, "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )
    
    if (is.null(data$Quotes) || length(data$Quotes) == 0) return(NULL)
    
    # Precio mínimo de la primera quote
    price <- data$Quotes[[1]]$MinPrice
    if (is.null(price)) return(NULL)
    
    # Intentar obtener nombre de aerolínea desde Carriers
    carrier_id   <- data$Quotes[[1]]$OutboundLeg$CarrierIds[[1]]
    carrier_name <- tryCatch({
      carriers <- data$Carriers
      match    <- Filter(function(c) c$CarrierId == carrier_id, carriers)
      if (length(match) > 0) match[[1]]$Name else get_airline_for_dest(dest_code)
    }, error = function(e) get_airline_for_dest(dest_code))
    
    list(price = as.numeric(price), airline = carrier_name)
    
  }, error = function(e) NULL)
}


get_skyscanner_live_prices <- function(origin, date_from, date_to, budget_tier, user_profile) {
  
  sky_key <- Sys.getenv("SKYSCANNER_API_KEY")
  
  # Presupuesto máximo según perfil
  max_price <- switch(budget_tier,
                      "I can afford it I want to have fun"            = 800,
                      "I have some money saved for this"              = 400,
                      "Just the necessary I don't want to go all out" = 250,
                      "I'm in a budget but I need to get out"         = 150,
                      400)
  
  outbound_date <- format(as.Date(date_from), "%Y-%m")
  
  # Destinos ordenados por mood del usuario
  mood_dests <- get_mood_destinations(user_profile)
  
  all_results <- list()
  
  # Llamadas a la API en orden de prioridad emocional
  for (dest_code in mood_dests) {
    Sys.sleep(0.18) # Respetar rate limit de RapidAPI
    
    result <- fetch_skyscanner_quote(origin, dest_code, outbound_date, sky_key)
    
    if (!is.null(result) && result$price <= max_price) {
      meta <- dest_meta[[dest_code]]
      if (!is.null(meta)) {
        all_results[[length(all_results) + 1]] <- data.frame(
          destino        = meta$destino,
          pais           = meta$pais,
          precio         = result$price,
          aerolinea      = result$airline,
          duracion_horas = round(geosphere_dist_hours(origin, dest_code), 1),
          lat            = meta$lat,
          lon            = meta$lon,
          dest_iata      = dest_code,
          mood_priority  = which(mood_dests == dest_code),
          booking_url    = sprintf(
            "https://www.skyscanner.net/transport/flights/%s/%s/%s/%s/",
            tolower(origin), tolower(dest_code),
            format(as.Date(date_from), "%y%m%d"),
            format(as.Date(date_to),   "%y%m%d")
          ),
          stringsAsFactors = FALSE
        )
      }
    }
    
    # Parar cuando tengamos suficientes destinos relevantes
    if (length(all_results) >= 15) break
  }
  
  if (length(all_results) > 0) {
    df <- do.call(rbind, all_results)
    # Ordenar por prioridad emocional (mood_priority) y luego por precio
    df <- df %>% arrange(mood_priority, precio) %>% select(-mood_priority, -dest_iata)
    message(sprintf("Skyscanner API: %d rutas reales obtenidas (mood: %s)", nrow(df),
                    substr(user_profile$interests, 1, 30)))
    return(list(data = df, source = "live"))
  }
  
  # Fallback calibrado por temporada si la API no responde
  message("Fallback: precios calibrados por temporada.")
  df <- get_seasonally_adjusted_prices(origin, date_from, date_to, max_price, user_profile)
  return(list(data = df, source = "estimated"))
}

# Estimación simple de horas de vuelo basada en distancia geográfica aproximada
geosphere_dist_hours <- function(origin_code, dest_code) {
  # Tiempos medios desde BCN/MAD a destinos comunes (horas)
  flight_times <- c(
    LIS=1.5, FCO=2.5, BER=2.8, ATH=3.2, DUB=2.5, RAK=2.8, IST=3.5,
    CDG=2.0, AMS=2.2, PRG=2.5, BCN=1.2, MXP=1.8, BRU=2.1, VIE=2.7,
    BUD=2.5, CPH=2.5, ARN=3.0, WAW=2.0, HEL=3.5, KEF=4.0, IBZ=1.0,
    DXB=6.5, BKK=11.0,HKT=11.5,AYT=3.2, FNC=2.5, PDL=3.2, FAO=1.0,
    OPO=1.2, PMI=1.0, ALC=1.2, AGP=1.5, NCE=1.5, JTR=3.5, JMK=3.3,
    HER=3.0, RHO=3.2, GVA=1.8, ZRH=1.9, INN=1.7, SZG=1.8, EDI=2.3,
    FLR=2.2, VCE=2.0, NAP=2.5, MRS=1.6, TLS=1.5, BOD=1.7, SCQ=1.1,
    LPA=2.5, MAN=2.3, LHR=2.4, LGW=2.4, STN=2.4, HAM=2.5, MIA=10.0,
    LAS=11.0,BGO=3.2, TRD=3.5, OSL=3.0, MIA=10.0
  )
  ft <- flight_times[dest_code]
  if (is.null(ft) || is.na(ft)) return(2.5)
  return(as.numeric(ft))
}

# Precios calibrados por temporada (fallback) — también mood-ordered
get_seasonally_adjusted_prices <- function(origin, date_from, date_to, max_price, user_profile) {
  
  travel_month  <- as.integer(format(as.Date(date_from), "%m"))
  days_advance  <- as.numeric(as.Date(date_from) - Sys.Date())
  
  season_factor <- dplyr::case_when(
    travel_month %in% c(7, 8)    ~ 1.35,
    travel_month %in% c(12)      ~ 1.25,
    travel_month %in% c(3, 4, 5) ~ 1.10,
    travel_month %in% c(6, 9, 10)~ 1.05,
    TRUE                         ~ 0.85
  )
  advance_factor <- dplyr::case_when(
    days_advance < 7  ~ 1.45,
    days_advance < 14 ~ 1.25,
    days_advance < 30 ~ 1.10,
    days_advance < 60 ~ 1.00,
    TRUE              ~ 0.90
  )
  
  # Base de precios completa — mood priorities aplicadas después
  base_prices <- data.frame(
    dest_iata  = c("LIS","FCO","BER","ATH","DUB","RAK","IST","CDG","AMS","PRG",
                   "BCN","MXP","BRU","VIE","BUD","CPH","ARN","WAW","HEL","KEF",
                   "IBZ","PMI","FAO","FNC","OPO","ALC","AGP","LPA","JTR","HER",
                   "RHO","JMK","AYT","GVA","ZRH","INN","EDI","NAP","VCE","FLR"),
    precio_base= c(79,125,105,145,99,160,175,115,135,85,
                   55,110,120,155,100,130,150,110,180,230,
                   65,70,60,120,90,58,62,110,190,155,
                   160,195,135,145,160,120,130,115,140,125),
    stringsAsFactors = FALSE
  )
  
  mood_dests <- get_mood_destinations(user_profile)
  
  base_prices <- base_prices %>%
    mutate(
      mood_priority = match(dest_iata, mood_dests),
      mood_priority = ifelse(is.na(mood_priority), 99, mood_priority)
    )
  
  result <- base_prices %>%
    rowwise() %>%
    mutate(
      meta      = list(dest_meta[[dest_iata]]),
      destino   = if (!is.null(meta)) meta$destino else NA_character_,
      pais      = if (!is.null(meta)) meta$pais    else NA_character_,
      lat       = if (!is.null(meta)) meta$lat     else NA_real_,
      lon       = if (!is.null(meta)) meta$lon     else NA_real_,
      precio    = round(precio_base * season_factor * advance_factor * runif(1, 0.92, 1.08)),
      aerolinea = get_airline_for_dest(dest_iata),
      duracion_horas = geosphere_dist_hours("BCN", dest_iata),
      booking_url = sprintf(
        "https://www.skyscanner.net/transport/flights/%s/%s/%s/%s/",
        tolower(origin), tolower(dest_iata),
        format(as.Date(date_from), "%y%m%d"),
        format(as.Date(date_to),   "%y%m%d")
      )
    ) %>%
    ungroup() %>%
    filter(!is.na(destino), precio <= max_price) %>%
    arrange(mood_priority, precio) %>%
    select(destino, pais, precio, aerolinea, duracion_horas, lat, lon, booking_url)
  
  return(result)
}

# =============================================================================
# GROQ API — Análisis con contexto emocional completo
# =============================================================================

get_groq_analysis <- function(destinations, user_profile) {
  groq_key <- Sys.getenv("GROQ_API_KEY")
  if (groq_key == "") {
    message("No GROQ key. Using mock.")
    return(get_mock_groq_analysis(destinations, user_profile))
  }
  if (is.null(destinations) || nrow(destinations) == 0)
    return(data.frame(destination=character(), price=numeric(), reason=character(),
                      match_score=numeric(), booking_url=character(), stringsAsFactors=FALSE))
  
  perfil_texto <- paste0(
    "The traveller answered the following quiz:\n",
    "- Why they want to travel: ",         user_profile$why_travel,     "\n",
    "- What they have been missing: ",     user_profile$missing,        "\n",
    "- Number of travellers: ",            user_profile$num_travellers, "\n",
    "- How they feel about money: ",       user_profile$budget,         "\n",
    "- How long they want to travel: ",    user_profile$duration,       "\n",
    "- What their body needs right now: ", user_profile$interests,      "\n",
    "- Flying from: ",                     user_profile$origin_display
  )
  
  destinos_texto <- paste(
    apply(destinations, 1, function(row)
      paste0("- ", row["destino"], " (", row["pais"], "): €", row["precio"],
             ", ", row["duracion_horas"], "h flight")),
    collapse = "\n"
  )
  
  prompt <- paste0(
    perfil_texto, "\n\n",
    "Available flights (already sorted by emotional fit for this person):\n",
    destinos_texto, "\n\n",
    "Based ONLY on this person's emotional state, needs, budget and the destinations listed,",
    " pick the TOP 3 destinations that best match their mood and needs.\n",
    "The 'reason' must be deeply personal — connect it to their exact quiz answers and how",
    " the destination will make them feel.\n\n",
    "Respond ONLY with a valid JSON array, no markdown, no extra text:\n",
    '[{"destination":"CityName","price":123,"match_score":92,"reason":"..."},',
    '{"destination":"CityName","price":123,"match_score":85,"reason":"..."},',
    '{"destination":"CityName","price":123,"match_score":78,"reason":"..."}]'
  )
  
  response <- tryCatch({
    httr::POST(
      url = "https://api.groq.com/openai/v1/chat/completions",
      httr::add_headers("Authorization" = paste("Bearer", groq_key),
                        "Content-Type" = "application/json"),
      body = jsonlite::toJSON(list(
        model = "llama-3.3-70b-versatile",
        max_tokens = 600,
        temperature = 0.7,
        messages = list(
          list(role = "system", content = "You are a travel expert that understands emotions deeply. Respond ONLY with valid JSON arrays, no markdown, no extra text."),
          list(role = "user",   content = prompt)
        )
      ), auto_unbox = TRUE),
      encode = "raw"
    )
  }, error = function(e) {
    message("Groq call failed: ", e$message)
    return(NULL)
  })
  
  tryCatch({
    if (is.null(response) || response$status_code != 200) {
      message("Groq API error")
      return(get_mock_groq_analysis(destinations, user_profile))
    }
    
    raw_content <- httr::content(response, "text", encoding = "UTF-8")
    parsed      <- jsonlite::fromJSON(raw_content, simplifyVector = FALSE)
    texto       <- trimws(gsub("```json|```", "", parsed$choices[[1]]$message$content))
    df          <- as.data.frame(jsonlite::fromJSON(texto))
    
    if (!all(c("destination","price","match_score","reason") %in% names(df))) {
      message("Missing columns in AI response")
      return(get_mock_groq_analysis(destinations, user_profile))
    }
    
    df$booking_url <- sapply(df$destination, function(dest) {
      match_row <- destinations[grepl(dest, destinations$destino, ignore.case = TRUE), ]
      if (nrow(match_row) > 0 && "booking_url" %in% names(match_row))
        return(match_row$booking_url[1])
      return("https://www.skyscanner.net")
    })
    
    return(df)
    
  }, error = function(e) {
    message("Parse failed: ", e$message, " — using mock fallback")
    get_mock_groq_analysis(destinations, user_profile)
  })
}

get_mock_groq_analysis <- function(destinations, user_profile = NULL) {
  if (is.null(destinations) || nrow(destinations) == 0)
    return(data.frame(destination=character(), price=numeric(), reason=character(),
                      match_score=numeric(), booking_url=character(), stringsAsFactors=FALSE))
  
  interests <- if (!is.null(user_profile)) tolower(user_profile$interests %||% "") else ""
  
  # Razones personalizadas por mood
  get_reason <- function(dest, interests) {
    if (grepl("dance|stay up late", interests)) {
      if (grepl("Ibiza|Berlin|Amsterdam|Dublin", dest))
        return("This is where night owls go when they need to forget everything — exactly what you said you need.")
      return("Known for its vibrant nightlife, this destination will let you stay up as late as your body allows.")
    }
    if (grepl("slow down|do nothing", interests)) {
      if (grepl("Madeira|Azores|Faro|Alicante", dest))
        return("Quiet beaches, slow mornings, and no agenda — exactly what you said your body is asking for.")
      return("A place where time slows down, perfect for your need to rest and recover.")
    }
    if (grepl("beauty|art|history", interests)) {
      if (grepl("Rome|Athens|Vienna|Florence", dest))
        return("Centuries of history and art around every corner — this will feed your soul.")
      return("Rich in culture and beauty, this city will satisfy your hunger for art and wonder.")
    }
    if (grepl("connect|food|people", interests)) {
      if (grepl("Rome|Istanbul|Paris|Lisbon", dest))
        return("The food, the people, the stories in every corner — connection is guaranteed here.")
      return("A city that lives on the streets, where connecting with locals is part of the experience.")
    }
    if (grepl("alive|movement|adventure", interests)) {
      return("High energy, endless things to discover — this place will make you feel fully alive.")
    }
    return("Perfectly matched to your current state of mind and travel needs.")
  }
  
  result <- data.frame(destination=character(), price=numeric(), reason=character(),
                       match_score=numeric(), booking_url=character(), stringsAsFactors=FALSE)
  
  # Tomar los primeros 3 destinos (ya vienen ordenados por mood priority)
  for (i in 1:min(3, nrow(destinations))) {
    dest  <- destinations$destino[i]
    price <- destinations$precio[i]
    url   <- if ("booking_url" %in% names(destinations)) destinations$booking_url[i] else "#"
    score <- 95 - (i - 1) * 7
    
    result <- rbind(result, data.frame(
      destination = dest,
      price       = price,
      reason      = get_reason(dest, interests),
      match_score = score,
      booking_url = url,
      stringsAsFactors = FALSE
    ))
  }
  
  result
}

# =============================================================================
# IMAGE QUIZ OPTIONS (Q5)
# =============================================================================

q5_image_options <- list(
  list(val="To slow down and do nothing for a while",
       img="https://images.unsplash.com/photo-1507525428034-b723cf961d3e?w=400&q=70",
       icon="🌿", label="Rest"),
  list(val="To feel alive — movement, Adventure, Energy",
       img="https://images.unsplash.com/photo-1551632811-561732d1e306?w=400&q=70",
       icon="⚡", label="Adventure"),
  list(val="To connect — food, People, Streets, Stories",
       img="https://images.unsplash.com/photo-1414235077428-338989a2e8c0?w=400&q=70",
       icon="🍽️", label="Connection"),
  list(val="To lose myself in beauty — art, History, Wonder",
       img="https://images.unsplash.com/photo-1499856871958-5b9627545d1a?w=400&q=70",
       icon="🏛️", label="Culture"),
  list(val="To dance, Stay up late and forget about everything",
       img="https://images.unsplash.com/photo-1516450360452-9312f5e86fc7?w=400&q=70",
       icon="🎶", label="Nightlife"),
  list(val="To breathe — mountains, Ocean, Open Air",
       img="https://images.unsplash.com/photo-1506905925346-21bda4d32df4?w=400&q=70",
       icon="🌊", label="Nature")
)

build_image_quiz_q5 <- function() {
  div(
    style = "margin-bottom: 1.2rem;",
    tags$label(
      style = "font-size:0.7rem;font-weight:500;letter-spacing:0.14em;text-transform:uppercase;color:#6b8eb5;display:block;margin-bottom:1rem;",
      "What does your body need most right now?"
    ),
    div(
      id    = "q5_image_grid",
      style = "display:grid;grid-template-columns:repeat(3,1fr);gap:0.75rem;",
      tagList(lapply(q5_image_options, function(opt) {
        div(
          class        = "img-choice-card",
          `data-value` = opt$val,
          onclick      = "selectImageCard(this)",
          style = "position:relative;border-radius:8px;overflow:hidden;border:2px solid rgba(107,142,181,0.2);cursor:pointer;transition:all 0.25s ease;aspect-ratio:4/3;",
          tags$img(src=opt$img, alt=opt$label,
                   style="width:100%;height:100%;object-fit:cover;display:block;transition:transform 0.35s ease;"),
          div(
            style = "position:absolute;inset:0;background:linear-gradient(180deg,transparent 35%,rgba(5,17,31,0.9) 100%);display:flex;flex-direction:column;justify-content:flex-end;padding:0.55rem 0.65rem;",
            tags$span(style="font-size:1.1rem;line-height:1.2;", opt$icon),
            tags$span(style="font-size:0.72rem;font-weight:600;letter-spacing:0.08em;color:#fff;line-height:1.3;margin-top:2px;display:block;", opt$label)
          ),
          div(class="img-check",
              style="position:absolute;top:7px;right:7px;width:22px;height:22px;border-radius:50%;background:#0770e3;display:none;align-items:center;justify-content:center;font-size:0.72rem;color:#fff;font-weight:700;",
              HTML("&#10003;"))
        )
      }))
    ),
    tags$input(id="q5", type="hidden", value="", name="q5")
  )
}

# =============================================================================
# CSS
# =============================================================================

custom_css <- tags$head(
  tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
  tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=NA),
  tags$link(rel="stylesheet",
            href="https://fonts.googleapis.com/css2?family=Playfair+Display:ital,wght@0,400;0,700;1,400&family=Outfit:wght@300;400;500;600&display=swap"),
  tags$link(rel="stylesheet",
            href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
  tags$link(rel="stylesheet",
            href="https://cdn.jsdelivr.net/npm/flatpickr/dist/flatpickr.min.css"),
  tags$style(HTML("
    :root {
      --sky:   #0770e3;
      --sky2:  #1e90ff;
      --sky3:  #e8f1fd;
      --dark:  #05111f;
      --dark2: #091929;
      --dark3: #0d2137;
      --slate: #6b8eb5;
      --cream: #eef4ff;
      --white: #ffffff;
      --gold:  #f0b429;
      --font-display: 'Playfair Display', Georgia, serif;
      --font-body:    'Outfit', system-ui, sans-serif;
    }
    *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
    html, body { font-family: var(--font-body); background: var(--dark); color: var(--cream); min-height: 100vh; overflow-x: hidden; }

    .navbar, nav.navbar {
      background: rgba(5,17,31,0.96) !important;
      backdrop-filter: blur(16px);
      border-bottom: 1px solid rgba(7,112,227,0.2) !important;
      padding: 0 2.5rem !important;
      position: sticky; top: 0; z-index: 1000;
    }
    .navbar-brand {
      font-family: var(--font-display) !important;
      font-size: 1.6rem !important; font-weight: 700 !important;
      color: var(--white) !important; letter-spacing: 0.02em;
    }
    .navbar-nav .nav-link {
      font-family: var(--font-body) !important;
      font-size: 0.73rem !important; font-weight: 500 !important;
      color: var(--slate) !important; letter-spacing: 0.15em;
      text-transform: uppercase; padding: 1.3rem 1rem !important;
      border-bottom: 2px solid transparent; transition: all 0.25s ease;
    }
    .navbar-nav .nav-link:hover, .navbar-nav .nav-link.active {
      color: var(--sky2) !important; border-bottom-color: var(--sky);
    }

    #static-hero {
      position: relative; width: 100%; height: 520px; overflow: hidden;
      background-image: url('https://images.unsplash.com/photo-1583422409516-2895a77efded?w=1600&q=85');
      background-size: cover; background-position: center 40%;
    }
    .hero-overlay {
      position: absolute; inset: 0;
      background: linear-gradient(120deg, rgba(5,17,31,0.88) 0%, rgba(5,17,31,0.50) 55%, rgba(7,112,227,0.12) 100%);
    }
    .hero-content {
      position: absolute; bottom: 0; left: 0; right: 0;
      padding: 3rem 4rem; animation: fadeUp 0.9s ease forwards;
    }
    @keyframes fadeUp { from { opacity:0; transform:translateY(30px); } to { opacity:1; transform:translateY(0); } }
    .hero-tag {
      display: inline-block; font-size: 0.65rem; font-weight: 600; letter-spacing: 0.28em;
      text-transform: uppercase; color: var(--sky2);
      border: 1px solid rgba(30,144,255,0.35); padding: 0.3rem 0.75rem; border-radius: 2px; margin-bottom: 0.8rem;
    }
    .hero-title {
      font-family: var(--font-display); font-size: clamp(2rem, 4.5vw, 3.4rem);
      font-weight: 700; color: var(--white); line-height: 1.18; margin-bottom: 0.7rem;
    }
    .hero-title em { color: var(--sky2); font-style: italic; }
    .hero-sub { font-size: 0.88rem; color: rgba(238,244,255,0.65); font-weight: 300; letter-spacing: 0.04em; }
    .hero-sky-badge {
      position: absolute; top: 20px; right: 28px; z-index: 10;
      display: flex; align-items: center; gap: 8px;
      background: rgba(5,17,31,0.72); backdrop-filter: blur(8px);
      border: 1px solid rgba(7,112,227,0.3); border-radius: 4px; padding: 6px 14px 6px 10px;
    }
    .sky-badge-text { font-size: 0.6rem; font-weight: 500; letter-spacing: 0.12em; text-transform: uppercase; color: var(--slate); }

    .price-source-badge {
      display: inline-flex; align-items: center; gap: 6px;
      font-size: 0.65rem; font-weight: 600; letter-spacing: 0.12em;
      text-transform: uppercase; color: var(--sky2);
      background: rgba(7,112,227,0.1); border: 1px solid rgba(7,112,227,0.25);
      border-radius: 3px; padding: 4px 10px; margin-bottom: 0.8rem;
    }
    .price-source-badge.live   { color: #22c55e; border-color: rgba(34,197,94,0.3); background: rgba(34,197,94,0.08); }
    .price-source-badge.estimated { color: var(--gold); border-color: rgba(240,180,41,0.3); background: rgba(240,180,41,0.07); }

    .mood-badge {
      display: inline-flex; align-items: center; gap: 7px;
      font-size: 0.66rem; font-weight: 600; letter-spacing: 0.14em;
      text-transform: uppercase; color: #a78bfa;
      background: rgba(167,139,250,0.08); border: 1px solid rgba(167,139,250,0.25);
      border-radius: 3px; padding: 4px 10px; margin-bottom: 0.6rem;
    }

    .date-picker-section {
      background: rgba(7,112,227,0.05); border: 1px solid rgba(7,112,227,0.18);
      border-radius: 4px; padding: 1.4rem 1.6rem; margin-bottom: 1.2rem;
    }
    .date-picker-section .dp-title {
      font-size: 0.68rem; font-weight: 600; letter-spacing: 0.22em; text-transform: uppercase;
      color: var(--sky); margin-bottom: 1rem; padding-bottom: 0.6rem; border-bottom: 1px solid rgba(7,112,227,0.15);
    }
    .date-row { display: grid; grid-template-columns: 1fr 1fr auto; gap: 1rem; align-items: end; }
    .flatpickr-input {
      background: rgba(255,255,255,0.05) !important; border: 1px solid rgba(107,142,181,0.25) !important;
      border-radius: 3px !important; color: var(--cream) !important;
      font-family: var(--font-body) !important; font-size: 0.85rem !important;
      padding: 0.6rem 0.85rem !important; width: 100%; transition: border-color 0.2s ease; cursor: pointer;
    }
    .flatpickr-input:focus { border-color: var(--sky) !important; outline: none !important; }
    .flatpickr-calendar {
      background: var(--dark2) !important; border: 1px solid rgba(7,112,227,0.25) !important;
      border-radius: 6px !important; box-shadow: 0 16px 48px rgba(0,0,0,0.6) !important;
    }
    .flatpickr-months { background: var(--dark3) !important; border-radius: 6px 6px 0 0 !important; }
    .flatpickr-month, .flatpickr-current-month, .flatpickr-monthDropdown-months,
    .flatpickr-current-month input.cur-year { color: var(--cream) !important; fill: var(--cream) !important; background: transparent !important; }
    .flatpickr-weekday { color: var(--slate) !important; background: transparent !important; }
    .flatpickr-day { color: var(--cream) !important; border-radius: 3px !important; }
    .flatpickr-day:hover { background: rgba(7,112,227,0.18) !important; border-color: transparent !important; }
    .flatpickr-day.selected { background: var(--sky) !important; border-color: var(--sky) !important; }
    .flatpickr-day.inRange { background: rgba(7,112,227,0.12) !important; box-shadow: none !important; border-color: transparent !important; }
    .flatpickr-day.startRange, .flatpickr-day.endRange { background: var(--sky) !important; border-color: var(--sky) !important; }
    .flatpickr-day.flatpickr-disabled { color: rgba(107,142,181,0.3) !important; }
    .flatpickr-prev-month svg, .flatpickr-next-month svg { fill: var(--slate) !important; }
    .date-summary {
      font-size: 0.78rem; color: var(--sky2);
      background: rgba(7,112,227,0.08); border-radius: 3px;
      padding: 0.45rem 0.85rem; margin-top: 0.6rem; display: none;
    }

    .progress-strip {
      background: var(--dark2); border-bottom: 1px solid rgba(7,112,227,0.15);
      padding: 1rem 4rem; display: flex; align-items: center;
    }
    .progress-step { display: flex; align-items: center; gap: 0.6rem; flex: 1; }
    .step-num {
      width: 26px; height: 26px; border-radius: 50%;
      border: 1px solid var(--slate);
      display: flex; align-items: center; justify-content: center;
      font-size: 0.68rem; font-weight: 600; color: var(--slate);
      flex-shrink: 0; transition: all 0.3s ease;
    }
    .step-num.done   { background: var(--sky); border-color: var(--sky); color: var(--white); }
    .step-num.active { border-color: var(--sky2); color: var(--sky2); }
    .step-label { font-size: 0.66rem; letter-spacing: 0.14em; text-transform: uppercase; color: var(--slate); white-space: nowrap; }
    .step-connector { flex: 1; height: 1px; background: rgba(107,142,181,0.2); margin: 0 0.6rem; }

    .fm-section { max-width: 1150px; margin: 2.5rem auto; padding: 0 2rem; }
    .fm-section-title { font-family: var(--font-display); font-size: 1.9rem; font-weight: 400; color: var(--white); margin-bottom: 0.3rem; }
    .fm-section-sub { font-size: 0.72rem; color: var(--slate); letter-spacing: 0.16em; text-transform: uppercase; margin-bottom: 2.2rem; padding-bottom: 1.2rem; border-bottom: 1px solid rgba(107,142,181,0.15); }
    .fm-card { background: rgba(255,255,255,0.03); border: 1px solid rgba(7,112,227,0.14); border-radius: 4px; padding: 1.8rem; margin-bottom: 1.4rem; }
    .fm-card-title { font-size: 0.68rem; font-weight: 600; letter-spacing: 0.22em; text-transform: uppercase; color: var(--sky); margin-bottom: 1.4rem; padding-bottom: 0.7rem; border-bottom: 1px solid rgba(7,112,227,0.15); }
    label, .control-label, legend { font-family: var(--font-body) !important; font-size: 0.7rem !important; font-weight: 500 !important; letter-spacing: 0.14em !important; text-transform: uppercase !important; color: var(--slate) !important; margin-bottom: 0.5rem !important; }
    .form-control, .form-select, select.form-control { background: rgba(255,255,255,0.05) !important; border: 1px solid rgba(107,142,181,0.22) !important; border-radius: 3px !important; color: var(--cream) !important; font-family: var(--font-body) !important; font-size: 0.85rem !important; padding: 0.6rem 0.85rem !important; transition: border-color 0.2s ease; }
    .form-control:focus, .form-select:focus { background: rgba(7,112,227,0.06) !important; border-color: var(--sky) !important; box-shadow: 0 0 0 2px rgba(7,112,227,0.18) !important; color: var(--cream) !important; outline: none !important; }
    .form-control option, select option { background: var(--dark2) !important; color: var(--cream) !important; }
    .radio label { font-size: 0.84rem !important; letter-spacing: 0.02em !important; text-transform: none !important; color: var(--cream) !important; font-weight: 300 !important; cursor: pointer; }
    input[type='radio'] { accent-color: var(--sky) !important; }
    input[type='number'] { background: rgba(255,255,255,0.05) !important; border: 1px solid rgba(107,142,181,0.22) !important; color: var(--cream) !important; border-radius: 3px !important; }
    pre.shiny-text-output { background: rgba(7,112,227,0.06) !important; border: 1px solid rgba(7,112,227,0.15) !important; border-radius: 3px !important; color: var(--sky2) !important; font-size: 0.82rem !important; padding: 0.75rem 1rem !important; font-family: var(--font-body) !important; }

    .btn { font-family: var(--font-body) !important; font-size: 0.7rem !important; font-weight: 600 !important; letter-spacing: 0.18em !important; text-transform: uppercase !important; border-radius: 3px !important; padding: 0.65rem 1.6rem !important; transition: all 0.22s ease !important; border: none !important; }
    .btn-primary { background: var(--sky) !important; color: var(--white) !important; }
    .btn-primary:hover { background: var(--sky2) !important; transform: translateY(-1px); box-shadow: 0 6px 22px rgba(7,112,227,0.35) !important; }
    .btn-info { background: transparent !important; color: var(--sky) !important; border: 1px solid var(--sky) !important; }
    .btn-info:hover { background: rgba(7,112,227,0.1) !important; }
    .btn-success { background: #0a7c4e !important; color: var(--white) !important; }
    .btn-success:hover { background: #0d9860 !important; transform: translateY(-1px); }
    .btn-warning { background: var(--sky) !important; color: var(--white) !important; }
    .btn-warning:hover { background: var(--sky2) !important; }
    .btn-secondary { background: rgba(107,142,181,0.12) !important; color: var(--slate) !important; border: 1px solid rgba(107,142,181,0.2) !important; }
    .btn-secondary:hover { background: rgba(107,142,181,0.22) !important; color: var(--cream) !important; }
    .btn-share { background: transparent !important; color: var(--gold) !important; border: 1px solid rgba(240,180,41,0.4) !important; font-size: 0.65rem !important; padding: 0.35rem 0.85rem !important; margin-top: 0.5rem; }
    .btn-share:hover { background: rgba(240,180,41,0.1) !important; color: var(--gold) !important; }

    .img-choice-card:hover img { transform: scale(1.07); }
    .img-choice-card.selected { border-color: var(--sky) !important; box-shadow: 0 0 0 3px rgba(7,112,227,0.45); }
    .img-choice-card.selected .img-check { display: flex !important; }
    @media (max-width: 600px) { #q5_image_grid { grid-template-columns: repeat(2, 1fr) !important; } }

    .dataTables_wrapper { color: var(--cream) !important; font-size: 0.82rem; }
    table.dataTable { background: transparent !important; }
    table.dataTable thead th { background: rgba(7,112,227,0.1) !important; color: var(--sky2) !important; font-size: 0.63rem !important; letter-spacing: 0.18em !important; text-transform: uppercase !important; font-weight: 600 !important; border-bottom: 1px solid rgba(7,112,227,0.22) !important; padding: 0.85rem 1rem !important; }
    table.dataTable tbody tr { background: rgba(255,255,255,0.015) !important; transition: background 0.15s ease; }
    table.dataTable tbody tr:hover { background: rgba(7,112,227,0.06) !important; }
    table.dataTable tbody td { color: var(--cream) !important; border-bottom: 1px solid rgba(255,255,255,0.04) !important; padding: 0.7rem 1rem !important; }
    table.dataTable tbody tr.selected td { background: rgba(7,112,227,0.15) !important; color: var(--sky2) !important; }
    .dataTables_info, .dataTables_length label, .dataTables_filter label { color: var(--slate) !important; font-size: 0.74rem !important; }
    .dataTables_filter input, .dataTables_length select { background: rgba(255,255,255,0.05) !important; border: 1px solid rgba(107,142,181,0.22) !important; color: var(--cream) !important; border-radius: 3px !important; padding: 0.3rem 0.6rem !important; }
    .paginate_button { background: transparent !important; color: var(--slate) !important; border: none !important; border-radius: 3px !important; }
    .paginate_button.current, .paginate_button:hover { background: rgba(7,112,227,0.14) !important; color: var(--sky2) !important; border: none !important; }

    .rec-card { background: rgba(255,255,255,0.03); border: 1px solid rgba(7,112,227,0.15); border-top: 3px solid var(--sky); border-radius: 4px; padding: 1.8rem; transition: transform 0.28s ease, box-shadow 0.28s ease; }
    .rec-card:hover { transform: translateY(-5px); box-shadow: 0 18px 44px rgba(0,0,0,0.45); }
    .rec-rank { font-family: var(--font-display); font-size: 3.2rem; font-weight: 400; color: rgba(7,112,227,0.2); line-height: 1; margin-bottom: 0.5rem; }
    .rec-city { font-family: var(--font-display); font-size: 1.5rem; font-weight: 700; color: var(--white); margin-bottom: 0.3rem; }
    .rec-price { font-size: 1.05rem; color: var(--sky2); font-weight: 600; margin-bottom: 0.3rem; }
    .rec-score-bar { height: 2px; background: rgba(7,112,227,0.15); border-radius: 2px; margin: 1rem 0 0.3rem; overflow: hidden; }
    .rec-score-fill { height: 100%; background: linear-gradient(90deg, var(--sky), var(--sky2)); border-radius: 2px; }
    .rec-score-label { font-size: 0.68rem; letter-spacing: 0.14em; text-transform: uppercase; color: var(--slate); }
    .rec-reason { font-size: 0.84rem; color: rgba(238,244,255,0.68); line-height: 1.65; font-weight: 300; margin-top: 0.8rem; }
    .rec-price-note { font-size: 0.68rem; color: var(--slate); margin-top: 4px; letter-spacing: 0.06em; }

    .lock-state { text-align: center; padding: 5rem 2rem; }
    .lock-state h3 { font-family: var(--font-display); font-size: 1.7rem; font-weight: 400; margin-bottom: 0.7rem; color: rgba(238,244,255,0.35); }
    .lock-state p { font-size: 0.82rem; letter-spacing: 0.06em; color: var(--slate); }
    .lock-icon { font-size: 2.2rem; color: rgba(107,142,181,0.25); margin-bottom: 1.3rem; }

    .two-col { display: grid; grid-template-columns: 320px 1fr; gap: 1.8rem; align-items: start; }
    hr { border: none !important; border-top: 1px solid rgba(107,142,181,0.12) !important; margin: 1.6rem 0 !important; }
    .card { background: transparent !important; border: none !important; box-shadow: none !important; }
    .card-header { background: transparent !important; border: none !important; }
    .card-body { padding: 0 !important; }
    .shiny-notification { background: var(--dark2) !important; border: 1px solid rgba(7,112,227,0.25) !important; border-left: 3px solid var(--sky) !important; color: var(--cream) !important; border-radius: 3px !important; font-family: var(--font-body) !important; font-size: 0.82rem !important; }
    ::-webkit-scrollbar { width: 5px; }
    ::-webkit-scrollbar-track { background: var(--dark); }
    ::-webkit-scrollbar-thumb { background: rgba(7,112,227,0.35); border-radius: 3px; }
    ::-webkit-scrollbar-thumb:hover { background: var(--sky); }
    @media (max-width: 768px) {
      .two-col { grid-template-columns: 1fr; }
      .hero-content { padding: 2rem; }
      .fm-section { margin: 1.5rem auto; padding: 0 1rem; }
      .progress-strip { padding: 0.8rem; }
      .step-label { display: none; }
      .date-row { grid-template-columns: 1fr; }
    }
  "))
)

# =============================================================================
# JAVASCRIPT
# =============================================================================

app_js <- tagList(
  tags$script(src="https://cdn.jsdelivr.net/npm/flatpickr"),
  tags$script(HTML("
$(document).on('shiny:sessioninitialized', function() { initDatePickers(); });

function initDatePickers() {
  if (document.getElementById('dep_date_input')) {
    flatpickr('#dep_date_input', {
      minDate: 'today', dateFormat: 'Y-m-d', disableMobile: false,
      onChange: function(selectedDates, dateStr) {
        if (typeof Shiny !== 'undefined') Shiny.setInputValue('dep_date_val', dateStr, {priority:'event'});
        updateReturnMinDate(dateStr); updateDateSummary();
      }
    });
  }
  if (document.getElementById('ret_date_input')) {
    flatpickr('#ret_date_input', {
      minDate: 'today', dateFormat: 'Y-m-d', disableMobile: false,
      onChange: function(selectedDates, dateStr) {
        if (typeof Shiny !== 'undefined') Shiny.setInputValue('ret_date_val', dateStr, {priority:'event'});
        updateDateSummary();
      }
    });
  }
}

function updateReturnMinDate(depDate) {
  var retPicker = document.getElementById('ret_date_input');
  if (retPicker && retPicker._flatpickr) {
    retPicker._flatpickr.set('minDate', depDate);
    var retVal = retPicker._flatpickr.selectedDates[0];
    if (retVal && new Date(retVal) < new Date(depDate)) retPicker._flatpickr.setDate(depDate, true);
  }
}

function updateDateSummary() {
  var dep = document.getElementById('dep_date_input');
  var ret = document.getElementById('ret_date_input');
  var summary = document.getElementById('date-summary');
  if (!dep || !ret || !summary) return;
  var d1 = dep._flatpickr ? dep._flatpickr.selectedDates[0] : null;
  var d2 = ret._flatpickr ? ret._flatpickr.selectedDates[0] : null;
  if (d1 && d2) {
    var diffDays = Math.round((d2 - d1) / (1000 * 60 * 60 * 24));
    var opts = { day:'numeric', month:'short', year:'numeric' };
    summary.style.display = 'block';
    summary.innerHTML = '<i class=\"fa fa-calendar-check\" style=\"margin-right:6px;color:var(--sky);\"></i>' +
      d1.toLocaleDateString('en-GB', opts) + ' &nbsp;→&nbsp; ' +
      d2.toLocaleDateString('en-GB', opts) +
      ' &nbsp;·&nbsp; <strong>' + diffDays + ' day' + (diffDays !== 1 ? 's' : '') + '</strong>';
  } else { summary.style.display = 'none'; }
}

function selectImageCard(el) {
  document.querySelectorAll('.img-choice-card').forEach(function(c) { c.classList.remove('selected'); });
  el.classList.add('selected');
  var val = el.getAttribute('data-value');
  var hidden = document.getElementById('q5');
  if (hidden) hidden.value = val;
  if (typeof Shiny !== 'undefined' && Shiny.setInputValue) Shiny.setInputValue('q5', val, {priority:'event'});
}

function openShareEmail(dest, price, airline, dateFrom, dateTo) {
  var recipient = (document.getElementById('share_email') || {}).value || '';
  var subject = encodeURIComponent('Check out this destination: ' + dest);
  var body = encodeURIComponent(
    'Hey!\\n\\nI found a great flight to ' + dest + ' for only \\u20ac' + price +
    ' with ' + airline + '.\\n\\nDates: ' + dateFrom + ' to ' + dateTo +
    '\\n\\nFound via FlyMood \\u2708'
  );
  window.location.href = 'mailto:' + recipient + '?subject=' + subject + '&body=' + body;
}
  "))
)

# =============================================================================
# HERO
# =============================================================================

build_static_hero <- function() {
  div(
    id = "static-hero",
    div(class = "hero-overlay"),
    div(
      class = "hero-sky-badge",
      tags$svg(
        xmlns="http://www.w3.org/2000/svg", viewBox="0 0 110 28", height="18", style="display:block;",
        tags$text(x="0",  y="20", style="font-family:'Arial Black',Arial,sans-serif;font-size:18px;font-weight:900;fill:#0770e3;", "sky"),
        tags$text(x="42", y="20", style="font-family:Arial,sans-serif;font-size:18px;font-weight:400;fill:#eef4ff;", "scanner")
      ),
      div(class="sky-badge-text", "Powered by Skyscanner")
    ),
    div(
      class = "hero-content",
      tags$span(class="hero-tag", "Barcelona, Spain"),
      tags$h1(class="hero-title", HTML("Where will your<br><em>mood</em> take you?")),
      tags$p(class="hero-sub", "AI-powered travel matched to how you feel right now — real prices, real flights")
    )
  )
}

build_progress <- function(step_done = 0) {
  steps <- c("Profile", "Dates & Search", "Destinations & AI")
  items <- list()
  for (i in seq_along(steps)) {
    cls <- if (i <= step_done) "step-num done" else if (i == step_done + 1) "step-num active" else "step-num"
    num <- if (i <= step_done) HTML("&#10003;") else i
    items[[length(items)+1]] <- div(class="progress-step",
                                    div(class=cls, num),
                                    span(class="step-label", steps[i]))
    if (i < length(steps)) items[[length(items)+1]] <- div(class="step-connector")
  }
  div(class="progress-strip", tagList(items))
}

# =============================================================================
# UI
# =============================================================================

ui <- tagList(
  custom_css, app_js,
  navbarPage(
    title = tagList(
      tags$span(
        style="font-family:'Playfair Display',serif;font-weight:700;color:#fff;",
        HTML("Fly"), tags$span(style="color:#0770e3;", "Mood")
      )
    ),
    windowTitle = "FlyMood — AI Travel Matching",
    collapsible  = TRUE,
    theme = bs_theme(
      version=5, bootswatch=NULL, bg="#05111f", fg="#eef4ff",
      primary="#0770e3", base_font=font_google("Outfit")
    ),
    
    # ── TAB 1: Profile ──────────────────────────────────────────────────────
    tabPanel("Profile",
             build_static_hero(),
             uiOutput("progress_ui_1"),
             div(class="fm-section",
                 h2(class="fm-section-title", "Your Travel Profile"),
                 p(class="fm-section-sub", "Step 01 — Tell us about yourself"),
                 div(class="two-col",
                     div(
                       div(class="fm-card",
                           p(class="fm-card-title", HTML('<i class="fa fa-plane-departure" style="margin-right:6px;"></i>Origin Airport')),
                           selectInput("country", "Country of Departure", choices=paises_disponibles, selected="Spain"),
                           conditionalPanel(
                             "input.country != 'Others'",
                             selectInput("airport", "Select Airport", choices=NULL)
                           ),
                           conditionalPanel(
                             "input.country == 'Others'",
                             textInput("custom_airport", "Airport / IATA Code", placeholder="e.g. Dubai (DXB)"),
                             textInput("custom_country", "Country", placeholder="e.g. United Arab Emirates")
                           ),
                           verbatimTextOutput("selected_airport_display")
                       ),
                       div(class="fm-card",
                           p(class="fm-card-title", HTML('<i class="fa fa-route" style="margin-right:6px;"></i>Journey Progress')),
                           uiOutput("progress_status")
                       ),
                       div(class="fm-card",
                           p(class="fm-card-title", HTML('<i class="fa fa-circle-info" style="margin-right:6px;"></i>Powered By')),
                           div(style="display:flex;align-items:center;gap:10px;margin-bottom:0.8rem;",
                               tags$svg(
                                 xmlns="http://www.w3.org/2000/svg", viewBox="0 0 110 28", height="20", style="display:block;",
                                 tags$text(x="0",  y="20", style="font-family:'Arial Black',Arial,sans-serif;font-size:18px;font-weight:900;fill:#0770e3;", "sky"),
                                 tags$text(x="42", y="20", style="font-family:Arial,sans-serif;font-size:18px;font-weight:400;fill:#eef4ff;", "scanner")
                               ),
                               tags$span(style="font-size:0.75rem;color:var(--slate);", "Real-time flight pricing")
                           ),
                           tags$span(
                             style="font-size:0.72rem;color:var(--slate);letter-spacing:0.06em;",
                             HTML('<i class="fa fa-brain" style="color:var(--sky);margin-right:4px;"></i>AI analysis by Groq Llama 3.3')
                           )
                       )
                     ),
                     div(class="fm-card",
                         p(class="fm-card-title", HTML('<i class="fa fa-clipboard-question" style="margin-right:6px;"></i>Travel Questionnaire')),
                         p(style="font-size:0.78rem;color:var(--slate);margin-bottom:1.4rem;",
                           "Your answers shape which destinations we recommend — the more honest, the better the match."),
                         radioButtons("q1", quiz_preguntas$pregunta[1],
                                      choices=trimws(strsplit(quiz_preguntas$opciones[1], ",")[[1]])),
                         hr(),
                         radioButtons("q2", quiz_preguntas$pregunta[2],
                                      choices=trimws(strsplit(quiz_preguntas$opciones[2], ",")[[1]])),
                         conditionalPanel(
                           "input.q2 == 'Laughs and memories to share with people'",
                           numericInput("num_travellers", "Number of Travellers (including you)", value=2, min=2, max=20)
                         ),
                         hr(),
                         radioButtons("q3", quiz_preguntas$pregunta[3],
                                      choices=trimws(strsplit(quiz_preguntas$opciones[3], ",")[[1]])),
                         hr(),
                         radioButtons("q4", quiz_preguntas$pregunta[4],
                                      choices=trimws(strsplit(quiz_preguntas$opciones[4], ",")[[1]])),
                         hr(),
                         build_image_quiz_q5(),
                         br(),
                         actionButton("save_profile", "Save Profile & Continue",
                                      class="btn-primary", icon=icon("arrow-right"))
                     )
                 )
             )
    ),
    
    # ── TAB 2: Dates & Search ───────────────────────────────────────────────
    tabPanel("Dates & Search",
             build_static_hero(),
             uiOutput("progress_ui_2"),
             div(class="fm-section",
                 h2(class="fm-section-title", "Travel Dates"),
                 p(class="fm-section-sub", "Step 02 — Choose your departure & return"),
                 conditionalPanel(
                   "output.profile_exists == true",
                   div(class="fm-card",
                       p(class="fm-card-title", HTML('<i class="fa fa-calendar-days" style="margin-right:6px;"></i>Select Your Travel Dates')),
                       p(style="font-size:0.78rem;color:var(--slate);margin-bottom:1.4rem;",
                         HTML('Pick your outbound and return dates. Prices are fetched live from Skyscanner for the selected period.')),
                       div(class="date-picker-section",
                           div(class="dp-title", HTML('<i class="fa fa-calendar" style="margin-right:6px;"></i>Manual Date Selection')),
                           div(class="date-row",
                               div(
                                 tags$label(style="font-size:0.65rem;font-weight:600;letter-spacing:0.14em;text-transform:uppercase;color:var(--slate);display:block;margin-bottom:6px;", "Departure date"),
                                 tags$input(id="dep_date_input", type="text", class="flatpickr-input", placeholder="Select departure date", readonly="readonly")
                               ),
                               div(
                                 tags$label(style="font-size:0.65rem;font-weight:600;letter-spacing:0.14em;text-transform:uppercase;color:var(--slate);display:block;margin-bottom:6px;", "Return date"),
                                 tags$input(id="ret_date_input", type="text", class="flatpickr-input", placeholder="Select return date", readonly="readonly")
                               ),
                               div(
                                 tags$label(style="font-size:0.65rem;font-weight:600;letter-spacing:0.14em;text-transform:uppercase;color:transparent;display:block;margin-bottom:6px;", "."),
                                 actionButton("use_manual_dates", "Search Flights", class="btn-success", icon=icon("magnifying-glass"))
                               )
                           ),
                           div(id="date-summary", class="date-summary")
                       ),
                       div(style="margin-top:1.5rem;",
                           tags$details(
                             tags$summary(
                               style="font-size:0.7rem;font-weight:600;letter-spacing:0.14em;text-transform:uppercase;color:var(--slate);cursor:pointer;padding:0.5rem 0;",
                               HTML('<i class="fa fa-calendar-import" style="margin-right:6px;"></i>Or import from calendar (.ics)')
                             ),
                             div(style="padding-top:1rem;",
                                 p(style="font-size:0.76rem;color:var(--slate);margin-bottom:0.8rem;",
                                   "Export your Google Calendar or Outlook as .ics and we'll find your free windows automatically."),
                                 fileInput("ics_file", "ICS Calendar File (.ics)", accept=".ics"),
                                 actionButton("process_calendar", "Detect Free Windows", class="btn-info", icon=icon("calendar-check"))
                             )
                           )
                       )
                   ),
                   conditionalPanel(
                     "output.calendar_processed == true",
                     div(class="fm-card",
                         p(class="fm-card-title", HTML('<i class="fa fa-list-check" style="margin-right:6px;"></i>Available Windows from Calendar')),
                         DTOutput("free_dates_table"),
                         br(),
                         p(style="font-size:0.76rem;color:var(--slate);",
                           HTML('<i class="fa fa-hand-pointer" style="margin-right:4px;color:var(--sky);"></i>Click a row to select it, then confirm below.')),
                         br(),
                         actionButton("select_window", "Use This Window & Search Flights", class="btn-success", icon=icon("plane-departure"))
                     )
                   )
                 ),
                 conditionalPanel(
                   "output.profile_exists == false",
                   div(class="lock-state",
                       div(class="lock-icon", icon("lock")),
                       tags$h3("Profile Required"),
                       p("Complete your travel profile in Step 01 first.")
                   )
                 )
             )
    ),
    
    # ── TAB 3: Destinations + AI ────────────────────────────────────────────
    tabPanel("Destinations & AI",
             build_static_hero(),
             uiOutput("progress_ui_3"),
             div(class="fm-section",
                 h2(class="fm-section-title", "Destinations & AI Recommendations"),
                 p(class="fm-section-sub", "Step 03 — Explore routes & get your curated selection"),
                 conditionalPanel(
                   "output.window_selected == true",
                   div(class="fm-card",
                       p(class="fm-card-title", HTML('<i class="fa fa-table" style="margin-right:6px;"></i>Available Routes')),
                       # Badge de mood + fuente de precios
                       uiOutput("mood_badge_ui"),
                       uiOutput("price_source_badge_ui"),
                       p(style="font-size:0.76rem;color:var(--slate);margin-bottom:1rem;",
                         HTML('<i class="fa fa-info-circle" style="color:var(--sky);margin-right:4px;"></i>Destinations sorted by emotional fit. Click <b>Book Now</b> to reserve on Skyscanner.')),
                       DTOutput("cheap_destinations_table"),
                       br(),
                       # Share UI — solo visible si la persona NO eligió "Space and silence"
                       uiOutput("share_selected_ui")
                   ),
                   div(class="fm-card",
                       p(class="fm-card-title", HTML('<i class="fa fa-globe" style="margin-right:6px;"></i>World Map')),
                       plotlyOutput("destinations_map", height="480px")
                   ),
                   div(style="text-align:center;margin:1.5rem 0;",
                       actionButton("analyze_destinations", "Run AI Analysis",
                                    class="btn-primary", icon=icon("wand-magic-sparkles"))
                   ),
                   conditionalPanel(
                     "output.analyzed == true",
                     uiOutput("ai_recommendations"),
                     br(),
                     div(style="text-align:center;",
                         actionButton("reset_app", "Start Over", class="btn-secondary", icon=icon("rotate-left"))
                     )
                   )
                 ),
                 conditionalPanel(
                   "output.window_selected == false",
                   div(class="lock-state",
                       div(class="lock-icon", icon("lock")),
                       tags$h3("Select Travel Dates First"),
                       p("Choose your dates in Step 02 to explore available destinations.")
                   )
                 )
             )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  values <- reactiveValues(
    user_profile       = NULL,
    travel_window      = NULL,
    travel_windows     = NULL,
    cheap_destinations = NULL,
    ai_recommendations = NULL,
    calendar_processed = FALSE,
    window_selected    = FALSE,
    analyzed           = FALSE,
    price_source       = "estimated"
  )
  
  current_step <- reactive({
    if (values$analyzed)                          3
    else if (!is.null(values$cheap_destinations)) 3
    else if (values$window_selected)              2
    else if (!is.null(values$user_profile))       1
    else 0
  })
  
  for (sfx in c("1","2","3")) {
    local({
      s <- sfx
      output[[paste0("progress_ui_", s)]] <- renderUI({ build_progress(current_step()) })
    })
  }
  
  # ── Airport choices ──────────────────────────────────────────────────────
  observeEvent(input$country, {
    if (input$country != "Others" && input$country %in% names(aeropuertos_mundo)) {
      ap <- aeropuertos_mundo[[input$country]]
      updateSelectInput(session, "airport",
                        choices=setNames(ap$codigo, paste0(ap$ciudad, "  (", ap$codigo, ")")))
    }
  })
  
  output$selected_airport_display <- renderPrint({
    if (input$country == "Others") {
      cat(ifelse(nchar(trimws(input$custom_airport)) > 0,
                 paste0("Airport: ", input$custom_airport, "\nCountry: ",
                        ifelse(nchar(trimws(input$custom_country))>0, input$custom_country, "Other")),
                 "Enter your airport information above"))
    } else {
      req(input$airport)
      ap <- aeropuertos_mundo[[input$country]] %>% filter(codigo == input$airport)
      if (nrow(ap) > 0) cat(ap$ciudad[1], "(", input$airport, ") —", input$country)
    }
  })
  
  # ── Output flags ─────────────────────────────────────────────────────────
  output$profile_exists     <- reactive({ !is.null(values$user_profile) })
  output$calendar_processed <- reactive({ values$calendar_processed })
  output$window_selected    <- reactive({ values$window_selected })
  output$analyzed           <- reactive({ values$analyzed })
  outputOptions(output, "profile_exists",     suspendWhenHidden=FALSE)
  outputOptions(output, "calendar_processed", suspendWhenHidden=FALSE)
  outputOptions(output, "window_selected",    suspendWhenHidden=FALSE)
  outputOptions(output, "analyzed",           suspendWhenHidden=FALSE)
  
  # ── Progress status ───────────────────────────────────────────────────────
  output$progress_status <- renderUI({
    steps <- list(
      list(done=!is.null(values$user_profile),        label="Profile completed"),
      list(done=values$window_selected,                label="Travel dates selected"),
      list(done=!is.null(values$cheap_destinations),  label="Prices fetched from Skyscanner"),
      list(done=values$analyzed,                       label="AI analysis complete")
    )
    tagList(lapply(steps, function(s) {
      col <- if (s$done) "color:var(--sky);" else "color:rgba(107,142,181,0.4);"
      ic  <- if (s$done) "circle-check" else "circle"
      tags$div(
        style="display:flex;align-items:center;gap:0.6rem;margin-bottom:0.55rem;",
        tags$i(class=paste0("fa fa-", ic), style=paste0("font-size:0.78rem;", col)),
        tags$span(style=paste0("font-size:0.78rem;", col, "letter-spacing:0.02em;"), s$label)
      )
    }))
  })
  
  # ── Save profile ──────────────────────────────────────────────────────────
  observeEvent(input$save_profile, {
    if (any(sapply(list(input$q1, input$q2, input$q3, input$q4), is.null))) {
      showNotification("Please answer all questionnaire questions.", type="error"); return()
    }
    q5_val <- input$q5
    if (is.null(q5_val) || nchar(trimws(q5_val)) == 0) {
      showNotification("Please select an image for the last question.", type="error"); return()
    }
    if (input$country == "Others" && nchar(trimws(input$custom_airport)) == 0) {
      showNotification("Please enter your departure airport.", type="error"); return()
    }
    if (input$country != "Others" && is.null(input$airport)) {
      showNotification("Please select an airport.", type="error"); return()
    }
    
    is_group <- grepl("Laughs", input$q2)
    origin_display <- if (input$country == "Others")
      paste0(input$custom_airport, " (", ifelse(nchar(trimws(input$custom_country))>0, input$custom_country, "Other"), ")")
    else
      paste0(input$airport, " (", input$country, ")")
    
    values$user_profile <- list(
      why_travel     = input$q1,
      missing        = input$q2,
      budget         = input$q3,
      duration       = input$q4,
      interests      = q5_val,
      is_group       = is_group,
      wants_solo     = grepl("Space and silence", input$q2),  # <-- FLAG NUEVO
      num_travellers = if (is_group) max(2L, as.integer(input$num_travellers %||% 2)) else 1L,
      origin_airport = if (input$country == "Others") input$custom_airport else input$airport,
      origin_country = if (input$country == "Others") input$custom_country else input$country,
      origin_display = origin_display
    )
    showNotification(paste0("Profile saved. Departing from: ", origin_display),
                     type="message", duration=4)
  })
  
  # ── Helper: fetch flights ─────────────────────────────────────────────────
  fetch_flights_for_window <- function(window) {
    req(values$user_profile)
    withProgress(message="Fetching live prices from Skyscanner...", {
      result <- tryCatch(
        get_skyscanner_live_prices(
          origin      = values$user_profile$origin_airport,
          date_from   = window$start,
          date_to     = window$end,
          budget_tier = values$user_profile$budget,
          user_profile = values$user_profile
        ),
        error = function(e) { message("Price fetch error: ", e$message); NULL }
      )
      result
    })
  }
  
  # ── Manual date selection ─────────────────────────────────────────────────
  observeEvent(input$use_manual_dates, {
    req(values$user_profile)
    dep <- input$dep_date_val
    ret <- input$ret_date_val
    
    if (is.null(dep) || dep == "" || is.null(ret) || ret == "") {
      showNotification("Please select both departure and return dates.", type="error"); return()
    }
    dep_date <- tryCatch(as.Date(dep), error=function(e) NULL)
    ret_date <- tryCatch(as.Date(ret), error=function(e) NULL)
    if (is.null(dep_date) || is.null(ret_date)) {
      showNotification("Invalid date format. Please reselect.", type="error"); return()
    }
    if (ret_date <= dep_date) {
      showNotification("Return date must be after departure date.", type="error"); return()
    }
    
    values$travel_window <- list(start=dep_date, end=ret_date, days=as.numeric(ret_date-dep_date))
    
    result <- fetch_flights_for_window(values$travel_window)
    if (!is.null(result) && !is.null(result$data) && nrow(result$data) > 0) {
      values$cheap_destinations <- result$data
      values$price_source       <- result$source
      values$window_selected    <- TRUE
      n <- nrow(result$data)
      src_msg <- if (result$source == "live") "live from Skyscanner" else "calibrated by season & availability"
      showNotification(
        paste0(n, " routes found (", src_msg, "), sorted by emotional fit for your profile."),
        type="message"
      )
    } else {
      showNotification("No routes found within your budget for these dates. Try adjusting.", type="warning")
    }
  })
  
  # ── ICS calendar processing ───────────────────────────────────────────────
  observeEvent(input$process_calendar, {
    req(input$ics_file, values$user_profile)
    withProgress(message="Analysing calendar...", {
      values$travel_windows     <- parse_ics_simple(input$ics_file$datapath)
      values$calendar_processed <- TRUE
    })
    showNotification(paste0(length(values$travel_windows), " travel windows identified."), type="message")
  })
  
  parse_ics_simple <- function(filepath) {
    tryCatch({
      lines <- readLines(filepath, warn=FALSE)
      occupied_dates <- c()
      for (line in lines) {
        if (grepl("DTSTART", line, ignore.case=TRUE)) {
          matches <- regmatches(line, regexec("(\\d{4})(\\d{2})(\\d{2})", line))
          if (length(matches[[1]]) > 0) {
            fecha_str <- paste(matches[[1]][2], matches[[1]][3], matches[[1]][4], sep="-")
            occupied_dates <- c(occupied_dates, fecha_str)
          }
        }
      }
      if (length(occupied_dates) == 0)
        return(list(list(start=Sys.Date()+7, end=Sys.Date()+12, days=5),
                    list(start=Sys.Date()+21, end=Sys.Date()+26, days=5),
                    list(start=Sys.Date()+35, end=Sys.Date()+40, days=5)))
      
      occupied_dates <- unique(as.Date(occupied_dates))
      all_dates  <- seq(Sys.Date(), Sys.Date()+60, by="day")
      free_dates <- all_dates[!all_dates %in% occupied_dates]
      travel_windows <- list()
      current_start  <- NULL
      current_end    <- NULL
      for (i in seq_along(free_dates)) {
        if (is.null(current_start)) {
          current_start <- free_dates[i]; current_end <- free_dates[i]
        } else if (free_dates[i] - current_end == 1) {
          current_end <- free_dates[i]
        } else {
          if (current_end - current_start >= 2)
            travel_windows <- append(travel_windows, list(list(
              start=current_start, end=current_end, days=as.numeric(current_end-current_start)+1)))
          current_start <- free_dates[i]; current_end <- free_dates[i]
        }
      }
      if (!is.null(current_start) && current_end-current_start >= 2)
        travel_windows <- append(travel_windows, list(list(
          start=current_start, end=current_end, days=as.numeric(current_end-current_start)+1)))
      if (length(travel_windows) == 0)
        travel_windows <- list(list(start=Sys.Date()+7, end=Sys.Date()+12, days=5))
      return(travel_windows)
    }, error=function(e) {
      list(list(start=Sys.Date()+7, end=Sys.Date()+12, days=5))
    })
  }
  
  output$free_dates_table <- renderDT({
    req(values$travel_windows)
    df <- do.call(rbind, lapply(values$travel_windows, function(w)
      data.frame(From=format(w$start, "%d %b %Y"), To=format(w$end, "%d %b %Y"),
                 Duration=paste(w$days, "days"))))
    datatable(df, options=list(pageLength=8, dom="tip", scrollX=FALSE),
              selection=list(mode="single", target="row"), rownames=FALSE, class="display nowrap")
  })
  
  observeEvent(input$select_window, {
    req(values$travel_windows, input$free_dates_table_rows_selected)
    values$travel_window <- values$travel_windows[[input$free_dates_table_rows_selected[1]]]
    result <- fetch_flights_for_window(values$travel_window)
    if (!is.null(result) && !is.null(result$data) && nrow(result$data) > 0) {
      values$cheap_destinations <- result$data
      values$price_source       <- result$source
      values$window_selected    <- TRUE
      showNotification(paste0(nrow(result$data), " routes found."), type="message")
    } else {
      showNotification("No routes found within budget for this window.", type="warning")
    }
  })
  
  # ── Mood badge ────────────────────────────────────────────────────────────
  output$mood_badge_ui <- renderUI({
    req(values$user_profile)
    interests <- values$user_profile$interests %||% ""
    mood_icon <- dplyr::case_when(
      grepl("dance|stay up late", tolower(interests))         ~ "🎶 Nightlife mood",
      grepl("breathe|mountain|ocean", tolower(interests))     ~ "🌊 Nature mood",
      grepl("slow down|do nothing", tolower(interests))       ~ "🌿 Rest mood",
      grepl("beauty|art|history", tolower(interests))         ~ "🏛️ Culture mood",
      grepl("connect|food|people", tolower(interests))        ~ "🍽️ Connection mood",
      grepl("alive|movement|adventure", tolower(interests))   ~ "⚡ Adventure mood",
      TRUE                                                    ~ "✈️ Travel mood"
    )
    div(class="mood-badge",
        HTML(paste0('<i class="fa fa-wand-magic-sparkles" style="margin-right:5px;"></i>',
                    'Destinations filtered for your mood: <strong style="margin-left:4px;">', mood_icon, '</strong>')))
  })
  
  # ── Price source badge ────────────────────────────────────────────────────
  output$price_source_badge_ui <- renderUI({
    req(values$cheap_destinations)
    if (values$price_source == "live") {
      div(class="price-source-badge live",
          HTML('<i class="fa fa-wifi" style="margin-right:5px;"></i>Live prices from Skyscanner — exact fares'))
    } else {
      div(class="price-source-badge estimated",
          HTML('<i class="fa fa-chart-line" style="margin-right:5px;"></i>Prices calibrated by season & demand — click Book Now for exact fare on Skyscanner'))
    }
  })
  
  # ── Destinations table ────────────────────────────────────────────────────
  output$cheap_destinations_table <- renderDT({
    req(values$cheap_destinations)
    df <- values$cheap_destinations %>%
      mutate(
        Book = paste0(
          '<a href="', booking_url, '" target="_blank" ',
          'style="background:#0770e3;color:#fff;padding:5px 14px;border-radius:3px;',
          'text-decoration:none;font-size:0.7rem;font-weight:600;display:inline-block;" ',
          'onmouseover="this.style.background=\'#1e90ff\'" onmouseout="this.style.background=\'#0770e3\'">',
          'Book Now <i class="fa fa-external-link" style="margin-left:4px;font-size:0.65rem;"></i></a>'
        )
      ) %>%
      select(destino, pais, precio, aerolinea, duracion_horas, Book) %>%
      rename(Destination="destino", Country="pais", `Price (€)`="precio",
             Airline="aerolinea", `Duration (h)`="duracion_horas", ` `="Book")
    
    datatable(df,
              options=list(pageLength=10, dom="ftipr", scrollX=FALSE,
                           columnDefs=list(list(targets=5, orderable=FALSE))),
              selection=list(mode="single", target="row"),
              rownames=FALSE, escape=FALSE, class="display nowrap")
  })
  
  # ── Share UI — OCULTO si la persona eligió "Space and silence just for me" ─
  output$share_selected_ui <- renderUI({
    
    # Si quiere silencio y soledad → no mostrar nunca el share
    req(values$user_profile)
    if (isTRUE(values$user_profile$wants_solo)) return(NULL)
    
    sel <- input$cheap_destinations_table_rows_selected
    req(sel, values$cheap_destinations, values$travel_window)
    
    row      <- values$cheap_destinations[sel, ]
    dest     <- row$destino
    price    <- row$precio
    airline  <- row$aerolinea
    date_from <- format(values$travel_window$start, "%d %b %Y")
    date_to   <- format(values$travel_window$end,   "%d %b %Y")
    
    div(class="fm-card", style="border-color:rgba(240,180,41,0.3);",
        p(class="fm-card-title", style="color:var(--gold);",
          HTML('<i class="fa fa-share-nodes" style="margin-right:6px;"></i>Share This Destination')),
        p(style="font-size:0.82rem;color:var(--cream);margin-bottom:0.5rem;",
          HTML(paste0('<strong style="color:var(--sky2);">', dest, '</strong>',
                      ' — €', price, ' with ', airline,
                      ' &nbsp;|&nbsp; ', date_from, ' → ', date_to))),
        p(style="font-size:0.76rem;color:var(--slate);margin-bottom:1rem;",
          "Send this flight to a friend so they can join you."),
        div(style="display:flex;gap:0.8rem;align-items:flex-end;flex-wrap:wrap;",
            div(style="flex:1;min-width:220px;",
                tags$label(style="font-size:0.65rem;letter-spacing:0.14em;text-transform:uppercase;color:var(--slate);display:block;margin-bottom:4px;",
                           "Friend's email address"),
                textInput("share_email", label=NULL, placeholder="friend@email.com", width="100%")
            ),
            actionButton("send_share_email", "Send via Email",
                         class="btn-share", icon=icon("envelope"),
                         onclick=sprintf("openShareEmail('%s','%s','%s','%s','%s')",
                                         dest, price, airline, date_from, date_to))
        ),
        tags$small(style="font-size:0.65rem;color:rgba(107,142,181,0.5);margin-top:0.5rem;display:block;",
                   "Opens your email client with a pre-filled message ready to send.")
    )
  })
  
  # ── World map ─────────────────────────────────────────────────────────────
  output$destinations_map <- renderPlotly({
    req(values$cheap_destinations)
    d <- values$cheap_destinations %>% filter(!is.na(lat), !is.na(lon))
    if (nrow(d) == 0) {
      return(plotly_empty() %>% layout(
        title=list(text="Map not available", font=list(color="#6b8eb5")),
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)"
      ))
    }
    plot_ly(data=d) %>%
      add_trace(
        type="scattergeo", lon=~lon, lat=~lat, mode="markers",
        marker=list(
          size=12, color=~precio,
          colorscale=list(c(0,"#0770e3"), c(0.5,"#1e90ff"), c(1,"#f0b429")),
          showscale=TRUE,
          colorbar=list(title="€/person",
                        tickfont=list(color="#6b8eb5"),
                        titlefont=list(color="#6b8eb5")),
          line=list(color="rgba(5,17,31,0.7)", width=1)
        ),
        text=~paste0("<b>", destino, "</b><br>", pais, "<br>€", precio, "<br>", aerolinea),
        hoverinfo="text"
      ) %>%
      layout(
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
        geo=list(
          scope="world", showframe=FALSE,
          bgcolor="rgba(5,17,31,0)",
          landcolor="rgba(13,33,55,0.92)",
          oceancolor="rgba(5,17,31,0.97)",
          lakecolor="rgba(5,17,31,0.7)",
          showocean=TRUE, showlakes=TRUE,
          countrycolor="rgba(107,142,181,0.18)",
          coastlinecolor="rgba(107,142,181,0.12)",
          projection=list(type="natural earth"),
          framecolor="rgba(0,0,0,0)"
        ),
        margin=list(l=0, r=0, t=0, b=0),
        font=list(family="Outfit,sans-serif", color="#6b8eb5")
      )
  })
  
  # ── AI analysis ───────────────────────────────────────────────────────────
  observeEvent(input$analyze_destinations, {
    req(values$cheap_destinations, values$user_profile)
    withProgress(message="AI is curating your recommendations...", {
      values$ai_recommendations <- get_groq_analysis(
        values$cheap_destinations,
        values$user_profile
      )
      values$analyzed <- TRUE
    })
    showNotification("AI analysis complete. Scroll down for your recommendations.", type="message")
  })
  
  # ── AI Recommendations UI ─────────────────────────────────────────────────
  output$ai_recommendations <- renderUI({
    req(values$ai_recommendations, values$travel_window)
    rec <- values$ai_recommendations
    if (nrow(rec) == 0)
      return(div(class="fm-card",
                 p(style="color:var(--slate);",
                   "No destinations found. Try adjusting your budget or dates.")))
    
    ranks <- c("01", "02", "03")
    cards <- lapply(seq_len(nrow(rec)), function(i) {
      r <- rec[i, ]
      div(class="rec-card",
          div(class="rec-rank", ranks[i]),
          div(class="rec-city", r$destination),
          div(class="rec-price", paste0("€", r$price, " / person")),
          div(class="rec-price-note",
              HTML(if (values$price_source == "live")
                '<i class="fa fa-circle-check" style="color:#22c55e;margin-right:4px;"></i>Live price from Skyscanner'
                else
                  '<i class="fa fa-info-circle" style="margin-right:4px;"></i>Indicative — confirm exact fare on Skyscanner')),
          div(class="rec-score-bar",
              div(class="rec-score-fill", style=paste0("width:", r$match_score, "%"))),
          div(style="display:flex;justify-content:space-between;margin-bottom:0.8rem;",
              span(class="rec-score-label", "Match Score"),
              span(class="rec-score-label", paste0(r$match_score, "%"))),
          div(class="rec-reason", r$reason),
          div(style="margin-top:1.2rem;",
              tags$a(
                href=r$booking_url, target="_blank",
                style="display:block;text-align:center;background:#0770e3;color:#fff;padding:0.75rem 1.2rem;border-radius:3px;text-decoration:none;font-size:0.75rem;letter-spacing:0.12em;font-weight:600;transition:all 0.2s ease;",
                onmouseover="this.style.background='#1e90ff'",
                onmouseout ="this.style.background='#0770e3'",
                HTML('Book on Skyscanner <i class="fa fa-external-link" style="margin-left:6px;"></i>')
              )
          )
      )
    })
    
    trip_label <- paste0(
      values$travel_window$days, "-day journey — ",
      format(values$travel_window$start, "%d %b"),
      " to ", format(values$travel_window$end, "%d %b %Y")
    )
    
    div(class="fm-card",
        p(class="fm-card-title",
          HTML('<i class="fa fa-star" style="margin-right:6px;"></i>AI Curated for You')),
        p(style="font-size:0.76rem;color:var(--slate);letter-spacing:0.1em;text-transform:uppercase;margin-bottom:1.5rem;",
          trip_label),
        div(style="display:grid;grid-template-columns:repeat(auto-fit,minmax(260px,1fr));gap:1.5rem;",
            tagList(cards))
    )
  })
  
  # ── Reset ─────────────────────────────────────────────────────────────────
  observeEvent(input$reset_app, {
    values$user_profile        <- NULL
    values$travel_window       <- NULL
    values$travel_windows      <- NULL
    values$cheap_destinations  <- NULL
    values$ai_recommendations  <- NULL
    values$calendar_processed  <- FALSE
    values$window_selected     <- FALSE
    values$analyzed            <- FALSE
    values$price_source        <- "estimated"
    showNotification("Application reset. Ready to start a new search.", type="message")
  })
}

shinyApp(ui=ui, server=server)