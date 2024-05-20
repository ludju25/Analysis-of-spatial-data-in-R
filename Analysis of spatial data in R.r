################# INSTALACIJA POTREBNIH PAKETA #################################
#install.packages("ctv")
#CTV paket jedan je od osnovih i korisnijih paketa za analizu prostornih podataka

#install.packages("sp") 
#sp je "spatial", odnosno paket za obradu prostornih podataka 
#Osnovna klasa sp paketa je spatial klasa koja sadrzi dva unosa.

#install.packages("pkgbuild") 
#Potreban paket za preuzimanje iduceg paketa "spDataLarge"

#devtools::install_github("Nowosad/spDataLarge")
#Paket za velike spatial podatke

#install.packages("sf") 
#Klase i funkcije za vektorske podatke,sustav za geografske vektorske podatke 

#install.packages("raster") 
#Klase i funkcije za rasterske podatke

#install.packages("spData") 
#Paket za ucitavanje prostornih geografskih podataka

#install.packages("spDataLarge")
#Paket koji sadrzi "velike" prostorne podatke

################## PRIKAZ RADA S PAKETIMA I PLOTANJE ###########################
library("sf")
library("spData") #ucitavanje geografskih podataka 
library("spDataLarge") #ucitavanje velikih geografskih podataka

names(world) 
#Imena atributa unutar world data framea koji je sadrzan u "spDataLarge" library


vignette(package = "sf") #dijelovi sf paketa


data("world") #citanje podataka unutar "world" podatkovnog skupa 
windows() #kreiranje prozora za prikaz plotova
plot(world) #plotanje vise razlicitih karti svijeta po raznim parametrima

summary(world["lifeExp"]) 
#Rezultat nam vraca i opcenite i prostorne podatke za prosjecnu zivotnu dob 
#u data frameu "world", pri cemu ona iznosi cca 71 godinu


library("sp")
world_sp = as(world, Class = "Spatial") 
#Spremanje prostornih (spatial) podataka unutar zasebnog spatial atributa

world_sf = st_as_sf(world_sp, "sf")
#Pretvorba podataka "sp" tipa u "sf" (noviji i napredniji) tip spatial podataka


world_asia = world[world$continent == "Asia", ]
#Kreiranje data seta sa svim potrebnim podacima svih Azijskih zemalja 

asia = st_union(world_asia) #objedinjenje podataka Azije u oblike listi 

plot(world["pop"],reset=FALSE, main = "Populacija")
#Graf za populaciju

plot(asia, add = TRUE, col = "red")
#Dodavanje "slojeva" dobar je nacin za provjeru geografske korespodencije 
#unutar plotanog skupa podataka 

plot(world["continent"], reset = FALSE, main = "Kontinenti po bojama")
#Graf svih kontinenata odvojenih po bojama

cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)
#Dodavanje kruznih centroida sa podrucjima najvece napucenosti prema broju 
#stanovnika iz 2015. godine na prethodno kreirani graf

############################# VECTOR DATA ######################################
#Set funkcija za kreiranje jednostavnih geometrijskih objekata (sfg): 
#A point: st_point()
  # A linestring: st_linestring()
  # A polygon: st_polygon()
  # A multipoint: st_multipoint()
  # A multilinestring: st_multilinestring()
  # A multipolygon: st_multipolygon()
  # A geometry collection: st_geometrycollection()

#SFG objekti mogu biti stvoreni iz tri bazna R tipa podataka:
#1. Numerickog vektora : jedinstvena tocka
#2. Matrice: skupa tocaka, gdje svaki red predstavlja tocku, vise tocki ili
#            linijski niz
#3. Lista: kolekcija objekata kao sto su matrice, vise linijskih nizova ili 
#           geometrijskih skupova podataka

#Kreiranje tocaka 
st_point(c(5, 2)) # XY point
st_point(c(5, 2, 3)) #XYZ tocka

#Kreiranje matrice s vise tocaka 
multipoint_matrix = rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2))
st_multipoint(multipoint_matrix)

#Kreiranje i ispis liste multi-poligona i geometrijskih kolekcija
polygon_list = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
st_polygon(polygon_list)
#################################################

#U vecini slucajeva sfc objekt sadrzi objekte istih geometrijskih tipova, ali 
#uz pomoc st_geometry_type() mozemo pretvoriti sfc objekt u zeljeni tip poligona
multilinestring_list1 = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)),
                             rbind(c(1, 2), c(2, 4)))
multilinestring1 = st_multilinestring((multilinestring_list1))
multilinestring_list2 = list(rbind(c(2, 9), c(7, 9), c(5, 6), c(4, 7), c(2, 7)),
                             rbind(c(1, 7), c(3, 8)))
multilinestring2 = st_multilinestring((multilinestring_list2))
multilinestring_sfc = st_sfc(multilinestring1, multilinestring2)
st_geometry_type(multilinestring_sfc) #linija pretvorbe


lnd_point = st_point(c(0.1, 51.5)) # sfg object
lnd_geom = st_sfc(lnd_point, crs = 4326) # sfc object
lnd_attrib = data.frame( # data.frame object
  name = "London",
  temperature = 25,
  date = as.Date("2017-06-21")
)
lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)
lnd_sf
#Prvo --> koordinate su koristene za stvaranje jednostavnog geometrijskog 
#obiljezja (sfg). 
#Drugo --> geometrija je pretvorena u jednostavni stupac geometrijske znacajke 
#(sfc), s prikladnim CRS-om (Coordinate Reference Systemom). 
#Trece --> atributi su pohranjeni u a data.frame, koji je spojen sa sfc objektom 
#pomocu st_sf(), sto rezultira sf objektom 


############################# RASTER DATA ######################################
#Geografski rasterski model podataka obicno se sastoji od rasterskog zaglavlja 
#i matrica (s redcima i stupcima) koja predstavlja jednako rasporedene celije. 
#Zaglavlje rastera definira koordinate, referentni sustav, opseg i porijeklo.
#Rasterske karte obicno predstavljaju kontinuirane pojave kao sto su:
#nadmorska visina, temperatura ili gustoca naseljenosti.

#Kreiranje "RasterLayer" objekta pod nazivom "new_raster"
library(raster) #potreban library za rad s rasterima
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
new_raster = raster(raster_filepath)

new_raster 
#Ispisuje dimenzije rastera, rezolucije i ostale podatke

dim(new_raster) #dimenzije kreiranog rastera
ncell(new_raster) #broj celija (pixela) kreiranog rastera 

windows()
plot(new_raster) 
#Plotanje grafa nadmorske visine 
##########################################################

#Kreiranje viseslojnog rastera
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge") 
r_brick = brick(multi_raster_file)

r_brick #podaci viseslojnog rastera (dimenzije, ime klase, rezolucija, crs...)

nlayers(r_brick) #broj slojeva kreiranog raster objekta 
#######################################################

#"Raster stack" omoguce kombiniranje vise raster brickova
raster_on_disk = raster(r_brick, layer = 1)
raster_in_memory = raster(xmn = 301905, xmx = 335745,
                          ymn = 4111245, ymx = 4154085,
                          res = 30)
values(raster_in_memory) = sample(seq_len(ncell(raster_in_memory)))
crs(raster_in_memory) = crs(raster_on_disk)

r_stack = stack(raster_in_memory, raster_on_disk)
r_stack

################################  CRS ##########################################
#Dva glavna nacina za opisivanje CRS-a (Coordinate Reference Systema) u R-u su 
#preko "EPSG" koda ili preko "proj4string" definicije.
#EPSG kodovi su kraci ali su proj4string fleksibilniji pri definiranju parametara 
#i omogucuju vise modifikacija

crs_data = rgdal::make_EPSG() #kreiranje CRS podataka preko EPSG kodova
View(crs_data) #pregled kreiranih CRS EPSG podataka

#install.packages("rgdal") 
#Potrebni paket za "Geospatialne" apstraktne podatke

library("rgdal")
vector_filepath = system.file("vector/zion.gpkg", package = "spDataLarge")
new_vector = st_read(vector_filepath) 
#ispis geometrijskog tipa, dimenzija i prostora kreiranog vektora

st_crs(new_vector) #dobivanje CRS-a za kreirani vektor 

projection(new_raster) 
#!!!!Rasteri prihvacaju samo proj4 definicije !!!!

#Vazna znacajka CRS-a je da sadrze informacije o koristenim prostornim
#jedinicama, odnosno jesu li to m^2, km^2, stope ili slicno

luxembourg = world[world$name_long == "Luxembourg", ]
st_area(luxembourg) 
#Na ovom konkretnom primjeru vidimo i provjeravamo da li CRS sustav koristi 
#zeljene metricke mjerne jedinice (u ovom slucaju su to m^2)

units::set_units(st_area(luxembourg), km^2) 
#Mijenjanje postavljenih mjernih jedinica 
#Bitno je naglasiti da je sf jedini paket koji podrzava mjerne jedinice 

#install.packages("dplyr")
#Paket za manipulaciju data frame objektima unutar i van memorije sustava

#install.packages("stringr")
#Paket za manipulaciju sa stringovima

library(dplyr)
library(stringr)

methods(class = "sf")
#Ispisivanje metoda unutar klasa "sf" libraryja

st_sf(data.frame(n = world$name_long), g = world$geom)
#Omogucuje importanje i imenovanje geometrijskih podataka potrebnih za rad 
#iz razlicitih prostornih baza podataka

sel_area = world$area_km2 < 10000
summary(sel_area)
#Gornje dvije linije koda nam vracaju koliko je zemalja iz seta podataka manjih
#od 10 000 km2 (u binarnom obliku tj. TRUE/FALSE)

world6 = filter(world, lifeExp > 82)
world6 
#Jednostavna filtracija liste zemalja sa ocekivanim zivotnim vijekom > 82 godine 


####Slijedeci blok koda je zakomentiran jer nije se mogao compileati u report,
####inace je kod u potpunosti funkcionalan i potreban za rad projekta.#######

#Svjetska proizvodnja kave po drzavama 2017. godine (u tonama) i graficki prikaz
#coffe <- data.frame(coffee_data)
#class(world_coffee)
#plot(world_coffee["coffee_production_2017"])


###################### SPATIAL (PROSTORNI) PODACI ##############################

canterbury = nz %>% filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]

canterbury_height 
#Definicija i ispis svih visinske regije unutar Canterbury regije na 
#Novom Zelandu sa njihovom visinom, tipom objekta i geometrijskim oznakama

nz_height[canterbury, , op = st_disjoint] 
#Vraca nam GPS lokacije (tocke) tih visinskih objekata kao i njihove visine

##########################################################
nz_avheight = aggregate(x = nz_height, by = nz, FUN = mean)
nz_avheight2 = nz %>%
  st_join(nz_height) %>%
  group_by(Name) %>%
  summarize(elevation = mean(elevation, na.rm = TRUE))
windows()
plot(nz_avheight2)
#Plotanje grafa sa prikazom razlicitih visinskih regija na Novom Zelandu

#########################################################
nz_heighest = nz_height %>% top_n(n = 1, wt = elevation)
canterbury_centroid = st_centroid(canterbury)
st_distance(nz_heighest, canterbury_centroid)
#Gornji blok koda vraca nam udaljenost najvise tocke Novog Zelanda od sredista
#Canterbury regije na Novom Zelandu
#######################################################

co = filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:3, ], co)

plot(st_geometry(co)[2])
plot(st_geometry(nz_height)[2:3], add = TRUE) 

#Gornji bloku koda vraca nam daljenost izmedu druge i trece najvise tocke 
#Novog zelanda koje se pritom obje nalaze u Otago regiji, te njihove lokacije
#i udaljenosti mozemo vidjeti na grafu prikazane kruznicama

##############################################################

#Spajanje rastera (RasterLaysera) 
aut = getData("alt", country = "AUT", mask = TRUE)
ch = getData("alt", country = "CHE", mask = TRUE)
aut_ch = merge(aut, ch)

aut_ch 
#Prikaz podataka spojenih rastera

plot(aut_ch) 
#Visinska karta dva spojena rastera (Austrija i Svicarska)

######################### GEOMETRIJA NA KARTAMA ################################

seine_simp = st_simplify(seine, dTolerance = 2000)
#Prostorno i graficko pojednostavljivanje geometrije prikaza toka rijeke Seine

object.size(seine) 
#Velicina pojednostavljenog objekta

windows()
plot(seine, main= "Seine")
plot(seine_simp, main = "Geometrijska simplifikacija Seine")
#Usporedba originalne i pojednostavljene geometrije na karti seine 
#Vidimo da su na pojednostavljenijem prikazu linije ostrije i ravnije od onoga
#na originalnom grafu


####################### KREIRANJE MAPA (MAPIRANJE) #############################

#Osnovno plotanje grafa tj. karte ocekivanje zivotne dobi u svijetu
png(filename = "lifeExp.png", width = 500, height = 350)
plot(world["lifeExp"])
dev.off()
###########################################

#install.packages("tmap")
#Paket za kreiranje i manipuliranje kartama (mapama)

library(tmap)
tmap_obj = tm_shape(world) +
  tm_polygons(col = "lifeExp")
tmap_save(tm = tmap_obj, filename = "lifeExp_tmap.png")
#Spremanje naprednijeg grafa ocekivane zivotne dobi kreiranog uz pomoc "tmap"
#paketa direktno na racunalo


tmap_mode("view")
tmap_obj
#Ukoliko zelimo direktno unutar R-a pregledati kreirane mape moramo prvo
#postaviti "tmap" paket na "view" mode i potom ga jednostavno prikazujemo 
#unosom i pokretanjem naziva kreirane mape 

###########################################

#install.packages("mapview")
#Paket za interaktivni pregled prostornih podataka u R-u, tj. za kreiranje 
#animiranih karata (u obliku GIF-ova)

#install.packages("leaflet.providers")
#Paket potreban za uspjesan rad i postavljanje mapview paketa

#webshot::install_phantomjs()
#Preuzimanje phantomjs-a potrebnog za rad mapview paketa

library(mapview)
library(leaflet.providers)


map_nz = tm_shape(nz) + tm_polygons()
tmap_save(tm=map_nz, filename = "novizeland.png")
#Kreiranje karte Novog Zelanda i spremanje na racunalo

map_nz
#Interaktivni prikaz mape Novog Zelanda unutar R-a

urb_anim = tm_shape(world) + tm_polygons() +
  tm_shape(urban_agglomerations) + tm_dots(size = "population_millions") +
  tm_facets(along = "year", free.coords = FALSE)

tmap_animation(urb_anim, filename = "urb_anim.gif", delay = 25)
tmap_mode("view")
urb_anim
#Unutar R-a takoder mozemo obaviti prikaz kreirane animirane karte porasta 
#svjetske populacije od 1950. godine, ali nam se isti ne prikazuje u 
#animiranom obliku. Stoga, kartu moramo spremiti na racunalo odakle je mozemo 
#pokrenuti u i vidjeti kao animaciju u GIF formatu. 

################################################################################