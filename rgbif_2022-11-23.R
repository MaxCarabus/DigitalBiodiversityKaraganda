setwd('/home/carabus/bdi/Karaganda/practice_today')

# install.packages('rgbif')
library(rgbif)
# описание пакета: https://cran.r-project.org/web/packages/rgbif/rgbif.pdf

# install.packages('maptools')
library(maptools)
# описание пакета https://cran.r-project.org/web/packages/maptools/maptools.pdf

# install.packages('geopdata')
library(geodata)
# описание пакета https://cran.r-project.org/web/packages/geodata/geodata.pdf

# выбираем вид
sp = 'Felis manul'

# проверяем название черех GBIF Taxonomy Backbone
spName = name_backbone(sp)
colnames(spName)
key = spName$usageKey
# gbif.org/species/2435023

spName$scientificName
spName$status

# проверяем число находок
occ_count(taxonKey = key)

# задаём набор терминов Darwin Core (полей)
# Darwin Core quick reference guide https://dwc.tdwg.org/terms/

terms = c('key','scientificName','decimalLatitude','decimalLongitude','country',
          'countryCode','eventDate','year','month','day','coordinateUncertaintyInMeters')


occurs = occ_search(taxonKey = key)
nrow(occurs$data)
occurs = occ_search(taxonKey = key, limit = 643, fields = terms)$data
nrow(occurs)
colnames(occurs)

# отфильтровываем находки без координат
occurs$decimalLatitude
occurs = occurs[!is.na(occurs$decimalLatitude),]
nrow(occurs)
occurs$decimalLongitude

# 1 января 
occurs[occurs$month == 1, ]
occurs[occurs$month == 1 & occurs$day == 1, ]

occurs = occurs[occurs$month != 1 & occurs$day != 1, ]


# границы стран
data('wrld_simpl')
class(wrld_simpl)
head(wrld_simpl)

plot(wrld_simpl, col = 'lightcyan')
# цвета в R http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
points(occurs$decimalLongitude, occurs$decimalLatitude, col = 'red', cex = .7, pch = 16)
# https://www.inaturalist.org/observations?place_id=any&quality_grade=research&subview=map&taxon_id=42029&verifiable=any

occurs[occurs$decimalLongitude < 0,]
# https://www.gbif.org/occurrence/1702674496
# https://www.gbif.org/occurrence/1702674467
# https://www.gbif.org/occurrence/1702673260

occurs = occurs[occurs$decimalLongitude > 0,]
min(occurs$decimalLatitude)
max(occurs$decimalLatitude)
min(occurs$decimalLongitude)
max(occurs$decimalLongitude)

plot(wrld_simpl, xlim = c(45,118), ylim = c(27,53), col = 'lightcyan', axes = T)
points(occurs$decimalLongitude, occurs$decimalLatitude, col = 'red', cex = .7, pch = 16)
grid()


# выберем точки только для территории Казахстана

# по атрибутам  
unique(occurs$countryCode) # список уникальных значений
occursAttr = occurs[occurs$countryCode == 'KZ',] 


KZboundary = gadm(country = 'KZ', level = 0, path = 'geodata/')
plot(KZboundary)
grid() 
points(occursAttr$decimalLongitude,occursAttr$decimalLatitude, pch = 16, col = 'red', cex =  1.2)

plot(occursAttr$decimalLongitude, occursAttr$decimalLatitude)
plot(KZboundary, add = T, border = 'cyan', lwd = 2)
nrow(occursAttr) 

# пространственный запрос
occursGeo = occurs
coordinates(occursGeo) = ~ decimalLongitude + decimalLatitude

KZboundary = as(KZboundary, "Spatial")
occursGeoKZ = intersect(occursGeo, KZboundary)

plot(KZboundary)
plot(occursGeoKZ, pch = 16, add = T)
points(occursAttr$decimalLongitude, occursAttr$decimalLatitude, pch = 16, col = 'red', cex = .7)

occursAttr$coordinateUncertaintyInMeters

# индивидуальный участок - среднее 23 для самок, 98 для самцов - 61
sqrt(61/3.14)

ch = convHull(manulGeoKZ)
plot(ch, add = T, col = 'green')

# https://rspatial.org/raster/sdm/index.html