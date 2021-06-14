

# Curation Script ---------------------------------------------------------

# Dataset: LTER Hoge Kempen National Park Arachnids
# Location: Hoge Kempen, Belgium
# Curator: Cher Chow
# Date: 29-Apr-2020

# Set up ------------------------------------------------------------------
# load the necessary packages
require(tidyverse)
require(maps)
require(readxl)

rm(list=ls()) # clear up the environment before starting

# make sure your working directory is set before running these lines
setwd(file.choose() %>% dirname()) # for reference, my default is my working BioTIME folder
dt <- read_excel('./CurationProgress/Originals/B2SHARE/S007.xlsx', sheet=1, skip=3, col_names=T, na='')

# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
# 1746 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric? Y

# check if these columns need to be kept
# remove if they're consistent for whole dataset
dt$Site <- if(length(unique(dt$Site)) == 1){NULL}
dt$Habitat <- if(length(unique(dt$Habitat)) == 1){NULL}
dt$Group <- if(length(unique(dt$Group)) == 1) {NULL}

# Year, month and day must be integers or factors? Y
colnames(dt)[1] <- 'Year' # just a rename
dt$Year <- as.factor(dt$Year)

# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA

# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA, just year
# Taxonomic fields must be characters or factors? Y


# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.

min(dt$Abundance) # check the minimum (no zeroes) Y
sum(dt$Abundance=="") # no blanks Y

# YEAR MONTH DAY
# again, no negative values, 0s or NAs, and all are logical
# Year < 2020, month < 12, day < 31

summary(dt[,1]) # looks good to me

# LAT LONG
# no blanks, no NAs
# Latitude constrained from -90 to 90.
# Longitude constrained -180 to 180.
# Manually input centroid coordinates from metadata provided by Haase
dt$Latitude <- as.numeric(rep('50.931', nrow(dt)))
dt$Longitude <- as.numeric(rep('5.626', nrow(dt)))

world_map <- map_data('world') # check whether the GPS coordinates match expectations
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt[1,], aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(-10,10), ylim=c(40,60))

# all looks good

# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning
# check that genera are genera, not family names (-idae/eae)
dt$Species <- dt$Taxon # make a copy
# check the species list for misspellings or non-BioTIME taxonomic convention names
sort(unique(dt$Species))
dt$Genus <- word(dt$Taxon, 1)
dt$Species <- word(dt$Species, start=2, end=-1)

sort(unique(dt$Genus)) # check genera too just in case the eyes didn't pick up on that
replace_g <- c( # make a replacement vector, pattern = replacement
  'Drassylus' = 'Drassyllus',
  'Hyposinga' = 'Hypsosinga',
  'Palludiphantes' = 'Palliduphantes',
  'Porhomma' = 'Porrhomma',
  'Tenuiphanthes' = 'Tenuiphantes'
  )
dt$Genus <- str_replace_all(dt$Genus, replace_g)
dt$Taxon <- NULL # get rid of it now that we've split and checked

# check family too
sort(unique(dt$Family))

# Prepare raw data --------------------------------------------------------

dt <- dt %>% arrange(Year, Family, Genus, Species)
# now create empty columns needed to fit to template
dt$Biomass <- rep('', dim(dt)[1])
dt$Plot <- rep('', dim(dt)[1])
dt$DepthElevation <- rep('', dim(dt)[1])
dt$Day <- rep('', dim(dt)[1])
dt$Month <- rep('', dim(dt)[1])
dt$StudyID <- rep('', dim(dt)[1])
dt$SampleDescription <- rep('', dim(dt)[1])

# aggregate abundance records that are same species, plot, and survey day.
dt_merged <- dt %>% group_by(Biomass, Family, Genus, Species, SampleDescription, Plot, Latitude, Longitude, DepthElevation, Day, Month, Year, StudyID) %>% 
  summarise(Abundance=sum(Abundance)) %>% ungroup() %>% arrange(Year, Family, Genus, Species)
dim(dt)[1]-dim(dt_merged)[1] # any change in aggregating? yep. 1229 rows lost

dataset.name <- 'LTER_HogeKempenNationalPark_Arachnids'
dt_merged$SampleDescription <- as.factor(with(dt_merged, paste(dataset.name, Latitude, Longitude, Year, sep='_')))
length(levels(dt_merged$SampleDescription)) # 10 samples

# reorder columns by BioTIME format
dt_merged <- dt_merged[c('Abundance',
                         'Biomass',
                         'Family',
                         'Genus',
                         'Species',
                         'SampleDescription',
                         'Plot',
                         'Latitude',
                         'Longitude',
                         'DepthElevation',
                         'Day',
                         'Month',
                         'Year',
                         'StudyID')] %>% arrange(Year, Family, Genus, Species)
View(dt_merged) # final check :)
summary(dt_merged)
str(dt_merged)

# Export final ------------------------------------------------------------

setwd(file.choose() %>% dirname())
write.csv(dt_merged, paste0(getwd(), dataset.name, '_rawdata_CC.csv'), row.names=F)


# Convex Hull for centroid ------------------------------------------------

##load libraries
require(sp)
require(rgeos)
require(clipr)
clipr::write_clip(dt_merged)


##1. Convert data points into point spatial object
# this also transforms it from WGS84 coordinate reference to a mercator projection in km for calculating area in sq km
points204<- SpatialPoints(cbind(dt_merged$Longitude,dt_merged$Latitude), proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>% 
  sp::spTransform(., CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"))

##2. Calculate convex hull, area and centroid
convhull204<-gConvexHull(points204)
clipr::write_clip(gArea(convhull204))  ##get area

###get centroid
centroid204<-gCentroid(convhull204)    

##to get the coordinates
centroid204@coords
clipr::write_clip((centroid204@coords[c(2,1)]))

