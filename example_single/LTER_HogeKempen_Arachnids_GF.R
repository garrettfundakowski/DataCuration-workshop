

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
dt <- read_excel('./S007.xlsx', sheet=1, skip=3, col_names=T, na='')


# Structure check ---------------------------------------------------------

dim(dt) # check dimensions
# 1746 records
str(dt) # check structure
summary(dt)
# Abundance and/or biomass, latitude and longitude numeric?
is.numeric(dt$Abundance) #T
# Year, month and day must be integers or factors
is.factor(dt$year) | is.integer(dt$year) #F
# Secondary fields such as trawl, plot, transect etc must be factors or integers? NA
# Secondary fields such as elevation or depth will normally be numeric unless in a treatment format. NA here
# Date should be POSIXct (not applicable in this case) NA, just year
# Taxonomic fields must be characters or factors? 
is.factor(dt$Family) | is.character(dt$Family) #T
is.factor(dt$Taxon) | is.character(dt$Taxon) #T


# Structure fix ----------------------------------------------------------
dt$year<-as.factor(dt$year) # convert year to a factor

# check if these columns need to be kept
# remove if they're consistent for whole dataset
dt$Site <- if(length(unique(dt$Site)) == 1){NULL}
dt$Habitat <- if(length(unique(dt$Habitat)) == 1){NULL}
dt$Group <- if(length(unique(dt$Group)) == 1) {NULL}

colnames(dt)[1] <- 'Year' # just a rename


# Primary field check -----------------------------------------------------

# ABUNDANCES
# No negative values, zeroes, or NAs in abundance/biomass fields.
min(dt$Abundance)>0 #T 
sum(dt$Abundance=="")==0 #T
# if there are rows that need to be removed
# dt <- dt[!is.na(dt$Abundance),]
# dt <- dt[!which(dt$Abundance == 0),]
# or with dplyr's filter() function, which can handle multiple conditions
# dt <- dt %>% filter(!Abundance == '' | Abundance == 0 | is.na(Abundance))

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

#plot to visually check whether the GPS coordinates match expectations
world_map <- map_data('world')
world <- ggplot(world_map) + 
  geom_polygon(aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  coord_fixed() + 
  labs(x='Longitude', y='Latitude') +
  theme_bw() + theme(panel.grid=element_blank())
points <- world + geom_point(data=dt[1,], aes(x=Longitude, y=Latitude), shape=21)

points_zoom <- points + coord_fixed(xlim=c(-10,10), ylim=c(40,60))
points_zoom
# all looks good


# Secondary field ---------------------------------------------------------
# Plot and treatment must be inspected for NA, NULL, blank or values unspecified in source methods
# We must check for misspellings and revalue levels if needed
# For this, we can use:
# sort(unique(dt$plot))
# and rename levels using:
# levels(dt$plot) <- plyr::revalue(levels(dt$plot),
#                                 c("old_name"="new_name",
#                                   "old_name1"="new_name1"))

# Repeat processes used for primary fields.
# DepthElevation can be 0, but not NaN, NA, NULL or blank only in some records.


# Taxonomic field check ---------------------------------------------------

# No NAs in taxonomic fields, remove all non-organism records
# misspellings check, but not taxonomic cleaning

dt$Species <- dt$Taxon # make a copy
# check that genera are genera, not family names (-idae/eae)
# this returns the record index number if there are any
str_which(dt$Species, 'idae$|eae$')

# check the species list for misspellings or non-BioTIME taxonomic convention names
# Do visual checks before splitting taxa up into the three columns.
sort(unique(dt$Species)) %>% word(., start=2, end=-1)
# check genera
# this keeps IDs adjacent to their same-genus misspellings, but only looking at the last few words to check
sort(unique(word(dt$Species, 1))) 
# check family
sort(unique(dt$Family))

#make a replacement vector, pattern = replacement
replace_g <- c(
  'Drassylus' = 'Drassyllus',
  'Hyposinga' = 'Hypsosinga',
  'Palludiphantes' = 'Palliduphantes',
  'Porhomma' = 'Porrhomma',
  'Tenuiphanthes' = 'Tenuiphantes'
)
replace_f <- c(
  'Theidiidae' = 'Theridiidae'
  #opliones = opiliones but also this is an order?
  #opliones = 61 -> nemastomatidae
  #oplionidae = 99, 646 -> phalangiidae
)
replace_s <- c(
  'nigrum brevisetorum' = 'nigrum' #this is a weird case of females are nigbr and males are either nig or bre
)


# separate taxon names - at the moment Species is 'genus species subspecies'
dt$Genus <- word(dt$Species, 1) # this will just copy over genus
dt$Species <- word(dt$Species, start=2) # this will eliminate any subspecies and just keep the species name

#check again for mispellings
sort(unique(dt$Genus))
sort(unique(dt$Species))

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

