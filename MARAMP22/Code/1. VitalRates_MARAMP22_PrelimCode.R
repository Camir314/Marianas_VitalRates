#### MARAMP 2022 Vital Rates QC, initial dataframe creations and descriptive stats
### Corinne Amir 
### July 2023

library(dplyr)
library(anytime)
library(ggplot2)
library(ggmap)
library(viridis)
library(ggspatial)
library(ggrepel)
library(stringr)
library(sp)

# setwd("C:/Users/Corinne.Amir/Documents/N drive backup/Arc/Arc_Exports/")
setwd("C:/Users/Corinne.Amir/Documents/Vital Rates/Analysis/MARAMP22/CSV files/")
raw <- read.csv("MARAMP22_VitalRates_07-24-2023.csv")
ll <- read.csv("MARAMP22_VitalRates_LatLong.csv")
effort <- read.csv("MARAMP22_SurveyEffort.csv")
# mari <- read.csv("MARAMP22_VitalRates_patchlevel_CLEAN.csv") # created in this script
# mari_col <- read.csv("MARAMP22_VitalRates_colonylevel_CLEAN.csv") # created in this script


#### QC Data ####
## (OPTIONAL) Remove superfluous columns:
colnames(raw)
raw <- raw %>% select(-c(OID_, TL_SurfA,QC_Check))


## Look for potential issues in the data:
lapply(raw, unique)
# 392 unique genets
# 394 unique ids = ids and genet numbers copied throughout dataset = need unique row 
# only 21 unique TL_date = missing a date
# TL class includes 10 species. Round up to genus
# Check on TagLab vs Arc differences in area and perimeter
# Remove rows based on TL_Note: Out of bounds in 2022
# RM is included in morph code = likely change to "EM"


## Remove or alter rows based on TL_Note:
raw <- raw %>% filter(TL_Note != "Out of bounds in 2022") # remove colonies that could not be tracked into 2022


## QC Morph code:
a <- raw[raw$Morph_Code != "MD" & raw$Morph_Code != "EM" & raw$Morph_Code != "BR" & raw$Morph_Code != "TB" 
        & raw$Morph_Code != "PL" & raw$Morph_Code != "KN",]

raw <- raw %>% mutate(Morph_Code = case_when(Morph_Code == "RM" ~ "EM",
                                             Morph_Code == "EM" ~ "EM",
                                             Morph_Code == "TB" ~ "TB",
                                             Morph_Code == "BR" ~ "BR",
                                             Morph_Code == "KN" ~ "KN",
                                             Morph_Code == "PL" ~ "PL",
                                             Morph_Code == "MD" ~ "MD"))


## Check if TimePt is labelled correctly:
a <- raw %>% group_by(Site, TL_Date, TimePt) %>% summarise(sum(TimePt))
  # GUA-025: 1/1/2015, should be 5/3/2017
  # SAI-012: both collected on 4/22, should be 5/18/2017
raw$TL_Date <- as.factor(raw$TL_Date)   
raw <- raw %>% mutate(TL_Date = recode(TL_Date,"1/1/2017" = "5/3/2017", "4/22/2017" = "5/18/2017"))

raw$TL_Date <- anydate(raw$TL_Date) # Change date format


# Add leading zeros to genet and colony code

raw$TL_id <- str_pad(raw$TL_id, 3, pad = "0")
raw$TL_Genet <- str_pad(raw$TL_Genet, 3, pad = "0")

#### Create additional columns #####

mari <- raw

## Create unique name for all genets: 
mari$Genet_full <- paste(mari$Site,  mari$TL_Genet, mari$TimePt, sep = "_")
mari[11,] # double check

## Create unique name for all genets: 
mari$Patch_full <- paste(mari$Site,mari$TL_id, mari$TimePt,  sep = "_")
mari[11,] # double check


## Roll TL_Class up to genus (consider separating PGRA)
mari <- mari %>% mutate(Genus = case_when(TL_Class == "PMEA" ~ "POCS",
                                          TL_Class == "PVER" ~ "POCS",
                                          TL_Class == "AGLO" ~ "ACSP",
                                          TL_Class == "PGRA" ~ "POCS",
                                          TL_Class == "PLOB" ~ "POSP",
                                          TL_Class == "PLUT" ~ "POSP",
                                          TL_Class == "POCS" ~ "POCS",
                                          TL_Class == "MOSP" ~ "MOSP",
                                          TL_Class == "ACSP" ~ "ACSP",
                                          TL_Class == "POSP" ~ "POSP"))
lapply(mari, unique) # double check


## Add Lat and Long
ll <- ll %>% select(-c(Region,Year)) %>% rename(Site = ESD.Site.Name)

mari <- left_join(ll, mari)


## Add m2 surveyed (collected from tracking spreadsheet)

mari <- left_join(mari, effort)


# Turn TimePt into Year
mari$Year <- str_sub(mari$TL_Date,1,4)
mari %>% group_by(Site, TL_Date, Year) %>% summarise() # QC check


## Add area:perimeter ratio

mari$area_perim <- mari$Shape_Area/mari$Shape_Leng


## Add region code

mari <- mari %>% mutate(Region = case_when(Island == "MAU" ~ "North",
                                           Island == "PAG" ~ "North",
                                           Island == "ASC" ~ "North",
                                           Island == "SAI" ~ "South",
                                           Island == "GUA" ~ "South"))


#### Descriptive Tables using patch data ####

a <- mari %>% group_by(Site, Annotator, TimePt) %>% summarise(n = n()) # Site-Years with the most and least patches
# OCC-SAI-009_2022: 352
# OCC-MAU-002_2017: 385
# OCC-GUA-015_2022: 25
# OCC-PAG-013_2017: 59


mari %>% group_by(Annotator) %>% summarise(n = n()) # Total patches surveyed by each annotator 
# JC: 1475 / 7 = 211 patches per Site-Year
# CA: 1127 / 9 = 125
# MSL: 539 / 4 = 135
# IGB: 428 / 4 = 107


mari %>% group_by(Genus) %>% summarise(n = n()) # How many total patches per genus
# POSP: 2702
# ACSP: 402
# POCS: 383
# MOSP: 82

mari %>% group_by(Genus, TimePt) %>% summarise(n = n()) # Patches per species over time (account survey effort)
# POSP: 1083 -> 1356 -> 263
# ACSP: 233 -> 145 -> 24
# POCS: 183 -> 90 -> 110
# MOSP: 75 -> 6 -> 1


a<-mari %>% group_by(Site, TimePt) %>%    # Proportion of area surveyed to area annotated
    # summarise(ant_area = sum(Shape_Area)) %>% 
    summarise(surv_area = length(unique(Quadrat)))


a <- mari %>% group_by(Site, Genus, TimePt) %>% summarise(n = log(mean(TL_Area))) # Mean patch size

a <- mari %>% group_by(Genus, Year, Island) %>% summarise(n = n())

a <- mari %>% group_by(Year,Genus) %>% summarise(n = n())

#### Consolidate into colony dataframe ####

# Create colony dataframe:
mari_col <- mari %>% dplyr::select(Genet_full, TL_Area, TL_Perim, Shape_Leng, Shape_Area, area_perim) 
mari_meta <- mari %>% dplyr::select(Genet_full, Island, Site, TimePt, Year, TL_Date, Latitude,Longitude,
                             Genus, TL_Class, TL_Genet, Quadrat, Effort) %>%
                      distinct()
mari_col <- aggregate(.~Genet_full, data = mari_col, sum)
mari_col <- left_join(mari_meta,mari_col)

              #group_by(Genet_full) %>% summarise(nrow(patches)) # Add in patch count 

# Total genets per Site-Year (ONLY colonies that survived 2+ time points):
mari_col$Site_Genet <- paste(mari_col$Site,mari_col$TL_Genet,  sep = "_") # use this column to filter



#### Descriptive Tables using colony data ####
# Total genets per Site-Year:
a <- mari_col %>% group_by(Site, TimePt, Genus) %>% 
        summarise(nColonies = n())

a <- mari_col %>% filter(TimePt !=0) %>% # look for recruits
                  group_by(Site_Genet) %>% 
                  filter(n()==1) %>% 
                  group_by(Site, Year,Genus) %>%
                  summarise(nColonies = n()) 

a <- mari_col %>% filter(TimePt !=2) %>% # look for growth/shrinkage/fission/fusion events
                  group_by(Site_Genet) %>% 
                  filter(n()>1) %>% 
                  group_by(Site, Genus) %>%
                  summarise(nColonies = n()) %>%
                  mutate(actual = nColonies/2)

a <- mari_col %>% filter(Island == "MAU" & TimePt !=0) %>% # find #transitions for additional Maug time point 
                  droplevels() %>%
                  group_by(Site_Genet) %>% 
                  filter(n()>1) %>% 
                  group_by(Site, Genus) %>%
                  summarise(nColonies = n()) %>%
                  mutate(actual = nColonies/2)

# Total area annotated per Site-Year:
b <- mari_col %>% group_by(Site, TimePt) %>% 
        summarise(tot_area = sum(Shape_Area))

    # Overall total:
    b <- sum(b$tot_area) # 60.53m2

# Add together:
aa <- left_join(a,b)





#### Create dataframe for change in colony planar area ####
mari_17 <- mari_col %>% filter(Year == 2017) %>% 
           rename(c(TL_Area_2017 = TL_Area, TL_Perim_2017 = TL_Perim, area_perim_2017 = area_perim,
                    Shape_Leng_2017 = Shape_Leng, Shape_Area_2017 = Shape_Area)) %>%
           select(-c(TimePt, Year, TL_Date, Genet_full))
mari_22 <- mari_col %>% filter(Year == 2022) %>% 
           rename(c(TL_Area_2022 = TL_Area, TL_Perim_2022 = TL_Perim, area_perim_2022 = area_perim,
           Shape_Leng_2022 = Shape_Leng, Shape_Area_2022 = Shape_Area)) %>%
           select(-c(TimePt, Year, TL_Date, Genet_full))

mari_change <- left_join(mari_17, mari_22)
mari_change <- na.omit(mari_change) # a = 423, this = 439 

a <- mari_change %>% group_by(Site_Genet) %>%
  mutate(TL_Area_Change = TL_Area_2022 - TL_Area_2017) %>%
  mutate(TL_Perim_Change = TL_Perim_2022 - TL_Perim_2017) %>%
  mutate(Shape_Area_Change = TL_Perim_2022 - TL_Perim_2017) %>%
  mutate(Shape_Length_Change = TL_Perim_2022 - TL_Perim_2017)
  



# Deal with dispute in number of colonies in mult time points:
a <- mari_change %>% group_by(Site, Genus)%>%
  summarise(nColonies = n())
a <- mari_col %>% filter(TimePt !=2) %>% # adding TimePt filter shows just 2014-2017 transition for Maug Sites
  group_by(Site_Genet) %>% 
  filter(n()>1) %>% 
  group_by(Site, Genus) %>%
  summarise(nColonies = n()) %>%
  mutate(actual = nColonies/2)
 
#### Preliminary Plots ####

# Size frequency distribution (patches)
ggplot(data = mari %>% filter(Site =="OCC-PAG-006"), 
       aes(x=log(TL_Area))) + # Total
  geom_histogram() +
  facet_wrap(vars(Genus,Year), scales = "free_y", nrow = 3) 


# Kernel Density Plots (patches)
# mline <- mari %>% group_by(Site, Genus, Year) %>% summarise(Mean = log(mean(TL_Area))) # mean value by site
mline <- mari %>% group_by(Region, Genus, Year) %>% summarise(Mean = log(mean(Shape_Area))) # mean value by region


aa <-ggplot(data = mari %>% filter(Region == "South" & Year != "2014") %>% droplevels(), # By Site (plug and chug)
       aes(x=log(Shape_Area), group = Year, fill = Year)) +
  # geom_histogram(aes(y = ..density..),alpha = 0.4, binwidth = .4) + overlay histogram (it isnt raw counts, its proportion)
  geom_density() +
  # scale_fill_manual(values = alpha(c("#F1A340", "#998EC3"),0.75)) +
  scale_fill_manual(values = alpha(c("#F1A340", "#998EC3","aquamarine1"),0.75)) + # Maug sites
  facet_wrap(vars(Genus), nrow = 1, drop = F) + 
  geom_vline(data = mline %>% filter(Region == "South"& Year != "2014") ,
             aes(xintercept = Mean, color = Year),
             size = 1.1 ) +
  # scale_color_discrete(c("chocolate", "darkcyan", "chartreuse3")) +
  ggtitle("South/Populated") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())



# Boxplot for change in area


# Other stuff I wanna plot
mari %>% group_by(Site, TimePt) %>% summarise(area = sum(Shape_Area)) %>%
  ggplot(aes(x = Site, y = area, fill = as.factor(TimePt))) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~TimePt)


# Create Dataframe for qqplot (use Tom's code)
mari_0 <- mari_col %>% filter(TimePt == 0)
mari_1 <- mari_col %>% filter(TimePt == 1)
mari_2 <- mari_col %>% filter(TimePt == 2)

ggplot(mari_col, aes(x = ))
stat_qq()

#### Plot Site Map ####
  
setwd('C:/Users/Corinne.Amir/Documents/Github/ncrmp_common_maps')
utm = read.csv('data/misc/ncrmp_utm_zones.csv')                               # Load UTM zones (double check they're correct)
  

islands = c("GUA", "SAI", "TIN", "ROT", "MAU", "ASC", "FDP","AGR","AGU", "ALA","SAR","PAG","GUG") 
                       
utm_i = utm %>% subset(Island_Code %in%  islands)                                  # Subset required utm zone(s) (incorporate into function like Kisei later)
utm_i = utm %>% subset(Island_Code == islands)


load('data/gis_island_boundaries/ncrmp_islands_shp.RData')                    # Load island boundaries - shapefile vertices that have x,y coordinates
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"    # Sets a coordinate reference system to the above shapefile --- not sure how/if this works


ISL_this = ISL_bounds[which(ISL_bounds$ISLAND_CD %in% islands),]  
  
ggmap::register_google("AIzaSyDpirvA5gB7bmbEbwB1Pk__6jiV4SXAEcY")

map <- get_map(location = c(mean(mari$Longitude, na.rm = T), mean(mari$Latitude, na.rm = T)), 
               maptype = "satellite",
               zoom = 6, 
               force = T)

mari_n <- mari %>% filter(Island == "MAU" | Island == "ASC")
map_n <- get_map(location = c(mean(mari_n$Longitude, na.rm = T), mean(mari_n$Latitude, na.rm = T)), 
                 maptype = "satellite",
                 zoom = 6, 
                 force = T)

mari_pag <- mari %>% filter(Island == "PAG" )
map_pag <- get_map(location = c(mean(mari_pag$Longitude, na.rm = T), mean(mari_pag$Latitude, na.rm = T)), 
                 maptype = "satellite",
                 zoom = 6, 
                 force = T)

mari_s <- mari %>% filter(Island == "GUA" | Island == "SAI")
map_s <- get_map(location = c(mean(mari_s$Longitude, na.rm = T), mean(mari_s$Latitude, na.rm = T)), 
                   maptype = "satellite",
                   zoom = 6, 
                   force = T)

dev.off()
mari_full <- ggmap(map) + 
             geom_polygon(data = ISL_this,
                          aes(long, lat, group=group),
                          fill = "darkgrey",
                          color = NA,
                          alpha = 0.9) +
             geom_label_repel(data = mari %>% distinct(Site, .keep_all = T), 
                              aes(Longitude, Latitude, label = Site),
                              box.padding = .8,
                              size = 4.5,
                              min.segment.length = .5,
                              segment.color = "white",
                              segment.size = 0.3,
                              nudge.x = .2) +
             geom_spatial_point(data = mari, aes(Longitude, Latitude), 
                                size = .5, 
                                color = "red", 
                                crs = 4326) +
             scale_x_continuous(limits=c(144.1,146.6)) +
             scale_y_continuous(limits=c(13.3,20.3)) 
             # annotation_scale(location = "br")
             # annotation_scale(location = "bl", width_hint = 0.3, height = unit(0.55, "cm"), text_cex = 1) + 

# ggsave("C:/Users/Corinne.Amir/Documents/Vital Rates/Analysis/MARAMP22/MARAMP22_map_full.pdf")
ggsave("C:/Users/Corinne.Amir/Documents/Vital Rates/Analysis/MARAMP22/MARAMP22_map.jpg",
       width = 5, height = 12)



mari_1 <- ggmap(map_n) + 
  geom_polygon(data = ISL_this,
               aes(long, lat, group=group),
               fill = "darkgrey",
               color = NA,
               alpha = 0.9) +
  geom_label_repel(data = mari_n %>% distinct(Site, .keep_all = T), 
                   aes(Longitude, Latitude, label = Site),
                   box.padding = .8,
                   size = 3.5,
                   min.segment.length = .7,
                   segment.color = "white",
                   segment.size = 0.3,
                   nudge.x = .2) +
  geom_spatial_point(data = mari_n, aes(Longitude, Latitude), 
                     size = 1.7, 
                     color = "red", 
                     crs = 4326) +
  scale_x_continuous(limits=c(145.1,145.5)) +
  scale_y_continuous(limits=c(19.6,20.1)) 
ggsave("C:/Users/Corinne.Amir/Documents/Vital Rates/Analysis/MARAMP22/MARAMP22_map_n.jpg")

mari_2 <- ggmap(map_pag) + 
  geom_polygon(data = ISL_this,
               aes(long, lat, group=group),
               fill = "darkgrey",
               color = NA,
               alpha = 0.9) +
  geom_label_repel(data = mari_pag %>% distinct(Site, .keep_all = T), 
                   aes(Longitude, Latitude, label = Site),
                   box.padding = .8,
                   size = 4,
                   min.segment.length = .7,
                   segment.color = "white",
                   segment.size = 0.3,
                   nudge.x = .2) +
  geom_spatial_point(data = mari_pag, aes(Longitude, Latitude), 
                     size = 3, 
                     color = "red", 
                     crs = 4326) +
  scale_x_continuous(limits=c(145.6,145.9)) +
  scale_y_continuous(limits=c(17.97,18.23)) 
ggsave("C:/Users/Corinne.Amir/Documents/Vital Rates/Analysis/MARAMP22/MARAMP22_map_pag.jpg")


mari_3 <- ggmap(map_s) + 
  geom_polygon(data = ISL_this,
               aes(long, lat, group=group),
               fill = "darkgrey",
               color = NA,
               alpha = 0.9) +
  geom_label_repel(data = mari_s %>% distinct(Site, .keep_all = T), 
                   aes(Longitude, Latitude, label = Site),
                   box.padding = 1.2,
                   size = 2.7,
                   min.segment.length = .8,
                   segment.color = "white",
                   segment.size = 0.3,
                   nudge.x = .2) +
  geom_spatial_point(data = mari_s, aes(Longitude, Latitude), 
                     size = .5, 
                     color = "red", 
                     crs = 4326) +
  scale_x_continuous(limits=c(144.4,145.8)) +
  scale_y_continuous(limits=c(13.2,15.4)) 

#### Run Statistics ####
#### Kolmogorov-Smirnov tests ####
# Break out individual sites, groups and years that look different based on kernel density plots/SFDs
# Sites that have enough patches:
  # For POSP: all sites except GUA-015
  # For ACSP: maybe SAI-009
  # For POCS: MAU-019 (but 2017 only has three corals)

# First, lets not split by site:
posp0 <- mari %>%
  filter(Year %in% "2017",
         Genus == "POSP")
posp1 <- mari %>%
  filter(Year %in% "2022",
         Genus == "POSP")

acsp0 <- mari %>%
  filter(Year %in% "2017",
         Genus == "ACSP")
acsp1 <- mari %>%
  filter(Year %in% "2022",
         Genus == "ACSP")

pocs0 <- mari %>%
  filter(Year %in% "2017",
         Genus == "POCS")
pocs1 <- mari %>%
  filter(Year %in% "2022",
         Genus == "POCS")

# Second, split by populated vs unpopulated:
# POSP
unpop.posp0 <- mari %>% 
  filter(Island != "GUA" & Island != "SAI") %>%
  filter(Year %in% "2017",
  Genus == "POSP")
unpop.posp1 <- mari %>% 
  filter(Island != "GUA" & Island != "SAI") %>%
  filter(Year %in% "2022",
         Genus == "POSP")

pop.posp0 <- mari %>% 
  filter(Island == "GUA" | Island == "SAI") %>%
  filter(Year %in% "2017",
         Genus == "POSP")
pop.posp1 <- mari %>% 
  filter(Island == "GUA" | Island == "SAI") %>%
  filter(Year %in% "2022",
         Genus == "POSP")

#ACSP
unpop.acsp0 <- mari %>% 
  filter(Island != "GUA" & Island != "SAI")%>%
  filter(Year %in% "2017",
         Genus == "ACSP") 
unpop.acsp1 <- mari %>% 
  filter(Island != "GUA" & Island != "SAI") %>%
  filter(Year %in% "2022",
         Genus == "ACSP") 

pop.acsp0 <- mari %>% 
  filter(Island == "GUA" | Island == "SAI") %>%
  filter(Year %in% "2017",
         Genus == "ACSP") # Guam only contributes 1 coral
pop.acsp1 <- mari %>% 
  filter(Island == "GUA" | Island == "SAI") %>%
  filter(Year %in% "2022",
         Genus == "ACSP") # Guam only contributes 1 coral

# POCS
unpop.pocs0 <- mari %>% 
  filter(Island != "GUA" & Island != "SAI")%>%
  filter(Year %in% "2017",
         Genus == "POCS") # only 27 corals, none from Pagan
unpop.pocs1 <- mari %>% 
  filter(Island != "GUA" & Island != "SAI") %>%
  filter(Year %in% "2022",
         Genus == "POCS") # Pagan only contributes 2 corals

pop.pocs0 <- mari %>% 
  filter(Island == "GUA" | Island == "SAI") %>%
  filter(Year %in% "2017",
         Genus == "POCS") 
pop.pocs1 <- mari %>% 
  filter(Island == "GUA" | Island == "SAI") %>%
  filter(Year %in% "2022",
         Genus == "POCS") # only 26 corals

# Kolmogorov-Smirnov tests:
ks.test(posp0$TL_Area, posp1$TL_Area) # D = 0.064877, p-value = 0.009682
ks.test(acsp0$TL_Area, acsp1$TL_Area) # D = 0.22441, p-value = 0.0002861
ks.test(pocs0$TL_Area, pocs1$TL_Area) # D = 0.1688, p-value = 0.04478

ks.test(unpop.posp0$TL_Area, unpop.posp1$TL_Area) # D = 0.094853, p-value = 0.004505
ks.test(unpop.acsp0$TL_Area, unpop.acsp1$TL_Area) # D = 0.38083, p-value = 0.0001103
ks.test(unpop.pocs0$TL_Area, unpop.pocs1$TL_Area) # D = 0.56427, p-value = 8.995e-07

ks.test(pop.posp0$TL_Area, pop.posp1$TL_Area) # D = 0.206, p-value = 2.83e-11
ks.test(pop.acsp0$TL_Area, pop.acsp1$TL_Area) # D = 0.31988, p-value = 0.001423
ks.test(pop.pocs0$TL_Area, pop.pocs1$TL_Area) # D = 0.32456, p-value = 0.01164

ks.test(pop.posp0$TL_Area, unpop.posp0$TL_Area) # D = 0.23539, p-value < 2.2e-16
ks.test(pop.acsp0$TL_Area, unpop.acsp0$TL_Area) # D = 0.18831, p-value = 0.1432: ACSP distribution NOT different between populated and unpopulated islands before bleaching event!
ks.test(pop.pocs0$TL_Area, unpop.pocs0$TL_Area) # D = 0.50975, p-value = 6.377e-05

ks.test(pop.posp1$TL_Area, unpop.posp1$TL_Area) # D = 0.10806, p-value = 0.002973
ks.test(pop.acsp1$TL_Area, unpop.acsp1$TL_Area) # D = 0.58855, p-value = 3.7e-10
ks.test(pop.pocs1$TL_Area, unpop.pocs1$TL_Area) # D = 0.37908, p-value = 0.0004607


#### Anovas ####
# Create dataframes
posp <- mari %>% filter(Genus == "POSP")
acsp <- mari %>% filter(Genus == "ACSP")
pocs <- mari %>% filter(Genus == "POCS")



# Is mean size different between Islands 
# Assumptions
bartlett.test(TL_Area~as.factor(Year), data = posp) # significant: unequal variances
bartlett.test(TL_Area~as.factor(Year), data = acsp) # significant
bartlett.test(TL_Area~as.factor(Year), data = pocs) # significant
tapply(posp$TL_Area, list(as.factor(posp$Year)), shapiro.test) # all significant: non-normal distribution

bartlett.test(TL_Area~interaction(as.factor(OBS_YEAR), DEPTH_BIN), data = swa) # significant
tapply(swa$AVG_DEPTH, list(swa$OBS_YEAR), shapiro.test) # all significant
tapply(swa$AVG_DEPTH, list(swa$DEPTH_BIN), shapiro.test) # all significant


#two-way anova
summary(aov(CCA~as.factor(Year)*TRT, data = cca)) #trt and year are significant but interaction is not
TukeyHSD(aov(CCA~as.factor(Year), data = cca)) 
# 1709 > 1510, 1606, 1609
TukeyHSD(aov(CCA~TRT, data = cca)) 
# REM > BYSP, RAN, XSP


#### Format dataframe into archive csv file ####
# Add in Island_Code, DataorError, Error_Category
archive <- mari_col
colnames(mari)
head(archive)

archive <- archive %>% filter(Genus != "MOSP") # Remove MOSP because prevalence is too low


# Remove colonies <19cm2 in all time points
AdSize=(2.5^2*pi) 
`%notin%` <- Negate(`%in%`) 

t0 <- mari_col %>% filter(Year == "2014" & TL_Area < AdSize & Shape_Area < .0019) %>% 
  dplyr::select(c(Site, Genus, TL_Genet, Site_Genet))
t1 <- mari_col %>% filter(Year == "2017" & TL_Area < AdSize & Shape_Area < .0019) %>% 
  dplyr::select(c(Site, Genus, TL_Genet, Site_Genet)) 
t2 <- mari_col %>% filter(Year == "2022" & TL_Area < AdSize & Shape_Area < .0019) %>% 
  dplyr::select(c(Site, Genus, TL_Genet, Site_Genet))

step1 <- inner_join(t0,t1)
step1 <- step1 %>% distinct() 

step2 <- inner_join(t1,t2)
step2 <- step2 %>% distinct() 

smallcol <- rbind(step1,step2) %>% distinct()

archive<-subset(archive, Site_Genet %notin% smallcol$Site_Genet)



archive$Site <- sub("-", "_", archive$Site);archive$Site <- sub("-", "_", archive$Site) #twice for both underscores
archive$Error_Category <- "Growth Data"
archive$DataorError <- "DATA"
archive$Genet_full <- paste(archive$Site, archive$TL_Genet, archive$TimePt, sep = "_")
archive <- rename(archive, "Island_Code" = "Island")
archive <- archive %>% mutate(Island = case_when(Island_Code == "GUA" ~ "Guam",
                                                Island_Code == "MAU" ~ "Maug",
                                                Island_Code == "PAG" ~ "Pagan",
                                                Island_Code == "ASC" ~ "Asuncion",
                                                Island_Code == "SAI" ~ "Saipan"))
archive <- rename(archive, "Genus_Code" = "Genus")
archive <- archive %>% mutate(Genus = case_when(Genus_Code == "POSP" ~ "Porites sp.",
                                                Genus_Code == "MOSP" ~ "Montipora sp.",
                                                Genus_Code == "POCS" ~ "Pocillopora sp.",
                                                Genus_Code == "ACSP" ~ "Acropora sp."))
archive <- rename(archive, "Spec_Code" = "TL_Class")
archive <- rename(archive, "Date" = "TL_Date")
archive <- rename(archive, "ColonyName" = "Genet_full")
archive <- rename(archive, "Shape_Length" = "Shape_Leng")

archive <- archive %>% dplyr::select(-c(TL_Genet, TL_Cx, TL_Cy, TL_Area, TL_Perim, TL_Note, 
                                        Quadrat, Morph_Code, TimePt, Annotator, area_perim))
                    
archive <- archive %>% dplyr::select(Site, Island, Island_Code, Latitude, Longitude, Date, 
                              DataorError, Error_Category, ColonyName, Spec_Code, Genus,
                              Genus_Code, Shape_Length, Shape_Area)

head(archive)

#### Export Data ####
setwd('C:/Users/Corinne.Amir/Documents/Vital Rates/Analysis/MARAMP22/CSV files')
write.csv(mari,"MARAMP22_VitalRates_CLEAN.csv",row.names = F)
write.csv(mari_col,"MARAMP22_VitalRates_colonylevel_CLEAN.csv",row.names = F)
write.csv(archive,"MARAMP22_VitalRates_colonylevel_archive.csv",row.names = F)

