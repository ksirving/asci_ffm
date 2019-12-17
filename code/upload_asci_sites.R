
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)
library(mapview)
library(janitor)
library(lubridate)
library(raster)

#  statewide study - asci - ffm

# bugs <- read.csv("/Users/katieirving/Documents/Projects/UC Davis/data/Taxonomy_Ryan.csv")
# head(bugs)
#  upload algae data - asci scores downloaded from https://sites.google.com/view/asci/results - Susie

setwd("/Users/katieirving/Documents/git/asci_ffm-master")

asci_scor <- read.csv("input_data/asci.scores_dec2019.csv", header=T)
head(asci_scor)
#  use MMI (d=diatom, sha=soft algae, hybrid = both)bugs <- read_csv(file = "/Users/katieirving/Documents/Projects/UC Davis/data/Taxonomy_Ryan.csv") %>% 


# subset to only sites and hybrid MMI score
# asci_scor <- asci_scor[,c(1,7)]
colnames(asci_scor)[1] <- "SampleID"

#  upload site details - for coordinates

algae_raw <- read.csv("input_data/algae.bug.data.10172019.csv", header = T)
head(algae_raw)
names(algae_raw)
dim(algae_raw)# 130753     26

#  get coords
algae_sites <- algae_raw[,c(1,17,18)]
head(algae_sites)
dim(algae_sites)


#  merge coords with asci scores
head(asci_scor)
dim(asci_scor)  # 2588    2
str(asci_scor)


asci_scor_sites <- merge(algae_sites, asci_scor, by.x="SampleID_old", by.y="SampleID")
head(asci_scor_sites)
dim(asci_scor_sites) # 128764      4
str(algae_sites)
#  lose ~2K Samples

asci_scor_sites <- asci_scor_sites[!duplicated(asci_scor_sites),]
dim(asci_scor_sites) #2625    4 - more sites here than the asci scores df as some samples have 2x reps
#  remmove NAs
sum(is.na(asci_scor_sites))
asci_scor_sites <- na.omit(asci_scor_sites)


#  remove duplicates

head(asci_scor_sites)
write.csv(asci_scor_sites, "/Users/katieirving/Documents/git/asci_ffm-master/output_data/asci_scores_coords.csv")

#  split date - station IDs etc

?separate
# use lubridate/tidyr to fix dates to a consistent format
algae <- separate(asci_scor_sites, col = SampleID_old , into=c("AA", "BB", "YYYY"), remove = T) %>% 
  mutate(AA=as.integer(AA),
         BB=as.integer(BB)) %>% 
  mutate(
    "MM"=case_when(
      AA > 12 ~ BB,
      AA < 13 ~ AA),
    "DD" = case_when(
      AA > 12 ~ AA,
      AA < 13 ~ BB),
    "YYYY" = as.integer(YYYY),
    "sampledate" = ymd(paste0(YYYY,"-", MM, "-",DD))
  ) %>% 
  dplyr::select(StationCode:YYYY, MM, DD, sampledate, everything(), -c(AA, BB))

# how many missing SampleID's?: 7991
sum(is.na(bugs$SampleID))

# how many missing Sampledates?
sum(is.na(bugs$sampledate))

coords <- asci_scor_sites[, 2:3]
#  coords into numeric
asci_scor_sites$Latitude <- as.numeric(as.character(asci_scor_sites$Latitude))
asci_scor_sites$Longitude <- as.numeric(as.character(asci_scor_sites$Longitude))

#  remove NAs
sum(is.na(asci_scor_sites)) #50
asci_scor_sites <- na.omit(asci_scor_sites)

#  spatial point df
# coordinates(asci_scor_sites) <- c("Latitude","Longitude")
asci_scor_sites <- asci_scor_sites %>% 
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=F) # define coords to make spatial

plot(asci_scor_sites)
save(asci_scor_sites, file="output_data/asci_scores_coords.RData")


# load(file="/Users/katieirving/Documents/git/bmi_ffm_links/data_output/bmi_trc_metrics_safit1.rda")
# head(bmi_trc)
# str(bmi_trc)

#  component metrics for Algae? only OoverE & MMI for diatoms and soft bodied

#  bug data

load(file="/Users/katieirving/Documents/git/bmi_ffm_links/data_output/bmi_cleaned_all.rda")

bmi_sites <- bmi_clean[,c(3,6:7)]
# dim(bmi_sites) # 310216      3
bmi_sites2 <- distinct(bmi_sites)
write.csv(bmi_sites2, "/Users/katieirving/Documents/Projects/CEFF/GIS/bmi_sites.csv") 
# dim(bmi_sites2) # 5627    3

bmi_sites2 <- bmi_sites2 %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) # define coords to make spatial


#  compare algae sites with bmi sites
plot(bmi_sites2)
plot(asci_scor_sites)
head(asci_scor_sites)
head(bmi_sites2)
write.csv(asci_scor_sites, "/Users/katieirving/Documents/Projects/CEFF/GIS/asci_sites.csv") 

#  pair sites
#  distance between sites

install.packages("spatstat")
# library(spatstat)
# 
# install.packages("geosphere")
# library(geosphere)
# 
# dists <- distHaversine(bmi_sites, asci_sites)

#  check same crs
st_crs(bmi_sites2)
st_crs(asci_scor_sites)

?st_join
?inner_join

test_join <- st_join(bmi_sites2, asci_scor_sites, join=st_within, left=F)
head(test_join)




