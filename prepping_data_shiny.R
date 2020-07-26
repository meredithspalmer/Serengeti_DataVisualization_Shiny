library(here)
library(overlap)
library(maptools)
library(lubridate)
library(tidyverse)
library(Hmisc)

# Import and merge data, covariates -----------------------------------------------

dat <- read.csv("sample_dat_serengeti.csv")
dat <- dat[dat$season %in% c("S01", "S02", "S03", "S04"),]
dat <- dat[c("subject_id", "season", "site", "capture_date_local", "capture_time_local", "question__species", "question__count_median", "question__standing", "question__resting", "question__moving", "question__eating", "question__interacting", "question__young_present", "pielous_evenness_index")]
names(dat)[c(2, 4:14)] <- c("data.season", "date", "time", "species", "count", "standing", "resting", "moving", "eating", "interacting", "young_present", "evenness")
dat$datetime <- strptime(paste(dat$date, dat$time), "%Y-%m-%d %H:%M:%S")
dat$season <- ifelse(month(dat$datetime) %in% c(6:10), "Dry Season", "Wet Season")

covs <- read.csv("CTCovariates_NEW.csv") #note: some sites have NA for some values
 
covs <- covs[, -c(4,9,12)]
covs_w <- covs; covs_d <- covs; covs_w$season <- "Wet Season"; covs_d$season <- "Dry Season"
covs_w$LION.ENCOUNT.DRY <- NULL; names(covs_w)[5] <- "LION.ENCOUNT"
covs_d$LION.ENCOUNT.WET <- NULL; names(covs_d)[5] <- "LION.ENCOUNT"
covs <- rbind(covs_w, covs_d)
covs$PER.TREE.COV <- covs$PER.TREE.COV/max(covs$PER.TREE.COV, na.rm=T)
names(covs)[1] <- "site"

dat <- merge(dat, covs)

# order dat and calculate time difs 
dat <- dat[order(dat$site, dat$species, dat$datetime),]
dat$index <- paste(dat$site, dat$species)
dat$delta.time.secs <- unlist(tapply(dat$datetime, INDEX = dat$index,
                                     FUN = function(x) c(0, `units<-`(diff(x), "secs"))))

# Format and scale times ----------------------------------------------------------

# set spatial coordinates
coords <- matrix(c(34.83, -2.33), nrow=1) %>%
  sp::SpatialPoints(proj4string=sp::CRS("+proj=longlat +datum=WGS84"))

# specify date format
dat$Date <- as.POSIXct(dat$date, format = "%Y-%m-%d", tz = "Africa/Dar_Es_Salaam")

# convert time to radians (could be done more efficiently with pipe)
dat$radians <- ((hour(dat$datetime)*60 + minute(dat$datetime))/(24*60)) * 2 * pi

# calculate suntime using function from overlap package, and coordinates and dates as formatted above
dat$Time.Sun <- sunTime(dat$radian, dat$Date, coords)

# Drop records outside of operation dates -----------------------------------------

dat$date <- strptime(dat$date, "%Y-%m-%d")
dat <- dat[!dat$date < strptime("2010-08-01", "%Y-%m-%d"),]
dat <- dat[!dat$date > strptime("2012-05-31", "%Y-%m-%d"),]

# take only columns we want/need
dat <- dat[c("site", "subject_id", "data.season", "season", "date", "time", "datetime", "Date", "radians", "Time.Sun", "delta.time.secs", "species", "count", "standing", "resting", "moving", "eating", "interacting", "young_present", "evenness", "ARCX", "ARCY", "HABITAT", "LION.ENCOUNT", "RIV.DIST.M", "KOP.DIST.M", "ROAD.DIST.M", "PER.TREE.COV")]


# Merge with species metadata -----------------------------------------------------

dat$species <- capitalize(as.character(dat$species))
dat[dat$species == "GazelleThomsons",]$species <- "Thomson's Gazelle"
dat[dat$species == "OtherBird",]$species <- "Bird (Unidentified)"
dat[dat$species == "HyenaSpotted",]$species <- "Spotted Hyena"
dat[dat$species == "GazelleGrants",]$species <- "Grant's Gazelle"
dat[dat$species == "GuineaFowl",]$species <- "Guinea Fowl"
dat[dat$species == "InsectSpider",]$species <- "Insect or Spider"
dat[dat$species == "VervetMonkey",]$species <- "Vervet Monkey"
dat[dat$species == "DikDik",]$species <- "Dik Dik"
dat[dat$species == "KoriBustard",]$species <- "Kori Bustard"
dat[dat$species == "LionMale",]$species <- "Lion (Male)"
dat[dat$species == "SecretaryBird",]$species <- "Secretary Bird"
dat[dat$species == "LionFemale",]$species <- "Lion (Female or Cub)"
dat[dat$species == "HyenaStriped",]$species <- "Striped Hyena"
dat[dat$species == "HoneyBadger",]$species <- "Honey Badger"
dat[dat$species == "BatEaredFox",]$species <- "Bat-Eared Fox"
dat[dat$species == "ReptilesAmphibians",]$species <- "Reptiles or Amphibians"
dat$species <- factor(dat$species)


# Export cleaned file -------------------------------------------------------------

write.csv(dat, "basic_serengeti/cleaned_data_for_shiny.csv", row.names=F)
