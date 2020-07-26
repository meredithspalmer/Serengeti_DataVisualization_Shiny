# Serengeti Camera Trap Data Shiny App

library(tidyverse); library(shiny); library(shinythemes); library(reshape)
library(overlap); library(plotly); library(here); library(scales); library(plyr)
library(shinydashboard); library(leaflet); library(camtrapR); library(dplyr)
library(sp); library(rgdal); library(broom); library(viridis); library(stringr)
library(ggmap); library(magrittr); library(vroom); library(sf); library(lubridate)
library(unmarked)

# setwd() creates problem when trying to publish to shiny.io, so don't run it
# setwd("~/Desktop/Grad School/Code/Serengeti_Shiny/basic_serengeti")

# no scientific notation and round to 2 decimals
options(scipen = 999) #, digits = 2)


# Data import -------------------------------------------------------------

# import shapefile
grid <- st_read("SerengetiGridLatLong.shp") 
grid$site <- as.character(grid$site)

# import record table 
dat <- read.csv("cleaned_data_for_shiny.csv")
dat$Date <- as.Date(dat$Date)
dat$count <- as.character(dat$count)
dat[dat$count == "11-50",]$count <- "30"
dat[dat$count == "51+",]$count <- "75"
dat$count <- as.numeric(dat$count)

# juveniles, behaviors
dat$juveniles <- ifelse(dat$young_present >= 0.02, "Juveniles", "No_Juveniles")
dat$standing_yn <- ifelse(dat$standing >= 0.5, "Standing", "Not_Standing")
dat$eating_yn <- ifelse(dat$eating > 0.02, "Eating", "Not_Eating")
dat$interacting_yn <- ifelse(dat$interacting > 0, "Interacting", "Not_Interacting")
dat$moving_yn <- ifelse(dat$moving >= 0.02, "Moving", "Not_Moving")
dat$resting_yn <- ifelse(dat$resting >= 0.02, "Resting", "Not_Resting")

# strip just month
dat$Month <- month(dat$Date)

# import camera metadata
camera_metadata <- dat[c("site", "ARCX", "ARCY", "LION.ENCOUNT", "RIV.DIST.M", "KOP.DIST.M", 
                         "ROAD.DIST.M", "PER.TREE.COV")]
names(camera_metadata) <- c("site", "X.Coord", "Y.Coord", "Lion_Density", "River_Distance", 
                            "Kopje_Distance", "Road_Distance", "Percent_Tree Cover")
camera_metadata$River_Distance <- scale(camera_metadata$River_Distance)
camera_metadata$Kopje_Distance <- scale(camera_metadata$Kopje_Distance)
camera_metadata$Road_Distance <- scale(camera_metadata$Road_Distance)
camera_metadata$site <- as.character(camera_metadata$site)
camera_metadata <- camera_metadata[!duplicated(camera_metadata),]

# --> at SOME POINT, fix issue with there being different wet and dry season values for lion density
# FOR NOW, average to single value 
camera_metadata <-ddply(camera_metadata, .(site, X.Coord, Y.Coord, River_Distance, Kopje_Distance, Road_Distance, `Percent_Tree Cover`), summarise, Lion_Density = mean(Lion_Density))

# also one issue with moved site 
x <- camera_metadata[camera_metadata$site == "C07",]
x <- ddply(x, .(site, X.Coord, Y.Coord), summarise, River_Distance = mean(River_Distance), Kopje_Distance = mean(Kopje_Distance), Lion_Density = mean(Lion_Density), Road_Distance = mean(Road_Distance), `Percent_Tree Cover` = mean(`Percent_Tree Cover`))
camera_metadata <- camera_metadata[!camera_metadata$site == "C07",]
camera_metadata <- rbind(camera_metadata, x)

# specify seasons for each month-year
seasons <- data.frame(Month = c(1:12), 
                      Season = c(rep("Wet Season", 5), rep("Dry Season", 5), rep("Wet Season", 2)))
                       
# import camera operation spreadsheet
camera_operation <- read.csv("SER_S1-12_siterolleffortexpanded.csv")
camera_operation["date"] <- lapply(camera_operation["date"], as.Date)
camera_operation <- camera_operation[camera_operation$site %in% unique(dat$site),]

# Define timeplot function ------------------------------------------------

timeplot <-function (A, n.grid = 128, kmax = 3, linecol = "#00BFC4",  ...) {
  bwA <- getBandWidth(A, kmax = kmax)
  
  xsc <- 24/(2 * pi)
  xxRad <- seq(0, 2 * pi, length = n.grid)
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  
  ylim <- c(0, max(densA))
  plot(0, 0, type = "n", ylim = ylim, xlim = range(xx), xlab = "Time", 
       ylab = "Activity", xaxt = "n", ...)
  axis(1, at = c(0, 6, 12, 18, 24), labels = c("Midnight", "Sunrise", "Noon", "Sunset", "Midnight"))
  lines(xx, densA, lty = 1, col = linecol, lwd = 2)
  return(invisible(list(x = xx, densityA = densA)))
}


# Define overlap function -------------------------------------------------

overlapPlot2 <- function (A, B, xscale = 24, linetype = c(1, 1), linecol = c("#F8766D", "#00BFC4"),  
                          linewidth = c(2, 2), n.grid = 128, kmax = 3, adjust = 1, ...) {
  bwA <- getBandWidth(A, kmax = kmax)/adjust
  bwB <- getBandWidth(B, kmax = kmax)/adjust
  if (is.na(bwA) || is.na(bwB)) 
    stop("Bandwidth estimation failed.")
  xsc <- if (is.na(xscale))
    1
  else xscale/(2 * pi)
  xxRad <- seq(0, 2 * pi, length = n.grid)
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  densB <- densityFit(B, xxRad, bwB)/xsc
  densOL <- pmin(densA, densB)
  ylim <- c(0, max(densA, densB))
  plot(0, 0, type = "n", ylim = ylim, xlim = range(xx), xlab = "Time", ylab = "Density", xaxt = "n", ...)
  if (is.na(xscale)) {
    axis(1, at = c(0, pi/2, pi, 3 * pi/2, 2 * pi), labels = c("0", expression(pi/2), expression(pi), 
                                                              expression(3 * pi/2), expression(2 * pi)))
  }
  else if (xscale == 24) {
    axis(1, at = c(0, 6, 12, 18, 24), labels = c("Midnight", "Sunrise", "Noon", "Sunset", "Midnight"))
  }
  else {
    axis(1)
  }
  lines(xx, densA, lty = linetype[1], col = linecol[1], lwd = linewidth[[1]])
  lines(xx, densB, lty = linetype[2], col = linecol[2], lwd = linewidth[[2]])
  return(invisible(list(x = xx, densityA = densA, densityB = densB)))
}


# Define RAI functions -----------------------------------------------------

# define RAI calculation function - using record table that has ALREADY BEEN SUBSET
rai.calculate <- function(record.table.subset, camop, start.date, end.date) {
  
  # calculate how long the camera was functioning in that time period
  
  # change start and end date to character
  start.date <- as.character(start.date)
  end.date <- as.character(end.date)
  daterange <- interval(start.date, end.date)
  
  # sum rows within specified dates (there are 1s when camera was operating, NA when not)
  camop <- camop[which(camop$date %within% daterange),]
  camop$date <- as.character(camop$date)
  camop <- camop %>% 
    dplyr::group_by(site) %>%
    dplyr::summarise(Operation = dplyr::n()) 
  camop$site <- as.character(camop$site)
  
  # calculate number of observations of each classification type at each camera
  record_count <- record.table.subset %>%
    dplyr::group_by(site) %>%
    dplyr::summarise(Detections = n(), # counts number of observations of each species
                     TotalCounts = sum(count)) # total number of animals observed
  record_count$site <- as.character(record_count$site)
  
  # join camera operation dates and observations
  RAI.table <- left_join(camop, record_count)
  
  # replace NA with 0 
  RAI.table[is.na(RAI.table)] <- 0
  
  # calculate RAI
  RAI.table$RAI.det <- RAI.table$Detections / RAI.table$Operation  
  RAI.table$RAI.count <- RAI.table$TotalCounts / RAI.table$Operation 
  
  # replace infinity with NA
  RAI.table %<>% mutate_if(is.numeric, list(~na_if(., Inf)))
  
  return(RAI.table)
  
}

rai.monthly <- function(record.table.subset, camop, start.date, end.date) {
  
  # calculate how long the camera was functioning in that time period
  
  # change start and end date to character
  start.date <- as.character(start.date)
  end.date <- as.character(end.date)
  daterange <- interval(start.date, end.date)
  
  # calculate number of detectionsa nd total counts in date range 
  record.table.subset <- record.table.subset %>%
    dplyr::group_by(site, Month) %>%
    dplyr::summarise(Detections = dplyr::n(), TotalCounts = sum(count))
  record.table.subset$site <- as.character(record.table.subset$site)
  
  # sum rows and counts within specified date range 
  camop <- camop[which(camop$date %within% daterange),]
  camop$Month <- month(camop$date)
  camop <- camop %>% 
    dplyr::group_by(site, Month) %>%
    dplyr::summarise(Operation = dplyr::n())
  camop$site <- as.character(camop$site)
  
  # join 
  RAI.table <- left_join(camop, record.table.subset)
  
  # replace NA with 0 
  RAI.table[is.na(RAI.table)] <- 0
  
  # calculate RAI
  RAI.table$RAI.det <- RAI.table$Detections / RAI.table$Operation  ##used to be 'RAI'
  RAI.table$RAI.count <- RAI.table$TotalCounts / RAI.table$Operation 
  
  # replace infinity with NA
  RAI.table %<>% mutate_if(is.numeric, list(~na_if(., Inf)))
  
  # merge with season
  RAI.table$Month <- as.character(str_pad(RAI.table$Month, 2, pad="0"))
  seasons$Month <- as.character(str_pad(seasons$Month, 2, pad="0"))
  RAI.table <- left_join(RAI.table, seasons) %>% as.data.frame()
  
  return(RAI.table)
  
}

# define occupancy model function 
#   om.calculate(records_subset, camera_operation, date_range[1], date_range[2], om_cov, detection_window, camera_metadata)
om.calculate <- function(record.table.subset, camop, start.date, end.date, covariate, window, cam_covs){
 
  # create detection histories 
  if(window == 1){ #window == day 
    record.table.subset$window <- paste(day(record.table.subset$Date), record.table.subset$Month, year(record.table.subset$Date))
    camop$window <- paste(day(camop$date), month(camop$date), year(camop$date))
    
  } else { #window == week
    record.table.subset$window <- paste(week(record.table.subset$Date), year(record.table.subset$Date))
    camop$window <- paste(week(camop$date), year(camop$date))
  }
    
  # all rows of data are a sighting; summarize presence/absence by week 
  record.table.subset <- record.table.subset[c("site", "window")]
  record.table.subset <- record.table.subset[!duplicated(record.table.subset),]
  record.table.subset$occu <- 1
  
  # search effort (when cameras on/off) 
  start.date <- as.character(start.date)
  end.date <- as.character(end.date)
  daterange <- interval(start.date, end.date)
  camop <- camop[which(camop$date %within% daterange),]
  camop$date <- NULL #remove date column
  camop <- camop[!duplicated(camop),] #thin down to one row per window
  
  # join search effort and data
  record.table.subset <- join(camop, record.table.subset, type="left")
  record.table.subset[is.na(record.table.subset)] <- 0 #NAs when cams on but no sightings; change NAs to 0s
  
  record.table.subset <- spread(record.table.subset, window, occu) #rows = sites; columns = detection replicates 
  #NAs are when cameras were not on -- do not change to 0s!				  
  sites <- unique(record.table.subset$site) #create list of camera trap sites
  n <- length(sites) #number of sites
  record.table.subset$site <- NULL #now data contains just detection histories (need for 'unmarked')
  
  # covariates 
  cam_covs <- cam_covs[cam_covs$site %in% sites,] #select covariates for sites in our data
  cam_covs <- cam_covs[c("site", covariate)]
  cam_covs <- cam_covs[!duplicated(cam_covs),]
  names(cam_covs)[2] <- "siteCovs"
  cam_covs_mod$site <- NULL #need to remove for 'unmarked' 

  # model 
  camtrap <- unmarkedFrameOccu(y=record.table.subset, siteCovs=cam_covs_mod)
  
  fm1 <- occu(~1 ~siteCovs, camtrap)
  occu_pred <- predict(fm1, newdata = cam_covs, type = "state")
  occu_pred <- cbind(cam_covs, occu_pred)
  
  return(occu_pred)
}

# Define leaflet legend function ------------------------------------------

# function that adds option to reverse order of legend https://github.com/rstudio/leaflet/issues/256
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft", 
                                                    "topleft"), pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins	
      
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
      
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
      
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, 
                       na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}
