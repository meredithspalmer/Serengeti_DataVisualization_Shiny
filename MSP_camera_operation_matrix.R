## Create a camera operation matrix for camtrapR-related code

se <- read.csv("../SER_S1-12_siterolleffortexpanded.csv")
se$date <- as.Date(se$date)
se <- se[!se$date < strptime("2010-08-01", "%Y-%m-%d"),]
se <- se[!se$date > strptime("2014-12-31", "%Y-%m-%d"),]
se <- se[!duplicated(se),]

# weird issues -- at some point, look at how SE generated again 
se <- se[!(se$site == "K12" & se$date == strptime("2013-02-28", "%Y-%m-%d")),]  
se <- se[!(se$site == "L09" & se$date == strptime("2014-02-25", "%Y-%m-%d")),]  
se <- se[!(se$site == "L11" & se$date == strptime("2012-08-13", "%Y-%m-%d")),] 
se <- se[!(se$site == "M12" & se$date == strptime("2011-02-18", "%Y-%m-%d")),]  
se <- se[!(se$site == "O05" & se$date == strptime("2013-09-09", "%Y-%m-%d")),]  
se <- se[!(se$site == "D13" & se$date == strptime("2011-12-29", "%Y-%m-%d")),]
se <- se[!(se$site == "G06" & se$date == strptime("2013-01-22", "%Y-%m-%d")),]
se <- se[!(se$site == "G09" & se$date == strptime("2013-02-01", "%Y-%m-%d")),]
se <- se[!(se$site == "I13" & se$date == strptime("2013-02-11", "%Y-%m-%d")),]
se <- se[!(se$site == "J04" & se$date == strptime("2013-07-05", "%Y-%m-%d")),]
se <- se[!(se$site == "S13" & se$date == strptime("2012-09-04", "%Y-%m-%d")),]
se <- se[!(se$site == "C11" & se$date == strptime("2012-12-08", "%Y-%m-%d")),]
se <- se[!(se$site == "N03" & se$date == strptime("2013-09-02", "%Y-%m-%d")),]
se <- se[!(se$site == "P04" & se$date == strptime("2013-09-09", "%Y-%m-%d")),]
se <- se[!(se$site == "E08" & se$date == strptime("2012-10-01", "%Y-%m-%d")),]
se <- se[!(se$site == "O03" & se$date == strptime("2012-05-26", "%Y-%m-%d")),]
se <- se[!(se$site == "Q06" & se$date %in% c(as.Date("2013-05-11"), as.Date("2013-07-04"), 
                                             as.Date("2013-09-09"), as.Date("2013-11-12"))),]  
se <- se[!(se$site == "D05" & se$date %in% c(as.Date("2012-07-21"), as.Date("2012-11-19"), 
                                             as.Date("2014-04-29"))),]
se <- se[!(se$site == "C04" & se$date %in% c(as.Date("2011-12-30"), as.Date("2013-06-03"))),]
se <- se[!(se$site == "D07" & se$date %in% c(as.Date("2013-01-29"), as.Date("2013-10-31"))),]
se <- se[!(se$site == "D09" & se$date %in% c(as.Date("2013-04-13"), as.Date("2013-06-25"))),]
se <- se[!(se$site == "E06" & se$date %in% c(as.Date("2013-01-22"), as.Date("2013-05-25"), 
                                             as.Date("2013-07-29"))),]
se <- se[!(se$site == "F05" & se$date %in% c(as.Date("2013-09-10"), as.Date("2014-02-03"),
                                             as.Date("2014-04-23"), as.Date("2014-06-27"))),]
se <- se[!(se$site == "G13" & se$date %in% c(as.Date("2012-12-11"), as.Date("2014-08-19"))),]
se <- se[!(se$site == "I07" & se$date %in% c(as.Date("2012-08-09"), as.Date("2012-10-14"))),]
se <- se[!(se$site == "R07" & se$date %in% c(as.Date("2013-02-27"), as.Date("2013-11-12"))),]
sites <- unique(se$site)

camera_operation <- NULL
for(i in 1:length(sites)){
  test <- se[se$site == sites[i],]
  test <- test[order(test$date),]
  test <- test[!duplicated(test),]
  test$consecutive <- c(NA,diff(as.Date(test$date))==1)
  
  camera_operation_sub <- NULL
  camera_operation_sub$Station <- as.character(sites[i])
  camera_operation_sub$Setup_date <- as.character(min(test$date))
  camera_operation_sub$Retrieval_date <- as.character(max(test$date))
  camera_operation_sub <- data.frame(camera_operation_sub)
  
  if(sum(test$consecutive == F, na.rm=T) > 0){
    starts <- which(test$consecutive == F)
    stops <- starts - 1
    newtest <- test[c(starts, stops),]
    newtest <- newtest[order(newtest$date),]
    newtest.start <- newtest[newtest$consecutive == F,]
    names(newtest.start)[2] <- "Start"; newtest.start$consecutive <- NULL
    newtest.stop <- newtest[newtest$consecutive == T,]
    names(newtest.stop)[2] <- "Stop"; newtest.stop$consecutive <- NULL
    
    for(j in 1:nrow(newtest.start)){
      problem_from <- paste("Problem", j, "_from", sep="") 
      problem_to <- paste("Problem", j, "_to", sep="")
      newdf <- data.frame(from = as.character(newtest.stop$Stop[j]), to = as.character(newtest.start$Start[j]))
      names(newdf) <- c(problem_from, problem_to)
      camera_operation_sub <- cbind(camera_operation_sub, newdf)
      }
    } 
    
    if (i == 1){
      camera_operation <- camera_operation_sub
    } else {
      og.df <- (length(camera_operation)-3)/2
      new.df <- (length(camera_operation_sub)-3)/2
      
      if(og.df == new.df){
        camera_operation <- rbind(camera_operation, camera_operation_sub)
        
      } else if(og.df > new.df){
        for(k in (new.df+1):og.df){
          problem_from <- paste("Problem", k, "_from", sep="") 
          problem_to <- paste("Problem", k, "_to", sep="")
          newdf <- data.frame(from = NA, to = NA)
          names(newdf) <- c(problem_from, problem_to)
          camera_operation_sub <- cbind(camera_operation_sub, newdf)
        }
        camera_operation <- rbind(camera_operation, camera_operation_sub)
        
      } else if(og.df < new.df){
        for(k in (og.df+1):new.df){
          problem_from <- paste("Problem", k, "_from", sep="") 
          problem_to <- paste("Problem", k, "_to", sep="")
          newdf <- data.frame(from = NA, to = NA)
          names(newdf) <- c(problem_from, problem_to)
          camera_operation <- cbind(camera_operation, newdf)
        }
        camera_operation <- rbind(camera_operation, camera_operation_sub)
      }
    }
  }
head(camera_operation)
nrow(camera_operation)
write.csv(camera_operation, "camera_operation_test.csv", row.names=F)
