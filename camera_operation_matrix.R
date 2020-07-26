## Create a camera operation matrix for camtrapR-related code

# Data from Zooniverse comes back in a drastically different format from that data that comes back from camtrapR.
# This code takes the list of camera operation dates (one column of cameras, one column of all dates each camera 
# operational) and converts the search effort into a camtrapR record table. 

se <- read.csv("../SER_S1-12_siterolleffortexpanded.csv")
se$date <- as.Date(se$date)
se <- se[!se$date < strptime("2010-08-01", "%Y-%m-%d"),]
se <- se[!se$date > strptime("2014-12-31", "%Y-%m-%d"),]
se <- se[!duplicated(se),]
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

write.csv(camera_operation, "camera_operation_test.csv", row.names=F)
