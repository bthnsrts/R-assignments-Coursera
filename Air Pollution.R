

pollutantmean <- function(directory,pollutant,id=1:332){
  dir <- list.files(directory)
  dir <- matrix(dir,nrow=332,ncol=1)
  total_sum <-0
  total_length <-0
  for(i in id){
    x <- read.csv(paste0(directory,"\\",dir[i,1],sep=""))
    total_sum <- sum(x[[pollutant]],na.rm=TRUE) + total_sum
    total_length <- total_length + length(x[which(!is.na(x[[pollutant]])),1])
  }
  return (total_sum/total_length)
}


complete <- function(directory,id){
  nobs <- rep(0,length(id))
  dir <- list.files(directory)
  dir <- matrix(dir,nrow=332,ncol=1)
  a <-1
  for(i in id){
    x <- read.csv(paste0(directory,"\\",dir[i,1],sep=""))
    nobs[a]<-length(x[which(!is.na(x[["sulfate"]]) &!is.na(x[["nitrate"]])), 3])
    a =a +1
  }
  return (data.frame(id,nobs))
}


read<- function(directory,threshold=0){
  noth <-complete(directory,1:332)
  id <-(noth[which(noth[["nobs"]]>threshold),1])
  return (id)
  
}
corr<- function(directory,threshold=0,id=read(directory,threshold)){
  dir <- list.files(directory)
  dir <- matrix(dir,nrow=332,ncol=1)
  cors <- vector(mode="numeric",length=length(id))
  a <-1
  for(i in id){
    x <- read.csv(paste0(directory,"\\",dir[i,1],sep=""))
    t<-x[which(!is.na(x[["sulfate"]]) &!is.na(x[["nitrate"]])), ]
    cors[a] <- cor(t[["sulfate"]],t[["nitrate"]])
    a=a+1
  }
  return (cors)
}
