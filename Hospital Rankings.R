hospital_data<-read.csv("H:\\Users\\batuk\\Downloads\\rprog_data_ProgAssignment3-data\\hospital-data.csv")
outcomes <- read.csv("H:\\Users\\batuk\\Downloads\\rprog_data_ProgAssignment3-data\\outcome-of-care-measures.csv")

outcomes[,11]<-as.numeric(outcomes[,11])
hist(outcomes[,11])
colnames(outcomes)[17]="heart failure"
colnames(outcomes)[11]="heart attack"
colnames(outcomes)[23]="pneumonia"
################ Returns best hospital in a given state for given outcome ###############
best <- function(state_name, outcome_name){
  if(all("heart failure" != outcome_name) && all("heart attack" != outcome_name) && all("pneumonia" != outcome_name)){
       error <- "invalid outcome"
     return (error)
  }
  flag <-1
  for(state in outcomes$"State"){
    if(state_name == state){
      flag <-0
      break
  }
  }
  if(flag){
    error <- "invalid state"
    return (error)
  }
    required <- outcomes[which(outcomes$State==state_name),c(outcome_name,"Hospital.Name") ]
    class(required[,1])="numeric"
    min_index <- which(min(required[,1],na.rm=TRUE)==required[,1])
    if(length(min_index) ==1){
      return (required[min_index,2])
    }
    else{
      hospitals <- required[[min_index,2]]
      hospitals<-sort(hospitals)
      return (hospitals[1])
   }
   
 
}
####################################################################################

##### Ranks hospitals by outcome in a given state and returns the ith hospital#######
rankhospital <- function(state_name,outcome_name,num){
  if(all("heart failure" != outcome_name) && all("heart attack" != outcome_name) && all("pneumonia" != outcome_name)){
    error <- "invalid outcome"
    return (error)
  }
  flag <-1
  for(state in outcomes$"State"){
    if(state_name == state){
      flag <-0
      break
    }
  }
  if(flag){
    error <- "invalid state"
    return (error)
  }
  required <- outcomes[which(outcomes$State==state_name),c(outcome_name,"Hospital.Name") ]
  class(required[,1])="numeric"
  required <- required[order(required[outcome_name],required["Hospital.Name"]),]
  if(class(num)=="numeric"){
    return (required[num,"Hospital.Name"])
  }
  else{
    if(all("best"== num)){
      return (required[1,"Hospital.Name"])
    }
    if(all("worst"== num)){
      return (tail(na.omit(required),1)[,2])
    }
  }
}
####################################################################################

######### Ranks hospitals by outcome in all states and returns the ith hospitals of their states respectively ############
sortsplit <- function(x){
  return (x[order(x[,1],x["Hospital.Name"]),])
}

rankall <- function(outcome_name, num="best"){
  if(all("heart failure" != outcome_name) && all("heart attack" != outcome_name) && all("pneumonia" != outcome_name)){
    error <- "invalid outcome"
    return (error)
  }
  data <- outcomes[,c(outcome_name,"Hospital.Name","State")]
  class(data[,outcome_name])="numeric"
  required <- data[complete.cases(data),]
  required <- required[order(required[,"State"],required[,outcome_name],required[,"Hospital.Name"]),]
  splitted <- split(required,required$State)
  hospital <- function(x) {
    if (all(num == "best")){
      y = 1
    } else if(all(num == "worst")) {
      y = nrow(x)
    } else {
      y = num
    }
    return(x[y,"Hospital.Name"])
  }
  
  result<-sapply(splitted,hospital)
  result <-data.frame(row.names=names(result),Hospital= result,State=names(result))
  

  return (result)
  
}