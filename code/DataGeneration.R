# Generation of the dataset

DataGeneration <- function(d) {
  
  if (d == 1){
    Yobs<-numeric(1000)
    for (i in 1:1000) {
      if(sample.int(2,1,prob=c(0.5,0.5))==1){
        Yobs[i]=rnorm(1,-5,1)
      }
      else{
        Yobs[i]=rnorm(1,5,1)
      }
    }
  }
  
  if (d==2){
    
  }
return(Yobs)
}
