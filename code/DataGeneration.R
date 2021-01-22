# Generation of the default dataset

DataGeneration <- function(d) {
  
  if (d == 1){

    Yobs<-numeric(1000)
    
    # mixture of 0.5 · N(−5, 1) + 0.5 · N(5, 1)
    for (i in 1:1000) {

      if(sample.int(2,1,prob=c(0.5,0.5))==1){

        Yobs[i]=rnorm(1,-5,1)

      }
      else{

        Yobs[i]=rnorm(1,5,1)

      }

    }

  }

  if (d == 2){

    Yobs<-matrix(data=NA,1000,2)
    
    for (i in 1:1000) {

      if(sample.int(2,1,prob=c(0.5,0.5))==1){

        Yobs[i,]=mvrnorm(1,rep(-5,2), matrix(c(1,0,0,1),2,2))

      }
      else{

        Yobs[i,]=mvrnorm(1,rep(5,2),matrix(c(1,0,0,1),2,2))

      }

    }

  }

return(Yobs)

}
