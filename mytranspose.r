library("optimbase")

mytranspose <- function(x) {
    if (is.vector(x)== T ){
            y <- transpose(x)
            
    }
    else if (is.matrix(x)==T){
        y <- matrix(1, nrow=ncol(x), ncol = nrow(x))
        if( ncol(x) == 0 || nrow(x)==0){
            return(x)
            break
        }
         for(i in 1:nrow(x)) {
          for(j in 1:ncol(x)) {
           y[j,i] <- x[i,j]
        }
     }
   }
    else if (is.null(x)==T){
        y <- x
    }
    else{
        y <- as.data.frame(t(as.matrix(x)))
    }
        return(y)
}

mytest <- function(x){
    if (length(x)==0){
        print("nothing")       
    }
    else if(is.na(x)==T){
        print("NA")
    }
    else if(mytranspose(mytranspose(x))== x){
        print("Collect")
    }
    else{
        print("Error")
    }
}


