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

myvar1 <-  matrix(1:10, nrow=5, ncol=2)
myvar1
mytranspose(myvar1)
mytest(myvar1)

myvar1 <-  matrix(NA, nrow=0, ncol=0)
myvar1
mytranspose(myvar1)
mytest(myvar1)

myvar1 <-  matrix(c(1,2), nrow=1, ncol=2)
myvar1
mytranspose(myvar1)
mytest(myvar1)

myvar1 <-  matrix(c(1,2), nrow=2, ncol=1)
myvar1
mytranspose(myvar1)
mytest(myvar1)

myvar2 <- c(1,2,NA,3)
myvar2
mytranspose(myvar2)
mytest(myvar2)

myvar2 <- c(NA)
myvar2
mytranspose(myvar2)
mytest(myvar2)

myvar2 <- c()
myvar2
mytranspose(myvar2)
mytest(myvar2)

d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata3 <- data.frame(d,e,f)

mydata3
mytranspose(mydata3)
mytest(mydata3)


