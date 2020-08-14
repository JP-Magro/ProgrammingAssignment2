#R Programming - Week 3 - Programming Assignment 2

##These functions are similar to a Class object (in object oriented languages),
##the first one creates an matrix object that carries within some operations
##like getting an specific value or setting it.
##The second one tries to "figure it out" if there is data in the parental
##environment and executes or not other part of its code.

#Function - "makeCacheMatrix"
## This function creates a matrix with caches that can store its inverse,
##it can also creates a parental environment with some functions.

makeCacheMatrix<-function(x=matrix()){  ###Setting the arguments as a matrix
        inv_cache<-NULL  ##Creating and setting the "inv_cache" to NULL
        set_cache<-function(y){  ##Creating a function that set a value inside the matrix
                x<<-y
                inv_cache<<-NULL
        }
        RMA<-function(){x}  ##A function that returns the matrix cached arguments
        SetInv<-function(inverse){inv_cache<<-inverse}  ##A function that calculates and set the inv of the matrix
        GetInv<-function(){inv_cache}  ##A function that returns the "inv_cache"
        list(set_cache=set_cache,RMA=RMA,SetInv=SetInv,GetInv=GetInv) ##One list that makes possible to declare functions with $
}


#Function - "cacheSolve"
## This function first check if there is any "inv_cache" in the matrix,
##if "inv_cache" have any data stored in it, i returns that data, if not
##the "cacheSolve" calculates the inverse of the declared matrix.

cacheSolve<-function(x,...){
        CHECK<-x$GetInv()  ###"Checker"
        if(!is.null(CHECK)){
                message("Getting inverse cached data")
                return(CHECK)
        }
        MtX<-x$RMA()   ###Processing the inverse of the matrix part.
        inv_cache<-solve(MtX,...)
        x$SetInv(inv_cache)
        inv_cache
}
