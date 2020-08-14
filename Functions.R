#Functions
makeCacheMatrix<-function(x=matrix()){
        inv_cache<-NULL
        set_cache<-function(y){
                x<<-y
                inv_cache<<-NULL
        }
        RMA<-function(){x}
        SetInv<-function(inverse){inv_cache<<-inverse}
        GetInv<-function(){inv_cache}
        list(set_cache=set_cache,RMA=RMA,SetInv=SetInv,GetInv=GetInv)
}

cacheSolve<-function(x,...){
        CHECK<-x$GetInv()
        if(!is.null(CHECK)){
                message("Getting inverse cached data")
                return(CHECK)
        }
        MtX<-x$RMA()
        inv_cache<-solve(MtX,...)
        x$SetInv(inv_cache)
        inv_cache
}