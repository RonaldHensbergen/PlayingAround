sierpinski <- function() {
        set.seed(1234)
        A<-data.frame(X=0,Y=0)
        B<-data.frame(X=2,Y=0)
        C<-data.frame(X=1,Y=1)
        
        set<-rbind(A,B,C)
        Z<-data.frame(X=0,Y=1)
        div<-0.5
        iterations = 10000
        
        result<-data.frame(X=rep(0,iterations),Y=rep(0,iterations),
                           dice=sample(1:3,iterations,replace=TRUE))
        result[1,1]<-Z$X
        result[1,2]<-Z$Y
        for (i in 2:iterations) {
                result[i,1]<-(result[i-1,1]+set[result[i,3],1])*div
                result[i,2]<-(result[i-1,2]+set[result[i,3],2])*div
        }
        
        plot(result$X,result$Y)
}