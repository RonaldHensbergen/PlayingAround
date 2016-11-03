sierpinski <- function(A=c(0,0),B=c(2,0),C=c(1,1),Z=c(0,1),div=0.5,iterations=10000) {
        set.seed(1234)

        set<-rbind(A,B,C)

        result<-data.frame(X=rep(0,iterations),Y=rep(0,iterations),
                           dice=sample(1:3,iterations,replace=TRUE))
        result[1,1]<-Z[1]
        result[1,2]<-Z[2]
        for (i in 2:iterations) {
                result[i,1]<-(result[i-1,1]+set[result[i,3],1])*div
                result[i,2]<-(result[i-1,2]+set[result[i,3],2])*div
        }
        
        plot(result$X,result$Y,pch=20)
}