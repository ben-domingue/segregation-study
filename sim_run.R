
hold<-list()
##now we're going to set the various conditions under which the simulation runs
err.sd<-c(0,.1,.5,1,2)
sch.sd<-c(.5)

par(mfrow=c(2,2))
for (k in qnorm(seq(.1,.9,by=.1))) {
    L<-list()
    for (err in err.sd) for (sch in sch.sd) {
                            list(sig=sch,err.sd=err,N=10000,n.school=10)->L[[paste(sch,err)]]
                        }
    library(parallel)
    makeCluster(4)->cl
    clusterApply(cl,L,sim,k=k)->hold
    stopCluster(cl)
    names(L)->names(hold)
    graf(hold)
    #legend("topleft",bty="n",title=paste("k=",k,sep=""!),c("observed","true"),lwd=2,col=c("black","red"),lty=1)
}



