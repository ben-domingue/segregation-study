
hold<-list()
##now we're going to set the various conditions under which the simulation runs
err.sd<-c(0,.1,.5,1,1.5,2) #these are the SDs for the individual-level errors
sch.sd<-c(.5) #this is associated with the variation in school-level means

par(mfrow=c(3,3))
for (k in qnorm(seq(.1,.9,by=.1))) {
    L<-list()
    for (err in err.sd) for (sch in sch.sd) {
                            list(sig=sch,err.sd=err,Nperschool=500,n.school=50,niter=25)->L[[paste(sch,err)]] #these are the arguments passed to each iteration of the simulation
                        }
    library(parallel)
    makeCluster(4)->cl #modify this (e.g., change 4 to X) where X is the number of local processors. 
    clusterApply(cl,L,sim,k=k)->hold
    stopCluster(cl)
    names(L)->names(hold)
    graf(hold)
    mtext(side=3,line=0,paste("k=",k))
    #legend("topleft",bty="n",title=paste("k=",k,sep=""!),c("observed","true"),lwd=2,col=c("black","red"),lty=1)
}




