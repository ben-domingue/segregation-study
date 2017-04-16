ent1<-function(p) p*(1-p) 
ent2<-function(p) ifelse(p!=0 & p!=1,-(p*log2(p)+(1-p)*log2(1-p)),0) #see footnote 6
ent.list<-list(r=ent1,h=ent2)

##now we're going to set the various conditions under which the simulation runs
err.sd<-c(0,.1,.5,1,1.5,2) #these are the SDs for the individual-level errors
sch.sd<-c(.5) #this is associated with the variation in school-level means

tab<-list()
for (k in qnorm(seq(.1,.9,by=.1))) {
    L<-list()
    for (err in err.sd) for (sch in sch.sd) for (ent in names(ent.list)) {
                                                ent.list[[ent]]->ent.fun
                                                list(sig=sch,err.sd=err,Nperschool=500,n.school=50,niter=25,ent=ent.fun)->L[[paste(k,sch,err,ent)]] #these are the arguments passed to each iteration of the simulation
                        }
    library(parallel)
    makeCluster(4)->cl #modify this (e.g., change 4 to X) where X is the number of local processors. 
    clusterApply(cl,L,sim,k=k)->hold
    stopCluster(cl)
    strsplit(names(L)," ")->txt
    for (i in 1:length(hold)) {
        txt[[i]]->tmp
        nrow(hold[[i]])->nr
        matrix(tmp,byrow=TRUE,nr,length(tmp))->tmp
        names(tmp)<-c("k","sch.sd","err.sd","segfun")
        cbind(tmp,hold[[i]])->tmp
        tmp->hold[[i]]
    }
    do.call("rbind",hold)->tab[[as.character(k)]]
}
do.call("rbind",tab)->tab
data.frame(tab)->tab
write.csv(tab,"",row.names=FALSE)




