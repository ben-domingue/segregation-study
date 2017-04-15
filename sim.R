##these are aux functions. the main simulation is in sim_run.R

sim<-function(L,k) { #this simulates everything based on a few key parameters
    h<-function(df,k) { #this computes the basic segregation statistic based on a value for k and:
                                        #a data frame "df" that contains columns
                                        #g which is the group variable and
                                        #o which is the observed variable
        ent<-function(p) p*(1-p) #ent<-function(p) ifelse(p!=0 & p!=1,-(p*log2(p)+(1-p)*log2(1-p)),0) #see footnote 6
        p<-function(y,k) sum(y<=k)/length(y) #see p. 12 
                                        #
        unique(df$g)->grps
        p(df$o,k)->pk
        ent(pk)->epk
        S<-list()
        for (grp in grps) {
            df[df$g==grp,]->tmp
            p(tmp$o,k)->po
            nrow(tmp)->n
            S[[as.character(grp)]]<-(n/(nrow(df)*epk))*(epk-ent(po))
        }
        sum(unlist(S))
    }
    ##
    for (i in 1:length(L)) assign(names(L)[i],L[[i]])
    ##
    out.true<-out<-numeric()
    for (i in 1:niter) { #we are going to run each simulation condition niter times
############################################
                                        #new school assignment procedure
        rnorm(n.school,mean=0,sd=sig)->M
        rep(.5,n.school)->S
        cbind(M,S)->schools
        tmp<-list()
        for (ii in 1:nrow(schools)) {
            rnorm(Nperschool,schools[ii,1],schools[ii,2])->th
            rnorm(Nperschool,0,sd=err.sd)->e
            cbind(ii,th,th+e)->tmp[[ii]]
        }
        do.call("rbind",tmp)->tmp
        data.frame(o=tmp[,3],g=tmp[,1])->df
        h(df,k)->out[i]
        data.frame(o=tmp[,2],g=tmp[,1])->df
        h(df,k)->out.true[i]
############################################
    }
    c(obs=mean(out),true=mean(out.true))
}


##graphics!
graf<-function(hold) {
    do.call("rbind",hold)->xx
    par(mgp=c(2,1,0))
    plot(xx[,1],type="l",ylab="seg",xlab="",xaxt="n",,lwd=2,ylim=c(0,1))
    strsplit(names(hold)," ")->nms
    sapply(nms,"[",1)->tmp
    for (i in 1:length(tmp)) mtext(side=1,at=i,tmp[i],line=0.25,,cex=.7)
    #mtext(side=1,line=.25,at=-2,"sch sd",adj=0)
    sapply(nms,"[",2)->tmp
    for (i in 1:length(tmp)) mtext(side=1,at=i,tmp[i],line=1,cex=.7)
    #mtext(side=1,line=1,at=-2,"err sd",adj=0)
    lines(xx[,2],type="l",col="red",lwd=2)
}
