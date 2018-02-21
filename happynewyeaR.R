## Season's Greetings in R 
## =======================


## for geek level+++, try txtplot ;-)
## uncomment lines 8 and 96
## comment line 98
## library(txtplot) # install this library if needed


rm(list=ls())

maxiter <- 100
n <- 100

{
    stari <- 0
    star <- cbind(0,0,"")
    wind <- round(runif(1,min=-5,max=5),0)
    for (i in 1:maxiter) {
        cat("\014"); cat(rep("\n",50))
       neige <- rbind(
            cbind(
                1:100,
                runif(100,min=0.3,max=1),
                '.'),
            cbind(
                c(1:50,85:100),
                runif(length(c(1:50,85:100)),min=0.01,max=0.3),
                ".")
           )

        if(stari < 0 & runif(1,0,1) < 0.1) {
            theta <- runif(1,min=0,max=2*pi)
            stari <- 11
            star <- cbind(round(runif(1,10,40),1),round(runif(1,0.3,1),2),"#")
        }

        stari <- stari-1
        
        if(stari >= 0 & stari < 10) {
            star <- rbind(
                star,
                cbind(round(as.numeric(star[10-stari,1])+cos(theta),1),
                      round(as.numeric(star[10-stari,2])+sin(theta)/20,2),
                      "#"
                      )
                )
        }

        if(stari < 0 ){
               star <- cbind(0,0,"")
           }

        maison <- rbind(
            cbind(1:100,0,"-"),
            cbind(55:80,0.25,'='),
            cbind(rep(55,10),seq(0,0.25,length=10),'I'),
            cbind(rep(80,10),seq(0,0.25,length=10),'I'),
            cbind(rep(78,4),seq(0.25,0.35,length=4),'|'),
            cbind(rep(77,4),seq(0.25,0.35,length=4),'|'),
            cbind(c(66,68),c(0,0,0.05,0.05,0.1,0.1),'H'),
            c(74,0.15,'['),
            c(76,0.15,']'),
            c(58,0.15,'['),
            c(60,0.15,']')
            )
 
        if (i == 1) {
            fumee <- cbind(rep(c(77,78),2),0.4,rep(sample(c("§","#","@","&"),1),4))
        }
        else {
            fumee <- rbind(
                fumee,
                cbind(c(c(as.numeric(fumee[((4*(i-1))-1),1]),
                          as.numeric(fumee[((4*(i-1))),1])),
                        c(as.numeric(fumee[((4*(i-1))-1),1]),
                          as.numeric(fumee[((4*(i-1))),1]))+wind),
                      as.numeric(fumee[((4*(i-1))-1),2])+0.05,
                      NA##fumee[((4*(i-1))-3):(4*(i-1)),3]
                      )
                )
            fumee[,3] <- c(rep(sample(c("~","@","§"),1),4),fumee[1:(4*(i-1)),3])
        }

        d <- rbind(neige,
                   maison,
                   fumee[as.numeric(fumee[,1])<100 & as.numeric(fumee[,2])<1,],
                   star[as.numeric(star[,1])<100 & as.numeric(star[,2])<1,]
                   )
        
        x <- as.numeric(d[,1])
        y <- as.numeric(d[,2])
        pch <- d[,3]

        ##        txtplot(x,y,pch=pch)
        
        plot(x,y,pch=gsub('\\.','*',pch),xaxt='n',yaxt='n',bty='n',xlab='',ylab='',main=substr("meRRy chRistmas and happy new yeaR !",1,i-29))
        Sys.sleep(.1) 
    }
}
