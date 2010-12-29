# list(list) -> matrix
ll2m <- function(lls) {
    lmax <- Reduce(max, Map(length, lls))
    matrix(nrow=lmax, ncol=length(lls), 
      data = unlist(Map(function(ls) unlist(c(ls,rep(NA,lmax - length(ls)))), lls))
    )
}

####################################################################

confcnv <- function(conf)
{
   gamma <- conf["gamma",1]
   cnfV  <- Map(function(x) 
                  if (grepl("betaN",x)) round(conf[x,1]/gamma, digit=6)
                  else round(conf[x,1], digit=6)
               ,rownames(conf))
   cnfF  <- Map(function(x) sub("betaN","rr",x), rownames(conf))
   names(cnfV) <- cnfF
   cnfV
}


Ptripplot <- function(time,place)
{
    plot(time,place,axes=F,ylim=c(0,6),type="l")
    axis(2,0:6,c("塾","学校","会社","家","スーパー","公園","鉄道"))
    axis(1,seq(0,max(time),1440),c(0:(max(time)%/%1440)))
    box()
}

Atripplot <- function(time,place)
{
    plot(time,place,axes=F,ylim=c(0,5),type="l")
    axis(2,0:4,c("八王子","立川","吉祥寺","新宿","東京"))
    axis(1,seq(0,max(time),1440),c(0:(max(time)%/%1440)))
    box()
}

Ptripplot1day <- function(time,place)
{
    plot(time,place,axes=F,ylim=c(0,6),type="l")
    axis(2,0:6,c("塾","学校","会社","家","スーパー","公園","鉄道"))

    loc <- seq(min(time),max(time),60)
    lab <- Map(function(x) (x %% 1440) %/% 60, loc)
    axis(1,loc,lab)
    box()
}


ptripplot <- function(mat,n)
{
    Ptripplot(mat[,1],mat[,3*n+4])
}

ptripplot1day <- function(mat,n)
{
    Ptripplot1day(mat[,1],mat[,3*n+4])
}


atripplot <- function(mat,n)
{
    Atripplot(mat[,1],mat[,3*n+2])
}


pop_sum_plot1 <- function(x,offset,ylab,maxday,conf,maxE)
{
    t <- x[,1]/1440
    e <- apply(x[,4*(0:4)+offset], 1, sum)

    if (missing(maxE)) {
      maxE_ = max(e)
    } else {
      maxE_ = maxE
    }

    plot(t,e,main="e",type="l", xlim=c(0,maxday),ylim=c(0,1.5*maxE_),ylab=ylab,xlab="time [days]",lty=1,lwd=2)
}


popplot1 <- function(x,offset,ylab,maxday,conf,maxE)
{
    t <- matrix(rep(x[,1]/1440,5), ncol=5)
    e <- x[,4*(0:4)+offset]

    #cat(max(e))

    if (missing(maxE)) {
      maxE_ = max(e)
    } else {
      maxE_ = maxE
    }

    matplot(t,e,main="e",type="l", xlim=c(0,maxday),ylim=c(0,1.5*maxE_),ylab=ylab,xlab="time [days]",lty=1,lwd=2)
    legend(maxday*0.8,1.5*max(e),c("八王子","立川","吉祥寺","新宿","東京"),lty=1,lwd=2,col=1:5)
    if (!missing(conf)) {
        legend(0,1.5*max(e), paste(names(conf), conf))
    }
}

popplot <- function(x,maxday,conf,tag,toFile,maxE)
{
    nuse = 2
    if (missing(toFile) || !toFile) {
	n = length(dev.list())
	if (n < nuse) {
	    for (i in 1:(nuse-n)) {
		x11()
	    }
	}
	dv = dev.list() 
    } else {
      dv <- c(0,0)
      png(paste("R_",tag,".png",sep=""))
      dv[1] <- dev.cur()
      png(paste("E_",tag,".png",sep=""))
      dv[2] <- dev.cur()
    }

    dev.set(dv[1])  
    popplot1(x,5,"R",maxday,conf)
    if   (!missing(tag) && !toFile) {savePlot(file = paste("R_",tag,".png",sep=""), type="png")}

    dev.set(dv[2])  
    popplot1(x,3,"E",maxday,conf,maxE)
    if (!missing(tag) && !toFile) {savePlot(file = paste("E_",tag,".png",sep=""), type="png")}
    
    if (!(missing(toFile) || !toFile)) {
        Map(dev.off, dv)
    }

    if (missing(maxE)) {
      png(paste("R_SUM_",tag,".png",sep=""))
      pop_sum_plot1(x,5,"R",maxday,conf)
      dev.off()

      png(paste("E_SUM_",tag,".png",sep=""))
      pop_sum_plot1(x,3,"E",maxday,conf)
      dev.off()
    } else {
      png(paste("R_SUM_",tag,".png",sep=""))
      pop_sum_plot1(x,5,"R",maxday,conf,maxE*5)
      dev.off()

      png(paste("E_SUM_",tag,".png",sep=""))
      pop_sum_plot1(x,3,"E",maxday,conf,maxE*5)
      dev.off()
   }
}

popplotf <- function(tag,maxday)
{
   x <- read.csv(paste("pop_",tag,".csv",sep=""))
   conf <- read.csv(paste("conf_",tag,".csv",sep=""),row.name=1,header=F)
   popplot(x,maxday,confcnv(conf),tag,TRUE)
}

# tag = フォルダ名として、これを使え!!
popplotd <- function(tag,maxday,maxE)
{
   pwd <- getwd()
   setwd(tag)
   popfiles <- grep("pop", dir("./"), value=T)

   Map(function(popf) {
         x    <- read.csv(popf)
         conf <- read.csv(sub("pop","conf", popf),row.name=1,header=F)
         tag  <- sub(".csv","", sub("pop_", "", popf))
         popplot(x,maxday,confcnv(conf),tag,TRUE,maxE)
       }
     , popfiles)

   setwd(pwd)
}

popplotmcf <- function(tag,maxday)
{
   pwd <- getwd()
   setwd(tag)
   citynames <- c("八王子","立川","吉祥寺","新宿","東京")

   #cat("entering", getwd(),"\n")
   files <- grep("pop", dir("./"), value=T)

   xs <- Map(read.csv, files)

   #cat("|files|=", length(files))

   conf <- read.csv(sub("pop","conf",files[[1]]),row.name=1,header=F)

   nrows <- nrow(xs[[1]])
   mab <- seq(2,nrows-1,8)

   # t <- matrix(rep(x[,1]/1440,5), ncol=5)
   ts <- ll2m(Map(function(x) x[mab,1]/1440, xs))

   #idxCity in 0:4
   idxCity <- 0

   #idxVar  in 2(S), 3(E), 4(I), 5(R)
   idxVar  <- 3


   es <- ll2m(Map(function(x) x[mab,4*idxCity + idxVar], xs))
   maxE = max(es)
   matplot(ts,es,main="e",type="l", xlim=c(0,maxday),ylim=c(0,1.5*maxE)
          , xlab="time [days]", ylab="E", lty=1, lwd=1)

   # 再生産率表示にする。
   #cnfL0 <- paste(rownames(conf), conf[,1])
   gamma <- conf["gamma",1]
   cnfV  <- Map(function(x) 
                  if (grepl("betaN",x)) round(conf[x,1]/gamma, digit=6)
                  else round(conf[x,1], digit=6)
               ,rownames(conf))
   cnfF  <- Map(function(x) sub("betaN","rr",x), rownames(conf))
   legend(0,1.5*maxE, paste(cnfF,cnfV,sep=" = "))

   setwd(pwd)
   files[[1]] 
}

# ToDo:
# (1) 個別に書いたものも必要。スライドショーで見たいので、jpegかpngが良い。
#  ... これまで使ってきたやつをただし、jpeg時下がきで
# jpeg()
#    ....
# dev.close()
# (2) 3例しか見ていないが、何かいえることはないか。
# (3) (2)をもとに、として。系統的にパラメータサーチをする
