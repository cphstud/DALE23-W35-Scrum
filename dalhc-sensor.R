library(serial)
library(stringr)
library(stringi)
library(logr)

conarduino <- serialConnection(
  name = "mytest",
  #port = "cu.usbserial-146101",
  port = "COM3",
  mode = "9600,n,8,1",
  newline = 1,
  translation = "cr",
  handshake = "xonxoff",
  buffersize = 4096
)

log_open("log.txt")

difflimit=75

open(conarduino)
options(digits.secs=2)  

dfnames=c("obs","ts")
totaldf=as.data.frame(matrix(nrow=0,ncol=2))
colnames(totaldf)=dfnames
stoptime=Sys.time()+20
flush(conarduino)

while (Sys.time() < stoptime) {
  tmp=read.serialConnection(conarduino)
  count=calcdist2(tmp)
  totaldf = rbind(totaldf,count)
  flush(conarduino)
  Sys.sleep(5)
}

close(conarduino)


calcdist2 <- function(rawin) {
  v=as.numeric(unlist(stri_split(rawin,regex = "\n",omit_empty=T)))
  ts = Sys.time()
  retval = data.frame(obs=v,time=ts)
  #brutlist=sort(abs(unique(diff(v))))
  log_print(retval)
  #retval=sum(brutlist > difflimit)
  #print(v)
  ##return(round(retval/2))
  return(retval)
}

log_close()

calcdist <- function(rawin) {
  retval=0L
  v=as.numeric(unlist(stri_split(rawin,regex = "\n",omit_empty=T)))
  brutlist=sort(abs(unique(diff(v))))
  log_print(brutlist)
  retval=sum(brutlist > difflimit)
  print(v)
  return(round(retval/2))
}
