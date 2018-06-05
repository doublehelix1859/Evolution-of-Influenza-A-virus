library(ggplot2)
library(reshape)
library(reshape2)
install.packages("gridExtra")
library(gridExtra)


#trial_3(perfectly work)
a.1<-read.table("t3.txt",header=T)
a.1
c.1<-subset(a.1, select = -c(1:2))
c.1
e.1<-data.frame(Year=row.names(c.1),c.1,stringsAsFactors=T)
e.1
f.1<-melt(e.1,id='Year')
f.1
f.1$Year<-as.numeric(f.1$Year)
#create plot
p.1<-ggplot(f.1,aes(x=Year,y=value))+ 
  geom_area(aes(fill=variable),position='fill')+ 
  ggtitle("HA_trial_3")+ylab("Frequency")+
  scale_x_continuous(labels=c(2009,2010,2011,2012))+ 
  scale_fill_discrete(name="Amino acid")
p.1


p1<-p + geom_area(aes(fill=variable)) + theme(legend.position="bottom")
p2<-p + geom_area(aes(fill=variable),position='fill')
p2
plot <- ggplot(e, aes(x=Year, y=value)) + theme(legend.position="right") 
plot
f<-melt(e)
f

####
df<-'t2.csv'
df.1<-read.csv(file=df,header=T,sep=",",na.strings=c(''))
df.1<-subset(a.1, select = -c(1:2))
df.1
df.2<-data.frame(Year=row.names(df.1),df.1,stringsAsFactors=T)
df.2
df.3<-melt(df.2,id='Year')
df.3
df.3$Year<-as.numeric(df.3$Year)
df.3
str(df.3)
#create plot
df.4<-ggplot(df.3,aes(x=Year,y=value))+ 
  geom_area(aes(fill=variable),position='fill')+ 
  ggtitle("HA_trial_4")+ylab("Frequency")+
  scale_x_continuous(labels=c(2009,2010,2011,2012))+ 
  scale_fill_discrete(name="Amino acid")
df.4



#standard format from internet
p.dat<-data.frame(step=row.names(grp.dat),grp.dat,stringsAsFactors=F)
p.dat<-melt(p.dat,id='step')
p.dat$step<-as.numeric(p.dat$step)

p<-ggplot(p.dat,aes(x=step,y=value))

p1<-p + geom_area(aes(fill=variable)) + theme(legend.position="bottom")

p2<-p + geom_area(aes(fill=variable),position='fill')

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


#previous work
data.PB2.7<-read.table("PB2_gb_667.txt",header=T)
#667
data.PB2.7
p.dat.PB2.7 <- data.frame(Year=row.names(data.PB2.7),data.PB2.7,stringsAsFactors=F)
p.dat.PB2.7 <- melt(p.dat.PB2.7,id='Year')
p.dat.PB2.7$Year <- as.numeric(p.dat.PB2.7$Year)
#create plots
PB2.p7 <- ggplot(p.dat.PB2.7, aes(x=Year, y=value)) + theme(legend.position="right") 
PB2.p7
PB2.p7 + geom_area(aes(fill=variable)) 
PB2.a7<-PB2.p7 + geom_area(aes(fill=variable),position='fill') + ggtitle("PB2_gb_667")+ylab("Frequency")+scale_x_continuous(breaks=c(2009,2010,2011,2012,2013,2014,2015,2016,2017))+ scale_fill_discrete(name="Amino acid")
PB2.a7

#standard format
data$Sector=factor(data$Sector , levels=levels(data$Sector)[c(1,4,3,2,7,6,5)])
ggplot(data, aes(x=Year, y=Value, fill=Sector)) + 
  geom_area()

#finishing_touch
a<-'HA.all.FQ.pos.csv'
b<-read.csv(file=a,header=T,sep=",",na.strings=c(''))
for (i in 1:566){
  b.1<-b[b$aapos==i,]
  name<-paste("HA_Global_AA_",i,sep="")
  b.2<-data.frame(year=as.numeric(b.1$year),aa=b.1$aa,f3=b.1$freq)
  
  k<- ggplot(b.2,aes(x=year,y=f3))+
    geom_area(aes(fill=aa),position='fill') +
    ggtitle(name)+ylab("Frequency")+
    scale_x_continuous(breaks=c(2009,2010,2011,2012,2013,2014,2015,2016,2017))+ 
    scale_fill_discrete(name="Amino acid")
  ggsave(k, file=paste(name, ".pdf", sep=""), width=10, height=10,limitsize=FALSE)
}



#numeric order
a<-'HA.all.FQ.pos.csv'
b<-read.csv(file=a,header=T,sep=",",na.strings=c(''))
for (i in 1:566){
  b.1<-b[b$aapos==i,]
  name<-paste("HA_Global_AA_",i,"_count",sep="")
  b.2<-data.frame(year=as.numeric(b.1$year),aa=b.1$aa,f3=b.1$count)
  
  k<- ggplot(b.2,aes(x=year,y=f3))+
    geom_area(aes(fill=aa),position='fill') +
    ggtitle(name)+ylab("Frequency")+
    scale_x_continuous(breaks=c(2009,2010,2011,2012,2013,2014,2015,2016,2017))+ 
    scale_fill_discrete(name="Amino acid")
  ggsave(k, file=paste(name, ".pdf", sep=""), width=10, height=10,limitsize=FALSE)
}
