source("Scripts/loadLibraries.r")
source("Scripts/PlottingScripts/PlotVerse.r")
load("DerivedData/FlowBookCorpus.rdata")
meta = read.delim("SourceData/verseMetadata.txt",header=T)

# 4.1 ---------------------------------------------------------------

# text, see Word file.

# 4.2., Lose Yourself demo, transcription -----------------------------------
load("DerivedData/FlowBookCorpus.rdata")
PlotVerse("loseYourselfDemo1",m.range=-1:7,meas.scale = .8, Width=3.5,plot.rhymeClasses = T)
quartz.save(file="Examples/Chapter 4/Example 4.2 (bottom).pdf",type="pdf");dev.off()
PlotVerse("loseYourselfDemo1",m.range=8:15,meas.scale = .8, Width=3.5,plot.rhymeClasses = T)
quartz.save(file="Examples/Chapter 4/Example 4.2 (top).pdf",type="pdf");dev.off()

# 4.3a and 3b. accents on each position of “Lose Yourself” demo ------------

x = corpora %>% filter(verse=="loseYourselfDemo1",accent==1) %>%
  .[["beatIndex"]] %>% mod(4)
a = table(c(x,seq(0,3.75,.25)))-1
names(a) = 0:15
b = table(c(a,0:16))-1

quartz(width=4.55,height=2)
par(mfrow=c(1,2),mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),las=1,bty='n',
    family="Times New Roman",cex=.65)
barplot(a,
        mgp = c(2,1,0),family="Times New Roman",
        col = rep(c("black",rep("gray",3)),4),cex.names = .5,
        xlab = "Metric position",ylab="Count of accents")
#par(fig = c(0,1,0,.5),new=T)
barplot(b,cex.names = .5,mgp = c(2,1,0),family="Times New Roman",
        xlab = "Count of accents",ylab="Number of positions")
quartz.save(file="Examples/Chapter 4/Example 4.3.pdf",type="pdf");dev.off()
remove(list=c("x","a","b"))

# 4.4. Lose Yourself, released version, transcription -----------------------

PlotVerse("loseYourself1",m.range=-1:7,meas.scale = .8, Width=3.5,plot.rhymeClasses = T)
quartz.save(file="Examples/Chapter 4/Example 4.4 (bottom).pdf",type="pdf");dev.off()
PlotVerse("loseYourself1",m.range=8:15,meas.scale = .8, Width=3.5,plot.rhymeClasses = T)
quartz.save(file="Examples/Chapter 4/Example 4.4 (top).pdf",type="pdf");dev.off()

# 4.5a and 5b (compare to Example 3) --------------------------------

x1 = corpora %>% 
  filter(verse=="loseYourself1",accent==1,beatIndex<32) %>%
  select(syllable,beatIndex) %>% 
  .[["beatIndex"]] %>% mod(4)
x2 = corpora %>% 
  filter(verse=="loseYourself1",accent==1,beatIndex>=32) %>%
  select(syllable,beatIndex) %>% 
  .[["beatIndex"]] %>% mod(4)
a1 = table(c(x1,seq(0,3.75,.25)))-1
names(a1) = 0:15
b1 = table(c(a1,0:8))-1

a2 = table(c(x2,seq(0,3.75,.25)))-1
names(a2) = 0:15
b2 = table(c(a2,0:8))-1

quartz(width=4.55,height=4.5)
par(mfrow=c(2,2),mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
barplot(a1,las=1,cex.names=.5,family="Times New Roman",
        col = rep(c("black",rep("gray",3)),4),
        xlab = "Metric position",ylab="Count of accents")
barplot(b1,las=1,cex.names = .5,family="Times New Roman",
        xlab = "Count of accents",ylab="Number of positions")
barplot(a2,las=1,cex.names=.5,family="Times New Roman",
        col = rep(c("black",rep("gray",3)),4),
        xlab = "Metric position",ylab="Count of accents")
barplot(b2,las=1,cex.names = .5,family="Times New Roman",
        xlab = "Count of accents",ylab="Number of positions")
quartz.save(file="Examples/Chapter 4/Example 4.5.pdf",type="pdf");dev.off()
remove(list=c("x1","x2","a1","a2","b1","b2"))


# 4.6. Temperley reprint ----------------------------------------------------


# 4.7. 8 Mile, verse 3, mm. 21–24 -----------------------------------
load("DerivedData/FlowBookCorpus.rdata")
PlotVerse(verseTitle = "8mile3", row.index = 270:307)
quartz.save(file="Examples/Chapter 4/Example 4.7.pdf",type="pdf");dev.off()

# 4.8. Two grooves plotted around the circle ------------------------
source("Scripts/PlottingScripts/PlotSet.r")
quartz(width=4.55,height=2.225)
par(mfrow=c(1,2),mar=rep(0,4),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
PlotSet(c(2,4,7,10,12,15),16,F)
PlotSet(c(0,3,5,8,11,13),16,F)
quartz.save(file="Examples/Chapter 4/Example 4.8.pdf",type="pdf");dev.off()


# 4.9.  All intervals of 333322 -------------------------------------------------

# Plot <333322> with all intervals
set = c(0,3,6,9,12,14)
card = 16
PlotSet(set,card,launch.quartz = T,width=2)
apply(combn(set,2),2,function(i) AddLine(i,16))
ints = apply(combn(set,2),2,diff) %>% sort
ints[ints>8] = 16 - ints[ints>8]
ints = sort(ints)
quartz.save(file="Examples/Chapter 4/Example 4.9.pdf",type="pdf");dev.off()
remove(list=c("AddLine","PlotSet","card","ints","set"))

# 4.10. ICH of 333322 --------------------------------------------------------

GetIntervalContentHistogram = function(set,card){
  ints = matrix(c(
    combn(set,2) %>% apply(.,2,diff),
    combn(set,2) %>% .[2:1,] %>% apply(.,2,diff) %>% mod(.,card)),
    dim(combn(set,2))[2],
    2
  )
  ints = apply(ints,1,min)
  ints = c(ints, 1:(ceiling(card/2)))
  table(ints)-1
}

x = GetIntervalContentHistogram(c(0,3,6,9,12,14),16)
quartz(width=2,height=2)
par(mar=c(3,3,1,0),mgp = c(2,1,0), cex.axis = .65,cex.lab=.65)

barplot(x,cex.names = .75,xlab="Duration",mgp = c(2,1,0),ylab="Count",las=1,family="Times New Roman")
quartz.save(file="Examples/Chapter 4/Example 4.10.pdf",type="pdf");dev.off()
remove(list=c("GetIntervalContentHistogram","x"))
# 4.11. (See Word document) -------------------------------------------------


# 4.12. (Reprint of Pressing) -----------------------------------------------


# 4.13. complexity of different rotations of groove classes ------------------
source("Scripts/AnalysisScripts/Chapter 4 Analysis Script (excerpt).r")
r = range(vals)
quartz(width=4.5,height=2.5)
par(mfcol = c(7,1),mar=c(0,0,0,0),cex = .65)
llply(1:7,function(i){
  plot(vals[[i]],rep(0,length(vals[[i]])),xlim = r,xaxt="n",pch=20,bty="n",
       yaxt="n")
  text(vals[[i]],rep(0,length(vals[[i]])),0:(length(vals[[i]])-1),pos=1,family="Times New Roman",font=3)
  points(0:4,rep(0,5),pch="|")
  lines(r,rep(0,2))
  text(0.25,0,names(vals)[i],pos = 3,family="Times New Roman",cex=1.25)
  
  if(2.64 %in% i){text(1:4,rep(0,4),1:4,pos=1)}
})
quartz.save(file="Examples/Chapter 4/Example 4.13.pdf",type="pdf")
quartz.save(file="Examples/Chapter 6/Example 6.2.pdf",type="pdf");dev.off()
remove(list=c("groove.classes","r","vals"))

# 4.14: Three segments with <332 2222> --------------------------------------

PlotVerse("astonMartin",row.index = 17:64,plot.3limit = F)
quartz.save(file="Examples/Chapter 4/Example 4.14a.pdf",type="pdf");dev.off()

PlotVerse("goToSleep1",m.range = 2:5,plot.3limit = F)
quartz.save(file="Examples/Chapter 4/Example 4.14b.pdf",type="pdf");dev.off()

PlotVerse("theWayIAm",m.range = 15:18,plot.3limit = F)
quartz.save(file="Examples/Chapter 4/Example 4.14c.pdf",type="pdf");dev.off()

# 15: Drug Ballad, verse 1, mm. 1–4 ---------------------------------------

PlotVerse("drugBallad1",m.range=0:3)
quartz.save(file="Examples/Chapter 4/Example 4.15.pdf",type="pdf");dev.off()

# 16: Renegade, verse two, mm. 5-8 ----------------------------------------

PlotVerse("renegade2",m.range=4:7,plot.rhymeClasses = T)
quartz.save(file="Examples/Chapter 4/Example 4.16.pdf",type="pdf");dev.off()

# 17 Possible grooves beginning at 0 of m. 5 ------------------------------

grooves = fread("DerivedData/corpus_grooves.txt")
grooves = tbl_df(grooves)
grooves %>% filter(verse=="renegade2", start==64,adj.length>=16) %>% 
  select(class,rotation,effort,rate,length,adj.length) %>% 
  group_by(adj.length) %>% slice(1) %>% arrange(class,desc(adj.length)) %>% 
  ungroup %>% mutate(index=(dim(.)[1]):1) -> x
quartz(width=4.55,height=2)
par(mar = c(0,0,0,0),cex=.65)
x1 = -35
plot(0,0,col="white",xlim=c(x1,64),ylim=c(dim(x)[1],0),yaxt="n",ylab="",xaxt="n",xlab="",bty="n")
for(i in 1:dim(x)[1]){
  lines(c(0,x$length[i]),rep(i,2),lty=match(x$class[i],unique(x$class)),lwd=2,col=gray(.7))
  lines(c(0,x$adj.length[i]),rep(i,2),lty=match(x$class[i],unique(x$class)),lwd=2)
  text(x1,i,
       paste(i,". ",x$class[i],"-",x$rotation[i]," (",x$length[i],",",x$effort[i],")",sep=""),
       font=3,pos=4,family="Times New Roman")
}
text(seq(0,64,16),rep(0,5),c("m. 5","m. 6","m. 7","m.8","m. 9"),font=3,family="Times New Roman",cex=.65)
quartz.save(file="Examples/Chapter 4/Example 4.17.pdf",type="pdf");dev.off()
remove(list=c("x","x1","i","grooves"))

# 18 Segmentation of “Renegade --------------------------------------------
source("Scripts/PlottingScripts/PlotGrooveSegmentation.r")
segments = fread("DerivedData/corpus_groove_segments.txt",header=T)
PlotGrooveSegmentation("renegade2")
quartz.save(file="Examples/Chapter 4/Example 4.18.pdf",type="pdf");dev.off()
remove(list=c("segments","GrooveSwapDistance","GrooveSwapDistanceVerse","RotateGroovePF","PlotGrooveSegmentation"))

# Example 6.13 Groove segmentations of verses of Soldier ------------------
# Note: this needs to be rotated on the page.
source("Scripts/PlottingScripts/PlotGrooveSegmentation.r")
quartz(width=4.5,height=2.5)
par(mfcol = c(2,2),mar=c(0,0,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
llply(c("loseYourselfDemo1","loseYourself1"),PlotGrooveSegmentation,
      r.limit=c(0,.125),new.quartz=F,plot.all.costs=F,
      x.max=1.2)
quartz.save(file="Examples/Chapter 4/Example 4.19.pdf",type="pdf");dev.off()
