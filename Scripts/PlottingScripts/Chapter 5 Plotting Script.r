source("Scripts/loadLibraries.r")
source("Scripts/PlottingScripts/PlotVerse.r")
source("Scripts/PlottingScripts/PlotGrooveSegmentation.r")
source("Scripts/AnalysisScripts/CentralPillarFunctions.r")
source("Scripts/AnalysisScripts/GroovinessFeatureFunctions.R")
load("DerivedData/FlowBookCorpus.rdata")
source("Scripts/AnalysisScripts/RhymeEntropyFunction.r")

meta = read.delim("SourceData/verseMetadata.txt",header=T)
features = read.delim("DerivedData/CorpusGlobalFeatures.txt")

# Example 5.1, visual demonstration of the t.test ------------------------

alim = c(-1,1)
blim = c(-.5,1.5)
cwin = 1.5
bwin = .25
n = 20
a =  runif(n,alim[1],alim[2])
b =  runif(n/2,mean(blim)-bwin,mean(blim)+bwin)
c1 = runif(n/8,mean(blim)-cwin,mean(blim)-(cwin-1))
c2 = runif(n/8,mean(blim)+(cwin-1),mean(blim)+cwin)
c = c(c1,c2)
quartz(width=4.55,height=1)
par(mfcol = c(3,1),font.main=3,mar=c(0,0,1,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
l = list(a,b,c)
llply(1:3, function(i){
  plot(l[[i]],rep(0,length(l[[i]])),type = "o",xlim=range(c(a,b,c)),pch=20,yaxt="n",xaxt="n",
       ylim=c(-1,.5))
  text(mean(l[[i]]),0,"*",cex = 2)
  title(main=letters[i],line=0,adj=0)
})
t.test(a,b)$p.value
t.test(a,c)$p.value
quartz.save(file="Examples/Chapter 5/Example 5.1.pdf",type="pdf");dev.off()
remove(list=c("a","alim","b","blim","c","c1","c2","cwin","l","n"))

# Example 5.2. Density of syllable delivery -------------------------------

quartz(width=4.55,height=1.5)
par(mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
x = density(features$sylsPerSecond);x$call = ""
plot(x,xlab="Syllables per second")
remove(list = c("x","sylRate"))
quartz.save(file="Examples/Chapter 5/Example 5.2.pdf",type="pdf");dev.off()

# Example 5.3a Syllable rate by region------------------------------------------

quartz(width=4.55,height=1.5)
par(mfcol=c(1,2),mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
x = features %>% filter(region=="east") %>% .$sylsPerSecond %>% density
x$call=""
plot(x,xlab="Syllables per second",xlim=c(2,9))
x = llply(c("west","south","midwest"),function(r){
  x = features %>% filter(region==r) %>% .$sylsPerSecond %>% density
  x$call=""
  lines(x,lty=which(c("west","south","midwest")==r)+1)
})
legend(x = 6,y = 1,legend = c("east","west","south","midwest"),
       title="region",lty=1:4,
       bty="n",y.intersp = .75)
remove(list=c("x"))

# Example 5.3b Syllable rate over time ------------------------------------
# A function for extracting the p-value from a linear model, from Stephen Turner,
# http://www.gettinggeneticsdone.com/2011/01/rstats-function-for-extracting-f-test-p.html
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

x = features %>% filter(corpus=="genre1") %>% .$year
y = features %>% filter(corpus=="genre1") %>% .$sylsPerSecond
plot(x,y,xlab="Year",ylab="Syllables per second",pch=20)
abline(lm(y~x))
pValue = lm(y~x) %>% lmp %>% round(.,digits=2)
text(x = 1980,y = 6.5,paste("p < ",pValue,sep=""), font=3,pos=4)
quartz.save(file="Examples/Chapter 5/Example 5.3.pdf",type="pdf");dev.off()
remove(list=c("pValue","x","y"))

# Example 5.4a, tempo over time -------------------------------------------
quartz(width=4.55,height=1.5)
par(mfrow=c(1,2),mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
x = features %>% filter(corpus=="genre1") %>% .$year
y = features %>% filter(corpus=="genre1") %>% .$tempo
plot(x,y,xlab="Year",ylab="Beats per minute",pch=20)
abline(lm(y~x))
pValue = lm(y~x) %>% lmp %>% round(.,digits=4)
text(x = 1980,y = 135,"p < .0001", font=3,pos=4)

# Example 5.4b Saturation over time ---------------------------------------

x = features %>% filter(corpus=="genre1") %>% .$year
y = features %>% filter(corpus=="genre1") %>% .$saturation

plot(x,y,xlab="Year",ylab="Saturation",pch=20)
abline(lm(y~x))
pValue = lm(y~x) %>% lmp %>% round(.,digits=4)
text(x = 1980,y = 14,paste("p < .003",sep=""), font=3,pos=4)
quartz.save(file="Examples/Chapter 5/Example 5.4.pdf",type="pdf");dev.off()
remove(list=c("pValue","titles","x","y"))


# Example 5.5 Change in saturation ------------------------------------------------

quartz(width=3,height=1.5)
par(mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
d = features %>% filter(corpus=="genre1") %>% .$sat.delta %>% density
d$call=""
plot(d,xlab="Change in saturation")

x1 = features %>% filter(verse=="blackAndYellow") %>% .$sat.delta
y1 = d$y[which.min(abs(d$x - x1))]
points(x1,y1,pch=20)
text(x1,y1,"Black and Yellow",pos=3)

x1 = features %>% filter(verse=="theBreaks") %>% .$sat.delta
y1 = d$y[which.min(abs(d$x - x1))]
points(x1,y1,pch=20)
text(x1,y1,"The Breaks",pos=3)

quartz.save(file="Examples/Chapter 5/Example 5.5.pdf",type="pdf");dev.off()
remove(list = c("d","x1","y1"))

# Example 5.6a, Kurtis Blow -----------------------------------------------

PlotVerse("theBreaks",m.range = -1:3)
quartz.save(file="Examples/Chapter 5/Example 5.6a.pdf",type="pdf");dev.off()

# Example 5.6b, Wiz Khalifa -----------------------------------------------

PlotVerse("blackAndYellow",m.range = 4:7)
quartz.save(file="Examples/Chapter 5/Example 5.6b.pdf",type="pdf");dev.off()


# Example 5.7: Circular average of line and breath ending -------------------------------------------------
source("Scripts/AnalysisScripts/CentralPillarFunctions.r")

titles = corpora %>% filter(corpus=="genre1") %>% .$verse %>% unique
x = data.frame(
  phBeginning = laply(titles,function(i) getVersePillar(i,"beginning","phrase")),
  liBeginning = laply(titles,function(i) getVersePillar(i,"beginning","line")),
  phEnding = laply(titles,function(i) getVersePillar(i,"ending","phrase")),
  liEnding = laply(titles,function(i) getVersePillar(i,"ending","line"))
)
quartz(width=3,height=1.5)
par(mar = c(0,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n')
ylim1 = c(0,.7)
x$phEnding %>% 
  positions2CircularDensity %>% 
  plot(.,main="",shrink = 1.4,zero.line=F,plot.type="line",
       xaxt="n",xlab="",ylim=ylim1,xlim = c(-pi,2*pi))
x$phBeginning %>% 
  positions2CircularDensity %>% 
  lines(.,main="",zero.line=F,plot.type="line",lty=2)
y = llply(0:3,function(i) lines(rep(i/4*2*pi,2),ylim1,lty=3))
y = llply(0:3,function(i) text(i/4*2*pi,ylim1[2],c(0,4,8,12)[i+1],pos = 4,font=3))
y = llply(0:3,function(i) lines(rep((i/4*2*pi)-(2*pi),2),ylim1,lty=3))
y = llply(0:3,function(i) text((i/4*2*pi)-(2*pi),ylim1[2],c(0,4,8,12)[i+1],pos = 4,font=3))
quartz.save(file="Examples/Chapter 5/Example 5.7.pdf",type="pdf");dev.off()
remove(list=c("getVersePillar","getCircularDensityPeak","positions2CircularDensity","x","y","ylim1"))

# Example 5.8., Kanye West, Stronger, mm. 5-8 -----------------------------

PlotVerse("stronger",m.range = 8:11)
quartz.save(file="Examples/Chapter 5/Example 5.8.pdf",type="pdf");dev.off()

# Example 5.9 (Text, no plotting) -----------------------------------------


# Example 5.10a Stronger plot -----------------------------------------------------------

PlotVerse("stronger",row.index = 3:36,plot.rhymeClasses = T)
quartz.save(file="Examples/Chapter 5/Example 5.10a.pdf",type="pdf");dev.off()

# Example 5.10b My Crew plot ------------------------------------------------------------

PlotVerse("myCrew",row.index = 169:220,plot.rhymeClasses = T)
quartz.save(file="Examples/Chapter 5/Example 5.10b.pdf",type="pdf");dev.off()

# Example 5.11a Histograms of IRIs in stronger and My Crew, consecutive -----------------------------------------

quartz(width=3,height=1.5)
par(mfrow = c(1,2),mar = c(3.5,3.5,1,1),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',family="Times New Roman")

corpora %>% 
  filter(verse=="stronger") %>% 
  slice(3:36) %>% 
  getIRIs(.,"consecutive") %>% 
  c(.,seq(0,8,.25)) %>% 
  table %>%
  subtract(1) %>% 
  barplot(.,xlab="IRI",ylab="Frequency",ylim=c(0,3))
corpora %>% 
  filter(verse=="myCrew") %>% 
  slice(169:220) %>% 
  getIRIs(.,"consecutive") %>% 
  c(.,seq(0,8,.25)) %>% 
  table %>%
  subtract(1) %>% 
  barplot(.,xlab="IRI",ylab="Frequency",ylim=c(0,3))
quartz.save(file="Examples/Chapter 5/Example 5.11a.pdf",type="pdf");dev.off()

# Example 5.11b. Histograms of IRIs in stronger and My Crew, all -------------------------------------------------
quartz(width=3,height=1.5)
par(mfrow = c(1,2),mar = c(3.5,3.5,1,1),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',family="Times New Roman")

corpora %>% 
  filter(verse=="stronger") %>% 
  slice(3:36) %>% 
  getIRIs(.,"all") %>% 
  c(.,seq(0,8,.25)) %>% 
  table %>%
  subtract(1) %>% 
  barplot(.,xlab="IRI",ylab="Frequency",ylim=c(0,3))
corpora %>% 
  filter(verse=="myCrew") %>% 
  slice(169:220) %>% 
  getIRIs(.,"all") %>% 
  c(.,seq(0,8,.25)) %>% 
  table %>%
  subtract(1) %>% 
  barplot(.,xlab="IRI",ylab="Frequency",ylim=c(0,3))
quartz.save(file="Examples/Chapter 5/Example 5.11b.pdf",type="pdf");dev.off()

# Example 5.12 Density of Entropy, corpus ---------------------------------------------------
# with Stronger, My Crew, and Lighter's Up
quartz(width=3,height=1.5)
par(mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
d = features %>% filter(corpus=="genre1") %>% .$rhymeEntropy %>% density
d$call=""
plot(d,xlab="Rhyme entropy")

x1 = features %>% filter(verse=="stronger") %>% .$rhymeEntropy
y1 = d$y[which.min(abs(d$x - x1))]
points(x1,y1,pch=20)
text(x1,y1,"Stronger",pos=3)

x1 = features %>% filter(verse=="myCrew") %>% .$rhymeEntropy
y1 = d$y[which.min(abs(d$x - x1))]
points(x1,y1,pch=20)
text(x1,y1,"My Crew",pos=3)

x1 = features %>% filter(verse=="lightersUp") %>% .$rhymeEntropy
y1 = d$y[which.min(abs(d$x - x1))]
points(x1,y1,pch=20)
text(x1,y1,"Lighters Up",pos=3)
quartz.save(file="Examples/Chapter 5/Example 5.12.pdf",type="pdf");dev.off()
remove(list=c("x","x1","y1","d"))

# Example 5.13 Plot of “Lighters Up" ---------------------------------------------------

PlotVerse("lightersUp", row.index = 1:86,plot.rhymeClasses = T)
quartz.save(file="Examples/Chapter 5/Example 5.13.pdf",type="pdf");dev.off()

# Example 5.14 Histogram of IRIs, “Lighters Up" ----------------------------------------

quartz(width=3,height=1.5)
par(mar = c(3.5,3.5,1,1),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',family="Times New Roman")

corpora %>% 
  filter(verse=="lightersUp") %>% 
  #slice(1:49) %>% 
  getIRIs(.,"all") %>% 
  c(.,seq(0,8,.25)) %>% 
  table %>%
  subtract(1) %>% 
  barplot(.,xlab="IRI",ylab="Frequency")
quartz.save(file="Examples/Chapter 5/Example 5.14.pdf",type="pdf");dev.off()
remove(list="Verse2IRIs")

# Example 5.15 Density of histograms, mod 4 ---------------------------------------------------
# with Stronger, My Crew, and Lighter's Up
quartz(width=3,height=1.5)
par(mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
d = features %>% filter(corpus=="genre1") %>% .$rhymeEntropy4 %>% density
d$call=""
plot(d,xlab="Rhyme entropy (mod4)")

x1 = features %>% filter(verse=="stronger") %>% .$rhymeEntropy4
y1 = d$y[which.min(abs(d$x - x1))]
points(x1,y1,pch=20)
text(x1,y1,"Stronger",pos=3)

x1 = features %>% filter(verse=="myCrew") %>% .$rhymeEntropy4
y1 = d$y[which.min(abs(d$x - x1))]
points(x1,y1,pch=20)
text(x1,y1,"My Crew",pos=3)

x1 = features %>% filter(verse=="lightersUp") %>% .$rhymeEntropy4
y1 = d$y[which.min(abs(d$x - x1))]
points(x1,y1,pch=20)
text(x1,y1,"Lighters Up",pos=1)
remove(list = "d","x1","y1")
quartz.save(file="Examples/Chapter 5/Example 5.15.pdf",type="pdf");dev.off()
remove(list=c("getIRIs","getVerseEntropy","vals","vals.mod4"))


###########################################################################
# Grooviness --------------------------------------------------------------
###########################################################################


# Example 5.16: Segmentation plots of Rick Ross and Wiz Khalifa -----------------------

# (the motivating example is Aston Martin Music vs. Black and Yellow)
# Proportion of verse in a single groove
quartz(width=4.5,height = 1)
par(mar=c(0,0,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
PlotGrooveSegmentation("blackAndYellow",new.quartz = F,r.limit = 0,plot.all.costs = F)
quartz.save(file="Examples/Chapter 5/Example 5.16a.pdf",type="pdf");dev.off()
quartz(width=4.5,height = 1)
par(mar=c(0,0,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
PlotGrooveSegmentation("astonMartin",new.quartz = F,r.limit = 0,plot.all.costs = F)
quartz.save(file="Examples/Chapter 5/Example 5.16b.pdf",type="pdf");dev.off()

# Example 5.17 Density of grooviness ---------------------------------------------------
quartz(width=3,height=1.5)
par(mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
d = features %>% filter(corpus=="genre1") %>% .$grooviness %>% density
d$call=""
plot(d,xlab="Grooviness")

x1 = features %>% filter(verse=="astonMartin") %>% .$grooviness
y1 = d$y[which.min(abs(d$x - x1))]
points(x1,y1,pch=20)
text(x1,y1,"Aston Martin Music",pos=3)

x1 = features %>% filter(verse=="blackAndYellow") %>% .$grooviness
y1 = d$y[which.min(abs(d$x - x1))]
points(x1,y1,pch=20)
text(x1,y1,"Black and Yellow",pos=1)

quartz.save(file="Examples/Chapter 5/Example 5.17.pdf",type="pdf");dev.off()
remove(list=c("d","x","x1","y1"))


###########################################################################
# Groove Adherence --------------------------------------------------------
###########################################################################

# Example 5.18: Groove segmentations of The Magic Clap and Feel the New Heartbeat -------

quartz(width=4.5,height = 1.75)
par(mfcol = c(2,1),mar=c(0,0,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
PlotGrooveSegmentation("theMagicClap",new.quartz = F,r.limit = 0,plot.all.costs = F,next.level = .125)
PlotGrooveSegmentation("theMagicClap",new.quartz = F,r.limit = 0.125,plot.all.costs = F, mute.adherence = T)
quartz.save(file="Examples/Chapter 5/Example 5.18a.pdf",type="pdf");dev.off()

quartz(width=4.5,height = 1.75)
par(mfcol = c(2,1),mar=c(0,0,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
PlotGrooveSegmentation("feelTheNew",new.quartz = F,r.limit = 0,plot.all.costs = F,x.max = .72,next.level = .125)
PlotGrooveSegmentation("feelTheNew",new.quartz = F,r.limit = 0.125,plot.all.costs = F,x.max = .72,
                       mute.adherence = T)
quartz.save(file="Examples/Chapter 5/Example 5.18b.pdf",type="pdf");dev.off()

# Example 5.19: Density of adherence ----------------------------------------------------

quartz(width=3,height=1.5)
par(mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
d = features %>% filter(corpus=="genre1") %>% .$adherence %>% density
d$call=""
plot(d,xlab="Groove adherence")

x1 = features %>% filter(verse=="feelTheNew") %>% .$adherence
y1 = d$y[which.min(abs(d$x - x1))]
points(x1,y1,pch=20)
text(x1,y1,"Aston Martin Music",pos=3)

x1 = features %>% filter(verse=="blackAndYellow") %>% .$adherence
y1 = d$y[which.min(abs(d$x - x1))]
points(x1,y1,pch=20)
text(x1,y1,"Black and Yellow",pos=1)
quartz.save(file="Examples/Chapter 5/Example 5.19.pdf",type="pdf");dev.off()



###########################################################################
# Groove Typicality -------------------------------------------------------
###########################################################################

# Example 5.20 Typicality in Aint Now Half and The Breaks -----------------

quartz(width=4.5,height = 2)
par(mfcol = c(2,1),mar=c(0,0,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
PlotGrooveSegmentation("aintNoHalf",new.quartz = F,r.limit = 0,plot.all.costs = F)
PlotGrooveSegmentation("aintNoHalf",new.quartz = F,r.limit = 0.125,plot.all.costs = F)
quartz.save(file="Examples/Chapter 5/Example 5.20a.pdf",type="pdf");dev.off()
quartz(width=4.5,height = 2)
par(mfcol = c(2,1),mar=c(0,0,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
PlotGrooveSegmentation("theBreaks",new.quartz = F,r.limit = 0,plot.all.costs = F)
PlotGrooveSegmentation("theBreaks",new.quartz = F,r.limit = 0.125,plot.all.costs = F)
quartz.save(file="Examples/Chapter 5/Example 5.20b.pdf",type="pdf");dev.off()


# Example 5.21, PCA visual demonstration ----------------------------------

quartz(width=4.5,height=4.5)
par(mfrow = c(2,2),mar=c(6,6,1,1),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
x = data.frame(duple = seq(.7,.3,-.1),nonduple = seq(.3,.7,.1))
p = prcomp(x,
           center = TRUE,
           scale. = TRUE,
           retx = T)
r = lm(x$nonduple~x$duple)
# Upper Left
plot(x$duple,x$nonduple,ylab="Percent non-duple",xlab="Percent duple",pch=20,cex=2,
     xlim=c(0,1),ylim=c(0,1))
abline(r,lty=2)
abline(0,1,lty=3)
text(x = x$duple,y = x$nonduple,labels = 5:1,pos=3)
title(sub = paste("x-axis variance = ",round(var(x$duple),digits=2),sep=""),line=3.5)
title(sub = paste("y-axis variance = ",round(var(x$nonduple),digits=2),sep=""),line=4.5)
# Upper right
w = 3
plot(p$x[,1],p$x[,2],ylim = w*c(-1,1),xlim=w*c(-1,1),
     ylab="Component #2",xlab="Component #1",pch=20,cex=2)
text(p$x[,1],p$x[,2],labels = 5:1,pos=3)
abline(0,0,lty=2)
lines(c(0,0),w*c(1,-1),lty=3)
title(sub = paste("x-axis variance = ",var(p$x[,1]),sep=""),line=3.5)
title(sub = paste("y-axis variance = 0"),line=4.5)

# Lower left
x = data.frame(duple = c(seq(.7,.4,-.1),.1),nonduple = seq(.3,.7,.1))
p = prcomp(x,
           center = TRUE,
           scale. = TRUE,
           retx = T)
r = lm(x$nonduple~x$duple)

plot(x$duple,x$nonduple,ylab="Percent non-duple",xlab="Percent duple",pch=20,cex=2,
     ylim =c(0,1),xlim=c(0,1))
text(x = x$duple,y = x$nonduple,labels = 5:1,pos=3)

abline(a = .8037736,b = -.6604,lty=2)
abline(a = -.2,b = 1.5143,lty=3)
title(sub = paste("x-axis variance = ",round(var(x$duple),digits=2),sep=""),line=3.5)
title(sub = paste("y-axis variance = ",round(var(x$nonduple),digits=2),sep=""),line=4.5)

# Lower right
plot(p$x[,1],p$x[,2],ylim = w*c(-1,1),xlim=w*c(-1,1),
     ylab="Component #2",xlab="Component #1",pch=20,cex=2)
lines(c(-w,w),c(0,0),lty=2)
lines(c(0,0),c(-w,w),lty=3)
text(p$x[,1],p$x[,2],labels = 5:1,pos=3)
title(sub = paste("x-axis variance = ",round(var(p$x[,1]),digits=2),sep=""),line=3.5)
title(sub = paste("y-axis variance = ",round(var(p$x[,2]),digits=2),sep=""),line=4.5)
lines(rep(p$x[1,1],2),c(p$x[1,2],-4),lwd=.5,lty=3)
lines(rep(p$x[5,1],2),c(p$x[5,2],-4),lwd=.5,lty=3)
remove(list=c("d","p","r","w","x1","y1","x"))
quartz.save(file="Examples/Chapter 5/Example 5.21.pdf",type="pdf");dev.off()


# Example 5.22, bi-plot of correspondence analysis ------------------------


# "Outliers are points that have high absolute co-ordinate values and high 
# contributions. They are represented, on the graph, very far from the centroïd. 
# In this case, the remaining row/column points tend to be tightly clustered in 
# the graph which become difficult to interpret.

# In the CA output, the coordinates of row/column points represent the number of 
# standard deviations the row/column is away from the barycentre 
# (Bendixen M. 2003)." Alboukadel Kassambara, "Correspondence Analysis in R"
# STDHA Blog http://www.sthda.com/english/wiki/correspondence-analysis-in-r-the-ultimate-guide-for-the-analysis-the-visualization-and-the-interpretation-r-software-and-data-mining

devtools::install_github("kassambara/factoextra")
library("factoextra")

# Make 'm', a matrix detailing the proportion of each verse spent in 
# each groove class.
segments = read.delim("DerivedData/corpus_groove_segments.txt",header=T)
segments = tbl_df(segments)
ignore = c("cadillactica","thugLove","twentyFiveBucks","holyGrail","drinkInMyCup","disAintWhat","studio","tilItsGone")
# the above are verses that have lots of C24 or C32 rhythms, and the 
# model of groove proposed here, tailored to C16, does not address them well.
m = segments %>% filter(!verse %in% ignore,
                        corpus=="genre1",
                        effort.rate == .125) %>% 
  group_by(verse) %>% 
  mutate(p = length/sum(length)) %>% 
  group_by(verse,class) %>% 
  summarize(n = round(sum(p),digits=2)) %>% 
  acast(verse~class)
for(i in 1:7){m[is.na(m[,i]),i] = 0} # replace NA with 0

targets = meta %>% filter(tranche=="genre1") %>% .$title %>% unique %>% as.character
m = m[rownames(m) %in% targets,]
m.df = m %>% as.data.frame %>% 
  mutate(verse = rownames(m))
# Plot contribution of classes to dimensions
dt <- as.table(as.matrix(m))
flow.ca = CA(dt,ncp = 4,graph = F)
quartz(width=3,height=3)
par(mar=c(3.5,3.5,4,1),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',family="Times New Roman")

fviz_ca_biplot(flow.ca, map ="rowprincipal",
               arrow = c(FALSE, TRUE),invisible = "row",ylim=c(-2,5),xlim=c(-2,3))
quartz.save(file="Examples/Chapter 5/Example 5.22.pdf",type="pdf");dev.off()
remove(list=c("dt","i","ignore","m","segments","targets","titles"))

# Table 5.1. Atypicality --------------------------------------------------

# Typicality: contribution of each verse to the first four dimensions, 
# scaled by eigenvalues
eig <- get_eigenvalue(flow.ca)
contrib <- flow.ca$row$contrib
typ = laply(1:4,function(i) contrib[,i] * eig[i,1]) %>% 
  colSums %>% unlist %>% unname

m.df %>% 
  mutate(atypicality=typ) %>% 
  arrange(atypicality) %>% 
  slice(c(1:5,63:67)) %>% 
  .[c(8:9,7:1)] %>% 
  write.table(.,file="Examples/Chapter 5/Table5.1.txt",quote = F,sep = "\t")
remove(list=c("eig","typ","contrib","flow.ca","m.df"))

