source("Scripts/loadLibraries.r")
source("Scripts/PlottingScripts/PlotVerse.r")
load("DerivedData/FlowBookCorpus.rdata")
meta = read.delim("SourceData/verseMetadata.txt",header=T)

# Example 2.2. Chronological and geographical distribution of the corpus sample ----
# (n=225) and sub-sample (n=75)
a = meta %>% filter(grepl("genre",tranche)==T) %>% 
  select(artistOriginYear,region)
b = meta %>% filter(grepl("genre1",tranche)==T) %>% 
  select(artistOriginYear,region)

quartz(width=4.55,height=4.55)
par(mfrow=c(2,2),mar = c(3.5,3.5,3,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")

# Plot 1
a$artistOriginYear %>% hist(main="",ylim=c(0,50))
title(main = "Year of career beginning",font.main=1,line=2)
title(main = "(Population)",font.main=1,line=1)
# Plot 2
b$artistOriginYear %>% hist(main="",ylim=c(0,15))
title(main = "Year of career beginning",font.main=1,line=2)
title(main = "(Sample)",font.main=1,line=1)
# Plot 3
a1 = table(a$region)
a1 = a1[which(a1>0)]
a1 = sort(a1,decreasing=T)
barplot(a1,xaxt="n",ylab="Frequency")
text(x = seq(.5,length(a1),length.out = length(a1)),y = rep(-12,length(a1)), 
     names(a1),
     srt=45,xpd=T)
title(main = "Geographic region",font.main=1,line=2)
title(main = "(Population)",font.main=1,line=1)
# Plot 4
b1 = c(as.character(b$region),"east","south","west","midwest","non-US")
b1 = table(b1)-1
b1 = sort(b1,decreasing=T)
barplot(b1,xaxt="n",ylab="Frequency")
text(x = seq(.5,length(b1),length.out = length(b1)),y = rep(-5,length(b1)), 
     names(b1),
     srt=45,xpd=T)
title(main = "Geographic region",font.main=1,line=2)
title(main = "(Population)",font.main=1,line=1)
quartz.save(file="Examples/Chapter 2/Example 2.2.pdf",type="pdf");dev.off()
remove(list=c("a","b","a1","b1"))

# Example 2.5 Bubba Sparxxx, Deliverance, mm. 1-4 -------------------------

PlotVerse("deliverance",m.range = 0:3,plot.accent = F,plot.rhymeClasses = F,plot.phrase=F)
quartz.save(file="Examples/Chapter 2/Example 2.5.pdf",type="pdf");dev.off()


# Example 2.4. 8.Lil’ Wayne featuring Nikki, “Weezy Baby” (2005, 0:37–0:50, Excerpt 1.6) ----

PlotVerse("weezyBaby",m.range = 0:1,plot.accent = F,plot.rhymeClasses = F,plot.phrase=F)
# There's more to making this example, done in Excel and Word, so it isn't saved here.