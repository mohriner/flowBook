source("Scripts/loadLibraries.r")
source("Scripts/PlottingScripts/PlotVerse.r")
load("DerivedData/FlowBookCorpus.rdata")
remove(list="GetL3Durations")
meta = read.delim("SourceData/verseMetadata.txt",header=T)


# Example 3.1a. Jean Grae, "My Crew," (2003, 0:45–0:50) --------------------

PlotVerse("myCrew",row.index = 40:62,plot.accent = F,plot.rhymeClasses = F,plot.phrase=F)
points(x = seq(0,3.5,.5),y = rep(3,8),cex=1.25,pch=21,bg="white")
quartz.save(file="Examples/Chapter 3/Example 3.1a.pdf",type="pdf");dev.off()

# Example 3.1b. Logic, "Under Pressure," (2014, 0:36–0:43) --------------------

PlotVerse("underPressure",row.index = 67:105,plot.accent = F,plot.rhymeClasses = F,plot.phrase=F)
points(x = c(.25,.75,1.25,1.75,2.5,3.25,3.75),y = rep(5,7),cex=1.25,pch=21,bg="white")
quartz.save(file="Examples/Chapter 3/Example 3.1b.pdf",type="pdf");dev.off()

# Example 3.2a and b, notation, see other file ----------------------------

# Example 3.3a, Beethoven, see other file ---------------------------------

# Example 3.3b. -----------------------------------------------------------

x = read.delim("SourceData/Other/opus18no1_movement1.txt",header=T)
beethovenTable = x %>% filter(is.na(tie)|tie=="start") %>% group_by(part) %>% 
  mutate(dur = c(diff(beatIndex),NA)) %>% filter(dur<=4) %>% .$dur %>% table
load("DerivedData/FlowBookCorpus.rdata")
rapTable = corpora %>% filter(corpus=="genre1") %>% mutate(dur = 4/quant * duration) %>% 
  filter(dur<=4) %>% .$dur %>% c(.,0:4) %>% table
quartz(width=4.55,height=2)

beethovenTable = beethovenTable/sum(beethovenTable)
rapTable = rapTable/sum(rapTable)
names(rapTable) = rapTable %>% names %>% as.numeric %>% round(.,digits=2)
par(mfrow=c(1,2),mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
plot(beethovenTable,xlim=c(0,4),xaxp=c(0,4,5),
     xlab="Duration (beats)",ylab="Percentage",
     mgp = c(2,1,0),ylim=c(0,.7))
plot(rapTable,xlim=c(0,4),xaxp=c(0,4,5),bty="n",
     xlab="Duration (beats)",ylab="Percentage",
     mgp = c(2,1,0),ylim=c(0,.7))
remove(list=c("beethovenTable","rapTable","x"))
quartz.save(file="Examples/Chapter 3/Example 3.3b.pdf",type="pdf");dev.off()

# Example 3.4, phonological hierarchy, see other file ---------------------


# Example 3.5. Distribution of accents --------------------------------------
# See code for Endnote 5.
distrib = data.frame(primary = c(1164,148,571,275),
                     secondary = c(47,34,71,25),
                     nonaccent = c(227,1025,335,595),
                     monosyl = c(2232,1534,2547,2219))
quartz(width=4.55,height=2.5)
par(mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
barplot(as.matrix(distrib),beside=T,col = gray(seq(.75,0,length.out=4)),
        xlab = "CMUPD accent type",ylab="Count",family="Times New Roman",
        mgp = c(2,1,0),ylim=c(0,3000))
legend(x = 1,y = 3000,legend = c(0,1,2,3),title="mod 4",fill=gray(seq(.75,0,length.out=4)),
       bty="n",y.intersp = .75)
quartz.save(file="Examples/Chapter 3/Example 3.5.pdf",type="pdf");dev.off()


# Example 3.6. The Roots, I Remember, mm. 1–4 -----------------------------

PlotVerse("iRemember1",m.range = 0:3,plot.accent = F,plot.rhymeClasses = T,plot.phrase=F)
quartz.save(file="Examples/Chapter 3/Example 3.6.pdf",type="pdf");dev.off()

# Example 3.7. The Roots, Walk Alone, mm. 1–5 -----------------------------

PlotVerse("walkAlone3",m.range = 0:4,plot.accent = F,plot.rhymeClasses = T,
          plot.phrase=F,showOverride = F)
quartz.save(file="Examples/Chapter 3/Example 3.7.pdf",type="pdf");dev.off()


# Example 3.8, now or never -----------------------------------------------

PlotVerse("nowOrNever",row.index = 121:145,plot.accent=F,plot.rhymeClasses = F)
quartz.save(file="Examples/Chapter 3/Example 3.8.pdf",type="pdf");dev.off()

# Example 3.9a. Provisional accents in Jean Grae, “My Crew" ---------------

PlotVerse("myCrew",row.index = 40:62,plot.accent = T,plot.rhymeClasses = F,plot.phrase=F,
          showOverride = T)
quartz.save(file="Examples/Chapter 3/Example 3.9a.pdf",type="pdf");dev.off()

# Example 3.9b. Provisional accents in Logic, "Under Pressure ---------------

# Before sourcing the next line, comment out lines 29-44, which apply the second
# and third phases of the detection algorithm.
source("Scripts/DataScrubbingScripts/AppendAccent_partial.r")
accent1 = AppendAccent(corpora %>% 
               filter(verse=="underPressure") %>% 
               select(-accent)
             ) %>% 
  .[67:105,] %>% 
  filter(accent==T) %>% 
  select(word,beatIndex,accent) %>% 
  .$beatIndex
PlotVerse("underPressure",row.index = 67:106,
          plot.accent = F,
          small.only = T,
          plot.rhymeClasses = F,
          plot.phrase=T,
          showOverride = F)
points(x = accent1%%4,y = accent1%/%4,cex=1.25,pch=21,bg="white")
quartz.save(file="Examples/Chapter 3/Example 3.9b.pdf",type="pdf");dev.off()

# Example 3.10a. Provisional accents in Logic, "Under Pressure ---------------

# Before sourcing the next line, comment out lines 37-44, which apply the third phases of the detection algorithm.
source("Scripts/DataScrubbingScripts/AppendAccent_partial2.r")
accent1 = AppendAccent(corpora %>% 
                         filter(verse=="underPressure") %>% 
                         select(-accent)
) %>% 
  .[67:105,] %>% 
  filter(accent==T) %>% 
  select(word,beatIndex,accent) %>% 
  .$beatIndex
PlotVerse("underPressure",row.index = 67:106,
          plot.accent = F,
          small.only = T,
          plot.rhymeClasses = F,
          plot.phrase=T,
          showOverride = F)
points(x = accent1%%4,y = accent1%/%4,cex=1.25,pch=21,bg="white")
quartz.save(file="Examples/Chapter 3/Example 3.10a.pdf",type="pdf");dev.off()

# Example 3.10b ------------------------------------------------------------
load("DerivedData/FlowBookCorpus.rdata")

PlotVerse("underPressure",row.index = 67:106,
          plot.accent = T,
          plot.rhymeClasses = F,
          plot.phrase=T,
          showOverride = F,
          revertToggles = T)
quartz.save(file="Examples/Chapter 3/Example 3.10b.pdf",type="pdf");dev.off()

# Example 3.11 Y'all Ready Know, with manual corrections ----
PlotVerse("underPressure",row.index = 67:106,
          plot.accent = T,
          plot.rhymeClasses = F,
          plot.phrase=T,
          showOverride = T)
quartz.save(file="Examples/Chapter 3/Example 3.11.pdf",type="pdf");dev.off()

# Example 3.12a, Esham, Sunshine -------------------------------------------

PlotVerse("sunshine",revertToggles = T,row.index = 15:38,showOverride = F,
            plot.rhymeClasses = T)
quartz.save(file="Examples/Chapter 3/Example 3.12a.pdf",type="pdf");dev.off()

# Example 3.12b, Esham, Sunshine -------------------------------------------

PlotVerse("sunshine",revertToggles = F,row.index = 15:38,showOverride = T,
          plot.rhymeClasses = F)
quartz.save(file="Examples/Chapter 3/Example 3.12b.pdf",type="pdf");dev.off()

# Example 3.13, Kurtis Blow, “Basketball" ---------------------------------

PlotVerse("basketball",plot.rhymeClasses = F)
bi = c(0,1.25,2,4,6,10,12,13.75,16,17,17.75,20,22,24.75,26.75,28,30)
text(x = bi %%4, y = bi %/% 4-.35,">",family="Times New Roman",cex=.75)
quartz.save(file="Examples/Chapter 3/Example 3.13.pdf",type="pdf");dev.off()

# Example 3.14 The Fire and Lighthouse ------------------------------------

PlotVerse("theFire2",m.range = 2:3,plot.rhymeClasses = T)
quartz.save(file="Examples/Chapter 3/Example 3.14a.pdf",type="pdf");dev.off()

PlotVerse("lighthouse2",m.range = 10:11,plot.rhymeClasses = T)
quartz.save(file="Examples/Chapter 3/Example 3.14b.pdf",type="pdf");dev.off()

# 3.15a ---------------------------------------------------------------------
PlotVerse("feelTheNew",m.range = 0:3,plot.rhymeClasses = T)
quartz.save(file="Examples/Chapter 3/Example 3.15a.pdf",type="pdf");dev.off()


# 3.15b ---------------------------------------------------------------------
PlotVerse("whattaMan",m.range = -1:5,plot.rhymeClasses = T)
quartz.save(file="Examples/Chapter 3/Example 3.15b.pdf",type="pdf");dev.off()

