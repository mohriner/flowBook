source("Scripts/loadLibraries.r")
source("Scripts/PlottingScripts/PlotVerse.r")
load("DerivedData/FlowBookCorpus.rdata")
source("Scripts/PlottingScripts/PlotGrooveSegmentation.r")
segments = fread("DerivedData/corpus_groove_segments.txt",header=T)
corpora = fread("DerivedData/FlowBookCorpus.txt",header=T)

# Example 6.1: Distribution of groove classes ----
b = segments %>% filter(corpus %in% c("eminem","genre1"),effort.rate == .125) %>%
  group_by(corpus,class) %>% 
  summarize(s = sum(length)) %>% 
  group_by(corpus) %>% 
  mutate(p = round(s/sum(s),digits=2)) %>% 
  dcast(.,corpus~class)
quartz(width=4.5,height=3)
par(mfcol=c(2,1),mar=c(3.5,3.5,1,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
g.classes = c("<332_332>","<333232>","<333322>","<3223222>","<323_2222>","<332_2222>","<2222_2222>")
g.classes = gsub("<","",g.classes);g.classes = gsub(">","",g.classes)
barplot(as.matrix(b[,2:8]),beside=T,names.arg = g.classes)
legend(x = .5,y = .55,legend = c("Eminem","Rap Music"),
       fill=gray(c(.25,.75)),
       bty="n",y.intersp = .75)
barplot(as.matrix(b[,2:7]),beside=T,names.arg = g.classes[1:6])
legend(x = .5,y = .27,legend = c("Eminem","Rap Music"),
       fill=gray(c(.25,.75)),
       bty="n",y.intersp = .75)
quartz.save(file="Examples/Chapter 6/Example 6.1.pdf",type="pdf");dev.off()
remove(list=c("b"))

# Example 6.2: Metric complexity of groove classes ----
# This is a reprint of Example 4.13; see "Chapter 4 Plotting Script.r"

# Example 6.3: Distribution of rotations of <323_2222>
titles = corpora %>% 
  filter(corpus %in% c("genre1","eminem")) %>% 
  arrange(corpus,as.character(verse)) %>% 
  .$verse %>% unique %>% as.character
r = segments %>% filter(verse %in% titles,effort.rate == .125,
                        class %in% c("3232222")) %>%
  group_by(corpus,rotation) %>% 
  summarize(s = sum(length)) %>% 
  group_by(corpus) %>% 
  mutate(p = round(s/sum(s),digits=2)) %>% 
  acast(.,corpus~rotation)
r[1,][which(is.na(r[1,]))] = 0
r[2,][which(is.na(r[2,]))] = 0
quartz(width=4.5,height=1.5)
par(mar=c(3.5,3.5,1,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
barplot(as.matrix(r),beside=T)
legend(x = 0,y = .32,legend = c("Eminem","Rap Music"),
       fill=gray(c(.25,.75)),
       bty="n",y.intersp = .75)
quartz.save(file="Examples/Chapter 6/Example 6.3.pdf",type="pdf");dev.off()
remove(list=c("r","titles"))

# Example 6.4: Plot sets of rotations 2 and 4 of <323_2222> ---------------

source("Scripts/PlottingScripts/PlotSet.r")
RotatePF = function(pf,rot,expand = F){
  o = pf %>% strsplit("") %>% .[[1]] %>% as.numeric %>% cumsum
  a = rep(0,last(o))
  a[o] = 1
  a = c(last(a),a[1:(length(a)-1)])
  if(rot==0){
    return(which(a==1)-1)
  }
  a = c(a[((length(a)-rot)+1):length(a)],a[1:(length(a)-rot)])
  which(a==1) - 1
}
g = list(
  g0 = RotatePF("3232222",2),
  g1 = RotatePF("3232222",4)
  )
quartz(width=4.55,height=2.2525)
par(mar = rep(0,4),mfrow=c(1,2))
PlotSet(g[[1]],16,launch.quartz = F) # Rotation 2
PlotSet(g[[2]],16,launch.quartz = F) # Rotation 4
quartz.save(file="Examples/Chapter 6/Example 6.4.pdf",type="pdf");dev.off()
remove(list=c("RotatePF","g","PlotSet"))

# Example 6.5 “The Good Fight” versus “Till I Collapse" -------------------

PlotVerse("theGoodFight",m.range = 9:10)
quartz.save(file="Examples/Chapter 6/Example 6.5a.pdf",type="pdf");dev.off()
PlotVerse("tillICollapse1",m.range = 10:14)
quartz.save(file="Examples/Chapter 6/Example 6.5b.pdf",type="pdf");dev.off()



# Example 6.6. Pitch in "Go to Sleep" ----
source("Scripts/PlottingScripts/Chapter 6 plot of pitch in Go to Sleep.R")

# Example 6.7. Go to Sleep segmentation ----
PlotGrooveSegmentation("goToSleep1",plot.all.costs = F,r.limit = c(0,.0625),new.quartz = T)
quartz.save(file="Examples/Chapter 6/Example 6.7.pdf",type="pdf");dev.off()

# Example 6.8. Set plots of Go to Sleep grooves ----
source("Scripts/PlottingScripts/PlotSet.r")
RotatePF = function(pf,rot,expand = F){
  o = pf %>% strsplit("") %>% .[[1]] %>% as.numeric %>% cumsum
  a = rep(0,last(o))
  a[o] = 1
  a = c(last(a),a[1:(length(a)-1)])
  if(rot==0){
    return(which(a==1)-1)
  }
  a = c(a[((length(a)-rot)+1):length(a)],a[1:(length(a)-rot)])
  which(a==1) - 1
}


# Example 6.9, “The Re-Up” mm. 1-8 ----------------------------------------

PlotVerse("theReup",m.range = -1:7)
quartz.save(file="Examples/Chapter 6/Example 6.9.pdf",type="pdf");dev.off()

# Example 6.10, “The Re-Up” groove segmentation ---------------------------

PlotGrooveSegmentation("theReup",plot.all.costs = F, r.limit=c(0,.25),x.max = 1.05,new.quartz = T)
quartz.save(file="Examples/Chapter 6/Example 6.10.pdf",type="pdf");dev.off()

# Example 6.11, “The Re-Up” mm. 19-23 ----------------------------------------

PlotVerse("theReup",m.range = 18:23)
quartz.save(file="Examples/Chapter 6/Example 6.11.pdf",type="pdf");dev.off()


# Example 6.12, Opening hypermeasures of verses of “Soldier" ---------------

PlotVerse("soldier1",m.range = -1:3)
quartz.save(file="Examples/Chapter 6/Example 6.12a.pdf",type="pdf");dev.off()
PlotVerse("soldier2",m.range = -1:3)
quartz.save(file="Examples/Chapter 6/Example 6.12b.pdf",type="pdf");dev.off()
PlotVerse("soldier3",m.range = -1:3)
quartz.save(file="Examples/Chapter 6/Example 6.12c.pdf",type="pdf");dev.off()


# Example 6.13 Groove segmentations of verses of Soldier ------------------
# Note: this needs to be rotated on the page.
quartz(width=6.5,height=2.5)
par(mfcol = c(2,3),mar=c(0,0,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
llply(paste("soldier",1:3,sep=""),PlotGrooveSegmentation,r.limit=c(0,.25),x.max = 1.1,new.quartz=F,plot.all.costs=F)
quartz.save(file="Examples/Chapter 6/Example 6.13.pdf",type="pdf");dev.off()


# Example 6.14: Groove segmentation of tillICollapse1 ---------------------

PlotGrooveSegmentation("tillICollapse1",plot.all.costs = F, r.limit=c(0,.25),new.quartz = T)
quartz.save(file="Examples/Chapter 6/Example 6.14.pdf",type="pdf");dev.off()


# Example 6.15: Exemplars of groove in tillICollapse1 ---------------------

PlotVerse("tillICollapse1",m.range = 2:3,)
quartz.save(file="Examples/Chapter 6/Example 6.15a.pdf",type="pdf");dev.off()
PlotVerse("tillICollapse1",m.range = 14:15)
quartz.save(file="Examples/Chapter 6/Example 6.15b.pdf",type="pdf");dev.off()


# Example 6.16: Till I Collapse, verse 1, mm. 5-8 -------------------------

PlotVerse("tillICollapse1",m.range = 4:7)
quartz.save(file="Examples/Chapter 6/Example 6.16.pdf",type="pdf");dev.off()


# Example 6.17. Groove segmentation of Till I Collapse, Verse 2 -----------

PlotGrooveSegmentation("tillICollapse2",plot.all.costs = F, r.limit=c(.125),new.quartz = T)
quartz.save(file="Examples/Chapter 6/Example 6.17.pdf",type="pdf");dev.off()

# Example 6.18. Groove segmentation of Till I Collapse, Verse 2 -----------

PlotGrooveSegmentation("tillICollapse3",plot.all.costs = F, r.limit=c(.125),new.quartz = T)
quartz.save(file="Examples/Chapter 6/Example 6.18.pdf",type="pdf");dev.off()


# Example 6.19. Lyric themes in “8 Mile -----------------------------------

# See word file

# Example 6.20. 8 mile, verse 1, mm. 7-11 ---------------------------------

PlotVerse("8mile1",m.range = 6:10)
quartz.save(file="Examples/Chapter 6/Example 6.20.pdf",type="pdf");dev.off()

# Example 6.21. Groove segmentation of 8 Mile, Verse 2 -----------

PlotGrooveSegmentation("8mile2",plot.all.costs = F, r.limit=c(.125),new.quartz = T)
quartz.save(file="Examples/Chapter 6/Example 6.21.pdf",type="pdf");dev.off()

# Example 6.22. Groove segmentation of 8 Mile, Verse 2 -----------

PlotGrooveSegmentation("8mile3",plot.all.costs = F, r.limit=c(.125),new.quartz = T)
quartz.save(file="Examples/Chapter 6/Example 6.22.pdf",type="pdf");dev.off()