source("Scripts/loadLibraries.r")
source("Scripts/DataScrubbingScripts/AppendContinuousOnsets.r")
source("Scripts/PlottingScripts/PlotVerse.r")
source("Scripts/PlottingScripts/PlotVerseSpiral.r")
source("Scripts/AnalysisScripts/NonAlignmentFunctions.r")
meta = read.delim("SourceData/verseMetadata.txt",header=T)

# HookSections = list(53:67,96:111,197:211) # The "just to get by" indexes.
# Example 8.1 (Lyrics, see text). -----------------------------------------


# Example 8.2, Get By, combined, Hook 1 ---------------------------------

PlotVerse("getby1",combined=T,row.index = 53:67)
quartz.save(file="Examples/Chapter 8/Example 8.2.pdf",type="pdf");dev.off()

# Example 8.3, Get By, Hook 1, spiral -----------------------------------

PlotVerseSpiral("getby1",row.index = 53:67,print.mean.nonalignment = F,
                print.phase.adj = F,
                print.swing.adj = F,
                print.tempo.adj = F,
                width=3.75)
quartz.save(file="Examples/Chapter 8/Example 8.3.pdf",type="pdf");dev.off()

# Example 8.4, Get By, Hook 2, quantized spiral -------------------------

PlotVerseSpiral("getby1",row.index = 53:67,print.mean.nonalignment = F,
                quantized=T,
                print.phase.adj = F,
                print.swing.adj = F,
                print.tempo.adj = F,
                width=3.75)
quartz.save(file="Examples/Chapter 8/Example 8.4.pdf",type="pdf");dev.off()


# Example 8.5: Wave demonstration (in other file) -----------------------------------------

source("Scripts/PlottingScripts/Example 8.5 (Wave demo).R")

# Example 8.6: Adjustment demo (in other file) ----------------------------

source("Scripts/PlottingScripts/Example 8.6 (adjustment demo).R")

# Example 8.7: Reprint Example 8.3 ----------------------------------------

PlotVerseSpiral("getby1",row.index = 53:67,
                print.phase.adj = F,
                print.swing.adj = F,
                print.tempo.adj = F,
                width=3.75)
quartz.save(file="Examples/Chapter 8/Example 8.7.pdf",type="pdf");dev.off()


# Example 8.8: Hook 1, phase adjusted -----------------------------------
s = 53:67
corpora %>% filter(verse=="getby1") %>% slice(s) %>% 
  select(word,beatIndex,c.bi) %>% .$beatIndex -> bi.q
corpora %>% filter(verse=="getby1") %>% slice(s) %>% 
  select(word,beatIndex,c.bi) %>% .$c.bi -> bi.c
o = OptimizeAlignment(bi.q,bi.c,check.phase = T,check.swing = F,check.tempo = F)
quartz(width=4.55,height=2.25)
par(mfcol = c(1,2),mar=c(0,0,0,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
PlotVerseSpiral("getby1",row.index = s,
                print.phase.adj = T,
                print.swing.adj = F,
                print.tempo.adj = F,
                launch.quartz = F)
PlotVerseSpiral("getby1",row.index = s,
                phase.adj = o$phase,
                print.phase.adj = T,
                print.swing.adj = F,
                print.tempo.adj = F,
                launch.quartz = F)
quartz.save(file="Examples/Chapter 8/Example 8.8.pdf",type="pdf");dev.off()

# Example 8.9. Density of phase adjustment, Kweli vs. corpus --------------

source("Scripts/PlottingScripts/Example 8.9, Density of phase adjustment in Kweli vs. genre.3.R")

# Example 8.11. Hook 1, phase and swing ---------------------------------
s = 53:67
corpora %>% filter(verse=="getby1") %>% slice(s) %>% 
  select(word,beatIndex,c.bi) %>% .$beatIndex -> bi.q
corpora %>% filter(verse=="getby1") %>% slice(s) %>% 
  select(word,beatIndex,c.bi) %>% .$c.bi -> bi.c
quartz(width=4.55,height=4.55)
par(mfrow = c(2,2),mar=c(0,0,0,0),cex = .65,las=1,bty='n',
    family="Times New Roman")

o = OptimizeAlignment(bi.q,bi.c,check.phase = T,check.swing = F,check.tempo = F)
PlotVerseSpiral("getby1",row.index = s,
                print.phase.adj = T,
                print.swing.adj = F,
                print.tempo.adj = F,
                launch.quartz = F)
PlotVerseSpiral("getby1",row.index = s,
                phase.adj = o$phase,
                print.phase.adj = T,
                print.swing.adj = F,
                print.tempo.adj = F,
                launch.quartz = F)
o = OptimizeAlignment(bi.q,bi.c,check.phase = T,check.swing = T,check.tempo = F)
PlotVerseSpiral("getby1",row.index = s,
                print.phase.adj = T,
                print.swing.adj = T,
                print.tempo.adj = F,
                launch.quartz = F)
PlotVerseSpiral("getby1",row.index = s,
                phase.adj = o$phase,
                swing.adj = o$swing,
                print.phase.adj = T,
                print.swing.adj = T,
                print.tempo.adj = F,
                launch.quartz = F)
quartz.save(file="Examples/Chapter 8/Example 8.11.pdf",type="pdf");dev.off()

# Example 8.12 Phase, swing, and tempo ------------------------------------

s = 53:67
corpora %>% filter(verse=="getby1") %>% slice(s) %>% 
  select(word,beatIndex,c.bi) %>% .$beatIndex -> bi.q
corpora %>% filter(verse=="getby1") %>% slice(s) %>% 
  select(word,beatIndex,c.bi) %>% .$c.bi -> bi.c
quartz(width=4.55,height=6.75)
par(mfrow = c(3,2),mar=c(0,0,0,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
# Phase
o = OptimizeAlignment(bi.q,bi.c,check.phase = T,check.swing = F,check.tempo = F)
PlotVerseSpiral("getby1",row.index = s,
                print.phase.adj = T,
                print.swing.adj = F,
                print.tempo.adj = F,
                launch.quartz = F)
PlotVerseSpiral("getby1",row.index = s,
                phase.adj = o$phase,
                print.phase.adj = T,
                print.swing.adj = F,
                print.tempo.adj = F,
                launch.quartz = F)
# Phase + Swing
o = OptimizeAlignment(bi.q,bi.c,check.phase = T,check.swing = T,check.tempo = F)
PlotVerseSpiral("getby1",row.index = s,
                print.phase.adj = T,
                print.swing.adj = T,
                print.tempo.adj = F,
                launch.quartz = F)
PlotVerseSpiral("getby1",row.index = s,
                phase.adj = o$phase,
                swing.adj = o$swing,
                print.phase.adj = T,
                print.swing.adj = T,
                print.tempo.adj = F,
                launch.quartz = F)
# Phase + swing + tempo
o = OptimizeAlignment(bi.q,bi.c,check.phase = T,check.swing = T,check.tempo = T)
PlotVerseSpiral("getby1",row.index = s,
                print.phase.adj = T,
                print.swing.adj = T,
                print.tempo.adj = T,
                launch.quartz = F)
PlotVerseSpiral("getby1",row.index = s,
                phase.adj = o$phase,
                swing.adj = o$swing,
                tempo.adj = o$tempo,
                print.phase.adj = T,
                print.swing.adj = T,
                print.tempo.adj = T,
                launch.quartz = F)
# quartz.save(file="Examples/Chapter 8/Example 8.11.pdf",type="pdf") # needs to be manually resize and saved so the lowest line isn't cut off. Might work differently on other screens.

# Example 8.13, Get By, combined, Flow 3c ---------------------------------
corpora %>% filter(verse=="getby1") %>% .$word %>% .[173:196]
PlotVerse("getby1",combined=T,row.index = 173:196)
quartz.save(file="Examples/Chapter 8/Example 8.13.pdf",type="pdf");dev.off()

# Exampl8 8.14, Flow 3c, spiral -------------------------------------------

s = 173:196
corpora %>% filter(verse=="getby1") %>% slice(s) %>% 
  select(word,beatIndex,c.bi) %>% .$beatIndex -> bi.q
corpora %>% filter(verse=="getby1") %>% slice(s) %>% 
  select(word,beatIndex,c.bi) %>% .$c.bi -> bi.c
o = OptimizeAlignment(bi.q,bi.c,check.phase = T,check.swing = T,check.tempo = T)

quartz(width=4.55,height=2.25)
par(mfrow = c(1,2),mar=c(0,0,0,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
PlotVerseSpiral("getby1",row.index = s,
                print.phase.adj = T,
                print.swing.adj = T,
                print.tempo.adj = T,
                launch.quartz = F)
PlotVerseSpiral("getby1",row.index = s,
                phase.adj = o$phase,
                swing.adj = o$swing,
                tempo.adj = o$tempo,
                print.phase.adj = T,
                print.swing.adj = T,
                print.tempo.adj = T,
                launch.quartz = F)
quartz.save(file="Examples/Chapter 8/Example 8.14.pdf",type="pdf");dev.off()


# Example 8.15: Flow 3c, onsets against duration --------------------------

s = 175:197
x = corpora %>% filter(verse=="getby1") %>% slice(s) %>% .$c.bi %>% .[1:(length(.)-1)]
y = corpora %>% filter(verse=="getby1") %>% slice(s) %>% .$c.bi %>% diff
w = corpora %>% filter(verse=="getby1") %>% slice(s) %>% .$syllable %>% .[1:(length(.)-1)]
pch1 = corpora %>% filter(verse=="getby1") %>% slice(s) %>% .$beatIndex %>% diff %>% multiply_by(4)
quartz(width=3,height=2)
par(mar=c(3.5,3.5,0,0),mgp=2:0,cex = .65,las=1,bty='n',
    family="Times New Roman")
plot(x,y,type="p",bg = c("black","white")[pch1],pch = 21,ylim=c(0,.7),
     xlab="Beat",ylab="Duration (beats)")
abline(h=.25,lty=2,lwd=.5)
abline(h=.5,lty=2,lwd=.5)
text(x,y,w,pos=1,font=3)
llply(list(1:4,5:13,14:22),function(i){
  lm1 = lm(y[i]~x[i])
  lm1 %>% summary %>% names
  print(anova(lm1)[5] %>% unlist %>% .[1] %>% unname)
  lines(x[i],lm1$fitted.values)
})
legend(56,.75,fill=c("white","black"),
       legend = c("1/2 beat (quantized)","1/4 beat (quantized)"),
       bty="n")
quartz.save(file="Examples/Chapter 8/Example 8.15.pdf",type="pdf");dev.off()


# Example 8.16a, Flow 4, combined transcription ---------------------------
s = 212:243
PlotVerse("getby1",combined=T,row.index = s)
quartz.save(file="Examples/Chapter 8/Example 8.16a.pdf",type="pdf");dev.off()

# Example 8.16b: Flow 4, onsets against duration --------------------------

s = 212:243
x = corpora %>% filter(verse=="getby1") %>% slice(s) %>% .$c.bi %>% .[1:(length(.)-1)]
y = corpora %>% filter(verse=="getby1") %>% slice(s) %>% .$c.bi %>% diff
w = corpora %>% filter(verse=="getby1") %>% slice(s) %>% .$syllable %>% .[1:(length(.)-1)]
pch1 = corpora %>% filter(verse=="getby1") %>% slice(s) %>% .$beatIndex %>% diff %>% multiply_by(4)
quartz(width=3,height=2)
par(mar=c(3.5,3.5,0,0),mgp=2:0,cex = .65,las=1,bty='n',
    family="Times New Roman")
plot(x,y,type="p",bg = c("black","white")[pch1],pch = 21,ylim=c(0,.6),
     xlab="Beat",ylab="Duration (beats)")
abline(h=.25,lty=2,lwd=.5)
abline(h=.5,lty=2,lwd=.5)
text(x,y,w,pos=1,font=3)
legend(70.2,.65,fill=c("white","black"),
       legend = c("1/2 beat (quantized)","1/4 beat (quantized)"),
       bty="n")
quartz.save(file="Examples/Chapter 8/Example 8.16b.pdf",type="pdf");dev.off()


# Example 8.17: Density of unadjusted non-alignment -----------------------

load("DerivedData/NonAlignmentFeatures.rdata")
nonalign.meas %>% filter(corpus=="talibKweli") %>% .$async1 %>% density -> a;a$call=""
nonalign.meas %>% filter(corpus=="genre1",verse!="cream") %>% .$async1 %>% density  -> b;b$call=""
quartz(width=3,height=1.5)
par(mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
plot(a,xlab="Average unadjusted non-alignment",ylim=c(0,.11),lwd=2)
lines(b,lty=2,lwd=.5)
legend(25,.11,lwd=c(2,1),lty=c(1,2),legend=c("Talib Kweli","Rap Music"),bty="n")
quartz.save(file="Examples/Chapter 8/Example 8.17.pdf",type="pdf");dev.off()


# Example 8.18 Kweli and percent explained --------------------------------

quartz(width=4.55,height=2.5)
par(mfcol=c(1,2),mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
nonalign.meas %>% filter(verse!="cream",corpus=="talibKweli") %>%
  group_by(async1=round(async1)) %>% select(async1,expl) -> x
x.max = 30
boxplot(x$expl~x$async1,xlim=c(0,x.max),ylim=c(0,1),xlab="Unadjusted non-alignment (minutes)",ylab="Percent explained")
nonalign.meas %>% filter(verse!="cream",corpus=="genre1") %>%
  group_by(async1=round(async1)) %>% select(async1,expl) -> x
boxplot(x$expl~x$async1,xlim=c(0,x.max),ylim=c(0,1),xlab="Unadjusted non-alignment (minutes)",ylab="Percent explained")
quartz.save(file="Examples/Chapter 8/Example 8.18.pdf",type="pdf");dev.off()


# Table 8.1 Non-alignment features ----------------------------------------

x = laply(1:10,function(i){
  j = c(i*2-1,i*2)
  corpora %>% filter(verse=="getby1",line %in% j) %>% .$beatIndex -> bi.q
  corpora %>% filter(verse=="getby1",line %in% j) %>% .$c.bi -> bi.c
  o = OptimizeAlignment(bi.q,bi.c,T,T,T)
  o
},.progress="text")
x = as.data.frame(x)
features = cbind(section = c("Flow 1a","Flow 1b","Hook 1","Flow 2","Hook 2","Flow 3a","Flow 3b","Flow 3c","Hook 3","Flow 4"),x)
features = features[order(features[,1]),]
write.table(as.matrix(features),file = "Examples/Chapter 8/Table 8.1.txt",quote=F,sep="\t")
