source("Scripts/loadLibraries.r")
source("Scripts/PlottingScripts/PlotVerse.r")
load("DerivedData/FlowBookCorpus.rdata")
meta = read.delim("SourceData/verseMetadata.txt",header=T)

# 1., Classified, “Still Got It,” (2009, 0:40–0:52, Excerpt 1.1) -----------------------------------

PlotVerse("stillGotIt",plot.accent = F,plot.rhymeClasses = F,plot.phrase=F)
quartz.save(file="Examples/Chapter 1/Example 1.1.pdf",type="pdf");dev.off()

# 2., Ice-T, “I Ain’t New Ta This,” (1993, 1:14–1:20, Excerpt 1.2) -----------------------------------

PlotVerse("iAint",plot.accent = F,plot.rhymeClasses = F,plot.phrase=F)
quartz.save(file="Examples/Chapter 1/Example 1.2.pdf",type="pdf");dev.off()

# 3., Ice-T, “I Ain’t New Ta This,” recomposed to align copy and sloppy -----------------------------------

PlotVerse("iAintRecomposed",plot.accent = F,plot.rhymeClasses = F,plot.phrase=F)
quartz.save(file="Examples/Chapter 1/Example 1.3.pdf",type="pdf");dev.off()

# 4., Eminem, “Business” (2002, 0:42–0:52, Excerpt 1.3) -----------------------------------

PlotVerse("business1",plot.accent = F,plot.rhymeClasses = F,plot.phrase=F,
          m.range = -1:3,showOverride = F)
Gray = c(2.5,4,5.25,5.75,6.5,8,9.25,10.5,13.25,14.5)
points(Gray%%4,Gray%/%4,cex=1.25,col=gray(.75),pch=20)
Black = c(1.25,3,4.5,7,8.5,9.75,11,12.5,13.75,15)
points(Black%%4,Black%/%4,cex=1.25,col=gray(0),pch=20)
remove(list=c("Black","Gray"))
quartz.save(file="Examples/Chapter 1/Example 1.4.pdf",type="pdf");dev.off()

# 5.OutKast featuring Jay-Z and Killer Mike, “Flip Flop Rock” (2003, 1:42–2:02, Excerpt 1.4) ----

PlotVerse("flipFlopRock",plot.accent = F,plot.rhymeClasses = F,plot.phrase=F)
quartz.save(file="Examples/Chapter 1/Example 1.5.pdf",type="pdf");dev.off()

# 6.A$AP Rocky featuring ASAP Nast and Spaceghost Purrp, “Purple Swag: Chapter 2” (2011, 0:54–1:03, Excerpt 1.5) ----

PlotVerse("purpleSwag",plot.accent = F,plot.rhymeClasses = F,plot.phrase=F)
quartz.save(file="Examples/Chapter 1/Example 1.6.pdf",type="pdf");dev.off()

# 7.
PlotVerse("takeFromMe(halfTime)",plot.accent = F,plot.rhymeClasses = F,plot.phrase=F,
          row.index = 1:62)
quartz.save(file="Examples/Chapter 1/Example 1.7.pdf",type="pdf");dev.off()


# 8.Lil’ Wayne featuring Nikki, “Weezy Baby” (2005, 0:37–0:50, Excerpt 1.6) ----

meta = read.delim("SourceData/verseMetadata.txt",header=T)
PlotVerse("weezyBaby",plot.accent = F,plot.rhymeClasses = F,plot.phrase=F)
corpora %>% filter(verse=="weezyBaby") %>% .$syllable
corpora %>% filter(verse=="weezyBaby") %>% 
  slice(c(2,6,9,13,20,26,32,36,40,44)) %>% 
  .$beatIndex %>% divide_by_int(.,4) -> y
corpora %>% filter(verse=="weezyBaby") %>% 
  slice(c(2,6,9,13,20,26,32,36,40,44)) %>% 
  .$beatIndex %>% mod(.,4) -> x
points(x,y,pch=20)
quartz.save(file="Examples/Chapter 1/Example 1.8.pdf",type="pdf");dev.off()
remove(list=c("x","y"))
# 9. Twista, “Say What?” (1992, 1:16–1:21, Excerpt 1.7) ----

PlotVerse("sayWhat",plot.accent = F,plot.rhymeClasses = F,plot.phrase=F)
quartz.save(file="Examples/Chapter 1/Example 1.9.pdf",type="pdf");dev.off()

# 10. Fat Joe featuring The Game, “Breathe and Stop,” (2006, 2:18–2:30, Excerpt 1.8) ----

PlotVerse("breatheAndStop",plot.accent = F,plot.rhymeClasses = F,plot.phrase=F)
quartz.save(file="Examples/Chapter 1/Example 1.10.pdf",type="pdf");dev.off()

# 11a. Flobots, “Airplane Mode,” (2010, 3:14–3:40, Excerpt 1.9) ----

PlotVerse("airplaneMode",plot.accent = F,plot.rhymeClasses = F,plot.phrase=F)
quartz.save(file="Examples/Chapter 1/Example 1.11a.pdf",type="pdf");dev.off()

# 12a. T.I. featuring André 3000, “Sorry,” (2012, 2:11–2:26, Excerpt 1.10) ----

PlotVerse("sorry",plot.accent = F,plot.rhymeClasses = F,plot.phrase=F)
quartz.save(file="Examples/Chapter 1/Example 1.12a.pdf",type="pdf");dev.off()

# 13a. KRS-One, “Don’t Get So High,” (2008, 1:05–1:17, Excerpt 1.11) ----

PlotVerse("dontGet",plot.accent = F,plot.rhymeClasses = F,plot.phrase=F)
quartz.save(file="Examples/Chapter 1/Example 1.13a.pdf",type="pdf");dev.off()

# 14. KRS-One, “Don’t Get So High,” recomposed to maintain a consistent three-beat duration between rhymes. ----

PlotVerse("dontGetRecomposed",plot.accent = F,plot.rhymeClasses = F,plot.phrase=F)
quartz.save(file="Examples/Chapter 1/Example 1.14.pdf",type="pdf");dev.off()

# 15. Krizz Kaliko featuring Tech N9ne, “Strange,” mm. 9–14 (2012, 4:08–4:31, Excerpt 1.12) ----

PlotVerse("strange",plot.accent = F,plot.rhymeClasses = F,plot.phrase=F,m.numbers=9:14)
quartz.save(file="Examples/Chapter 1/Example 1.15.pdf",type="pdf");dev.off()

# 16. Tech N9ne, “Strange,” mm. 9–14, re-quantized to unified C32 metric space (i.e., without swing). ----

PlotVerse("strangeUnswung",plot.accent = F,plot.rhymeClasses = F,plot.phrase=F,m.numbers=9:14)
quartz.save(file="Examples/Chapter 1/Example 1.16.pdf",type="pdf");dev.off()

# 17. Tech N9ne, “Strange,” mm. 9–14, with larger circles representing accented syllables. ----

PlotVerse("strangeUnswung",plot.accent = T,plot.rhymeClasses = F,plot.phrase=F,m.numbers=9:14)
quartz.save(file="Examples/Chapter 1/Example 1.17.pdf",type="pdf");dev.off()

# # 18. Tech N9ne, “Strange,” mm. 9–14, schematic view of accent ----
# 
# PlotVerse("strangeUnswung",plot.accent = T,accentOnly = T,
#           plot.rhymeClasses = F,plot.phrase=F,m.numbers=9,
#           m.range = 0)
# quartz.save(file="Examples/Chapter 1/Example 1.18(1)_unannotated.pdf",type="pdf");dev.off()
# PlotVerse("strangeUnswung",plot.accent = T,accentOnly = T,
#           plot.rhymeClasses = F,plot.phrase=F,m.numbers=13,
#           m.range = 4)
# quartz.save(file="Examples/Chapter 1/Example 1.18(2)_unannotated.pdf",type="pdf");dev.off()
# # This image gets annotated, so what is plotted here is not identical to the
# # example in the book.

