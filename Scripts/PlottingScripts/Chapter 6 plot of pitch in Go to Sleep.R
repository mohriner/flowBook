source("Scripts/loadLibraries.r")
load("DerivedData/FlowBookCorpus.rdata")
tg2matrix = function(TG){
  l = grep("item",TG) %>% TG[.];l = l[2:length(l)]
  n = grep("name",TG) %>% TG[.]
  s1 = grep("item \\[1\\]",TG)
  s2 = grep("item \\[2\\]",TG)
  s3 = grep("item \\[3\\]",TG)
  
  # Lines with bracketed numbers
  ints = grep("\\[[[:digit:]]",TG)
  l1 = ints[which(ints>s1&ints<s2)]
  l2 = ints[which(ints>s2)]
  on1 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l1+1]))
  off1 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l1+2]))
  lab1 = gsub("[(text\\=[:space:]\")]","",TG[l1+3])
  on2 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l2+1]))
  off2 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l2+2]))
  lab2 = gsub("[(text\\=[:space:]\")]","",TG[l2+3])
  
  ph = data.frame(on1,off1,lab1)
  syl = data.frame(on2,off2,lab2)
  list(phones=ph,syl = syl)
}

# Convert the text grid
TG = readLines("SourceData/Other/goToSleep1.TextGrid")
onsets = tg2matrix(TG)[["syl"]][1:319,]
onsets = onsets[onsets$lab2!="sp",]
remove(list = c("tg2matrix"))
# Get pitch and intensity
p = read.delim("SourceData/Other/goToSleep1_PRAAT_pitchOutput.txt",
               header=T,sep=" ")
p = p[,c(1,4)]
p$Time_s = as.numeric(as.character(p$Time_s))
p$F0_Hz = as.numeric(as.character(p$F0_Hz))

inten = read.delim("SourceData/Other/goToSleep1_PRAAT_intensityOutput.txt",
                   header=T,sep=" ")[,c(1,4)]
inten$Time_s = as.numeric(as.character(inten$Time_s))
inten$Intensity_dB = as.numeric(as.character(inten$Intensity_dB))


onsets = mutate(onsets, pitch = laply(1:dim(onsets)[1],function(i){
  p1 = p[which(p$Time_s>=unlist(onsets$on2[i]) & p$Time_s<=unlist(onsets$off2[i])),]
  p1$F0_Hz %>% median(na.rm=T)
}))
onsets = mutate(onsets, inten = laply(1:dim(onsets)[1],function(i){
  i1 = inten[which(inten$Time_s>=unlist(onsets$on2[i]) & inten$Time_s<=unlist(onsets$off2[i])),]
  i1$Intensity_dB %>% median(na.rm=T)
}))

onsets = rbind(onsets[1:103,],rep(NA,5),onsets[104:dim(onsets)[1],]) # there's an extra syllable in the corpus that isn't in the textgrid
remove(list=c("inten","p","TG"))

# Combine pitch, intensity, and corpus representation
corpora %>% filter(verse=="goToSleep1") %>% 
  mutate(pitch = onsets$pitch) %>% 
  group_by(line) %>% 
  summarize(o = mean(beatIndex),p = median(pitch,na.rm=T)) %>% 
  .$o -> x
corpora %>% filter(verse=="goToSleep1") %>% 
  mutate(pitch = onsets$pitch) %>% 
  group_by(line) %>% 
  summarize(o = mean(beatIndex),p = median(pitch,na.rm=T)) %>% 
  .$p -> y

quartz(width=4.55,height=1.5)
par(mar = c(3.5,3.5,0,0),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',
    family="Times New Roman")
plot(x/4,y,pch=19,type="p",xlab="Measures elapsed",ylab="Median pitch (Hz)")
text(x/4,y,1:24,pos=1)
remove(list=c("x","y"))
quartz.save(file="Examples/Chapter 6/Example 6.6.pdf",type="pdf");dev.off()

