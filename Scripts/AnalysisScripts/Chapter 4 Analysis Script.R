source("Scripts/loadLibraries.r")
# Endnote 2: Percent of rhymes that are couplets ----------------------------------
load("DerivedData/FlowBookCorpus.rdata")

corpora %>% filter(rhymeIndex==1) %>% group_by(corpus,verse,rhymeClass) %>% 
  summarize(n=length(syllable)) %>% group_by(corpus,n) %>% 
  summarize(l = length(n)) %>% 
  acast(corpus~n) %>% 
  apply(.,1,function(i) round(i/sum(i,na.rm=T),digits=2)) %>% 
  t %>% 
  .[,2] %>% 
  .[c("genre1","eminem","blackThought","talibKweli")]

# Endnote 14: Interval content histograms ----------------------------------------------

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
GetIntervalContentHistogram(c(0,3,6),8)

GetIntervalContentHistogram(c(0,3,6,9,12,14),16)
groove.classes = list(
  c(2, 2, 2, 2, 2, 2, 2, 2),
  c(3, 3, 2, 3, 3, 2),
  c(3, 3, 3, 2, 3, 2),
  c(3, 3, 3, 3, 2, 2),
  c(3, 3, 2, 2, 2, 2, 2),
  c(3, 2, 3, 2, 2, 2, 2) ,
  c(3, 2, 2, 3, 2, 2, 2))
ICHs = laply(groove.classes,function(i){
  i%>% cumsum %>% .[1:(length(.)-1)] %>% c(0,.) %>% GetIntervalContentHistogram(.,16)
})
#ICHs[1,8] = 8
rownames(ICHs) = c("<2222 2222>", "<332 332>", "<333232>","<333322>","<332 2222>","<323 2222>","<3223222>")
write.table(ICHs,file="Examples/Chapter 4/Example 12 table.txt",quote=F,row.names=T,sep="\t")
remove(list=c("ICHs","GetIntervalContentHistogram"))

# Groove class ICH entropy
apply(ICHs,1,function(i){
  entropy(i)/log(length(i))
})


# Endnote 16: Distribution of accent within the beat -----------------------------------

corpora %>% 
  filter(corpus=="genre1",accent==1,beatIndex %% 1 %in% seq(0,.75, .25)) %>% 
  group_by(mod1 = beatIndex %% 1) %>% 
  summarize(n = length(syllable)) %>% 
  mutate(prop = round(n/sum(n),digits=2))

# Endnote 18: Estimating metric complexity -----------------------

source("Scripts/DataScrubbingScripts/GetL3Durations.R")
titles = corpora %>% filter(corpus=="genre1") %>% distinct(verse) %>% .[["verse"]] %>% as.character
m = matrix(0,0,4)
for(i in titles){
  d = GetL3Durations(i)
  s = d %>% strsplit(.,"") %>% unlist %>% as.numeric %>% cumsum %>% c(.,0) %>% sort
  a = rep(0,max(s)+11)
  a[s+1] = 1
  a = a[1:((length(a) %/% 4)*4)]
  x = matrix(a,length(a) %/% 4,4,byrow=T)
  m = rbind(m,x)
}
v = apply(m,1,function(i) paste(i,collapse="")) # 4-length character
counts = table(v)[c("1010","0010","1001","0100","0101")]
proportion = counts/sum(counts)
negLog = -log(proportion)
scaledNegLog = (negLog-min(negLog))/max(negLog-min(negLog))
scaledNegLog = c(0,.744,.721,.941,1);names(scaledNegLog) = c("1010","0010","1001","0100","0101")
remove(list=c("d","GetL3Durations","i","m","proportion","s","v","x"))
# Pressing's cognitive complexity, scaled from 0-1.
pressing.comp = c(1,5,4.5,7.5,10)
pressing.comp = (pressing.comp - 1)
pressing.comp= round(pressing.comp/max(pressing.comp),digits=2)
Table2 = data.frame(pattern = c("1010","0010","1001","0100","0101"),
                    count = unname(as.vector((counts))),
                    neg.log = round(unname(as.vector((negLog))),digits=2),
                    Pressing = pressing.comp,
                    scaled = unname(scaledNegLog))
names(Table2)[1] = "beat-accent type"
write.table(Table2,file="Examples/Chapter 4/Table 4.2.txt",quote=F,row.names=F,sep="\t")
remove(list=c("titles","Table2","a","pressing.comp","counts","negLog"))

# Endnote 19: Metric complexity of Lose Yourself and 8 Mile -------------------------

GetGrooveComplexity = function(onsets){
  weights = scaledNegLog
  a = rep(0,16);a[onsets+1] = 1
  m = apply(matrix(a,4,4,byrow=T),1,function(i) paste(i, collapse=""))
  round(sum(weights[m]),digits=2)
}
GetGrooveComplexity(onsets = c(2,4,7,10,12,15)) # Lose Yourself
GetGrooveComplexity(onsets = c(0,3,5,8,11,13)) # 8 Mile


# Endnote 20: Metric complexity for all the grooves -----------------------------------

onsets = llply(groove.classes, function(dseg){c(0,cumsum(dseg)[1:(length(dseg)-1)])})
rotations = llply(onsets,function(a){
  laply(0:15,function(i) sort((a+i)%%16))
})
names(rotations) = c("<2222 2222>", "<332 332>", "<333232>","<333322>","<332 2222>","<323 2222>","<3223222>")
comp = laply(rotations,function(i){
  apply(i,1,function(j) GetGrooveComplexity(j))
})
rownames(comp) = c("<2222 2222>", "<332 332>", "<333232>","<333322>","<332 2222>","<323 2222>","<3223222>")
vals = apply(comp,1,function(i) unique(i))
names(vals) = rownames(comp)
remove(list=c("GetGrooveComplexity","onsets","rotations","vals","comp","scaledNegLog"))

# 21. Dynamic time warping of [0, 3, 5, 8, 11, 13] and [0, 3, 6, 8, 11, 13] 
m5 = c(0,3,5,8,11,13) # beat class of Renegade, m. 6
m6 = c(0,3,6,8,11,13)
warp = dtw(m5,m6,distance.only=T)
print(warp$distance)
remove(list=c("m5","m6","warp"))

# 21. Grooves beginnings on 0 of m. 5 of "Renegade," verse two.
grooves = fread("DerivedData/corpus_grooves.txt") %>% tbl_df
x = grooves %>% filter(verse=="renegade2", start==64,adj.length>=16) %>% 
  select(class,effort,rate,length,adj.length) %>% 
  group_by(adj.length) %>% slice(1) %>% arrange(class,desc(adj.length)) %>% 
  ungroup %>% mutate(index=6:1)
remove(list=c("x","grooves"))

# Endnote 23: demonstration of effort in dtw ------------------------------

m5 = c(0,3,5,8,11,13) # beat class of Renegade, m. 6
m6 = c(0,3,6,8,11,13)
dtw(m5,m6,distance.only=T)$distance

mm56 = c(c(0,3,5,8,11,13),c(0,3,6,8,11,13)+16)
mm56.duple = seq(0,30,2)
dtw(mm56,mm56.duple,distance.only=T)$distance
dtw(c(0,2,4),c(0,3),distance.only=T)$distance
# Endnote 25: Grooves beginning in m. 5 of renegade ----
grooves = fread("DerivedData/corpus_grooves.txt") %>% tbl_df
x = grooves %>% filter(verse=="renegade2", start==64,adj.length>=16) %>% 
  select(class,effort,rate,length,adj.length) %>% 
  group_by(adj.length) %>% slice(1) %>% arrange(class,desc(adj.length)) %>% 
  ungroup %>% mutate(index=6:1)
# Endnote 27: Distribution of grooves (Table 4.3) ---------------------------------------------

segments = fread("DerivedData/corpus_groove_segments.txt",header=T)
Table3 = segments %>%  
  filter(corpus=="genre1") %>% 
  group_by(effort.rate,class) %>% 
  summarize(l = sum(length)) %>% 
  arrange(class,effort.rate) %>% 
  mutate(prop = round(l/sum(l,na.rm=T),digits=2)) %>% 
  acast(effort.rate~class) %>% 
  t %>% 
  .[order(.[,1]),]
Table3 = Table3[order(Table3[,1],decreasing=T),]
write.table(Table3,"Examples/Chapter 4/Table 4.3.txt")

# Endnote 33: rates of measures and tracks that are duple -----------------

# % of measures that are duple
segments %>% filter(corpus=="genre1",effort.rate == 0) %>% group_by(class) %>% 
  summarize(l = sum(length)) %>% mutate(prop = round(l/sum(l),digits=2))
segments %>% filter(corpus=="genre1",effort.rate == 0.25) %>% group_by(class) %>% 
  summarize(l = sum(length)) %>% mutate(prop = round(l/sum(l),digits=2))

# % of tracks exclusively duple
segments %>% filter(corpus=="genre1",effort.rate == 0) %>% group_by(verse,class) %>% 
  summarize(l = sum(length)) %>% mutate(prop = round(l/sum(l),digits=2)) %>% 
  filter(class=="22222222") %>% .$prop %>% equals(1) %>% sum
segments %>% filter(corpus=="genre1",effort.rate == 0.25) %>% group_by(verse,class) %>% 
  summarize(l = sum(length)) %>% mutate(prop = round(l/sum(l),digits=2)) %>% 
  filter(class=="22222222") %>% .$prop %>% equals(1) %>% sum
