# This excerpt provides some objects necessary for making Example 13

source("Scripts/loadLibraries.r")

# 4. Estimating metric complexity -----------------------

load("DerivedData/FlowBookCorpus.rdata")
groove.classes = list(
  c(2, 2, 2, 2, 2, 2, 2, 2),
  c(3, 3, 2, 3, 3, 2),
  c(3, 3, 3, 2, 3, 2),
  c(3, 3, 3, 3, 2, 2),
  c(3, 3, 2, 2, 2, 2, 2),
  c(3, 2, 3, 2, 2, 2, 2) ,
  c(3, 2, 2, 3, 2, 2, 2))
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

#write.table(Table2,file="Examples/Chapter 4/Table 2.txt",quote=F,row.names=F,sep="\t")


# 5 Metric complexity of Lose Yourself and 8 Mile -------------------------

GetGrooveComplexity = function(onsets){
  weights = scaledNegLog
  a = rep(0,16);a[onsets+1] = 1
  m = apply(matrix(a,4,4,byrow=T),1,function(i) paste(i, collapse=""))
  round(sum(weights[m]),digits=2)
}
GetGrooveComplexity(onsets = c(2,4,7,10,12,15)) # Lose Yourself
GetGrooveComplexity(onsets = c(0,3,5,8,11,13)) # 8 Mile


# 6 Metric complexity for all the grooves -----------------------------------

onsets = llply(groove.classes, function(dseg){c(0,cumsum(dseg)[1:(length(dseg)-1)])})
rotations = llply(onsets,function(a){
  laply(0:15,function(i) sort((a+i)%%16))
})
names(rotations) = c("<2222_2222>", "<332_332>", "<333232>","<333322>","<332_2222>","<323_2222>","<3223222>")
comp = laply(rotations,function(i){
  apply(i,1,function(j) GetGrooveComplexity(j))
})
rownames(comp) = c("<2222_2222>", "<332_332>", "<333232>","<333322>","<332_2222>","<323_2222>","<3223222>")
vals = apply(comp,1,function(i) unique(i))
names(vals) = rownames(comp)
remove(list=c("a","comp","counts","GetGrooveComplexity","negLog","onsets","rotations","scaledNegLog","titles"))

