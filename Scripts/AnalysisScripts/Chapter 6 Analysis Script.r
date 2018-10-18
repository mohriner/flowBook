source("Scripts/loadLibraries.r")
source("Scripts/AnalysisScripts/GroovinessFeatureFunctions.R")
meta = read.delim("SourceData/verseMetadata.txt")
grooves = fread("DerivedData/corpus_grooves.txt",header=T)
segments = fread("DerivedData/corpus_groove_segments.txt",header=T)
corpora = fread("DerivedData/FlowBookCorpus.txt",header=T)
source("Scripts/DataScrubbingScripts/GetL3Durations.R")
# Endnote 1: Avoidance of dupleness ----
titles = corpora %>% 
  filter(corpus %in% c("genre1","eminem")) %>% 
  arrange(corpus,as.character(verse)) %>% 
  .$verse %>% unique %>% as.character
getDupleness = function(verseTitle){
  n = segments %>% 
    filter(verse==verseTitle,effort.rate==0) %>% 
    .$end %>% max
  d = segments %>% 
    filter(verse==verseTitle,effort.rate==0,class=="22222222") %>% 
    .$length %>% sum
  round(d/n,digits=2)
}
d = laply(titles,getDupleness)
a = d[1:30]
b = d[31:105]
l = list(paste("(M = ",round(mean(a),digits=2),", SD = ",round(sd(a),digits=2),")",sep=""),
         paste("(M = ",round(mean(b),digits=2),", SD = ",round(sd(b),digits=2),")",sep=""),
         paste("t(",round(t.test(a,b)$parameter),") = ",
               round(t.test(a,b)$statistic,digits=2), ", p < ",
               round(t.test(a,b)$p.value,digits=3),
               sep=""))
l
remove(list=c("getDupleness","d","a","b","l"))

# Endnote 2: Verses with no duple groove ----
d = segments %>% filter(verse %in% titles,effort.rate==0.25) %>% 
  group_by(corpus,verse) %>% 
  summarize(d = !any(class=="22222222")) %>% .$d
a = d[1:30]
b = d[31:105]
l = list(paste("(M = ",round(mean(a),digits=2),", SD = ",round(sd(a),digits=2),")",sep=""),
         paste("(M = ",round(mean(b),digits=2),", SD = ",round(sd(b),digits=2),")",sep=""),
         paste("t(",round(t.test(a,b)$parameter),") = ",
               round(t.test(a,b)$statistic,digits=2), ", p < ",
               round(t.test(a,b)$p.value,digits=3),
               sep=""))
l
remove(list=c("d","a","b","l"))

# Endnote 3: Groove Adherence without the duple class -------------------------
x = laply(titles,GetGrooveAdherence,omit="22222222",.progress="text")
a = x[1:30]
b = x[31:105]
l = list(paste("(M = ",round(mean(a),digits=2),", SD = ",round(sd(a),digits=2),")",sep=""),
         paste("(M = ",round(mean(b),digits=2),", SD = ",round(sd(b),digits=2),")",sep=""),
         paste("t(",round(t.test(a,b)$parameter),") = ",
               round(t.test(a,b)$statistic,digits=2), ", p < ",
               round(t.test(a,b)$p.value,digits=3),
               sep=""))
l
remove(list=c("x","a","b"))

# Endnote 11: most frequent grooves in each of the three verses of "Soldier" ----
# and their complexity

segments %>% filter(grepl("soldier",verse),effort.rate==.25) %>% 
  group_by(verse,class) %>% 
  summarize(l = sum(length)) %>% 
  group_by(verse) %>% 
  arrange(desc(l)) %>% 
  slice(1) %>% 
  ungroup %>% 
  select(class,l,verse)
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
  g1 = RotatePF("22222222",0),
  g2 = RotatePF("3322222",8),
  g3 = RotatePF("333232",5))
laply(g,GetGrooveComplexity)

# Endnote 12: Increasing grooviness in verses of "Soldier" ----
GetGrooviness = function(v,lim = 32,e = 0){
  g = segments %>% 
    tbl_df %>% 
    filter(verse==v,effort.rate == e) %>% 
    mutate(long = length>=lim) %>% 
    group_by(verse,long) %>% 
    summarize(l = sum(length)) %>% 
    acast(verse~long, value.var="l") %>% 
    unlist
  if(!"FALSE" %in% colnames(g)){
    g = cbind("FALSE" = 0,g)
  }
  if(!"TRUE" %in% colnames(g)){
    g = cbind(g,"TRUE" = 0)
  }
  (g/sum(g)) %>% .[2] %>% round(.,digits=2)
}
laply(paste("soldier",1:3,sep=""),GetGrooviness)

# Endnote 15: Demonstration that Flobots reiteration of 3 IRIs is unique ----
titles = corpora %>% .$verse %>% unique %>% sort
IRIs = llply(titles,function(i){
  c1 = corpora %>% filter(verse==i) %>% .$rhymeClass %>% is.na %>% not %>% any
  if(c1==T){
    return(Verse2IRIs(i,method="consecutive",return.list=T))
  }else{
    return(vector())
  }
},.progress="text",.inform=T)
names(IRIs) = titles
Verse2IRIs("airplaneMode","consecutive") # Notice: 3, 3, 3.
target = c(3,3)
laply(IRIs,function(i){
  x = llply(i,function(j){
    if(length(j)>=3){
      return(rollapply(j,length(target),function(k){identical(k,target)}) %>% any)
    }else{
      return(F)
    }
  })
  any(unlist(x))
}) %>% which %>% titles[.]
target = c(3,3,3)
laply(IRIs,function(i){
  x = llply(i,function(j){
    if(length(j)>=3){
      return(rollapply(j,length(target),function(k){identical(k,target)}) %>% any)
    }else{
      return(F)
    }
  })
  any(unlist(x))
}) %>% which %>% titles[.]

# Start here --------------------------------------------------------------

charts = read.delim("SourceData/Other/genre1_chartPositions.txt")
charts %>% group_by(chart) %>% summarize(n=length(title))

Get.mean.IAI = function(v){
  x = GetL3Durations(v) %>% strsplit(.,"") %>% unlist %>% as.integer %>% mean()
  x
  
}
titles = corpora %>% filter(corpus=="genre1") %>% .$verse %>% unique
x = laply(titles,Get.mean.IAI,.progress="text")
x[is.nan(x)] = 0 # purely duple songs will have an entropy of NaN; convert to 0.
names(x) = titles
charts %>% mutate(IAI.ent = x[charts$titleCode]) %>% 
  group_by(peak.position==1) %>% 
  summarize(m = mean(IAI.ent))

load("DerivedData/CMU_contours.rdata")
GetMeanSylLength = function(v){
  w = corpora %>% filter(verse==v) %>% .$word %>% unique
  m = CMU_contours$word %>% tolower %>% is_in(w) %>% which
  mean(CMU_contours$n.syl[m])
}
m.syl = laply(titles,GetMeanSylLength)
charts = charts %>% mutate(syls = m.syl[charts$titleCode])
charts %>% filter(chart=="Hot 100") %>% .$syls -> a
charts %>% filter(chart=="") %>% .$syls -> b
t.test(a,b)
