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

GetGrooveComplexity = function(onsets){
  weights = scaledNegLog
  a = rep(0,16);a[onsets+1] = 1
  m = apply(matrix(a,4,4,byrow=T),1,function(i) paste(i, collapse=""))
  round(sum(weights[m]),digits=2)
}
remove(list=c("negLog","v","counts","proportion","m","titles"))