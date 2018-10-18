# Converts the beat indexes of syllables (or, by subsetting a before running the
# function, indexes of accented syllables) to a vector of 0s and 1s that represents
# whether each sixteenth-note position has an onset
IndexesToBinary = function(a)
{
  a2 = a[a>=0] # discard pick-ups
  a2 = round(a2*4) + 1
  l = ceiling(last(a2)/16) # fill out the last measure
  vec = rep(0,16*l)
  vec[a2] = 1
  vec
}

# Takes a verse name and returns three equal-length sequences indicating whether 
# metric positions, beginning at zero, have accents, non-accents, or silences.
GetRhythmVectors = function(v,m.range)
{
  if(is.na(m.range[1])){
    max.m = corpora %>% filter(verse==v) %>% .$beatIndex %>% max(na.rm=T) %>% divide_by_int(4)
    m.range = 0:max.m
  }
  a = corpora %>% filter(verse==v,accent==1,beatIndex %/% 4 %in% m.range) %>% 
    distinct(beatIndex) %>% 
    .$beatIndex
  a = IndexesToBinary(a)
  n = corpora %>% filter(verse==v,accent==0,beatIndex %/% 4 %in% m.range) %>% 
    distinct(beatIndex) %>% 
    .$beatIndex # beat index of non-accents
  if(length(n)==0){
    n = rep(0,length(a))
  }else{
    n = IndexesToBinary(n)
  }
  # beat index of silences:
  s = corpora %>% filter(verse==v,beatIndex %/% 4 %in% m.range) %>% distinct(beatIndex) %>% .$beatIndex
  s = IndexesToBinary(s)
  s = (-s)+1
  
  # Truncate to the last position carrying a 1 in a (accented) or n (nonaccented)
  lim = max(last(which(a==1)),last(which(n==1)),na.rm=T)
  a = a[1:lim]
  n = n[1:lim]
  s = s[1:lim]
  
  V = list(a = a,n = n,s = s)
  if(!all(laply(V,length)==max(laply(V,length))))
  {
    l = max(laply(V,length))
    V = llply(V,function(i){
      if(length(i)!=l){
        i[(length(i)+1):l] = 0
      }
      i
    })
  }
  return(V)
}

# Takes a verse and gets the limit-3 vector, whereby accents are events, the 
# positions surrounding accents are not, and silent eight-note positions not
# following accents are accents
GetL3RhythmVector = function(v,m.range)
{
  r = GetRhythmVectors(v,m.range)
  l3 = rep(NA,length(r$a))
  
  # Make positions with accents 1; those on either side 0; be sure not to add
  # an extra zero if the last element is an accent.
  l3[r$a==1] = 1;l3[c(which(r$a==1)+1,which(r$a==1)-1)] = 0
  l3 = l3[1:length(r$a)]
  
  # If first-position in the verse is zero, it should be 1. This creates an ad-
  # jacent accent, but otherwise the verse isn't zero-oriented correctly
  if(!is.na(l3[1])){if(l3[1] == 0){l3[1] = 1}}
  
  # Add NAs to fill out the final beat.
  n.beats = ceiling(length(l3)/4)
  
  if(length(l3)<(n.beats*4)){
    l3 = c(l3,rep(NA,(n.beats*4)-length(l3)))
  }
  # Make a matrix by beat
  m = matrix(l3,n.beats,4,byrow=T)
  
  # Remove all NA rows (these are those unwanted in m.range)
  m = m[apply(m,1,is.na) %>% t %>% rowSums %>% equals(4) %>% not,]
  
  # If first- or third position in a beat is NA, it should be 1 (because it 
  # isn't adjoining an accent and duple values are preferred by the beat).
  m[which(is.na(m[,1])),1] = 1
  m[which(is.na(m[,3])),3] = 1
  
  # z = apply(m,1,paste,collapse="-")
  # unique(z[grepl("NA",z)])
  
  # Other NAs are 0
  l3 = as.vector(t(m))
  l3[is.na(l3)] = 0
  l3
}
# Takes a verse and gets a limit-3 sequence of durations (all 2s and 3s)
GetL3Durations = function(v,m.range = NA)
{
  l3 = GetL3RhythmVector(v,m.range)
  l3d = paste(diff(which(l3==1)),collapse="")
  l3d = gsub("4","22",l3d) # off-beat accents (say, from bc 1,5,9) will
  # return 4s; these should be split into 2s.
  l3d
}