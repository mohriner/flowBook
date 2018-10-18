# Takes a prime form and a rotation, returns series of 0s and 1s
RotateGroovePF = function(pf,rot,expand = F){
  o = pf %>% strsplit("") %>% .[[1]] %>% as.numeric %>% cumsum
  a = rep(0,last(o))
  a[o] = 1
  a = c(last(a),a[1:(length(a)-1)])
  if(rot==0){
    if(expand==T){
      a = rep(a,16/length(a))
    }
    return(a)
  }
  a = c(a[((length(a)-rot)+1):length(a)],a[1:(length(a)-rot)])
  if(expand==T){
    a = rep(a,16/length(a))
  }
  a
}

# This function takes two groove classes (as character strings), as well as
# their rotation, and returns the dynamic time warping of their onsets.
GrooveSwapDistance = function(seg.pat1,pf.start1,seg.pat2,pf.start2){
  if(seg.pat1=="22"){seg.pat1="22222222"}
  if(seg.pat2=="22"){seg.pat2="22222222"}
  if(seg.pat1=="332"){seg.pat1="332332"}
  if(seg.pat2=="332"){seg.pat2="332332"}
  a = RotateGroovePF(seg.pat1,pf.start1) 
  a = which(a==1)-1
  b = RotateGroovePF(seg.pat2,pf.start2) 
  b = which(b==1)-1
  dtw(x = a,y = b)$distance
}

# Applies the above function to a verse
GrooveSwapDistanceVerse = function(v,r.limit = .125){
  x = segments %>% filter(verse==v,effort.rate==r.limit) %>% select(class,rotation)
  disjuncture = vector()
  if(dim(x)[1]<2){
    return(data.frame(dtw=NA,endpoint=NA))
  }else{
    for(h in 1:(dim(x)[1]-1)){
      seg.pat1 = x$class[h] %>% as.character
      seg.pat2 = x$class[h+1] %>% as.character
      pf.start1 = x$rotation[h]
      pf.start2 = x$rotation[h+1]
      disjuncture = c(disjuncture,
                      GrooveSwapDistance(seg.pat1,pf.start1,seg.pat2,pf.start2))
    }
    return(
      data.frame(dtw = disjuncture,
                 endpoint = segments %>% filter(verse==v,effort.rate==r.limit) %>% 
                   .$end %>% .[1:(length(.)-1)])
    )
  }
}
