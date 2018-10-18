GetGrooviness = function(v,lim = 32,e = 0){
  g = segments %>% 
    tbl_df %>% 
    filter(verse==v,effort.rate == e) %>% 
    mutate(long = length>=lim) %>% 
    group_by(verse,long) %>% 
    summarize(l = sum(length))
  if(dim(g)[1]==0){return(NA)}
  g = g %>% 
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
GetGrooveAdherence = function(v,lim=32,omit = ""){
  g1 = segments %>% 
    filter(verse==v,effort.rate == .125,!class %in% omit) %>% 
    mutate(long = length>=32) %>% 
    filter(long==T) %>% 
    select(-effort.rate,-corpus,-long)
  if(dim(g1)[1]==0){return(0)}
  adhere = laply(1:dim(g1)[1],function(i){
    matches = segments %>% 
      filter(verse == v,
             class == g1$class[i],
             (start <= g1$start[i] & start >= g1$end[i])|(end >= g1$start[i] & end <= g1$end[i]),
             effort.rate == 0)
    if(any(matches$start<g1$start[i])){
      matches$start[matches$start<g1$start[i]] = g1$start[i]
    }
    if(any(matches$end>g1$end[i])){
      matches$end[matches$end>g1$end[i]] = g1$end[i]
    }
    sum(matches$end-matches$start)/g1$length[i]
    
  },.inform=T) %>% mean %>% round(.,digits=2)
  adhere
}