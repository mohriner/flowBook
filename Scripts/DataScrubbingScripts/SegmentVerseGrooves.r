# At different limits of cost, prune instances such that one instance per 
# timepoint remains. This step creates segments, in which observations are groove 
# segments, Note that this implies three segmentations of each verse are stored. 
# Variables are: segment start, segment end, segment length, pattern length,
# segment pattern, pf. start, and effort rate (0, .0625, .125, .25), verse, corpus.
grooves = fread("DerivedData/corpus_grooves.txt",header=T)
SegmentVerseGrooves = function(v){
  # Get max beat index
  m = (corpora %>% filter(verse==v) %>% select(beatIndex) %>% unlist %>%
         max(na.rm=T) %>% unname)*4
  
  # The line below sets the various rates of effort. These correspond to no swaps,
  # one per measure, one per half measure, and one per beat.
  effort.rates = c(0,.0625,.125,.25)
  s1 = llply(effort.rates,function(lim){
    # Filter grooves by verse and rate limit
    g1 = grooves %>% filter(verse==v,rate<=lim,end<=m)
    
    # Select 1 groove per position
    g = llply(0:max(g1$end),function(j){
      g2 = g1 %>% filter(start<=j,end>=j) %>% 
        filter(adj.length == max(adj.length)) %>% 
        arrange(desc(adj.length,start)) %>% 
        slice(1) %>% 
        select(class,rotation,start)
    },.inform=T)
    
    missing = laply(g,function(i) dim(i)[1])==0
    if(any(missing==T)){
      for(i in which(missing)){
        g[[i]] = data.frame(class=NA,rotation=NA,start=NA)
      }
    }
    g = melt(g,id.vars=1:3);g[,4] = g[,4]-1;names(g)[4] = "beat"
    
    # Segment
    pat.rotation = paste(g$class,g$rotation,sep="_")
    seg = data.frame(
      start = rle(pat.rotation)$lengths %>% cumsum %>% c(0,.) %>% .[1:(length(.)-1)],
      class = rle(pat.rotation)$values
    )
    seg$class = laply(seg$class,function(i){ i %>% as.character %>% 
                strsplit(.,"_") %>% unlist %>% .[[1]]
                },.inform=T)
    seg = mutate(seg,
                 end = (seg$start %>% c(.,max(g$beat)+1) %>% .[2:length(.)])-1
    )
    seg = mutate(seg,p = g$rotation[match(seg$start,g$beat)]);
    names(seg)[4] = "rotation"
    seg = mutate(seg, length = diff(c(seg$start, max(g$beat)+1)))
    seg = seg %>% filter(class!="NA")
    seg = seg[,c("class","rotation","start","end","length")]
    seg
  },.inform=T)
  s1 = melt(s1,id.vars = 1:5)
  s1$L1 = effort.rates[s1$L1]
  names(s1)[6] = "effort.rate"
  s1 = mutate(s1,verse = rep(v,dim(s1)[1])) 
  c1 = grooves %>% filter(verse==v) %>% slice(1) %>% .[["corpus"]] %>% rep(.,dim(s1)[1])
  s1 = s1 %>% mutate(corpus = c1)
  s1
}
