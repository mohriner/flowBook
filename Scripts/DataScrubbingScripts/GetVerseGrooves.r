source("Scripts/DataScrubbingScripts/groove dsegs (prime form).r")

if(!exists("lim")){lim = .25}
# Functions for finding prime form and displacement
rotate.string = function(s){
  s2 = paste(rep(s,2),collapse="")
  n = nchar(s)
  s3 = laply(1:n,function(i){
    substr(s2,i,i+(n-1)) 
  })
  s3
}
GetPForm = function(pat){
  if(pat=="none"){return("none")}
  m = match(groove.dsegs.pf,rotate.string(pat))
  pat.nr = groove.dsegs.pf[which(!is.na(m))]
  pat.nr
}
GetPFormDisplacement = function(pat,start){
  if(pat=="none"){return(0)}
  rotations = rotate.string(pat)
  m = match(groove.dsegs.pf,rotations)
  pat.nr = groove.dsegs.pf[which(!is.na(m))]
  s = unlist(gregexpr(pat.nr,paste(pat,pat,sep="")))
  disp = substr(pat,1,s-1) %>% strsplit(.,"") %>% .[[1]] %>% as.numeric %>% sum
  return(start+disp)
}

string2intVector = function(a){
  a %>% strsplit("") %>% unlist %>% as.numeric %>% cumsum %>% c(0,.) 
}
# The main function:
Get.G.dseg.instances = function(l3d.vec,pattern){
  x = l3d.vec
  pat = string2intVector(pattern)
  pat.n = last(pat)
  n.reps = ceiling(64/pat.n)
  # Get plausible repetitions
  efforts = llply(1:n.reps,function(n){
    pat.repeated = string2intVector(paste(rep(pattern,n),collapse=""))
    pat.n.repeated = last(pat.repeated)
    dur.exists = laply(x,function(i) (i+pat.n.repeated) %in% x)
    if(!any(dur.exists)){
      return(data.frame(
        effort=vector(),
        start = vector(),
        end = vector(),
        rate=vector(),
        reps = vector(),
        length = vector(),
        adj.length = vector(),
        groove = vector(),
        class = vector(),
        rotation = vector()
      ))
    }
    effort = llply(x[dur.exists],function(i){
      x1 = (x[which(x==i):which(x==(i+pat.n.repeated))]) # the position of
      # accents within the span that could be the groove.
      x2 = x1 - i
      d = (dtw(pat.repeated,x2,distance.only=T)$distance)
      df = data.frame(effort = d,start = min(x1),end = max(x1))
      df = df %>% mutate(rate = round(effort/(end-start),digits=2)) 
    },.inform=T)
    n.cols = dim(effort[[1]])[2]
    m = melt(effort,id.vars = 1:n.cols)[,1:n.cols]
    m = m %>% filter(rate<=lim)
  },.inform=T)
  n.cols = dim(efforts[[1]])[2]
  # Check that the groove pattern happened at least once
  if(!any(laply(efforts,dim)[,1]>0)){
    efforts = efforts[[1]]
    efforts = mutate(efforts,
                     reps = vector(),
                     length = vector(),
                     groove = vector(),
                     adj.length = vector(),
                     class = vector(),
                     rotation = vector())
    return(efforts)
  }
  efforts = melt(efforts,id.vars = 1:n.cols);
  colnames(efforts)[dim(efforts)[2]] = "reps"
  efforts = efforts %>% mutate(length = pat.n*reps,
                               groove = rep(pattern,dim(efforts)[1]))
  efforts = efforts %>% 
    mutate(adj.length= round(length*(1-(rate^.25)),digits=2))
  efforts = efforts %>% 
    mutate(class = rep(GetPForm(pattern),dim(efforts)[1]))
  efforts = efforts %>% 
    mutate(rotation = laply(efforts$start,function(i) {
      GetPFormDisplacement(pattern,i)
      },.inform=T)%%16)
  if(is.element("variable",names(efforts))){
    efforts = efforts[,-which(names(efforts)=="variable")]
    }
  if(is.element("value",names(efforts))){
    efforts = efforts[,-which(names(efforts)=="value")]
    }
  efforts
}

GetVerseGrooves = function(v,lim = 1){
  # Load prime forms
  source("Scripts/DataScrubbingScripts/GetL3Durations.R")
  
  l3d = GetL3Durations(v)
  l3d.vec = string2intVector(l3d)
  l3d.l = last(l3d.vec)
  
  # For each string...
  grooves.lists = llply(groove.dsegs,function(pattern){
    Get.G.dseg.instances(l3d.vec,pattern)
  },.progress="text",.inform=T)
  n.cols = dim(grooves.lists[[1]])[2]
  grooves = melt(grooves.lists,id.vars = 1:n.cols)[,1:(n.cols)]
  
  if(dim(grooves)[1]>0){
    # Add verse and corpus
    grooves = grooves %>% mutate(verse = rep(v,dim(grooves)[1]))
    c1 = corpora %>% filter(verse==v) %>% slice(1) %>% .[["corpus"]] %>% 
      rep(.,dim(grooves)[1])
    grooves = grooves %>% mutate(corpus = c1)
    return(grooves)
  }else{
    return(data.frame(
      effort=vector(),
      start = vector(),
      end = vector(),
      rate=vector(),
      reps = vector(),
      length = vector(),
      groove = vector(),
      adj.length = vector(),
      class = vector(),
      rotation = vector(),
      verse = vector(),
      corpus = vector()
    ))
  }
}

CorrectRotationalEquivalence = function(grooves){
  grooves$rotation[which(grooves$class=="22222222")] =
    grooves$rotation[which(grooves$class=="22222222")]  %% 2
  grooves$rotation[which(grooves$class=="332332")] =
    grooves$rotation[which(grooves$class=="332332")]  %% 8
  grooves
}