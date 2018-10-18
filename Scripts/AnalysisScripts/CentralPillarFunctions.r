getCircularDensityPeak = function(i){
  i %>% divide_by(16) %>% # get percentage of measure
    multiply_by(2*pi) %>% # convert to radians
    circular(.,type="angles",units="radians") %>% # convert to circular object type
    density.circular(.,kernel="vonmises",bw=25) %>% # get density
    .$y %>% # isolate y-axis values
    which.max %>% # which position is the peak?
    divide_by(.,512) %>% # because the density plot has 512 values
    multiply_by(16) # back to 0:15 position
}

getVersePillar = function(title,
                          location = c("beginning","ending"),
                          method = c("line","phrase")){
  c2 = corpora %>% 
    filter(verse==title)
  if(method=="line"){
    c2 = c2 %>% group_by(line) 
  }else{
    c2 = c2 %>% group_by(phrase)
  }
  if(location=="beginning"){
    c2 = c2 %>% slice(1)
  }else{
    c2 = c2 %>% slice(n())
  }
  positions = c2 %>% 
    mutate(position = round((beatIndex%%4)*4)) %>% 
    .$position
  getCircularDensityPeak(positions) %>% round(.,digits=2)
}

positions2CircularDensity = function(i){
  i %>% divide_by(16) %>% 
    multiply_by(2*pi) %>% 
    circular(.,type="angles",units="radians",zero = pi/2,rotation = "clock") %>% 
    density.circular(.,kernel="vonmises",bw=25)
}