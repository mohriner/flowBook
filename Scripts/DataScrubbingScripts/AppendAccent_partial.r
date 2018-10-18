# This script is identical to AppendAccent.r, but some of it is commented out in
# order to make one of the examples in Chapter 3.
AppendAccent = function(dat){
  #dat = corpora %>% filter(verse=="underPressure")
  beat.mod = .5 # The level called "on-beat"
  l.thresh = .34 # the defined "long syllable"
  n.syls = dim(dat)[1]
  accent = rep(0,n.syls)
  
  # Determine if syllable is on-beat, a word accent, a monosyllable, or longer
  # than the threshold. Add beat index also.
  dat = dat %>% 
    mutate(b = beatIndex %% beat.mod == 0, # on beat
           w = CMU_accent == 1, # word accent
           n = CMU_accent == 0, # word non-accent
           m = is.na(CMU_accent), # monosyllable
           l = ((4/quant)*duration) > l.thresh) # long syllable
  dat$w[is.na(dat$w)] = F # word accent of all monosyllables is FALSE
  
  # Assign accent to syllables that are 
  dat = dat %>% 
    mutate(accent = (b==T & # on-beat and
                       (w==T| # word accent, or
                          m==T| # monosyllable, or
                          n==T & l==T)) | # word non-accent and long syllable, 
             # ...OR...
             (b==F & # off-beat and
                (w==T | # word accent, or
                   l==T)))# long syllable
  
  # # Clear accents near word accents or off-beat, long monosyllables
  # w.bi = dat %>% filter(w==T|(b==F&m==T&l==T)) %>% .[["beatIndex"]]
  # near.w.bi = llply(w.bi,function(i){
  #   dat$beatIndex[abs(dat$beatIndex - i) <= 0.25]
  # }) %>% unlist %>% sort %>% unique
  # near.w.bi = near.w.bi[!(near.w.bi %in% w.bi)]
  # dat$accent[dat$beatIndex %in% near.w.bi] = F
  # 
  # # Add accents in the middle of 5-monosyllable spans currently accented <10001>
  # addOn = laply(1:(dim(dat)[1]-4),function(j){
  #   s1 = which(dat$beatIndex==dat$beatIndex[j])
  #   c1 = all((dat$beatIndex[s1:(s1+4)] - dat$beatIndex[j])==c(0,.25,.5,.75,1)) # Condition #1: all on adjacent positions
  #   c2 = all(dat$accent[s1:(s1+4)]==c(T,F,F,F,T)) # Condition 2: only first and last accented
  #   all(c1,c2)
  # },.inform=T) %>% which
  # dat$accent[addOn+2] = TRUE
  
  # Remove the columns used in evaluation
  dat = dat[,-(match(c("b","w","m","n","l"),names(dat)))]
  return(dat)
}
