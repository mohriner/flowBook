# This function takes a corrected textgrid and returns the onsets of syllables
# in beats, continuous(i.e., not quantized).
time_dat = read.delim("SourceData/verseAcapellaMetadata.txt")
TextGrid2ContinuousOnset = function(TG,artist,verse){
  a = artist
  v = verse
  s1 = grep("item \\[1\\]",TG)
  s2 = grep("item \\[2\\]",TG)
  
  # Lines with bracketed numbers
  ints = grep("\\[[[:digit:]]",TG)
  l1 = ints[which(ints>s1&ints<s2)]
  l2 = ints[which(ints>s2)]
  on1 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l1+1]))
  off1 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l1+2]))
  lab1 = gsub("[(text\\=[:space:]\")]","",TG[l1+3])
  on2 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l2+1]))
  off2 = as.numeric(gsub("[^([:digit:]\\.)]","",TG[l2+2]))
  lab2 = gsub("[(text\\=[:space:]\")]","",TG[l2+3])
  
  ph.matrix = data.frame(on1,off1,lab1)
  w.matrix = data.frame(on2,off2,lab2)
  # Abbreviate the two matrices
  ph.matrix = ph.matrix[-which(ph.matrix$lab1=="sp"),]
  w.matrix = w.matrix[-which(w.matrix$lab2=="sp"),]
  if(any(w.matrix$lab2=="")){w.matrix = w.matrix[-which(w.matrix$lab2==""),]}
  
  # Get continuous onset
  onset = vector()
  words = vector()
  for(j in 1:length(w.matrix$lab2)){	# for each word
    lab = w.matrix$lab2[j] # text of word
    
    # Determine phone boundaries
    sW = as.vector(unlist(w.matrix[j,1]))	# word start (s)
    eW = as.vector(unlist(w.matrix[j,2]))	# word end (s)
    sP = which.min(abs(ph.matrix[,1]-sW))	# phone start (index)
    eP = which.min(abs(ph.matrix[,2]-eW))	# phone end (index)
    if(j==length(w.matrix$lab2)){eP = length(ph.matrix[,1])}
    
    phones = as.vector(unlist(ph.matrix[sP:eP,"lab1"]))
    v1 = which(is.element(phones,vowels))	# index of phones that are vowels
    onset = c(onset,ph.matrix$on1[(sP:eP)[v1]])
    words = c(words,rep(as.vector(unlist(w.matrix$lab2[j])),length(v1)))
  }
  # Mod to measure, accounting for drift. To do this, you need to change the 
  # duration of the measure by the percentage discovered in measuring drift.
  i = match(verse,time_dat$title)
  dbA = time_dat$dbA[i]
  meas = 240/time_dat$tempo[i]
  
  d1 = time_dat$diff1[i]
  d2 = time_dat$diff2[i]
  m1 = time_dat$mixRef1[i]
  m2 = time_dat$mixRef2[i]
  a1 = time_dat$accRef1[i]
  a2 = time_dat$accRef2[i]
  drift = (d1-d2)/(a2 - a1)
  if(a1 == 0){drift = 0}
  dbA = dbA - ((time_dat$dbAmeas[i])*meas)
  
  # The next line is the important one for getting rid of drift
  meas = meas+(meas*drift) # Adjust measure length for drift
  onset1 = onset + (m1-a1) - dbA
  c.meas = floor(onset1/meas)
  c.beat = ((onset1%%meas)/meas)*4
  beatIndex.c = c.meas * 4 + c.beat
  data.frame(word=words,beatIndex.c = beatIndex.c)
}