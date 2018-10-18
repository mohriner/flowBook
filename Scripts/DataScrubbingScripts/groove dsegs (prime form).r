# Get possible patterns:
number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - noBits))]
  }
}
r = llply(6:7,function(n)
{
  x = laply(0:((2^n)-1),number2binary,noBits=n) + 2
  x = x[rowSums(x)==16,]
  x
}
)
strings.nr = unlist(llply(r,function(i){
  apply(i,1,function(j){
    paste(j,collapse="")
  })
}))
groove.dsegs = c("22222222",strings.nr)
rotate.string = function(s){
  s2 = paste(rep(s,2),collapse="")
  n = nchar(s)
  s3 = laply(1:n,function(i){
    substr(s2,i,i+(n-1)) 
  })
  s3
}
u = laply(groove.dsegs,function(i){
  strings2 = groove.dsegs[(which(groove.dsegs==i)+1):length(groove.dsegs)]
  any(is.element(rotate.string(i),strings2))
})
groove.dsegs.pf = c(groove.dsegs[!u],groove.dsegs[length(groove.dsegs)])
remove(list=c("number2binary","r","u","strings.nr"))
