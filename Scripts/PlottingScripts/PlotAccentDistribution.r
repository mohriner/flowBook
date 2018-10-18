PlotAccentDistribution = function(v,m.range=NA){
  dat = corpora %>% filter(verse==v) %>% 
    mutate(meas = beatIndex %/% 4)
  if(!is.na(m.range[1])){
    dat = dat %>% filter(meas %in% m.range)
  }
  BIs = dat %>% filter(accent==1) %>% 
    .[["beatIndex"]] %>% mod(4)
  tab = table(c(BIs,seq(0,3.75,.25)))-1
  names(tab) = 0:15
  n.meas = dat$meas %>% unique %>% length
  tab = round(tab/n.meas,digits=2)
  quartz(width=4.5,height=1.5)
  par(mar=c(3.5,3.5,1,1),mgp = c(2.5,1,0),cex = .65,las=1,bty='n',family="Times New Roman")
  barplot(tab,col = rep(c("black",rep("gray",3)),4),cex.names=.5,ylim=c(0,1),
          xlab = "metric position",ylab="% of measure accenting position")
}