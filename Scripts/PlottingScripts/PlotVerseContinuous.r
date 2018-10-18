### The main plotting function ###
plotVerseContinuous = function(
	verse,
	tight = T, 
	save.plot = T,
	plot.beat.type=T,
	plot.accent = T,
	plot.rhymeClasses = F,
	plot.rhymeFeatures=T,
	black_and_white=T,
	m.range = NA,
	row.index=NA,
	closeScreen = F,
	Width=4.5,
	Meas.height=.75)
	{
	###---Initial data processing---###
	v = verse
  dat = corpora %>% filter(verse==v)

	read.delim("SourceData/verseMetadata.txt") %>% 
	  filter(title==v) %>% 
	  .$bpm
	tempo = tempo[which(tempo[,"piece"]==strsplit(piece.path,"_")[[1]][2]),4]
	
	# Remove mid-beat accents
	dat = removeMidBeatAccents(dat)
	
	# Filter by row if there's a row.index argument
	if(!is.na(row.index[1])){
	  dat = dat[row.index,]
	}
	
	# Remove measures not in m.range if m.range is specified
	if(!is.na(m.range[1])){
		dat = dat[which(is.element(dat[,"c.meas"],m.range)),]
	}
	
	n.meas = diff(range(dat$c.meas[!is.na(dat$c.meas)]))+1

	
	meas.mult = .5
	if(is.element("r1.index",colnames(dat))){meas.mult = .65}
	
	if(plot.beat.type==T&tight==F){
		quartz(width = 6,height = n.meas*Meas.height)
	}else{
		quartz(width = Width,height = n.meas*Meas.height)
	}
	par(mar = c(0,0,0,0))
	
	if(plot.beat.type==T){par(fig=c(0,.65,0,1))}
	if(tight==T){dat[,"syllable"] = gsub("_","",dat[,"syllable"])}
	meas = 4 # should either be number of beats or duration in seconds, depending on data
	#meas = 3.09
	plot(0,0,col="white",xlim=c(-.15,meas),
		ylim=rev(range(dat$c.meas[!is.na(dat$c.meas)])+c(-.5,.5)),yaxp=c(n.meas,0,n.meas),
		xaxt="n",bty="n",las=1,mgp=c(1,0,0),tcl=.01,family="Times New Roman",font=3,
		yaxt="n")
	llply(max(dat$c.meas[!is.na(dat$c.meas)]):min(dat$c.meas[!is.na(dat$c.meas)]),function(x) lines(c(0,4),rep(x,2),lwd=.5))	
	
	###---Layer 1: the meter grid
	# Draw the meter: not a graph-paper-like grid
	# get points
	n.meas = length(seq(min(dat$c.meas[!is.na(dat$c.meas)]),max(dat$c.meas[!is.na(dat$c.meas)])))
	xs = rep(seq(0,4,length.out=17),each=2)
	ys = rep(0,34)
	ys[seq(1,33,2)] = c(rep(c(-.25,-.12,-.18,-.12),4),-.25)
	xs2 = rep(NA,17*3)
	xs2[seq(1,16*3+1,by=3)] = xs[seq(1,16*2+1,by=2)]
	xs2[seq(2,16*3+2,by=3)] = xs[seq(2,16*2+2,by=2)]
	ys2 = rep(NA,17*3)
	ys2[seq(1,16*3+1,by=3)] = ys[seq(1,16*2+1,by=2)]
	ys2[seq(2,16*3+2,by=3)] = ys[seq(2,16*2+2,by=2)]
	xs3 = rep(xs2,n.meas)
	ys3 = ys2+rep(seq(min(dat$c.meas,na.rm=T),max(dat$c.meas,na.rm=T)),each=length(ys2))
	line_coords = list(x=xs3,y=ys3)
	# The little dots at the top
	xs = seq(0,4,length.out=17)
	ys= c(rep(c(-.25,-.12,-.18,-.12),4),-.25)
	ys2 = rep(seq(min(dat$c.meas,na.rm=T),max(dat$c.meas,na.rm=T)),each=length(ys)) + rep(ys,length(seq(min(dat$c.meas,na.rm=T),max(dat$c.meas,na.rm=T))))
	xs2 = rep(xs,n.meas)
	point_coords = list(x = xs2,y=ys2)
	lines(line_coords,lwd=.5)
	points(point_coords,pch=20,cex=.25)
	points(as.numeric(dat$c.beat),as.numeric(dat$c.meas),pch=20,cex=.75)
	
	###---Draw the points
	# Point size, determined by accent, cex= .5 or 1.25
	p.cex = rep(.5,length(dat[,1]));
	if(is.element("accent",colnames(dat))){p.cex[which(dat[,"accent"]==1)] = 1.25}

	overridden = rep("black",length(dat[,1]))
	# Heed the accent override
	if(is.element("accentOverRide",colnames(dat))){
		a1 = which(dat[,"accentOverRide"]==1)
		if(length(a1)>0){p.cex[a1] = 1.25;overridden[a1] = "red"}
		a0 = which(dat[,"accentOverRide"]==0)
		if(length(a0)>0){p.cex[a0] = .5;overridden[a0] = "red"}
	}
	
	if(plot.accent==FALSE){p.cex = rep(.5,length(dat[,1]))}
	if(black_and_white==TRUE){overridden = rep("black",length(dat[,1]))}

	points(dat[,"c.beat"],dat[,"c.meas"],cex=p.cex,pch=21,bg="white",col = overridden)
		
	###---Layer 2: the syllables
	# Plot syllable text
	# Put commas on syls with lexical endings
	syls.to.plot =as.character(dat[,"syllable"])
	if(is.element("breathEnding",colnames(dat))){
		lineEndings = which(dat[,"breathEnding"]==1)
	}else{
		lineEndings = which(dat[,"lineEnding"]==1)
	}
	syls.to.plot[lineEndings]  = paste(syls.to.plot[lineEndings],",",sep="")
	text(dat[,"c.beat"],dat[,"c.meas"]-.05,syls.to.plot,pos=1,cex=.75,font=3,family="Times New Roman")

	###---Layer 3, option 1: rhyme classes or features
	# draw the lines first so the numbers are plotted over them
	if(plot.rhymeClasses==TRUE){
		if(is.element("r1.index",colnames(dat))){
			n.rhymes = max(dat[,"r1.index"],na.rm=T)
			for(i in 1:n.rhymes){
				r = which(dat[,"r1.index"]==i)
				
				if(length(r)>0){
					l = max(dat[r,"r1.syl"]) # the limit, n of syls in the rhyme
					r.syls = dat[r,"r1.syl"]
					t = rep("c",length(r))
					
					# First syllable is beginning
					t[1] = "b"
					
					# Last syllable is either solo or ending
					t[length(t)] = "e"; if(r.syls[length(r.syls)]==1){t[length(t)] = "s"}
					
					# Figure out the rest
					for(j in 1:(length(r)-1)){
						if(all(r.syls[j+1]==1,r.syls[j] ==1 )){t[j] = "s"}else{
							if(sign(r.syls[j+1] - r.syls[j])==-1){t[j] = "e"}
						}
					}
					
					if(length(r)>2){
						for(j in 2:(length(r)-1)){
							if(t[j] == "e"){t[j+1] = "b"}
						}
					}
					
					# Draw some lines
					if(is.element("b",t)){
						for(j in which(t=="b")){
							start1 = r[j] # index of first syllable of rhyme
							end1 = r[which(t=="e")[which(t=="e")>j][1]] # index of last syllable of rhyme
							if(is.na(end1)){end1 = start1}
							if(dat[start1,"c.meas"]==dat[end1,"c.meas"]){
								# The rhyme is contained within a measure
								lines(dat[c(start1,end1),"c.beat"],dat[c(start1,end1),"c.meas"]-rep(.35,2),lwd = .5)
							}else{
								# Not the same measure
								lines(c(dat[start1,"c.beat"],4.05),rep(dat[start1,"c.meas"],2)-.35,lwd = .5)
								lines(c(dat[end1,"c.beat"],-.1),rep(dat[end1,"c.meas"],2)-.35,lwd = .5)
							}
						}
					}
				}
			}
		}
			
		# Number them
		if(is.element("r1.index",colnames(dat))){
			# Rhyme layer 1
			r.syls = which(!is.na(dat[,"r1.index"]))
			points(dat[r.syls,"c.beat"],dat[r.syls,"c.meas"]-.35,pch = 16,cex = 1.5)
			text(dat[r.syls,"c.beat"],dat[r.syls,"c.meas"]-.35,dat[r.syls,"r1.index"],col="white",cex=.5)
		
			if(is.element("r2.index",colnames(dat))){
				# Rhyme layer 2, and they can't be on top of each other
				r2_level = rep(.35,length(dat[,1]))
				r2_level[which(!is.na(dat[,"r1.index"])&!is.na(dat[,"r2.index"]))] = .5
				
				r2.syls = which(!is.na(dat[,"r2.index"]))
				
				if(length(r2.syls)>0){
					points(dat[r2.syls,"c.beat"],dat[r2.syls,"c.meas"]-r2_level[r2.syls],pch = 16,cex = 1.5)
					text(dat[r2.syls,"c.beat"],dat[r2.syls,"c.meas"]-r2_level[r2.syls],dat[r2.syls,"r2.index"],col="white",cex=.5)
				}
			}
			
		}
	}
	
	if(plot.rhymeFeatures==TRUE){
		dat = read.delim(paste("Data/verse data/",piece.path,"_rhythmFeatures.txt",sep=""),header=T)
		r.classes = dat[,"r1.index"]
		if(is.element("r2.index",colnames(dat))){
			r.classes = c(r.classes,dat[,"r2.index"])
		}
		r.classes = sort(unique(r.classes))
		
		for(j in r.classes){
			r.index = j
			rclass.m = get.rclass.matrix(piece,r.index)
			is.accented = get.rClass.accent(rclass.m,piece)
			rclass.accent = rev(which(is.accented==1))[1]
			conf = get.rolling.RhymeScore(piece,r.index)
			for(k in 1:length(rclass.m[,1])){
				x1 = dat[rclass.m[k,rclass.accent],"c.beat"]
				y1 = dat[rclass.m[k,rclass.accent],"c.meas"]-.05
				text(x1,y1+.35,conf[k],font=3,cex=.65,family="Times New Roman")
				
				if(k!=1){
					this.bi = dat[rclass.m[k,rclass.accent],"c.beat"]+(dat[rclass.m[k,rclass.accent],"c.meas"]*4)
					last.bi = dat[rclass.m[k-1,rclass.accent],"c.beat"]+(dat[rclass.m[k-1,rclass.accent],"c.meas"]*4)
					dur = this.bi - last.bi
					polygon(c(x1-.15,x1-.15,x1+.15,x1+.15),c(y1-.25,y1-.45,y1-.45,y1-.25),lwd=.5,bg="white")
					text(x1,y1-.35,dur,cex=.85,family="Times New Roman")
				}	
				
				if(k==1){
					polygon(c(x1-.15,x1-.15,x1+.15,x1+.15),c(y1-.25,y1-.45,y1-.45,y1-.25),lwd=.5,bg="white")
					text(x1,y1-.35,"*",family="Times New Roman")
				}
			}
		}
	}
	
	###---Number measures---###
	for(i in unique(dat[,"c.meas"])){
		text(-.15,i,i+1,family="Times New Roman",font=3)
	}
	
	###---Another plot: beat qualities
	if(plot.beat.type==T){
		quals = getBeatTypes(piece,conditions)
		plotBeatType(piece,quals,add=T,save.plot = FALSE,m.range)
	}
	
	if(save.plot==T){
		quartz.save(paste("Plots/Verse Transcriptions/",piece.path,".pdf",sep=""),type="pdf")
	}
	
	# Close the screen
	if(closeScreen==T){
		if(length(dev.list())>0){
			dev.off()
		}
	}
}
