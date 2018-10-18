
f = 2
t = seq(0,2*pi*f,length.out = 1000)
quartz(width=4.55,height=1)
par(mfcol=c(2,4),mar = c(1,1,1,1))
par(cex = .65,las=1,bty='n',
    family="Times New Roman")

# 1. f = 2, no phase
plot(t,sin(t),type = "l",xaxt="n",yaxt="n",ylab="",xlab="",bty="n")
zero = which(round(sin(t),digits=1)==0)
zero = c(zero[diff(zero)!=1],length(t))
points(t[zero],rep(0,length(zero)),pch=19)
title(main="(a)",font.main=3,adj = 0)

# 2. same
plot(t,sin(t),type = "l",xaxt="n",yaxt="n",ylab="",xlab="",bty="n")
zero = which(round(sin(t),digits=1)==0)
zero = c(zero[diff(zero)!=1],length(t))
points(t[zero],rep(0,length(zero)),pch=19)

# 3. same
plot(t,sin(t),type = "l",xaxt="n",yaxt="n",ylab="",xlab="",bty="n")
zero = which(round(sin(t),digits=1)==0)
zero = c(zero[diff(zero)!=1],length(t))
points(t[zero],rep(0,length(zero)),pch=19)
title(main="(b)",font.main=3,adj = 0)

# 4. phase adjustment
t1 = seq(0,2*pi*f,length.out = 1000)-1
plot(t,sin(t1),type = "l",xaxt="n",yaxt="n",ylab="",xlab="",bty="n")
zero = which(round(sin(t1),digits=1)==0)
zero = c(zero[diff(zero)!=1],829)
points(t[zero],rep(0,length(zero)),pch=19)

# 5. same
plot(t,sin(t),type = "l",xaxt="n",yaxt="n",ylab="",xlab="",bty="n")
zero = which(round(sin(t),digits=1)==0)
zero = c(zero[diff(zero)!=1],length(t))
points(t[zero],rep(0,length(zero)),pch=19)
title(main="(c)",font.main=3,adj = 0)

# 6. frequency adjustment
t2 = seq(0,2*pi*(f+.5),length.out = 1000)
plot(t,sin(t2),type = "l",xaxt="n",yaxt="n",ylab="",xlab="",bty="n")
zero = which(round(sin(t2),digits=1)==0)
zero = c(zero[diff(zero)!=1],length(t))
points(t[zero],rep(0,length(zero)),pch=19)


# 7. same
plot(t,sin(t),type = "l",xaxt="n",yaxt="n",ylab="",xlab="",bty="n")
zero = which(round(sin(t),digits=1)==0)
zero = c(zero[diff(zero)!=1],length(t))
points(t[zero],rep(0,length(zero)),pch=19)
title(main="(d)",font.main=3,adj = 0)

# 8. frequence+phase
t3 = seq(0,2*pi*(f+.5),length.out = 1000)-1
plot(t,sin(t3),type = "l",xaxt="n",yaxt="n",ylab="",xlab="",bty="n")
zero = which(round(sin(t3),digits=1)==0)
zero = c(zero[diff(zero)!=1],864)
points(t[zero],rep(0,length(zero)),pch=19)
remove(list=c("f","t","t1","t2","t3","zero"))
quartz.save(file="Examples/Chapter 8/Example 8.5.pdf",type="pdf");dev.off()