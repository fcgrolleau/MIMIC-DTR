lancet_red <- "#AD002A99"
lancet_blue <- "#0099B4FF"
lancet_gray <- "#ADB6B6FF"

trans_blue <- rgb(col2rgb(lancet_blue)[1],col2rgb(lancet_blue)[2],col2rgb(lancet_blue)[3],  maxColorValue = 255, alpha = 25)
trans_red <- rgb(col2rgb(lancet_red)[1],col2rgb(lancet_red)[2],col2rgb(lancet_red)[3], maxColorValue = 255, alpha = 125)

dev.new(width=3*3, height=3, pointsize=11, noRStudioGD = TRUE)
par(mfrow=c(1,3), mar=c(3.5,3.5,1,1)+.1, mgp=c(1.75,0.5,0), tcl=-0.4)
plot(c(-30,30), c(0,20),type="n", yaxs="i", axes=F, ylab="", xlab="")
polygon(c(0,par("usr")[1],par("usr")[1],0), c(0, par("usr")[1]/qnorm(0.025),0,0), col=lancet_gray, border=NA)
polygon(c(0,par("usr")[2],par("usr")[2],0), c(0, par("usr")[2]/qnorm(0.975),0,0), col=lancet_gray, border=NA)


abline(v=0, lwd=0.75, lty=2)
arrows(-1, par("usr")[4]-.5, -28, par("usr")[4]-.5, length = 0.07, lwd=0.5, xpd=TRUE)
arrows(1, par("usr")[4]-.5, 28, par("usr")[4]-.5, length = 0.07, lwd=0.5, xpd=TRUE)

mtext(text="recommends RRT initation", side=3, line=0, at=2, cex=1/2.0, adj=0)
mtext(text="recommends no RRT initation", side=3, line=0, at=-28, cex=1/2.0, adj=0)

mtext("Standard Error", side=2, line=2.25, cex=.83)
mtext("Blip (first decision)", side=1, line=2.25, cex=.83)

lgd <- legend(x = mean(c(par("usr")[1],par("usr")[2])), y =  mean(c(par("usr")[3],par("usr")[4])), cex= .9, 
              c("Initatied RRT", "Did not initiate RRT"), pch=16, col = c("blue", "red"), plot = F)

polygon(c(par("usr")[1], -5, -5, par("usr")[1]), c(0, 0, 5*1/qnorm(.975), 5*1/qnorm(.975)), col="white", border=NA)

legend(x = par("usr")[1], y =  par("usr")[3] + lgd$rect$h-.2, cex= .9,
       c("Initatied RRT", "Did not initiate RRT"),
       pch=16, col = c(lancet_red, lancet_blue), bty = "n", plot = T)

segments(par('usr')[1], 0, par('usr')[2], 0, lwd=1)
axis(1, at=seq(-30, 30,by=10), lwd=0, lwd.tick=0.75)
axis(2, las=1, lwd=0.75, lwd.tick=0.75)

points(ite_k1[actual_ttts$a1==0], ite_se_k1[actual_ttts$a1==0], col=trans_blue, pch=16)
points(ite_k1[actual_ttts$a1==1], ite_se_k1[actual_ttts$a1==1], col=trans_red, pch=16, add=TRUE)


plot(c(-30,30), c(0,20),type="n", yaxs="i", axes=F, ylab="", xlab="")
polygon(c(0,par("usr")[1],par("usr")[1],0), c(0, par("usr")[1]/qnorm(0.025),0,0), col=lancet_gray, border=NA)
polygon(c(0,par("usr")[2],par("usr")[2],0), c(0, par("usr")[2]/qnorm(0.975),0,0), col=lancet_gray, border=NA)
segments(par('usr')[1], 0, par('usr')[2], 0, lwd=1)
axis(1, at=seq(-30, 30,by=10), lwd=0, lwd.tick=0.75)
axis(2, las=1, lwd=0.75, lwd.tick=0.75)
abline(v=0, lwd=0.75, lty=2)

arrows(-1, par("usr")[4]-.5, -28, par("usr")[4]-.5, length = 0.07, lwd=0.5, xpd=TRUE)
arrows(1, par("usr")[4]-.5, 28, par("usr")[4]-.5, length = 0.07, lwd=0.5, xpd=TRUE)

mtext(text="recommends RRT initation", side=3, line=0, at=2, cex=1/2.0, adj=0)
mtext(text="recommends no RRT initation", side=3, line=0, at=-28, cex=1/2.0, adj=0)
mtext("Blip (second decision)", side=1, line=2.25, cex=.83)

points(ite_k2[actual_ttts$a2==0], ite_se_k2[actual_ttts$a2==0], col=trans_blue, pch=16)
points(ite_k2[actual_ttts$a2==1], ite_se_k2[actual_ttts$a2==1], col=trans_red, pch=16, add=TRUE)

plot(c(-30,30), c(0,20),type="n", yaxs="i", axes=F, ylab="", xlab="")
polygon(c(0,par("usr")[1],par("usr")[1],0), c(0, par("usr")[1]/qnorm(0.025),0,0), col=lancet_gray, border=NA)
polygon(c(0,par("usr")[2],par("usr")[2],0), c(0, par("usr")[2]/qnorm(0.975),0,0), col=lancet_gray, border=NA)
segments(par('usr')[1], 0, par('usr')[2], 0, lwd=1)
axis(1, at=seq(-30, 30,by=10), lwd=0, lwd.tick=0.75)
axis(2, las=1, lwd=0.75, lwd.tick=0.75)
abline(v=0, lwd=0.75, lty=2)

arrows(-1, par("usr")[4]-.5, -28, par("usr")[4]-.5, length = 0.07, lwd=0.5, xpd=TRUE)
arrows(1, par("usr")[4]-.5, 28, par("usr")[4]-.5, length = 0.07, lwd=0.5, xpd=TRUE)

mtext(text="recommends RRT initation", side=3, line=0, at=2, cex=1/2.0, adj=0)
mtext(text="recommends no RRT initation", side=3, line=0, at=-28, cex=1/2.0, adj=0)
mtext("Blip (third decision)", side=1, line=2.25, cex=.83)

points(ite_k3[actual_ttts$a3==0], ite_se_k3[actual_ttts$a3==0], col=trans_blue, pch=16)
points(ite_k3[actual_ttts$a3==1], ite_se_k3[actual_ttts$a3==1], col=trans_red, pch=16, add=TRUE)

dev.copy2pdf(file = "fig1.pdf")