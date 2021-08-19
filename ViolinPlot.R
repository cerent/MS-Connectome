
  setwd("/ChangeDirectory/")
  pdf(file = "AUCresults1.pdf",   # The directory you want to save the file in
      width = 18, # The width of the plot in inches
      height = 12)
  
 # AUC results
  yalist2<-list(yalist_pairwise_edss2,yalist_regional_edss2)
  
  labels1<-c("SC","FC","eSC", "eFC")
  sequence<-seq(0.5,1,0.05);par(mar=c(12,6,6,2.1));par(mfrow=c(1,2))
  main1<-c("Pairwise Connectivity","Regional Connectivity \n (Node Strength)")
  par(mar=c(12,6,6,2.1));par(mfrow=c(1,2))
  ylab1=c("Area Under ROC Curve (AUC)","")
  
  for(modeltype in 1:2){
    yalist1<-yalist2[[modeltype]]
    plot(0,0,type="n",xlim=c(0,(length(yalist1)+1)), ylim=c(0.5,1.3),  
         xaxt = 'n',yaxt = 'n', xlab ="", ylab = ylab1[modeltype],  
         main =main1[modeltype],cex.main=2,cex.lab=1.8)
    axis(2,at=seq(0.5,1,0.05),labels=seq(0.5,1,0.05),lwd.ticks=1,cex.axis=2)
    for(hline in seq(0.5,1,0.05)){abline(h = hline,lty=2)}
    library(vioplot)
    col1 = c("lightblue","lightgreen","blue","green")
    for (i in 1:(length(yalist1))) { vioplot(na.omit(yalist1[[i]]), at = i, add = T, col =col1[i] ) }
    axis(side=1,las=3,at=1:(length(yalist1)),labels=labels1,cex.axis=1.4)
    
    # add segments
    if(modeltype==1){
      segments(1,1.25,1,1.24,lwd=2);segments(1,1.25,4.1,1.25,lwd=2);text(4,1.26, "*", cex = 1.5)
      segments(2,1.20,2,1.19,lwd=2);segments(2,1.20,4.1,1.20,lwd=2);text(3,1.21, "*", cex = 1.5);text(4,1.21, "*", cex = 1.5)
      segments(3,1.15,3,1.14,lwd=2);segments(3,1.15,4.1,1.15,lwd=2);text(4,1.16, "*", cex = 1.5)

    }
    if(modeltype==2){
      segments(1,1.25,1,1.24,lwd=2);segments(1,1.25,4.1,1.25,lwd=2);text(3,1.26, "*", cex = 1.5);text(4,1.26, "*", cex = 1.5)
      segments(2,1.20,2,1.19,lwd=2);segments(2,1.20,4.1,1.20,lwd=2);text(3,1.21, "*", cex = 1.5);text(4,1.21, "*", cex = 1.5)
      segments(3,1.15,3,1.14,lwd=2);segments(3,1.15,4.1,1.15,lwd=2)

      
    }
    
  }
  dev.off()

