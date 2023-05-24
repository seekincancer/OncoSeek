library("pROC")
library("dplyr")
library("tidyr")

load(file="./data/all_cohort_merge.rds")

windowsFonts(A=windowsFont("Times New Roman"))
outfile_name = "./output/fig3_A.jpg"
jpeg(filename=outfile_name, width=6, height=6, units="in",bg="white", res=300,family="A")
temp_roc_df = as.data.frame(all.POC.merge) %>% filter((Source == "SeekIn"))
roc1 = roc(as.factor(ifelse(temp_roc_df$Type=="Cancer","1","0")),
           as.numeric(as.vector(temp_roc_df$merge_prob)))
plot(roc1,type = "l",cex.axis=1.6,cex.lab=2.2,col = "#A50026",lwd=3,xlim=c(1,0),  font = 2, font.lab=2)
auc_list = c(paste0("Training cohort: ",round(roc1$auc,3)))
temp_roc_df = as.data.frame(all.POC.merge) %>% filter((Source == "SYSMH"))
roc2 = roc(as.factor(ifelse(temp_roc_df$Type=="Cancer","1","0")),
           as.numeric(as.vector(temp_roc_df$merge_prob)))
lines(roc2,type = "l",cex.axis=1.3,cex.lab=2,col = "#FDAE61",lwd=3,xlim=c(1,0),  font = 2)
auc_list = c(auc_list, paste0("Validation cohort 1: ",round(roc2$auc,3)))
temp_roc_df = as.data.frame(all.POC.merge) %>% filter((Source == "JHUSM"))
roc3 = roc(as.factor(ifelse(temp_roc_df$Type=="Cancer","1","0")),
           as.numeric(as.vector(temp_roc_df$merge_prob)))
lines(roc3,type = "l",cex.axis=1.3,cex.lab=2,col = "#006837",lwd=3,xlim=c(1,0),  font = 2)
auc_list = c(auc_list, paste0("Validation cohort 2: ",sprintf("%0.3f", round(roc3$auc,3))))
abline(v=0.9, lty=2, col="grey", lwd=2)
legend(0.7,0,auc_list,bty="n",cex=1.3,col = c("#A50026","#FDAE61","#006837"),lwd=2,seg.len=0.4,yjust = 0.1,lty=1,x.intersp = 0.5,y.intersp=0.8, text.font=2, title="AUC")

dev.off()

roc.test(roc1, roc2, method="delong")
roc.test(roc1, roc3, method="delong")
