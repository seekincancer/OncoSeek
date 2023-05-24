library("tidyr")
library("pROC")
library("dplyr")
library("ggplot2")

load(file="./data/multi_model_POC.rda")

total_model_POC_merge = total.model.POC
total_model_POC_merge[total_model_POC_merge$Gender=="M", "glm_prob"] = total_model_POC_merge[total_model_POC_merge$Gender=="M", "glm_prob"]-glm_male_cutoff
total_model_POC_merge[total_model_POC_merge$Gender=="F", "glm_prob"] = total_model_POC_merge[total_model_POC_merge$Gender=="F", "glm_prob"]-glm_female_cutoff
total_model_POC_merge[total_model_POC_merge$Gender=="M", "svm_prob"] = total_model_POC_merge[total_model_POC_merge$Gender=="M", "svm_prob"]-svm_male_cutoff
total_model_POC_merge[total_model_POC_merge$Gender=="F", "svm_prob"] = total_model_POC_merge[total_model_POC_merge$Gender=="F", "svm_prob"]-svm_female_cutoff
total_model_POC_merge[total_model_POC_merge$Gender=="M", "gbm_prob"] = total_model_POC_merge[total_model_POC_merge$Gender=="M", "gbm_prob"]-gbm_male_cutoff
total_model_POC_merge[total_model_POC_merge$Gender=="F", "gbm_prob"] = total_model_POC_merge[total_model_POC_merge$Gender=="F", "gbm_prob"]-gbm_female_cutoff
total_model_POC_merge[total_model_POC_merge$Gender=="M", "rf_prob"] = total_model_POC_merge[total_model_POC_merge$Gender=="M", "rf_prob"]-rf_male_cutoff
total_model_POC_merge[total_model_POC_merge$Gender=="F", "rf_prob"] = total_model_POC_merge[total_model_POC_merge$Gender=="F", "rf_prob"]-rf_female_cutoff

# A
windowsFonts(A=windowsFont("Times New Roman"))
par(family="A",  font = 2, cex.axis=1.3,cex.lab=2)
outfile_name = "./output/figS4_A.jpg"
jpeg(filename=outfile_name, width=6, height=6, units="in",bg="white", res=300, family="A")
yanse=RColorBrewer::brewer.pal(10,"RdYlGn")
temp_roc_df = as.data.frame(total_model_POC_merge) %>% filter((Source == "SeekIn"))
roc1 = roc(as.factor(ifelse(temp_roc_df$Type=="Cancer","1","0")),
           as.numeric(as.vector(temp_roc_df$glm_prob)))
plot(roc1,type = "l",cex.axis=1.6,cex.lab=2.2,col = yanse[1],lwd=3,xlim=c(1,0), font = 2, font.lab=2)
auc_list = c(paste0("GLM Training cohort: ",sprintf("%0.3f", round(roc1$auc,3))))
temp_roc_df = as.data.frame(total_model_POC_merge) %>% filter((Source == "SeekIn"))
roc2 = roc(as.factor(ifelse(temp_roc_df$Type=="Cancer","1","0")),
           as.numeric(as.vector(temp_roc_df$svm_prob)))
lines(roc2,type = "l",cex.axis=1.3,cex.lab=2,col = yanse[4],lwd=3,xlim=c(1,0))
auc_list = c(auc_list, paste0("SVM Training cohort: ",sprintf("%0.3f", round(roc2$auc,3))))
temp_roc_df = as.data.frame(total_model_POC_merge) %>% filter((Source == "SeekIn"))
roc3 = roc(as.factor(ifelse(temp_roc_df$Type=="Cancer","1","0")),
           as.numeric(as.vector(temp_roc_df$gbm_prob)))
lines(roc3,type = "l",cex.axis=1.3,cex.lab=2,col = yanse[7],lwd=3,xlim=c(1,0))
auc_list = c(auc_list, paste0("GBM Training cohort: ",sprintf("%0.3f", round(roc3$auc,3))))
temp_roc_df = as.data.frame(total_model_POC_merge) %>% filter((Source == "SeekIn"))
roc4 = roc(as.factor(ifelse(temp_roc_df$Type=="Cancer","1","0")),
           as.numeric(as.vector(temp_roc_df$rf_prob)))
lines(roc4,type = "l",cex.axis=1.3,cex.lab=2,col = yanse[10],lwd=3,xlim=c(1,0))
auc_list = c(auc_list, paste0("RF Training cohort: ",sprintf("%0.3f", round(roc3$auc,3))))
abline(v=0.9, lty=2, col="grey", lwd=2)
legend(0.7,0,auc_list[c(3,1,4,2)],bty="n",cex=1.3,col = c(yanse[7],yanse[1],yanse[10],yanse[4]),lwd=2,seg.len=0.4,yjust = 0.1,lty=1,x.intersp = 0.5,y.intersp=0.8, title = "AUC", text.font=2)

dev.off()
# 
# glm = roc1
# svm = roc2
# gbm = roc3
# rf = roc4
# 
# roc.test(glm, svm)
# roc.test(glm, gbm)
# roc.test(glm, rf)

# rocthr1 <- ci(roc1, of="thresholds", thresholds=0)
# rocthr2 <- ci(roc2, of="thresholds", thresholds=0)
# rocthr3 <- ci(roc3, of="thresholds", thresholds=0)
# rocthr4 <- ci(roc4, of="thresholds", thresholds=0)

#B
roc_df = data.frame()
source_group = c("SeekIn", "SYSMH", "JHUSM")
for(source_name in source_group){
  temp_roc_df = as.data.frame(total_model_POC_merge) %>% filter((Source == source_name))
  temp_roc = roc(as.factor(ifelse(temp_roc_df$Type=="Cancer","1","0")),
             as.numeric(as.vector(temp_roc_df$gbm_prob)))
  temp_rocthr = ci(temp_roc, of="thresholds", thresholds=0)
  roc_df = rbind(roc_df, c(paste0("gbm_",source_name),"specificity",temp_rocthr$specificity))
  roc_df = rbind(roc_df, c(paste0("gbm_",source_name),"sensitivity",temp_rocthr$sensitivity))

  temp_roc = roc(as.factor(ifelse(temp_roc_df$Type=="Cancer","1","0")),
                 as.numeric(as.vector(temp_roc_df$glm_prob)))
  temp_rocthr = ci(temp_roc, of="thresholds", thresholds=0)
  roc_df = rbind(roc_df, c(paste0("glm_",source_name),"specificity",temp_rocthr$specificity))
  roc_df = rbind(roc_df, c(paste0("glm_",source_name),"sensitivity",temp_rocthr$sensitivity))
  
  temp_roc = roc(as.factor(ifelse(temp_roc_df$Type=="Cancer","1","0")),
                 as.numeric(as.vector(temp_roc_df$rf_prob)))
  temp_rocthr = ci(temp_roc, of="thresholds", thresholds=0)
  roc_df = rbind(roc_df, c(paste0("rf_",source_name),"specificity",temp_rocthr$specificity))
  roc_df = rbind(roc_df, c(paste0("rf_",source_name),"sensitivity",temp_rocthr$sensitivity))
  
  temp_roc = roc(as.factor(ifelse(temp_roc_df$Type=="Cancer","1","0")),
                 as.numeric(as.vector(temp_roc_df$svm_prob)))
  temp_rocthr = ci(temp_roc, of="thresholds", thresholds=0)
  roc_df = rbind(roc_df, c(paste0("svm_",source_name),"specificity",temp_rocthr$specificity))
  roc_df = rbind(roc_df, c(paste0("svm_",source_name),"sensitivity",temp_rocthr$sensitivity))
}
colnames(roc_df) = c("model", "group", "low", "median", "high")
roc_df$low = as.numeric(roc_df$low)
roc_df$median = as.numeric(roc_df$median)
roc_df$high = as.numeric(roc_df$high)
roc_df$model = factor(roc_df$model, levels = roc_df[roc_df$group=="specificity","model"])

windowsFonts(A=windowsFont("Times New Roman"))
outfile_name = "./output/figS4_B.jpg"
jpeg(filename=outfile_name, width=7.23, height=6, units="in",bg="white", res=300, family="A")
ggplot(data=roc_df[c(9:24),], aes(x=model, y=median, color=group))+
  geom_point(size=2.5, position=position_dodge(1))+
  geom_errorbar(aes(ymin=low, ymax=high), width=0, position=position_dodge(1),size=1)+
  geom_vline(xintercept = 4.5, lty=2, color="grey", linewidth=1)+
  xlab("")+
  ylab("")+
  annotate('text',x=2.5,y=1.1,
           label="Validation cohort 1",
           size=6.5,color='black', family = "A",  fontface = "bold")+
  annotate('text',x=6.55,y=1.1,
           label="Validation cohort 2",
           size=6.5,color='black', family = "A",  fontface = "bold")+
  scale_x_discrete(breaks=c("gbm_SYSMH","glm_SYSMH","rf_SYSMH","svm_SYSMH","gbm_JHUSM","glm_JHUSM","rf_JHUSM","svm_JHUSM"),
                   labels=c("GBM","GLM","RF","SVM","GBM","GLM","RF","SVM"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1.1), breaks=c(0,0.25,0.5,0.75,1))+
  scale_color_manual(values = c("#4473C5FF", "#ED7D31FF"))+
  theme_classic()+
  theme(
    plot.margin = margin(t = 30,
                         r = 5,
                         b = 5,
                         l = 5),
    # legend.position = "none",
    legend.title = element_blank(),
    text=element_text(size=20, family = "A"),
    legend.text = element_text(size = 15,  face = "bold"),
    axis.title = element_text(size = 25,  face = "bold"),
    axis.text = element_text(size = 20,  face = "bold",colour="black"),
    axis.text.x = element_text(angle = 90, vjust=0.25))

dev.off()




