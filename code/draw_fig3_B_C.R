library("epiR")
library("data.table")
library("dplyr")
library("tidyr")

load(file="./data/all_cohort_merge.rds")

# B
cancertype_df = data.frame("statistic"=c(), "est"=c(), "lower"=c(), "upper"=c())
for(cancertype in names(table(all.POC.merge$CancerType))){
  tp = nrow(filter(all.POC.merge, merge_prob>=0&CancerType==cancertype))
  fp = nrow(filter(all.POC.merge, merge_prob>=0&CancerType=="Healthy"))
  fn = nrow(filter(all.POC.merge, merge_prob<0&CancerType==cancertype))
  tn = nrow(filter(all.POC.merge, merge_prob<0&CancerType=="Healthy"))
  aaa = epi.tests(c(tp,fp,fn,tn), digits = 3)$detail[3,]
  aaa[1,1] = paste0(cancertype, " (") %>% paste0(table(all.POC.merge$CancerType)[cancertype]) %>% paste0(")")
  # aaa[1,1] = cancertype
  cancertype_df = rbind(cancertype_df, aaa)
}
cancertype_df = filter(cancertype_df, !(statistic%like%"Healthy")&!(statistic%like%"Others"))
cancertype_df = cancertype_df[order(cancertype_df$est, decreasing = T),]

cancertype_df$statistic = factor(cancertype_df$statistic, levels = cancertype_df$statistic)

windowsFonts(A=windowsFont("Times New Roman"))
outfile_name = "./output/fig3_B.jpg"
jpeg(filename=outfile_name, width=6, height=7.23, units="in",bg="white", res=300)
p = ggplot(data=cancertype_df, aes(x=statistic, y=est))+
  geom_point(size=2.5, position=position_dodge(1))+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=position_dodge(1),size=1)+
  # ylim(0,1)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1))+
  xlab("")+
  ylab("Sensitivity")+
  # scale_x_discrete(breaks=c("GBM V1","GLM V1","RF V1","SVM V1","GBM V2","GLM V2","RF V2","SVM V2"), 
  # labels=c("GBM","GLM","RF","SVM","GBM","GLM","RF","SVM"))+
  theme_classic()+
  theme(
    plot.margin = margin(t = 30,
                         r = 5,
                         b = 5,
                         l = 5),
    # legend.position = "none",
    text=element_text(size=20, family = "A"),
    legend.text = element_text(size = 15,  face = "bold"),
    axis.title = element_text(size = 25,  face = "bold"),
    axis.text = element_text(size = 20,  face = "bold",colour="black"),
    axis.text.x = element_text(angle = 90, vjust=0.25))
print(p)
dev.off()

#C
cancerstage_df = data.frame("statistic"=c(), "est"=c(), "lower"=c(), "upper"=c())
for(cancerstage in names(table(all.POC.merge$Stage))){
  tp = nrow(filter(all.POC.merge, merge_prob>=0&Stage==cancerstage))
  fp = nrow(filter(all.POC.merge, merge_prob>=0&CancerType=="Healthy"))
  fn = nrow(filter(all.POC.merge, merge_prob<0&Stage==cancerstage))
  tn = nrow(filter(all.POC.merge, merge_prob<0&CancerType=="Healthy"))
  aaa = epi.tests(c(tp,fp,fn,tn), digits = 3)$detail[3,]
  aaa[1,1] = paste0(cancerstage, " (") %>% paste0(table(all.POC.merge$Stage)[cancerstage]) %>% paste0(")")
  # aaa[1,1] = cancertype
  cancerstage_df = rbind(cancerstage_df, aaa)
}
cancerstage_df = filter(cancerstage_df, !(statistic%like%"Unknown"))

windowsFonts(A=windowsFont("Times New Roman"))
outfile_name = "./output/fig3_C.jpg"
jpeg(filename=outfile_name, width=6, height=6, units="in",bg="white", res=300)
p = ggplot(data=cancerstage_df, aes(x=statistic, y=est))+
  geom_point(size=2.5, position=position_dodge(1))+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=position_dodge(1),size=1)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1))+
  xlab("")+
  ylab("Sensitivity")+
  theme_classic()+
  theme(
    plot.margin = margin(t = 30,
                         r = 5,
                         b = 5,
                         l = 5),
    # legend.position = "none",
    text=element_text(size=20, family = "A"),
    legend.text = element_text(size = 15,  face = "bold"),
    axis.title = element_text(size = 25,  face = "bold"),
    axis.text = element_text(size = 20,  face = "bold",colour="black"),
    axis.text.x = element_text(angle = 90, vjust=0.25))
print(p)
dev.off()

