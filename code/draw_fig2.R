library("ggpubr")

load(file="./data/jh_cohort.rds")

# fig2
# only use JH cohort for test

count_ptm = jh.cohort
count_ptm$Type = as.factor(count_ptm$Type)
# JH cohort has no CA724
count_ptm$CA724 = 0.1

count_ptm$CancerType = factor(count_ptm$CancerType, levels=c("Healthy", "Breast", "Colorectum", "Esophagus", "Liver", "Lung", "Lymphoma", "Ovary", "Pancreas", "Stomach", "Others"))

# eng
# count_ptm$CancerType = as.character(count_ptm$CancerType)
# count_ptm[count_ptm$CancerType=="Esophagus", "CancerType"] = "Oesophagus"
# count_ptm$CancerType = factor(count_ptm$CancerType, levels=c("Healthy", "Breast", "Colorectum", "Liver", "Lung", "Lymphoma", "Oesophagus", "Ovary", "Pancreas", "Stomach", "Others"))

options(scipen = 5)
yanse=RColorBrewer::brewer.pal(10,"Set3")
ptm_name = c("AFP","CA125","CA153","CA199","CA211","CA724","CEA")
new_ptm_name = c("AFP (IU/ml)","CA125 (U/ml)","CA15-3 (U/ml)","CA19-9 (U/ml)","CYFRA21-1 (ng/ml)","CA72-4 (U/ml)","CEA (ng/ml)")
ptm_cutoff = c(5.8,35,26.4,27,3.3,6.9,4.7)
count = 0
bbb = data.frame(matrix(nrow=10))
windowsFonts(A=windowsFont("Times New Roman"))
fmt_dcimals <- function(decimals=0){
  function(x) format(x,nsmall = decimals,scientific = FALSE)
}

for(i in ptm_name){
  count = count + 1
  h <- formula(paste0(i," ~ CancerType"))
  pvalue_output = compare_means(formula=h, data=count_ptm, ref.group = "Healthy", alternative = "greater")
  outfile_name = paste0(paste0("./output/fig2_wilcoxon_test_",ptm_name[count]), ".txt")
  write.table(pvalue_output, file=outfile_name, sep="\t", quote = F)
  
  outfile_name = paste0(paste0("./output/fig2_",ptm_name[count]), ".jpg")
  jpeg(filename=outfile_name, width=6, height=6, units="in",bg="white", res=300)
  p=ggviolin(data=count_ptm, x="CancerType", y=ptm_name[count], add="boxplot",
             fill="CancerType")+
    geom_hline(yintercept = ptm_cutoff[count], linewidth=1,lty=2, color="grey")+
    
    scale_y_log10(labels=fmt_dcimals(1))+
    ylab(new_ptm_name[count])+
    scale_fill_manual(breaks = c("Healthy", "Breast", "Colorectum", "Esophagus", "Liver", "Lung", "Lymphoma", "Ovary", "Pancreas", "Others","Stomach"),
                       values = c("#FFFFFF", yanse))+
    # eng
    # scale_fill_manual(breaks = c("Healthy", "Breast", "Colorectum", "Liver", "Lung", "Lymphoma", "Oesophagus", "Ovary", "Pancreas", "Others","Stomach"),
    #                   values = c("#FFFFFF", yanse))+
    xlab("")+
    theme_classic()+
    theme(
      plot.margin = margin(t = 30,
                           r = 5,
                           b = 5,
                           l = 5),
      legend.position="none",
      text=element_text(size=20, family = "A"),
      legend.text = element_text(size = 15,  face = "bold"),
      axis.title = element_text(size = 25,  face = "bold"),
      axis.text = element_text(size = 20,  face = "bold",colour="black"),
      axis.text.x = element_text(angle = 90, vjust=0.25))
  print(p)
  dev.off()
}

