load(file="./data/jh_cohort.rds")

# fig S2
# only use JH cohort for test
print(shapiro.test(jh.cohort$AFP))

windowsFonts(A=windowsFont("Times New Roman"))
outfile_name = "./output/fig_S2.jpg"
jpeg(filename=outfile_name, width=6, height=6, units="in",bg="white", res=300,family="A")
par(mar = c(5, 5, 2, 2) + 0.1)
qqnorm(jh.cohort$AFP,
       cex.axis=1.3,cex.lab=2,col = "black",lwd=1,  font = 2, font.lab=2, main=NA)
qqline(jh.cohort$AFP)
dev.off()

# fig S3
count_ptm = jh.cohort
count_ptm$CA724 = 0.001
ptm_name = c("AFP","CA125","CA153","CA199","CA211","CA724","CEA")
ptm_cutoff = c(5.8,35,26.4,27,3.3,6.9,4.7)

temp_sens = sum(count_ptm$Type=="Cancer"&count_ptm$AFP>5.8) / sum(count_ptm$Type=="Cancer")
temp_spec = sum(count_ptm$Type=="Healthy"&count_ptm$AFP<5.8) / sum(count_ptm$Type=="Healthy")
add_ptm = data.frame("number"=1, "sens"=temp_sens, "spec"=temp_spec)

temp_sens = sum(count_ptm$Type=="Cancer"&(count_ptm$AFP>5.8|count_ptm$CA125>35)) / sum(count_ptm$Type=="Cancer")
temp_spec = sum(count_ptm$Type=="Healthy"&count_ptm$AFP<5.8&count_ptm$CA125<35) / sum(count_ptm$Type=="Healthy")
add_ptm = rbind(add_ptm, data.frame("number"=2, "sens"=temp_sens, "spec"=temp_spec))

temp_sens = sum(count_ptm$Type=="Cancer"&(count_ptm$AFP>5.8|count_ptm$CA125>35|count_ptm$CA153>26.4)) / sum(count_ptm$Type=="Cancer")
temp_spec = sum(count_ptm$Type=="Healthy"&count_ptm$AFP<5.8&count_ptm$CA125<35&count_ptm$CA153<26.4) / sum(count_ptm$Type=="Healthy")
add_ptm = rbind(add_ptm, data.frame("number"=3, "sens"=temp_sens, "spec"=temp_spec))

temp_sens = sum(count_ptm$Type=="Cancer"&(count_ptm$AFP>5.8|count_ptm$CA125>35|count_ptm$CA153>26.4|count_ptm$CA199>27)) / sum(count_ptm$Type=="Cancer")
temp_spec = sum(count_ptm$Type=="Healthy"&count_ptm$AFP<5.8&count_ptm$CA125<35&count_ptm$CA153<26.4&count_ptm$CA199<27) / sum(count_ptm$Type=="Healthy")
add_ptm = rbind(add_ptm, data.frame("number"=4, "sens"=temp_sens, "spec"=temp_spec))

temp_sens = sum(count_ptm$Type=="Cancer"&(count_ptm$AFP>5.8|count_ptm$CA125>35|count_ptm$CA153>26.4|count_ptm$CA199>27|count_ptm$CA211>3.3)) / sum(count_ptm$Type=="Cancer")
temp_spec = sum(count_ptm$Type=="Healthy"&count_ptm$AFP<5.8&count_ptm$CA125<35&count_ptm$CA153<26.4&count_ptm$CA199<27&count_ptm$CA211<3.3) / sum(count_ptm$Type=="Healthy")
add_ptm = rbind(add_ptm, data.frame("number"=5, "sens"=temp_sens, "spec"=temp_spec))

temp_sens = sum(count_ptm$Type=="Cancer"&(count_ptm$AFP>5.8|count_ptm$CA125>35|count_ptm$CA153>26.4|count_ptm$CA199>27|count_ptm$CA211>3.3|count_ptm$CA724>6.9)) / sum(count_ptm$Type=="Cancer")
temp_spec = sum(count_ptm$Type=="Healthy"&count_ptm$AFP<5.8&count_ptm$CA125<35&count_ptm$CA153<26.4&count_ptm$CA199<27&count_ptm$CA211<3.3&count_ptm$CA724<6.9) / sum(count_ptm$Type=="Healthy")
add_ptm = rbind(add_ptm, data.frame("number"=6, "sens"=temp_sens, "spec"=temp_spec))

temp_sens = sum(count_ptm$Type=="Cancer"&(count_ptm$AFP>5.8|count_ptm$CA125>35|count_ptm$CA153>26.4|count_ptm$CA199>27|count_ptm$CA211>3.3|count_ptm$CA724>6.9|count_ptm$CEA>4.7)) / sum(count_ptm$Type=="Cancer")
temp_spec = sum(count_ptm$Type=="Healthy"&count_ptm$AFP<5.8&count_ptm$CA125<35&count_ptm$CA153<26.4&count_ptm$CA199<27&count_ptm$CA211<3.3&count_ptm$CA724<6.9&count_ptm$CEA<4.7) / sum(count_ptm$Type=="Healthy")
add_ptm = rbind(add_ptm, data.frame("number"=7, "sens"=temp_sens, "spec"=temp_spec))

write.table(add_ptm, file="./output/fig_S3_table.txt", sep="\t", quote = F)

