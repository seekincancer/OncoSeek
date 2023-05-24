load(file="./data/all_cohort.rds")

# merge gender model
all.POC.train.male = all.POC[all.POC$Source=="SeekIn"&all.POC$Gender=="M",]
roc0 = roc(as.factor(ifelse(all.POC.train.male$Type=="Cancer","1","0")),
           as.numeric(as.vector(all.POC.train.male$merge_prob)))
male_cutoff = roc0$thresholds[which(roc0$specificities>=0.9)[1]]
all.POC.train.female = all.POC[all.POC$Source=="SeekIn"&all.POC$Gender=="F",]
roc0 = roc(as.factor(ifelse(all.POC.train.female$Type=="Cancer","1","0")),
           as.numeric(as.vector(all.POC.train.female$merge_prob)))
female_cutoff = roc0$thresholds[which(roc0$specificities>=0.9)[1]]

all.POC.merge = as.data.frame(all.POC)
all.POC.merge[all.POC.merge$Gender=="M", "merge_prob"] = all.POC.merge[all.POC.merge$Gender=="M", "merge_prob"]-male_cutoff
all.POC.merge[all.POC.merge$Gender=="F", "merge_prob"] = all.POC.merge[all.POC.merge$Gender=="F", "merge_prob"]-female_cutoff

save("all.POC","all.POC.merge","female_cutoff","male_cutoff", file="./data/all_cohort_merge.rds")
