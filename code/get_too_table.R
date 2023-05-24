final.too.prob.cla = read.csv("./data/too.txt", sep="\t")

top1 = sum(final.too.prob.cla$truth == final.too.prob.cla$first)/nrow(final.too.prob.cla)
top2 = sum(final.too.prob.cla$truth == final.too.prob.cla$first|final.too.prob.cla$truth == final.too.prob.cla$second)/nrow(final.too.prob.cla)

print(top1)
print(top2)

too_table = table(final.too.prob.cla$truth, final.too.prob.cla$first)

write.table(too_table, file="./output/too_table.txt", sep="\t", quote = F)

