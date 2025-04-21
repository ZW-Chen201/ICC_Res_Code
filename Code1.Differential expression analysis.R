#01.Differential expression analysis
library(limma)
logFoldChange=2
P=0.05
setwd("C:\\Users\\")
rt=read.table("genematrix.txt",sep="\t",header=T,check.names=F)
rt=as.matrix(rt)
rownames(rt)=rt[,1]
exp=rt[,2:ncol(rt)]
dimnames=list(rownames(exp),colnames(exp))
rt=matrix(as.numeric(as.matrix(exp)),nrow=nrow(exp),dimnames=dimnames)
rt=avereps(rt)
rt=rt[rowMeans(rt)>0,]
rt=log2(rt+1)
modType=c(rep("con",5),rep("treat",5))
design <- model.matrix(~0+factor(modType))
colnames(design) <- c("con","treat")
fit <- lmFit(rt,design)
cont.matrix<-makeContrasts(treat-con,levels=design)
fit2 <- contrasts.fit(fit, cont.matrix)
fit2 <- eBayes(fit2)
#write table
allDiff=topTable(fit2,adjust='fdr',number=200000)
write.table(allDiff,file="limmaTab.xls",sep="\t",quote=F)
diffSig <- allDiff[with(allDiff, (abs(logFC)>logFoldChange & P.Value < P )), ]
write.table(diffSig,file="diff.xls",sep="\t",quote=F)