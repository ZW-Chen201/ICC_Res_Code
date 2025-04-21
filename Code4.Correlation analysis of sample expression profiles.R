#04.Correlation analysis of sample expression profiles
library(pheatmap)
library(RColorBrewer)
setwd("C:\\Users\\")
rt = read.table("genematix.txt", sep="\t", header=T, check.names=F)
rt = as.matrix(rt)
rownames(rt) = rt[,1]
exp = rt[,2:ncol(rt)]
dimnames = list(rownames(exp), colnames(exp))
rt = matrix(as.numeric(as.matrix(exp)), nrow=nrow(exp), dimnames=dimnames)
gene_variance = apply(rt, 1, var)
top_1000_genes = names(sort(gene_variance, decreasing = TRUE)[1:1000])
rt_filtered = rt[top_1000_genes, ]
cor_matrix = cor(rt_filtered, method="spearman")
# Generate the heatmap
pdf("correlation_heatmap.pdf")
pheatmap(cor_matrix, 
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         color = colorRampPalette(rev(brewer.pal(9, "RdYlBu")))(100),
         main = "Correlation Heatmap",
         show_rownames = T, 
         show_colnames = T)
dev.off()