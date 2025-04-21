library(ComplexHeatmap)
library(readxl)
library(circlize)
library(RColorBrewer)
setwd("C:\\Users\\")
expr <- read_excel("heatmap_top15.xlsx")
expr_matrix <- as.matrix(expr[,-1])
rownames(expr_matrix) <- expr[[1]]
expr_scaled <- t(scale(t(expr_matrix)))
sample_group <- factor(c(rep("A", 5), rep("B", 5)))
col_anno <- HeatmapAnnotation(
  Group = sample_group,
  col = list(Group = c("A" = "#66C2A5", "B" = "#FC8D62")),
  annotation_legend_param = list(title = "Sample Group")
)
col_fun <- colorRamp2(
  seq(-2, 2, length.out = 100),
  colorRampPalette(rev(brewer.pal(9, "RdYlBu")))(100)
)
# Draw the heatmap
pdf("Heatmap.pdf", width = 8, height = 6)
draw(
  Heatmap(
    expr_scaled,
    name = "Z-score",
    col = col_fun,
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    top_annotation = col_anno,
    show_column_names = FALSE,
    row_names_side = "left",
    row_names_gp = gpar(fontsize = 10),
    column_title = "XXX",
    column_title_gp = gpar(fontsize = 16, fontface = "bold"),
    heatmap_legend_param = list(
      title = "Expression",
      title_gp = gpar(fontsize = 12),
      labels_gp = gpar(fontsize = 10))))
dev.off()