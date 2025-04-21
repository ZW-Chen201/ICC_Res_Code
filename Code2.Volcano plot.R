#02.Volcano plot
library(ggplot2)
library(readxl)
library(dplyr)
library(ggrepel)
setwd("C:\\Users\\")
df <- read_excel("limmaTab.xlsx")
df <- df %>%
  mutate(
    logFC = as.numeric(logFC),
    P.Value = as.numeric(P.Value),
    sig = case_when(
      logFC > 2 & P.Value < 0.05 ~ "Up",
      logFC < -2 & P.Value < 0.05 ~ "Down",
      TRUE ~ "Not sig"
    )
  )
diff_genes <- df %>% filter(abs(logFC) > 2 & P.Value < 0.05)
custom_colors <- c(
  "Up" = "#FB7F72",
  "Down" = "#7FB1D3",
  "Not sig" = "grey"
)
# Draw the volcano plot
p <- ggplot(df, aes(x = logFC, y = -log10(P.Value))) +
  
  geom_point(data = subset(df, sig == "Not sig"),
             color = "grey", alpha = 0.6, size = 3) +
  
  geom_point(data = subset(df, sig != "Not sig"),
             aes(fill = sig), size = 5, shape = 21,
             color = "black", stroke = 1.2) +
  scale_fill_manual(values = custom_colors) +
  
  geom_text_repel(data = diff_genes, aes(label = Gene),
                  size = 5, max.overlaps = Inf,
                  box.padding = 0.5, point.padding = 0.3,
                  segment.color = "grey50") +
  
  geom_vline(xintercept = c(-2, 2), linetype = "dashed") +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  labs(
    title = "XXX",
    x = "Log2 Fold Change",
    y = "-Log10(p-value)",
    fill = NULL
  ) +
  theme_bw(base_size = 20) +  
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 22),
    legend.text = element_text(size = 22),
    legend.position = "right"
  )
ggsave("volcano plot.pdf", plot = p, width = 12, height = 9)