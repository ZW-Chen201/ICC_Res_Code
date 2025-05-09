#05.Survival analysis
library(survival)
library(survminer)
library(ggplot2)
library(readxl)
setwd("C:\\Users\\")
data <- read_excel("survival.xlsx")
fit <- survfit(Surv(time, event) ~ group, data = data)
print(fit)
# Plot Kaplan-Meier survival curve
km_plot <- ggsurvplot(
  fit = fit,
  data = data,
  fun = "pct",
  palette = c("#0072B5", "#BC3C29"),
  linetype = 1,
  pval = TRUE,
  censor = TRUE,
  censor.shape = "|",
  censor.size = 4,
  risk.table = FALSE,
  conf.int = FALSE,
  legend.labs = c("gene low", "gene high"),
  legend.title = NULL,
  xlab = "Time (months)",
  ylab = "Overall survival",
  break.time.by = 25,
  xlim = c(0, max(data$time, na.rm = TRUE)),
  ggtheme = theme_classic(base_size = 14) +  # ����׵�������
    theme(
      axis.line = element_line(color = "black"),    # ����X/Y����
      axis.ticks = element_line(color = "black"),   # ����������̶���
      panel.border = element_blank(),               # ȥ���߿�
      panel.grid = element_blank(),                 # ȥ������
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.position = "right"
    )
)
ggsave("survival_curve.pdf", km_plot$plot, width = 6, height = 5)