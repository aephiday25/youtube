library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(scales)

channel_pca <- prcomp(x = channel, 
                      center = TRUE, 
                      scale. = TRUE)


# Plot korelasi setelah PCA
p.mat <- mycor.mtest(
  mat = channel_pca$x
  )

corrplot(
  corr = cor(channel_pca$x), 
  method = "circle", 
  type = "lower", 
  title = "Plot Matriks Korelasi PC", 
  mar = c(0, 0, 2, 0), 
  # Add coefficient of correlation
  addCoef.col = "black", 
  number.cex = 0.7, 
  # Text label color and rotation
  tl.col = "#E12A26", 
  tl.srt = 0.1, 
  tl.offset = 0.4, 
  tl.cex = 1, 
  # Combine with significance
  p.mat = p.mat, 
  sig.level = 0.05, 
  insig = "pch",
  # hide correlation coefficient on the principal diagonal
  diag = TRUE 
) 

dtf <- data.frame(
  channel = rownames(channel_pca$x), 
  channel_pca$x
  )
datapc <- data.frame(
  varname = rownames(channel_pca$rotation), 
  channel_pca$rotation
  )

mult <- min(
  (max(dtf[, "PC2"]) - min(dtf[, "PC2"])/(max(datapc[, "PC2"]) - min(datapc[, "PC2"]))),
  (max(dtf[, "PC1"]) - min(dtf[, "PC1"])/(max(datapc[, "PC1"]) - min(datapc[, "PC1"])))
)

datapc <- transform(datapc,
                    v1 = 0.7 * mult * PC1,
                    v2 = 0.7 * mult * PC2
)
vars <- apply(channel_pca$x, 2, var)  
(props <- vars/sum(vars))

p <- ggplot(
  data = dtf, 
  mapping = aes(PC1, PC2)
  ) +
  geom_text(
    mapping = aes(label = channel), 
    alpha = 0.5, 
    size = 2
    ) + 
  geom_hline(
    mapping = aes(yintercept = 0), 
    size = 0.2
    ) + 
  geom_vline(
    mapping = aes(xintercept = 0), 
    size = 0.2
    ) + 
  labs(
    x = paste0("PC1 (", round(props[1], 4)*100, "%)"),
    y = paste0("PC2 (", round(props[2], 4)*100, "%)")
    ) +
  geom_text(
    data = datapc, 
    mapping = aes(x = v1, y = v2, label = varname), 
    size = 5, 
    vjust = -0.25, 
    color = "red"
    ) + 
  geom_segment(
    data = datapc, 
    mapping = aes(x = 0, y = 0, xend = v1, yend = v2), 
    arrow = arrow(length = unit(0.2, "cm")), 
    alpha = 0.75, 
    color = "red"
    ) + 
  theme_minimal() + 
  theme(
    text = element_text(size = 16)
    )

print(p)
## Atau dengan plotly::ggplotly() untuk lebih interaktif
# plotly::ggplotly(p, tooltip = "text")

(cumprops <- cumsum(props))

# Banyaknya PCs dengan Cumulative Proportion > 85%
(pc.index <- min(which(cumprops > 0.85)))

# Visualisasi Principal Component
ggplot(
  data = data.frame(pc = 1:ncol(channel_pca$x), cumprops = cumprops), 
  mapping = aes(x = pc, y = cumprops)
  ) +
  geom_line(
    color = "#E12A26", 
    size = 2
    ) + 
  geom_point(
    size = 2
    ) + 
  geom_vline(
    xintercept = pc.index, 
    linetype = 2, 
    color = "red", 
    size = 1
    ) + 
  labs(
    subtitle = paste0("PC = ", pc.index, " with Cummulative Prop. = ", round(cumprops[pc.index] *100, 2), "%"),
    x = "Number of Principal Components",
    y = "Cumulative Proportion of Variance"
    ) + 
  scale_x_continuous(
    breaks = seq(1, ncol(channel_pca$x), by = 2)
    ) +
  scale_y_continuous(
    labels = scales::percent_format(1)
    ) + 
  geom_label(
    x = pc.index, 
    y = cumprops[pc.index], 
    label = paste0(round(cumprops[pc.index] *100, 2), "%"), 
    hjust = 0.5, 
    vjust = 2
    ) + 
  theme_minimal() + 
  theme(
    text = element_text(size = 16)
    )

# Tabel kumulatif varians
summary(channel_pca)
