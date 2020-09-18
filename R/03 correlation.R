library(tibble)
library(corrplot)

channel_tbl

channel <- channel_tbl %>% 
  select(
    channel_name, 
    avg_view, 
    subscriberCount, 
    usia_channel, 
    viewCount, 
    videoCount,
    penghasilan_bulanan, 
    penghasilan_pervideo, 
    paparan
    ) %>% 
  column_to_rownames(
    var = "channel_name"
    )

mycor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- mycor.mtest(channel)

corrplot(
  corr = cor(channel), 
  method = "circle", 
  type = "lower", 
  title = "Plot Matriks Korelasi Channel", 
  mar = c(0, 0, 2, 0), 
  addCoef.col = "black", 
  number.cex = 0.7, # Add coefficient of correlation
  tl.col = "#E12A26", 
  tl.srt = 0.1, 
  tl.offset = 0.4, 
  tl.cex = 1, #Text label color and rotation
  # p.mat = p.mat, sig.level = 0.05, insig = "pch", # Combine with significance
  diag = TRUE # show correlation coefficient on the principal diagonal
) 

channel %>% 
  ggplot(
    mapping = aes(
      x = usia_channel, 
      y = subscriberCount
      )
    ) + 
  geom_point() + 
  scale_x_continuous(
    labels = scales::comma
    ) + 
  scale_y_continuous(
    labels = scales::comma
    ) + 
  theme_minimal() + 
  theme(
    text = element_text(size = 16)
    )

channel %>% 
  ggplot(
    mapping = aes(
      x = penghasilan_pervideo, 
      y = paparan
      )
    ) + 
  geom_point() + 
  scale_x_continuous(
    labels = scales::comma
    ) + 
  scale_y_continuous(
    labels = scales::comma
    ) + 
  theme_minimal() + 
  theme(
    text = element_text(size = 16)
    )
