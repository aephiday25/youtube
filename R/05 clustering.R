library(factoextra)
library(gridExtra)
library(tidyr)

channel_pca_dt <- channel_pca$x[, 1:pc.index]

grid.arrange(
  fviz_nbclust(
    x = channel_pca_dt, 
    FUNcluster = kmeans, 
    k.max = 10, 
    method = "wss"
    ) + 
    theme_light() + 
    labs(title = "Metode Elbow") + 
    theme(text = element_text(size = 16)),
  fviz_nbclust(
    x = channel_pca_dt, 
    FUNcluster = kmeans, 
    k.max = 10, 
    method = "silhouette"
    ) + 
    theme_light() + 
    labs(
      title = "Metode Silhouette"
      ) + 
    theme(
      text = element_text(size = 16)
      ),
  nrow = 1
)

kc <- 2
set.seed(1001)
kmean.obj <- kmeans(
  x = channel_pca_dt, 
  centers = kc, 
  iter.max = 1000, 
  nstart = 25
  )

channel_tbl <- channel_tbl %>% 
  mutate(
    cluster = kmean.obj$cluster
    )

grid.arrange(
  channel_tbl %>% 
    count(cluster) %>% 
    mutate(
      pct = round(n/sum(n), 4)*100
      ) %>% 
    ggplot(
      mapping = aes(factor(cluster), n, fill = factor(cluster))
      ) + 
    geom_col(
      alpha = 0.7
      ) + 
    geom_text(
      mapping = aes(label = paste0(n, " (", pct, "%)")), 
      vjust = -0.25
      ) + 
    theme_light() + 
    labs(
      title = "Banyaknya Anggota Cluster",
      x = "Cluster",
      y = "Banyaknya Channel",
      fill = "Cluster"
      ) + 
    theme(
      legend.position = "top", 
      text = element_text(size = 16)),
  
  fviz_cluster(
    object = kmean.obj, 
    data = channel_pca_dt, 
    show.clust.cent = TRUE
    ) + 
    theme_light() + 
    labs(
      fill = "Cluster",
      color = "Cluster",
      shape = "Cluster"
      ) + 
    theme(
      legend.position = "top",
      text = element_text(size = 16)
      ),
  nrow = 1
)

# Profiling
channel_cluster <- channel_tbl %>% 
  group_by(cluster)

channel_cluster %>% 
  count(cluster, kategori) %>% 
  group_by(cluster) %>% 
  summarise(
    n_kategori = n()
    ) %>% 
  mutate(
    pct = n_kategori/sum(n_kategori)
    ) %>% View()

channel_cluster %>% 
  summarise(
    nmember = n(),
    
    avg_subscriberCount = mean(subscriberCount),
    avg_usia_channel = mean(usia_channel),
    avg_viewCount = mean(viewCount),
    avg_videoCount = mean(videoCount),
    avg_penghasilan_bulanan = mean(penghasilan_bulanan),
    avg_pervideo = mean(penghasilan_pervideo),
    avg_paparan = mean(paparan)
  ) %>% 
  tidyr::pivot_longer(
    cols = nmember:avg_paparan, 
    names_to = "Variable", 
    values_to = "value"
    ) %>% 
  tidyr::pivot_wider(
    id_cols = Variable, 
    names_from = cluster, 
    values_from = value, 
    names_prefix = "Cluster"
    ) %>% #write.csv("data-raw/profile.csv", row.names = FALSE)#View()
  filter(substr(Variable, 1, 3) == "avg")
