# Data Visualization
library(dplyr)
library(ggplot2)
library(wordcloud2)
options(scipen = 99)

channel_tbl <- readRDS("data-raw/channels.rds") %>% 
  as_tibble()

channel_tbl %>% 
  count(kategori) %>% 
  ggplot(
    mapping = aes(
      x = reorder(kategori, n), 
      y = n
      )
    ) + 
  geom_col(
    fill = "#E12A26"
    ) + 
  scale_y_continuous(
    limits = c(0, 100, by = 25)
    ) + 
  labs(
    title = "Kategori Channel di Indonesia",
    x = "Kategori",
    y = "Banyaknya Channel"
    ) + 
  coord_flip() + 
  theme_minimal() + 
  theme(
    panel.grid.major.y = element_blank(),
    text = element_text(size = 16)
    )

channel_tbl %>% 
  filter(
    local_ranking <= 20
    ) %>% 
  ggplot(
    mapping = aes(
        x = reorder(channel_title, -local_ranking), 
        y = subscriberCount
        )
    ) + 
  geom_col(
    fill = "#E12A26"
    ) + 
  labs(
    title = "Top 20 Channel Indonesia",
    caption = "*Berdasarkan Jumlah Subscriber",
    x = "Nama Channel",
    y = "Subscriber (juta)"
    ) + 
  coord_flip() + 
  theme_minimal() + 
  theme(
    panel.grid.major.y = element_blank(),
    text = element_text(size = 16), 
    plot.caption = element_text(face = "italic")
    )
  
channel_tbl %>% 
  select(
    channel_name, 
    subscriberCount
    ) %>% 
  rename(
    word = channel_name, 
    freq = subscriberCount
    ) %>% 
  wordcloud2()

channel_tbl %>% 
  ggplot(mapping = aes(
    x = usia_channel, 
    fill = kategori, 
    color = kategori
    )
    ) + 
  geom_histogram(
    alpha = 0.6, 
    binwidth = 1
    ) + 
  labs(
    title = "Durasi Bergabung dengan YouTube",
    x = "Usia Channel (tahun)",
    y = "Banyaknya Channel"
    ) + 
  facet_wrap( ~ kategori) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    text = element_text(size = 16), 
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
    )

channel_tbl %>% 
  ggplot(
    mapping = aes(
      x = penghasilan_bulanan/10^6, 
      fill = kategori, 
      color = kategori
      )
    ) + 
  geom_histogram(
    alpha = 0.6, 
    binwidth = 500
    ) + 
  labs(
    title = "Penghasilan Bulanan Channel",
    caption = "*lebar batang: 500 juta",
    x = "Penghasilan (juta)",
    y = "Banyaknya Channel"
    ) + 
  facet_wrap(~ kategori) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    text = element_text(size = 16), 
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    plot.caption = element_text(face = "italic")
    )


channel_tbl %>% 
  ggplot(mapping = aes(
    x = viewCount/10^9, 
    fill = kategori, 
    color = kategori
    )
    ) + 
  geom_histogram(
    alpha = 0.6
    ) + 
  labs(
    title = "Total View Channel",
    x = "Total View (miliar)",
    y = "Banyaknya Channel"
    ) +
  facet_wrap( ~ kategori) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    text = element_text(size = 16), 
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    plot.caption = element_text(face = "italic")
    )

channel_tbl %>% 
  head(5) %>% 
  mutate(
    pct = round(videoCount/sum(videoCount), 4)*100
    ) %>% 
  ggplot(
    mapping = aes(
      x = reorder(channel_title, -videoCount), 
      y = videoCount
      )
    ) + 
  geom_col(
    fill = "#E12A26"
    ) + 
  geom_text(
    mapping = aes(
      label = paste0(formatC(videoCount, big.mark = ",", decimal.mark = ".", format = "d"), " (", pct, "%)")), 
    vjust = -0.25
    ) + 
  labs(
    title = "Banyaknya Video Top 5 Subscriber Channel",
    y = "Banyaknya Video",
    x = "Channel"
    ) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    text = element_text(size = 16), 
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    plot.caption = element_text(face = "italic")
  )

channel_tbl %>% 
  head(5) %>% 
  mutate(
    pct = round(viewCount/sum(viewCount), 4)*100
    ) %>% 
  ggplot(
    mapping = aes(
      x = reorder(channel_title, -viewCount), 
      y = round(viewCount/10^9, 2)
      )
    ) + 
  geom_col(
    fill = "#E12A26"
    ) + 
  geom_text(
    mapping = aes(
      label = paste0(round(viewCount/10^9, 2), " (", pct, "%)")), 
    vjust = -0.25
    ) + 
  labs(
    title = "Banyaknya View Top 5 Subscriber Channel",
    y = "Banyaknya View (miliar)",
    x = "Channel"
    ) +
  theme_minimal() + 
  theme(
    legend.position = "none",
    text = element_text(size = 16), 
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    plot.caption = element_text(face = "italic")
  )
