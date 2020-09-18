library(rvest)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)
library(tuber)
options(scipen = 99)

channel <- read_html("data-raw/Top 250 YouTubers Terbanyak Disubscribed di Indonesia - NoxInfluencer.html") %>% 
  html_nodes("td.profile") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  str_replace_all(pattern = "https://id.noxinfluencer.com/youtube/channel/", "") %>% 
  .[-c(36, 41)]

source("R/fun_info_detail.R")

details <- purrr::map_dfr(channel, info_detail) %>% 
  mutate(tenure_bulan = as.duration(join_date %--% Sys.Date())/dmonths(1)) %>% 
  relocate(tenure_bulan, .after = join_date)


influencer_tbl <- read_html("data-raw/Top 250 YouTubers Terbanyak Disubscribed di Indonesia - NoxInfluencer.html") %>% 
  html_table() %>% .[[1]] %>% 
  select(-1) %>% 
  clean_names() %>% 
  as_tibble() %>% 
  rename(nama = informasi_dasar)

influencer_tbl <- influencer_tbl %>% 
  mutate(
    local_ranking = 1:nrow(influencer_tbl),
    subscriber_juta = str_sub(jumlah_subscriber, start = 1L,  
                              end = str_locate(jumlah_subscriber, "JT")[,1] - 1) %>% as.numeric(),
    avg_view = str_sub(avg_view, start = 1L, end = str_locate(avg_view, " ")[,1] - 1),
    avg_view = case_when(str_detect(avg_view, "JT") ~ as.numeric(str_replace(avg_view, "JT", ""))*10^6,
                         str_detect(avg_view, "RB") ~ as.numeric(str_replace(avg_view, "RB", ""))*10^5,
                         TRUE ~ as.numeric(avg_view)),
    channel = channel
    ) %>% 
  relocate(
    channel, 
    .after = nama
    ) %>% 
  relocate(
    local_ranking, 
    .before = nama
    ) %>% 
  select(
    -jumlah_subscriber
    ) 

influencer_tbl <- inner_join(
  influencer_tbl, 
  details, 
  by = c("nama", "channel")
  ) %>% 
  drop_na()

saveRDS(influencer_tbl, "data-raw/influencer.rds")

tuber::yt_oauth(app_id = "app-ID", 
                app_secret = "secret-code")

ch_stats <- map(channel, function(x){
  message(x)
  tryCatch(get_channel_stats(x), 
           error = function(c) return(NULL), 
           warning = function(c) return(NULL))
})

ch_stat <- map_dfr(ch_stats, 
                   function(ii) 
                     tibble(
                       channel_id = ii$id, 
                       channel_title = ii$snippet$title,
                       published_at = ii$snippet$publishedAt,
                       bind_cols(ii$statistics)
                     )
)

nm <- read.csv("data-raw/nama.csv", header = TRUE)

ch_stat %>% 
  mutate(
    usia_channel = as.duration(published_at %--% Sys.time())/dyears(1),
    viewCount = as.numeric(viewCount),
    commentCount = as.numeric(commentCount),
    subscriberCount = as.numeric(subscriberCount),
    videoCount = as.numeric(videoCount)
  ) %>% 
  left_join(
    y = nm
  ) %>% 
  relocate(
    channel_name, 
    .after = channel_title
  ) %>% 
  relocate(
    usia_channel, 
    .after = published_at
  ) %>% 
  select(
    -published_at
  ) %>% 
  inner_join(
    y = influencer_tbl, 
    by = c("channel_id" = "channel", "channel_title" = "nama")
  ) %>% 
  select(
    channel_id,
    channel_title,
    channel_name,
    join_date, 
    usia_channel, 
    viewCount, 
    subscriberCount, 
    videoCount:avg_view, 
    ranking,
    penghasilan_bulanan_rendah,
    penghasilan_bulanan_tinggi,
    penghasilan_pervideo,
    paparan
  ) %>% 
  rename(
    published_at = join_date, 
    global_ranking = ranking
  ) %>% 
  mutate(
    penghasilan_bulanan = (penghasilan_bulanan_rendah + penghasilan_bulanan_tinggi)/2, 
    .after = penghasilan_bulanan_tinggi
  ) %>% 
  saveRDS(
    file = "data-raw/channels.rds"
  )
# glimpse()
