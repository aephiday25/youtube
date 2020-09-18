info_detail <- function(channel){
  detail <- read_html(paste0("https://id.noxinfluencer.com/youtube/channel/", channel))
  
  nama <- detail %>% 
    html_nodes("section.info-block") %>% 
    html_nodes("div.title") %>% 
    html_text()
  
  info <- detail %>% 
    html_nodes("div.info-item") %>% 
    html_nodes("p.item-value") %>% 
    html_text() %>% 
    .[1:3] %>% 
    str_trim()
  
  base_info <- tibble(nama = nama,
                      join_date = info[1],
                      area = info[2],
                      bahasa = info[3])
  
  content_info <- detail %>% 
    html_nodes("div.channel-content-item") %>% 
    html_nodes("ul.base-data") %>% 
    html_nodes("li.base-data-item") %>% 
    html_nodes("div.value-content") %>% 
    html_nodes("span") %>% 
    html_text()
  
  if(length(content_info) == 6){
    content_info <- content_info[c(2,6)]
  } else {
    content_info <- content_info[c(3,7)]
  }
  
  content_info <- tibble(total_ditonton = case_when(str_detect(content_info[1], "M") ~ as.numeric(str_remove_all(content_info[1], "M"))*10^9, 
                                                    str_detect(content_info[1], "JT") ~ as.numeric(str_remove_all(content_info[1], "JT"))*10^6,
                                                    str_detect(content_info[1], "RB") ~ as.numeric(str_remove_all(content_info[1], "RB"))*10^3,
                                                    TRUE ~ as.numeric(content_info[1])),
                         total_video = case_when(str_detect(content_info[2], "M") ~ as.numeric(str_remove_all(content_info[2], "M"))*10^9, 
                                                 str_detect(content_info[2], "JT") ~ as.numeric(str_remove_all(content_info[2], "JT"))*10^6,
                                                 str_detect(content_info[2], "RB") ~ as.numeric(str_remove_all(content_info[2], "RB"))*10^3,
                                                 TRUE ~ as.numeric(content_info[2])))
  ranking <- tibble(ranking = detail %>% 
                      html_nodes("div.rank") %>% 
                      html_nodes("a") %>% 
                      html_text() %>% .[1] %>% 
                      str_extract("[0-9]+") %>% 
                      as.numeric())
  
  perkiraan_penghasilan <- detail %>% 
    html_nodes("div.card-item") %>% 
    html_nodes("div.est-content") %>% 
    html_text() %>% 
    str_remove_all("Rp ") %>% 
    str_remove_all("\\(Setiap  Video\\)") %>% 
    str_trim()
  
  perkiraan2 <- perkiraan_penghasilan[2] %>% 
    str_split("-") %>% 
    unlist()
  perkiraan2 <- case_when(str_detect(perkiraan2, "M") ~ as.numeric(str_remove_all(perkiraan2, "M"))*10^9, 
                          str_detect(perkiraan2, "JT") ~ as.numeric(str_remove_all(perkiraan2, "JT"))*10^6,
                          str_detect(perkiraan2, "RB") ~ as.numeric(str_remove_all(perkiraan2, "RB"))*10^3,
                          TRUE ~ as.numeric(perkiraan2))
  
  perkiraan_penghasilan <- tibble(penghasilan_bulanan_rendah = perkiraan2[1],
                                  penghasilan_bulanan_tinggi = perkiraan2[2]) %>% 
    mutate(penghasilan_pervideo = case_when(str_detect(perkiraan_penghasilan[1], "M") ~ as.numeric(str_remove_all(perkiraan_penghasilan[1], "M"))*10^9, 
                                            str_detect(perkiraan_penghasilan[1], "JT") ~ as.numeric(str_remove_all(perkiraan_penghasilan[1], "JT"))*10^6,
                                            str_detect(perkiraan_penghasilan[1], "RB") ~ as.numeric(str_remove_all(perkiraan_penghasilan[1], "RB"))*10^3,
                                            TRUE ~ as.numeric(perkiraan_penghasilan[1])))
  
  paparan <- detail %>% 
    html_nodes("div.card-item") %>% 
    html_nodes("div.card-footer") %>% 
    html_nodes("span.cpm") %>% 
    html_text() %>% .[2] %>% 
    str_trim()
  
    
  
  paparan <- tibble(paparan = case_when(str_detect(paparan, "M") ~ as.numeric(str_remove_all(paparan, "M"))*10^9, 
                                        str_detect(paparan, "JT") ~ as.numeric(str_remove_all(paparan, "JT"))*10^6,
                                        str_detect(paparan, "RB") ~ as.numeric(str_remove_all(paparan, "RB"))*10^3,
                                        TRUE ~ as.numeric(paparan)))
  
  bind_cols(tibble(channel = channel), base_info, content_info, ranking, perkiraan_penghasilan, paparan)
}
