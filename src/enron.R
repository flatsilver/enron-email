library(tidyverse)
library(lubridate)
library(igraph)
library(tidygraph)
library(ggraph)

# 大元のデータは以下サイトからダウンロードすることができます
# https://www.cs.cmu.edu/~./enron/
# ですが、非常に重いのでKaggleからダウンロードするのがベターです
# https://www.kaggle.com/wcukierski/enron-email-dataset

# dataディレクトリのcsvを読み込む
enron <- read_csv("data/emails.csv")

# データの確認
head(enron)

# parse -------------------------------------------------------------------

# headerとbodyを分割する位置を特定
breaks <- str_locate(enron$message, "\n\n")

# headerとbodyに分割
header <- str_sub(enron$message, end = breaks[, 1] - 1)
body <- str_sub(enron$message, start = breaks[, 2] + 1)

# headerをそれぞれの要素に分割するfunctionを定義
parse_header <- function(header) {
  message_id <- str_sub(str_extract(header, "Message-ID:.*"), 12)
  date <- str_sub(str_extract(header, "Date:.*"), 7)
  from <- str_sub(str_extract(header, "From:.*"), 7)
  to <- str_sub(str_extract(header, "To:.*"), 5)
  subject <- str_sub(str_extract(header, "Subject:.*"), 10)
  x_cc <- str_sub(str_extract(header,'X\\-cc:.*'), 7)
  x_bcc <- str_sub(str_extract(header,'X\\-bcc:.*'), 8)
  
  parsed_header <- tibble(message_id, date, from, to, subject, x_cc, x_bcc)
  return(parsed_header)
}

# parseしたheaderを取得
parsed_header <- parse_header(header)

# parse date
parsed_header <- parsed_header %>%
  mutate(date = parse_date_time(parsed_header$date,
                                "%a, %d %m %Y %H:%M:%S %z",
                                locale = "C"))

# split file
file_split <- as_tibble(str_split(enron$file, "/", simplify = TRUE))

# parseしたデータを結合
enron <- bind_cols(file_split, parsed_header) %>% 
  mutate(body = body)

# データの絞り込み ----------------------------------------------------------------

# 2000/6/1 ~ 2001/5/31の期間に絞り込む
enron_filtered <- enron %>% 
  filter(between(
    date,
    ymd_hms("2000-06-01-00-00-00"),
    ymd_hms("2001-05-31-23-59-59")
  )) %>% 
  mutate(
    year = year(date),
    month = month(date)
  )

# 12ヶ月間毎月メールを送信している社員のvectorを取得
full_emp <- enron_filtered %>% 
  group_by(from, year, month) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(from) %>% 
  count() %>% 
  filter(n >= 12) %>% 
  pull(from)

# emailデータを12ヶ月間メールを送信している社員 AND "@enron.com"に絞り込む
full_month_appear_emp <- enron_filtered %>%
  filter(from %in% full_emp &
           str_detect(from, "@enron.com") & str_detect(to, "@enron.com")) %>%
  select(date, from, to, year, month)

# 6月のデータのみに絞り、toカラムに2人以上送信相手がいる行をseparate
enron_june <- full_month_appear_emp %>% 
  filter(month == 6 & to != "") %>% 
  separate_rows(to, sep = ",") %>% 
  mutate(to = str_trim(to))  # ""の削除

enron_june_from <- enron_june %>% 
  pull(from) %>% 
  unique()

# 辺リストの作成
enron_edge_list <- enron_june %>% 
  filter(to %in% enron_june_from) %>% 
  group_by(from, to) %>% 
  count()

# 辺リストの確認
head(enron_edge_list)

# 人数の確認
enron_edge_list %>% 
  pull(from) %>% 
  unique() %>% 
  length()

# tbl_graphオブジェクトの作成 ------------------------------------------------------

# tbl_graphオブジェクトへ変換
g <- as_tbl_graph(enron_edge_list, directed = FALSE)

g

# グラフが連結しているか確認
g %>% is.connected()

# 連結しているかグラフを描画して確認
g %>% 
  ggraph(layout = "kk") +
  geom_edge_arc(alpha = 0.8, colour = "lightgray") +
  geom_node_point(size = 3, colour = "steelblue") +
  theme_graph()

# 連結したグラフのみに絞る
g_conected <- g %>% 
  mutate(component = group_components()) %>% 
  filter(component == 1)

# グラフが連結しているか確認
g_conected %>% is.connected()

# 密度
g_conected %>% graph.density()

# 推移性
g_conected %>% transitivity()

# 次数, 固有ベクトル中心性, 媒介中心性の計算
g_conected <- g_conected %>% 
  mutate(
    degree = degree(g_conected),
    eigen_centrality = eigen_centrality(g_conected)$vector,
    betweenness = betweenness(g_conected)
  )

g_conected

# コミュニティの検出
# どうしても検出したい場合は、一度多重グラフを取り除く必要がある
g_conected %>% 
  morph(to_simple) %>% 
  mutate(community = as.factor(group_fast_greedy())) %>% 
  unmorph()

# ネットワーク図の描画 --------------------------------------------------------------

# コミュニティ検出以外実施
g_conected %>% 
  mutate(name = str_remove(name, "@enron.com")) %>% 
  ggraph(layout = "kk") +
  geom_edge_arc(aes(width = n), alpha = 0.8, colour = "lightgray") +
  scale_edge_width(range = c(0.5, 1)) +
  geom_node_point(aes(size = degree), colour = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 1.2) +
  theme_graph()

# コミュニティの検出
g_conected %>% 
  mutate(name = str_remove(name, "@enron.com")) %>%  
  morph(to_simple) %>% 
  mutate(community = as.factor(group_fast_greedy())) %>% 
  unmorph() %>% 
  ggraph(layout = "kk") +
  geom_edge_arc(aes(width = n), alpha = 0.8, colour = "lightgray") +
  scale_edge_width(range = c(0.5, 1)) +
  geom_node_point(aes(size = degree, colour = community)) +
  geom_node_text(aes(label = name, colour = community),
                 repel = TRUE, size = 1.2) +
  theme_graph() +
  theme(legend.position = "none")

# 固有ベクトル中心性をnode sizeに使ったversion
g_conected %>% 
  mutate(name = str_remove(name, "@enron.com")) %>%  
  morph(to_simple) %>% 
  mutate(community = as.factor(group_fast_greedy())) %>% 
  unmorph() %>% 
  ggraph(layout = "kk") +
  geom_edge_arc(aes(width = n), alpha = 0.8, colour = "lightgray") +
  scale_edge_width(range = c(0.5, 1)) +
  geom_node_point(aes(size = eigen_centrality, colour = community)) +
  geom_node_text(aes(label = name, colour = community),
                 repel = TRUE, size = 1.2) +
  theme_graph() +
  theme(legend.position = "none")

# 媒介中心性をnode sizeに使ったversion
g_conected %>% 
  mutate(name = str_remove(name, "@enron.com")) %>%  
  morph(to_simple) %>% 
  mutate(community = as.factor(group_fast_greedy())) %>% 
  unmorph() %>% 
  ggraph(layout = "kk") +
  geom_edge_arc(aes(width = n), alpha = 0.8, colour = "lightgray") +
  scale_edge_width(range = c(0.5, 1)) +
  geom_node_point(aes(size = betweenness, colour = community)) +
  geom_node_text(aes(label = name, colour = community),
                 repel = TRUE, size = 1.2) +
  theme_graph() +
  theme(legend.position = "none")
