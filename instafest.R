# Librerías ---------------------------------------------------------------

library(spotifyr)
library(tidyverse)
library(tibble)
library(tidymodels)
library(gt)
library(broom)
library(ggimage)
library(ggwordcloud)
library(showtext)
library(rjson)
font_add_google(name = "Roboto", family = "custom_font")

## Automatically use showtext to render text
# showtext_auto()

# Variables de entorno ----------------------------------------------------

credentials <- fromJSON(file = "credentials.json")

Sys.setenv(SPOTIFY_CLIENT_ID = credentials$SPOTIFY_CLIENT_ID)
Sys.setenv(SPOTIFY_CLIENT_SECRET = credentials$SPOTIFY_CLIENT_SECRET)


# Autenticación spotify ---------------------------------------------------
access_token <- get_spotify_access_token(
  client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
  client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")
)


# Data --------------------------------------------------------------------
df <- get_my_top_artists_or_tracks(type = 'artists',
                                   time_range = 'medium_term',
                                   limit = 50)


# Visualización de los datos ----------------------------------------------
df %>%
  select(name, genres) %>%
  rowwise() %>%
  mutate(genres = paste(genres, collapse = ', ')) %>%
  ungroup %>%
  gt()


# Data para clustering ----------------------------------------------------

df_clusters <- df %>%
  select(artist = name, genres) %>%
  unnest_longer(genres) %>%
  mutate(value = 1) %>%
  pivot_wider(id_cols = artist,
              names_from = genres,
              values_from = value) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, 1))) %>%
  left_join(df %>% select(followers.total, artist = name) %>%
              rownames_to_column('relevance')) %>%
  janitor::clean_names()



# PCA ---------------------------------------------------------------------

preproc <- recipe( ~ ., data = df_clusters) %>%
  
  update_role(artist, new_role = 'id') %>%
  
  step_rm(followers_total, relevance) %>%
  
  step_nzv(all_numeric()) %>%
  
  step_pca(all_numeric(), num_comp = 2)


pca_artists <- preproc %>%
  
  prep() %>%
  
  juice()


# Clusters ----------------------------------------------------------------


set.seed(42)
artists_clusters <- df_clusters %>%
  
  select(-followers_total,-relevance) %>%
  
  column_to_rownames('artist') %>%
  
  kmeans(centers = 3)


tidy(artists_clusters)


df_final <- augment(artists_clusters, df_clusters) %>%
  left_join(pca_artists, by = 'artist')


df_final %>%
  ggplot(aes(x = PC1, y = PC2, color = .cluster)) +
  geom_point()

# Flyer -------------------------------------------------------------------

p1 <- df_final %>% select(artist, .cluster, relevance) %>%
  mutate(relevance = as.integer(relevance),
         dia = paste0('Día ', .cluster)) %>%
  ggplot(aes(label = artist, size = -relevance)) +
  geom_text_wordcloud(color = 'white') +
  facet_wrap( ~ dia, scales = 'free_y', nrow = 3) +
  theme_void() +
  theme(
    plot.margin = unit(c(5, 0, 10, 0), "cm"),
    strip.text = element_text(colour = 'white', size=30)
    # text = element_text(family = 'sans', size=40)
  )

p1

p2 <- ggbackground(p1, background='instafest_blank.png')
p2

ggsave('instafest2.png', plot=p2, height=10, width=10, scale=1)


