# Questão A
library(pacman)
p_load(dplyr, tidyverse, googleway, ggmap)

#chave não funciona mais
chave<- "AIzaSyCZNA_A-6P3IwrmcdrbU9sX1KURDEpJ3xM"
register_google(key = chave)

inicio <- google_places(
  search_string = "Delegacias do DF",
  key = chave, 
  page_token = NULL,
  language = "pt-BR")

resultados <- inicio$results
Sys.sleep(2)

for(x in 1:30) {
  del <- google_places(
    search_string = "Delegacias do DF",
    key = chave, 
    page_token = inicio$next_page_token,
    language = "pt-BR")
  
  
  dados <- del$results
  
  resultados <- bind_rows(resultados, dados)
  
  # Check for next page token
  if (!is.null(del$next_page_token)) {
    page_token <- del$next_page_token
    Sys.sleep(2) # Wait a bit before making the next request
  } else {
    break
  }
}
save(resultados, file = "del.Rda")

# Questão B
load("infra.Rda")

direcao <- google_directions(origin = banco[905,11:12],
                             destination = "Departamento de Estatística da UnB, Brasília, Brasil",
                             key = chave,
                             mode = "driving") 

pontos <- banco[905,11:12] %>%
  mutate(lat = auinf_local_latitude, lon = auinf_local_longitude) %>%
  select(lon, lat) %>%
  bind_rows(geocode("Departamento de Estatística da UnB, Brasília, Brasil")) %>%
  mutate(label = c("905", "CIC/EST"), popup = c("Infração número 905", "Departamento de Estatística da UnB"))

rota <- decode_pl(direcao$routes$overview_polyline$points)

save(rota, file = "rota.Rda")
save(pontos, file = "ponto.Rda")
