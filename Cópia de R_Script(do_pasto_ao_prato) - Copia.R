# Limpar o ambiente, console e gráficos
rm(list = ls())  
cat("\014")  
graphics.off()  

# Lista de pacotes necessários
pacotes <- c("tidyverse", "httr", "jsonlite", "sf", "geobr", "fuzzyjoin", "ggplot2", "rnaturalearth", "rnaturalearthdata", "ggspatial")

# Verifica e instala pacotes que ainda não estão instalados
if (length(setdiff(pacotes, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pacotes, rownames(installed.packages())))
}

# Carregar pacotes necessários
lapply(pacotes, library, character.only = TRUE)

# Fechar todas as conexões abertas
closeAllConnections()

# Função para baixar dados dos fornecedores
get_suppliers_by_cnae <- function(cnae_code) {
  url <- paste0("https://compras.dados.gov.br/fornecedores/v1/fornecedores.json?id_cnae=", cnae_code)
  response <- GET(url)
  data <- fromJSON(content(response, "text"), flatten = TRUE)
  
  if ("_embedded" %in% names(data)) {
    suppliers <- data[["_embedded"]][["fornecedores"]]
    return(suppliers)
  } else {
    return(NULL)
  }
}

# CNAEs a serem consultados
cnaes <- c(1011201, 1011205, 1013901, 1013902)

# Baixar dados para cada CNAE
suppliers_list <- lapply(cnaes, get_suppliers_by_cnae)

# Remover listas vazias (caso não haja dados)
suppliers_list <- suppliers_list[!sapply(suppliers_list, is.null)]

# Combinar todos os fornecedores
all_suppliers <- do.call(rbind, suppliers_list)

# Criar um novo dataframe com as informações necessárias
municipios_df <- all_suppliers %>%
  mutate(
    codigo_municipio = str_extract(`_links.municipio.title`, "\\d+"),
    nome_municipio = str_extract(`_links.municipio.title`, ":\\s*(.*)") %>% str_replace_all("[:]", ""),
    uf = uf,
    nome_empresa = nome,
    porte_empresa = case_when(
      id_porte_empresa == 1 ~ "Pequeno",
      id_porte_empresa == 2 ~ "Médio",
      id_porte_empresa == 3 ~ "Grande",
      TRUE ~ "Não especificado"
    )
  ) %>%
  select(codigo_municipio, nome_municipio, uf, nome_empresa, porte_empresa)

# Obter a base de dados dos municípios do Brasil do geobr
municipios_brasil <- read_municipality(year = 2020, simplified = TRUE)

# Remover acentuação dos nomes dos municípios para evitar problemas de comparação
municipios_df <- municipios_df %>%
  mutate(nome_municipio = iconv(nome_municipio, to = "ASCII//TRANSLIT"))

municipios_brasil <- municipios_brasil %>%
  mutate(name_muni = iconv(name_muni, to = "ASCII//TRANSLIT"))

# Fazer a junção aproximada usando o nome do município e UF
municipios_com_coordenadas <- stringdist_left_join(
  municipios_df,
  municipios_brasil,
  by = c("nome_municipio" = "name_muni", "uf" = "abbrev_state"),
  method = "jw",
  max_dist = 0.15,
  distance_col = "distancia"
)

# Manter apenas as correspondências mais próximas
if ("nome_municipio.x" %in% colnames(municipios_com_coordenadas)) {
  municipios_com_coordenadas <- municipios_com_coordenadas %>%
    group_by(nome_municipio.x) %>%
    slice_min(order_by = distancia)
} else {
  warning("Coluna 'nome_municipio.x' não encontrada.")
}

# Contar o número de fornecedores por estado
supplier_count_by_state <- table(municipios_df$uf)
df_supplier_count <- as.data.frame(supplier_count_by_state)
colnames(df_supplier_count) <- c("State", "Supplier_Count")

# Calcular a média de fornecedores por estado
mean_supplier_count <- mean(df_supplier_count$Supplier_Count)

# Gráfico de barras com linha de média
ggplot(df_supplier_count, aes(x = reorder(State, -Supplier_Count), y = Supplier_Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_hline(aes(yintercept = mean_supplier_count), color = "red", linetype = "dashed", linewidth = 1) +
  theme_minimal() +
  labs(title = "Número de Fornecedores de Carne por Estado",
       x = "Estado",
       y = "Número de Fornecedores") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("text", x = Inf, y = mean_supplier_count, label = paste("Média =", round(mean_supplier_count, 2)),
           vjust = -0.5, hjust = 1.1, color = "red")

### Mapa do Brasil com intensidade de fornecedores por estado ###
brazil <- ne_states(country = "Brazil", returnclass = "sf")

# Ajustar os códigos dos estados para correspondência
df_supplier_count$State <- toupper(df_supplier_count$State)

# Juntar os dados de fornecedores com o mapa do Brasil
brazil_data <- merge(brazil, df_supplier_count, by.x = "postal", by.y = "State", all.x = TRUE)

# Substituir NA por 0 para estados sem fornecedores
brazil_data$Supplier_Count[is.na(brazil_data$Supplier_Count)] <- 0

# Gerar o mapa de calor
ggplot(data = brazil_data) +
  geom_sf(aes(fill = Supplier_Count)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "gray90") +
  theme_minimal() +
  labs(title = "Distribuição de Fornecedores de Carne por Estado",
       fill = "Fornecedores",
       caption = "Fonte: API de Compras Governamentais") +
  theme(legend.position = "right") +
  ggspatial::annotation_scale(location = "br", width_hint = 0.5) +  # Escala no canto inferior direito
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering())  # Rosa dos ventos no canto superior direito

# Plotar o mapa com os pontos dos municípios fornecedores
ggplot() +
  geom_sf(data = municipios_brasil, fill = "lightgray", color = "white") +
  geom_sf(data = municipios_com_coordenadas, aes(geometry = geom), color = "red", size = 2) +
  theme_minimal() +
  labs(title = "Distribuição de Municípios Fornecedores de Carne no Brasil",
       caption = "Fonte: API de Compras Governamentais") +
  theme(legend.position = "none") +
  ggspatial::annotation_scale(location = "br", width_hint = 0.5) +  # Escala no canto inferior direito
  ggspatial::annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering())  # Rosa dos ventos no canto superior direito

