## EXERCICIO AULA DE SEXTA FEIRA 28/02 ##

#PACOTES####
#install.packages('WDI') #PRIMEIRO A EXECUTAR
library(WDI) # CARREGAR O PACOTE
# library(tidyverse)

#VARIAVEIS ####
# Importações de bens e serviços = NE.IMP.GNFS.CD
# Exportações de bens e serviços =  NE.EXP.GNFS.CD

variaveis <- c("IMP" = 'NE.IMP.GNFS.CD', "EXP" = 'NE.EXP.GNFS.CD')

#base de dados de cortes transversal####
dados <- WDI(indicator = variaveis, 
             country= 'all', #todos = all
             start = 2023, end = 2023) 

#base completa do Brasil####
dadosBR_Com <- WDI(indicator = variaveis, 
                   country= 'BR', #codigo Iso2c
                   start = , end = )
#graficos base de todos os países####

modelo_ct <- lm(EXP ~ IMP, data = dados) #faz a regressão
summary(modelo_ct) #visualizar os resultados

modelo_ct_br <- lm(EXP ~ IMP, data = dadosBR_Com)
summary(modelo_ct_br)

head(dados)

#grafico importacao paises em 2023
# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Assumindo que 'dados' já está carregado
# Criar um mapeamento para traduzir os nomes dos países de inglês para português
traduzir_paises <- c(
  "United States" = "Estados Unidos",
  "China" = "China",
  "Germany" = "Alemanha",
  "United Kingdom" = "Reino Unido",
  "France" = "França",
  "Netherlands" = "Países Baixos",
  "Japan" = "Japão",
  "India" = "Índia",
  "Italy" = "Itália",
  "Canada" = "Canadá",
  "Brazil" = "Brasil",
  "Australia" = "Austrália",
  "Argentina" = "Argentina",
  "Spain" = "Espanha",
  "Switzerland" = "Suíça",
  "Turkey" = "Turquia",
  "South Africa" = "África do Sul",
  "Indonesia" = "Indonésia",
  "Saudi Arabia" = "Arábia Saudita",
  "Nigeria" = "Nigéria",
  "Egypt" = "Egito",
  "Pakistan" = "Paquistão",
  "Thailand" = "Tailândia",
  "Vietnam" = "Vietnã",
  "Malaysia" = "Malásia",
  "Philippines" = "Filipinas",
  "Poland" = "Polônia",
  "South Korea" = "Coreia do Sul",
  "Mexico" = "México",
  "Russia" = "Rússia"
)

# Lista de países soberanos em português
paises_soberanos <- c("Brasil", "Estados Unidos", "Canadá", "China", "Alemanha", "Japão", "Índia",
                      "Reino Unido", "França", "Itália", "Rússia", "Coreia do Sul", "México",
                      "Austrália", "Argentina", "Espanha", "Países Baixos", "Suíça", "Turquia",
                      "África do Sul", "Indonésia", "Arábia Saudita", "Nigéria", "Egito", "Paquistão",
                      "Tailândia", "Vietnã", "Malásia", "Filipinas", "Polônia")

# Traduzir os nomes dos países no dataframe e selecionar os 10 principais
dados_top10 <- dados %>%
  mutate(country = recode(country, !!!traduzir_paises)) %>% # Traduzir os nomes
  mutate(total_trade = IMP + EXP) %>%
  # Filtrar apenas os países da lista soberana
  filter(country %in% paises_soberanos) %>%
  arrange(desc(total_trade)) %>%
  slice_head(n = 10) %>%
  select(country, IMP, EXP) %>%
  pivot_longer(cols = c("IMP", "EXP"), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value))

# Criar o gráfico simplificado
ggplot(dados_top10, aes(x = reorder(country, value), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) + # Barras mais estreitas
  coord_flip() + # Manter horizontal
  scale_y_continuous(labels = dollar_format(scale = 1e-9, suffix = "B"), # Simplificar para bilhões
                     expand = c(0, 0), # Remover espaço extra no eixo
                     limits = c(0, max(dados_top10$value, na.rm = TRUE) * 1.1)) + # Ajustar limite superior
  labs(title = "Top 10 Países em Comércio (2023)",
       subtitle = "Em bilhões de dólares",
       x = NULL, y = NULL,
       fill = NULL) + # Remover títulos desnecessários
  scale_fill_manual(values = c("IMP" = "#2c7fb8", "EXP" = "#f03b20"), 
                    labels = c("Importações", "Exportações")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(size = 10, hjust = 0, color = "grey50"),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5), # Texto do eixo X mais legível
    axis.text.y = element_text(size = 12),
    legend.position = "top",
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90"), # Grade sutil
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Salvar o gráfico (opcional)
ggsave("grafico_top10_paises_soberanos.png", width = 10, height = 5, dpi = 300)

#graficos base de dados 
# Carregar pacotes necessários
# Carregar pacotes necessários
# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(scales)

# Filtrar dados para remover valores NA
dadosBR_Com <- dadosBR_Com %>%
  filter(!is.na(IMP) & !is.na(EXP))

# Transformar os dados para o formato longo (long format) para o ggplot
dadosBR_Com_long <- dadosBR_Com %>%
  select(year, IMP, EXP) %>%
  pivot_longer(cols = c("IMP", "EXP"), names_to = "Tipo_Comercio", values_to = "Valor") %>%
  filter(!is.na(Valor))

# Criar o gráfico com Importações e Exportações
grafico_balanca <- ggplot(dadosBR_Com_long, aes(x = year, y = Valor, color = Tipo_Comercio)) +
  geom_line(size = 1) + # Linhas para importações e exportações
  geom_point(size = 2) + # Pontos para destacar os anos
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") + # Linha em y=0 para equilíbrio
  scale_y_continuous(labels = dollar_format(scale = 1e-6, suffix = "M"), # Em milhões
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(min(dadosBR_Com$year), max(dadosBR_Com$year), by = 10)) + # Rótulos a cada 10 anos
  scale_color_manual(values = c("IMP" = "#2c7fb8", "EXP" = "#f03b20"),
                     labels = c("Importações", "Exportações")) +
  labs(title = "Balança Comercial do Brasil",
       subtitle = "Importações e Exportações (em milhões de dólares)",
       x = "Ano",
       y = NULL,
       color = "Legenda") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(size = 10, hjust = 0, color = "grey50"),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 12),
    legend.position = "top",
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Exibir o gráfico
print(grafico_balanca)

# Salvar o gráfico (opcional)
ggsave("grafico_balanca_comercial_brasil.png", width = 10, height = 5, dpi = 300)