##CORTE  TRANSVERSAL
### COLETAR DADOS DE DUAS VARIÁVEIS
#PACOTES####
#install.packages('WDI') #PRIMEIRO A EXECUTAR
library(WDI) # CARREGAR O PACOTE
library(ggplot2)
library(scales)    # Para formatar os eixos
library(ggrepel)
##TIRAR O "e" do numero final
options(scipen = 9)


#VARIAVEIS ####
# Importações de bens e serviços = NE.IMP.GNFS.CD
# Exportações de bens e serviços =  NE.EXP.GNFS.CD
variaveis <- c("IMP" = 'NE.IMP.GNFS.CD', "EXP" = 'NE.EXP.GNFS.CD')

### FAZER UM GRAFICO DE DISPERÇÃO####
#### UMA VARIÁVEL EM CADA EIXO (Y E X)
# Baixar os dados do WDI
# Baixar os dados do WDI
dados <- WDI(country = "all", 
             indicator = c("IMP" = "NE.IMP.GNFS.CD",  # Importações
                           "EXP" = "NE.EXP.GNFS.CD"),  # Exportações
             start = 2023, 
             end = 2023)

# Filtrar para remover agregados (como "World", "European Union", etc.)
dados <- dados[!grepl("World|Union|Region|Developing|Developed|OECD|Euro|High income|Low income|Middle income", dados$country, ignore.case = TRUE), ]

# Filtrar para remover os pontos mais distantes (outliers)
dados <- dados[!(dados$EXP >= 5.5e12 & dados$EXP <= 9.5e12 & dados$IMP >= 4.5e12 & dados$IMP <= 8.5e12), ]
dados <- dados[!(dados$EXP >= 12e12 & dados$IMP >= 12e12), ]

# Função para formatar os valores em bilhões
formato_bilhoes <- function(x) {
  paste0(x / 1e9, " bi")
}

# Lista de países principais para destacar
paises_principais <- c("Brazil", "United States", "China", "India", "Germany", "Japan", "Italy", "France")

# Criar uma coluna para indicar se o país é um dos principais (para colorir em vermelho)
dados$highlight <- ifelse(dados$country %in% paises_principais, "Principal", "Outros")

# Criar uma coluna para destacar o Brasil com um tamanho maior
dados$size <- ifelse(dados$country == "Brazil", 4, 2)  # Tamanho maior para o Brasil

# Filtrar os dados para incluir apenas os países principais para os rótulos
dados_rotulos <- dados[dados$country %in% paises_principais, ]

# Criar o gráfico de dispersão
ggplot(dados, aes(x = EXP, y = IMP)) +
  # Plotar os pontos com cores e tamanhos diferentes
  geom_point(aes(color = highlight, size = size)) +
  scale_color_manual(values = c("Principal" = "red", "Outros" = "black")) +
  scale_size_identity() +  # Usar os valores de size diretamente
  # Adicionar rótulos para os países principais com ajustes
  geom_text_repel(data = dados_rotulos, 
                  aes(label = country), 
                  size = 3.5,              # Tamanho do texto
                  box.padding = 2,         # Distância entre o rótulo e o ponto
                  point.padding = 1,       # Distância entre o ponto e o início da linha do rótulo
                  force = 10,              # Forçar os rótulos a se afastarem
                  force_pull = 0.5,        # Ajustar a força de atração para o ponto
                  nudge_y = ifelse(dados_rotulos$country == "Brazil", 2e11, 0),  # Mover o rótulo do Brasil para cima
                  max.overlaps = Inf,      # Permitir que todos os rótulos sejam exibidos
                  segment.color = "grey50",# Cor da linha que conecta o rótulo ao ponto
                  segment.size = 0.5) +    # Espessura da linha
  # Formatar os eixos com a função personalizada
  scale_x_continuous(labels = formato_bilhoes, 
                     breaks = seq(0, max(dados$EXP, na.rm = TRUE), by = 5e11)) +
  scale_y_continuous(labels = formato_bilhoes, 
                     breaks = seq(0, max(dados$IMP, na.rm = TRUE), by = 5e11)) +
  labs(title = "Dispersão entre Exportações e Importações (2023)",  # Adicionar o ano no título
       x = "Exportações (US$)",
       y = "Importações (US$)") +
  theme_minimal() +
  theme(legend.position = "none",  # Remover a legenda
        axis.title.x = element_text(margin = margin(t = 15)),  # Afastar o rótulo do eixo x
        axis.title.y = element_text(margin = margin(r = 15)))  # Afastar o rótulo do eixo y

## FAZER UM GRAFICO TEMPORAL
# Baixar os dados do WDI para acesso à internet no Japão
# Carregar os pacotes necessários
library(WDI)
library(ggplot2)
library(scales)
library(gganimate)

install.packages('gganimate') 

# Baixar os dados do WDI para acesso à internet no Japão
dados <- WDI(country = "JP",  # Código do Japão
             indicator = "IT.NET.USER.ZS",  # % da população com acesso à internet
             start = 1986, 
             end = 2022)

# Criar o gráfico de série temporal com animação
p <- ggplot(dados, aes(x = year, y = IT.NET.USER.ZS)) +
  geom_line(color = "#C3B1E1") +  # Linha em tom pastel (lilás suave)
  geom_point(color = "#C3B1E1", size = 2) +  # Pontos no mesmo tom pastel
  labs(title = "🌐 Evolução do Acesso à Internet no Japão (1986-2022) 🌐",
       x = "Ano",
       y = "% da População") +
  scale_y_continuous(labels = percent_format(scale = 1),  # Formatar o eixo y como percentual
                     breaks = seq(0, 100, by = 5)) +  # Mostrar de 5 em 5%
  scale_x_continuous(breaks = seq(1986, 2022, by = 4)) +  # Ajustar os anos de 4 em 4
  theme_minimal() +
  theme(axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        plot.title = element_text(size = 16, 
                                  face = "bold", 
                                  color = "#4B0082", 
                                  hjust = 0.5, 
                                  margin = margin(b = 15))) +
  # Adicionar a animação
  transition_reveal(year)  # A linha será revelada gradualmente ao longo dos anos

# Renderizar a animação
animate(p, fps = 20, duration = 10, width = 800, height = 600, renderer = gifski_renderer())

# Salvar a animação como GIF
anim_save("acesso_internet_japao.gif")
anim_save("C:/Users/arthu/OneDrive/Área de Trabalho/Faculdade/5° Semestre/Econometria I/acesso_internet_japao.gif")