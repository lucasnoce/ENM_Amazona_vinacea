library(ggplot2)
library(gridExtra)


# Extrair as curvas de resposta do modelo
# response_curve_bio <- getResponseCurve(modelo_vif)

# Verificar a estrutura dos dados das curvas de resposta
str(response_curve_bio) # Isso ajudará a entender como os dados estão organizados


# Define as variáveis e suas unidades de medida
variables_units <- list(
  "wc2.1_30s_bio_9" = "°C",    # Ajuste conforme necessário
  "consensus_full_class_2" = "%",    # Ajuste conforme necessário
  "declividade_br" = "graus"
)

# Função para gerar o gráfico com unidade de medida
plot_response_curve <- function(variable_name, unit) {
  # Extrair o data frame da variável específica
  curve_data <- as.data.frame(response_curve_bio@response[[variable_name]])
  
  # Renomear a primeira coluna para 'Valor' e selecionar a coluna do modelo desejado, por exemplo, 'maxent_ID-1'
  colnames(curve_data)[1] <- "Valor"
  
  # Criar o gráfico
  ggplot(curve_data, aes(x = Valor, y = `maxent_ID-1`)) + # Ajuste 'maxent_ID-1' conforme necessário
    geom_line(color = "blue") +
    labs(
      title = paste(variable_name),
      x = paste(variable_name, "(", unit, ")", sep = ""),  # Unidade de medida no eixo x
      y = "Probabilidade de Presença"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold"),  # Título maior
      axis.title = element_text(size = 8),                # Títulos dos eixos
      axis.text = element_text(size = 8),                 # Texto dos eixos
      legend.text = element_text(size = 10),               # Texto da legenda (se houver)
      legend.title = element_text(size = 12)               # Título da legenda
    )
}

# Gera e exibe todos os gráficos para cada variável e unidade
lapply(names(variables_units), function(var) {
  plot_response_curve(var, variables_units[[var]])
})


# Gera todos os gráficos
plots <- lapply(names(variables_units), function(var) {
  plot_response_curve(var, variables_units[[var]])
})

# Organiza os gráficos lado a lado
do.call(grid.arrange, c(plots, ncol = 3))  # Ajuste ncol para o número de colunas desejado

