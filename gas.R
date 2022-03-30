if(!require(data.table))
  install.packages("data.table")
library(data.table)

if(!require(readr))
  install.packages("readr")
library(readr)

if(!require(dplyr))
  install.packages("dplyr")
library(dplyr)

if(!require(ggplot2))
  install.packages("ggplot2")
library(ggplot2)

if(!require(qqplotr))
  install.packages("qqplotr")
library(qqplotr)


if(!require(ggpubr))
  install.packages("ggpubr")
library(ggpubr)

if(!require(rstatix))
  install.packages("rstatix") 
library(rstatix)   


if(!require(sqldf))
  install.packages("sqldf")
library(sqldf)


project_path = 'C://Users//joaov//Documents//Programming//R//r-cotation-gas'

file_full_path = paste(project_path, "//ca-2021-02-1.csv", sep = "")

data <- read.csv(file_full_path, sep=";")


col_names = c(
  "RegiaoSigla",
  "EstadoSigla",
  "Municipio",
  "Revenda",
  "CNPJDaRevenda",
  "NomeDaRua",
  "NumeroRua",
  "Complemento",
  "Bairro",
  "Cep",
  "Produto",
  "DataDaColeta",
  "ValorDeVenda",
  "ValorDeCompra",
  "UnidadeDeMedida",
  "Bandeira"
)


setnames(data, col_names)

csv_clean <- data %>% 
  select(
    "RegiaoSigla", 
    "EstadoSigla", 
    "Produto", 
    "Revenda", 
    "UnidadeDeMedida", 
    "Bandeira")

percent_type_gas = prop.table(table(csv_clean$Produto))*100

col_names_type_gas = c(
  "Diesel",
  "Diesel S10",
  "Etanol",
  "Gasolina",
  "Aditivada",
  "GNV")

col_type_gas_color = c(
  "red",
  "orange", 
  "yellow", 
  "green", 
  "blue", 
  "violet")

barplot(percent_type_gas, 
        ylab = "Porcentagem",
        xlab = "Tipo de combustível",
        names.arg = col_names_type_gas,
        col = col_type_gas_color,
        main = "Porcentagem de combustíveis mais vendidos")
legend("topright",
       legend = col_names_type_gas,
       fill = col_type_gas_color,
       cex = 0.65
)

gp_band <- aggregate(csv_clean$Bandeira, by=list(csv_clean$Bandeira), FUN=length)


col_names = c(
  "Distribuidora",
  "Quantidade")

setnames(gp_band, col_names)

top_10_sellers <- tail(gp_band[order(gp_band$Quantidade,decreasing=FALSE),],10)

ggplot(
  top_10_sellers, 
  aes(x=Quantidade, y=Distribuidora)) + geom_point() + 
  labs(
    title="Comparação TOP 10 maiores distribuidoras", 
    x="Distribuidora", 
    y="Número de Unidades") 



gp_est <- aggregate(csv_clean$Bandeira, by=list(csv_clean$EstadoSigla), FUN=length)

col_names_est = c(
  "Estado",
  "Quantidade")

setnames(gp_est, col_names_est)

top_10_states <- tail(gp_est[order(gp_est$Quantidade,decreasing=FALSE),],10)

ggplot(
  top_10_states, 
  aes(x=Quantidade, y=Estado)) + geom_point() + 
  labs(
    title="Comparação TOP 10 estados com mais postos", 
    x="Quantidade de postos", 
    y="Unidades federativas") 


