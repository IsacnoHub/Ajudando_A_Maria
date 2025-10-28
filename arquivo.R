# =========================================================
# Análise e Gráficos - Base: cancer-risk-factors.csv
# Autor: Isac Azevedo
# =========================================================

# Limpa o ambiente
rm(list = ls())

# Caminho do arquivo (ajuste se necessário)
dados <- read.csv("cancer-risk-factors.csv", sep = ";", header = TRUE)

# =========================================================
# 1️⃣ Estatísticas descritivas
# =========================================================
cat("Média da Idade:", mean(dados$Age, na.rm = TRUE), "\n")
cat("Média do IMC:", mean(dados$BMI, na.rm = TRUE), "\n")
cat("Média do Tabagismo:", mean(dados$Smoking, na.rm = TRUE), "\n")
cat("Média do Uso de Álcool:", mean(dados$Alcohol_Use, na.rm = TRUE), "\n")
cat("Média da Dieta de Carne Vermelha:", mean(dados$Diet_Red_Meat, na.rm = TRUE), "\n")
cat("Média da Dieta com Sal e Processados:", mean(dados$Diet_Salted_Processed, na.rm = TRUE), "\n")

# Correlações principais
cat("\nCorrelação (IMC vs Tabagismo):", cor(dados$BMI, dados$Smoking, use = "complete.obs"), "\n")
cat("Correlação (Álcool vs Carne Vermelha):", cor(dados$Alcohol_Use, dados$Diet_Red_Meat, use = "complete.obs"), "\n")

# =========================================================
# 2️⃣ Gráficos
# =========================================================

# HISTOGRAMA - Idade
hist(dados$Age,
     main = "Histograma - Idade - Isac Azevedo",
     xlab = "Idade (anos)",
     ylab = "Frequência",
     col = "skyblue",
     border = "white")

# GRÁFICO DE BARRAS - Nível de Risco
barplot(table(dados$Risk_level),
        main = "Gráfico de Barras - Nível de Risco - Isac Azevedo",
        xlab = "Nível de Risco (0=Baixo, 2=Médio, 3=Alto)",
        ylab = "Quantidade de Indivíduos",
        col = c("lightgreen", "gold", "tomato"))

# BOXPlot - IMC por Nível de Risco
boxplot(BMI ~ Risk_level, data = dados,
        main = "Boxplot - IMC por Nível de Risco - Isac Azevedo",
        xlab = "Nível de Risco",
        ylab = "Índice de Massa Corporal (IMC)",
        col = c("lightblue", "orange", "red"))

# DISPERSÃO - Idade vs IMC
plot(dados$Age, dados$BMI,
     main = "Dispersão - Idade vs IMC - Isac Azevedo",
     xlab = "Idade (anos)",
     ylab = "IMC",
     pch = 19,
     col = "purple")

# GRÁFICO DE BARRAS - Gênero
barplot(table(dados$Gender),
        main = "Gráfico de Barras - Gênero - Isac Azevedo",
        xlab = "Gênero",
        ylab = "Quantidade de Indivíduos",
        col = c("pink", "lightblue"))

# GRÁFICO DE BARRAS EMPILHADO - Risco por Gênero
tab_genero_risco <- table(dados$Gender, dados$Risk_level)
barplot(tab_genero_risco,
        main = "Gráfico de Barras - Risco por Gênero - Isac Azevedo",
        xlab = "Nível de Risco",
        ylab = "Quantidade de Indivíduos",
        col = c("pink", "lightblue"),
        legend = rownames(tab_genero_risco))

# =========================================================
# Fim do script
# =========================================================

