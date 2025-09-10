# Cargar librería
library(dplyr)

# Leer CSV
datos <- read.csv("D:/X SEMESTE/EST - ESPACIAL/superficie_puno.csv")

# Filtrar solo Puno
puno <- datos %>%
  filter(REGION == "PUNO")

# Agrupar por distrito y sumar superficie
puno_distritos <- puno %>%
  group_by(NOMBREDI) %>%
  summarise(superficie_total = sum(RESFIN, na.rm = TRUE),
            productores = sum(FACTOR_PRODUCTOR, na.rm = TRUE),
            parcelas = n())

# Ver resultados
print(puno_distritos)
