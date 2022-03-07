
#1.1
seminario <- data.frame(
  tipo = c("engenheiros", "professores", "analistas", "alunos"),
  participantes = c(32, 20, 16, 12)
)
view(seminario)


#1.2
Desc(seminario$participantes)
summary(seminario$participantes)

#2.1
library(readxl)
obesidade <- read_excel("C:/Users/nunor/Desktop/ME/Labs/1/obesidade.xlsx")
View(obesidade)