#Хаддад некула БМТ2-21М
#Мини-группа 7, номер 7
#Анализируемые параметры AU02_c, AU04_c и AU05_c 
library(dplyr)
library(corrplot)
library(lsr)  # для вычисления коэффициента V Крамера
library(questionr)  # для вычисления коэффициента V Крамера

rm(list = ls())

# Считывание данных
r01 <- read.csv("r01_q.csv")
                          

# Агрегирование данных
AU02 <- data.frame(count(r01, mark, AU02_c))
AU02

cNames <- unique(AU02$AU02_c)
cNames

rNames <- unique(AU02$mark)
rNames

AU02matrix <- matrix(AU02$n, nrow = nrow(AU02) / 2, byrow = TRUE)
AU02matrix

row.names(AU02matrix) <- rNames
colnames(AU02matrix) <- cNames
AU02matrix

# Построение мозаичной диаграммы
mosaicplot(AU02matrix, main = 'AU02 Mosaic Plot',
           xlab = 'Mark', ylab = 'AU02_c', shade = TRUE)

# Тест хи-квадрат
AU02chisq <- chisq.test(AU02matrix)
AU02chisq 
#p-value < 2.2e-16 
#Нулевая гипотеза заключается в отсутствии межгрупповых различий
#Нулевая гипотеза отвергается в пользу альтернативной о том, что различия есть

AU02chisq$observed  # таблица наблюдаемых значений (чисел)
round(AU02chisq$expected, 0)  # таблица ожидаемых значений (чисел)
round(AU02chisq$residuals, 0)  # таблица остатков Пирсона

  
#Сравним группы попарно между собой
# Все возможные сравнения
# {0-2} {0-3} {0-4} {0-5}
#       {2-3} {2-4} {2-5}
#             {3-4} {3-5}
#                   {4-5}


# {0-2}
AU02.0to2 <- AU02matrix[1:2, ]
AU02.0to2
chisq.test(AU02.0to2)  

# {0-3}
AU02.0to3 <- AU02matrix[c(1,3), ]
AU02.0to3
chisq.test(AU02.0to3)

# {0-4}
AU02.0to4 <- AU02matrix[c(1,4), ]
AU02.0to4
chisq.test(AU02.0to4)

# {0-5}
AU02.0to5 <- AU02matrix[c(1,5), ]
AU02.0to5
chisq.test(AU02.0to5)

# {2-3}
AU02.2to3 <- AU02matrix[2:3, ]
AU02.2to3
chisq.test(AU02.2to3)

# {2-4}
AU02.2to4 <- AU02matrix[c(2,4), ]
AU02.2to4
chisq.test(AU02.2to4)

# {2-5}
AU02.2to5 <- AU02matrix[c(2,5), ]
AU02.2to5
chisq.test(AU02.2to5)

# {3-4}
AU02.3to4 <- AU02matrix[3:4, ]
AU02.3to4
chisq.test(AU02.3to4)

# {3-5}
AU02.3to5 <- AU02matrix[c(3,5), ]
AU02.3to5
chisq.test(AU02.3to5)

# {4-5}
AU02.4to5 <- AU02matrix[4:5, ]
AU02.4to5
chisq.test(AU02.4to5)  

# Вычисление коэффициента V Крамера
cramersV(AU02matrix)
cramer.v(AU02matrix)

#0.14 - взаимосвязь слабая

AU02jointMatrix <- matrix(rep(0, 8), nrow = 4)

AU02jointMatrix[1, ] <- AU02matrix[1, ] 
AU02jointMatrix[2, ] <- AU02matrix[2, ]
AU02jointMatrix[3, ] <- AU02matrix[3, ] + AU02matrix[4, ]
AU02jointMatrix[4, ] <- AU02matrix[5, ]


row.names(AU02jointMatrix) <- c("0", "2", "34", "5")
colnames(AU02jointMatrix) <- c("0", "1")
AU02jointMatrix

mosaicplot(AU02jointMatrix, main = 'Joint AU02 Mosaic Plot',
           xlab = 'Mark', ylab = 'AU04_c', shade = TRUE)

# {02-3}
AU02joint.02to3 <- AU02jointMatrix[1:2, ]
AU02joint.02to3
chisq.test(AU02joint.02to3)  # различия есть

# {02-4}
AU02joint.0to34 <- AU02jointMatrix[c(1,3), ]
AU02joint.0to34
chisq.test(AU02joint.0to34)  # различий нет

# {02-5}
AU02joint.0to5 <- AU02jointMatrix[c(1,4), ]
AU02joint.0to5
chisq.test(AU02joint.0to5)  # различия есть

# {2-34}
AU02joint.2to34 <- AU02jointMatrix[2:3, ]
AU02joint.2to34
chisq.test(AU02joint.2to34)  # различия есть

# {2-5}
AU02joint.2to5 <- AU02jointMatrix[c(2,4), ]
AU02joint.2to5
chisq.test(AU02joint.2to5)  # различия есть

# {34-5}
AU02joint.34to5 <- AU02jointMatrix[3:4, ]
AU02joint.34to5
chisq.test(AU02joint.34to5)  # различия есть

# Повторное вычисление коэффициента V Крамера
cramersV(AU02jointMatrix)
cramer.v(AU02jointMatrix)

#Коэффициент V Крамера не изменился



# ----------------------AU45--------------------------------

# Агрегирование данных
AU04 <- data.frame(count(r01, mark, AU04_c))
AU04

cNames <- unique(AU04$AU04_c)
cNames

rNames <- unique(AU04$mark)
rNames

AU04matrix <- matrix(AU04$n, nrow = nrow(AU04) / 2, byrow = TRUE)
AU04matrix

row.names(AU04matrix) <- rNames
colnames(AU04matrix) <- cNames
AU04matrix

# Построение мозаичной диаграммы
mosaicplot(AU04matrix, main = 'AU04 Mosaic Plot',
           xlab = 'Mark', ylab = 'AU04_c', shade = TRUE)

# Тест хи-квадрат
AU04chisq <- chisq.test(AU04matrix)
AU04chisq 
#p-value < 2.2e-16 
#Нулевая гипотеза заключается в отсутствии межгрупповых различий
#Нулевая гипотеза отвергается в пользу альтернативной о том, что различия есть

AU04chisq$observed  # таблица наблюдаемых значений (чисел)
round(AU04chisq$expected, 0)  # таблица ожидаемых значений (чисел)
round(AU04chisq$residuals, 0)  # таблица остатков Пирсона

# Визуализация таблицы остатков Пирсона

AU04contrib <- AU04chisq$residuals ^ 2 / AU04chisq$statistic * 100
AU04contrib <- round(AU04contrib, 0)

#Сравним группы попарно между собой
# Все возможные сравнения
# {0-2} {0-3} {0-4} {0-5}
#       {2-3} {2-4} {2-5}
#             {3-4} {3-5}
#                   {4-5}


# {0-2}
AU04.0to2 <- AU04matrix[1:2, ]
AU04.0to2
chisq.test(AU04.0to2)  

# {0-3}
AU04.0to3 <- AU04matrix[c(1,3), ]
AU04.0to3
chisq.test(AU04.0to3)

# {0-4}
AU04.0to4 <- AU04matrix[c(1,4), ]
AU04.0to4
chisq.test(AU04.0to4)

# {0-5}
AU04.0to5 <- AU04matrix[c(1,5), ]
AU04.0to5
chisq.test(AU04.0to5)

# {2-3}
AU04.2to3 <- AU04matrix[2:3, ]
AU04.2to3
chisq.test(AU04.2to3)

# {2-4}
AU04.2to4 <- AU04matrix[c(2,4), ]
AU04.2to4
chisq.test(AU04.2to4)

# {2-5}
AU04.2to5 <- AU04matrix[c(2,5), ]
AU04.2to5
chisq.test(AU04.2to5)

# {3-4}
AU04.3to4 <- AU04matrix[3:4, ]
AU04.3to4
chisq.test(AU04.3to4)      #различий нет

# {3-5}
AU04.3to5 <- AU04matrix[c(3,5), ]
AU04.3to5
chisq.test(AU04.3to5)

# {4-5}
AU04.4to5 <- AU04matrix[4:5, ]
AU04.4to5
chisq.test(AU04.4to5)  

# Вычисление коэффициента V Крамера
cramersV(AU04matrix)
cramer.v(AU04matrix)

#0.36 - взаимосвязь cредняя

# Объединение групп "3" и "4"


# Повторное выполнение теста хи-квадрат
chisq.test(AU02jointMatrix)


# Различия есть. Повторное выполнение попарных сравнений
# {0-2} {0-34} {0-5}
#       {2-34} {2-5} 
#              {34-5}

# ----------------------AU5--------------------------------


# Агрегирование данных
AU05 <- data.frame(count(r01, mark, AU05_c))
AU05

cNames <- unique(AU05$AU05_c)
cNames

rNames <- unique(AU05$mark)
rNames

AU05matrix <- matrix(AU05$n, nrow = nrow(AU05) / 2, byrow = TRUE)
AU05matrix

row.names(AU05matrix) <- rNames
colnames(AU05matrix) <- cNames
AU05matrix

# Построение мозаичной диаграммы
mosaicplot(AU05matrix, main = 'AU05 Mosaic Plot',
           xlab = 'Mark', ylab = 'AU05_c', shade = TRUE)

# Тест хи-квадрат
AU05chisq <- chisq.test(AU05matrix)
AU05chisq 
#p-value < 2.2e-16 
#Нулевая гипотеза заключается в отсутствии межгрупповых различий
#Нулевая гипотеза отвергается в пользу альтернативной о том, что различия есть

AU05chisq$observed  # таблица наблюдаемых значений (чисел)
round(AU05chisq$expected, 0)  # таблица ожидаемых значений (чисел)
round(AU05chisq$residuals, 0)  # таблица остатков Пирсона

# Визуализация таблицы остатков Пирсона

AU05contrib <- AU05chisq$residuals ^ 2 / AU05chisq$statistic * 100
AU05contrib <- round(AU05contrib, 0)


#Сравним группы попарно между собой
# Все возможные сравнения
# {0-2} {0-3} {0-4} {0-5}
#       {2-3} {2-4} {2-5}
#             {3-4} {3-5}
#                   {4-5}


# {0-2}
AU05.0to2 <- AU05matrix[1:2, ]
AU05.0to2
chisq.test(AU05.0to2)  #различий нет

# {0-3}
AU05.0to3 <- AU05matrix[c(1,3), ]
AU05.0to3
chisq.test(AU05.0to3)

# {0-4}
AU05.0to4 <- AU05matrix[c(1,4), ]
AU05.0to4
chisq.test(AU05.0to4)

# {0-5}
AU05.0to5 <- AU05matrix[c(1,5), ]
AU05.0to5
chisq.test(AU05.0to5)

# {2-3}
AU05.2to3 <- AU05matrix[2:3, ]
AU05.2to3
chisq.test(AU05.2to3)

# {2-4}
AU05.2to4 <- AU05matrix[c(2,4), ]
AU05.2to4
chisq.test(AU05.2to4)

# {2-5}
AU05.2to5 <- AU05matrix[c(2,5), ]
AU05.2to5
chisq.test(AU05.2to5)

# {3-4}
AU05.3to4 <- AU05matrix[3:4, ]
AU05.3to4
chisq.test(AU05.3to4)   

# {3-5}
AU05.3to5 <- AU05matrix[c(3,5), ]
AU05.3to5
chisq.test(AU05.3to5)

# {4-5}
AU05.4to5 <- AU05matrix[4:5, ]
AU05.4to5
chisq.test(AU05.4to5)  

# Вычисление коэффициента V Крамера
cramersV(AU05matrix)
cramer.v(AU05matrix)

#0.149 - взаимосвязь несущественная

# Объединение групп "0" и "2" и "3" "4"

AU05jointMatrix <- matrix(rep(0, 6), nrow = 3)

AU05jointMatrix[1, ] <- AU05matrix[1, ] + AU05matrix[2, ]
AU05jointMatrix[2, ] <- AU05matrix[3, ] 
AU05jointMatrix[3, ] <- AU05matrix[4, ] + AU05matrix[5, ]


row.names(AU05jointMatrix) <- c("02", "3", "45")
colnames(AU05jointMatrix) <- c("0", "1")
AU05jointMatrix

mosaicplot(AU05jointMatrix, main = 'Joint AU05 Mosaic Plot',
           xlab = 'Mark', ylab = 'AU05_c', shade = TRUE)

# Повторное выполнение теста хи-квадрат
chisq.test(AU05jointMatrix)


# Различия есть. Повторное выполнение попарных сравнений
# {02-34} {02-5}
#         {34-5} 


# {02-34}
AU05joint.02to34 <- AU05jointMatrix[1:2, ]
AU05joint.02to34
chisq.test(AU05joint.02to34)  # различия есть

# {02-5}
AU05joint.02to5 <- AU05jointMatrix[c(1,3), ]
AU05joint.02to5
chisq.test(AU05joint.02to5)  # различия есть

# {34-5}
AU05joint.34to5 <- AU05jointMatrix[2:3, ]
AU05joint.34to5
chisq.test(AU05joint.34to5)  # различия есть

# Повторное вычисление коэффициента V Крамера
cramersV(AU05jointMatrix)
cramer.v(AU05jointMatrix)

#Коэффициент V Крамера не изменился
