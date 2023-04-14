# 1
# Выполнить импорт любых таблиц данных из csv-файла и xls-таблицы.

# 2
# Выполнить дескриптивный анализ данных из ЛР №2.

# 3
# Выполнить сортировку наборов данных по выбранному признаку.

# 4
# Сформировать отдельные наборы данных по одинаковому признаку (например, составить
# subdataset, из студентов, отдавших предпочтение по шкале > 0.7 определенной книге),
# вывести результат, выполнить подсчет размерностей новых таблиц, снова выполнить их
# анализ – гистограмма, боксплот, серединные меры (см п.2)

Lab3 <- setRefClass(
  "Lab3",

  methods = list(
    task1 = function() {
      # импорт данных из файла csv
      travel_data <- read.csv("Lab2/travel_dataset.csv", header = TRUE, sep = ";")
      travel_data
    },

    task2 = function(travel_data) {
      # Описание данных: summary(travel_data) дает сводную статистику по каждой переменной в выборке,
      # включая количество наблюдений, среднее значение, стандартное отклонение, минимальное
      # и максимальное значения, и значения квартилей.

      # Статистика по каждой переменной:
      # sapply(travel_data, function(x) c(mean=mean(x), sd=sd(x), median=median(x), min=min(x), max=max(x),
      # n_missing=sum(is.na(x))))
      # возвращает статистические данные по каждой переменной в выборке, включая среднее значение, стандартное отклонение,
      # медиану, минимальное и максимальное значения, и количество пропущенных значений.

      # Гистограммы: hist(travel_data[, i], main=names(travel_data)[i], xlab=names(travel_data)[i])
      # строит гистограмму для каждой переменной в выборке, чтобы проиллюстрировать распределение данных.

      # просмотр данных
      print("Просмотр данных")
      head <- head(travel_data)
      print(head)

      # описание данных
      print("Описание данных")
      summary <- summary(travel_data)
      print(summary)

      # статистика по каждой переменной
      print("Статистика: ")
      stats <- sapply(
        travel_data,
        function(x)
          c(
            mean = mean(x, na.rm = TRUE),
            sd = sd(x, na.rm = TRUE),
            median = median(x, na.rm = TRUE),
            min = min(x, na.rm = TRUE),
            max = max(x, na.rm = TRUE),
            n_missing = sum(is.na(x))
          )
      )
      print(stats)

      # гистограммы для каждой переменной
      par(mfrow = c(3, 3))

      for (i in 1:9) {
        hist(travel_data[, i], main = names(travel_data)[i], xlab = "Score")
      }
    },

    task3 = function(travel_data) {
      mean_values <- apply(travel_data, 2, mean, na.rm = TRUE)
      sorted_less_cols <- names(travel_data)[order(-mean_values)]
      sorted_more_cols <- names(travel_data)[order(mean_values)]

      print("До сортировки:")
      print(names(travel_data))
      print("По среднему значению по возрастанию")
      print(sorted_more_cols)
      print("По среднему значению по убыванию")
      print(sorted_less_cols)
    },

    task4 = function() {
      travel_data <- task1()
      # travel_data <- self$task1()
      travel_data <- travel_data[, 3:12]

      # Сформировать набор где в столбце Kazakhstan больше 7

      newTable <- apply(travel_data, 1, function(x) x["Kazakhstan"] > 7)

      # Вывести результат
      print("Набор где в столбце Kazakhstan больше 7")
      print(travel_data[newTable,])

      print("Размерность новой таблицы")
      print(dim(travel_data[newTable,]))

      .self$task2(travel_data[newTable,])

      # Построить boxplot
      par(mfrow = c(2, 1))
      boxplot(travel_data[newTable,], las = 2, main = "Stats of people who gave Kazahstan 8+ score", ylab = "Score", col = rainbow(10))

      # Серединная мера
      print("Серединная мера")
      print(apply(travel_data[newTable,], 2, median, na.rm = TRUE))
    }
  )
)

lab <- Lab3()
# lab$task1()
# lab$task2(lab$task1()[, 3:12])
# lab$task3(lab$task1()[, 3:12])
lab$task4()
