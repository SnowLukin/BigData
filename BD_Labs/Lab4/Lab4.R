# create class Lab4 with methods task1, task2, task3, task4

Lab4 <- setRefClass(
  "Lab4",
  methods = list(

    getDictionaryData = function() {
      dictionaryData <- read.csv("Lab4/OlympicsDB/dictionary.csv", header = TRUE, sep = ",")
      dictionaryData
    },

    getSummerData = function() {
      summerData <- read.csv("Lab4/OlympicsDB/summer.csv", header = TRUE, sep = ",")
      summerData
    },

    getWinterData = function() {
      winterData <- read.csv("Lab4/OlympicsDB/winter.csv", header = TRUE, sep = ",")
      winterData
    },

    task1 = function() {
      # Вывести графики динамики олимпийских достижений России по виду спорта
      # относительно временной шкалы

      dictionaryData <- .self$getDictionaryData()
      summerData <- .self$getSummerData()
      winterData <- .self$getWinterData()

      # Код страны россия
      countryId <- dictionaryData$Code[dictionaryData$Country == "Russia"]

      summerYears <- unique(summerData$Year[summerData$Country == countryId])
      winterYears <- unique(winterData$Year[winterData$Country == countryId])
      years <- sort(unique(c(summerYears, winterYears)))
      # print(years)

      totalMedals <- sapply(
        years,
        function(year) {
          summer <- summerData$Medal[summerData$Country == countryId & summerData$Year == year]
          winter <- winterData$Medal[winterData$Country == countryId & winterData$Year == year]
          length(summer) + length(winter)
        }
      )
      print(totalMedals)

      par(mar = c(5, 4, 4, 2) + 0.1)
      plot(
        years,
        totalMedals,
        type = "b",
        xlab = "Year",
        ylab = "Medals",
        main = "Olympic achievements of Russia",
        col = "red", pch = 19, lty = 4, lwd = 2, cex = 2
      )

      # Вывести столбчатую диаграмму по количеству мест 1-8 спортсменов России по
      # каждой Олимпиаде по тяжелой атлетике

      totalPlaces <- sapply(
        summerYears,
        function(year) {
          summer <- summerData$Medal[summerData$Country == countryId &
                                       summerData$Year == year &
                                       summerData$Sport == "Weightlifting"]
          length(summer)
        }
      )
      print(totalPlaces)
      # Зимой атлетики нет
      barplot(
        totalPlaces,
        names.arg = summerYears,
        xlab = "Year",
        ylab = "Places",
        main = "Olympic achievements of Russia in Athletics",
        col = rainbow(6)
      )

      # Вывести круговую диаграмму по количеству
      # первых мест в каждой из олимпиад

      totalFirstPlaces <- sapply(
        summerYears,
        function(year) {
          summer <- summerData$Medal[summerData$Country == countryId &
                                       summerData$Year == year &
                                       summerData$Medal == "Gold" &
                                       summerData$Sport == "Weightlifting"]
          length(summer)
        }
      )

      pie(
        totalFirstPlaces,
        labels = summerYears,
        main = "Olympic achievements of Russia in Weightlifting"
      )

      # Вывести функциональные графики - тенденции изменения
      # количества призовых мест отдельно по мужчинам и женщинам за последние 30 лет.

      # past 30 years from 2023 for summerYears
      filteredSummerYears <- summerYears[summerYears >= 2023 - 30]

      print(filteredSummerYears)
      totalPlacesFor30YearsMen <- sapply(
        filteredSummerYears,
        function(year) {
          summer <- summerData$Medal[summerData$Country == countryId &
                                       summerData$Year == year &
                                       summerData$Sport == "Weightlifting" &
                                       summerData$Gender == "Men"]
          length(summer)
        }
      )

      totalPlacesFor30YearsWomen <- sapply(
        filteredSummerYears,
        function(year) {
          summer <- summerData$Medal[summerData$Country == countryId &
                                       summerData$Year == year &
                                       summerData$Sport == "Weightlifting" &
                                       summerData$Gender == "Women"]
          length(summer)
        }
      )

      par(mar = c(5, 4, 4, 2) + 0.1)
      plot(
        filteredSummerYears,
        totalPlacesFor30YearsMen,
        type = "b",
        col = "blue",
        lwd = 2,
        xlab = "Year", ylab = "Medals",
        main = "Weightlifting achievements of Russia for past 30 years.",
        ylim = c(0, max(max(totalPlacesFor30YearsMen), max(totalPlacesFor30YearsWomen)))
      )
      lines(filteredSummerYears, totalPlacesFor30YearsWomen, type = "b", col = "red", lwd = 2)

      legend("topright", legend = c("Men", "Women"), col = c("blue", "red"), lty = 1, lwd = 2)
    },

    task2 = function() {
      # Вывести графики изменения спортивных достижений 1) по золотым медалям и 2) по
      # призовым 3-местам по 7-и странам-призерам (разными цветами и точками) за последние 6
      # олимпиад.

      dictionaryData <- .self$getDictionaryData()
      summerData <- .self$getSummerData()
      winterData <- .self$getWinterData()

      summerYears <- unique(summerData$Year)
      winterYears <- unique(winterData$Year)
      years <- sort(unique(c(summerYears, winterYears)))

      lastSixGameYears <- tail(years, 6)
      print(lastSixGameYears)

      # по 7 странам призерам
      # Calculate the total number of medals for each country in the last six game years
      medalsByCountrySummer <- aggregate(summerData$Medal[summerData$Year %in% lastSixGameYears], by = list(summerData$Country[summerData$Year %in% lastSixGameYears]), length)
      medalsByCountryWinter <- aggregate(winterData$Medal[winterData$Year %in% lastSixGameYears], by = list(winterData$Country[winterData$Year %in% lastSixGameYears]), length)
      medalsByCountry <- merge(medalsByCountrySummer, medalsByCountryWinter, by = "Group.1", all = TRUE)

      # Rename the columns
      names(medalsByCountry) <- c("Country", "SummerMedals", "WinterMedals")
      # Replace missing values with 0
      medalsByCountry[is.na(medalsByCountry)] <- 0

      # Add a column for total medals
      medalsByCountry$TotalMedals <- medalsByCountry$SummerMedals + medalsByCountry$WinterMedals

      # Aggregate total medals by country
      medalsByCountry <- aggregate(medalsByCountry$TotalMedals, by = list(medalsByCountry$Country), sum)

      # Rename the columns
      names(medalsByCountry) <- c("Country", "Total Medals")
      print(medalsByCountry)

      # Sort the data frame by total medals in descending order
      medalsByCountry <- medalsByCountry[order(-medalsByCountry$`Total Medals`),]

      # Get the top six countries with the most medals in the last six game years
      topSevenCountries <- head(medalsByCountry$Country, n = 7)

      print(topSevenCountries)

      # Initialize a list to store the medal data for each country
      medalsByCountry <- list()

      # Loop through each country ID
      for (countryId in topSevenCountries) {
        yearsToInclude <- lastSixGameYears

        # Initialize a vector to store the medal data for each year
        medalsByYear <- vector("integer", length(yearsToInclude))

        # Loop through each year and count the number of medals won by the country
        for (i in seq_along(yearsToInclude)) {
          year <- yearsToInclude[i]
          medalsSummer <- summerData$Medal[summerData$Country == countryId &
                                       summerData$Year == year &
                                       (summerData$Medal == "Bronze" |
                                         summerData$Medal == "Silver" |
                                         summerData$Medal == "Gold")]
          medalsWinter <- winterData$Medal[winterData$Country == countryId &
                                       winterData$Year == year &
                                       (winterData$Medal == "Bronze" |
                                         winterData$Medal == "Silver" |
                                         winterData$Medal == "Gold")]
          medalsByYear[i] <- length(medalsSummer) + length(medalsWinter)
        }

        # Add the medal data for the country to the list
        medalsByCountry[[countryId]] <- medalsByYear
      }

      # Convert the list to a data frame
      medalData <- data.frame(matrix(unlist(medalsByCountry), ncol = length(yearsToInclude), byrow = TRUE))
      names(medalData) <- yearsToInclude
      # row.names(medalData) <- unique(summerData$Country)
      row.names(medalData) <- topSevenCountries

      print(medalData)

      # # Set the plot margins and labels
      par(mar = c(5, 4, 4, 2) + 0.1)
      plot(
        lastSixGameYears,
        medalData[1,],
        type = "b",
        xlab = "Year",
        ylab = "Country",
        ylim = c(0, max(medalData)),
      )

      # Add the lines to the plot
      for (i in seq_len(nrow(medalData))) {
        row <- as.numeric(medalData[i,])
        lines(lastSixGameYears, row, type = "b", col = i, lwd = 2, lty = 1)
      }

      # Add a legend to the plot
      legend("topright", legend = rownames(medalData), col = seq_len(nrow(medalData)), lwd = 2)

      # Initialize a list to store the medal data for each country
      medalsByCountry <- list()

      # Loop through each country ID
      for (countryId in topSevenCountries) {
        # Get the unique years that the country competed in
        years <- unique(summerData$Year[summerData$Country == countryId])

        # Filter out the most recent Olympic games
        yearsToInclude <- lastSixGameYears

        # Initialize a vector to store the medal data for each year
        medalsByYear <- vector("integer", length(yearsToInclude))

        # Loop through each year and count the number of medals won by the country
        for (i in seq_along(yearsToInclude)) {
          year <- yearsToInclude[i]
          medalsSummer <- summerData$Medal[summerData$Country == countryId &
                                       summerData$Year == year &
                                       (summerData$Medal == "Gold")]
          medalsWinter <- winterData$Medal[winterData$Country == countryId &
                                       winterData$Year == year &
                                       (winterData$Medal == "Gold")]
          medalsByYear[i] <- length(medalsSummer) + length(medalsWinter)
        }

        # Add the medal data for the country to the list
        medalsByCountry[[countryId]] <- medalsByYear
      }

      # Convert the list to a data frame
      medalData <- data.frame(matrix(unlist(medalsByCountry), ncol = length(yearsToInclude), byrow = TRUE))
      names(medalData) <- yearsToInclude
      # row.names(medalData) <- unique(summerData$Country)
      row.names(medalData) <- topSevenCountries
      # Set the plot margins and labels
      par(mar = c(5, 4, 4, 2) + 0.1)
      plot(
        lastSixGameYears,
        medalData[1,],
        type = "b",
        xlab = "Year",
        ylab = "Country",
        ylim = c(0, max(medalData)),
      )

      # Add the lines to the plot
      for (i in seq_len(nrow(medalData))) {
        row <- as.numeric(medalData[i,])
        lines(lastSixGameYears, row, type = "b", col = i, lwd = 2, lty = 1)
      }

      # Add a legend to the plot
      legend("topright", legend = rownames(medalData), col = seq_len(nrow(medalData)), lwd = 2)
    },

    task3 = function() {

      dictionaryData <- .self$getDictionaryData()
      summerData <- .self$getSummerData()
      winterData <- .self$getWinterData()

      # Код страны россия
      countryId <- dictionaryData$Code[dictionaryData$Country == "Russia"]

      summerYears <- unique(summerData$Year[summerData$Country == countryId])
      winterYears <- unique(winterData$Year[winterData$Country == countryId])
      years <- sort(unique(c(summerYears, winterYears)))

      lastSixGameYears <- tail(summerYears, 6)
      print(lastSixGameYears)

      totalPlacesMen <- sapply(
        lastSixGameYears,
        function(year) {
          summer <- summerData$Medal[summerData$Country == countryId &
                                       summerData$Year == year &
                                       summerData$Sport == "Weightlifting" &
                                       summerData$Gender == "Men"]
          length(summer)
        }
      )

      totalPlacesWomen <- sapply(
        lastSixGameYears,
        function(year) {
          summer <- summerData$Medal[summerData$Country == countryId &
                                       summerData$Year == year &
                                       summerData$Sport == "Weightlifting" &
                                       summerData$Gender == "Women"]
          length(summer)
        }
      )

      par(mar = c(5, 4, 4, 2) + 0.1)
      plot(
        lastSixGameYears,
        totalPlacesMen,
        type = "b",
        col = "blue",
        lwd = 2,
        xlab = "Year", ylab = "Medals",
        main = "Weightlifting achievements of Russia for the last 6 summer games.",
        ylim = c(0, max(max(totalPlacesMen), max(totalPlacesWomen)))
      )
      lines(lastSixGameYears, totalPlacesWomen, type = "b", col = "red", lwd = 2)

      legend("topright", legend = c("Men", "Women"), col = c("blue", "red"), lty = 1, lwd = 2)

      pieData <- c(sum(totalPlacesMen), sum(totalPlacesWomen))
      barplot(
        pieData,
        names.arg = c('men', 'women'),
        xlab = "Year",
        ylab = "Places",
        main = "Weightlifting achievements of Russia for the last 6 summer games.",
        col = rainbow(6),
        ylim = c(0, max(pieData))
      )


      pie(
        pieData,
        labels = c('men', 'women'),
        main = "Weightlifting achievements of Russia for the last 6 summer games."
      )
    }

  )
)

lab4 <- Lab4$new()
lab4$task1()
lab4$task2()
lab4$task3()