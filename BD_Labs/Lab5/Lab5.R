# Installing rvest library
# install.packages("rvest", repos = "http://cran.us.r-project.org")
library(rvest)
library(dplyr)

load_table_from_web <- function(year) {
	url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", year)

	table <- url %>%
		read_html() %>%
		html_nodes("table") %>%
		html_table(fill = TRUE)

	# Second table is the one we need
	table <- as.data.frame(table[2])
	# For some reason the Rank column set for NA by default.
	# Fixing it with this line
	table <- table %>% mutate(Rank = row_number())
	# Add year column
	table <- table %>% mutate(Year = year)
	# table_name <- paste0("QoL", year, ".csv")
	# write.csv(table, table_name, row.names = FALSE)
}

load_csv_2016_2023 <- function() {
	years <- 2016:2023
	# Latvia, Estonia, Lithuania, Bulgaria, Hungary
	# make one csv for all years for these countries
	table <- data.frame()
	for (year in years) {
		table <- rbind(table, load_table_from_web(year))
		# Leave only Latvia, Estonia, Lithuania, Bulgaria, Hungary
		table <- table %>% filter(Country %in% c("Latvia", "Estonia", "Lithuania", "Bulgaria", "Hungary"))
	}
	write.csv(table, "Lab5/QoL2016_2023.csv", row.names = FALSE)
}

countries_rating_all_years <- function() {
	table <- read.csv("Lab5/QoL2016_2023.csv")

	lithuania <- table[table$Country == "Lithuania",]
	estonia <- table[table$Country == "Estonia",]
	latvia <- table[table$Country == "Latvia",]
	bulgaria <- table[table$Country == "Bulgaria",]
	hungary <- table[table$Country == "Hungary",]

	par(mar = c(5, 4, 4, 2) + 0.1)
	plot(lithuania$Year, lithuania$Rank, type = "b", col = "red", xlab = "Year", ylab = "Rank", main = "Countries rating by year", ylim = c(min(table$Rank), max(table$Rank) + 10))
	lines(estonia$Year, estonia$Rank, type = "b", col = "blue")
	lines(latvia$Year, latvia$Rank, type = "b", col = "green")
	lines(bulgaria$Year, bulgaria$Rank, type = "b", col = "orange")
	lines(hungary$Year, hungary$Rank, type = "b", col = "black")
	legend("topright", legend = c("Lithuania", "Estonia", "Latvia", "Bulgaria", "Hungary"), col = c("red", "blue", "green", "orange", "black"), lty = 1, cex = 0.8)
}

countries_properties_all_years <- function() {
	table <- read.csv("Lab5/QoL2016_2023.csv")

	lithuania <- table[table$Country == "Lithuania",]
	estonia <- table[table$Country == "Estonia",]
	latvia <- table[table$Country == "Latvia",]
	bulgaria <- table[table$Country == "Bulgaria",]
	hungary <- table[table$Country == "Hungary",]

	par(mfrow = c(2, 2))
	plot(lithuania$Year, lithuania$Quality.of.Life.Index, type = "b", col = "red", xlab = "Year", ylab = "Quality of Life Index", main = "Quality of Life by year", ylim = c(min(table$Quality.of.Life.Index), max(table$Quality.of.Life.Index) + 10))
	lines(estonia$Year, estonia$Quality.of.Life.Index, type = "b", col = "blue")
	lines(latvia$Year, latvia$Quality.of.Life.Index, type = "b", col = "green")
	lines(bulgaria$Year, bulgaria$Quality.of.Life.Index, type = "b", col = "orange")
	lines(hungary$Year, hungary$Quality.of.Life.Index, type = "b", col = "black")
	legend("topleft", legend = c("Lithuania", "Estonia", "Latvia", "Bulgaria", "Hungary"), col = c("red", "blue", "green", "orange", "black"), lty = 1, cex = 0.5)

	plot(lithuania$Year, lithuania$Purchasing.Power.Index, type = "b", col = "red", xlab = "Year", ylab = "Purchasing Power Index", main = "Purchasing Power by year", ylim = c(min(table$Purchasing.Power.Index), max(table$Purchasing.Power.Index) + 10))
	lines(estonia$Year, estonia$Purchasing.Power.Index, type = "b", col = "blue")
	lines(latvia$Year, latvia$Purchasing.Power.Index, type = "b", col = "green")
	lines(bulgaria$Year, bulgaria$Purchasing.Power.Index, type = "b", col = "orange")
	lines(hungary$Year, hungary$Purchasing.Power.Index, type = "b", col = "black")
	legend("topleft", legend = c("Lithuania", "Estonia", "Latvia", "Bulgaria", "Hungary"), col = c("red", "blue", "green", "orange", "black"), lty = 1, cex = 0.5)

	plot(lithuania$Year, lithuania$Safety.Index, type = "b", col = "red", xlab = "Year", ylab = "Safety Index", main = "Safety by year", ylim = c(min(table$Safety.Index), max(table$Safety.Index) + 10))
	lines(estonia$Year, estonia$Safety.Index, type = "b", col = "blue")
	lines(latvia$Year, latvia$Safety.Index, type = "b", col = "green")
	lines(bulgaria$Year, bulgaria$Safety.Index, type = "b", col = "orange")
	lines(hungary$Year, hungary$Safety.Index, type = "b", col = "black")
	legend("topleft", legend = c("Lithuania", "Estonia", "Latvia", "Bulgaria", "Hungary"), col = c("red", "blue", "green", "orange", "black"), lty = 1, cex = 0.5)

	plot(lithuania$Year, lithuania$Health.Care.Index, type = "b", col = "red", xlab = "Year", ylab = "Health Care Index", main = "Health Care Index by year", ylim = c(min(table$Health.Care.Index), max(table$Health.Care.Index) + 10))
	lines(estonia$Year, estonia$Health.Care.Index, type = "b", col = "blue")
	lines(latvia$Year, latvia$Health.Care.Index, type = "b", col = "green")
	lines(bulgaria$Year, bulgaria$Health.Care.Index, type = "b", col = "orange")
	lines(hungary$Year, hungary$Health.Care.Index, type = "b", col = "black")
	legend("topleft", legend = c("Lithuania", "Estonia", "Latvia", "Bulgaria", "Hungary"), col = c("red", "blue", "green", "orange", "black"), lty = 1, cex = 0.5)

	plot(lithuania$Year, lithuania$Cost.of.Living.Index, type = "b", col = "red", xlab = "Year", ylab = "Cost of Living Index", main = "Cost of Living Index by year", ylim = c(min(table$Cost.of.Living.Index), max(table$Cost.of.Living.Index) + 10))
	lines(estonia$Year, estonia$Cost.of.Living.Index, type = "b", col = "blue")
	lines(latvia$Year, latvia$Cost.of.Living.Index, type = "b", col = "green")
	lines(bulgaria$Year, bulgaria$Cost.of.Living.Index, type = "b", col = "orange")
	lines(hungary$Year, hungary$Cost.of.Living.Index, type = "b", col = "black")
	legend("topleft", legend = c("Lithuania", "Estonia", "Latvia", "Bulgaria", "Hungary"), col = c("red", "blue", "green", "orange", "black"), lty = 1, cex = 0.5)

	plot(lithuania$Year, lithuania$Property.Price.to.Income.Ratio, type = "b", col = "red", xlab = "Year", ylab = "Property Price to Income Ratio", main = "Property Price to Income by year", ylim = c(min(table$Property.Price.to.Income.Ratio), max(table$Property.Price.to.Income.Ratio) + 10))
	lines(estonia$Year, estonia$Property.Price.to.Income.Ratio, type = "b", col = "blue")
	lines(latvia$Year, latvia$Property.Price.to.Income.Ratio, type = "b", col = "green")
	lines(bulgaria$Year, bulgaria$Property.Price.to.Income.Ratio, type = "b", col = "orange")
	lines(hungary$Year, hungary$Property.Price.to.Income.Ratio, type = "b", col = "black")
	legend("topleft", legend = c("Lithuania", "Estonia", "Latvia", "Bulgaria", "Hungary"), col = c("red", "blue", "green", "orange", "black"), lty = 1, cex = 0.5)

	plot(lithuania$Year, lithuania$Traffic.Commute.Time.Index, type = "b", col = "red", xlab = "Year", ylab = "Traffic Commute Time Index", main = "Traffic Commute Time Index by year", ylim = c(min(table$Traffic.Commute.Time.Index), max(table$Traffic.Commute.Time.Index) + 5))
	lines(estonia$Year, estonia$Traffic.Commute.Time.Index, type = "b", col = "blue")
	lines(latvia$Year, latvia$Traffic.Commute.Time.Index, type = "b", col = "green")
	lines(bulgaria$Year, bulgaria$Traffic.Commute.Time.Index, type = "b", col = "orange")
	lines(hungary$Year, hungary$Traffic.Commute.Time.Index, type = "b", col = "black")
	legend("topleft", legend = c("Lithuania", "Estonia", "Latvia", "Bulgaria", "Hungary"), col = c("red", "blue", "green", "orange", "black"), lty = 1, cex = 0.5)

	plot(lithuania$Year, lithuania$Pollution.Index, type = "b", col = "red", xlab = "Year", ylab = "Pollution Index", main = "Pollution Index by year", ylim = c(min(table$Pollution.Index), max(table$Pollution.Index) + 10))
	lines(estonia$Year, estonia$Pollution.Index, type = "b", col = "blue")
	lines(latvia$Year, latvia$Pollution.Index, type = "b", col = "green")
	lines(bulgaria$Year, bulgaria$Pollution.Index, type = "b", col = "orange")
	lines(hungary$Year, hungary$Pollution.Index, type = "b", col = "black")
	legend("topleft", legend = c("Lithuania", "Estonia", "Latvia", "Bulgaria", "Hungary"), col = c("red", "blue", "green", "orange", "black"), lty = 1, cex = 0.5)

	plot(lithuania$Year, lithuania$Climate.Index, type = "b", col = "red", xlab = "Year", ylab = "Climate Index", main = "Climate Index by year", ylim = c(min(table$Climate.Index), max(table$Climate.Index) + 10))
	lines(estonia$Year, estonia$Climate.Index, type = "b", col = "blue")
	lines(latvia$Year, latvia$Climate.Index, type = "b", col = "green")
	lines(bulgaria$Year, bulgaria$Climate.Index, type = "b", col = "orange")
	lines(hungary$Year, hungary$Climate.Index, type = "b", col = "black")
	legend("topleft", legend = c("Lithuania", "Estonia", "Latvia", "Bulgaria", "Hungary"), col = c("red", "blue", "green", "orange", "black"), lty = 1, cex = 0.5)
}

museum_data <- function() {
	url <- "https://tonkosti.ru/Музеи_Санкт-Петербурга"
	webpage <- read_html(url)

	museum_names <- webpage %>%
		html_nodes(css = 'h3.poster__title a') %>%
		html_text(trim = TRUE)

	museum_addresses <- webpage %>%
		html_nodes(css = 'div.poster__addr') %>%
		html_text(trim = TRUE)

	museum_links <- webpage %>%
		html_nodes(css = 'div.poster__img a') %>%
		html_attr("href")

	museums_info <- data.frame(
		"Museum_Name" = museum_names,
		"Museum_Address" = museum_addresses,
		"Museum_Link" = museum_links
	)
	print(museums_info)
	write.csv(museums_info, file = "museums_info.csv", row.names = FALSE)
}

countries_rating_all_years()
countries_properties_all_years()
# museum_data()