
test1 <- function () {
    time <- rep(0, 10)
    num <- (1:10)
    res <- double(10)

    # Task1
    t0 <- Sys.time()
    xA <- seq(10, 20, by = 10)

    res[1] <- sum(xA)
    t1 <- Sys.time()
    time[1] <- difftime(t1, t0, units = "min")

    # Task2
    t0 <- Sys.time()
    res[2] <- length(xA)
    res[3] <- mean(xA)
    t1 <- Sys.time()
    time[2] <- difftime(t1, t0, units = "min")

    # Task3
    t0 <- Sys.time()
    xB <- rnorm(length(xA)+7, mean = 5)
    res[4] <- round(sd(xB))
    t1 <- Sys.time()
    time[3] <- difftime(t1, t0, units = "min")

    # Task4
    t0 <- Sys.time()
    arr <- array(xA, dim = c(5, length(xA)/5))
    res[5] <- round(sum(sin(arr)), 4)
    t1 <- Sys.time()
    time[4] <- difftime(t1, t0, units = "min")

    # Task5
    t0 <- Sys.time()
    matr <- matrix(xA, nrow = 5, ncol = length(xA)/5)
    matr <- matr[-c(2, 5), ]
    res[6] <- nrow(matr) + ncol(matr)
    t1 <- Sys.time()
    time[5] <- difftime(t1, t0, units = "min")

    # Task6
    t0 <- Sys.time()
    list <- list(c(rep(TRUE, 5), rep(FALSE, 5)), c(rep(TRUE, 5), rep(FALSE, 5)), c(rep(TRUE, 5), rep(FALSE, 5)))
    res[7] <- Reduce(`&`, list)
    t1 <- Sys.time()
    time[6] <- difftime(t1, t0, units = "min")

    # Task7
    t0 <- Sys.time()
    res[8] <- identical(arr, matr)
    t1 <- Sys.time()
    time[7] <- difftime(t1, t0, units = "min")

    # Task8
    t0 <- Sys.time()
    arr <- matr
    res[9] <- identical(arr, matr)
    t1 <- Sys.time()
    time[8] <- difftime(t1, t0, units = "min")

    # Task9
    t0 <- Sys.time()
    df <- data.frame(Num=num, RES=res, Time=time)
    t1 <- Sys.time()
    time[9] <- difftime(t1, t0, units = "min")

    total_time <- sum(time)

    print(df)
    print(total_time)
}

test2 <- function () {
    time <- rep(0, 10)
    num <- (1:10)
    res <- double(10)

    # Task1
    t0 <- Sys.time()
    xA <- seq(10, 20, by = 10)
    res[1] <- prod(xA)/10^19
    t1 <- Sys.time()
    time[1] <- difftime(t1, t0, units = "min")

    # Task2
    t0 <- Sys.time()
    res[2] <- length(xA)
    res[3] <- mean(xA)
    t1 <- Sys.time()
    time[2] <- difftime(t1, t0, units = "min")

    # Task3
    t0 <- Sys.time()
    xB <- rnorm(length(xA)+7, mean = 5)
    res[4] <- round(sd(xB))
    t1 <- Sys.time()
    time[3] <- difftime(t1, t0, units = "min")

    # Task4
    t0 <- Sys.time()
    arr <- array(xA, dim = c(5, length(xA)/5))
    res[5] <- round(sum(sin(arr)), 4)
    t1 <- Sys.time()
    time[4] <- difftime(t1, t0, units = "min")

    # Task5
    t0 <- Sys.time()
    matr <- matrix(xA, nrow = 5, ncol = length(xA)/5)
    matr <- matr[-c(1, 3, 5), ]
    res[6] <- nrow(matr) + ncol(matr)
    t1 <- Sys.time()
    time[5] <- difftime(t1, t0, units = "min")

    # Task6
    t0 <- Sys.time()
    list <- list(c(rep(TRUE, 2), rep(FALSE, 2)), c(rep(TRUE, 2), rep(FALSE, 2)))
    res[7] <- Reduce(`&`, list)
    t1 <- Sys.time()
    time[6] <- difftime(t1, t0, units = "min")

    # Task7
    t0 <- Sys.time()
    res[8] <- identical(arr, matr)
    t1 <- Sys.time()
    time[7] <- difftime(t1, t0, units = "min")

    # Task8
    t0 <- Sys.time()
    arr <- matr
    res[9] <- identical(arr, matr)
    t1 <- Sys.time()
    time[8] <- difftime(t1, t0, units = "min")

    # Task9
    t0 <- Sys.time()
    df <- data.frame(Num=num, RES=res, Time=time)
    t1 <- Sys.time()
    time[9] <- difftime(t1, t0, units = "min")

    total_time <- sum(time)

    print(df)
    print(total_time)
}