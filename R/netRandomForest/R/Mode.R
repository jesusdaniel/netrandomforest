## Source: https://stackoverflow.com/questions/63020045/get-the-most-frequent-value-per-row-and-account-for-ties

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}