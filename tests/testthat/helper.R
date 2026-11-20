rd_fragment <- function(x) {
  tools::parse_Rd(textConnection(x), fragment = TRUE)
}
