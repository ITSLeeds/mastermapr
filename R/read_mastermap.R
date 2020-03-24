#' Read mastermap data
#'
#' @param dir Directory where MasterMap files live
#' @param type_match Type of feature to return, matched on directory
#' @param n How many files to return (all by default, can be resource-consuming)
#'
#' @export
read_mastermap <- function(dir, type_match = "roadlink", n = 5) {
  f_full = list.files(path = dir, pattern = type_match, ignore.case = TRUE, full.names = TRUE)[n]
  f = list.files(path = dir, pattern = type_match, ignore.case = TRUE)[n]
  message("Found these files: ", f)
  res = lapply(f, sf::st_read)
  res_df = do.call(rbind, res)
  res_df
}
