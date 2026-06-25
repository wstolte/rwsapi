
#' @title Reduce dataframe columns by nesting single-value columns.
#'
#' @description
#' TO BE WORKED OUT
#'
#' @param df Dataframe
#' @param outputFormat Single-value column reduction in attributes ("attributed") or nested ("nested")
#'
#' @return Simplified dataframe
#'
#' @examples
#' \dontrun{
#' nested_df <- simplify_df(df, outputFormat = "nested")
#' # to go back to original dataframe:
#' nested_df %>% unnest(data)
#' }
#'
#' @export
simplify_df <- function(df, outputFormat = c("nested", "attributed")) {

  # 1. Identify columns with only 1 unique non-NA value
  is_constant <- sapply(df, function(col) {
    length(unique(col[!is.na(col)])) == 1
  })

  # 2. Extract constant values as a named list
  const_vals <- lapply(df[is_constant], function(col) unique(col[!is.na(col)]))
  const_names <- names(const_vals)

  # 2.5 Nest dataframe based on constant columns

  df_nested <- df %>% dplyr::group_by(across(all_of(const_names))) %>% tidyr::nest()

  # 3. Remove constant columns from dataframe
  df_simplified <- df[!is_constant]

  # 4. Store constant metadata as attributes
  for (nm in names(const_vals)) {
    attr(df_simplified, nm) <- const_vals[[nm]]
  }

  # 5. Add attribute listing all metadata names (easy lookup)
  attr(df_simplified, "constant_columns") <- names(const_vals)

  if(outputFormat == "attributed"){
    return(df_simplified)} else{
      return(df_nested)
    }
}
