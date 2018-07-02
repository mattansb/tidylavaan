#' Method for importing lavaan output to tbl_graph
#'
#' Method for importing lavaan output to tbl_graph
#'
#' @author Mattan S. Ben-Shachar
#' @param object a lavaan object, returned from lavaan's sem/cfa functions.
#' @param standardize should the edge coeffciants be standerdized. curretnyl only `TRUE` is supported.
#'
#' @return a tbl_graph object that can further be processed with tidygraph, and plotted with ggraph.
#' @export
#' @import tidygraph
#' @import magrittr
#' @import dplyr
#' @import lavaan
#'
#' @examples see README
as_tbl_graph.lavaan <- function(object, standardize = TRUE) {

  if (standardize) {
  params <- standardizedSolution(object)
  } else {
    params <- parameterEstimates(object)
  }
  params_tidy <- params %>%
    select(3,1,2,everything()) %>%
    mutate(
      relation_full = case_when(
        op == "=~"  ~ "latent",
        op == "~"   ~ "regression",
        op == "~~"  ~ "covariance",
        op == "~1"  ~ "intercept",
        op == "|"   ~ "threshold",
        op == "~*~" ~ "scale",
        op == "<~"  ~ "latent (formative)",
        TRUE        ~ NA_character_
      ),
      relation_type = case_when(
        op %in% c("=~","~","<~") ~ "regression",
        op == "~~"               ~ "covariance",
        TRUE                     ~ NA_character_
      ),
      rhs2 = rhs,
      rhs = ifelse(relation_full == "latent", lhs, rhs),
      lhs = ifelse(relation_full == "latent", rhs2, lhs)
    ) %>%
    filter(!op %in% c("|","~1","~*~"),
           rhs != lhs) %>%
    select(-rhs2)

  latent_nodes <- with(params_tidy,
                       unique(rhs[relation_full=="latent"]))

  graph <- params_tidy %>%
    as_tbl_graph() %>%
    activate(nodes) %>%
    mutate(latent = name %in% latent_nodes)
}
