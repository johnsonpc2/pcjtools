#' @keywords internal
#' @title A suite of tools built to make data wrangling and analysis easier
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @importFrom data.table data.table
## usethis namespace: end
NULL

utils::globalVariables(c("sim_index", "x", "choice",
                         "p_resp", "N", "rt", "rt_q",
                         ".", "n_resp", "rt_p",
                         "stimulus", "prev_stimulus"))
