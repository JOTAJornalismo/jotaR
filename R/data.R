#' @encoding UTF-8
#' @title Governismo

#' @description Governism Data. This dataset contains the following columns for each federal deputy:
#'
#' \itemize{
#' \item ID The Chamber's ID for the deputy
#' \item Nome The deputy name
#' \item Partido The political party label
#' \item UF The State label
#' \item Reeleito If the deputy is incumbent or not
#' \item Indice An indice of voting behavior
#' \item Apuracao An indicator of inside contact
#' \item Twitter Self-expression on Twitter about a topic
#' \item Prob_Votacao Voting prediction probability
#' \item Intercept The intercept
#' \item Beta1 The regression beta_{1}
#' \item D1 The First dimension
#' \item Beta2 The regression beta_{2}
#' \item D2 The Second dimension
#' \item Beta3 The regression beta_{3}
#' \item D3 The Third dimension
#' \item Previsao_Voto Voting behavior prediction
#' \item Membro_CESP If the deputy is member of the CESP
#' }
#'
#' @note This is
#' @source JOTA Jornalismo \emph{<https://jota.info>}.
#'
#' @docType data
#' @keywords datasets
#' @name Governismo
#' @usage data(Governismo)
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(rJOTA::Governismo)} variables and \Sexpr[stage=build,results=rd]{nrow(rJOTA::Governismo)} observations.
NULL
