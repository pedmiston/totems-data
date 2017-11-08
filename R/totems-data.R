#' Information about teams
#' @docType data
#' @usage data("TeamInfo")
"TeamInfo"

#' Information about players
#'
#' \describe{
#'   \item{PlayerID}{unique identifier for players}
#'   \item{TeamID}{unique identifier for teams}
#'   \item{Strategy}{strategy assigned to players in this team}
#'   \item{Generation}{generation of participant. Only relevant for Diachronic teams.}
#'   \item{SessionDuration}{the length of time this player played the game in a single session}
#'   \item{PlayerIX}{index for each player in a team, e.g., P1 and P2. Assigned at random.}
#' }
#'
#' @docType data
#' @usage data("PlayerInfo")
"PlayerInfo"

#' Workshop guesses made by players in the Totems experiment.
#'
#' The raw observations for the Totems experiment are individual
#' workshop guesses, consisting of a Guess (e.g., "0_0_1_3") and a
#' Result, which is 0 if the guess was incorrect, and the number
#' of the created item if correct.
#'
#' \describe{
#'   \item{PlayerID}{unique identifier for players}
#'   \item{PlayerTime}{seconds, time elapsed for this player when this guess was made}
#'   \item{TeamTime}{seconds, time elapsed for this team when this guess was made}
#'   \item{GuessNum}{current guess number}
#'   \item{TeamGuessNum}{current team guess number}
#'   \item{Guess}{a string created from a combination of up to 4 items that represents a guess}
#'   \item{Result}{numeric, the item number of the result. Result == 0 if the guess is incorrect.}
#'   \item{Score}{numeric, the score, if any, received for this guess. Normal innovations are 15 points each. Anything that is used to make a Totem is worth much more than any tool. More advanced totems are worth more points.}
#'   \item{UniqueGuess}{bool, has this guess been made by this player before?}
#'   \item{TeamUniqueGuess}{bool, has this guess been made by this team before?}
#'   \item{UniqueItem}{bool, has this item been made by this player before?}
#'   \item{TeamUniqueItem}{bool, has this item been made by this team before?}
#' }
#' @docType data
#' @usage data("Guesses")
"Guesses"

#' Inventories uncovered by teams in the Totems experiment.
#'
#' \describe{
#'   \item{TeamID}{unique identifier for teams}
#'   \item{TeamInventory}{a string representing all items that have been discovered by this team}
#'   \item{NumInnovations}{number of new items discovered by the team}
#'   \item{NumAdjacent}{number of discoveries that could be made from this inventory}
#'   \item{TeamGuesses}{number of guesses it took to make a new discovery}
#'   \item{TeamUniqueGuesses}{number of unique guesses for the team it took to make a new discovery}
#'   \item{UniqueGuesses}{number of unique guesses for the players it took to make a new discovery}
#'   \item{Duration}{the amount of time it took the team to make a new discovery}
#'   \item{Difficulty}{the difficulty of this inventory, taken as a function of
#'     the number of items that can be combined relative to the number of adajcent
#'     and correct items}
#' }
#'
#' @docType data
#' @usage data("Inventories")
"Inventories"

#' Cumulative performance sampled at a regular interval.
#'
#' @docType data
#' @usage data("SampledPerformance")
"SampledPerformance"
