% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_funs.R
\name{blackjack_bust_auto}
\alias{blackjack_bust_auto}
\title{Title blakcjack_bust_auto}
\usage{
blackjack_bust_auto(trials = 10)
}
\arguments{
\item{trials}{Input numeric length 1. It is the number of trials of the game.
It is not recommended to make the trials much more than 50
as the graph will not display that many dots well.}
}
\value{
list of c(time_to_bust, data)
        time_to_bust = List of draws need to "bust"
        data = list of "time_to_bust", Frequency of the number of cards
        at "bust", and probabilities.
}
\description{
This will simulate drawing hand of BlackJack and count the number of draws
   needed to "Bust:.
}
\examples{

blackjack_bust_auto()
blackjack_bust_auto(trials = 25)


}
