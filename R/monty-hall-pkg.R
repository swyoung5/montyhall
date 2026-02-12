#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#'    Pick the player's initial door.
#'
#' @description
#'    `select_door()` randomly returns one of the three doors.
#'
#' @details
#'    This door is selected randomly to represent the player's lack of
#'    knowledge surrounding which door is which.
#'
#' @param
#'    This function uses no arguments.
#'
#' @return
#'    Integer. The function returns a single number between 1 and 3.
#'
#' @examples
#'     select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'    Open a goat door.
#'
#' @description
#'    `open_goat_door` returns the number of a door with a goat behind it
#'    from the two remaining doors after the initial player choice.
#'
#' @details
#'    Monty always opens a door with a goat behind it from the remaining two.
#'
#' @param `game` Character vector. This argument inputs the order of the
#'    doors (car and goats) to ensure a goat door is selected.
#'
#' @param `a.pick` Integer. This is the initial door the player picked.
#'
#'
#' @return Integer. This returns the number of the opened door.
#'
#'
#' @examples
#'     open_goat_door()
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'    Player switch or stay.
#'
#' @description
#'    This function determines whether or not the player switches doors.
#'
#' @details
#'    After Monty opens a goat door, the player may either double down
#'    on their original pick or switch to the last remaining door.
#'
#' @param `stay=T` Logical. Controls whether or not the player switches.
#'
#' @param `opened.door` Integer. The goat door opened by Monty.
#'
#' @param `a.pick` Integer. The player's initial pick.
#'
#' @return Integer. The number of the chosen door.
#'
#' @examples
#'     change_door()
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title
#'    Determine Win or Lose
#' @description
#'    This function determines whether the door chosen is the winning door
#'    or not.
#'
#' @details
#'    Win if the final choice of door is the car, lose if it is a goat.
#'
#' @param `final.pick` Integer. The number of the player's final choice
#'    of door.
#'
#' @param `game` Character vector. The position of each goat and the car.
#'
#' @return Character vector. This will print the word "win" or "lose".
#'
#' @examples
#'     determine_winner()
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'    Play Game
#'
#' @description
#'    This function plays an entire round of the Monty Hall game. It assigns
#'    the positions of the goats and car, has the player's initial pick, opens
#'    a goat door, and then reveals if they won depending on if they switched.
#'
#' @details
#'    This goes through each step previously documented.
#'
#' @param
#'    No parameters.
#'
#' @return A data frame. The frame contains each strategy (switch or stay) and
#'    the result of each.
#'
#' @examples
#'     play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'    Play multiple games.
#'
#' @description
#'    This plays the entire game multiple times depending on the input.
#'
#' @details
#'    This will calculate the overall win rate of each game strategy.
#'
#' @param `n=` Integer. How many times the game will be played.
#'
#' @return Data frame. The frame will contain each strategy and their
#'    respective win rates.
#'
#' @examples
#'     play_n_games()
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}
