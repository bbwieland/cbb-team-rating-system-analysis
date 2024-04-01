roundrobin <- Gmisc::roundrobin

even <- seq(1,10)
odd <- seq(1,11)

robin.matrix <- roundrobin(11, rounds = 20)

colnames(robin.matrix) <- odd
ncol(robin.matrix)


column_to_df <- function(column, input_matrix) {
  
  team1 = colnames(input_matrix)[column]
  team2 = input_matrix[,column]
  
  output_df <- data.frame(Team1 = team1, Team2 = team2)
  return(output_df)
  
}



column_to_df(1, input_matrix = robin.matrix)

