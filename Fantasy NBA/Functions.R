# Optimize the Daily Fantasy Lineup
Optimization_Lineup <- function(data)
{
  
  # Set consistent column names
  cnames <- tolower(colnames(data))
  cnames[4] <- "game_info"
  cnames[5] <- "avg_ppg"
  setnames(data, colnames(data), cnames)
  
  
  
  # Setup categorical values
  dummy_vars <- matrix(as.numeric(model.matrix(avg_ppg ~ position, data = data)), ncol = 5)
  dummy_vars[, 1] <- ifelse(data$position == "C", 1, 0)
  
  # Setup our guard and forward alternates
  dummy_vars <- cbind(dummy_vars, ifelse(data$position == "PG" | data$position == "SG", 1, 0))
  dummy_vars <- cbind(dummy_vars, ifelse(data$position == "SF" | data$position == "PF", 1, 0))
  
  # Copy matrix to set min values
  dummy_vars <- cbind(dummy_vars, dummy_vars)
  
  
  # Add in salary and ppg info 
  dummy_vars <- cbind(dummy_vars, 1, data$salary, data$avg_ppg)
  cnames <- c("c", "pf", "pg", "sg", "sg", "g", "f", 
              "c", "pf", "pg", "sg", "sg", "g", "f",
              "tot", "salary", "avg_ppg")
  
  colnames(dummy_vars) <- cnames
  dummy_vars <- t(dummy_vars)
  
  # Vharacter vector of direction constraints
  dir <- c("<=", "<=", "<=", "<=", "<=", "<=", "<=", 
           ">=", ">=", ">=", ">=", ">=", ">=", ">=",
           "==", "<=")
  
  # Right hand side constraints
  # Max number of players at each position [1-5]
  # 3 guards and 3 fowrads [6-7]
  # Total number selected 
  rhs <- c(2, 3, 3, 3, 3, 4, 4, 
           # min players at each position
           1, 1, 1, 1, 1, 1, 1,
           # max players and max salary
           8, 50000)
  
  
  # maximize or minimize the problem
  max <- T
  
  # Binary objective variables
  types <- "B"
  
  # Objective values to maximize
  obj <- dummy_vars[17, ]
  
  # Matrix
  mat <- dummy_vars[1:16, ]
  
  # Setup the solver
  sol <- Rsymphony_solve_LP(obj = obj,
                            mat = mat,
                            dir = dir,
                            rhs = rhs,
                            types = types,
                            max = max)
  
  #paste0("Based on players average PPG we expect the daily fantasy team to produce ", sol$objval, " points")
  
  # Examine selected team
  data <- as.data.frame(data)
  
  data$selected <- sol$solution
  data <- data[data$selected == 1, ]
  
  
  # Total players selected
  print("Total Player:")
  cat(nrow(data))
  cat("\n")
  cat("\n")
  
  # Table of positions selected to ensure we have at least 1 of each position and constraints are met
  print("Each Position:")
  print(table(data$position))
  cat("\n")
  cat("\n")
  
  print("Total Salary:")
  cat(sum(data$salary))
  cat("\n")
  cat("\n")
  
  # Total points aligns with the solver
  print("Expected Total Points")
  message(sum(data$avg_ppg))
  cat("\n")
  
  # Average salary for each player compare with the simple average salary, 6250 = 50000/8
  print("Number of players whose average salary calculated by solver bigger than or equal to $6250:")
  message(sum(data$salary>=mean(data$salary)))
  cat("\n")
  
  # Everything looks good!
  data <- data %>%
    arrange(salary)
  
  # Print the team to the console
  print("Detail Lineup:")
  cat("\n")
  print(data)
}