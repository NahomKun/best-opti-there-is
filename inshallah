 # Load required packages
  library(lpSolve)
  
  # Function to generate lineups
  generate_lineups <- function(player_stats, num_lineups, locked_players, max_exposure) {
    
    # Define objective function (maximize player fantasy points)
    objective <- player_stats$FPPG
    
    # Define constraints
    # Position constraints
    pg_constraint <- ifelse(player_stats$position %in% c("PG", "PG/SG", "SG/PG"), 1, 0)
    sg_constraint <- ifelse(player_stats$position %in% c("SG", "SG/SF", "SF/SG", "PG/SG", "SG/PG"), 1, 0)
    sf_constraint <- ifelse(player_stats$position %in% c("SF", "SF/PF", "PF/SF", "SG/SF", "SF/SG"), 1, 0)
    pf_constraint <- ifelse(player_stats$position %in% c("PF", "PF/C", "C/PF", "SF/PF", "PF/SF"), 1, 0)
    c_constraint <- ifelse(player_stats$position %in% c("C", "PF/C", "C/PF"), 1, 0)
    position_constraints <- cbind(pg_constraint, sg_constraint, sf_constraint, pf_constraint, c_constraint)
    # Salary constraint
    salary_constraint <- rep(1, nrow(player_stats))
    # Locked players constraint
    locked_constraint <- ifelse(player_stats$player %in% locked_players, num_lineups, 0)
    # Max exposure constraint
    max_exposure_constraint <- -max_exposure * num_lineups
    constraints <- cbind(position_constraints, salary_constraint, locked_constraint, max_exposure_constraint)
    rhs <- c(2, 2, 2, 2, 1, 60000, rep(num_lineups, length(locked_players)), rep(0, nrow(player_stats)))
    direction <- c(rep("==", 5), "==", rep(">=", length(locked_players)), rep("<=", nrow(player_stats)))
    
    # Set up optimization problem
    lp <- make.lp(nrow(constraints), nrow(player_stats))
    set.objfn(lp, objective)
    set.constr.matrix(lp, constraints)
    set.rhs(lp, rhs)
    set.direction(lp, direction)
    
    # Set up loop to generate multiple lineups
    lineups <- list()
    
    for (i in 1:num_lineups) {
      solve(lp)
      # Extract optimal lineup
      lineup <- as.character(get.variables(lp))
      lineups[[i]] <- player_stats[lineup == "1", "player"]
    }
    lineups
  }
  
  locked_players <- c("Devin Booker", "Jalen Williams")
  # Set maximum exposure for other players (percentage of lineups in which they can appear)
  max_exposure <- 0.8
  
  # Call function to generate lineups
  lineups <- generate_lineups(player_stats, num_lineups, locked_players, max_exposure)
  
  # Print generated lineups
  lineups
