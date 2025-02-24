# Specify the path to your text file
file_path <- "microban.txt"

# Read the entire file into a character vector
lines <- readLines(file_path)

# Function to convert lines to a matrix
lines_to_matrix <- function(lines) {
  matrix(unlist(strsplit(lines, "")), nrow = length(lines), byrow = TRUE)
}

plot_soko <- function(goals, boxes, player, grid) {
  
  rows <- nrow(grid)
  cols <- ncol(grid)
  
  # Create a blank plot with appropriate dimensions
  plot(0, 0, type = "n", xlim = c(0, cols + 1), ylim = c(0, rows + 1), xlab = "", ylab = "")
  
  
  # Add walls
  walls <- which(grid == "W", arr.ind = TRUE)
  #points(  walls[,1], walls[,2],  pch = 15, col = "gray", cex = 2)
  #m = as.maze(rotated_matrix)
  #plot(m, col = 'black', walls=TRUE)
  points(walls[, 2] , walls[, 1], pch = 22, col = "black", cex = 5)
  
  # Add edges
  edges <- which(grid == "1", arr.ind = TRUE)
  points(edges[, 2] , edges[, 1], pch = 4, col = "purple", cex = 1)
  
  # Add player
  points(player[1], player[2], pch = 19, col = "black", cex = 2)
  
  # Add boxes
  points(boxes[1,] , boxes[2,], pch = 22, col = "brown", cex = 2)
  
  # Add goals
  points(goals[1,] , goals[2,], pch = 16, col = "red", cex = 1)
}

mod <- function(g, m){
  ng = NA
  if(length(g)==0){return (NA)}
  for(i in 1:nrow(g)){
    vals = g[i,]
    x = vals[2]
    y = nrow(m) - (vals[1]-1)
    
    if(!is.matrix(ng)){
      ng = cbind(c(x,y))
    }else{
      ng = cbind(ng, c(x,y))
    }
    
  }
  return(ng)
}



inds <- function(lines){
  inds = list()
  i=1
  for (line in lines){
    if (startsWith(line, ";")){
      inds[[length(inds)+1]]=i
    }
    i = i+1
  }
  return(inds)
}




makegrids <- function(inds){
  mats = list()
  for(i in 2:length(inds)){
    l = lines[inds[[(i-1)]] : inds[[i]]]
    rows = list()
    maxlen = 0
    for (line in l){
      if (startsWith(line, ";")) {
        # print(line)
        next
      }
      line = lines_to_matrix(line)
      if (length(line)==0){
        next 
      }
      rows[[length(rows)+1]]= line
      if(ncol(line)>maxlen){
        maxlen = ncol(line)
      }
      
    }
    #print('maxlen')
    #print(maxlen)
    
    upd = list()
    for (row in rows){
      num_cols_to_add = maxlen - ncol(row)
      #print(num_cols_to_add)
      if (num_cols_to_add > 0) {
        for(i in 1:num_cols_to_add){
          row <- cbind(row, "#")
        }
        
        #print(row)
        upd[[length(upd)+1]]=row
        next
      }
      upd[[length(upd)+1]]=row
    }
    combined_matrix <- do.call(rbind, upd)
    combined_matrix[combined_matrix == "#"] <- "W"
    goals <- which(combined_matrix == ".", arr.ind = TRUE)
    boxes <- which(combined_matrix == "$", arr.ind = TRUE)
    boxes = mod(boxes, combined_matrix)
    goals = mod(goals, combined_matrix)
    player = which(combined_matrix == "@", arr.ind = TRUE)
    player = mod(player, combined_matrix)
    box_goal = which(combined_matrix == "*", arr.ind = TRUE)
    box_goal = mod(box_goal, combined_matrix)
    #print(box_goal)
    if(is.matrix(box_goal)){
      boxes = cbind(boxes, box_goal)
      goals = cbind(goals, box_goal)
    }
    
    modified_matrix <- apply(combined_matrix, c(1, 2), function(x) ifelse(x != "W", ".", x))
    #print(modified_matrix)
    
    mats[[length(mats)+1]] = list(m = modified_matrix, g=goals, b=boxes, p=player,
                                  bg = box_goal)
    
    
    
  }
  return(mats)
}

ind = inds(lines)
grids = makegrids(ind)



plotgrids <- function(grid){
  sokoban_grid = grid$m
  p = grid$p
  b = grid$b
  g = grid$g
  
  original_matrix <- sokoban_grid
  rotated_matrix <- original_matrix[nrow(original_matrix):1, ]
  grid <- rotated_matrix
  
  plot_soko(g,b,p,grid)
}


getgrid <-function(no, grids){
  g = grids[[no]]
  plot = g$m
  goals = g$g
  boxes = g$b
  player = g$p
  
  return(list(grid = plot, goals = goals, boxes=boxes, player=player ))
}

test <- function(run, returnmatrix, plotgames, timeLimit, numgames){
  my_matrix = matrix(
    data = NA,  
    ncol = 3, 
    nrow = numgames,
    dimnames = list(NULL, c("passed", "time", "push"))  
  )
  startTime=Sys.time()
  for(i in 1:numgames){
    midTime=Sys.time()
    
    if (as.numeric(midTime)-as.numeric(startTime)>timeLimit) {
      cat("\nRun terminated due to slowness. time taken\n", as.numeric(midTime)-as.numeric(startTime))
      break
    }
    
    re = run(i, plotgames)
    
    endTime=Sys.time()
    
    passed = re$pass
    if(passed){
      time = as.numeric(endTime)-as.numeric(midTime)
      push = re$numpushes
      
      my_matrix[i, "passed"] <- passed
      my_matrix[i, "time"] <- time
      my_matrix[i, "push"] <- push
      
    }
 
  }
  totalend = Sys.time()
  totalendtime = (as.numeric(totalend)-as.numeric(startTime))/60
  
  mean_time = mean(my_matrix[, "time"], na.rm = TRUE)
  sd_time = sd(my_matrix[, "time"], na.rm = TRUE)
  nump = length(which(my_matrix[,"passed"]==TRUE))
  
  cat('\nnumber of puzzles passed:',nump)
  
  cat('\nmean execution time: in seconds', mean_time)
  
  cat('\nsd of execution time:', sd_time)
  
  cat('\nrun time for all games: in minutes', totalendtime, '\n')
  
  if (returnmatrix){
    return(my_matrix)
  }
  
  
}


