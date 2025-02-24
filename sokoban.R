source('makegrids.R')

#' find tiles around box
#' tiles(box, grid, boxes)
tiles <- function(box, grid, boxes){
  potential = list(c(box[1], box[2]+1), c(box[1], box[2]-1),
                   c(box[1]-1, box[2]), c(box[1]+1, box[2]))
  neighs = list()
  
  for (i in 1: length(potential)){
    pt = potential[[i]]
    x = pt[1]
    y = pt[2]
    
    if(x>ncol(grid) | x<1 | y>nrow(grid) | y<1){next}
    
    point_to_check = c(x,y)
    is_in_boxes = any(apply(boxes, 2, function(col) all(col == point_to_check)))
    
    if (grid[pt[2], pt[1]]!="W" && !is_in_boxes){
      neighs[[length(neighs)+1]]=pt
    }
  }
  return(neighs)
}

#' create path in terms of wasd
process_moves <- function(player, new_player_pos){
  if (new_player_pos[1]-player[1] >0){
    mv1 = "d"
  }else if(new_player_pos[1]-player[1] <0){
    mv1 = "a"
  }else if(new_player_pos[2]-player[2] >0){
    mv1 = "w"
  }else if(new_player_pos[2]-player[2] <0){
    mv1 = "s"
  }
  return(mv1)
}
convert_to_points <- function(adjlist, path){
  g = adjlist
  s=path
  coords = list()
  for (i in 1:length(s)){
    ind = s[[i]]
    name = names(g[ind])
    
    coordinates_components <- strsplit(name, ",")[[1]]
    coordinates_numeric <- as.numeric(coordinates_components)
    coords[[length(coords)+1]]=coordinates_numeric
  }
  return(coords)
}
create_path <- function(pts){
  mvs = list()
  for (i in 2:length(pts)){
    p = pts[[i-1]]
    d = pts[[i]]
    
    mv = process_moves(p,d)
    mvs[[length(mvs)+1]]=mv
  }
  return(mvs)
}

#' adj list for bfs
create_adjacency_list <- function(grid, boxes) {
  nrows <- nrow(grid)
  ncols <- ncol(grid)
  
  adjacency_list <- list()
  
  for (col in 1:nrows) {
    for (row in 1:ncols) {
      # Skip non-empty cells
      if (grid[col, row] != "." ) {
        if( grid[col, row] != "1"){
          next
        }
        
      }
      
      current_cell <- c(row, col)
      
      neighbors <- tiles(current_cell, grid, boxes)
      
      adjacency_list[[toString(current_cell)]] <- neighbors
    }
  }
  
  return(adjacency_list)
}

#' bfs that returns path from player to tile
new_bfs <- function(start, goal, grid, boxes){
  g = create_adjacency_list(grid, boxes)
  n = length(g)
  
  bsolve <- function(s){
    target_coordinates <- toString(s)
    index_value <- which(names(g) == target_coordinates)
    q = c(index_value)
    visited <- as.list(rep(FALSE, n))
    visited[index_value]=TRUE
    prev <- as.list(rep(NA, n))
    
    while(length(q)>0){
      node = q[1]
      q = q[-1]
      neighs = g[[node]]
      if (length(neighs)==0){return (NA)}
      
      for (l in 1: length(neighs)){
        nex = toString(neighs[[l]])
        ind <- which(names(g) == nex)
        if (visited[ind]==FALSE){
          q = c(q, ind)
          visited[ind]=TRUE
          prev[[ind]] = node
        }
      }
    }
    return (prev)
  }
  
  recon <- function(s, e, prev){
    if(!is.list(prev)){return (NA)}
    
    target_coordinates <- toString(e)
    gind <- which(names(g) == target_coordinates)
    target_coordinates <- toString(s)
    sind <- which(names(g) == target_coordinates)
    path = list()
    
    if(!is.na(prev[[gind]])){
      path[[length(path)+1]] = gind
      
      while(TRUE){
        path[[length(path)+1]] = prev[[path[[length(path)]]]]
  
        if (path[[length(path)]]==sind){
          break
        }
      }
      
      pathrev = rev(path)
      if (!is.na(pathrev[[1]]) && pathrev[[1]] == sind){
        return(pathrev)
      }else{
        return(NA)
      }
    }
    else{
      return(NA)
    }
  }
  
  prev = bsolve(start)
  path = recon(start, goal, prev)
  return(path)
}

#' available pushes
#' bfs search to valid tiles around box
#' if you reach a tile, determine if you can push it in a certain direction
#' if you can push return the path leading to the push
#' returns path to tile, the box to be pushed, the tile to push from.
pushes <- function(boxes, grid, player){
  
  ps = list()
  g = create_adjacency_list(grid, boxes)
  
  for (i in 1:ncol(boxes)){
    box = boxes[,i]
    
    # valid tiles around box:
    n = tiles(box, grid, boxes)
    
    if(length(n)==0){
      next
    }
    
    for (f in 1:length(n)){
      tile = n[[f]]
      
      # if path and tile on same position return empty path
      if(player[1]==tile[1] && player[2]==tile[2]){
        ps[[length(ps)+1]] = list(pa = list(), b = box, t=tile)
        next
      }
      
      s = new_bfs(player, tile, grid, boxes)
      
      
      if(!is.na(s[1])){
        
        p = convert_to_points(g, s)
        path = create_path(p)
        ps[[length(ps)+1]] = list(pa = path, b = box, t=tile)
        
      }
    }
  }
  return(ps)
}

# play game
play_game <- function(goals, boxes, player, grid, move){
  
  #for (i in 1: length(move)){}
  mv1=move
  
  if(mv1=='q'){return(list(player = 'q', boxes = boxes))}
  
  oldplayer <- player
  new_player_pos <- player
  new_box_pos <- boxes
  
  if (mv1== "w"){
    new_player_pos[2] = new_player_pos[2]+1
  }else if(mv1=="a"){
    new_player_pos[1] = new_player_pos[1]-1
  }else if(mv1=="s"){
    new_player_pos[2] = new_player_pos[2]-1
  }else if(mv1=="d"){
    new_player_pos[1] = new_player_pos[1]+1
  }else{
    return(list(player = oldplayer, boxes = boxes))
  }
  
  # move box in same direction if player on box
  matching_boxes_indices <- which(new_player_pos[1] == boxes[1, ] & new_player_pos[2] == boxes[2, ])  
  if(length(matching_boxes_indices)>0){
    change_x = new_player_pos[1] - player[1]
    change_y = new_player_pos[2] - player[2]
    
    new_box_pos[,matching_boxes_indices][1] = new_box_pos[,matching_boxes_indices][1]+change_x
    new_box_pos[,matching_boxes_indices][2] = new_box_pos[,matching_boxes_indices][2]+change_y
    x = new_box_pos[,matching_boxes_indices][1]
    y = new_box_pos[,matching_boxes_indices][2]
    
    
    # if box has moved onto a "."
    if(grid[y, x] == "."){
      return(list(player = new_player_pos, boxes = new_box_pos))
    }else{
      return(list(player = oldplayer, boxes = boxes))
    }
  }
  
  # can only move on dots
  if (grid[new_player_pos[2], new_player_pos[1]] == "." | grid[new_player_pos[2], new_player_pos[1]] == "1") {
    
    return(list(player = new_player_pos, boxes = boxes))
  } else { # player doesnt change position
    return(list(player = oldplayer, boxes = boxes))
  }
  
  
  
}

exploreneighs <- function(r,c, grid, visited,rq,cq, nodes_in_next_layer){
  dr = c(-1,+1,0,0)
  dc = c(0,0,+1,-1)
  rq=rq
  cq=cq
  visited=visited
  nodes_in_next_layer = nodes_in_next_layer
  for (i in 1:4){
    rr = r + dr[i]
    cc = c + dc[i]
    
    if (grid[cc,rr]!="."){
      next
    }
    if(visited[cc,rr]){
      next
    }
    rq = c(rq, rr)
    cq = c(cq, cc)
    visited[cc,rr] = TRUE
    
    nodes_in_next_layer = nodes_in_next_layer + 1
    
  }
  return(list(rq=rq, cq=cq, visited=visited, nodes_in_next_layer = nodes_in_next_layer))
}

# takes box and goals and returns number of pushes from box to closest goal
bfs <- function(start, goals, grid){
  rows <- nrow(grid)
  cols <- ncol(grid)
  
  sr = start[1]
  sc = start[2]
  rq = c()
  cq = c()
  
  move_count = 0
  nodes_left_in_layer = 1
  nodes_in_next_layer = 0
  
  reached_end = FALSE
  
  visited <- matrix(FALSE, nrow = rows, ncol = cols)
  
  dr = c(-1,+1,0,0)
  dc = c(0,0,+1,-1)
  
  
  rq = c(sr) # enq
  cq = c(sc) # enq
  
  visited[sc,sr]=TRUE
  # solve
  while(length(rq)>0){
    r = rq[1] 
    c = cq[1] 
    
    rq = rq[-1] # deq
    cq = cq[-1] # deq
    
    match_goal = which(goals[1,]==r & goals[2,]==c)
    if(!is.na(match_goal[1])){
      return (move_count)
      
    }
    res = exploreneighs(r,c, grid, visited,rq,cq, nodes_in_next_layer)
    rq = res$rq
    cq = res$cq
    visited = res$visited
    nodes_in_next_layer = res$nodes_in_next_layer
    nodes_left_in_layer = nodes_left_in_layer -1
    
    
    
    if(nodes_left_in_layer==0){
      nodes_left_in_layer = nodes_in_next_layer
      nodes_in_next_layer = 0
      move_count = move_count + 1
    }
    
    
  }
  return (-1)
  
}

#' check if pt is blocked by boxes or walls
checkcorners <- function(pt, grid, boxes, goals){
  x = pt[1]
  y = pt[2]
  corners = list(list(c(x, y-1), c(x+1, y)),
                 list(c(x, y+1), c(x-1, y)),
                 list(c(x, y+1), c(x+1, y)),
                 list(c(x, y-1), c(x-1, y)))
  ingoal = any(apply(goals, 2, function(col) all(col == pt)))
  
  
  for (p in corners){
    
    pt1 = p[[1]]
    x1 = pt1[[1]]
    y1 = pt1[[2]]
    
    pt2 = p[[2]]
    x2 = pt2[[1]]
    y2 = pt2[[2]]
    
    if(x1>ncol(grid) | x2 > ncol(grid) | x1<1 | x2<1
       | y1>nrow(grid) | y2>nrow(grid)| y1<1 | y2<1){next}
    point_to_check = c(x1,y1)
    point_to_check1 = c(x2,y2)
    
    
    if (grid[y1, x1]=="W"){
      if(grid[y2, x2]=="W"){
        if(ingoal){return (list(val=FALSE, corner=TRUE))}
        return (list(val=TRUE, corner=TRUE))
      }
    }
    
  }
  
  return (list(val = FALSE, corner = FALSE ))
  
}

block <- function(pt, grid, boxes, goals){
  
  
  checkwalls <- function(){
    x = pt[1]
    y = pt[2]
    walls = list(list(c(x, y-1), c(x+1, y-1), c(x-1, y-1)),
                 list(c(x, y+1), c(x+1, y+1), c(x-1, y+1)),
                 list(c(x-1, y), c(x-1, y+1), c(x-1, y-1)),
                 list(c(x+1, y), c(x+1, y+1), c(x+1, y-1)))
    di =1
    for (p in walls){
      
      pt1 = p[[1]]
      x1 = pt1[[1]]
      y1 = pt1[[2]]
      
      pt2 = p[[2]]
      x2 = pt2[[1]]
      y2 = pt2[[2]]
      
      pt3 = p[[3]]
      x3 = pt3[[1]]
      y3 = pt3[[2]]
      
      if(x1>ncol(grid) | x2 > ncol(grid) | x1<1 | x2<1
         | y1>nrow(grid) | y2>nrow(grid)| y1<1 | y2<1 |
         x3>ncol(grid) | y3>nrow(grid)| x3<1 | y3<1 ){next}
      
      
      
      if (grid[y1, x1]=="W"){
        if(grid[y2, x2]=="W"){
          if(grid[y3,x3]=="W")
            
            return (list(p = TRUE, dir = di, pt=pt))
        }
      }
      di = di+1 
    }
    
    return(list(p = FALSE))
    
  }
  
  v = checkwalls()
  if(v$p){
    if(v$dir==1 || v$dir==2){
     
      if (v$pt[2] %in% goals[, 2]) {
        return(FALSE)
      }
      
    }
    if(v$dir==3 || v$dir==4){
     
      if (v$pt[1] %in% goals[, 1]) {
        return(FALSE)
      }
      
    }
  }
  
  return(v$p)
  
}

#' when indexing grid start with cols first (index2) then rows!
#' for each neigh - return its state: player, boxes, path from player to box move
#' deal with box locked position i.e p - 4,4 b1 - 5,4 b2 - 5,3
change_state <- function(neighs, goals, boxes, grid){
  
  g = list()
  
  for (i in 1:length(neighs)){
    curr = neighs[[i]]
    
    
    player = curr$t
    new_player_pos = curr$b
    newpa = curr$pa
    
    new_box_pos <- boxes
    mv1 = "x"
    if (new_player_pos[1]-player[1] >0){
      mv1 = "d"
    }else if(new_player_pos[1]-player[1] <0){
      mv1 = "a"
    }else if(new_player_pos[2]-player[2] >0){
      mv1 = "w"
    }else if(new_player_pos[2]-player[2] <0){
      mv1 = "s"
    }
    # move box in same direction if player on box
    matching_boxes_indices <- which(new_player_pos[1] == boxes[1, ] & new_player_pos[2] == boxes[2, ])
    
    
    c = is.na(matching_boxes_indices[1])
    
    
    if(!c){
      
      change_x = new_player_pos[1] - player[1]
      change_y = new_player_pos[2] - player[2]
      
      new_box_pos[,matching_boxes_indices][1] = new_box_pos[,matching_boxes_indices][1]+change_x
      new_box_pos[,matching_boxes_indices][2] = new_box_pos[,matching_boxes_indices][2]+change_y
      x = new_box_pos[,matching_boxes_indices][1]
      y = new_box_pos[,matching_boxes_indices][2]
      
      # cannot move a box ontop of another
      point_to_check = c(x,y)
      
      is_in_boxes = any(apply(boxes, 2, function(col) all(col == point_to_check)))
      
      if(grid[y,x]=="W"){next}
      if(is_in_boxes){next}
      
      # avoid blocking boxes in corners (corners can be walls or box or both)
      tempboxes = boxes[ , -matching_boxes_indices]
      
      val = checkcorners(point_to_check, grid, tempboxes, goals)$val
      corn = checkcorners(point_to_check, grid, tempboxes, goals)$corner
      if (val){
        next
      }
      #if (!corn){
      #  if(block(point_to_check, grid, tempboxes, goals)){
      #    next
      #  }
      #}
      
      # move box one more time in same direction and check if you block any
      
      # box cannot move onto "1" / x - edge cost
      # if box has moved onto a "."
      
      if(grid[y, x] == "." && !is_in_boxes){
        # use below only for special puzzle
        
        
        newpa[[length(newpa)+1]]=mv1
        g[[i]]=list(player = new_player_pos, boxes = new_box_pos, path = newpa)
        # man dist of states goal box to goal node
        
        next
        
      }else{
        
        next
      }
    }
    
  } # end neighs for loop
  filtered_list <- Filter(Negate(is.null), g)
  return(filtered_list)
}

#' cost from each box to goal
btogoal <- function(boxes,goals,grid){
  t = 0
  for (i in 1:ncol(boxes)){
    box = c(boxes[,i])
    s = bfs(box, goals, grid)
    t = t + s
  }
  return(t)
}

#' postion of player
#' get available pushes (neighbours), return path leading to push
#' change state for each available push. return push direction + path leading to push
astar <- function(goals, boxes, player, grid){
  nodes <- list()
  
  frontier <- list(list(player = player, boxes=boxes, cost=0, path=list()))
  gh = 1
  while( TRUE){
    if(length(frontier)==0){
      print("========================================FAILED================")
      return("failure")
      break
    }
    
    gh = gh+1
    
    ## Select the node, N, with minimal cost in frontier :
    costs <- sapply(frontier, function(item) item$cost)
    
    # Find the index of the element with the lowest cost
    lowest_cost_index <- which.min(costs)
    
    # Access the element with the lowest cost
    lowest_cost_element <- frontier[[lowest_cost_index]]
   
    
    ## Remove N from the frontier
    frontier=frontier[-lowest_cost_index]
    
    all_match <- all(apply(lowest_cost_element$boxes, 2, function(box) any(apply(goals, 2, function(goal) all(identical(box, goal))))))
    
    ## check if N is the goal. changing boxes position
    if(all_match){
      
      #return (list(path = lowest_cost_element$path, player=player, boxes=boxes))
     
      return (list(path = lowest_cost_element$path, numpush=length(nodes)))
      break
    }
    
    # find neighbours
    
   
    neighs = pushes(lowest_cost_element$boxes, grid, lowest_cost_element$player)
    # change state and see if player can be moved into neighs position.
    # for each neigh - return its state: player, boxes, man, path to neigh
  
    
    result = change_state(neighs, goals, lowest_cost_element$boxes ,grid)
    
    if(length(result)==0){next}
    # get man distance from goal_b to goal node for each state
    
    for(i in 1:length(result)){
      path <- lowest_cost_element$path
      res = result[[i]]
      elementcost = btogoal(res$boxes, goals, grid) #+ length(res$path)  #+ length(path) 
      
      
      elementpath = c(path, res$path)
      
      element <- list(player = res$player,
                      boxes = res$boxes,
                      cost=elementcost,
                      path= elementpath
      )
      
      
      ## add neighbours to frontier if not in nodes list
      
      # Check if element is in nodes
      element_in_nodes <- any(sapply(nodes, function(node) all(node$player == element$player & all(node$boxes == element$boxes))))
      
      # Check if element is in frontier
      element_in_frontier <- any(sapply(frontier, function(node) all(node$player == element$player & all(node$boxes == element$boxes))))
      
      compare_cost <- function(el1, el2) {
        return(el1$cost < el2$cost)  # Change to < for higher total cost
      }
      
      # Check if the element is in the frontier with a higher total cost
      element_in_frontier_higherCost <- any(sapply(frontier, function(node) all(node$player == element$player & all(node$boxes == element$boxes) & compare_cost(element, node))))
      
     
      if(!element_in_nodes ){
        frontier[[length(frontier)+1]]= element
        #plot_sokoban(goals, element$boxes, element$player, grid)
      }else if(element_in_frontier_higherCost){
        index_to_replace <- which(sapply(frontier, function(node) all(node$player == element$player & all(node$boxes == element$boxes) & compare_cost(element, node))))
        frontier[[index_to_replace]] <- element
      }
      
    }
    
    nodes[[length(nodes)+1 ]] = list(player = lowest_cost_element$player,
                                     boxes = lowest_cost_element$boxes)
    
    
    
    
  }# end while loop
  
}

plot_sokoban <- function(goals, boxes, player, grid) {
  
  rows <- nrow(grid)
  cols <- ncol(grid)
  
  # Create a blank plot with appropriate dimensions
  plot(0, 0, type = "n", xlim = c(0, cols + 1), ylim = c(0, rows + 1), xlab = "", ylab = "")
  
  
  # Add walls
  walls <- which(grid == "W", arr.ind = TRUE)
  
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


#' change <- to =
run_game <- function(myfunction, goals, boxes, player, grid, pause, toplot){
  original_matrix <- grid
  rotated_matrix <- original_matrix[nrow(original_matrix):1, ]
  grid <- rotated_matrix
  
  if(toplot){
    plot_sokoban(goals, boxes, player, grid)
    
  }
  
  
  start_time <- Sys.time()
  re = myfunction(goals, boxes, player, grid)
  path = re$path
  numpush = re$numpush
  end_time <- Sys.time()
  
  # Calculate elapsed time
  elapsed_time <- end_time - start_time
  
  
  if (!is.list(path)){
    return(list(pass = FALSE))
    break
    }
  #' man play to return path then adjust player and boxes accordingly
  
  nplayer = player
  nboxes = boxes
  
  if(toplot){
    for (i in 1:length(path)){
      
      move = path[[i]]
      
      pb = play_game(goals, nboxes, nplayer, grid, move)
      nplayer = pb$player
      nboxes = pb$boxes
      
      
      all_match <- all(apply(nboxes, 2, function(box) any(apply(goals, 2, function(goal) all(identical(box, goal))))))
      
      plot_sokoban(goals, nboxes, nplayer, grid)
      if(all_match){
        
        return(list(pass=TRUE, runtime = elapsed_time, 
                    len=length(path), numpushes=numpush))
        #return('congrats! puzzle solved')
      }
      
      
      Sys.sleep(pause)
      
      
    }
  }else{
    
    return(list(pass=TRUE, runtime = elapsed_time, 
                len=length(path), numpushes=numpush))
  }
  
}

run <- function(puzzle_number, toplot){
  data = getgrid(puzzle_number, grids)
  sokoban_grid = data$grid
  goals = data$goals
  boxes = data$boxes
  player = data$player
  
  
  return(run_game(astar, goals, boxes, player, sokoban_grid, 0.3, toplot))
  
}

run(4, TRUE)
#b = run_game(astar, goals, boxes, player, sokoban_grid, 0.5, TRUE)
#test(run, returnmatrix=TRUE, plotgames=FALSE, timeLimit=600, numgames=20)

