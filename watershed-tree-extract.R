# get extents of points
get_extents <- function(pts, buf) {
  x = pts[,1]
  y = pts[,2]
  minX = min(x) - buf
  maxX = max(x) + buf
  minY = min(y) - buf
  maxY = max(y) + buf
  ext_list <- c(minX, maxX, minY, maxY)
}

# crop raster with extent
crop_raster <- function(ras, pts, buf) {
  # pts is a list of x,y
  # make extent list based on points
  extent_list = get_extents
  # extent_list is in form c(minX, maxX, minY, maxY)
  ras_c <- crop(ras, extent_list)
}

# reverse chm
reverse_chm <- function(chm) {
  rhm <- chm*-1
}

# fill chm_r
fill_pits <- function(rhm) {
  
  
}

# find flow direction
flow_dir <- function(ras) {
  ras_flow <- ras
  ord = c(-1,0,1)
  
  # walk through each point in raster
  for (c in 2:(ncol(ras)-1)) {
    for (r in 2:(nrow(ras)-1)) {
      # arrive at raster grid
      
      # make temp record of change in elevation
      neighbor_list = c()
      for (dy in ord) {
        #print(dy)
        for (dx in ord) {
          elev_diff <- c()
          if (!is.na(ras[r,c])) {
            if (!is.na(ras[r+dx, c+dy]) ) {
              elev_diff = ras[r,c] - ras[r+dx, c+dy]
              #print(elev_diff)
            } 
          }
          neighbor_list <- c(neighbor_list, elev_diff)
          print(neighbor_list)
        }
      }
      # finished grid, find values for current grid
      print(paste("r", r))
      print(paste("c", c))
      print(ras_flow[r, c])
      print(which.max(neighbor_list))
      ras_flow[r,c] <- which.max(neighbor_list)
    }
    
  }
  # ras_flow[1,] <- NA
  # ras_flow[nrow(ras),] <- NA
  # ras_flow[,ncol(ras)] <- NA
  # ras_flow[,1] <- NA
  print(ras_flow)
  ras_flow
}

# find flow accumulation
# define outlets

# 
# Save as shapefile
# writePointsShape(locs.gb, "locsgb")

