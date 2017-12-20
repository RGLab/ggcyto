#' extend the original flowWorkspace:::.mergeGates function to restore quadGate when applicable
#'
#' For internal usage.
#' 
#' @param gh a GatingHierarchy
#' @param pops a vector of population names
#'
#' @return a nested list of data structure that captures the information of parent, grouped populations (with the same projections)
#' and the reconstructed quadGate object and the respective quadrant pattern
#'
#' @examples
#' library(flowWorkspace)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(file.path(dataDir, "gs_manual"))
#' #get the GatingHierarchy object
#' gh <- gs[[1]]
#' pops <- getChildren(gh, "CD4")
#' grps  <- ggcyto:::merge.quad.gates(gh, pops)
#' length(grps) # pops are grouped into two
#' grps[[1]] # each group is annotaed with quadGate information
#'
#' ggcyto:::merge.quad.gates(gh, getChildren(gh, "CD3+")) # cd3 subsets are not coercible to quadgate thus return as they are
merge.quad.gates <- function(gh, pops, bool = TRUE){
  #split pops into groups based on parent and projections
  groups <- flowWorkspace:::.mergeGates(gh, pops, bool = bool, merge = TRUE)
  #try to parse each group and attach quadgate info as needed
  lapply(groups, function(grp){

    if(is.list(grp))
    {
      pops <- grp[["popIds"]]

      if(length(pops) == 4)
      {
        env <- new.env(parent = emptyenv())
        env[["isQuad"]] <- TRUE
        #extract coord and check the number of points first
        points.list <- lapply(pops, function(pop){
          gate <- getGate(gh, pop)
          df <- as.data.frame(fortify(gate))
          df <- unique(df)

          if(nrow(df)!=4)
            env[["isQuad"]] <- FALSE
          df
          })
        #keep checking by selecting the common point
        if(env[["isQuad"]])
        {
          common.points <- Reduce(merge, points.list)
          common.points <- unique(common.points) #remove the redundancy of float number
          if(nrow(common.points)!=1)
            env[["isQuad"]] <- FALSE
        }
        #check the relative position of coord
        if(env[["isQuad"]])
        {
          quad.patterns <- sapply(points.list, function(points){
            quad.pattern <- sapply(1:2, function(i){
              dif <- points[, i] - common.points[, i]
              if(all(dif == 0))
                env[["isQuad"]] <- FALSE
              else if(all(dif >= 0))
                return("+")
              else if(all(dif <= 0))
                return("-")
              else
                env[["isQuad"]] <- FALSE
          })
          paste(quad.pattern, collapse = "")

          })

        }

        if(env[["isQuad"]])
        {
          quad.gate <- quadGate(.gate = common.points)
          grp[["popIds"]] <- list(quad.gate = quadGate(.gate = common.points)
                                  , quad.pattern = quad.patterns
                                  , pop.name = pops)
        }

      }

    }
    return (grp)
  })
}