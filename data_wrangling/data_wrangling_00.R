library(tidyverse)
library(stringr)
library(httr)

#' @examples
#' \donttest{
#' ## get the play-by-play data for the San Francisco Giants' 2012 season
#' getRetrosheet("play", 2012, "SFN")
#'
#' ## get the roster data for the 1995 season, listed by team
#' getRetrosheet("roster", 1995)
#' 
#' ## get the team IDs forthe 2010 season
#' getTeamIDs(2010)
#' }

#########################################
################ THE CODE ###############
#########################################

#' TYPE in {"play", "roster"}
getRetrosheet <- function(type, year, team) {
  type <- match.arg(type, c("game", "play", "roster", "schedule"))
  
  if(type == "play" && missing(team)) {
    stop("argument 'team' must be supplied when 'type = \"play\"")
  }
  
  path <- switch(type,
                 "game" = "/gamelogs/gl%d.zip",
                 "play" = "/events/%deve.zip",
                 "roster" = "/events/%deve.zip",
                 "schedule" = "/schedule/%dSKED.ZIP")
  
  # Download to a temp location
  fullPath <- sprintf(paste0("https://www.retrosheet.org", path), year)
  #browser()
  if(!http_error(fullPath)) {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    RETRY("GET", url = fullPath, write_disk(tmp, overwrite=TRUE), timeout(15))
  } else {
    stop(sprintf("'%s' is not a valid url or path", fullPath))
  }
  fname <- unzip(tmp, list = TRUE)$Name
  

  if(type == "roster") {
    rosFiles <- grep(".ROS", fname, value = TRUE, fixed = TRUE)
    read <- lapply(rosFiles, function(x) {
      zcon <- unz(tmp, filename = x)
      o <- read.csv(zcon, header = FALSE, 
                    stringsAsFactors = FALSE,
                    col.names = c("retroID", "Last", "First", "Bat", "Throw", "Team", "Pos"))
      tibble(o)
    })
    out <- setNames(read, substr(rosFiles, 1L, 3L))
    return(out)
  }
  zcon <- unz(tmp, filename = paste0("TEAM", year))
  allTeams <- readLines(zcon)
  close(zcon)
  team <- match.arg(team, substr(allTeams, 1L, 3L))
  
  rgx <- paste(team, "EV", sep = ".")
  fnm <- grep(rgx, fname, value = TRUE, fixed = TRUE)
  zcon <- unz(tmp, filename = fnm)
  r <- readLines(zcon)
  close(zcon)
  g <- grepl("^id", r)
  sr <- unname(split(gsub("\"", "", r), cumsum(g)))
  ####### HERE
  sr
}

getTeamIDs <- function(year) {
  stopifnot(is.numeric(year), length(year) == 1L)
  path <- sprintf("https://www.retrosheet.org/events/%deve.zip", year)
  if (!http_error(path)) {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    RETRY("GET", url = path, write_disk(tmp, overwrite=TRUE), timeout(15))
  } else {
    available <- grep(year, getFileNames()$event)
    if(!length(available)) {
      return(NA)
      
    }
  }
  fname <- paste0("TEAM", year)
  unzip(tmp, files = fname)
  on.exit(unlink(fname), add = TRUE)
  
  read <- suppressWarnings(read.csv(fname, header = FALSE, stringsAsFactors = FALSE))[c(1, 4)]
  
  out <- structure(read[[1L]], .Names = read[[2L]])
  unname(out)
}

#########################################
################ EXAMPLES ###############
#########################################

# {
#   x = getRetrosheet("play", 2012, "LAN")
#   x
#   y = getRetrosheet("roster", 1995)
#   y
#   z = getTeamIDs(2010)
#   z
# }
