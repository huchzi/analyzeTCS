#' Calculates scores for the Panel D15 according to Vingrys et al.
#'
#' Calculates scores for the Panel D15 according to Vingrys et al.
#' @param caps a vector of cap numbers
#' @return a list with parameters
#' @export
vingrys <- function(caps)
{
  if (is.na(min(caps))) return(data.frame(    angle = NA,
                                        major = NA,
                                        minor = NA,
                                        TES = NA,
                                        sIndex = NA,
                                        cIndex = NA))
  u <- analyzeTCS::D15_vectors$u
  v <- analyzeTCS::D15_vectors$v
  
  caps <- c(0, caps) + 1
  du <- sapply(2:16, function (x) return(u[caps][x] - u[caps][x - 1]))
  dv <- sapply(2:16, function (x) return(v[caps][x] - v[caps][x - 1]))
  
  v2 <- sum(dv^2)
  u2 <- sum(du^2)
  uv <- sum(du * dv)
  
  if ((u2 - v2) == 0) { a0 <- 0.7854 } else { a0 <- atan(2 * uv / (u2 - v2)) / 2 }
  i0 <- u2 * sin(a0)^2 + v2 * cos(a0)^2 - 2 * uv * sin(a0) * cos(a0)
  if (a0 < 0) { a1 <- a0 + 1.5708 } else { a1 <- a0 - 1.5708 }
  i1 <- u2 * sin(a1)^2 + v2 * cos(a1)^2 - 2 * uv * sin(a1) * cos(a1)
  if (i1 > i0)
  {
    p <- a0
    a0 <- a1
    a1 <- p
    p <- i0
    i0 <- i1
    i1 <- p
  }
  r0 <- sqrt(i0 / 15)
  r1 <- sqrt(i1 / 15)
  r <- sqrt(r0 ^ 2 + r1 ^ 2)
  r2 <- 9.234669
  
  result <- data.frame(
    angle = 57.3 * a1,
    major = r0,
    minor = r1,
    TES = r,
    sIndex = r0 / r1,
    cIndex = r0 / r2
  )
  
  return(result)
}

# examples from the publication
# n <- c(0, 15, 1, 14, 2, 13, 12, 3, 4, 11, 10, 5, 9, 6, 8, 7) + 1 # protanope
# n <- c(0, 1, 2, 3, 4, 5, 6, 7, 15, 14, 13, 12, 11, 10, 9, 8) + 1
# n <- c(0, 1, 2, 3, 4, 5, 6, 7, 9, 8, 10, 11, 12, 13, 14, 15) + 1
