joint_pres <- function (dat, left, right) sum(dat[left, ]>0&dat[right, ]>0)
joint_abs <- function (dat, left, right) sum(dat[left, ]  ==  0&dat[right, ]  ==  0)
left_pres <- function (dat, left, right) sum(dat[left, ]>0&dat[right, ]  ==  0)
right_pres <- function (dat, left, right) sum(dat[left, ]  ==  0&dat[right, ]>0)


association <- function (dat, index = "matching") {
                       INDICES <- c("matching", "rogers", "mweight", 
                                  "jaccard", "sorensen", "simratio", 
                                  "whittaker")  
                       index <- match.arg(index, INDICES)
                       
                       S <- matrix(nrow = nrow(dat), ncol = nrow(dat))
                       dimnames(S) <- list(rownames(dat), rownames(dat))
                       
                       if (index  ==  "whittaker") {
                            matr <- dat
                            for (i in 1:nrow(matr)) {
                                    matr[i, ] <- dat[i, ]/sum(dat[i, ])
                            }
                            for (right in 1:nrow(matr)) {
                                #cat(round(right/nrow(matr)*100, 2), 
                                #     "%", "done", "\n")
                                for (left in right:nrow(matr)) {
                                   S[left, right] <- 0.5 *
                                    sum(abs(matr[left, ] - matr[right, ]), 
                                        na.rm = T)
                                }
                            }
                            D <- as.dist(round(S, 2), upper = T)
                        }
                        else {
                       for (right in 1:nrow(dat)) {
                            for (left in right:nrow(dat)) {

                               a <- joint_pres(dat, left, right)
                               b <- left_pres(dat, left, right)
                               c <- right_pres(dat, left, right)
                               d <- joint_abs(dat, left, right)
                               
                                if (index  ==  "matching") {
                                    S[left, right] <- (a+d)/(a+b+c+d)
                                }
                                if (index  ==  "rogers") {
                                    S[left, right] <- (a+d)/(a+2*b+2*c+d)
                                }
                                if (index  ==  "mweight") {
                                    S[left, right] <- 2*(a+d)/(2*a+b+c+2*d)
                                }
                                if (index  ==  "jaccard"){
                                    S[left, right] <- a/(a+b+c)
                                }
                                if (index  ==  "sorensen") {
                                    S[left, right] <- 2*a/(2*a+b+c)
                                }
                                
                                if (index  ==  "simratio") {
                                    S[left, right] <- sum(dat[left, ] * 
                                    dat[right, ])/
                                    (sum(dat[left, ]^2+dat[right, ]^2-
                                     dat[left, ]*dat[right, ]))
                                }
                               }
                        }
                        D <- as.dist(round(1-S, 2), upper = T)
                        }
                        return(D)
}





pairwise.anosim <- function (matr, grouping, index = "chord") {
                 INDICES <- c("matching", "rogers", "mweight", 
                              "jaccard", "sorensen", "simratio", 
                              "whittaker", "euclidean", "orchiai", 
                              "chord", "bray")  
                 index <- match.arg(index, INDICES)
                 if (index == "jaccard") index <- "steinhaus"
                 if (index == "orchiai") index <- "ochiai"

    dis <- matrix(ncol = 4, 
              nrow = sum(seq(1 : (length(unique(grouping)) - 1))))
    dis <- as.data.frame(dis)
    colnames(dis)[1] <- "day_i"
    colnames(dis)[2] <- "day_j"
    colnames(dis)[3] <- "stat"
    colnames(dis)[4] <- "pval"
    num <- 1

    for (i in 1:(length(unique(grouping)) - 1)) {
        for (j in (i+1):length(unique(grouping))) {

             dis[num, 1] <- unique(grouping)[i]
             dis[num, 2] <- unique(grouping)[j]

             dat <- matr[grouping == unique(grouping)[i]|
                         grouping == unique(grouping)[j], ]
             group <- grouping[grouping == unique(grouping)[i]|
                               grouping == unique(grouping)[j]]

             if (index %in% c("matching", "rogers", "mweight", 
                              "simratio", "whittaker")) {
                 distmatr <- association(dat, index)
             }

             if (index %in% c("steinhaus", "sorensen", "bray", "ochiai")) {
                 library(labdsv)
                 distmatr <- dsvdis(dat, index)
             }
    
             if (index == "chord") {
                 library(vegan)
                 distmatr <- dist(decostand(dat, "norm"), "euclidean")
             }
    
             if (index == "euclidean") {
                 distmatr <- dist(dat, index)
             }

             temp <- anosim(distmatr, grouping)
             dis[num, 3] <- round(temp$statistic, 3)
             dis[num, 4] <- round(temp$signif, 3)
             num <- num+1
        }
    }

    return (dis)
}





