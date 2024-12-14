## HEADER ####
## who: J Collins
## what: Experimental design script 
## when: 

## CONTENTS ####
## 00 Setup
## 01 <replace with brief decription of 1st section>
## 02 <replace with brief decription of 2nd section>
## 03 <etc>

## 00 Setup ####

#define vector of packages to load
some_packages <- c('tidyverse', 'agricolae', 'agricolaeplotr', "desplot")

#load all packages at once
lapply(some_packages, library, character.only=TRUE)


## Experiment Parameters ####

# 2 pesiticides
TrtPest   <- paste0("P", 1:2)    # P1 & P2
n_TrtPest <- n_distinct(TrtPest) # 2

# 4 soil treatments
TrtSoil   <- paste0("S", 1:4)    # S1 - S4
n_TrtSoil <- n_distinct(TrtSoil) # 4

# 6 fertilizer
TrtFert   <- paste0("N", 1:6)    # N1 - N6
n_TrtFert <- n_distinct(TrtFert) # 6

# 15 genotpyes
TrtGen    <- paste0("G", 1:15)   # G1 - G15
n_TrtGen  <- n_distinct(TrtGen)  # 15

# 2 check varieties
Checks    <- c("Std_A", "Std_B")

# number of replicates
n_Reps    <- 4



## RCBD - Randomized complete block design ####

rcbd_out <- design.rcbd(trt = TrtSoil,
                        r = n_Reps,
                        seed = 42)

# Add Row and Col 
rcbd_out$bookRowCol <- rcbd_out$book %>% 
  mutate(Row = block %>% as.integer) %>% 
  group_by(Row) %>% 
  mutate(Col = 1:n()) %>% 
  ungroup()

# Plot field layout
desplot(TrtSoil ~ Row + Col, flip = TRUE,
        text = TrtSoil, cex = 1, shorten = "no",
        out1 = block,
        data = rcbd_out$bookRowCol,
        main = "randomized complete block design", 
        show.key = T, key.cex = 0.5)




## RCBD Facotrial - Randomized complete block design ####

fac2rcbd_out <- design.ab(trt = c(n_TrtFert, n_TrtSoil), 
                          design = "rcbd",
                          r = n_Reps, 
                          seed = 42)

# Add Row and Col 
fac2rcbd_out$bookRowCol <- fac2rcbd_out$book %>%
  bind_cols(expand.grid(Row = 1:n_TrtFert,
                        Col = 1:(n_TrtSoil*n_Reps))) %>% 
  mutate(TrtFert = paste0("N", A),
         TrtSoil = paste0("S", B))

# Plot field layout
desplot(block ~ Col + Row | block, flip = TRUE,
        out1 = Row, out1.gpar = list(col = "grey", lty = 1),
        out2 = Col, out2.gpar = list(col = "grey", lty = 1), 
        text = TrtFert, cex = 1, shorten = "no", col=TrtSoil,
        data = fac2rcbd_out$bookRowCol,
        main = "RCBD", 
        show.key = T, key.cex = 0.5)















