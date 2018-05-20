####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################

CypherFile<-function(infile,outfile,seed){

alphStd<- vector(mode='list', length=36)
alphNumStd<- vector(mode='list', length=36)
names(alphStd) <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
alphStd[["a"]] <- 1; alphStd[["b"]] <- 2; alphStd[["c"]] <- 3; alphStd[["d"]] <- 4; alphStd[["e"]] <- 5; alphStd[["f"]] <- 6; alphStd[["g"]] <- 7; alphStd[["h"]] <- 8; alphStd[["i"]] <- 9; alphStd[["j"]] <- 10; alphStd[["k"]] <- 11; alphStd[["l"]] <- 12; alphStd[["m"]] <- 13; alphStd[["n"]] <- 14; alphStd[["o"]] <- 15; alphStd[["p"]] <- 16; alphStd[["q"]] <- 17; alphStd[["r"]] <- 18; alphStd[["s"]] <- 19; alphStd[["t"]] <- 20; alphStd[["u"]] <- 21; alphStd[["v"]] <- 22; alphStd[["w"]] <- 23; alphStd[["x"]] <- 24; alphStd[["y"]] <- 25; alphStd[["z"]] <- 26; alphStd[["0"]] <- 27; alphStd[["1"]] <- 28; alphStd[["2"]] <- 29; alphStd[["3"]] <- 30; alphStd[["4"]] <- 31; alphStd[["5"]] <- 32; alphStd[["6"]] <- 33; alphStd[["7"]] <- 34; alphStd[["8"]] <- 35; alphStd[["9"]] <- 36
names(alphNumStd) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36)
alphNumStd[[1]] <- "a"; alphNumStd[[2]] <- "b"; alphNumStd[[3]] <- "c"; alphNumStd[[4]] <- "d"; alphNumStd[[5]] <- "e"; alphNumStd[[6]] <- "f"; alphNumStd[[7]] <- "g"; alphNumStd[[8]] <- "h"; alphNumStd[[9]] <- "i"; alphNumStd[[10]] <- "j"; alphNumStd[[11]] <- "k"; alphNumStd[[12]] <- "l"; alphNumStd[[13]] <- "m"; alphNumStd[[14]] <- "n"; alphNumStd[[15]] <- "o"; alphNumStd[[16]] <- "p"; alphNumStd[[17]] <- "q"; alphNumStd[[18]] <- "r"; alphNumStd[[19]] <- "s"; alphNumStd[[20]] <- "t"; alphNumStd[[21]] <- "u"; alphNumStd[[22]] <- "v"; alphNumStd[[23]] <- "w"; alphNumStd[[24]] <- "x"; alphNumStd[[25]] <- "y"; alphNumStd[[26]] <- "z"; alphNumStd[[27]] <- "0"; alphNumStd[[28]] <- "1"; alphNumStd[[29]] <- "2"; alphNumStd[[30]] <- "3"; alphNumStd[[31]] <- "4"; alphNumStd[[32]] <- "5"; alphNumStd[[33]] <- "6"; alphNumStd[[34]] <- "7"; alphNumStd[[35]] <- "8"; alphNumStd[[36]] <- "9"

#Functions
Char_to_Num <- function(x){
  y<-vector()
  for (i in 1:nchar(x)){
    CurChar <- substr(x, i,i)
    y <- c(y, alphStd[[CurChar]][1])
    if (is.null(alphStd[[CurChar]][1])){
      y <- c(y, CurChar)
    }
  }
  return(y)
}

Translate <- function(x, t){
  y<-vector()
  n<-0
  for (i in 1:length(x)){
    C <- as.integer(x[i])
    if (!is.na(C)){
      n<-n+1
      C<-C+as.integer(t[n])
      if (C>36){C<-C-36}
      y<-c(y, as.character(C))
    }
    else{y<-c(y, x[i])}
  }
  return(y)
}

Num_to_Char <- function(x){
  y<-vector()
  for (i in 1:length(x)){
    C <- as.integer(x[i])
    if (!is.na(C)){
      y<-c(y, alphNumStd[[C]][1])
    }
    else{y<-c(y, x[i])}
  }
  return(y)
}

#Set the seed/cypher
cypher <- Char_to_Num(seed)
cypher <- cypher[! cypher %in% " "]

#Load the note
notePath <- infile
note <- readChar(notePath, file.info(notePath)$size)
note_num <- Char_to_Num(note)

#Get translator to a vector of appropriate length
translator <- vector()
while (length(translator)<nchar(note)){
  translator <- c(translator, cypher)
}

#Run functions
note_NEWnum<-Translate(note_num, translator)
note_translated <- Num_to_Char(note_NEWnum)

#Write output
write(note_translated, file=outfile, ncolumns=length(note_translated), sep="")
}

CypherStr<-function(input,seed){alphStd<- vector(mode='list', length=36)
alphNumStd<- vector(mode='list', length=36)
names(alphStd) <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
alphStd[["a"]] <- 1; alphStd[["b"]] <- 2; alphStd[["c"]] <- 3; alphStd[["d"]] <- 4; alphStd[["e"]] <- 5; alphStd[["f"]] <- 6; alphStd[["g"]] <- 7; alphStd[["h"]] <- 8; alphStd[["i"]] <- 9; alphStd[["j"]] <- 10; alphStd[["k"]] <- 11; alphStd[["l"]] <- 12; alphStd[["m"]] <- 13; alphStd[["n"]] <- 14; alphStd[["o"]] <- 15; alphStd[["p"]] <- 16; alphStd[["q"]] <- 17; alphStd[["r"]] <- 18; alphStd[["s"]] <- 19; alphStd[["t"]] <- 20; alphStd[["u"]] <- 21; alphStd[["v"]] <- 22; alphStd[["w"]] <- 23; alphStd[["x"]] <- 24; alphStd[["y"]] <- 25; alphStd[["z"]] <- 26; alphStd[["0"]] <- 27; alphStd[["1"]] <- 28; alphStd[["2"]] <- 29; alphStd[["3"]] <- 30; alphStd[["4"]] <- 31; alphStd[["5"]] <- 32; alphStd[["6"]] <- 33; alphStd[["7"]] <- 34; alphStd[["8"]] <- 35; alphStd[["9"]] <- 36
names(alphNumStd) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36)
alphNumStd[[1]] <- "a"; alphNumStd[[2]] <- "b"; alphNumStd[[3]] <- "c"; alphNumStd[[4]] <- "d"; alphNumStd[[5]] <- "e"; alphNumStd[[6]] <- "f"; alphNumStd[[7]] <- "g"; alphNumStd[[8]] <- "h"; alphNumStd[[9]] <- "i"; alphNumStd[[10]] <- "j"; alphNumStd[[11]] <- "k"; alphNumStd[[12]] <- "l"; alphNumStd[[13]] <- "m"; alphNumStd[[14]] <- "n"; alphNumStd[[15]] <- "o"; alphNumStd[[16]] <- "p"; alphNumStd[[17]] <- "q"; alphNumStd[[18]] <- "r"; alphNumStd[[19]] <- "s"; alphNumStd[[20]] <- "t"; alphNumStd[[21]] <- "u"; alphNumStd[[22]] <- "v"; alphNumStd[[23]] <- "w"; alphNumStd[[24]] <- "x"; alphNumStd[[25]] <- "y"; alphNumStd[[26]] <- "z"; alphNumStd[[27]] <- "0"; alphNumStd[[28]] <- "1"; alphNumStd[[29]] <- "2"; alphNumStd[[30]] <- "3"; alphNumStd[[31]] <- "4"; alphNumStd[[32]] <- "5"; alphNumStd[[33]] <- "6"; alphNumStd[[34]] <- "7"; alphNumStd[[35]] <- "8"; alphNumStd[[36]] <- "9"

#Functions
Char_to_Num <- function(x){
  y<-vector()
  for (i in 1:nchar(x)){
    CurChar <- substr(x, i,i)
    y <- c(y, alphStd[[CurChar]][1])
    if (is.null(alphStd[[CurChar]][1])){
      y <- c(y, CurChar)
    }
  }
  return(y)
}

Translate <- function(x, t){
  y<-vector()
  n<-0
  for (i in 1:length(x)){
    C <- as.integer(x[i])
    if (!is.na(C)){
      n<-n+1
      C<-C+as.integer(t[n])
      if (C>36){C<-C-36}
      y<-c(y, as.character(C))
    }
    else{y<-c(y, x[i])}
  }
  return(y)
}

Num_to_Char <- function(x){
  y<-vector()
  for (i in 1:length(x)){
    C <- as.integer(x[i])
    if (!is.na(C)){
      y<-c(y, alphNumStd[[C]][1])
    }
    else{y<-c(y, x[i])}
  }
  return(y)
}

#Set the seed/cypher
cypher <- Char_to_Num(seed)
cypher <- cypher[! cypher %in% " "]

#Load the note
note<-input
note_num <- Char_to_Num(note)

#Get translator to a vector of appropriate length
translator <- vector()
while (length(translator)<nchar(note)){
  translator <- c(translator, cypher)
}

#Run functions
note_NEWnum<-Translate(note_num, translator)
note_translated <- Num_to_Char(note_NEWnum)


#Write output
return(paste(as.vector(note_translated), collapse=""))
}

