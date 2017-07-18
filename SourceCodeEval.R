

Input_Values <- function (){
	File_Name <- readline('Please input a file name: ')
	fil <- readLines(File_Name)
	fil <<- unlist(fil)

	# No input validation, because for now I am the only user

	Language <- readline('Please input the language of this file: ')
	
	Answers <- list(fil,Language)
	
	return (Answers)
}

Eval_Input <- function (fil,Language) {
	Dictionary <- list(Language)
	fil <- unlist(fil)
	for (i in 1:length(fil)) {
	  Current_String <- strsplit(fil[i],'')
	  Current_String <- unlist(Current_String)
	  
		y <- 0
		Last_Eval <- TRUE
		
		Words <- list()
		cat(length(Current_String))
		
		
		for (j in 1:length(Current_String)) {
		  if (length(Current_String) == 0) {
		    break
		  }
			if (grepl('\\s',Current_String[j]) == TRUE) {
				#pass over the spaces
			  Last_Eval <- TRUE
			} else if (grepl('\\w|\\(|\\)',Current_String[j]) == TRUE){
			 if (Last_Eval == TRUE){
			    y <- y+1
			    Words[y] <- Current_String[j]
			    Last_Eval <- FALSE
			  }else{
			    Words[y] <- paste(Words[y],Current_String[j],sep='')
			  }
			} else {
				#Add a function to check if parentheses has text on both sides
			  
			  Last_Eval <- TRUE
			}
			
			
		}
		
		for (i in 1:length(Words)) {
		  if (length(Words) == 0) {
		    next
		  }
		  if (grepl('\\w',Words[i]) == FALSE){
		    Words[i] <- NULL
		  }
		}
		
		for (i in 1:length(Words)) {
		  Dictionary <- c(Dictionary,Words[i])
		}
		
		
	}
	
	return(Dictionary)
}

Main_Menu <- function() {
	
	done <- 0
	
	while (done == 0) {
		cat('\nSOURCE CODE EVALUATOR\n')
		cat('MAIN MENU\n')
		cat('~~~~~~~~~~~~~~~~~~~~~\n')
		cat('1.Load File(s) and Language\n')
		cat('2.Evaluate Data...\n')
		cat('3.Quit\n')
		input <- readline('Which would you like to do? ')
		
		switch(input,
		'1' = Input_List <- Input_Values(),
		'2' = (Data_Menu(Input_List[1],Input_List[2])),
		'3' = done <- 1,
		cat('the input was not a 1,2, or 3'))
	}
}

Input_Stats<- function(fil,Language,Dictionary) {
  Dictionary <- unlist(Dictionary)
  Dee <<- Dictionary
  c <- 1
  #assigning the list a max size to save time
  Counted_Words <- vector('list')
  New_Dict <- vector()
  for (i in 1:length(Dictionary)) {
    Counted_Words[Dictionary[i]] = 1
    for (j in 1:(length(Dictionary))) {
      if (i == j) {
        next()
      }
      else if (Dictionary[i] == Dictionary[j]) {
        Counted_Words[Dictionary[i]] <- Counted_Words[[Dictionary[i]]]+1
      }
      
    }
  }
  #here to test
  
  poo <<- Counted_Words
  pee <<- New_Dict
  
  y <- list(Counted_Words,New_Dict)
  
  return(y)
  
}

Data_Menu <- function(fil,Language) {
  #All stats are done here, then display method is done later
  Dictionary <- Eval_Input(fil,Language)
  Word_Base <- Input_Stats(fil,Language,Dictionary)
  y <<- Word_Base
  done <- 0
  
  while (done == 0) {
    cat('\nSOURCE CODE EVALUATOR\n')
    cat('DATA MENU\n')
    cat('~~~~~~~~~~~~~~~~~~~~~\n')
    cat('1.Show Stats and Graphs (in console)\n')
    cat('2.Print out Stats and Graphs\n')
    cat('3.Go back to Main Menu\n')
    input <- readline('Which would you like to do? ')
    
    switch(input,
           '1' = CatStats(Word_Base[1],Word_Base[2]),
           '2' = ,
           '3' = done <- 1,
           cat('the input was not a 1,2, or 3'))
  }
}

CatStats <- function(New_Dict,Stat_List){
  cat('Here is the entire Dictionary and amount of times the word is found in this file\n')
  cat('Word | Amount of Times Appeared\n')
  for (i in 1:length(Stat_List)){
    string <- paste(New_Dict[i],' | ',Stat_List[[i]],'\n')
    cat(string)
  }
}

Main_Menu()
