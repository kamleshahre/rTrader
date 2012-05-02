# load packages

require("websockets")
require("RJSONIO")

# load modules

import <- function(dir, e=.GlobalEnv){ 
    assign("__path__",dir,envir=e) 
    reload(e) 
    invisible(e) 
} 

reload <- function(e){ 
	path = get("__path__",e) 
    files = list.files(path,".R$",full.names=TRUE,recursive=TRUE,ignore.case=TRUE) 
    for(f in files){ 
	    sys.source(f,envir=e) 
    } 
}

import("./modules/utils")
import("./modules/core")
import("./modules/algos")


## main
host <- "localhost"
port <- 5000

Stream <- websocket(host, port=port)

setup <- function(algos) {

	algos <- c(algos)

	Active <- list()
    
	for (algo in algos) {
        .it <- algo$new(stream=Stream, watchlist=c("MSFT", "GOOG"))	
		Active <- c(Active, .it)
	}

	Active

}

run_them_all <- function(DATA, WS, HEADER) {
   
	updateStream(rawToChar(DATA), Stream)
	for (algo in Active) {
        algo$trade()	
	}

}


Active <- setup(c(testalgo))

set_callback("receive", run_them_all, Stream)

timer <- 0
while(TRUE) {
    timer <- timer + 1
	if (timer > 9000) {
        service(Stream)
		timer <- 0
	}
}
