testalgo <- setRefClass("testalgo", 
    contains=c("algo"),
	fields = list(wait="numeric", ordercount="numeric"), 
    methods=list( 
	    trade = function() {
			if (!length(.self$wait) || .self$wait < -10) {
			    .self$wait <- -10
			}
			if (!length(.self$ordercount) || .self$ordercount< 0) {
			    .self$ordercount <- 0
			}
			ticker <- .self$watchlist[[1]]
			order <- list(ticker=ticker, 
						 type="LIMIT", 
						 buy=T, 
						 size=abs(floor((rnorm(1)*240))+990), 
						 price=as.numeric(.self$price(ticker)[1]-0.01))
			if (length(.self$openorders()) < 8 & .self$ordercount<20) {
			    .self$placeorder(order)
				order$buy <- FALSE
				order$price <- order$price + 0.02
			    .self$placeorder(order)
				.self$wait <- .self$wait + 1
				.self$ordercount <- .self$ordercount + 1
			} else if (.self$wait<2){ 
				howmany <- 0
				for (order in .self$openorders()) {
					cat("Canceling order:", order$id, "\n")
					.self$cancelorder(order$id)
					.self$ordercount <- .self$ordercount - 1
					howmany <- howmany + 1
					if (howmany > 30) {
					    break
					}
				}
				.self$wait <- 25
			} else {
			    .self$wait <- .self$wait - 1
			}

    })
)


