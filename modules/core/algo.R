require("RJSONIO")
require("digest")
require("websockets")



updateStream <- function(json_data, stream) {
    data <- fromJSON(json_data)
	stream$portfolio <- data$port
	stream$ob <- data$port$orderBook
	stream$event <- data$event
	stream$event.info <- data$data
	stream$prices <- data$port$LastPrice
}

algo <- setRefClass("algo", 
    fields = list(
		watchlist = "vector", 
		stream = "environment" 
	), 
    methods = list(
	    price = function(tickers=.self$watchlist) {
			tickers = c(tickers)
	        prices = .self$stream$prices               	
			prices[names(prices) %in% tickers]
		},             			   
		openorders = function() {
		    .self$stream$ob$orders
		}, 
		placeorder = function(order) {
			if (is.null(order$tracker)) {
			    tracker = digest(c(.self, Sys.time())) 
				order$tracker <- tracker
				msg <- list()
				msg$incoming_order = order
				msg <- toJSON(msg)
				websocket_write(msg, .self$stream)
				cat("Placed Order:", msg, "\n")
            } else if (is.null(.self$stream$ob$byOrderId[[order$tracker]])) {
				msg <- list()
				msg$incoming_order = order
				msg <- toJSON(msg)
				websocket_write(msg, .self$stream)
				cat("Placed Order:", msg, "\n")
			}
			invisible(.self)
		}, 
		cancelorder = function(order_id) {
			if (!is.null(.self$stream$ob$byOrderId[[order_id]])) {
				msg <- list()
			    msg$cancel_order = order_id
				msg <- toJSON(msg)
			    websocket_write(msg, .self$stream)
				cat("\n", msg)
            } else {
			    cat("No order exists with that id.")
			}
			invisible(.self)
		}
	)			
)


