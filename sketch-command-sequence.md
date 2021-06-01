# User Input
1. RMaxima::execute(command)
	-> MaximaChain::executeCommand(command) (1)
		-> MaximaChain::crudeExecute(command) (2)
			-> MaximaChain::sendCommand(command) (3)
				-> MaximaChain::checkInput(command[i], state) (4)
				-> Applies MaximaIOHookRegExp
		-> MaximaChain::readReply() (5)
			-> 
						-> Reply constructor (r)

		-> if(reply->outs.isempty()) ... 
			-> requireUser() (6)
			-> CheckPrompt() (MaximaChain.h)
			-> isInterrupted() (7)
			-> checkMaximaError() (8)
		-> Check if output reference label is in output-delimiters
			-> copy label number to prompt ID
			-> return result
		-> MaximChain::crudeExecute(";")
		-> Rcpp::stop("Unsupported: " + reply->concatenateParts())
			-> reply->concatenateParts() join betweens and outs to one string

