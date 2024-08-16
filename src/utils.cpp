#include<Rcpp.h>
#include<string>


const int STATE_END = -1;

std::string ltrim(std::string s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) {
        return !std::isspace(ch);
    }));

    return s;
}

std::string rtrim(std::string s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) {
        return !std::isspace(ch);
    }).base(), s.end());

    return s;
}

// [[Rcpp::export]]
std::string trim(std::string s) {
    return rtrim(ltrim(s));
}

enum Input
{
    IN_TERMINATOR,
    IN_BACKSLASH,
    IN_QUOTEMARK,
    IN_OTHER
};

int checkInput(char c, int state)
{
    
    Input in = IN_OTHER;
    if (c == ';' || c == '$')
    {
        in = IN_TERMINATOR;
    }
    else if (c == '\\')
    {
        in = IN_BACKSLASH;
    }
    else if (c == '"')
    {
        in = IN_QUOTEMARK;
    }

    switch (state)
    {
    case 0:
        switch (in)
        {
        case IN_BACKSLASH:
            return 1;
        case IN_TERMINATOR:
            return STATE_END;
        case IN_QUOTEMARK:
            return 2;
        default:
            return 0;
        }
    case 1:
        return 0;
    case 2:
        switch (in)
        {
        case IN_BACKSLASH:
            return 3;
        case IN_TERMINATOR:
            return 2;
        case IN_QUOTEMARK:
            return 0;
        default:
            return 2;
        }
    case 3:
        return 2;
    }
    return state;
}

// [[Rcpp::export]]
std::string checkCommand(std::string command)
{ 
	trim(command);
	int state = 0; 
	
	for (size_t i = 0; i < command.size(); ++i) 
	{
		state = checkInput(command[i], state);
		if (state == STATE_END && (i + 1) != command.size())
		{
		// throw std::runtime_error("Bad expression: "
		// "only one ;|$ terminated expression at a time is allowed");
		Rcpp::stop("Bad expression: "
				"only one ;|$ termination expression at a time is allowed");
		} 
	}

    if (state != STATE_END)
    {
        command += ";";
    }

    return command;
}
