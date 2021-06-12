#include<Rcpp.h>
#include<string>

enum Input
{
    IN_TERMINATOR,
    IN_BACKSLASH,
    IN_QUOTEMARK,
    IN_OTHER
};

const int STATE_END = -1;

// trim a std::string
// trim from start (in place)
// void ltrim(std::string &s) {
//     s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) {
//         return !std::isspace(ch);
//     }));
// }
std::string ltrim(std::string s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) {
        return !std::isspace(ch);
    }));

    return s;
}

// trim from end (in place)
// void rtrim(std::string &s) {
//     s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) {
//         return !std::isspace(ch);
//     }).base(), s.end());
// }
std::string rtrim(std::string s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) {
        return !std::isspace(ch);
    }).base(), s.end());

    return s;
}

// trim from both ends (in place)
// void trim(std::string s) {
//     ltrim(s);
//     rtrim(s);
// }

// [[Rcpp::export]]
std::string trim(std::string s) {
    return rtrim(ltrim(s));
}

// trim from start (copying)
// std::string ltrim_copy(std::string s) {
//     ltrim(s);
//     return s;
// }
// 
// // trim from end (copying)
// std::string rtrim_copy(std::string s) {
//     rtrim(s);
//     return s;
// }
// 
// // trim from both ends (copying)
// // [[Rcpp::export]]
// std::string trim_copy(std::string s) {
//     trim(s);
//     return s;
// }

int checkInput(char c, int state)
{
    
    // 0: backslash->1, terminator->end, quot_mark->2, other->0
    // 1: any->0
    // 2: backslash->3, terminator->2, quot_mark->0, other->2
    // 3: any->2
    // end:
    
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
	// remove leading and trailing spaces from command 
	// alg::trim(command); 
	trim(command);
	int state = 0; 
	
	// this loop solely checks whether the command issued terminates correctly 
	for (size_t i = 0; i < command.size(); ++i) 
	{
		// get check ith character from string:
		// if a termination character (; or $) 
		// occures at position next-to-last it is concluded
		// that there are two termination characters and an exception gets thrown
		// cause a command is obliged to END with a termination character
		state = checkInput(command[i], state);
		if (state == STATE_END && (i + 1) != command.size())
		{
		// throw std::runtime_error("Bad expression: "
		// "only one ;|$ terminated expression at a time is allowed");
		Rcpp::stop("Bad expression: "
				"only one ;|$ termination expression at a time is allowed");
		} 
	}

    //if there is no termination character at all add ";"
    if (state != STATE_END)
    {
        command += ";";
    }

    return command;
}
