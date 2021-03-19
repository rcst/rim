#include<Rcpp.h>
#include "MaximaChain.h"

#include <fstream>
#include <signal.h>
#include <sstream>
#include <boost/algorithm/string.hpp>
#include<regex>

namespace alg = boost::algorithm;

using namespace Maxima;

enum Input
{
    IN_TERMINATOR,
    IN_BACKSLASH,
    IN_QUOTEMARK,
    IN_OTHER
};

const int STATE_END = -1;

MaximaChain::MaximaIOHook::~MaximaIOHook()
{
}

MaximaChain::MaximaChain(const std::string &maximaPath,
                         const std::string &workingDir,
                         const std::string &utilsDir,
			 const std::string &display)
  : workingDirectory(fs::system_complete(workingDir)),
    utilsDirectory(fs::system_complete(utilsDir)),
    lastPromptId(1),
    pid(0)
{ 
	std::vector<std::string> args; 
	args.push_back(maximaPath); 
	args.push_back("-q"); 
	args.push_back("--userdir=" + utilsDirectory.string()); 
	args.push_back("--init=maxima-init-tex2"); 
	
	// work-around since std::regex has no member function empty() 
	maximaIOHookRegexStr = std::string(); 
	maximaIOHookRegex = std::regex(maximaIOHookRegexStr); 
	
	process.reset(new bp::child(maximaPath, 
				args, 
				(bp::std_out & bp::std_err) > is, 
				bp::std_in < os)); 
	getPid(); 
	
	Reply(is); 
	
	// flush the pipe stream 
	readReply();
}

MaximaChain::~MaximaChain()
{ 
	sendCommand("quit()"); 
	os.pipe().close(); 
	process->wait();
}

void MaximaChain::getPid()
{ 
	// variable pid is actually not read within MaximaChain class 
	#ifdef _WIN32
	std::string pidStr;
	// std::getline(process->get_stdout(), pidStr);
	std::getline(is, pidStr);

	std::regex pidRegex("(pid=)?(\\d+)");
	std::match_results<std::string::iterator> pidRegexMatch;
        if (std::regex_match(pidStr.begin(),
                               pidStr.end(),
                               pidRegexMatch,pidRegex))
        {
            std::istringstream pidStream(std::string(pidRegexMatch[2].first,
                                         pidRegexMatch[2].second));
            pidStream >> pid;
	    return;
        } 
#endif

	// pid = process->get_id(); 
	pid = process->id();
}

size_t MaximaChain::readData(std::istream &in, Reply::RawReply &reply)
{ 
	const size_t bufSize = 4096; 
	char buf[bufSize]; 
	
	size_t readTotal = 0; 
	size_t charsRead; 
	
	do 
	{ 
		//std::readsome reads "bufSize" characters from istream "in" 
		//and writes them into character array "buf" 
		charsRead = in.readsome(buf, bufSize); 
		reply.insert(reply.end(), buf, buf + charsRead); 
		readTotal += charsRead; 
	} while (charsRead != 0);

	return readTotal;
}

void MaximaChain::sendCommand(std::string command)
{ 
	// remove leading and trailing spaces from command 
	alg::trim(command); 
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

    // maximaIOHookRegex is a boost::std::regex object
    // it is empty in the state of current implementation
    // test: Rcpp::Rcout << "maximaIOHookRegex.empty(): " << maximaIOHookRegex.empty() << std::endl;
    // returns: maximaIOHookRegex.empty(): 1
    if (!maximaIOHookRegexStr.empty())
    {
        std::match_results<std::string::iterator> m;
        std::string::iterator start = command.begin();
        std::string::iterator end = command.end();

        while (std::regex_search(start, end, m, maximaIOHookRegex))
        {
            std::string rep = maximaIOHook->handle(
            std::string(m[1].first, m[1].second),
            std::string(m[2].first, m[2].second));

            size_t newStartPosition = m[0].second - start + rep.size() -
                                     (m[2].second - m[2].first);

            command.replace(m[2].first, m[2].second, rep);

            start = command.begin() + newStartPosition;
            end = command.end();
        }
    }

    // process->get_stdin() << command << std::endl;
    os << command << std::endl;
}

MaximaChain::Reply::Reply()
{
}

MaximaChain::Reply::Reply(bp::ipstream &in)
{ 
    //std::regex promptExpr("prompt;>>(.*)<<prompt;"); 
    std::regex promptExpr("prompt;>>([[:space:]|[:print:]]*)<<prompt;"); 
    std::match_results<Reply::It> promptMatch;

    do 
    { 
        char c; 
	if (!in.get(c)) 
	{ 
            std::string wasRead(reply.begin(), reply.end()); 
	    // throw std::runtime_error("Read failed. Was read: " + wasRead);

	    Rcpp::stop("Read failed. Was read: " + wasRead); 
	} 
	
	reply.push_back(c);
    }
    while (!(reply.back() == ';' && std::regex_search(
           reply.begin(), reply.end(), promptMatch, promptExpr)));

    prompt = promptMatch[1]; 
    
    std::regex outExpr("out;>>([[:space:]|[:print:]]*?)<<out;");
    Reply::It start = reply.begin();
    Reply::It end = promptMatch[0].first;
    std::match_results<Reply::It> what; 
    
    while(std::regex_search(start, end, what, outExpr)) 
    { 
                outs.push_back(what[1]); 
                betweens.push_back(Reply::Range(start, what[0].first)); 
                start = what[0].second; 
    }
    
    //std::regex outExpr("out;>>(.*?)<<out;");
    
    // Rcpp::Rcout << "Printing reply: begin -> promptMatch[0].first ..." << std::endl;
    //     for(auto i = start; i != end; ++i)
    // 	    Rcpp::Rcout << *i;
    //     Rcpp::Rcout << std::endl;
    // Rcpp::Rcout << "End reply ..." << std::endl;
    
    // loops over pairs of outExpr as declared in the display.lisp file
    // i.e. out;>> and <<out;
    // stores outputs in ... "outs"

    betweens.push_back(Reply::Range(start, end)); 
    std::match_results<Reply::It> outInPrompt;

    if (std::regex_search(promptMatch[1].first, promptMatch[1].second, 
			    outInPrompt, outExpr)) 
    { 
	    prompt = outInPrompt[1]; 
    }

    std::regex validPrompt_("\\s*\\(%i(\\d+)\\)\\s*");
    std::match_results<Reply::It>validPromptMatch;

    if (std::regex_match(prompt.first, prompt.second, validPromptMatch,
        validPrompt_))
    {
    validPrompt = true;
        std::istringstream idStream(std::string(validPromptMatch[1].first,
                                    validPromptMatch[1].second));
        idStream >> promptId;
    }
    else
    {
        validPrompt = false;
        promptId = 0;
    }
}

MaximaChain::ReplyPtr MaximaChain::crudeExecute(const std::string &command)
{
    sendCommand(command);
    return readReply();
}

MaximaChain::ReplyPtr MaximaChain::readReply()
{
	//lastPromptId = 1 on first command
    size_t lastPromptId_ = lastPromptId;

    ReplyPtr result;
    do
    {
        // result = ReplyPtr(new Reply(process->get_stdout()));
        result = ReplyPtr(new Reply(is));
    }
    while (result->outs.empty() && result->CheckPrompt() &&
           result->getPromptId() <= lastPromptId_ && result->isInterrupted());

    lastPromptId = result->getPromptId();
    return result;
}

std::string MaximaChain::executeCommand(const std::string &command)
{
    ReplyPtr reply = crudeExecute(command);

    // print content of reply
    // for(auto i = reply->reply.cbegin(); i != reply->reply.cend(); ++i)
    //         Rcpp::Rcout << *i;
    // Rcpp::Rcout << std::endl;

    if (reply->outs.empty())
    {
	    std::match_results<Reply::It> match;
	    if(reply->requireUser(match)) 
		    return(std::string(match[1].first, match[1].second));

        if (!reply->CheckPrompt())
        {
            crudeExecute(";");
	    Rcpp::stop("Unsupported: " + reply->concatenateParts());
        }

        if (reply->isInterrupted())
        {
		Rcpp::stop("Command execution was interrupted.");
        }

	return std::string("");
    }

    //std::regex validOut("\\s*\\(%o\\d+\\)\\s*(.*)\\s*");
    std::regex validOut("\\s*\\(%o\\d+\\)\\s*([[:space:]|[:print:]]*)\\s*");

    Reply::Range lastOut = reply->outs.back();

    std::match_results<Reply::It> what;
    
    // output results of command to R-console if the result is truely valid, i.e. after (%o..)
    if (std::regex_match(lastOut.first, lastOut.second, what, validOut))
    {
	return alg::trim_copy(std::string(what[1].first, what[1].second));
    }

    crudeExecute(";");

    Rcpp::stop("Unsupported: " + reply->concatenateParts());
}

std::string MaximaChain::Reply::concatenateParts()
{
    std::string result;
    for (size_t i = 0; i < outs.size(); ++i)
    {
	result.insert(result.end(), betweens[i].first, betweens[i].second);
	result.insert(result.end(), outs[i].first, outs[i].second);
    }
	result.insert(result.end(), betweens.back().first, betweens.back().second);
	result.insert(result.end(), prompt.first, prompt.second);

	return result;
}

bool MaximaChain::Reply::isInterrupted() const
{
    std::regex interrupt("Console interrupt|User break|Interactive interrupt");
    std::match_results<Reply::It> match;

    return std::regex_search(betweens.back().first,
			       betweens.back().second,
      			       match, interrupt);
}

bool MaximaChain::Reply::requireUser(std::match_results<Reply::It> &match) const
{
    std::regex askExpr("TEXT;>>([[:space:]|[:print:]]*?)<<TEXT;");
    
    return std::regex_search(prompt.first, prompt.second, match, askExpr);
}

std::string MaximaChain::executeCommandList(const std::string &command)
{
    return crudeExecute(command)->concatenateParts();
}

boost::process::pid_t MaximaChain::getId() const
{
    return pid;
}

void MaximaChain::setMaximaIOHook(const std::string &hookRegex, MaximaIOHook *hook)
{
    maximaIOHook = hook;
    maximaIOHookRegex = std::regex(hookRegex);
}

int MaximaChain::checkInput(char c, int state) const
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

const fs::path &MaximaChain::getWorkingDirectory() const
{
    return workingDirectory;
}

void MaximaChain::loadModule(const std::string &module)
{
	if(!module.empty()) 
		crudeExecute("load(\"" + module + "\")$");
	else
		Rcpp::stop("Please provide a valid module name");
}
