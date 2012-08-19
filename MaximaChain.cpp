
#include "MaximaChain.h"

#include <fstream>
#include <signal.h>
#include <sstream>
#include "boost/algorithm/string.hpp"

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
                         const std::string &utilsDir)
  : workingDirectory(fs::system_complete(workingDir)),
    utilsDirectory(fs::system_complete(utilsDir)),
    lastPromptId(1),
    pid(0)
{
    std::vector<std::string> args;
    args.push_back(maximaPath);
    args.push_back("-q");
    args.push_back("-p");
    args.push_back((utilsDirectory / "display.lisp").external_file_string());

    bp::context ctx;
    ctx.stdin_behavior = bp::capture_stream();
    ctx.stdout_behavior = bp::capture_stream();
    ctx.stderr_behavior = bp::redirect_stream_to_stdout();
    ctx.work_directory = workingDirectory.external_directory_string();
    ctx.environment = bp::self::get_environment();

    process.reset(new bp::child(bp::launch(maximaPath, args, ctx)));
    getPid();
    Reply(process->get_stdout());
}

MaximaChain::~MaximaChain()
{
    sendCommand("quit()");
    process->get_stdin().close();
    process->wait();
}

void MaximaChain::getPid()
{
    #ifdef _WIN32
	std::string pidStr;
	std::getline(process->get_stdout(), pidStr);

	boost::regex pidRegex("(pid=)?(\\d+)");
	boost::match_results<std::string::iterator> pidRegexMatch;
        if (boost::regex_match(pidStr.begin(),
                               pidStr.end(),
                               pidRegexMatch,pidRegex))
        {
            std::istringstream pidStream(std::string(pidRegexMatch[2].first,
                                         pidRegexMatch[2].second));
            pidStream >> pid;
	    return;
        }
    #endif

    pid = process->get_id();
}

size_t MaximaChain::readData(std::istream &in, Reply::RawReply &reply)
{
    const size_t bufSize = 4096;
    char buf[bufSize];

    size_t readTotal = 0;
    size_t charsRead;

    do
    {
        charsRead = in.readsome(buf, bufSize);
	reply.insert(reply.end(), buf, buf + charsRead);
	readTotal += charsRead;
    }
    while (charsRead != 0);

    return readTotal;
}

void MaximaChain::sendCommand(std::string command)
{
    alg::trim(command);
    int state = 0;

    for (size_t i = 0; i < command.size(); ++i)
    {
        state = checkInput(command[i], state);
        if (state == STATE_END && (i + 1) != command.size())
        {
            throw std::runtime_error("Bad expression: "
            "only one ;|$ terminated expression at a time is allowed");
        }
    }

    if (state != STATE_END)
    {
        command += ";";
    }

    if (!maximaIOHookRegex.empty())
    {
        boost::match_results<std::string::iterator> m;
        std::string::iterator start = command.begin();
        std::string::iterator end = command.end();

        while (boost::regex_search(start, end, m, maximaIOHookRegex))
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

    process->get_stdin() << command << std::endl;
}

MaximaChain::Reply::Reply(std::istream &in)
{
    boost::regex promptExpr("prompt;>>(.*)<<prompt;");

    boost::match_results<Reply::It> promptMatch;

    do
    {
        char c;

        if (!in.get(c))
        {
            std::string wasRead(reply.begin(), reply.end());
            throw std::runtime_error("Read failed. Was read: " + wasRead);
        }

        reply.push_back(c);
    }
    while (!(reply.back() == ';' && boost::regex_search(
           reply.begin(), reply.end(), promptMatch, promptExpr)));

	prompt = promptMatch[1];

	boost::regex outExpr("out;>>(.*?)<<out;");
	Reply::It start = reply.begin();
	Reply::It end = promptMatch[0].first;

	boost::match_results<Reply::It> what;

	while(boost::regex_search(start, end, what, outExpr))
	{
		outs.push_back(what[1]);
		betweens.push_back(Reply::Range(start, what[0].first));
		start = what[0].second;
	}

	betweens.push_back(Reply::Range(start, end));
	boost::match_results<Reply::It> outInPrompt;

	if (boost::regex_search(promptMatch[1].first, promptMatch[1].second,
		outInPrompt, outExpr))
	{
		prompt = outInPrompt[1];
	}

    boost::regex validPrompt_("\\s*\\(%i(\\d+)\\)\\s*");
    boost::match_results<Reply::It>validPromptMatch;

    if (boost::regex_match(prompt.first, prompt.second, validPromptMatch,
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
    size_t lastPromptId_ = lastPromptId;

    ReplyPtr result;
    do
    {
        result = ReplyPtr(new Reply(process->get_stdout()));
    }
    while (result->outs.empty() && result->CheckPrompt() &&
           result->getPromptId() <= lastPromptId_ && result->isInterrupted());

    lastPromptId = result->getPromptId();
    return result;
}

std::string MaximaChain::executeCommand(const std::string &command)
{
    ReplyPtr reply = crudeExecute(command);

    if (reply->outs.empty())
    {
        if (!reply->CheckPrompt())
        {
            crudeExecute(";");
            throw std::runtime_error("Unsupported: " + reply->concatenateParts());
        }

        if (reply->isInterrupted())
        {
            throw std::runtime_error("Command execution was interrupted.");
        }

	throw std::runtime_error("Maxima error: " +
                                 std::string(reply->betweens.back().first,
                                             reply->betweens.back().second));
    }

    boost::regex validOut("\\s*\\(%o\\d+\\)\\s*(.*)\\s*");

    Reply::Range lastOut = reply->outs.back();

    boost::match_results<Reply::It> what;
    
    if (boost::regex_match(lastOut.first, lastOut.second, what, validOut))
    {
        return alg::trim_copy(std::string(what[1].first, what[1].second));
    }

    crudeExecute(";");

    throw std::runtime_error("Unsupported: " + reply->concatenateParts());
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
    boost::regex interrupt("Console interrupt|User break|Interactive interrupt");
    boost::match_results<Reply::It> match;

    return boost::regex_search(betweens.back().first,
			       betweens.back().second,
      			       match, interrupt);
}

std::string MaximaChain::executeCommandList(const std::string &command)
{
    return crudeExecute(command)->concatenateParts();
}

bp::process::id_type MaximaChain::getId() const
{
    return pid;
}

void MaximaChain::setMaximaIOHook(const std::string &hookRegex, MaximaIOHook *hook)
{
    maximaIOHook = hook;
    maximaIOHookRegex = boost::regex(hookRegex);
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


