#ifndef MAXIMACHAIN_H_INCLUDED
#define MAXIMACHAIN_H_INCLUDED


#include <cstddef>
#include <string>
#include <deque>

#include "boost/process.hpp"
#include "boost/regex.hpp"
#include "boost/filesystem.hpp"
#include "boost/scoped_ptr.hpp"
#include "boost/shared_ptr.hpp"

namespace bp = ::boost::process;
namespace fs = ::boost::filesystem;
using std::size_t;

namespace Maxima
{

class MaximaChain {
	public:
        class MaximaIOHook
        {
        public:
            virtual ~MaximaIOHook();
            virtual std::string handle(const std::string &first,
                const std::string &second) = 0;
        };

        MaximaChain(const std::string &maximaPath,    // full path to Maxima exe-file
            const std::string &workingDir = ".",         // working directory of new process
            const std::string &utilsDir = ".");          // utils directory where is display.lisp

		~MaximaChain();

		std::string executeCommand(const std::string &command); //sends command to Maxima and returs Maxima output after last command

		std::string executeInteractive(const std::string &command); // sends command to Maxima and returns raw Maxima output


		bp::process::id_type getId() const;
        const fs::path &getWorkingDirectory() const;

        void setMaximaIOHook(const std::string &hookRegex, MaximaIOHook *hook);

    private:
        struct Reply {
            typedef std::deque<char> RawReply;
            typedef RawReply::iterator It;
            typedef std::pair<It, It> Range;

            RawReply reply;

            Range prompt;
            std::deque<Range> outs;
            std::deque<Range> betweens;

            Reply(std::istream &in);

            std::string concatenateParts();

            bool CheckPrompt() const { return validPrompt; } // true if prompt is valid (%i\d+)

            size_t getPromptId() const { return promptId; }

            bool isInterrupted() const;

        private:
            Reply(const Reply &);
            void operator=(const Reply &);

            bool validPrompt;
            size_t promptId;
        };

        typedef boost::shared_ptr<MaximaChain::Reply> ReplyPtr;

        static size_t readData(std::istream &in, Reply::RawReply &reply); // returns the number of bytes read


        ReplyPtr executeCommandRaw(const std::string &command); // sends command and get reply

        void sendCommand(std::string command); // command must end with ';' or '$'. In annother case we append ';'

        int checkInputExpression(char nextChar, int checkerState) const; // check if the input expression valid

        ReplyPtr readReply();

		void readPid();

        boost::scoped_ptr<bp::child> process;
        MaximaIOHook *maximaIOHook;
        boost::regex maximaIOHookRegex;
        fs::path workingDirectory;
        fs::path utilsDirectory;
        size_t lastPromptId;
		size_t pid;
};

}


#endif // MAXIMACHAIN_H_INCLUDED
