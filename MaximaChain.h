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

class MaximaChain
{
    public:

        MaximaChain(const std::string &maximaPath,          // Full path to the Maxima executable
                    const std::string &workingDir = ".",    // Working directory for output files
                    const std::string &utilsDir = ".");     // Utils directory that contains display.lisp

        ~MaximaChain();

	std::string executeCommand(const std::string &command); 

	std::string executeCommandList(const std::string &command); 

        // Command must end with ';' or '$'. In annother case we append ';'
        void sendCommand(std::string command);              

	bp::process::id_type getId() const;
        
        const fs::path &getWorkingDirectory() const;

        class MaximaIOHook
        {
          public:
            virtual ~MaximaIOHook();
     
            virtual std::string handle(const std::string &first,
                                       const std::string &second) = 0;
        };

        void setMaximaIOHook(const std::string &hookRegex, MaximaIOHook *hook);
 
   private:
        struct Reply
        {
            typedef std::deque<char> RawReply;

            typedef RawReply::iterator It;

            typedef std::pair<It, It> Range;

            RawReply reply;

            Range prompt;

            std::deque<Range> outs;

            std::deque<Range> betweens;

            Reply(std::istream &in);

            std::string concatenateParts();

            // True if prompt is valid (%i\d+)
            bool CheckPrompt() const { return validPrompt; } 

            size_t getPromptId() const { return promptId; }

            bool isInterrupted() const;


        private:
            Reply(const Reply &);

            void operator=(const Reply &);

            bool validPrompt;

            size_t promptId;
        };

        typedef boost::shared_ptr<MaximaChain::Reply> ReplyPtr;

        // Returns the number of bytes read
        static size_t readData(std::istream &in, Reply::RawReply &reply);

        // Sends command and get reply in Maxima style
        ReplyPtr crudeExecute(const std::string &command); 

        // Check if the input expression valid
        int checkInput(char nextChar, int checkerState) const;

        ReplyPtr readReply();

	void getPid();

        boost::scoped_ptr<bp::child> process;

        MaximaIOHook* maximaIOHook;

        boost::regex maximaIOHookRegex;

        fs::path workingDirectory;

        fs::path utilsDirectory;

        size_t lastPromptId;

	size_t pid;
};

} // namespace Maxima


#endif // MAXIMACHAIN_H_INCLUDED
