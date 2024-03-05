/* Part of the MUST Project, under BSD-3-Clause License
 * See https://hpc.rwth-aachen.de/must/LICENSE for license information.
 * SPDX-License-Identifier: BSD-3-Clause
 */

/**
 * @file MsgLoggerHtml.cpp
 *       @see MUST::MsgLoggerHtml.
 *
 *  @date 20.01.2011
 *  @author Tobias Hilbrich
 */

#include "GtiMacros.h"
#include "MustEnums.h"
#include "mustConfig.h"
#include "MustDefines.h"

#include "MsgLoggerCommon.hpp"
#include "MsgLoggerHtml.h"

#include <string.h>
#include <sys/stat.h>

using namespace gti;
using namespace must;

using std::endl;
using std::map;
using std::ofstream;
using std::string;

mGET_INSTANCE_FUNCTION(MsgLoggerHtml)
mFREE_INSTANCE_FUNCTION(MsgLoggerHtml)
mPNMPI_REGISTRATIONPOINT_FUNCTION(MsgLoggerHtml)

namespace
{

/**
 * Print information on the build like the exact version, etc.
 * @param out the stream to print to
 */
auto printBuildInfo(ofstream& out) -> void
{
    out << "<div class=\"build-info\">\n"
           "<p>\n"
           R"(<label for="must-version-string">MUST Version:</label> <span id="must-version-string">)"
        << MUST_VERSION
        << "</span>\n"
           "</p>\n"
           "</div>\n";
}
} // namespace

//=============================
// Constructor
//=============================
MsgLoggerHtml::MsgLoggerHtml(const char* instanceName)
    : gti::ModuleBase<MsgLoggerHtml, I_MessageLogger>(instanceName), MsgLoggerBase()
{
    std::vector<gti::I_Module*> subModInstances;
    subModInstances = createSubModuleInstances();

    // save sub modules
    myLIdModule = (I_LocationAnalysis*)subModInstances[0];
    myPIdModule = (I_ParallelIdAnalysis*)subModInstances[1];
    /* Make sure the base output directory exists before opening the file. */
    must_ensure_dir_exists(get_base_output_dir().c_str());
    /* Initialize the initial log-file. */
    this->openFile(0, "MUST_Output.html", strlen("MUST_Output.html"));
}

//=============================
// Destructor
//=============================
MsgLoggerHtml::~MsgLoggerHtml(void)
{
    if (myLIdModule)
        destroySubModuleInstance((I_Module*)myLIdModule);
    myLIdModule = NULL;

    if (myPIdModule)
        destroySubModuleInstance((I_Module*)myPIdModule);
    myPIdModule = NULL;

    /* If MUST did not detect an error in the application, the last file should
     * contain a message, so one can check the status by just looking in the
     * last HTML file. */
    if (myErrorCode <= MustInformationMessage) {
        char temp[] = "MUST detected no MPI usage errors nor any suspicious "
                      "behavior during this application run.";
        log(MUST_MESSAGE_NO_ERROR,
            false,
            0,
            0,
            this->fileHandles.rbegin()->first,
            MustNoneMessage,
            temp,
            strlen(temp),
            0,
            NULL,
            NULL);
    }

    /* Close all open log-files. */
    map<size_t, NamedOfstream>::iterator iter = this->fileHandles.begin();
    while (iter != this->fileHandles.end()) {
        /* The fileId needs to be stored in a temporary variable instead of
         * accessing it directly, as the iterator needs to be incremented,
         * before the current iterator gets deleted. */
        size_t fileId = iter->first;
        iter++;

        this->closeFile(fileId);
    }
}

//=============================
// log
//=============================
gti::GTI_ANALYSIS_RETURN MsgLoggerHtml::log(
    int msgId,
    int hasLocation,
    uint64_t pId,
    uint64_t lId,
    size_t fileId,
    int msgType,
    char* text,
    int textLen,
    int numReferences,
    uint64_t* refPIds,
    uint64_t* refLIds)
{
    if (!hasLocation)
        return logStrided(
            msgId,
            pId,
            lId,
            fileId,
            0,
            0,
            0,
            msgType,
            text,
            textLen,
            numReferences,
            refPIds,
            refLIds);

    return logStrided(
        msgId,
        pId,
        lId,
        fileId,
        myPIdModule->getInfoForId(pId).rank,
        1,
        1,
        msgType,
        text,
        textLen,
        numReferences,
        refPIds,
        refLIds);
}

//=============================
// log
//=============================
gti::GTI_ANALYSIS_RETURN MsgLoggerHtml::logStrided(
    int msgId,
    uint64_t pId,
    uint64_t lId,
    size_t fileId,
    int startRank,
    int stride,
    int count,
    int msgTypeTemp,
    char* text,
    int textLen,
    int numReferences,
    uint64_t* refPIds,
    uint64_t* refLIds)
{
    rememberErrorcode(msgTypeTemp);
    MustMessageType msgType = (MustMessageType)msgTypeTemp;

    // select even or odd char
    char evenOrOdd = 'e';
    if (!myLineEven)
        evenOrOdd = 'o';

    // select even or odd char
    char infoWarnError = 'i';
    MustMessageIdNames mustNames_msgId = (MustMessageIdNames)msgId;
    std::string msgTypeStr = "Information";
    if (msgType == MustWarningMessage) {
        infoWarnError = 'w';
        msgTypeStr = MustErrorId2String[mustNames_msgId];
    } else if (msgType == MustErrorMessage) {
        infoWarnError = 'e';
        msgTypeStr = MustErrorId2String[mustNames_msgId];
    }

    // replace all '\n' characters with "<br>" in the text
    std::string::size_type p = 0;
    std::string tempText(text); // copy the text

    do {
        p = tempText.find('\n', p);
        if (p == std::string::npos)
            break;
        tempText.replace(p, 1, "<br>");
    } while (p != std::string::npos);

    // TODO: Threadsafety? Should be safe, we only have one logger.
    static int ctr = 0;

    // do the html output
    this->fileHandles[fileId]
        << "<tr onclick=\"showdetail(this, 'detail" << ctr
        << "');\" onmouseover=\"flagrow(this);\" onmouseout=\"deflagrow(this);\">"
        << "<td class=" << infoWarnError << evenOrOdd << "1 c1>";

    if (count > 0) {
        // CASE1: A single rank
        if (count == 1) {
            // Thread id only interest us if this concerns
            // a single rank
            ParallelInfo info = myPIdModule->getInfoForId(pId);
            if (info.threadid)
                this->fileHandles[fileId] << startRank << "(" << info.threadid << ")";
            else
                this->fileHandles[fileId] << startRank;
        }
        // CASE2: Continous ranks
        else if (stride == 1) {
            this->fileHandles[fileId] << startRank << "-" << startRank + (count - 1);
        }
        // CASE3: Strided ranks
        else {
            int last = startRank;
            for (int x = 0; x < count; x++) {
                if (last != startRank)
                    this->fileHandles[fileId] << ", ";

                this->fileHandles[fileId] << last;

                last += stride;

                if (x == 2 && count > 4) {
                    this->fileHandles[fileId] << ", ..., " << startRank + (count - 1) * stride;
                    break;
                }
            }
        }
    } else {
        this->fileHandles[fileId] << "&nbsp;";
    }

    this->fileHandles[fileId] << "</td>"
                              //<< "<td class="<< infoWarnError << evenOrOdd << "2>"
                              //<< "&nbsp;" //TODO currently we have no thread information
                              //<< "</td>"
                              << "<td class=" << infoWarnError << evenOrOdd << "2 c2>"
                              << "<b>" << msgTypeStr << "</b>"
                              << "</td>"
                              << "<td class=" << infoWarnError << evenOrOdd << "3 c3>"
                              << "<div class=\"shortmsg\">" << tempText << "</div>"
                              << "</td>"
                              << "</tr>"
                              << "<tr>"
                              << "<td colspan=\"3\" id=\"detail" << ctr++
                              << "\" style=\"visibility:hidden; display:none\">"
                              << "<div>Details:</div>"
                              << "<table>"
                              << "<tr>"
                              << "<td align=\"center\" bgcolor=\"#9999DD\"><b>Message</b></td>"
                              << "<td align=\"left\" bgcolor=\"#7777BB\"><b>From</b></td>"
                              << "<td align=\"left\" bgcolor=\"#9999DD\"><b>References</b></td>"
                              << "</tr>"
                              << "<tr>"
                              << "<td class=" << infoWarnError << evenOrOdd << "3 c3>" << tempText
                              << "</td>"
                              << "<td class=" << infoWarnError << evenOrOdd << "4 c4>";
    if (count > 0) {
        // if (count > 1)
        // Even if count == 1, it may still yield from a representative
        this->fileHandles[fileId] << "Representative location:<br>" << std::endl;

        printLocation(this->fileHandles[fileId], pId, lId);
    } else {
        this->fileHandles[fileId] << "&nbsp;";
    }

    this->fileHandles[fileId] << "</td>"
                              << "<td class=" << infoWarnError << evenOrOdd << "5 c5>";

    // References heading
    if (numReferences > 0)
        this->fileHandles[fileId] << "References of a representative process:<br><br>" << std::endl;

    // Print extra references
    for (int i = 0; i < numReferences; i++) {
        if (i != 0)
            this->fileHandles[fileId] << "<br><br>";

        this->fileHandles[fileId] << "reference " << (i + 1) << " rank "
                                  << myPIdModule->getInfoForId(refPIds[i]).rank << ": ";
        printLocation(this->fileHandles[fileId], refPIds[i], refLIds[i]);
    }
    this->fileHandles[fileId] << "&nbsp;"
                              << "</td>";

    // TODO Reference to MPI standard
    //  out
    //     << "<td class="<< infoWarnError << evenOrOdd << "6>"
    //    << "&nbsp;"
    //  << "</td>";

    // End the row
    this->fileHandles[fileId] << "</tr></table></td></tr>" << std::endl;

    // invert even/odd flag
    myLineEven = !myLineEven;

    return GTI_ANALYSIS_SUCCESS;
}

//=============================
// printTrailer
//=============================
void MsgLoggerHtml::printTrailer(ofstream& out, bool finalNotes)
{
    if (finalNotes) {
        // read the system time
        char buf[128];

        struct tm* ptr;
        time_t tm;
        tm = time(NULL);
        ptr = localtime(&tm);
        strftime(buf, 128, "%c.\n", ptr);
        out << "<p><b>MUST has completed successfully</b>, end date: " << buf << "</p>";
    }
    printBuildInfo(out);
    out << "</body></html>" << std::endl;
}

//=============================
// printHeader
//=============================
void MsgLoggerHtml::printHeader(ofstream& out)
{
    // read the system time
    char buf[128];

    struct tm* ptr;
    time_t tm;
    tm = time(NULL);
    ptr = localtime(&tm);
    strftime(buf, 128, "%c.\n", ptr);

    // print the header
    out << "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">" << std::endl
        << "<html>" << std::endl
        << "<head>" << std::endl
        << "<title>MUST Outputfile</title>" << std::endl
        << "<style type=\"text/css\">" << std::endl
        << "td,td,table {border:thin solid black}" << std::endl

        << "td.ie1, td.ie3, td.ie5, td.ie7 { background-color:#DDFFDD;}" << std::endl
        << "td.ie2, td.ie4, td.ie6 { background-color:#EEFFEE;}" << std::endl
        << "td.io1, td.io3, td.io5, td.io7 { background-color:#CCEECC;}" << std::endl
        << "td.io2, td.io4, td.io6 { background-color:#DDEEDD;}" << std::endl
        << "td.we1, td.we3, td.we5, td.we7 { background-color:#FFFFDD;}" << std::endl
        << "td.we2, td.we4, td.we6 { background-color:#FFFFEE;}" << std::endl
        << "td.wo1, td.wo3, td.wo5, td.wo7 { background-color:#EEEECC;}" << std::endl
        << "td.wo2, td.wo4, td.wo6{ background-color:#EEEEDD;}" << std::endl
        << "td.ee1, td.ee3, td.ee5, td.ee7 { background-color:#FFDDDD;}" << std::endl
        << "td.ee2, td.ee4, td.ee6 { background-color:#FFEEEE;}" << std::endl
        << "td.eo1, td.eo3, td.eo5, td.eo7{ background-color:#EECCCC;}" << std::endl
        << "td.eo2, td.eo4, td.eo6 { background-color:#EEDDDD;}" << std::endl
        << "td.c1{ text-align:center; vertical-align:middle;}" << std::endl
        << "td.c2{ text-align:center; vertical-align:middle; overflow:auto;}" << std::endl
        << "td.c3{ text-align:center; vertical-align:middle;}" << std::endl
        << "td.c4{ text-align:center; vertical-align:middle;}" << std::endl
        << "td.c5{ }" << std::endl
        << "td.c6{ text-align:left; vertical-align:middle;}" << std::endl
        << "td.c7{ text-align:center; vertical-align:middle;}" << std::endl

        << "div.shortmsg{ max-width:100%; text-overflow:ellipsis; display: inline-block; display: "
           "inline-block; white-space: nowrap; overflow:hidden; }"
        << std::endl
        << "div.shortmsg br { display: none; }" << std::endl
        << ".build-info label { font-weight: bold; }\n"
        << "</style>" << std::endl
        << "</head>" << std::endl
        << "<body>" << std::endl
        << "<p> <b>MUST Output</b>, starting date: " << buf << "</p>" << std::endl
        << "<script type=\"text/javascript\">" << std::endl
        << "function flagrow(obj)" << std::endl
        << "{" << std::endl
        << " for( var i = 0; i < obj.cells.length; i++ )" << std::endl
        << "  obj.cells[i].style.borderColor=\"#ff0000\";" << std::endl
        << "}" << std::endl
        << std::endl
        << "function deflagrow(obj)" << std::endl
        << "{" << std::endl
        << " for( var i = 0; i < obj.cells.length; i++ )" << std::endl
        << "  obj.cells[i].style.borderColor=\"\";" << std::endl
        << "}" << std::endl
        << std::endl
        << "function showdetail(me, name)" << std::endl
        << "{" << std::endl
        << "var obj = document.getElementById(name);" << std::endl
        << "if( obj )" << std::endl
        << "{" << std::endl
        << "var style = obj.style;" << std::endl
        << "var visible = String(style.visibility);" << std::endl
        << "if( visible == 'hidden' )" << std::endl
        << "{" << std::endl
        << "style.visibility = 'visible';" << std::endl
        << "style.display=\"\";" << std::endl
        << "}" << std::endl
        << "else" << std::endl
        << "{" << std::endl
        << "style.visibility = 'hidden';" << std::endl
        << "style.display=\"none\";" << std::endl
        << "}" << std::endl
        << "}" << std::endl
        << "}" << std::endl
        << "</script>" << std::endl
        << "<table border=\"0\" cellspacing=\"0\" cellpadding=\"0\" style=\"table-layout: fixed; "
           "width: 100%\">"
        << std::endl
        << "<tr>" << std::endl
        << "<td align=\"center\" bgcolor=\"#9999DD\" width=\"7%\"><b>Rank(s)</b></td>" << std::endl
        << "<td align=\"center\" bgcolor=\"#7777BB\" width=\"28%\"><b>Type</b></td>" << std::endl
        << "<td align=\"center\" bgcolor=\"#9999DD\"><b>Message</b></td>" << std::endl
        << "</tr>" << std::endl;

    /*<< "<table border=\"0\" width=\"100%\" cellspacing=\"0\" cellpadding=\"0\">" << std::endl
    << "<tr>" << std::endl
    << "<td align=\"center\" bgcolor=\"#9999DD\">"
    << "<b>Rank(s)</b>"
    << "</td>" << std::endl
    //<< "<td align=\"center\" bgcolor=\"#7777BB\">"
    //<< "<b>Thread</b>"
    //<< "</td>" << std::endl
    << "<td align=\"center\" bgcolor=\"#7777BB\">"
    << "<b>Type</b>"
    << "</td>" << std::endl
    << "<td align=\"center\" bgcolor=\"#9999DD\">"
    << "<b>Message</b>"
    << "</td>" << std::endl
    << "<td align=\"left\" bgcolor=\"#7777BB\">"
    << "<b>From</b>"
    << "</td>" << std::endl
    << "<td align=\"left\" bgcolor=\"#9999DD\">"
    << "<b>References</b>"
    << "</td>" << std::endl
    //<< "<td align=\"center\" bgcolor=\"#9999DD\">"
    //<< "<b>MPI-Standard Reference</b>"
    //<< "</td>" << std::endl
    << "</tr>" << std::endl;*/
}

//=============================
// printLocation
//=============================
void MsgLoggerHtml::printLocation(ofstream& out, MustParallelId pId, MustLocationId lId)
{
    const LocationInfo& ref = myLIdModule->getInfoForId(pId, lId);
#ifndef ENABLE_STACKTRACE
    out << "<b>" << ref.callName << "</b>";
    out << myLIdModule->toString(pId, lId);
    if (ref.codeptr == NULL)
        printOccurenceCount(out, lId);
#else
    out << "<b>" << ref.callName << "</b>";
    printOccurenceCount(out, lId);
    out << " called from: <br>" << std::endl;

    out << format_stacktrace(ref.stack, "<br>");
#endif
}

//=============================
// printOccurenceCount
//=============================
void MsgLoggerHtml::printOccurenceCount(std::ostream& out, MustLocationId lId)
{
    out << " (" << myLIdModule->getOccurenceCount(lId);

    if (myLIdModule->getOccurenceCount(lId) == 1)
        out << "st";
    else if (myLIdModule->getOccurenceCount(lId) == 2)
        out << "nd";
    else if (myLIdModule->getOccurenceCount(lId) == 3)
        out << "rd";
    else
        out << "th";

    out << " occurrence)";
}

//=============================
// openFile
//=============================
void MsgLoggerHtml::openFile(size_t fileId, const char* filename, size_t len)
{
    this->fileHandles.emplace(
        std::piecewise_construct,
        std::forward_as_tuple(fileId),
        std::forward_as_tuple(string(filename, len)));
    this->printHeader(this->fileHandles[fileId]);
}

//=============================
// closeFile
//=============================
void MsgLoggerHtml::closeFile(size_t fileId)
{
    /* Close the table of messages and start a new line before printing the
     * footer below. */
    this->fileHandles[fileId] << "</table>" << endl;

    /* If a next file was opened before, add a link to this file, so the user
     * can easily navigate through the files, without knowledge, which files
     * have been opened by the application. */
    if (this->fileHandles.find(fileId + 1) != this->fileHandles.end())
        this->fileHandles[fileId] << "<p>More information is available in the "
                                  << "<a href=\"" << this->fileHandles[fileId + 1].filename()
                                  << "\">next file</a>.</p>" << endl;

    this->printTrailer(this->fileHandles[fileId], true);

    /* Close the log-file. */
    this->fileHandles[fileId].close();
    this->fileHandles.erase(fileId);
}

/*EOF*/
