/* This file is part of GTI (Generic Tool Infrastructure)
 *
 * Copyright (C)
 *  2008-2019 ZIH, Technische Universitaet Dresden, Federal Republic of Germany
 *  2008-2019 Lawrence Livermore National Laboratories, United States of America
 *  2013-2019 RWTH Aachen University, Federal Republic of Germany
 *
 * See the LICENSE file in the package base directory for details
 */

/**
 * @file FileManagement.h
 *
 * @author Mathias Korepkat
 * @date 28.10.2011
 */

#ifndef FILEMANAGEMENT_H
#define FILEMANAGEMENT_H

#include <QtGui>
#include "QtXml/QDomDocument"

namespace gti
{
namespace guis
{
namespace mapping
{
class GtiMapper;
class FileManagement : public QWidget
{
    Q_OBJECT
  public:
    /**
     * constructor that creates important gui elements
     */
    explicit FileManagement(QWidget* parent = 0);

    /**
     * function that holds whether files are set or not
     * @return true if files are set, otherwise not
     */
    bool hasFiles();

    /**
     * function that holds whether files were set or unset.
     * This is needed by GtiMapper to Update SymbolManagement
     * return true if filelist has changed.
     */
    bool hasChanged();
  signals:

  private slots:
    /**
     * reimplmentation of showÉvent to set changed to false
     */
    void showEvent(QShowEvent* event);

    /**
     * Function that is called in response to a trigger signal
     * of the "ok" Button. This calls the function changeView of GtiMapper.
     */
    void leaveWidget();
  protected slots:
    /**
     * Function that is called in response to a trigger signal
     * of the "add" Button. This starts an openFileDialog and save
     * the result in the file listbox. And calls the load functions
     * of analyses::Analyses and calls::ApiCalls to load specifications.
     * the changed value will be set to true by this function.
     */
    void addFile();

    /**
     * Function that is called in response to a trigger signal
     * of the "remove" Button. This removes selected files from listbox.
     * And calls the unload functions
     * of analyses::Analyses to unload specifications.
     * the changed value will be set to true by this function.
     */
    void removeFile();

  protected:
    bool changed;          // value to save if something changed.
    QListWidget* fileList; // list widget containing all files

    /**
     * Function to check the XML file.
     * @param file filepath to the xml file
     * @param xmlType is set to the Type of the xml (doctype name)
     * @return true if the filepath is a valid xml, file otherwise false
     */
    bool validateXml(QString file, QString* xmlType);
};
} // namespace mapping
} // namespace guis
} // namespace gti
#endif // FILEMANAGEMENT_H
