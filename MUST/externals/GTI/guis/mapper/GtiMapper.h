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
 * @file GtiMapper.h
 *
 * @author Mathias Korepkat
 * @date 28.10.2011
 */

#ifndef GTIMAPPER_H
#define GTIMAPPER_H

#include <QtGui>

namespace gti
{
namespace guis
{
namespace mapping
{
class MappingSelection;
class SymbolManagement;
class Settings;
class FileManagement;
class ArgumentAssoziator;
class MappingOverview;

/**
 * main application window with a menubar and
 * a center area to show gui elements
 */
class GtiMapper : public QMainWindow
{
    // macro to generate meta object code
    Q_OBJECT
  public:
    /**
     * constructor that creates important gui elements
     */
    GtiMapper();

    /**
     * empty destructor
     */
    ~GtiMapper();

    /**
     * enumeration of all gui elements
     * that are shown directly in the MainWindow
     */
    enum View {
        SYMBOLMANAGEMENT = 0,
        SETTINGS,
        FILEMANAGEMENT,
        ARGUMENTASSOZIATOR,
        MAPPINGOVERVIEW
    };

    /**
     * Function to change the Gui Element in the center Area
     * @param view representation of the widget that should
     *             be made visible
     * @param item a container that hold informations that are required
     *             for the mapping widgets
     */
    void changeView(View view = SYMBOLMANAGEMENT, MappingSelection* item = NULL);

  protected slots:

    /**
     * Function that is called in response to a trigger signal
     * of the saveAction element in the Menubar.
     * This function searches for ApiGroups in the ApiCalls sigelton
     * and calls the save Function in the ApiGroup Class.
     * this is not implemented yet.
     */
    void save();

    /**
     * Function that is called in response to a trigger signal
     * of the loadAction element in the Menubar.
     * This function make the FileManagement visible by using
     * the changeView Method.
     */
    void load();

  protected:
    QAction* saveAction; // save Action of the menubar (global to make it inactive)

    // widgets that can be shown in the center area of this window
    SymbolManagement* mainWidget;         // mapping of Calculations and Calls / also Overview
    ArgumentAssoziator* argMappingWidget; // mapping of arguments of calls, analyses and operations
    MappingOverview* mappingWidget; // show mapping of analyses according to a specific call / also
                                    // reordering tool
    Settings* settingWidget;        // make some definitions like path to the dtds or tempfiles
    FileManagement* fileWidget;     // add and remove specifications
};
} // namespace mapping
} // namespace guis
} // namespace gti

#endif // GTIMAPPER_H
