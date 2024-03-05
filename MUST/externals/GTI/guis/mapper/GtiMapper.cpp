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
 * @file GtiMapper.cpp
 *
 * @author Mathias Korepkat
 * @date 28.10.2011
 */

#include "GtiMapper.h"

#include "MappingSelection.h"
#include "SymbolManagement.h"
#include "Settings.h"
#include "FileManagement.h"
#include "ArgumentAssoziator.h"
#include "MappingOverview.h"
#include "Analyses.h"

using namespace gti;
using namespace gti::guis::mapping;
using namespace gti::weaver::analyses;

GtiMapper::GtiMapper()
{

    // initialize Widgets
    fileWidget = NULL;
    settingWidget = NULL;
    argMappingWidget = NULL;
    mainWidget = NULL;
    mappingWidget = NULL;

    // set window size
    this->setMinimumSize(800, 600);

    // setup the menubar

    // define the menu elements
    // file
    QMenu* fileMenu = new QMenu(tr("&File"), this);

    // save
    saveAction = new QAction(tr("&Save"), fileMenu);
    QList<QKeySequence> ssc;
    ssc.append(QKeySequence(QKeySequence::NativeText));
    ssc.append(tr("Ctrl+S"));
    saveAction->setShortcuts(ssc);
    saveAction->setDisabled(true);

    // load
    QAction* load = new QAction(tr("&Load"), fileMenu);
    QList<QKeySequence> lsc;
    lsc.append(QKeySequence(QKeySequence::NativeText));
    lsc.append(tr("Ctrl+L"));
    load->setShortcuts(lsc);

    // connect add menu elements to the menubar
    fileMenu->addAction(load);
    fileMenu->addAction(saveAction);
    menuBar()->addMenu(fileMenu);

    // connect functions to the trigger event of the menu elements
    connect(load, SIGNAL(triggered()), this, SLOT(load()));
    connect(saveAction, SIGNAL(triggered()), this, SLOT(save()));

    // show defualt view
    changeView();
}

GtiMapper::~GtiMapper() { ; }

void GtiMapper::save()
{

    // TODO get an instance of analyses, to test the
    //      visibility of the namespaces
    Analyses* analyses = Analyses::getInstance();
    bool hasGroups = analyses->hasGroup("MUST_Base");
}

void GtiMapper::load()
{
    // show FileManager
    changeView(FILEMANAGEMENT);
}

void GtiMapper::changeView(View view, MappingSelection* item)
{

    // enable/disable save Action
    if (!saveAction->isEnabled() && fileWidget != NULL && fileWidget->hasFiles()) {
        saveAction->setEnabled(true);
    } else {
        saveAction->setEnabled(false);
    }

    // workaround to cut the ownership of mainwindow and widget
    if (centralWidget() != NULL)
        centralWidget()->setParent(NULL);

    switch (view) {
    case GtiMapper::SETTINGS: {
        if (fileWidget == NULL)
            fileWidget = new FileManagement(this);

        setCentralWidget(fileWidget);
    } break;
    case GtiMapper::FILEMANAGEMENT: {
        if (fileWidget == NULL)
            fileWidget = new FileManagement(this);

        setCentralWidget(fileWidget);
    } break;
    case GtiMapper::MAPPINGOVERVIEW: {
        if (mappingWidget == NULL)
            mappingWidget = new MappingOverview(NULL, this);

        setCentralWidget(mappingWidget);
    } break;
    case GtiMapper::ARGUMENTASSOZIATOR: {
        if (argMappingWidget != NULL)
            delete (argMappingWidget);
        argMappingWidget = new ArgumentAssoziator(this);

        setCentralWidget(argMappingWidget);
    } break;
    default: {

        if (fileWidget != NULL && fileWidget->hasChanged()) {
            delete (mainWidget);
            mainWidget = NULL;
        }

        if (mainWidget == NULL)
            mainWidget = new SymbolManagement(this);

        setCentralWidget(mainWidget);
    }
    }
}
