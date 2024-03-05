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
 * @file FileManagement.cpp
 *
 * @author Mathias Korepkat
 * @date 28.10.2011
 */

#include "FileManagement.h"
#include "GtiMapper.h"
#include <iostream>

using namespace gti;
using namespace gti::guis::mapping;

FileManagement::FileManagement(QWidget* parent) : QWidget(parent)
{
    setParent(parent);
    changed = false;
    QGridLayout* l = new QGridLayout(this);

    fileList = new QListWidget(this);
    fileList->setSelectionMode(QAbstractItemView::ExtendedSelection);
    QPushButton* add = new QPushButton(tr("add"));
    QPushButton* remove = new QPushButton(tr("remove"));
    QPushButton* ok = new QPushButton(tr("ok"));

    l->addWidget(fileList, 0, 0, 5, 1);
    l->addWidget(add, 1, 1);
    l->addWidget(remove, 2, 1);
    l->addWidget(ok, 4, 1);

    l->setRowStretch(0, 1);
    l->setRowStretch(3, 1);

    connect(add, SIGNAL(clicked()), this, SLOT(addFile()));
    connect(remove, SIGNAL(clicked()), this, SLOT(removeFile()));
    connect(ok, SIGNAL(clicked()), this, SLOT(leaveWidget()));
}

bool FileManagement::hasFiles()
{
    // TODO: need to be implemented
    if (fileList->count() > 0)
        return true;
    return false;
}

bool FileManagement::hasChanged()
{
    // TODO: need to be implemented
    return changed;
}

void FileManagement::addFile()
{

    changed = true;
    QFileDialog dialog(this);
    dialog.setAcceptMode(QFileDialog::AcceptOpen);
    dialog.setFileMode(QFileDialog::ExistingFiles);
    dialog.setDefaultSuffix("xml");
    QStringList filters;
    filters.append("apiSpecification (*.xml)");
    filters.append("analyseSpecification (*.xml)");
    dialog.setNameFilters(filters);
    dialog.setWindowModality(Qt::ApplicationModal);
    dialog.setModal(true);

    if (!dialog.exec())
        return;

    QStringList l = dialog.selectedFiles();

    foreach (QString str, l) {

        if (fileList->findItems(str, Qt::MatchCaseSensitive).count() == 0) {
            QString xmlType;
            validateXml(str, &xmlType);

            if (xmlType == "analysis-specification" || xmlType == "api-specification") {
                // TODO: need to be implemented
                // load analysis and apis
                fileList->addItem(str);
            }
        }
    }
}

void FileManagement::removeFile()
{

    QList<QListWidgetItem*> itemList = fileList->selectedItems();
    foreach (QListWidgetItem* item, itemList) {
        // TODO: need to be implemented
        // unload apis, analyses
        delete (item);
    }
    fileList->update();
    changed = true;
}

void FileManagement::showEvent(QShowEvent* event)
{
    changed = false;
    QWidget::showEvent(event);
}

void FileManagement::leaveWidget()
{
    ((GtiMapper*)this->parent())->changeView(GtiMapper::SYMBOLMANAGEMENT);
}

bool FileManagement::validateXml(QString file, QString* xmlType)
{

    // validate xml
    QFile queryFile(file);
    QDomDocument dom;
    QString errorStr;
    int errorLine, errorColumn;
    if (!dom.setContent(&queryFile, true, &errorStr, &errorLine, &errorColumn)) {
        std::cerr << "ERROR: " << file.toStdString() << ": " << errorStr.toStdString()
                  << " z:" << errorLine << " sp:" << errorColumn << std::endl;
        *xmlType = "";
        return false;
    }

    *xmlType = dom.doctype().name();
    return true;
}
