#-------------------------------------------------
#
# Project created by QtCreator 2011-10-25T15:53:34
#
#-------------------------------------------------

QT       += core gui

TARGET = GtiTool
TEMPLATE = app


SOURCES += main.cpp \
    GtiMapper.cpp \
    MappingSelection.cpp \
    SymbolManagement.cpp \
    Settings.cpp \
    FileManagement.cpp \
    ArgumentAssoziator.cpp \
    MappingOverview.cpp \
    CallTable.cpp \
    CalculationTree.cpp \
    CalculationTreeElement.cpp \
    DragDropCalculationItem.cpp \
    MappingGraph.cpp \
    MappingGraphItem.cpp \
    OperationItem.cpp \
    AnalyseArgumentItem.cpp \
    ApiArgumentItem.cpp \
    EdgeItem.cpp

HEADERS  += \
    GtiMapper.h \
    MappingSelection.h \
    SymbolManagement.h \
    Settings.h \
    FileManagement.h \
    ArgumentAssoziator.h \
    MappingOverview.h \
    CallTable.h \
    CalculationTree.h \
    CalculationTreeElement.h \
    DragDropCalculationItem.h \
    MappingGraph.h \
    MappingGraphItem.h \
    OperationItem.h \
    AnalyseArgumentItem.h \
    ApiArgumentItem.h \
    EdgeItem.h


INCLUDEPATH += /home/mk/workspace/gti/system-builder/weaver/analyses/
INCLUDEPATH += /home/mk/workspace/gti/system-builder/weaver/calls
INCLUDEPATH += /home/mk/workspace/gti/system-builder/weaver/weaver/gti
INCLUDEPATH += /home/mk/workspace/gti/system-builder/weaver/layout
INCLUDEPATH += /home/mk/workspace/gti/system-builder/weaver/utility
INCLUDEPATH += /home/mk/workspace/gti/system-builder/weaver/generation
INCLUDEPATH += /usr/include/libxml2/libxml/
INCLUDEPATH += /home/mk/workspace/gti/modules/interfaces
INCLUDEPATH += /home/mk/workspace/gti/modules/common
INCLUDEPATH += /home/mk/workspace/gti/system-builder/weaver
