# This file is part of GTI (Generic Tool Infrastructure)
#
# Copyright (C)
#   2008-2019 ZIH, Technische Universitaet Dresden, Federal Republic of Germany
#   2008-2019 Lawrence Livermore National Laboratories, United States of America
#   2013-2019 RWTH Aachen University, Federal Republic of Germany
#
# See the LICENSE file in the package base directory for details

##
# @file CMakeLists.cmake
#       CMake file for gtiMapper gui.
#
# @author Mathias Korepkat
# @date 28.10.2011

OPTION(ENABLE_GtiMapper "Build GtiMapper" OFF)
IF (ENABLE_GtiMapper)

    FIND_PACKAGE(Qt4 REQUIRED)
    FIND_PACKAGE(LibXml2 REQUIRED)

    #enable QtXml
    SET(QT_USE_QTXML TRUE)
    
    #set all sources
    SET(MAPPER_SOURCES 
            main.cpp
            GtiMapper.cpp
            MappingSelection.cpp
            SymbolManagement.cpp
            Settings.cpp
            FileManagement.cpp
            ArgumentAssoziator.cpp
            MappingOverview.cpp
            CallTable.cpp
            CalculationTree.cpp
            CalculationTreeElement.cpp
            DragDropCalculationItem.cpp
            MappingGraph.cpp
            MappingGraphItem.cpp
            OperationItem.cpp
            AnalyseArgumentItem.cpp
            ApiArgumentItem.cpp
            EdgeItem.cpp
       )
    
    #set object headers (headers using the Q_OBJECT macro)   
    SET(MAPPER_HEADERS
            GtiMapper.h
            MappingSelection.h
            SymbolManagement.h
            Settings.h
            FileManagement.h
            ArgumentAssoziator.h
            MappingOverview.h
            CalculationTree.h
      )
    
    #wrap all object headers to cpp files, using the moc tool of qt
    QT4_WRAP_CPP(MAPPER_HEADERS_MOC ${MAPPER_HEADERS})

    #Include directories and files
    INCLUDE_DIRECTORIES(${WEAVER_INCLUDE_DIRS})
    INCLUDE(${QT_USE_FILE})
    
    ADD_DEFINITIONS(${QT_DEFINITIONS})
    
    ADD_EXECUTABLE(gtiMapper ${MAPPER_SOURCES}
                             ${ANALYSES_SOURCES}
                             ${CALLS_SOURCES}
                             ${GTI_SOURCES}
                             ${LAYOUT_SOURCES}
                             ${UTILITY_SOURCES}
                             ${GENERATION_SOURCES}
                             ${MAPPER_HEADERS_MOC}
                  )
                  
    TARGET_LINK_LIBRARIES(gtiMapper ${QT_LIBRARIES} ${LIBXML2_LIBRARIES})
    SET_TARGET_PROPERTIES(gtiMapper PROPERTIES COMPILE_FLAGS "${LIBXML2_DEFINITIONS}")
    INSTALL(TARGETS gtiMapper
        PERMISSIONS 
            OWNER_READ OWNER_WRITE OWNER_EXECUTE 
            GROUP_EXECUTE GROUP_READ 
            WORLD_EXECUTE WORLD_READ
        RUNTIME DESTINATION bin
        )
        
ENDIF (ENABLE_GtiMapper)