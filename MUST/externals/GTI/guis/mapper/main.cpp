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

#include <QtGui/QApplication>
#include "GtiMapper.h"

using namespace gti;
using namespace gti::guis::mapping;

int main(int argc, char* argv[])
{
    QApplication a(argc, argv);
    GtiMapper w;
    w.show();

    return a.exec();
}