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
 * @file SymbolManagement.cpp
 *
 * @author Mathias Korepkat
 * @date 28.10.2011
 */

#include "SymbolManagement.h"
#include "GtiMapper.h"

using namespace gti;
using namespace gti::guis::mapping;

SymbolManagement::SymbolManagement(GtiMapper* parent) : QWidget(parent) {}