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
 * @file MappingOverview.h
 *
 * @author Mathias Korepkat
 * @date 28.10.2011
 */

#ifndef MAPPINGOVERVIEW_H
#define MAPPINGOVERVIEW_H

#include <QtGui>

namespace gti
{
namespace guis
{
namespace mapping
{
class MappingSelection;
class GtiMapper;

class MappingOverview : public QWidget
{
    Q_OBJECT
  public:
    MappingOverview(MappingSelection* item = NULL, GtiMapper* parent = NULL);
    ~MappingOverview();
};
} // namespace mapping
} // namespace guis
} // namespace gti

#endif // MAPPINGOVERVIEW_H