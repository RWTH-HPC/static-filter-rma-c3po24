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
 * @file MappingSelection.h
 *
 * @author Mathias Korepkat
 * @date 28.10.2011
 */

#ifndef MAPPINGSELECTION_H
#define MAPPINGSELECTION_H

#include <QWidget>

namespace gti
{
namespace guis
{
namespace mapping
{
class MappingSelection : public QWidget
{
    Q_OBJECT
  public:
    explicit MappingSelection(QWidget* parent = 0);

  signals:

  public slots:
};
} // namespace mapping
} // namespace guis
} // namespace gti

#endif // MAPPINGSELECTION_H
