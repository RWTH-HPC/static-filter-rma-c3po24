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
 * @file ArgumentAssoziator.h
 *
 * @author Mathias Korepkat
 * @date 28.10.2011
 */

#ifndef ARGUMENTASSOZIATOR_H
#define ARGUMENTASSOZIATOR_H

#include <QWidget>

namespace gti
{
namespace guis
{
namespace mapping
{
class ArgumentAssoziator : public QWidget
{
    Q_OBJECT
  public:
    explicit ArgumentAssoziator(QWidget* parent = 0);
};
} // namespace mapping
} // namespace guis
} // namespace gti
#endif // ARGUMENTASSOZIATOR_H
