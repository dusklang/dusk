//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <map>
#include <vector>

#include "AST/AST.h"
#include "Constraint.h"

std::map<int, Type> solveSystem(std::vector<Constraint>& constraints);
std::map<int, Type> solveConstraint(Constraint constraint);
