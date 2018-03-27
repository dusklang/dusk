//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <vector>

#include "AST/AST.h"
#include "Constraint.h"
#include "Solution.h"

Optional<Solution> solveSystem(const std::vector<Constraint>& constraints);
Optional<Solution> solveConstraint(const Constraint& constraint);
