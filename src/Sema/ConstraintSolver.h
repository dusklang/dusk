//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <vector>

#include "AST/AST.h"
#include "Constraint.h"
#include "Solution.h"

Optional<Solution> solveSystem(std::vector<Constraint> const& constraints);
Optional<Solution> solveConstraint(Constraint const& constraint);
