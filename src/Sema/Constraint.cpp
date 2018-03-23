//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Constraint.h"

void Constraint::dump(std::ostream& stream) {
    struct ConstraintVisitor: public boost::static_visitor<void> {
        std::ostream& stream;
        int indentationLevel = 0;
        ConstraintVisitor(std::ostream& stream) : stream(stream) {}
        void writeIndentation() const {
            for(int i = 0; i < indentationLevel; i++) {
                stream << "    ";
            }
        }
        void operator()(Constraint::EqualConstraint constraint) const {
            writeIndentation();
            stream << constraint.lhs.name() << " == " << constraint.rhs.name();
        }
    };

    ConstraintVisitor visitor(stream);
    boost::apply_visitor(visitor, data);
}
