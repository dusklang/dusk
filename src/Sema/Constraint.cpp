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
        void operator()(Constraint::BindOverloadConstraint constraint) const {
            writeIndentation();
            stream << constraint.ty.name() + " bound to declaration " + constraint.decl->range.getSubstring();
        }
        void operator()(Constraint::DisjunctionConstraint constraint) {
            writeIndentation();
            stream << "Disjunction:\n";
            indentationLevel += 1;
            bool first = true;
            for(auto& constraint: constraint.constraints) {
                if(!first) {
                    writeIndentation();
                    stream << "||\n";
                }
                first = false;
                boost::apply_visitor(*this, constraint.getData());
                stream << '\n';
            }
            indentationLevel -= 1;
        }
        void operator()(Constraint::ConjunctionConstraint constraint) {
            writeIndentation();
            stream << "Conjunction:\n";
            indentationLevel += 1;
            bool first = true;
            for(auto& constraint: constraint.constraints) {
                if(!first) {
                    writeIndentation();
                    stream << "&&\n";
                }
                first = false;
                boost::apply_visitor(*this, constraint.getData());
                stream << '\n';
            }
            indentationLevel -= 1;
        }
        void operator()(Constraint::IntegerLiteralConstraint constraint) const {
            writeIndentation();
            stream << constraint.ty.name() << " is an integer literal";
        }
        void operator()(Constraint::DecimalLiteralConstraint constraint) const {
            writeIndentation();
            stream << constraint.ty.name() << " is a decimal literal";
        }
    };

    ConstraintVisitor visitor(stream);
    boost::apply_visitor(visitor, data);
}
