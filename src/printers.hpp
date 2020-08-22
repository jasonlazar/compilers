#ifndef __PRINTERS_HPP__
#define __PRINTERS_HPP__

#include "enums.hpp"

#include "symbol.h"

std::ostream& operator << (std::ostream& out, Type t);
std::ostream& operator << (std::ostream& out, unary_ops op);
std::ostream& operator << (std::ostream& out, binary_ops op);

void printChar(std::ostream& out, char c);

#endif
