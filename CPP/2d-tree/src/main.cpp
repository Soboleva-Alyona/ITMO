#include "primitives.h"

#include <iostream>

using namespace kdtree;
int main()
{
    PointSet s;
    s.put(Point(6.0, 4.0));
    s.put(Point(5.0, 7.0));
    s.put(Point(4.0, 7.0));
    s.put(Point(9.0, 2.0));

    std::cout << s.size() << std::endl;

    std::cout << (s.contains(Point(6.0, 4.0)) ? "yes" : "no") << std::endl;
    std::cout << s;

    Rect rect(Point(0.0, 0.0), Point(4.0, 4.0));
    s.range(rect);

    std::cout << std::distance(s.begin(), s.end());
}
