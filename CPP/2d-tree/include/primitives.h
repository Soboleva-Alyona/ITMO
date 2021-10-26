#pragma once

#include <algorithm>
#include <cmath>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <queue>
#include <set>

class Point
{
public:
    Point(double x, double y)
        : a(x)
        , b(y)
    {
    }

    Point(const Point & p) = default;

    double x() const
    {
        return a;
    }
    double y() const
    {
        return b;
    }
    double distance(const Point & p) const
    {
        double sqr_delta_x = pow(a - p.x(), 2);
        double sqr_delta_y = pow(b - p.y(), 2);
        return sqrt(sqr_delta_x + sqr_delta_y);
    }

    bool operator<(const Point & p) const
    {
        return a < p.x() || (a == p.x() && b < p.y());
    }
    bool operator>(const Point & p) const
    {
        return a > p.x() || (a == p.x() && b > p.y());
    }
    bool operator<=(const Point & p) const
    {
        return !(*this > p);
    }
    bool operator>=(const Point & p) const
    {
        return !(*this < p);
    }
    bool operator==(const Point & p) const
    {
        return (p.x() == a && p.y() == b);
    }
    bool operator!=(const Point & p) const
    {
        return !(*this == p);
    }

    friend std::ostream & operator<<(std::ostream & os, const Point & p)
    {
        os << '(' << p.x() << ',' << p.y() << ')' << '\n';
        return os;
    }

    Point & operator=(const Point &) = default;

private:
    double a;
    double b;
};

class Rect
{
public:
    Rect(const Point & left_bottom, const Point & right_top)
        : lb(left_bottom)
        , rt(right_top)
    {
    }

    double xmin() const
    {
        return lb.x();
    }
    double ymin() const
    {
        return lb.y();
    }
    double xmax() const
    {
        return rt.x();
    }
    double ymax() const
    {
        return rt.y();
    }
    double distance(const Point & p) const
    {
        double delta_x = std::max({0.0, p.x() - xmax(), xmin() - p.x()});
        double delta_y = std::max({0.0, p.y() - ymax(), ymin() - p.y()});
        return std::sqrt(delta_x * delta_x + delta_y * delta_y);
    }

    bool contains(const Point & p) const
    {
        return p.x() <= xmax() && p.x() >= xmin() && p.y() <= ymax() && p.y() >= ymin();
    }
    bool intersects(const Rect & rect) const
    {
        return (contains(Point(rect.xmax(), rect.ymax())) || contains(Point(rect.xmin(), rect.ymax())) ||
                contains(Point(rect.xmax(), rect.ymin())) || contains(Point(rect.xmin(), rect.ymin())) ||
                rect.contains(lb) || rect.contains(rt) || rect.contains(Point(xmin(), ymax())) || rect.contains(Point(xmax(), ymin())));
    }

private:
    const Point lb;
    const Point rt;
};

template <class T>
class iterator
{
public:
    using iterator_category = std::forward_iterator_tag;
    using difference_type = std::ptrdiff_t;
    using value_type = Point;
    using pointer = const value_type *;
    using reference = const value_type &;

    iterator() = default;

    iterator(const std::shared_ptr<T> & shared_t)
        : points_ptr(shared_t)
        , points_it(points_ptr->begin())
    {
    }

    iterator(const typename T::iterator & iterator)
        : points_it(iterator)
    {
    }

    reference operator*() const
    {
        return *points_it;
    }

    pointer operator->() const
    {
        return points_it.operator->();
    }

    iterator & operator++()
    {
        ++points_it;
        return *this;
    }

    iterator operator++(int)
    {
        iterator temp = *this;
        operator++();
        return temp;
    }

    friend bool operator==(const iterator & lhs, const iterator & rhs)
    {
        return lhs.points_it == rhs.points_it;
    }

    friend bool operator!=(const iterator & lhs, const iterator & rhs)
    {
        return !(lhs == rhs);
    }

private:
    std::shared_ptr<T> points_ptr;
    typename T::iterator points_it;
};

namespace rbtree {

class PointSet
{
public:
    using PSet = std::set<Point>;
    using iterator = iterator<PSet>;

    PointSet(const std::string & filename = {})
    {
        if (filename.empty()) {
            return;
        }
        try {
            std::ifstream fs(filename);

            double x, y;
            while (fs) {
                fs >> x >> y;
                if (fs.fail()) {
                    break;
                }
                put(Point(x, y));
            }
        }
        catch (...) {
            std::cout << "Can't read " << filename << ".\n";
        }
    }

    bool empty() const
    {
        return points.empty();
    }
    std::size_t size() const
    {
        return points.size();
    }
    void put(const Point & p)
    {
        points.insert(p);
    }
    bool contains(const Point & p) const
    {
        return points.find(p) != points.end();
    }

    // second iterator points to an element out of range
    std::pair<iterator, iterator> range(const Rect & rect) const
    {
        auto rng_answ = std::make_shared<PSet>();
        for (auto i = points.begin(); i != points.end(); i++) {
            if (rect.contains(*i)) {
                rng_answ->insert(*i);
            }
        }
        return {rng_answ, rng_answ->end()};
    }
    iterator begin() const
    {
        return points.begin();
    }
    iterator end() const
    {
        return points.end();
    }

    std::optional<Point> nearest(const Point & p) const
    {
        auto answ = &p;
        double min_dist = p.distance(*points.begin());
        for (auto i = points.begin(); i != points.end(); i++) {
            if (p.distance(*i) <= min_dist) {
                min_dist = p.distance(*i);
                answ = i.operator->();
            }
        }
        return *answ;
    }
    // second iterator points to an element out of range
    std::pair<iterator, iterator> nearest(const Point & p, std::size_t k) const
    {
        std::set<Point> nrst_answ;
        std::map<const double, const Point *> m;
        for (auto i = points.begin(); i != points.end(); i++) {
            const double r = p.distance(*i);
            m.insert({r, &*i});
        }
        std::size_t cnt = 0;
        for (auto i = m.begin(); cnt < k; i++, cnt++) {
            if (i == m.end()) {
                break;
            }
            auto tmp = i->second;
            nrst_answ.insert(*tmp);
        }
        auto answ = std::make_shared<PSet>(nrst_answ);
        return {answ, answ->end()};
    }

    friend std::ostream & operator<<(std::ostream & os, const PointSet & pset)
    {
        for (auto i = pset.begin(); i != pset.end(); i++) {
            os << *(i);
        }
        return os;
    }

private:
    PSet points;
};

} // namespace rbtree

namespace kdtree {

class PointSet
{
    struct Node;
    using PVector = std::vector<Point>;
    using PtrNode = std::shared_ptr<Node>;

public:
    using iterator = iterator<PVector>;

    PointSet(const std::string & filename = {});

    bool x_y_greater_comparer(const bool & x_aligned, const PtrNode & n1, const PtrNode & n2) const;

    bool empty() const;
    std::size_t size() const;
    void put(const Point &);

    std::size_t height_(const PtrNode & node) const;

    bool contains(const Point &) const;

    bool contains(const PtrNode & node, const Point & p) const;

    std::pair<iterator, iterator> range(const Rect &) const;
    iterator begin() const;
    iterator end() const;

    std::optional<Point> nearest(const Point &) const;
    std::pair<iterator, iterator> nearest(const Point & p, std::size_t k) const;

    void nearest_k(const Point & p, const PtrNode & node, std::map<double, const Point *> & m) const;

    friend std::ostream & operator<<(std::ostream & os, const PointSet & pset);

    void tree_travel(std::vector<Point> & points, const PtrNode & node) const
    {
        if (node == nullptr) {
            return;
        }
        points.emplace_back(node->point);
        tree_travel(points, node->left);
        tree_travel(points, node->right);
    }

private:
    struct Node
    {
        const Point point;
        PtrNode left;
        PtrNode right;
        std::weak_ptr<Node> parent;
        bool x_aligned;

        Node(const Point & p)
            : point(p)
            , left(nullptr)
            , right(nullptr)
            , parent()
            , x_aligned(true){};
    };
    PtrNode m_root = nullptr;
    std::size_t KDTree_size = 0;
    mutable PVector m_points;
};

} // namespace kdtree