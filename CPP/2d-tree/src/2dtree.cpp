#include "primitives.h"

#include <stack>

namespace kdtree {
bool PointSet::x_y_greater_comparer(const bool & x_aligned, const PtrNode & n1, const PtrNode & n2) const
{
    auto p1 = n1->point;
    auto p2 = n2->point;
    return x_aligned ? p1.x() >= p2.x() : p1.y() >= p2.y();
}

PointSet::PointSet(const std::string & filename)
{
    if (filename.empty()) {
        return;
    }
    std::vector<Point> inc;
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
        return;
    }
}

bool PointSet::empty() const
{
    return m_root == nullptr;
}

std::size_t PointSet::size() const
{
    return KDTree_size;
}

void PointSet::put(const Point & p)
{
    if (contains(p)) {
        return;
    }
    if (m_root == nullptr) {
        m_root = std::make_shared<Node>(p);
        KDTree_size++;
        return;
    }
    else {
        auto cur = m_root;
        auto new_node = std::make_shared<Node>(p);
        PtrNode prev;
        while (cur != nullptr) {
            prev = cur;
            cur = x_y_greater_comparer(cur->x_aligned, new_node, cur) ? cur->right : cur->left;
        }

        if (x_y_greater_comparer(prev->x_aligned, new_node, prev)) {
            prev->right = new_node;
        }
        else {
            prev->left = new_node;
        }

        new_node->parent = prev;
        new_node->x_aligned = !prev->x_aligned;
    }
    KDTree_size++;
}

std::size_t PointSet::height_(const PtrNode & node) const
{
    return node == nullptr ? 0 : std::max(height_(node->left), height_(node->right)) + 1;
}

bool PointSet::contains(const Point & p) const
{
    return !empty() && contains(m_root, p);
}

bool PointSet::contains(const PtrNode & node, const Point & p) const
{
    if (node == nullptr) {
        return false;
    }
    auto cur = node->point;
    if (cur == p) {
        return true;
    }
    if (node->x_aligned) {
        if (cur.x() > p.x()) {
            return contains(node->left, p);
        }
        else {
            return contains(node->right, p);
        }
    }
    else {
        if (cur.y() > p.y()) {
            return contains(node->left, p);
        }
        else {
            return contains(node->right, p);
        }
    }
}

std::pair<PointSet::iterator, PointSet::iterator> PointSet::range(const Rect & rect) const
{
    std::vector<Point> range_answ;
    auto cur = m_root;
    PtrNode start_to_find = cur;
    while (cur != nullptr) {
        if (cur->x_aligned) {
            double x_coord = cur->point.x();
            if (x_coord < rect.xmin()) {
                cur = cur->right;
                start_to_find = cur;
            }
            else if (x_coord > rect.xmax()) {
                cur = cur->left;
                start_to_find = cur;
            }
            else {
                break;
            }
        }
        else {
            double y_coord = cur->point.y();
            if (y_coord < rect.ymin()) {
                cur = cur->right;
                start_to_find = cur;
            }
            else if (y_coord > rect.ymax()) {
                cur = cur->left;
                start_to_find = cur;
            }
            else {
                break;
            }
        }
    }

    if (start_to_find == nullptr) {
        return {std::make_shared<PVector>(range_answ), std::make_shared<PVector>(range_answ)->end()};
    }

    std::stack<PtrNode> s;
    s.push(nullptr);
    auto t = start_to_find;
    while (true) {
        if (t != nullptr) {
            auto p = t->point;
            if (rect.contains(p)) {
                range_answ.emplace_back(p);
            }
            s.push(t);
            t = t->left;
        }
        else {
            if (s.top() == nullptr) {
                break;
            }
            t = s.top();
            s.pop();
            t = t->right;
        }
    }
    auto result = std::make_shared<PVector>(range_answ);
    return {result, result->end()};
}

PointSet::iterator PointSet::begin() const
{
    if (m_points.size() < KDTree_size) {
        m_points.clear();
        tree_travel(m_points, m_root);
    }
    return m_points.begin();
}

PointSet::iterator PointSet::end() const
{
    return m_points.end();
}

std::optional<Point> PointSet::nearest(const Point & p) const
{
    auto start = m_root;
    auto min_dst = start->point.distance(p);
    auto answ = start;
    while (start != nullptr) {
        auto p_coord = start->x_aligned ? p.x() : p.y();
        auto n_coord = start->x_aligned ? start->point.x() : start->point.y();
        if (start->left != nullptr && start->right != nullptr) {
            start = p_coord < n_coord ? start->left : start->right;
        }
        else if (start->left != nullptr) {
            start = start->left;
        }
        else if (start->right != nullptr) {
            start = start->right;
        }
        else {
            break;
        }
        if (start->point.distance(p) <= min_dst) {
            min_dst = start->point.distance(p);
            answ = start;
        }
    }
    return answ->point;
}

std::pair<PointSet::iterator, PointSet::iterator> PointSet::nearest(const Point & p, std::size_t k) const
{
    std::vector<Point> answ;
    std::map<double, const Point *> m;
    nearest_k(p, m_root, m);
    std::size_t cnt = 0;
    for (auto i = m.begin(); cnt < k; i++, cnt++) {
        if (i == m.end()) {
            break;
        }
        answ.emplace_back(*i->second);
    }
    auto res = std::make_shared<PVector>(answ);
    return {res, res->end()};
}

void PointSet::nearest_k(const Point & p, const PtrNode & node, std::map<double, const Point *> & m) const
{
    if (node == nullptr) {
        return;
    }
    auto dist = node->point.distance(p);
    m.insert({dist, &node->point});
    nearest_k(p, node->left, m);
    nearest_k(p, node->right, m);
}

std::ostream & operator<<(std::ostream & os, const PointSet & pset)
{
    for (auto i = pset.begin(); i != pset.end(); i++) {
        os << *(i);
    }
    return os;
}
} // namespace kdtree
