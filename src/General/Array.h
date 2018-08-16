//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <utility>

template<typename T>
struct ReverseContainer {
    T& contained;

    typedef typename T::reverse_iterator iterator;
    typedef typename T::const_reverse_iterator const_iterator;
    typedef typename T::iterator reverse_iterator;
    typedef typename T::const_iterator const_reverse_iterator;

    iterator begin() { return contained.rbegin(); }
    iterator end() { return contained.rend(); }
    const_iterator begin() const { return contained.rbegin(); }
    const_iterator end() const { return contained.rend(); }

    reverse_iterator rbegin() { return contained.begin(); }
    reverse_iterator rend() { return contained.end(); }
    const_reverse_iterator rbegin() const { return contained.begin(); }
    const_reverse_iterator rend() const { return contained.end(); }
};

template<typename T>
ReverseContainer<T> reverse(T& contained) {
    return ReverseContainer<T> { contained };
}

template<typename T, typename U>
struct ZipContainer {
    T& t;
    U& u;

    struct iterator {
        std::pair<typename T::iterator, typename U::iterator> its;

        std::pair<typename T::reference, typename U::reference> operator*() {
            return std::pair<typename T::reference, typename U::reference>{ *its.first, *its.second };
        }
        iterator& operator++() {
            ++its.first; ++its.second;
            return *this;
        }
        bool operator!=(iterator const& other) const {
            return its.first != other.its.first && its.second != other.its.second;
        }
    };
    struct const_iterator {
        std::pair<typename T::const_iterator, typename U::const_iterator> its;

        std::pair<typename T::const_reference, typename U::const_reference> operator*() const {
            return std::pair { *its.first, *its.second };
        }
        const_iterator& operator++() {
            ++its.first; ++its.second;
        }
        bool operator!=(const_iterator const& other) const {
            return its.first != other.its.first && its.second != other.its.second;
        }
    };

    struct reverse_iterator {
        std::pair<typename T::reverse_iterator, typename U::reverse_iterator> its;

        std::pair<typename T::reference, typename U::reference> operator*() {
            return std::pair { *its.first, *its.second };
        }
        reverse_iterator& operator++() {
            ++its.first; ++its.second;
        }
        bool operator!=(reverse_iterator const& other) const {
            return its.first != other.its.first && its.second != other.its.second;
        }
    };
    struct const_reverse_iterator {
        std::pair<typename T::const_reverse_iterator, typename U::const_reverse_iterator> its;

        std::pair<typename T::const_reference, typename U::const_reference> operator*() const {
            return std::pair { *its.first, *its.second };
        }
        const_reverse_iterator& operator++() {
            ++its.first; ++its.second;
        }
        bool operator!=(const_reverse_iterator const& other) const {
            return its.first != other.its.first && its.second != other.its.second;
        }
    };

    template<typename IterT, typename IterU>
    std::pair<IterT, IterU> realEndIters(IterT tBegin, IterT tEnd, IterU uBegin, IterU uEnd) {
        while(tBegin != tEnd && uBegin != uEnd) {
            ++tBegin;
            ++uBegin;
        }
        return std::pair { tBegin, uBegin };
    }

    iterator begin() { return iterator {{t.begin(), u.begin()}}; }
    iterator end() {
        return iterator {
            realEndIters(t.begin(), t.end(), u.begin(), u.end())
        };
    }
    const_iterator begin() const { return iterator {{t.begin(), u.begin()}}; }
    const_iterator end() const {
        return iterator {
            realEndIters(t.begin(), t.end(), u.begin(), u.end())
        };
    }

    reverse_iterator rbegin() { return iterator {{t.begin(), u.begin()}}; }
    reverse_iterator rend() {
        return iterator {
            realEndIters(t.rbegin(), t.rend(), u.rbegin(), u.rend())
        };
    }
    const_reverse_iterator rbegin() const { return iterator {{t.begin(), u.begin()}}; }
    const_reverse_iterator rend() const {
        return iterator {
            realEndIters(t.rbegin(), t.rend(), u.rbegin(), u.rend())
        };
    }
};

template<typename T, typename U>
ZipContainer<T, U> zip(T& t, U& u) {
    return ZipContainer<T, U> { t, u };
}
