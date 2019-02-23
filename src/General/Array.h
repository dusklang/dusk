#pragma once

#include <utility>
// TODO: Is there any way to remove this dependency while still allowing llvm::ArrayRefs to be constructed from
// instances of our Array type?
#include "llvm/ADT/ArrayRef.h"

template<typename T>
struct ReverseContainer {
    T& contained;

    using iterator = typename T::reverse_iterator;
    using const_iterator = typename T::const_reverse_iterator;
    using reverse_iterator = typename T::iterator;
    using const_reverse_iterator = typename T::const_iterator;

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

template<typename Bound>
struct Range {
    Bound lowerBound, upperBound;

    struct iterator {
        Bound currentBound;

        Bound operator*() {
            return currentBound;
        }
        iterator operator++() {
            currentBound++;
            return *this;
        }
        bool operator != (iterator other) const {
            return currentBound != other.currentBound;
        }
    };
    iterator begin() const {
        return { lowerBound };
    }
    iterator end() const {
        return { upperBound };
    }
};

template<typename Bound>
Range<Bound> makeRange(Bound lowerBound, Bound upperBound) {
    return Range<Bound> { lowerBound, upperBound };
}

template<typename Bound>
struct InfiniteRange {
    Bound lowerBound;
};

template<typename Bound>
InfiniteRange<Bound> makeRange(Bound lowerBound) {
    return InfiniteRange<Bound> { lowerBound };
}

template<typename T>
class Array final {
    T* data;
    size_t _count;
    size_t _capacity;
    void deleteData() {
        operator delete(data);
    }
    static T* newData(size_t size) {
        return reinterpret_cast<T*>(operator new(sizeof(T) * size));
    }

public:
    Array() : data(nullptr), _count(0), _capacity(0) {}
    Array(std::initializer_list<T>&& list) : data(newData(list.size())), _count(list.size()) {
        auto dest = data;
        for(auto&& src: list) {
            std::memmove(dest, &src, sizeof(T));

            dest++;
        }
    }
    void destroy() {
        deleteData();
        _count = _capacity = 0;
    }
    void append(T elem) {
        if(_count == _capacity) {
            if(_capacity == 0) {
                _capacity = 20;
            } else {
                _capacity *= 2;
            }
            T* newBuf = newData(_capacity);
            std::memmove(newBuf, data, _count * sizeof(T));
            deleteData();
            data = newBuf;
        }
        std::memmove(data + _count, &elem, sizeof(T));
        _count++;
    }
    void reserve(size_t cap) {
        if(cap > _capacity) {
            _capacity = cap;
            T* newBuf = newData(_capacity);
            std::memmove(newBuf, data, _count * sizeof(T));
            deleteData();
            data = newBuf;
        }
    }
    T& operator[](size_t i) {
        assert(i < _count && "index out of bounds");
        return data[i];
    }
    T const& operator[](size_t i) const {
        assert(i < _count && "index out of bounds");
        return data[i];
    }
    size_t count() const { return _count; }
    size_t capacity() const { return _capacity; }
    bool isEmpty() const { return _count == 0; }
    T* first() {
        if(isEmpty()) {
            return nullptr;
        } else {
            return data;
        }
    }
    T* last() {
        if(isEmpty()) {
            return nullptr;
        } else {
            return &data[_count - 1];
        }
    }
    T removeLast() {
        assert(!isEmpty() && "empty Array");
        _count--;
        return std::move(data[_count]);
    }
    Range<size_t> indices() const {
        return { 0, _count };
    }
    void removeRange(InfiniteRange<size_t> range) {
        for(size_t i: makeRange(range.lowerBound, _count)) {
            data[i].~T();
        }
        _count = range.lowerBound;
    }
    llvm::ArrayRef<T> llvmArrayRef() const {
        return llvm::ArrayRef<T>(data, _count);
    }

    using iterator = T*;
    using const_iterator = T const*;

    struct reverse_iterator {
        T* elem;

        T& operator*() {
            return *elem;
        }
        reverse_iterator& operator++() {
            --elem;
            return *this;
        }
        bool operator!=(reverse_iterator const& other) const {
            return elem != other.elem;
        }
    };
    struct const_reverse_iterator {
        T const* elem;

        T const& operator*() {
            return *elem;
        }
        const_reverse_iterator& operator++() {
            --elem;
            return *this;
        }
        bool operator!=(const_reverse_iterator const& other) const {
            return elem != other.elem;
        }
    };

    using reference = T&;
    using const_reference = T const&;
    iterator begin() { return data; }
    iterator end() { return data + _count; }
    const_iterator begin() const { return data; }
    const_iterator end() const { return data + _count; }

    reverse_iterator rbegin() { return reverse_iterator { data + ((int)_count - 1) }; }
    reverse_iterator rend() { return reverse_iterator { data - 1 }; }
    const_reverse_iterator rbegin() const { return const_reverse_iterator { data + ((int)_count - 1) }; }
    const_reverse_iterator rend() const { return const_reverse_iterator { data - 1 }; }
};
