//  Copyright © 2019 Zach Wolfe. All rights reserved.

#pragma once

#include <utility>

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

template<typename T, size_t inlineSize = 0>
class Array final {
    T* data;
    uint32_t _size;
    void deleteData() {
        for(auto& elem: *this) {
            elem.~T();
        }
        operator delete(data);
    }
    static T* newData(uint32_t size) {
        return reinterpret_cast<T*>(operator new(sizeof(T) * size));
    }

public:
    Array() : data(nullptr), _size(0) {}
    Array(std::initializer_list<T>&& list) : data(newData(list.size())), _size(list.size()) {
        auto dest = data;
        for(auto&& src: list) {
            std::memmove(dest, &src, sizeof(T));

            dest++;
        }
    }
    Array(Array&& other) : data(nullptr), _size(0) {
        std::swap(data, other.data);
        std::swap(_size, other._size);
    }
    Array(Array& other) : _size(other._size) {
        data = newData(_size);
        auto dest = data;
        // Intentionally call copy constructor so we can just copy the raw bytes.
        for(T elem: other) {
            std::memmove(dest, &elem, sizeof(T));

            dest++;
        }
    }
    Array& operator=(Array& other) {
        deleteData();
        Array copy(other);
        std::swap(*this, copy);

        return *this;
    }
    Array& operator=(Array&& other) {
        deleteData();
        data = nullptr;
        _size = 0;
        std::swap(data, other.data);
        std::swap(_size, other._size);

        return *this;
    }
    ~Array() {
        deleteData();
    }
    void push_back(T&& elem) {
        T* newBuf = newData(_size + 1);
        std::memmove(newBuf, data, _size * sizeof(T));
        std::memmove(newBuf + _size, &elem, sizeof(T));
        deleteData();
        data = newBuf;
        _size++;
    }
    T& operator[](size_t i) {
        assertTrueMessage(i < _size, "index out of bounds");
        return data[i];
    }
    T const& operator[](size_t i) const {
        assertTrueMessage(i < _size, "index out of bounds");
        return data[i];
    }
    size_t size() const { return _size; }

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

    iterator begin() { return data; }
    iterator end() { return data + _size; }
    const_iterator begin() const { return data; }
    const_iterator end() const { return data + _size; }

    reverse_iterator rbegin() { return reverse_iterator { data + ((int)_size - 1) }; }
    reverse_iterator rend() { return reverse_iterator { data - 1 }; }
    const_reverse_iterator rbegin() const { return const_reverse_iterator { data + ((int)_size - 1) }; }
    const_reverse_iterator rend() const { return const_reverse_iterator { data - 1 }; }
};
