#pragma once

#include "Misc.h"

template<typename First, typename Second>
struct Pair final {
    First first;
    Second second;
};

template<typename T>
class ConstReverseContainer {

}

template<typename T>
class ReverseContainer {
    iterator _begin, _end;
public:
    explicit ReverseContainer(T& contained) : _begin(contained.rbegin()), _end(contained.rend()) {}

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
    T t;
    U u;

    struct iterator {
        Pair<typename T::iterator, typename U::iterator> its;

        Pair<typename T::reference, typename U::reference> operator*() {
            return Pair<typename T::reference, typename U::reference>{ *its.first, *its.second };
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
        Pair<typename T::const_iterator, typename U::const_iterator> its;

        Pair<typename T::const_reference, typename U::const_reference> operator*() const {
            return Pair { *its.first, *its.second };
        }
        const_iterator& operator++() {
            ++its.first; ++its.second;
        }
        bool operator!=(const_iterator const& other) const {
            return its.first != other.its.first && its.second != other.its.second;
        }
    };

    struct reverse_iterator {
        Pair<typename T::reverse_iterator, typename U::reverse_iterator> its;

        Pair<typename T::reference, typename U::reference> operator*() {
            return Pair { *its.first, *its.second };
        }
        reverse_iterator& operator++() {
            ++its.first; ++its.second;
        }
        bool operator!=(reverse_iterator const& other) const {
            return its.first != other.its.first && its.second != other.its.second;
        }
    };
    struct const_reverse_iterator {
        Pair<typename T::const_reverse_iterator, typename U::const_reverse_iterator> its;

        Pair<typename T::const_reference, typename U::const_reference> operator*() const {
            return Pair { *its.first, *its.second };
        }
        const_reverse_iterator& operator++() {
            ++its.first; ++its.second;
        }
        bool operator!=(const_reverse_iterator const& other) const {
            return its.first != other.its.first && its.second != other.its.second;
        }
    };

    template<typename IterT, typename IterU>
    Pair<IterT, IterU> realEndIters(IterT tBegin, IterT tEnd, IterU uBegin, IterU uEnd) {
        while(tBegin != tEnd && uBegin != uEnd) {
            ++tBegin;
            ++uBegin;
        }
        return Pair<IterT, IterU> { tBegin, uBegin };
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
ZipContainer<T, U> zip(T t, U u) {
    return ZipContainer<T, U> { t, u };
}

template<typename Bound = size_t>
class Range {
    Bound _lowerBound, _upperBound;

public:
    Range(Bound lowerBound, Bound upperBound) : _lowerBound(lowerBound), _upperBound(upperBound) {
        assert(lowerBound <= upperBound);
    }

    Bound lowerBound() const { return _lowerBound; }
    Bound upperBound() const { return _upperBound; }

    bool contains(Bound bound) const {
        return _lowerBound <= bound && bound < _upperBound;
    }

    bool contains(Range<Bound> other) const {
        return _lowerBound <= other._lowerBound && other._upperBound <= _upperBound;
    }

    using reference = Bound;
    using const_reference = Bound;
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
        return { _lowerBound };
    }
    iterator end() const {
        return { _upperBound };
    }
};

template<typename Bound = size_t>
struct InfiniteRange {
    Bound lowerBound;
};

template<typename T>
class Slice {
protected:
    T const* _data;
    size_t _count;

    Slice(T const* data, size_t count) : _data(data), _count(count) {}
public:
    Range<> indices() const {
        return { 0, _count };
    }
    Slice(char const* literal) {
        static_assert(std::is_same_v<T, char>, "can't construct non-char slice from char*");
        _count = strlen(literal);
        _data = literal;
    }
    size_t count() const { return _count; }
    bool isEmpty() const { return _count == 0; }
    T const& operator[](size_t i) const {
        assert(i < _count && "index out of bounds");
        return _data[i];
    }
    Slice<T> operator[](Range<> range) const {
        assert(indices().contains(range) && "range out of bounds");
        return Slice<T>(_data + range.lowerBound(), range.upperBound() - range.lowerBound());
    }
    T const* first() const {
        if(isEmpty()) return nullptr;
        return _data;
    }
    T const* last() const {
        if(isEmpty()) return nullptr;
        return &_data[_count - 1];
    }

    Slice<T> operator+ (Slice<T> other) const;
    bool operator== (Slice<T> other) const {
        if(_count != other._count) return false;
        for(auto [left, right]: zip(*this, other)) {
            if(left != right) return false;
        }
        return true;
    }

    using const_reference = T const&;
    using reference = const_reference;

    using const_iterator = T const*;
    using iterator = const_iterator;
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

    const_iterator begin() const { return _data; }
    const_iterator end() const { return _data + _count; }

    const_reverse_iterator rbegin() const { return const_reverse_iterator { _data + ((int)_count - 1) }; }
    const_reverse_iterator rend() const { return const_reverse_iterator { _data - 1 }; }
};

template<typename T>
class MutSlice: public Slice<T> {
protected:
    T* data() { return const_cast<T*>(this->_data); }

    MutSlice(T* data, size_t count) : Slice<T>(data, count) {}
public:
    using Slice<T>::operator[];
    T& operator[](size_t i) {
        assert(i < this->_count && "index out of bounds");
        return data()[i];
    }
    MutSlice<T> operator[](Range<> range) {
        assert(this->indices().contains(range) && "range out of bounds");
        return MutSlice<T>(data() + range.lowerBound(), range.upperBound() - range.lowerBound());
    }
    T* first() {
        if(this->isEmpty()) return nullptr;
        return data();
    }
    T* last() {
        if(this->isEmpty()) return nullptr;
        return &data()[this->_count - 1];
    }

    using iterator = T*;

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

    using Slice<T>::begin;
    using Slice<T>::end;
    iterator begin() { return data(); }
    iterator end() { return data() + this->_count; }

    using Slice<T>::rbegin;
    using Slice<T>::rend;
    reverse_iterator rbegin() { return reverse_iterator { data() + ((int)this->_count - 1) }; }
    reverse_iterator rend() { return reverse_iterator { data() - 1 }; }
};

constexpr size_t KB = 1024;
constexpr size_t MB = 1024 * KB;
constexpr size_t GB = 1024 * MB;

/// A collection of owned, growable data allocated from a fixed-size blob of bytes
template<typename T>
class Array: public MutSlice<T> {
    size_t _capacity;

    /// The number of bytes we had to advance to get from the buffer to our aligned data pointer.
    uint8_t offset;

    void deleteData() {
        operator delete(this->data());
    }
    static T* newData(size_t size) {
        return reinterpret_cast<T*>(operator new(sizeof(T) * size));
    }
public:
    Array(size_t maxSize) : MutSlice<T>(nullptr, 0) {
        _capacity = maxSize;
        void *buf = mmap(nullptr, (maxSize + 1) * sizeof(T), PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        assert(buf != MAP_FAILED);
        std::cout << "buf is " << buf << '\n';
        uint8_t* alignedBuf = (uint8_t*)(((uintptr_t)buf + (uintptr_t)(alignof(T) - 1)) & ~((uintptr_t)(alignof(T) - 1)));
        offset = alignedBuf - (uint8_t*)buf;
        this->_data = (T*)alignedBuf;
        this->_count = 0;
    }
    Array() : Array((8 * GB) / sizeof(T)) {}
    Array(Array<T>& other) : MutSlice<T>(other.data(), other.count()), _capacity(other._capacity) {}
    Array(std::initializer_list<T>&& list) : Array() {
        this->_count = list.size();
        T* dest = this->data();
        for(auto&& src: list) std::memmove(dest++, &src, sizeof(T));
    }
    Array(Slice<T> slice) : Array() {
        this->_count = slice.count();
        T* dest = this->data();
        for(auto src: slice) std::memmove(dest++, &src, sizeof(T));
    }

    void destroy() {
        munmap(this->data() - offset, (_capacity + 1) * sizeof(T));
        this->_count = this->_capacity = 0;
    }
    void append(T const& elem) {
        assert(this->_count < this->_capacity && "ran out of space!");
        std::memmove(this->data() + this->_count, &elem, sizeof(T));
        this->_count++;
    }
    void clear() {
        this->_count = 0;
    }
    void fill(T elem, size_t count) {
        for(size_t i = 0; i < count; i++) {
            append(elem);
        }
    }
    Array<T>& operator=(Slice<T> slice) {
        assert(slice.count() < this->_capacity && "slice too big");
        this->_count = slice.count();
        T* dest = this->data();
        for(auto elem: slice) std::memmove(dest++, &elem, sizeof(T));

        return *this;
    }

    size_t capacity() const { return _capacity; }

    T removeLast() {
        assert(!this->isEmpty() && "empty Array");
        this->_count--;
        return std::move(this->data()[this->_count]);
    }
    void removeRange(InfiniteRange<> range) {
        for(size_t i: makeRange(range.lowerBound, this->_count)) {
            this->data[i].~T();
        }
        this->_count = range.lowerBound;
    }
};

template<typename T>
Slice<T> Slice<T>::operator+ (Slice<T> other) const {
    Array<T> result(_count + other._count);
    // TODO: Speed.
    for(auto elem: *this) result.append(elem);
    for(auto elem: other) result.append(elem);
    return result;
}

using String = Array<char>;

using StringSlice = Slice<char>;
using MutStringSlice = MutSlice<char>;

std::ostream& operator << (std::ostream& stream, StringSlice slice);
