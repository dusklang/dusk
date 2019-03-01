#pragma once

#include <utility> // pair

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

    template<typename U, size_t SmallCapacity>
    friend class Array;

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
    template<typename U, size_t SmallCapacity>
    friend class Array;

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

    iterator begin() { return data(); }
    iterator end() { return data() + this->_count; }

    reverse_iterator rbegin() { return reverse_iterator { data() + ((int)this->_count - 1) }; }
    reverse_iterator rend() { return reverse_iterator { data() - 1 }; }
};

template<typename T>
class AnyArray: public MutSlice<T> {
protected:
    size_t _capacity;

    template<typename U, size_t SmallCapacity>
    friend class Array;

    AnyArray(T* data, size_t count, size_t capacity) : MutSlice<T>(data, count), _capacity(capacity) {}
public:
    virtual void destroy() = 0;
    virtual void append(T elem) = 0;
    virtual void reserve(size_t cap) = 0;

    size_t capacity() const { return _capacity; }

    T removeLast() {
        assert(!this->isEmpty() && "empty Array");
        this->_count--;
        return std::move(this->data[this->_count]);
    }
    void removeRange(InfiniteRange<> range) {
        for(size_t i: makeRange(range.lowerBound, this->_count)) {
            this->data[i].~T();
        }
        this->_count = range.lowerBound;
    }
};

/// A collection of owned, growable data.
template<typename T, size_t SmallCapacity = 0>
class Array: public AnyArray<T> {
    // TODO: don't know how, but it would be cool to make this occupy the same memory as
    // _data, since they are mutually exclusive.
    T smallData[SmallCapacity];
    void deleteData() {
        operator delete(this->data());
    }
protected:
    static T* newData(size_t size) {
        return reinterpret_cast<T*>(operator new(sizeof(T) * size));
    }
public:
    Array() : AnyArray<T>(smallData, 0, SmallCapacity) {}
    Array(AnyArray<T>& other) : AnyArray<T>(nullptr, other._count, other._capacity) {
        memset(smallData, 0, SmallCapacity * sizeof(T));
        if(this->_capacity > SmallCapacity) {
            this->_data = other.data();
        } else {
            memcpy(smallData, other.data(), sizeof(T) * this->count());
        }
    }
    Array(std::initializer_list<T>&& list) : Array() {
        this->_count = list.size();
        this->_capacity = std::max(list.size(), SmallCapacity);
        if(this->_capacity > SmallCapacity) {
            this->_data = newData(this->_capacity);
        }
        auto dest = this->data();
        for(auto&& src: list) {
            std::memmove(dest, &src, sizeof(T));

            dest++;
        }
    }
    Array(char const* literal) : Array() {
        static_assert(std::is_same_v<T, char>, "can't construct non-string with char*");
        this->_count = strlen(literal);
        this->_capacity = std::max(this->_count, SmallCapacity);
        if(this->_capacity > SmallCapacity) {
            this->_data = this->newData(this->_capacity);
        }
        memcpy(this->data(), literal, this->_count * sizeof(char));
    }
    virtual ~Array() {}

    void destroy() override {
        if(this->_capacity > SmallCapacity) deleteData();
        this->_count = this->_capacity = 0;
    }
    void append(T elem) override {
        if(this->_count == this->_capacity) {
            if(this->_capacity == SmallCapacity) {
                this->_capacity = std::max(SmallCapacity + 20, SmallCapacity * 2);
            } else {
                this->_capacity *= 2;
            }
            T* newBuf = newData(this->_capacity);
            std::memmove((void*)newBuf, (void*)this->data(), this->_count * sizeof(T));
            if(this->_count > SmallCapacity) deleteData();
            this->_data = newBuf;
        }
        std::memmove((void*)(this->data() + this->_count), (void*)&elem, sizeof(T));
        this->_count++;
    }
    void reserve(size_t cap) override {
        if(cap > this->_capacity) {
            T* newBuf = newData(cap);
            std::memmove((void*)newBuf, (void*)this->data(), this->_count * sizeof(T));
            if(this->_capacity > SmallCapacity) deleteData();
            this->_capacity = cap;
            this->_data = newBuf;
        }
    }
};

template<typename T>
Slice<T> Slice<T>::operator+ (Slice<T> other) const {
    Array<T> result;
    result.reserve(_count + other._count);
    // TODO: Speed.
    for(auto elem: *this) result.append(elem);
    for(auto elem: other) result.append(elem);
    return result;
}

template<size_t SmallCapacity = 0>
using String = Array<char, SmallCapacity>;

using StringSlice = Slice<char>;
using MutStringSlice = MutSlice<char>;

std::ostream& operator << (std::ostream& stream, StringSlice slice);
