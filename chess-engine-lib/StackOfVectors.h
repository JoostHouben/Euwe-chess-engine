#include "ClangDiagnosticIgnore.h"

#pragma once

#include "MyAssert.h"

#include <iterator>
#include <utility>
#include <vector>

template <typename T>
class StackVector;

template <typename T>
class StackOfVectors {
  public:
    StackOfVectors()
#ifndef NDEBUG
        : isLocked_(false)
#endif
    {
    }

    // Can't be copied or moved because that would invalidate all the StackVector objects.
    StackOfVectors(const StackOfVectors&) = delete;
    StackOfVectors(StackOfVectors&&)      = delete;

    void reserve(size_t size) { items_.reserve(size); }

    StackVector<T> makeStackVector() {
#ifndef NDEBUG
        MY_ASSERT(!isLocked_);
        isLocked_ = true;
#endif
        return StackVector<T>(*this);
    }

    size_t size() const { return items_.size(); }

  private:
    friend class StackVector<T>;

    using ContainerT = std::vector<T>;

    ContainerT items_;
#ifndef NDEBUG
    bool isLocked_;
#endif
};

template <typename T>
class StackVector;

template <typename T>
class StackVectorIterator {
  public:
    using difference_type   = int;
    using value_type        = T;
    using pointer           = T*;
    using reference         = T&;
    using iterator_category = std::random_access_iterator_tag;
    using iterator_concept  = std::contiguous_iterator_tag;

    StackVectorIterator(StackVector<T>& parent, int idx) : parent_(parent), idx_(idx) {}

    StackVectorIterator& operator=(const StackVectorIterator& rhs) {
        MY_ASSERT(&parent_ == &rhs.parent_);
        idx_ = rhs.idx_;
        return *this;
    }

    T& operator*() const { return parent_[idx_]; }

    T* operator->() { return &parent_[idx_]; }
    const T* operator->() const { return &parent_[idx_]; }

    StackVectorIterator& operator+=(int offset) {
        idx_ += offset;
        return *this;
    }

    StackVectorIterator& operator-=(int offset) {
        idx_ -= offset;
        return *this;
    }

    StackVectorIterator& operator++() {
        (*this) += 1;
        return *this;
    }

    StackVectorIterator operator++(int) {
        StackVectorIterator copy(*this);
        operator++();
        return copy;
    }

    StackVectorIterator& operator--() {
        (*this) -= 1;
        return *this;
    }

    StackVectorIterator& operator--(int) {
        StackVectorIterator copy(*this);
        operator--();
        return copy;
    }

    StackVectorIterator operator+(int offset) const {
        StackVectorIterator copy(*this);
        copy += offset;
        return copy;
    }
    StackVectorIterator operator-(int offset) const {
        StackVectorIterator copy(*this);
        copy -= offset;
        return copy;
    }

    int operator-(const StackVectorIterator& rhs) const {
        MY_ASSERT(&parent_ == &rhs.parent_);
        return idx_ - rhs.idx_;
    }

    T& operator[](int offset) { return *(*this + offset); }

    auto operator<=>(const StackVectorIterator& rhs) const {
        MY_ASSERT(&parent_ == &rhs.parent_);
        return idx_ <=> rhs.idx_;
    }

    bool operator==(const StackVectorIterator& rhs) const {
        MY_ASSERT(&parent_ == &rhs.parent_);
        return idx_ == rhs.idx_;
    }

  private:
    StackVector<T>& parent_;
    int idx_;
};

template <typename T>
StackVectorIterator<T> operator+(int offset, const StackVectorIterator<T>& it) {
    return (it + offset);
}

template <typename T>
StackVectorIterator<T> operator-(int offset, const StackVectorIterator<T>& it) {
    return (it - offset);
}

template <typename T>
class StackVectorConstIterator {
  public:
    using difference_type   = int;
    using value_type        = T;
    using pointer           = const T*;
    using reference         = const T&;
    using iterator_category = std::random_access_iterator_tag;
    using iterator_concept  = std::contiguous_iterator_tag;

    StackVectorConstIterator(const StackVector<T>& parent, int idx) : parent_(parent), idx_(idx) {}

    StackVectorConstIterator& operator=(const StackVectorConstIterator& rhs) {
        MY_ASSERT(&parent_ == &rhs.parent_);
        idx_ = rhs.idx_;
        return *this;
    }

    const T& operator*() const { return parent_[idx_]; }

    const T* operator->() const { return &parent_[idx_]; }

    StackVectorConstIterator& operator+=(int offset) {
        idx_ += offset;
        return *this;
    }

    StackVectorConstIterator& operator-=(int offset) {
        idx_ -= offset;
        return *this;
    }

    StackVectorConstIterator& operator++() {
        (*this) += 1;
        return *this;
    }

    StackVectorConstIterator operator++(int) {
        StackVectorConstIterator copy(*this);
        operator++();
        return copy;
    }

    StackVectorConstIterator& operator--() {
        (*this) -= 1;
        return *this;
    }

    StackVectorConstIterator& operator--(int) {
        StackVectorConstIterator copy(*this);
        operator--();
        return copy;
    }

    StackVectorConstIterator operator+(int offset) const {
        StackVectorConstIterator copy(*this);
        copy += offset;
        return copy;
    }
    StackVectorConstIterator operator-(int offset) const {
        StackVectorConstIterator copy(*this);
        copy -= offset;
        return copy;
    }

    int operator-(const StackVectorConstIterator& rhs) const {
        MY_ASSERT(&parent_ == &rhs.parent_);
        return idx_ - rhs.idx_;
    }

    const T& operator[](int offset) { return *(*this + offset); }

    auto operator<=>(const StackVectorConstIterator& rhs) const {
        MY_ASSERT(&parent_ == &rhs.parent_);
        return idx_ <=> rhs.idx_;
    }

    bool operator==(const StackVectorConstIterator& rhs) const {
        MY_ASSERT(&parent_ == &rhs.parent_);
        return idx_ == rhs.idx_;
    }

  private:
    const StackVector<T>& parent_;
    int idx_;
};

template <typename T>
StackVectorConstIterator<T> operator+(int offset, const StackVectorConstIterator<T>& it) {
    return (it + offset);
}

template <typename T>
StackVectorConstIterator<T> operator-(int offset, const StackVectorConstIterator<T>& it) {
    return (it - offset);
}

template <typename T>
struct std::iterator_traits<StackVectorIterator<T>> {
    using difference_type   = typename StackVectorIterator<T>::difference_type;
    using value_type        = typename StackVectorIterator<T>::value_type;
    using pointer           = typename StackVectorIterator<T>::pointer;
    using reference         = typename StackVectorIterator<T>::reference;
    using iterator_category = typename StackVectorIterator<T>::iterator_category;
    using iterator_concept  = typename StackVectorIterator<T>::iterator_concept;
};

template <typename T>
struct std::iterator_traits<StackVectorConstIterator<T>> {
    using difference_type   = typename StackVectorConstIterator<T>::difference_type;
    using value_type        = typename StackVectorConstIterator<T>::value_type;
    using pointer           = typename StackVectorConstIterator<T>::pointer;
    using reference         = typename StackVectorConstIterator<T>::reference;
    using iterator_category = typename StackVectorConstIterator<T>::iterator_category;
    using iterator_concept  = typename StackVectorConstIterator<T>::iterator_concept;
};

template <typename T>
class StackVector {
  private:
    using ContainerT = typename StackOfVectors<T>::ContainerT;

  public:
    using value_type        = T;
    using iterator          = StackVectorIterator<T>;
    using const_iterator    = StackVectorConstIterator<T>;
    using iterator_category = std::random_access_iterator_tag;
    using iterator_concept  = std::contiguous_iterator_tag;

    StackVector(const StackVector&) = delete;
    StackVector(StackVector&& other) noexcept
        : parent_(other.parent_), startIdx_(other.startIdx_), endIdx_(other.endIdx_) {
        other.endIdx_ = other.startIdx_;
#ifndef NDEBUG
        isLocked_       = other.isLocked_;
        other.isLocked_ = true;
#endif
    }

    ~StackVector() {
        // Note: when moved-from, size() == 0
        if (size() > 0) {
            parent_.items_.resize(startIdx_);
        }
    }

    void push_back(const T& item) {
#ifndef NDEBUG
        MY_ASSERT(!isLocked_);
#endif
        parent_.items_.push_back(item);
        ++endIdx_;
    }
    void push_back(T&& item) {
#ifndef NDEBUG
        MY_ASSERT(!isLocked_);
#endif
        parent_.items_.push_back(std::move(item));
        ++endIdx_;
    }

    template <typename... Args>
    void emplace_back(Args&&... args) {
#ifndef NDEBUG
        MY_ASSERT(!isLocked_);
#endif
        parent_.items_.emplace_back(std::forward<Args>(args)...);
        ++endIdx_;
    }

    void pop_back() {
#ifndef NDEBUG
        MY_ASSERT(!isLocked_);
#endif
        MY_ASSERT(endIdx_ > startIdx_);
        parent_.items_.pop_back();
        --endIdx_;
    }

    void hide_back() {
        MY_ASSERT(endIdx_ > startIdx_);
        --endIdx_;
    }

    void clear() {
#ifndef NDEBUG
        MY_ASSERT(!isLocked_);
#endif
        parent_.items_.resize(startIdx_);
        endIdx_ = startIdx_;
    }

    void lock() {
#ifndef NDEBUG
        MY_ASSERT(!isLocked_);
        isLocked_         = true;
        parent_.isLocked_ = false;
#endif
    }

    void reserve(size_t size) {
#ifndef NDEBUG
        MY_ASSERT(!isLocked_);
#endif
        parent_.reserve(size + startIdx_);
    }

    int size() const { return endIdx_ - startIdx_; }
    bool empty() const { return size() == 0; }

    iterator begin() { return iterator(*this, 0); }
    const_iterator begin() const { return const_iterator(*this, 0); }
    const_iterator cbegin() const { return begin(); }

    iterator end() { return iterator(*this, size()); }
    const_iterator end() const { return const_iterator(*this, size()); }
    const_iterator cend() const { return end(); }

    T& operator[](int idx) {
        MY_ASSERT(idx < size());
        return parent_.items_[startIdx_ + idx];
    }

    const T& operator[](int idx) const {
        MY_ASSERT(idx < size());
        return parent_.items_[startIdx_ + idx];
    }

    T& front() {
        MY_ASSERT(size() > 0);
        return (*this)[0];
    }
    const T& front() const {
        MY_ASSERT(size() > 0);
        return (*this)[0];
    }

    T& back() {
        MY_ASSERT(size() > 0);
        return (*this)[size() - 1];
    }
    const T& back() const {
        MY_ASSERT(size() > 0);
        return (*this)[size() - 1];
    }

    T* data() { return parent_.items_.data() + startIdx_; }
    const T* data() const { return parent_.items_.data() + startIdx_; }

  private:
    friend class StackOfVectors<T>;

    StackVector(StackOfVectors<T>& parent)
        : parent_(parent),
          startIdx_((int)parent.size()),
          endIdx_((int)parent.size())
#ifndef NDEBUG
          ,
          isLocked_(false)
#endif
    {
    }

    StackOfVectors<T>& parent_;

    int startIdx_;
    int endIdx_;

#ifndef NDEBUG
    bool isLocked_;
#endif
};
