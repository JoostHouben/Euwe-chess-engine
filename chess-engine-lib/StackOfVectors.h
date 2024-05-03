#pragma once

#include "MyAssert.h"

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
    StackOfVectors(StackOfVectors&&) = delete;

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
class StackVector {
  private:
    using ContainerT = typename StackOfVectors<T>::ContainerT;

  public:
    class Iterator {
      public:
        Iterator(StackVector& parent, int idx) : parent_(parent), idx_(idx) {}

        T operator*() const { return parent_[idx_]; }

        T* operator->() { return &parent_[idx_]; }
        const T* operator->() const { return &parent_[idx_]; }

        Iterator& operator++() {
            ++idx_;
            return *this;
        }

        Iterator& operator--() {
            --idx_;
            return *this;
        }

        Iterator operator+(int offset) const { return Iterator(parent_, idx_ + offset); }
        Iterator operator-(int offset) const { return Iterator(parent_, idx_ - offset); }

        auto operator<=>(const Iterator& rhs) const {
            MY_ASSERT(&parent_ == &rhs.parent_);
            return idx_ <=> rhs.idx_;
        }

        bool operator==(const Iterator& rhs) const {
            MY_ASSERT(&parent_ == &rhs.parent_);
            return idx_ == rhs.idx_;
        }

      private:
        StackVector& parent_;
        int idx_;
    };

    class ConstIterator {
      public:
        ConstIterator(const StackVector& parent, int idx) : parent_(parent), idx_(idx) {}

        T operator*() const { return parent_[idx_]; }

        const T* operator->() const { return &parent_[idx_]; }

        ConstIterator& operator++() {
            ++idx_;
            return *this;
        }

        ConstIterator& operator--() {
            --idx_;
            return *this;
        }

        ConstIterator operator+(int offset) const { return ConstIterator(parent_, idx_ + offset); }
        ConstIterator operator-(int offset) const { return ConstIterator(parent_, idx_ - offset); }

        auto operator<=>(const ConstIterator& rhs) const {
            MY_ASSERT(&parent_ == &rhs.parent_);
            return idx_ <=> rhs.idx_;
        }

        bool operator==(const ConstIterator& rhs) const {
            MY_ASSERT(&parent_ == &rhs.parent_);
            return idx_ == rhs.idx_;
        }

      private:
        const StackVector& parent_;
        int idx_;
    };

    using value_type = T;
    using iterator = Iterator;
    using const_iterator = ConstIterator;

    StackVector(const StackVector&) = delete;
    StackVector(StackVector&& other)
        : parent_(other.parent_), startIdx_(other.startIdx_), endIdx_(other.endIdx_) {
        other.endIdx_ = other.startIdx_;
#ifndef NDEBUG
        isLocked_ = other.isLocked_;
        other.isLocked_ = true;
#endif
    }

    ~StackVector() {
        // Note: when moved-from, size() == 0
        if (size() > 0) {
            MY_ASSERT((int)parent_.size() == endIdx_);
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

    void lock() {
#ifndef NDEBUG
        MY_ASSERT(!isLocked_);
        isLocked_ = true;
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

    iterator begin() { return Iterator(*this, 0); }
    const_iterator begin() const { return ConstIterator(*this, 0); }
    const_iterator cbegin() const { return begin(); }

    iterator end() { return Iterator(*this, size()); }
    const_iterator end() const { return ConstIterator(*this, size()); }
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
