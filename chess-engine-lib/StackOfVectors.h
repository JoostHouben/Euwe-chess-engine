#pragma once

#include <utility>
#include <vector>

#include <cassert>

template <typename T>
class StackVector;

template <typename T>
class StackOfVectors {
  public:
    StackOfVectors() : isLocked_(false) {}

    // Can't be copied or moved because that would invalidate all the StackVector objects.
    StackOfVectors(const StackOfVectors&) = delete;
    StackOfVectors(StackOfVectors&&) = delete;

    void reserve(size_t size) { items_.reserve(size); }

    StackVector<T> makeStackVector() {
        assert(!isLocked_);
        isLocked_ = true;
        return StackVector<T>(*this);
    }

    size_t size() const { return items_.size(); }

  private:
    friend class StackVector<T>;

    using ContainerT = std::vector<T>;

    ContainerT items_;
    bool isLocked_;
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
            assert(&parent_ == &rhs.parent_);
            return idx_ <=> rhs.idx_;
        }

        bool operator==(const Iterator& rhs) const {
            assert(&parent_ == &rhs.parent_);
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
            assert(&parent_ == &rhs.parent_);
            return idx_ <=> rhs.idx_;
        }

        bool operator==(const ConstIterator& rhs) const {
            assert(&parent_ == &rhs.parent_);
            return idx_ == rhs.idx_;
        }

      private:
        const StackVector& parent_;
        int idx_;
    };

    using value_type = T;
    using iterator = Iterator;
    using const_iterator = ConstIterator;
    //using reverse_iterator = typename ContainerT::reverse_iterator;
    //using const_reverse_iterator = typename ContainerT::const_reverse_iterator;

    void push_back(const T& item) {
        assert(!isLocked_);
        parent_.items_.push_back(item);
        ++endIdx_;
    }
    void push_back(T&& item) {
        assert(!isLocked_);
        parent_.items_.push_back(std::move(item));
        ++endIdx_;
    }

    template <typename... Args>
    void emplace_back(Args&&... args) {
        assert(!isLocked_);
        parent_.items_.emplace_back(std::forward<Args>(args)...);
        ++endIdx_;
    }

    void pop_back() {
        assert(!isLocked_);
        assert(endIdx_ > startIdx_);
        parent_.items_.pop_back();
        --endIdx_;
    }

    void lock() {
        assert(!isLocked_);
        isLocked_ = true;
        parent_.isLocked_ = false;
    }

    void reserve(size_t size) {
        assert(!isLocked_);
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

    //reverse_iterator rbegin() {
    //    return parent_.items_.rbegin() + (parent_.items_.size() - endIdx_);
    //}
    //const_reverse_iterator rbegin() const {
    //    return parent_.items_.rbegin() + (parent_.items_.size() - endIdx_);
    //}
    //const_reverse_iterator crbegin() const {
    //    return parent_.items_.crbegin() + (parent_.items_.size() - endIdx_);
    //}

    //reverse_iterator rend() { return parent_.items_.rend() - startIdx_; }
    //const_reverse_iterator rend() const { return parent_.items_.rend() - startIdx_; }
    //const_reverse_iterator crend() const { return parent_.items_.crend() - startIdx_; }

    T& operator[](int idx) {
        assert(idx < size());
        return parent_.items_[startIdx_ + idx];
    }

    const T& operator[](int idx) const {
        assert(idx < size());
        return parent_.items_[startIdx_ + idx];
    }

    T& front() {
        assert(size() > 0);
        return (*this)[0];
    }
    const T& front() const {
        assert(size() > 0);
        return (*this)[0];
    }

    T& back() {
        assert(size() > 0);
        return (*this)[size() - 1];
    }
    const T& back() const {
        assert(size() > 0);
        return (*this)[size() - 1];
    }

    T* data() { return parent_.items_.data() + startIdx_; }
    const T* data() const { return parent_.items_.data() + startIdx_; }

  private:
    friend class StackOfVectors<T>;

    StackVector(StackOfVectors<T>& parent)
        : parent_(parent), startIdx_(parent.size()), endIdx_(parent.size()), isLocked_(false) {}

    StackOfVectors<T>& parent_;

    int startIdx_;
    int endIdx_;

    bool isLocked_;
};
