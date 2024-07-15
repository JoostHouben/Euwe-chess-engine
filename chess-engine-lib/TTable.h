#include "ClangDiagnosticIgnore.h"

#pragma once

#include "BoardHash.h"
#include "Eval.h"
#include "Intrinsics.h"

#include <bit>
#include <optional>

#include <cstdint>

enum ScoreType : std::uint8_t { NotSet, Exact, LowerBound, UpperBound };

template <typename PayloadT>
struct TTEntry {
    HashT hash       = 0;
    PayloadT payload = {};
};

struct SearchTTPayload {
    std::uint8_t depth  = 0;
    ScoreType scoreType = ScoreType::NotSet;
    EvalT score         = 0;
    Move bestMove;  // TODO: could store smaller move representation
};

using SearchTTEntry = TTEntry<SearchTTPayload>;

template <typename PayloadT>
class TTable {
  public:
    using EntryT = TTEntry<PayloadT>;

    TTable(std::size_t requestedSize);
    ~TTable();

    [[nodiscard]] std::optional<EntryT> probe(HashT hash) const;

    void prefetch(HashT hash) const;

    template <typename FuncT>
    void store(const EntryT& entry, FuncT&& isMoreValuable);

    [[nodiscard]] int getNumInUse() const { return numInUse_; }
    [[nodiscard]] float getUtilization() const { return static_cast<float>(numInUse_) / size_; }

  private:
    [[nodiscard]] std::size_t computeIndex(HashT hash) const;

    EntryT* data_;
    std::size_t size_;
    std::size_t mask_;

    int numInUse_ = 0;
};

using SearchTTable = TTable<SearchTTPayload>;

template <typename PayloadT>
TTable<PayloadT>::TTable(const std::size_t requestedSize) : size_(std::bit_floor(requestedSize)) {
    data_ = new EntryT[size_];

    // size_ is a power of 2, so size_ - 1 is all 1s in binary.
    // size_ - 2 is all 1s except the least significant bit.
    // So by setting the mask to size_ - 2, `hash & mask_` will give us an even index in the range
    // [0, size_).
    mask_ = size_ - 2;
}

template <typename PayloadT>
TTable<PayloadT>::~TTable() {
    delete[] data_;
}

template <typename PayloadT>
std::optional<TTEntry<PayloadT>> TTable<PayloadT>::probe(const HashT hash) const {
    const std::size_t index         = computeIndex(hash);
    const std::size_t valuableIndex = index;
    const std::size_t recentIndex   = index | 1;

    const EntryT& recentEntry = data_[recentIndex];
    if (recentEntry.hash == hash) {
        return recentEntry;
    }

    const EntryT& valuableEntry = data_[valuableIndex];
    if (valuableEntry.hash == hash) {
        return valuableEntry;
    }

    return std::nullopt;
}

template <typename PayloadT>
template <typename FuncT>
void TTable<PayloadT>::store(const TTEntry<PayloadT>& entryToStore, FuncT&& isMoreValuable) {
    const std::size_t index         = computeIndex(entryToStore.hash);
    const std::size_t valuableIndex = index;
    const std::size_t recentIndex   = index | 1;

    EntryT& valuableEntry = data_[valuableIndex];
    EntryT& recentEntry   = data_[recentIndex];

    // First check if valuableEntry is unused.
    if (valuableEntry.hash == 0) {
        valuableEntry = entryToStore;
        ++numInUse_;
        // We stored the entry, no need to continue.
        return;
    }
    if (valuableEntry.hash == entryToStore.hash) {
        // Same position, update if more valuable
        if (isMoreValuable(entryToStore.payload, valuableEntry.payload)) {
            valuableEntry = entryToStore;
        }
        // We either stored the entry or found that we already have more valuable information for
        // this position. Either way, no need to continue.
        return;
    }
    // Otherwise, we have an index collision.
    if (isMoreValuable(entryToStore.payload, valuableEntry.payload)) {
        // New position is more valuable. Move the old valuable entry to the recent entry slot and
        // store the new entry in the valuable entry slot.
        if (recentEntry.hash == 0) {
            ++numInUse_;
        }
        recentEntry   = valuableEntry;
        valuableEntry = entryToStore;
    } else {
        // New position is less valuable. Store the new entry in the recent entry slot.
        if (recentEntry.hash == 0) {
            ++numInUse_;
        }
        recentEntry = entryToStore;
    }
}

template <typename PayloadT>
FORCE_INLINE void TTable<PayloadT>::prefetch(const HashT hash) const {
    const std::size_t index = computeIndex(hash);
    ::prefetch(&data_[index]);
}

template <typename PayloadT>
FORCE_INLINE std::size_t TTable<PayloadT>::computeIndex(const HashT hash) const {
    return hash & mask_;
}
