#include "ClangDiagnosticIgnore.h"

#pragma once

#include "BoardHash.h"
#include "Eval.h"

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

    TTable(std::size_t size);
    ~TTable();

    [[nodiscard]] std::optional<EntryT> probe(HashT hash) const;

    void store(const EntryT& entry);

    [[nodiscard]] int getNumInUse() const { return numInUse_; }
    [[nodiscard]] float getUtilization() const { return static_cast<float>(numInUse_) / size_; }

  private:
    EntryT* data_;
    std::size_t size_;

    int numInUse_ = 0;
};

using SearchTTable = TTable<SearchTTPayload>;

template <typename PayloadT>
TTable<PayloadT>::TTable(std::size_t size) : size_(size) {
    data_ = new EntryT[size_];
}

template <typename PayloadT>
TTable<PayloadT>::~TTable() {
    delete[] data_;
}

template <typename PayloadT>
std::optional<TTEntry<PayloadT>> TTable<PayloadT>::probe(HashT hash) const {
    // TODO: should we restrict the size to a power of 2?
    const std::size_t index = hash % size_;
    const EntryT& entry     = data_[index];
    if (entry.hash == hash) {
        return entry;
    }
    return std::nullopt;
}

template <typename PayloadT>
void TTable<PayloadT>::store(const TTEntry<PayloadT>& entry) {
    // TODO: consider more sophisticated replacement schemes
    const std::size_t index = entry.hash % size_;
    if (data_[index].hash == 0) {
        ++numInUse_;
    }
    data_[index] = entry;
}
