#include "ClangDiagnosticIgnore.h"

#pragma once

#include "BoardHash.h"
#include "Eval.h"

#include <optional>

#include <cstdint>

enum ScoreType : std::uint8_t { NotSet, Exact, LowerBound, UpperBound };

struct TTEntry {
    HashT hash          = 0;
    std::uint8_t depth  = 0;
    ScoreType scoreType = ScoreType::NotSet;
    EvalT score         = 0;
    Move bestMove;  // TODO: could store smaller move representation
};

class TTable {
  public:
    TTable(std::size_t size);
    ~TTable();

    [[nodiscard]] std::optional<TTEntry> probe(HashT hash) const;

    void store(const TTEntry& entry);

    [[nodiscard]] int getNumInUse() const { return numInUse_; }
    [[nodiscard]] float getUtilization() const { return static_cast<float>(numInUse_) / size_; }

  private:
    TTEntry* data_;
    std::size_t size_;

    int numInUse_ = 0;
};
