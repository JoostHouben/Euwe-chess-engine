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
};

class TTable {
  public:
    TTable(std::size_t size);
    ~TTable();

    [[nodiscard]] std::optional<TTEntry> probe(HashT hash) const;

    void store(const TTEntry& entry);

  private:
    TTEntry* data_;
    std::size_t size_;
};
