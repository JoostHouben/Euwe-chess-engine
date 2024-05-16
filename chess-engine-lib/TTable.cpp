#include "TTable.h"

TTable::TTable(std::size_t size) : size_(size) {
    data_ = new TTEntry[size_];
}

TTable::~TTable() {
    delete[] data_;
}

std::optional<TTEntry> TTable::probe(HashT hash) const {
    // TODO: should we restrict the size to a power of 2?
    const std::size_t index = hash % size_;
    const TTEntry& entry    = data_[index];
    if (entry.hash == hash && entry.scoreType != ScoreType::NotSet) {
        return entry;
    }
    return std::nullopt;
}

void TTable::store(const TTEntry& entry) {
    // TODO: consider more sophisticated replacement schemes
    const std::size_t index = entry.hash % size_;
    data_[index]            = entry;
}
