#include "ClangDiagnosticIgnore.h"

#pragma once

#include "FrontEndOption.h"
#include "IEngine.h"
#include "IFrontEnd.h"
#include "SearchInfo.h"
#include "SearchStatistics.h"

#include <iostream>
#include <memory>
#include <string>
#include <string_view>

class UciFrontEnd final : public IFrontEnd {
  public:
    UciFrontEnd(
            IEngine& engine,
            std::istream& in    = std::cin,
            std::ostream& out   = std::cout,
            std::ostream& debug = std::cerr);
    ~UciFrontEnd();

    void run() override;

    void reportFullSearch(
            const SearchInfo& searchInfo, const SearchStatistics& searchStatistics) const override;

    void reportPartialSearch(
            const SearchInfo& searchInfo, const SearchStatistics& searchStatistics) const override;

    void reportSearchStatistics(const SearchStatistics& searchStatistics) const override;

    void reportAspirationWindowReSearch(
            EvalT previousLowerBound,
            EvalT previousUpperBound,
            EvalT searchEval,
            EvalT newLowerBound,
            EvalT newUpperBound) const override;

    void reportDiscardedPv(std::string_view reason) const override;

    void reportDebugString(std::string_view message) const override;

    void addOption(FrontEndOption option) override;

  private:
    class Impl;

    std::unique_ptr<Impl> impl_;
};
