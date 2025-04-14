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
            std::string name,
            std::istream& in    = std::cin,
            std::ostream& out   = std::cout,
            std::ostream& debug = std::cerr);
    ~UciFrontEnd() override;

    UciFrontEnd(const UciFrontEnd&)            = delete;
    UciFrontEnd& operator=(const UciFrontEnd&) = delete;

    UciFrontEnd(UciFrontEnd&&)            = default;
    UciFrontEnd& operator=(UciFrontEnd&&) = default;

    void run() override;

    void reportFullSearch(const SearchInfo& searchInfo) const override;

    void reportPartialSearch(const SearchInfo& searchInfo) const override;

    void reportSearchStatistics(const SearchStatistics& searchStatistics) const override;

    void reportAspirationWindowReSearch(
            int depth,
            EvalT previousLowerBound,
            EvalT previousUpperBound,
            EvalT searchEval,
            EvalT newLowerBound,
            EvalT newUpperBound,
            const SearchStatistics& searchStatistics) const override;

    void reportDiscardedPv(std::string_view reason) const override;

    void reportError(std::string_view message) const override;

    void reportString(std::string_view message) const override;

    void reportDebugString(std::string_view message) const override;

    void addOption(FrontEndOption option) override;

  private:
    class Impl;

    std::unique_ptr<Impl> impl_;
};
