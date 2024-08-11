#include "ClangDiagnosticIgnore.h"

#pragma once

#include <functional>
#include <optional>
#include <sstream>
#include <string>
#include <vector>

class FrontEndOption {
  public:
    using OnSet = std::function<void(const std::string&)>;

    enum class Type {
        Action,
        Boolean,
        String,
        Integer,
        Alternative,
    };

    static FrontEndOption createAction(std::function<void()> onSet) {
        FrontEndOption option;
        option.type_  = Type::Action;
        option.onSet_ = [onSet = std::move(onSet)](const std::string&) {
            onSet();
        };
        return option;
    }

    static FrontEndOption createBoolean(const bool defaultValue, std::function<void(bool)> onSet) {
        std::ostringstream sstream;
        sstream << std::boolalpha << defaultValue;

        FrontEndOption option;
        option.type_         = Type::Boolean;
        option.defaultValue_ = sstream.str();
        option.onSet_        = [onSet = std::move(onSet)](const std::string& valueString) {
            std::istringstream sstream(valueString);
            bool value;
            sstream >> std::boolalpha >> value;

            if (sstream.good()) {
                onSet(value);
            }
        };
        return option;
    }

    static FrontEndOption createString(std::string defaultValue, OnSet onSet) {
        FrontEndOption option;
        option.type_         = Type::String;
        option.defaultValue_ = std::move(defaultValue);
        option.onSet_        = std::move(onSet);
        return option;
    }

    static FrontEndOption createInteger(
            const int defaultValue,
            const int minValue,
            const int maxValue,
            std::function<void(int)> onSet) {
        FrontEndOption option;
        option.type_         = Type::Integer;
        option.defaultValue_ = std::to_string(defaultValue);
        option.minValue_     = minValue;
        option.maxValue_     = maxValue;
        option.onSet_        = [onSet = std::move(onSet)](const std::string& valueString) {
            onSet(std::stoi(valueString));
        };
        return option;
    }

    static FrontEndOption createAlternative(
            std::vector<std::string> validValues, std::string defaultValue, OnSet onSet) {
        FrontEndOption option;
        option.type_         = Type::Alternative;
        option.validValues_  = std::move(validValues);
        option.defaultValue_ = std::move(defaultValue);
        option.onSet_        = std::move(onSet);
        return option;
    }

    const std::optional<std::string>& getDefaultValue() const { return defaultValue_; }
    const std::optional<int>& getMinValue() const { return minValue_; }
    const std::optional<int>& getMaxValue() const { return maxValue_; }
    const std::optional<std::vector<std::string>>& getValidValues() const { return validValues_; }

    Type getType() const { return type_; }

    const OnSet& getOnSet() const { return onSet_; }

  private:
    FrontEndOption() = default;

    std::optional<std::string> defaultValue_;
    std::optional<int> minValue_;
    std::optional<int> maxValue_;
    std::optional<std::vector<std::string>> validValues_;

    Type type_ = {};

    OnSet onSet_;
};
