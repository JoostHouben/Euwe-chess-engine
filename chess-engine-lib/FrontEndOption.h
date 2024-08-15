#pragma once

#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

class FrontEndOption {
  public:
    using OnSet = std::function<void(std::string_view)>;

    enum class Type {
        Action,
        Boolean,
        String,
        Integer,
        Alternative,
    };

    static FrontEndOption createAction(std::string name, std::function<void()> onTrigger);

    static FrontEndOption createBoolean(
            std::string name, bool defaultValue, std::function<void(bool)> onSet);
    static FrontEndOption createBoolean(std::string name, bool& value);

    static FrontEndOption createString(std::string name, std::string defaultValue, OnSet onSet);
    static FrontEndOption createString(std::string name, std::string& value);

    static FrontEndOption createInteger(
            std::string name,
            int defaultValue,
            int minValue,
            int maxValue,
            std::function<void(int)> onSet);
    static FrontEndOption createInteger(std::string name, int& value, int minValue, int maxValue);

    static FrontEndOption createAlternative(
            std::string name,
            std::string defaultValue,
            std::vector<std::string> validValues,
            OnSet onSet);
    static FrontEndOption createAlternative(
            std::string name, std::string& value, std::vector<std::string> validValues);

    const std::string& getName() const { return name_; }
    const std::optional<std::string>& getDefaultValue() const { return defaultValue_; }
    const std::optional<int>& getMinValue() const { return minValue_; }
    const std::optional<int>& getMaxValue() const { return maxValue_; }
    const std::optional<std::vector<std::string>>& getValidValues() const { return validValues_; }

    Type getType() const { return type_; }

    void set(std::string_view value);

    // Only valid for Action type options.
    void trigger();

  private:
    FrontEndOption() = default;

    std::string name_;

    std::optional<std::string> defaultValue_;
    std::optional<int> minValue_;
    std::optional<int> maxValue_;
    std::optional<std::vector<std::string>> validValues_;

    Type type_ = {};

    OnSet onSet_;
};
