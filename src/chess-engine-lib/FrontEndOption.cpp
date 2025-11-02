#include "FrontEndOption.h"

#include "MyAssert.h"
#include "RangePatches.h"

#include <charconv>
#include <expected>
#include <format>
#include <ranges>
#include <sstream>
#include <stdexcept>
#include <system_error>

namespace {

std::expected<bool, std::string> stringViewToBool(std::string_view valueString) {
    std::istringstream sstream{std::string(valueString)};
    bool value{};
    sstream >> std::boolalpha >> value;

    if (!sstream) {
        return std::unexpected(std::format("Invalid boolean value: '{}'", valueString));
    }

    return value;
}

std::expected<int, std::string> stringViewToInt(std::string_view valueString) {
    int value{};
    const auto result =
            std::from_chars(valueString.data(), valueString.data() + valueString.size(), value);

    if (result.ec != std::errc{}) {
        switch (result.ec) {
            case std::errc::invalid_argument:
                return std::unexpected(std::format("Invalid integer value: '{}'", valueString));
            case std::errc::result_out_of_range:
                return std::unexpected(
                        std::format("Integer value out of range: '{}'", valueString));
            default: {
                const auto error_code = std::make_error_code(result.ec);
                return std::unexpected(std::format(
                        "Unknown error while parsing integer value '{}': error code {}: {}",
                        valueString,
                        error_code.value(),
                        error_code.message()));
            }
        }
    }

    return value;
}

}  // namespace

FrontEndOption FrontEndOption::createAction(std::string name, std::function<void()> onTrigger) {
    FrontEndOption option;
    option.name_  = std::move(name);
    option.type_  = Type::Action;
    option.onSet_ = [onTrigger = std::move(onTrigger)](
                            std::string_view) -> std::expected<void, std::string> {
        onTrigger();
        return {};
    };
    return option;
}

FrontEndOption FrontEndOption::createBoolean(
        std::string name, const bool defaultValue, std::function<void(bool)> onSet) {
    std::ostringstream sstream;
    sstream << std::boolalpha << defaultValue;

    FrontEndOption option;
    option.name_         = std::move(name);
    option.type_         = Type::Boolean;
    option.defaultValue_ = sstream.str();
    option.onSet_        = [onSet = std::move(onSet)](
                            std::string_view valueString) -> std::expected<void, std::string> {
        auto r = stringViewToBool(valueString);
        if (!r)
            return std::unexpected(r.error());
        onSet(*r);
        return {};
    };
    return option;
}

FrontEndOption FrontEndOption::createBoolean(std::string name, bool& value) {
    return createBoolean(std::move(name), value, [&](bool v) { value = v; });
}

FrontEndOption FrontEndOption::createString(
        std::string name, std::string defaultValue, OnSet onSet) {
    FrontEndOption option;
    option.name_         = std::move(name);
    option.type_         = Type::String;
    option.defaultValue_ = std::move(defaultValue);
    option.onSet_        = std::move(onSet);
    return option;
}

FrontEndOption FrontEndOption::createString(std::string name, std::string& value) {
    return createString(
            std::move(name), value, [&](std::string_view v) -> std::expected<void, std::string> {
                value = v;
                return {};
            });
}

FrontEndOption FrontEndOption::createInteger(
        std::string name,
        const int defaultValue,
        const int minValue,
        const int maxValue,
        std::function<void(int)> onSet) {
    FrontEndOption option;
    option.name_         = std::move(name);
    option.type_         = Type::Integer;
    option.defaultValue_ = std::to_string(defaultValue);
    option.minValue_     = minValue;
    option.maxValue_     = maxValue;
    option.onSet_        = [=, onSet = std::move(onSet)](
                            std::string_view valueString) -> std::expected<void, std::string> {
        auto r = stringViewToInt(valueString);
        if (!r)
            return std::unexpected(r.error());
        const int value = *r;

        if (value < minValue || value > maxValue) {
            return std::unexpected(std::format(
                    "Value out of range: expected [{}, {}], got {}", minValue, maxValue, value));
        }

        onSet(value);
        return {};
    };
    return option;
}

FrontEndOption FrontEndOption::createInteger(
        std::string name, int& value, const int minValue, const int maxValue) {
    return createInteger(std::move(name), value, minValue, maxValue, [&](int v) { value = v; });
}

FrontEndOption FrontEndOption::createAlternative(
        std::string name,
        std::string defaultValue,
        std::vector<std::string> validValues,
        OnSet onSet) {
    FrontEndOption option;
    option.name_         = std::move(name);
    option.type_         = Type::Alternative;
    option.validValues_  = std::move(validValues);
    option.defaultValue_ = std::move(defaultValue);

    option.onSet_ = [onSet = std::move(onSet), validValues = *option.validValues_](
                            std::string_view valueString) -> std::expected<void, std::string> {
        const auto it = std::find(validValues.begin(), validValues.end(), valueString);
        if (it == validValues.end()) {
            const std::string validValuesString = validValues | joinToString(", ");
            return std::unexpected(std::format(
                    "Invalid value '{}'. Expected one of: [{}]", valueString, validValuesString));
        }
        return onSet(valueString);
    };
    return option;
}

FrontEndOption FrontEndOption::createAlternative(
        std::string name, std::string& value, std::vector<std::string> validValues) {
    return createAlternative(
            std::move(name), value, std::move(validValues), [&](std::string_view v) {
                value = v;
                return std::expected<void, std::string>{};
            });
}

const std::string& FrontEndOption::retrieveDefaultValue() const {
    MY_ASSERT(type_ != Type::Action);
    MY_ASSERT(defaultValue_.has_value());
    return *defaultValue_;
}

int FrontEndOption::retrieveMinValue() const {
    MY_ASSERT(type_ == Type::Integer);
    MY_ASSERT(minValue_.has_value());
    return *minValue_;
}

int FrontEndOption::retrieveMaxValue() const {
    MY_ASSERT(type_ == Type::Integer);
    MY_ASSERT(maxValue_.has_value());
    return *maxValue_;
}

const std::vector<std::string>& FrontEndOption::retrieveValidValues() const {
    MY_ASSERT(type_ == Type::Alternative);
    MY_ASSERT(validValues_.has_value());
    return *validValues_;
}

std::expected<void, std::string> FrontEndOption::set(std::string_view valueString) {
    return onSet_(valueString);
}

std::expected<void, std::string> FrontEndOption::trigger() {
    if (type_ != Type::Action) {
        return std::unexpected(std::string("Cannot set value for action option"));
    }

    return onSet_("");
}
