#include "FrontEndOption.h"

#include "MyAssert.h"

#include <charconv>
#include <format>
#include <stdexcept>
#include <system_error>

namespace {

bool stringViewToBool(std::string_view valueString) {
    std::istringstream sstream{std::string(valueString)};
    bool value{};
    sstream >> std::boolalpha >> value;

    if (!sstream) {
        throw std::invalid_argument(std::format("Invalid boolean value: '{}'", valueString));
    }

    return value;
}

int stringViewToInt(std::string_view valueString) {
    int value{};
    const auto result =
            std::from_chars(valueString.data(), valueString.data() + valueString.size(), value);

    if (result.ec != std::errc{}) {
        switch (result.ec) {
            case std::errc::invalid_argument:
                throw std::invalid_argument(
                        std::format("Invalid integer value: '{}'", valueString));
            case std::errc::result_out_of_range:
                throw std::out_of_range(
                        std::format("Integer value out of range: '{}'", valueString));
            default: {
                const auto error_code = std::make_error_code(result.ec);
                throw std::system_error(
                        error_code,
                        std::format(
                                "Unknown error while parsing integer value '{}': error code {}: "
                                "{} ",
                                valueString,
                                error_code.value(),
                                error_code.message()));
            }
        }
    }

    return value;
}

}  // namespace

FrontEndOption FrontEndOption::createAction(std::function<void()> onSet) {
    FrontEndOption option;
    option.type_  = Type::Action;
    option.onSet_ = [onSet = std::move(onSet)](std::string_view) {
        onSet();
    };
    return option;
}

FrontEndOption FrontEndOption::createBoolean(
        const bool defaultValue, std::function<void(bool)> onSet) {
    std::ostringstream sstream;
    sstream << std::boolalpha << defaultValue;

    FrontEndOption option;
    option.type_         = Type::Boolean;
    option.defaultValue_ = sstream.str();
    option.onSet_        = [onSet = std::move(onSet)](std::string_view valueString) {
        onSet(stringViewToBool(valueString));
    };
    return option;
}

FrontEndOption FrontEndOption::createString(std::string defaultValue, OnSet onSet) {
    FrontEndOption option;
    option.type_         = Type::String;
    option.defaultValue_ = std::move(defaultValue);
    option.onSet_        = std::move(onSet);
    return option;
}

FrontEndOption FrontEndOption::createInteger(
        const int defaultValue,
        const int minValue,
        const int maxValue,
        std::function<void(int)> onSet) {
    FrontEndOption option;
    option.type_         = Type::Integer;
    option.defaultValue_ = std::to_string(defaultValue);
    option.minValue_     = minValue;
    option.maxValue_     = maxValue;
    option.onSet_        = [=, onSet = std::move(onSet)](std::string_view valueString) {
        const int value = stringViewToInt(valueString);

        if (value < minValue || value > maxValue) {
            throw std::invalid_argument(std::format(
                    "Value out of range: expected [{}, {}], got {}", minValue, maxValue, value));
        }

        onSet(value);
    };
    return option;
}

FrontEndOption FrontEndOption::createAlternative(
        std::vector<std::string> validValues, std::string defaultValue, OnSet onSet) {
    FrontEndOption option;
    option.type_         = Type::Alternative;
    option.validValues_  = std::move(validValues);
    option.defaultValue_ = std::move(defaultValue);
    option.onSet_        = std::move(onSet);
    return option;
}

void FrontEndOption::set(std::string_view valueString) {
    onSet_(valueString);
}

void FrontEndOption::trigger() {
    if (type_ != Type::Action) {
        throw std::logic_error("Cannot set value for action option");
    }

    onSet_("");
}
