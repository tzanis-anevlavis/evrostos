// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

#include "report.hpp"

namespace evrostos::cli {

void validate_output(const std::filesystem::path& path, const std::filesystem::path& input);
std::filesystem::path publish_report(const Report& report, const std::filesystem::path& output,
                                     const std::filesystem::path& workspace, const std::filesystem::path& model,
                                     const std::filesystem::path* formula_list);

} // namespace evrostos::cli
