// SPDX-License-Identifier: GPL-3.0-or-later
#pragma once

namespace evrostos::backends {

// holds requires a proof over all admitted executions; violated requires a
// property counterexample. Failures and incomplete checks provide no truth bit.
enum class CheckOutcome { holds, violated, inconclusive, execution_error };

} // namespace evrostos::backends
