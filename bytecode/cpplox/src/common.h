#pragma once

#include <cstdint>
#include <limits>

// #define DEBUG_STRESS_GC
// #define DEBUG_LOG_GC
// #define DEBUG_PRINT_CODE
// #define DEBUG_TRACE_EXECUTION
#define NAN_BOXING

inline constexpr int kFramesMax = 64;
inline constexpr int kStackMax =
    kFramesMax * (std::numeric_limits<uint8_t>::max() + 1);
