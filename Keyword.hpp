#pragma once

#include <unordered_map>

namespace SauScript {

enum class Keyword {
    WHILE, IF, ELSE, TRY, CATCH, FOR, FN
};

inline std::unordered_map<std::string, Keyword> KEYWORDS{
        {"while",   Keyword::WHILE  },
        {"if",      Keyword::IF     },
        {"else",    Keyword::ELSE   },
        {"try",     Keyword::TRY    },
        {"catch",   Keyword::CATCH  },
        {"for",     Keyword::FOR    },
        {"fn",      Keyword::FN     }
};
}