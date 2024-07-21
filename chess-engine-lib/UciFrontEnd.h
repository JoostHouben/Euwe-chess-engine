#include "ClangDiagnosticIgnore.h"

#pragma once

#include "Engine.h"
#include "GameState.h"

#include <sstream>

class UciFrontEnd {
  public:
    UciFrontEnd();

    void run();

  private:
    void handleIsReady() const;
    void handlePosition(std::stringstream& lineSStream);
    void handleGo(std::stringstream& lineSStream);

    Engine engine_;
    GameState gameState_;
};
