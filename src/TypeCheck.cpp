#include <iostream>
#include "TypeCheck.h"
#include "VisitTypeCheck.h"
#include "Stella/Absyn.H"

namespace Stella
{

  void typecheckProgram(Program *program)
  {
    try
    {
      program->accept(new VisitTypeCheck());
    }
    catch (type_error e)
    {
      std::cout << "Type Error!\n"
                << e.what() << "\n";
      exit(1);
    }
  }

} // namespace Stella
