module type Interpreter = sig end

module Interpreter (Env : Environment) : Interpreter

