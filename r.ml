include R_Env

module Standard : Environment = struct

  include Standard

end

include Initialisation
include Sexptype
include Sexprec
include Data
include Allocation
include Read_internal
include Write_internal
include Symbols
include Conversion
include Internal
include Reduction
include Parser
include RevEng
