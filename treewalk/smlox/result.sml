structure Result :> RESULT = struct
  datatype ('a, 'b) t = Success of 'a | Failure of 'b
end
