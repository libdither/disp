import Checked

/-! The long interpreter runs: the interpreter on `size suc`, and the interpreter interpreting
itself. Millions of steps, so they are not in the default build: `lake build Long`. -/

theorem rln_long_programs : longPrograms.all (fun p => fastCheckWith gtable 1000000 p.2) = true := by
  native_decide
/-- None of them is skipped: the reference gives every one a value. -/
theorem rln_long_programs_have_values :
    longPrograms.all (fun p => match eval 1000000 p.2 with | .value _ => true | _ => false) = true := by
  native_decide
