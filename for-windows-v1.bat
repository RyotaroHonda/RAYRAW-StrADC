set vivado_path=%1
%vivado_path% -mode tcl -nolog -nojournal -source .\project-v1.tcl -tclargs --origin_dir .
