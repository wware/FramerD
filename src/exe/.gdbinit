set env PROMISCUOUS_FDSERVLET 1
set env LD_LIBRARY_PATH ../../lib
break fd_raise_exception
break fd_raise_detailed_exception
break fd_raise_lisp_exception
define lisp
  printf "%s\n",fd_object_to_string($arg0)
end

