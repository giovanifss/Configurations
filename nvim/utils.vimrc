"---------------------------------------------
"             HELPER FUNCTIONS
"---------------------------------------------
" Define the value of a variable if it does not exists
function! s:defn(var, val)
  if !exists(a:var)
    exec 'let '.a:var."='".a:val."'"
  endif
endfunction
