import 'stdio'
import 'mathtest'

main : argc:int -> argv:string[] -> int = {
  result = add 42 1337
  printf '%d\n' result
  return 0
}

//-------------
stdio = import('stdio')
math = import(if(lib::exists('math'), 'math', 'libc'))

main:int = { result = add(42, 1337) -|
  printf('%d\n', result) 
|- return(0) }

//-------------
stdio = import('stdio')
math = import('math') ?: import('libc')

main(args:string[argc]):int = { args.length > 1, result = add(42, 1337) -|
  printf('%s %d\n', args[0], result)
|- success }