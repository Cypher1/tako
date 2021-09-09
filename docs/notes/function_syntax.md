# Tako

## C++

### Function Form

```
float square(float x) { return x * x; }

float (*) float;
```

### Lambda Form

```
auto square = [](float x) { return x * x; }

std::function<float(float)>
```


## Rust

### Function Form

```
fn square (x: float) -> float { x * x }
```

### Lambda Form

```
let square = |x: float| -> float x * x
```


### Type (same for both!)
```
fn (x: float) -> float
```

## Haskell

### Function Form

```
square x = x * x
```

### Lambda Form
```
(\x -> x * x)
```

### Type (same for both!)
```
Float -> Float
```

## Tako

### Function Form

```
square(x) = x * x
```

### Lambda Form

```
(_(x) = x * x)
```
#### Passed to kwarg
```
foo(kwarg(x) = x * x)
```

### Type (same for both!)
```
// Type for kwarg
foo(kwarg(x:Float):Float)

// Type for function
square(x:Float):Float
```
