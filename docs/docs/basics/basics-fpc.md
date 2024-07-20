# Basics of Object Pascal using FPC

## Reserved Words

Reserved words are special words in the Pascal language that you cannot change or redefine.

!!! Important

    The Free Pascal Compiler lets you use uppercase or lowercase letters for these special words; they will work the same way.

The following keywords exist in Turbo Pascal mode.

```text
absolute    file            object      string  
and         for             of          then  
array       function        operator    to  
asm         goto            or          type  
begin       if              packed      unit  
case        implementation  procedure   until  
const       in              program     uses  
constructor inherited       record      var  
destructor  inline          reintroduce while   
div         interface       repeat      with  
do          label           self        xor
downto      mod             set         
else        nil             shl         
end         not             shr         
```

The special words in Object Pascal (used in Delphi or Objfpc mode) are the same as in Turbo Pascal, but with these extra keywords:

```text
as              finalization    library     raise   
class           finally         on          resourcestring     
dispinterface   initialization  out         threadvar  
except          inline          packed      try 
exports         is              property    
```

## Comments

Comments are pieces of the source code which are completely discarded by the compiler.

Use `{` and `}` or `//` for making comments in Object Pascal.

You might use conmments as follows.

```pascal linenums="1"
{  This is a single line comment.}  

// This is a single line comment. All is ignored till the end of the line.

{
  This is a multi-line comment in Object Pascal.
  You can write as much text as you want here,
  and it will all be ignored by the compiler.
  Multi-line comments are useful for explaining
  more complex parts of your code or adding
  detailed documentation.
}
```

## Main Block

An Object Pascal program must have a main (program) block, marked by `begin ... end.`. Note the `.` after the `end`.

```pascal linenums="1"
{ This is the main block }
begin
  { ... your code ... }
  
  // Example, print a message.
  WriteLn('Hello World!');
end.
```

## Variable Declaration

Declare variables within the `var` section of your program.

**Syntax**

```pascal linenums="1"
var
  variable_name:variable_type; 
```

**Example**

To declare a string and an integer, you might declare them like this.

```pascal linenums="1"
var
  firstName:string;
  age:integer;
```

Check out the official documentation [Types](https://www.freepascal.org/docs-html/ref/refch3.html#refse12.html) for a full list and explanation of the types you can use.


## Console Input and Output

### Read Input

In Free Pascal, `Read` and `ReadLn` are used for input, but they work a bit differently.

Use `Read` when you want to read input without moving to the next line.

```pascal linenums="1"
{$mode objfpc}{$H+}{$J-}
var
  num1, num2: integer;
begin
  { Using Read -- The next value read will be num 2. }
  Write('Enter two numbers: ');
  Read(num1);
  Read(num2);
  WriteLn('You entered: ', num1, ' and ', num2);
end.
```

Use `ReadLn` when you want to read input and move to the next line afterward.

```pascal linenums="1"
{$mode objfpc}{$H+}{$J-}
var
  num1, num2: integer;
begin
  { Using ReadLn -- The next value after a new line will be num 2. }
  Write('Enter two numbers on separate lines: ');
  ReadLn(num1);
  ReadLn(num2);
  WriteLn('You entered: ', num1, ' and ', num2);
end.
```

### Display Output


Similarly, `Write` and `WriteLn` are used to output text, but they behave differently.

Use `Write` to output text without moving to the next line. It keeps the cursor on the same line, so subsequent output will continue from where the previous output ended.


```pascal linenums="1"
{$mode objfpc}{$H+}{$J-}

begin
  { Using Write -- World! appears after Hello. }
  Write('Hello '); // No new line at the end of the string.
  Write('World!'); // No new line at the end of the string.

  // A spacer
  WriteLn;
end.
```

Use `WriteLn` to output text and then moves the cursor to the next line. It adds a newline character after the text, so any subsequent output starts on a new line.


```pascal linenums="1"
{$mode objfpc}{$H+}{$J-}

begin
  { Using WriteLn -- World! appears underneath Hello. }
  WriteLn('Hello '); // There is a new line at the end of the string.
  WriteLn('World!'); // There is a new line at the end of the string.
end.
```

## Variable Assignment

!!! Important
    After declaring a variable, make sure to initialise it before use, else you might end up with a garbage value.

!!! Important
    `:=` -- Assignment.

    `=`  -- Comparison, equality.

Use `:=` for assigning a variable to a value.

**Syntax**

```pascal
a_variable := a_value
```

**Example**

```pascal linenums="1" hl_lines="13 16 19 22"
program VariableAssignment;

  {$mode objfpc}{$H+}{$J-}

var
  studentName: string;
  studentAge: byte; // The range is 0..255, enough for storing a person's age.
  studentID: string;
  studentGroup: char;

begin
  { Assign a string to studentName variable}
  studentName := 'Jean Valjean';

  { Assign a number to studentAge variable}
  studentAge := 24;

  { Assign a string to studentID variable}
  studentID := 'j-24601';

  { Assign a char to studentGroup variable}
  studentGroup := 'A';

  { Display values to console }
  WriteLn('Student Name : ', studentName);
  WriteLn('Student Age  : ', studentAge);
  WriteLn('Student ID   : ', studentID);
  WriteLn('Student Group: ', studentGroup);

  { Pause console }
  ReadLn;
end.
```

Alternatively, you can use `Read` or `ReadLn` to assign values to variables.

**Example**

```pascal linenums="1" hl_lines="10"
program ReadExample2;

  {$mode objfpc}{$H+}{$J-}

var
  num1, num2: integer;
begin
  WriteLn('Enter two numbers separated by a space: ');
  { Assign first number to num1 and second one to num2. }
  ReadLn(num1, num2);
  WriteLn('You entered: ', num1, ' and ', num2);

  // Pause console
  ReadLn;
end.
```

## Procedures and Functions

### Procedure

A `procedure` is a block of code that performs a specific task but does not return a value. Use it when you want to execute a series of statements without returning a result.

**Syntax**

```pascal linenums="1"
procedure name_of_procedure;
begin
  { your code goes here }
end;
```

**Example**

```pascal linenums="1"
procedure SayHello;
begin
  WriteLn('Hello, world!');
end;
```

You call a procedure like this:

```pascal linenums="1"
begin
  SayHello;
end.
```

### Function

A `function` is similar to a procedure but it returns a value. Use this when you need to perform a task and get a result back.

**Syntax**

```pascal linenums="1"
function Add(params: type): type;
begin
  { your code goes here }
  Result := {value to return} 
end;
```

**Example**

```pascal linenums="1"
function function_name(a, b: integer): integer;
begin
  Result := a + b;
end;
```

And call the function like this:

```pascal linenums="1"
var
  result: integer;
begin
  result := Add(3, 5);
  WriteLn('The sum is: ', result); // Will print 8
end.
```

## Loops - the `for` Statement

**Syntax**

```pascal linenums="1"
for counter := startValue to endValue do
begin
  // statements
end;
```

**Example of `for..to..do` loop**

```pascal linenums="1"
  {$mode objfpc}{$H+}{$J-}

var
  i: integer;

begin
  for i := 1 to 10 do
  begin
    WriteLn('i = ', i);
  end;
end.
```

**Example of `for..downto..do` Loop**

```pascal linenums="1"
  {$mode objfpc}{$H+}{$J-}

var
  i: integer;

begin
  for i := 10 downto 1 do
  begin
    WriteLn('i = ', i);
  end;
end.
```


## Conditional Loops - `while` Statement

The `while` loop is used to repeat a block of statements as long as a condition is true.

**Syntax**

```pascal linenums="1"
while condition do
begin
  // statements
end;
```

**Example**

```pascal linenums="1"
  {$mode objfpc}{$H+}{$J-}

var
  i: integer;

begin
  i := 1;
  while i <= 10 do
  begin
    WriteLn('i = ', i);
    i := i + 1;
  end;
end.
```

## Conditional Loops - `repeat` Statement

The `repeat..until` loop is similar to the `while` loop, but the condition is checked after the loop has executed, ensuring the statements are executed at least once.

**Syntax**

```pascal linenums="1"
repeat
  // statements
until condition;
```

**Example**

```pascal linenums="1"
  {$mode objfpc}{$H+}{$J-}

var
  i: integer;

begin
  i := 1;
  repeat
    WriteLn('i = ', i);
    i := i + 1;
  until i > 10;
end.
```

## Choices - the `case` Statement

**Syntax**

```pascal linenums="1"
case expression of
  value1: 
    begin
      // statements for value1
    end;
  value2: 
    begin
      // statements for value2
    end;
  value3, value4: 
    begin
      // statements for value3 and value4
    end;
  else
    begin
      // statements if none of the above values match
    end;
end;
```

**Example**

```pascal linenums="1"
  {$mode objfpc}{$H+}{$J-}

var
  grade: Char;

begin
  Write('Enter a grade (A, B, C, D or F): ');
  ReadLn(grade);

  case grade of
    'A': 
      begin
        WriteLn('Excellent!');
      end;
    'B': 
      begin
        WriteLn('Good job!');
      end;
    'C': 
      begin
        WriteLn('Well done.');
      end;
    'D': 
      begin
        WriteLn('You passed.');
      end;
    'F': 
      begin
        WriteLn('You failed!');
      end;
    else
      begin
        WriteLn('Invalid grade');
      end;
  end;
end.
```

## Decisions - the `if` Statement

**Syntax**

```pascal linenums="1"
if condition then
begin
  // statements to execute if condition is true
end;
```

Optionally, you can include an else part to execute a different block of code if the condition is false.

```pascal linenums="1"
if condition then
begin
  // statements to execute if condition is true
end
else
begin
  // statements to execute if condition is false
end;
```

For multiple conditions, you can use else if:

```pascal linenums="1"
if condition1 then
begin
  // statements to execute if condition1 is true
end
else if condition2 then
begin
  // statements to execute if condition2 is true
end
else
begin
  // statements to execute if none of the conditions are true
end;
```

**Example of `if..then` Statement**

```pascal linenums="1"
  {$mode objfpc}{$H+}{$J-}

var
  age: integer;
begin
  Write('Enter your age: ');
  ReadLn(age);

  if age >= 18 then
  begin
    WriteLn('You are an adult.');
  end;
end.
```

**Example of `if..then..else` Statement**

```pascal linenums="1"
  {$mode objfpc}{$H+}{$J-}

var
  age: integer;
begin
  Write('Enter your age: ');
  ReadLn(age);

  if age >= 18 then
  begin
    WriteLn('You are an adult.');
  end
  else
  begin
    WriteLn('You are a minor.');
  end;
end.
```

**Example of many conditions with `else if` Statement**

```pascal linenums="1"
  {$mode objfpc}{$H+}{$J-}

var
  grade: char;
begin
  Write('Enter your grade (A, B, C, D, F): ');
  ReadLn(grade);

  if grade = 'A' then
  begin
    WriteLn('Excellent!');
  end
  else if grade = 'B' then
  begin
    WriteLn('Good job!');
  end
  else if grade = 'C' then
  begin
    WriteLn('Well done.');
  end
  else if grade = 'D' then
  begin
    WriteLn('You passed.');
  end
  else if grade = 'F' then
  begin
    WriteLn('Failed subject!');
  end
  else
  begin
    WriteLn('Invalid grade.');
  end;
end.
```

## String Operations

### Length of a String

Use `Length(str)` to find the length of a string.

**Example**

```pascal linenums="1" hl_lines="11"
  {$mode objfpc}{$H+}{$J-}

var
  str:string;
  len:integer;

begin
  str := 'Hello World!';
  
  // Get length of a string
  len:=Length(str);
  
  WriteLn('Length of ', str, ' is ', len); // Output: Length of Hello World! is 12
end.
```

### Finding Character at n-th Index

**Syntax**

```pascal linenums="1"
a_string_var[index];
```

**Example**

```pascal linenums="1" hl_lines="12"
  {$mode objfpc}{$H+}{$J-}

var
  str: string;
  ch: char;
  index: integer;

begin
  str := 'Hello World!';
  index := 1; // Change this value to test different indices

  ch := str[index];
  WriteLn('Character at index ', index, ' is: ', ch);
end.
```

### Concat Strings

You can concat string using the `+` operator or `Concat(str1,str2)`.

**Example**

```pascal linenums="1" hl_lines="11 15"
  {$mode objfpc}{$H+}{$J-}

var
  str1, str2, result: string;

begin
  str1 := 'Hello, ';
  str2 := 'world!';
  
  // Using + operator
  result := str1 + str2;
  WriteLn(result); // Output: Hello, world!
  
  // Using Concat function
  result := Concat(str1, str2);
  WriteLn(result); // Output: Hello, world!
end.
```

### Replace Substring

1. Include `SysUtils` in the `uses` section.
2. Use `StringReplace(...)`.

**Example**

```pascal linenums="1" hl_lines="13"
  {$mode objfpc}{$H+}{$J-}

uses
  SysUtils;

var
  str, result: string;

begin
  str := 'Hello, Pascal!';
  
  // Replacing 'Pascal' with 'world'
  result := StringReplace(str, 'Pascal', 'world', [rfReplaceAll, rfIgnoreCase]);
  WriteLn(result); // Output: Hello, world!
end.
```


### Changing Case

1. Include `SysUtils` in the `uses` section.
2. Use `LowerCase(str)` or `UpperCase(str)`.

**Example**

```pascal linenums="1" hl_lines="4 13 17"
  {$mode objfpc}{$H+}{$J-}

uses
  SysUtils;

var
  str, lowerStr, upperStr: string;

begin
  str := 'Hello, Pascal!';
  
  // Convert to lowercase
  lowerStr := LowerCase(str);
  WriteLn(lowerStr); // Output: hello, pascal!
  
  // Convert to uppercase
  upperStr := UpperCase(str);
  WriteLn(upperStr); // Output: HELLO, PASCAL!
end.
```

### Searching for a Substring

You can use `Pos(substr, str)` to find a position of a substring in a string.

**Example**

```pascal linenums="1" hl_lines="12"
  {$mode objfpc}{$H+}{$J-}

var
  str, substr: string;
  index: integer;

begin
  str := 'Hello, Pascal!';
  substr := 'Pascal';
  
  // Find position of 'Pascal' in str
  index := Pos(substr, str);
  WriteLn('Position of "', substr, '" in "', str, '" is: ', index); // Output: 8
end.

```

## Just for the `Record` ...

A `record` is a data structure that allows you to group different types of data together. This feature in Free Pascal allow you to create complex data structures and manage related data efficiently.

**Syntax**

```pascal linenums="1"
type
  TRecordName = record
    field1: dataType1;
    field2: dataType2;
    field3: dataType3;
    // Add more fields as needed
  end;
```

**Example**

```pascal
  {$mode objfpc}{$H+}{$J-}

type
  TPerson = record
    name: string;
    age: integer;
    height: Real;
  end;

var
  person1, person2: TPerson;

begin
  // Assign values to the fields of Person1
  person1.Name := 'Javert';
  person1.Age := 30;
  person1.Height := 5.9;

  // Print the values of Person1
  WriteLn('Person1 Name: ', person1.Name);
  WriteLn('Person1 Age: ', person1.Age);
  WriteLn('Person1 Height: ', person1.Height:0:2);

  // Assign values to the fields of Person2
  person2.Name := 'Jean Valjean';
  person2.Age := 25;
  person2.Height := 5.7;

  // Print the values of Person2
  WriteLn('Person2 Name: ', person2.Name);
  WriteLn('Person2 Age: ', person2.Age);
  WriteLn('Person2 Height: ', person2.Height:0:2);
end.
```

## Arrays

### Static Arrays

Static arrays have a fixed size defined at compile time.

**Syntax**

```pascal linenums="1"
var
  arrayName: array[startIndex..endIndex] of elementType;
```

**Example**


```pascal linenums="1"
program StaticArrayExample;

var
  numbers: array[1..5] of integer;
  i: integer;

begin
  // Initialising the array
  numbers[1] := 10;
  numbers[2] := 20;
  numbers[3] := 30;
  numbers[4] := 40;
  numbers[5] := 50;

  // Accessing and printing array elements
  for i := 1 to 5 do
    WriteLn('numbers[', i, '] = ', numbers[i]);
end.

```

### Dynamic Arrays

Dynamic arrays can be resized at runtime using the `SetLength` procedure.

**Syntax**

```pascal linenums="1"
var
  arrayName: array of elementType;
```


**Example**

```pascal linenums="1"
program DynamicArrayExample;

var
  numbers: array of integer;
  i: integer;

begin
  // Setting the length of the array
  SetLength(numbers, 5);

  // Initialising the array
  for i := 0 to High(numbers) do
    numbers[i] := (i + 1) * 10;

  // Accessing and printing array elements
  for i := 0 to High(numbers) do
    WriteLn('numbers[', i, '] = ', numbers[i]);

  // Resizing the array
  SetLength(numbers, 10);
  for i := 5 to 9 do
    numbers[i] := (i + 1) * 10;

  // Accessing and printing array elements after resizing
  for i := 0 to High(numbers) do
    WriteLn('numbers[', i, '] = ', numbers[i]);
end.

```

### Open Arrays

Open arrays are typically used in procedures or functions to accept arrays of varying sizes.

**Syntax**

```pascal linenums="1"
procedure ProcedureName(arrayName: array of elementType);
```


**Example**

```pascal linenums="1"
program OpenArrayExample;

procedure PrintArray(arr: array of integer);
var
  i: integer;
begin
  for i := Low(arr) to High(arr) do
    WriteLn('arr[', i, '] = ', arr[i]);
end;

var
  numbers: array[1..5] of integer;
begin
  // Initialising the array
  numbers[1] := 10;
  numbers[2] := 20;
  numbers[3] := 30;
  numbers[4] := 40;
  numbers[5] := 50;

  // Passing the array to the procedure
  PrintArray(numbers);
end.
```


## Enum Types

In Free Pascal, enumerated ordinal types are user-defined types that consist of a set of named values. These values are called enumeration constants, and each constant has an associated integer value, starting from 0 by default. 

Enumerated types provide a way to define a variable that can only take one of a specified set of values, making the code more readable and type-safe.

**Syntax**

```pascal linenums="1"
type
  TEnumName = (Value1, Value2, Value3, ...);
```

**Example**

```pascal linenums="1"
  {$mode objfpc}{$H+}{$J-}

type
  TDay = (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);

var
  today: TDay;
begin
  // Assign today var to TDay.Wednesday 
  today := Wednesday;
  
  WriteLn('Integer(today)      gives ', Integer(today));     // Prints 3
  WriteLn('Integer(Wednesday)) gives ', Integer(Wednesday)); // Prints 3
  WriteLn('TDay(0)             gives ', TDay(0));            // Prints Sunday

  if today = Wednesday then
  begin
    WriteLn('Today is Wednesday');
  end;
end.
```

**Example of an Enum in case statement**

```pascal linenums="1"
  {$mode objfpc}{$H+}{$J-}

type
  TColor = (Red, Green, Blue);

var
  color: TColor;
begin
  color := Green;
  case color of
    Red: WriteLn('Red');
    Green: WriteLn('Green');
    Blue: WriteLn('Blue');
  end;
end.
```


## Processing Text File

...

## Collection of values

...

## Pointers

...