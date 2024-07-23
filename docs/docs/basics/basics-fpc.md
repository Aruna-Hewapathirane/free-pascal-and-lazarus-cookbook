# Basics of Object Pascal using FPC

## Greetings!

This page is my go-to guide to the key basics of Object Pascal with the [Free Pascal Compiler](https://www.freepascal.org). It's not a complete guide, but it’s got all the essentials to help you dive in and discover just how powerful Free Pascal can be!

Hope you find it super helpful! 🚀

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

## Variables

### Declaration

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


### Assignment

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

## Console 

### User Input

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

### Display Text

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

## Round Floats 

### To Nearest Integers

You can use the following functions.

- [`Round`](https://www.freepascal.org/docs-html/rtl/system/round.html): Rounds a floating-point number to the nearest integer uses banker's rounding.
- [`Ceil`](https://www.freepascal.org/docs-html/rtl/math/ceil.html): Rounds a floating-point number up to the nearest integer.
- [`Floor`](https://www.freepascal.org/docs-html/rtl/math/floor.html): Rounds a floating-point number down to the nearest integer.

!!! Important

    For `Round`, in the case of .5 (equidistant from two numbers), the algorithm uses "banker's rounding": .5 values are always rounded towards the even number.

    Source: [https://www.freepascal.org/docs-html/rtl/system/round.html](https://www.freepascal.org/docs-html/rtl/system/round.html)

!!! Important
    Remember to add `Math` in the `uses` section `Ceil` and `Floor` functions.

**Examples**

```pascal linenums="1"
program RoundingExamples;

{$mode objfpc}{$H+}{$J-}

uses
  Math;  // Include the Math unit for Ceil and Floor

var
  num: Real;
  rounded: Integer;

  { Main Block }
begin
  num := 123.4567;

  // Using Round
  rounded := Round(num);  // Nearest integer, Banker's Rounding
  WriteLn('Rounded value (Round): ', rounded);

  // Using Ceil
  rounded := Ceil(num);   // Always rounds up
  WriteLn('Ceiling value (Ceil): ', rounded);

  // Using Floor
  rounded := Floor(num);  // Always rounds down
  WriteLn('Floor value (Floor): ', rounded);

  // Examples of Banker's Rounding
  num := 2.5;
  rounded := Round(num);  // Banker's Rounding
  WriteLn('Rounded value for 2.5 (Banker''s Rounding): ', rounded);

  num := 3.5;
  rounded := Round(num);  // Banker's Rounding
  WriteLn('Rounded value for 3.5 (Banker''s Rounding): ', rounded);

  // Pause console
  WriteLn('Press enter key to exit');
  ReadLn;
end.
```

### To `n` Decimal Places

Use the [RoundTo](https://www.freepascal.org/docs-html/rtl/math/roundto.html) function.

!!! Important

    `RoundTo` uses the standard `Round` function for this. Hence, in the case of .5 (equidistant from two numbers), the algorithm uses "banker's rounding": .5 values are always rounded towards the even number.
    
    Source: [https://www.freepascal.org/docs-html/rtl/math/roundto.html](https://www.freepascal.org/docs-html/rtl/math/roundto.html)

**Example**

```pascal linenums="1"
program NDecimalRoundingExample;

{$mode objfpc}{$H+}{$J-}

uses
  Math;

var
  num: real;
  rounded: real;
  n: integer;

  { Main Block }
begin
  num := 12345.678875;
  n := 4;  // Number of decimal places you want

  rounded := RoundTo(num, -n);

  WriteLn('Rounded Number: ', rounded: 0: 4);  // Format to 4 decimal places

  // Pause console
  WriteLn('Press enter key to exit');
  ReadLn;
end.
```

## Format Strings

### Format Numbers with Commas

1. **Include the `SysUtils` unit**, as the `Format` function is part of this unit.
2. Use the `Format` function with appropriate format specifiers.

See [Format](https://www.freepascal.org/docs-html/rtl/sysutils/format.html) for more info.

**Example**

```pascal linenums="1" hl_lines="5 6 16"
program FormatNumberCommas;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils;

var
  number: int64;
  formattedNumber: string;

  { Main Block }
begin
  // Formatting a number with commas
  number := 12345678;
  formattedNumber := Format('%.0n', [number * 1.0]);
  WriteLn('Formatted Number: ', formattedNumber);  // Output: 12,345,678

  // Pause console
  WriteLn('Press enter key to exit');
  ReadLn;
end.
```

- `'%.0n'` format specifier means *"format as a number with no decimal places, using the locale's thousands separator"*.

### Format Numbers as Currency

1. **Include the `SysUtils` unit**, as the `CurrToStrF` function is part of this unit.
2. Use the `CurrToStrF` function with appropriate format specifiers and decimal place.

See [`CurrToStrF`](https://www.freepascal.org/docs-html/3.2.2/rtl/sysutils/currtostrf.html) for more info.

**Example**

```pascal linenums="1" hl_lines="5 6 15"
program FormatCurrency;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils;

var
  amount: currency;
  formattedAmount: string;

  { Main Block }
begin
  amount := 12345678.90;
  formattedAmount := CurrToStrF(amount, ffCurrency, 2);
  WriteLn(formattedAmount);  // Output: $12,345,678.90

  // Pause console
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

- [`CurrToStrF`](https://www.freepascal.org/docs-html/3.2.2/rtl/sysutils/currtostrf.html) function: This formats a number as currency. The parameters are:
    - The number to format.
    - `ffCurrency`: A format specifier indicating that we want currency formatting.
    - `2`: The number of decimal places.


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

  { Main Block }
begin
  result := Add(3, 5);
  WriteLn('The sum is: ', result); // Will print 8
end.
```

## Loops

### The `for..to` Statement

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

  { Main Block }
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

  { Main Block }
begin
  for i := 10 downto 1 do
  begin
    WriteLn('i = ', i);
  end;
end.
```

### The `for..in` Statement

**Syntax**

```pascal
for element in collection do
begin
  // Your code here
end;
```

**Example**

```pascal
program ForInLoop;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils;

var
  numbers: array of integer;
  num: integer;

  { Main Block }
begin
  // Initialize the array
  numbers := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

  // Use the for..in loop to iterate over the array
  for num in numbers do
  begin
    WriteLn('Number: ', num);
  end;

  // Pause console
  WriteLn('Press enter key to quit');
  ReadLn;
end.
```

## Conditional Loops 

### The `while` Statement

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

  { Main Block }
begin
  i := 1;
  while i <= 10 do
  begin
    WriteLn('i = ', i);
    i := i + 1;
  end;
end.
```

### The `repeat` Statement

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

  { Main Block }
begin
  i := 1;
  repeat
    WriteLn('i = ', i);
    i := i + 1;
  until i > 10;
end.
```

## Choices - The `case` Statement

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

  { Main Block }
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

## Decisions - The `if` Statement

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

  { Main Block }
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

  { Main Block }
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

  { Main Block }
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

```pascal linenums="1" hl_lines="12"
  {$mode objfpc}{$H+}{$J-}

var
  str:string;
  len:integer;

  { Main Block }
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

  { Main Block }
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

```pascal linenums="1" hl_lines="11 16"
  {$mode objfpc}{$H+}{$J-}

var
  str1, str2, result: string;

  { Main Block }
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

```pascal linenums="1" hl_lines="14"
  {$mode objfpc}{$H+}{$J-}

uses
  SysUtils;

var
  str, result: string;

  { Main Block }
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

```pascal linenums="1" hl_lines="4 14 18"
  {$mode objfpc}{$H+}{$J-}

uses
  SysUtils;

var
  str, lowerStr, upperStr: string;

  { Main Block }
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

```pascal linenums="1" hl_lines="13"
  {$mode objfpc}{$H+}{$J-}

var
  str, substr: string;
  index: integer;

  { Main Block }
begin
  str := 'Hello, Pascal!';
  substr := 'Pascal';
  
  // Find position of 'Pascal' in str
  index := Pos(substr, str);
  WriteLn('Position of "', substr, '" in "', str, '" is: ', index); // Output: 8
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

  { Main Block }
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

  { Main Block }
begin
  color := Green;
  case color of
    Red: WriteLn('Red');
    Green: WriteLn('Green');
    Blue: WriteLn('Blue');
  end;
end.
```

## Subrange Types

A subrange is a subset of values within a specific range. In Free Pascal, subranges allow you to limit the values a variable can hold, which can help catch errors and make your code more robust and readable.

**Syntax**

```pascal linenums="1"
type
  SubrangeType = LowValue..HighValue;
```

**Example**

```pascal linenums="1"
program SubrangeDaysofWeek;

  {$mode objfpc}{$H+}{$J-}

type
  // Define a subrange type for days of the week (1 = Sunday, 7 = Saturday)
  TDayOfWeek = 1..7;

var
  // Declare a variable of type TDayOfWeek
  day: TDayOfWeek;

  { Main Block }
begin
  // Assign a valid value within the subrange to the variable
  day := 3;  // 3 represents Tuesday
  WriteLn('Day of the week is: ', day);

  // Uncommenting the following line would cause a compile-time error
  // because 8 is not within the defined subrange
  // day := 8;  // This will cause a compile-time error

  // Pause console
  WriteLn('Press enter key to quit');
  ReadLn;
end.
```

!!! Note

    **Why Not Just Use Integer?**
    
    Using a subrange like `TDayOfWeek` instead of a plain integer provides type safety. It ensures that the variable `day` can only be assigned values within the defined range (`1` to `7`). 
    
    This (1) helps prevent errors and makes your code more robust and (2) readable. For example, if you accidentally try to assign a value outside the range, the compiler will catch the error.

## Arrays

Arrays are useful when you need to handle multiple values of the same type. For example, if you have grades for students, you can use an array to store all these grades and easily access each one by its position.

### Defining Arrays

**1. Directly in the var section**

```pascal linenums="1"
var
  numbers: array[1..5] of integer;
```

**2. Using the type section**


```pascal linenums="1"
type
  TNumberArray = array[1..5] of Integer;

var
  numbers: TNumberArray;
```

### Working with Arrays

**Example**

```pascal linenums="1"
var
  numbers: array[1..5] of Integer;

  { Main Block }
begin
  numbers[1] := 10;  // Set the first element to 10
  numbers[2] := 20;  // Set the second element to 20
  writeln(numbers[1]); // This will print 10
end.
```

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

  { Main Block }
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

  { Main Block }
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

### Concat Dynamic Arrays

This operator is available in Delphi mode, but must be enabled explicily using the modeswitch arrayoperators in objfpc mode:

```pascal linenums="1"
{$mode objfpc}  
{$modeswitch arrayoperators}
```

**Syntax**

```pascal linenums="1"
resultArray := array1 + array2;
```


**Example**

```pascal linenums="1" hl_lines="4 33"
program DynArrayConcat;

  {$mode objfpc}{$H+}{$J-}
  {$modeswitch arrayoperators}

uses
  SysUtils;

type
  TIntArray = array of integer;

procedure PrintArray(arr: TIntArray);
var
  i: Integer;
begin
  for i := Low(arr) to High(arr) do
    Write(arr[i], ' ');
  WriteLn;
end;

var
  arr1, arr2, resultArr: TIntArray;

  { Main Block }
begin
  // Initialize the first array
  arr1 := [1, 2, 3, 4, 5];

  // Initialize the second array
  arr2 := [6, 7, 8, 9, 10];

  // Concatenate the arrays using the + operator
  resultArr := arr1 + arr2;

  // Print the arrays
  WriteLn('Array 1:');
  PrintArray(arr1);
  WriteLn('Array 2:');
  PrintArray(arr2);
  WriteLn('Concatenated Array:');
  PrintArray(resultArr);

  // Pause console
  ReadLn;
end.
```

See more info on the [Dynamic Array Operators](https://www.freepascal.org/docs-html/ref/refsu48.html) document.

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

  { Main Block }
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


## Records

Just for the `Record`, a `record` is a data structure that allows you to group different types of data together. This feature in Free Pascal allow you to create complex data structures and manage related data efficiently.

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

```pascal linenums="1"
  {$mode objfpc}{$H+}{$J-}

type
  TPerson = record
    name: string;
    age: integer;
    height: Real;
  end;

var
  person1, person2: TPerson;

  { Main Block }
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

## Advanced Records

In Free Pascal, an advanced record is a type of record that can do more than just store data. It can also have methods (which are like functions or procedures) and properties (ways to get or set values) attached to it.

You must include the following switch to use Advanced Records.

```pascal linenums="1"
{$modeswitch advancedrecords}
```

**Syntax**

```pascal linenums="1"
type
  TMyRecord = record
  private
    // Private fields and methods
    FField: Integer;
    procedure SetField(Value: Integer);
    function GetField: Integer;
  public
    // Public methods and properties
    procedure ShowInfo;
    property Field: Integer read GetField write SetField;
  end;
```

**Example: A Simple TRectangle Record**

Let’s create an advanced record to represent a rectangle. This record will store the width and height of the rectangle and include methods to calculate the area and display the rectangle's details.

We’ll create a record called `TRectangle` that has fields for width and height. It will also include a method to calculate the area and another to display the details.

```pascal linenums="1"
type
  TRectangle = record
  private
    FWidth, FHeight: Double;
    procedure SetWidth(Value: Double);
    procedure SetHeight(Value: Double);
    function GetWidth: Double;
    function GetHeight: Double;
  public
    constructor Create(AWidth, AHeight: Double);
    function Area: Double;
    procedure ShowDetails;
    property Width: Double read GetWidth write SetWidth;
    property Height: Double read GetHeight write SetHeight;
  end;

constructor TRectangle.Create(AWidth, AHeight: Double);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TRectangle.SetWidth(Value: Double);
begin
  FWidth := Value;
end;

procedure TRectangle.SetHeight(Value: Double);
begin
  FHeight := Value;
end;

function TRectangle.GetWidth: Double;
begin
  Result := FWidth;
end;

function TRectangle.GetHeight: Double;
begin
  Result := FHeight;
end;

function TRectangle.Area: Double;
begin
  Result := FWidth * FHeight;
end;

procedure TRectangle.ShowDetails;
begin
  WriteLn('Rectangle Width: ', FWidth:0:2);
  WriteLn('Rectangle Height: ', FHeight:0:2);
  WriteLn('Rectangle Area: ', Area:0:2);
end;
```

We use the `TRectangle` like this:

```pascal linenums="1"
var
  Rect: TRectangle;
  
  {Main Block}
begin
  // Create a rectangle with width 10 and height 5
  Rect := TRectangle.Create(10, 5);
  
  // Show the details of the rectangle
  Rect.ShowDetails;
end.
```

**Full Example**

```pascal linenums="1"
program AdvancedRecordDemo;

{$mode objfpc}{$H+}{$J-}

type
  TRectangle = record
  private
    FWidth, FHeight: Double;
    procedure SetWidth(Value: Double);
    procedure SetHeight(Value: Double);
    function GetWidth: Double;
    function GetHeight: Double;
  public
    constructor Create(AWidth, AHeight: Double);
    function Area: Double;
    procedure ShowDetails;
    property Width: Double read GetWidth write SetWidth;
    property Height: Double read GetHeight write SetHeight;
  end;

constructor TRectangle.Create(AWidth, AHeight: Double);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TRectangle.SetWidth(Value: Double);
begin
  FWidth := Value;
end;

procedure TRectangle.SetHeight(Value: Double);
begin
  FHeight := Value;
end;

function TRectangle.GetWidth: Double;
begin
  Result := FWidth;
end;

function TRectangle.GetHeight: Double;
begin
  Result := FHeight;
end;

function TRectangle.Area: Double;
begin
  Result := FWidth * FHeight;
end;

procedure TRectangle.ShowDetails;
begin
  WriteLn('Rectangle Width: ', FWidth:0:2);
  WriteLn('Rectangle Height: ', FHeight:0:2);
  WriteLn('Rectangle Area: ', Area:0:2);
end;

var
  Rect: TRectangle;

  { Main Block }
begin
  // Create a rectangle with width 10 and height 5
  Rect := TRectangle.Create(10, 5);
  
  // Show the details of the rectangle
  Rect.ShowDetails;
end.
```


## Pointers

> ... Avoid pointer whenever alternatives exist. If you want to learn, though, there's no silver bullet apart from: There has to be as many `Dispose` as `New`, period. 
> 
> Source: [Leledumbo's reply on 'Dispose of Pointer', 2023-08-10](https://forum.lazarus.freepascal.org/index.php/topic,64238.msg487999.html#msg487999).
 
**Example**

```pascal linenums="1"
program PointerExample;

var
  ptr: ^integer;  // Declare a pointer to Integer
  value: integer;

begin
  New(ptr);           // Allocate memory for an Integer
  ptr^ := 42;         // Assign value 42 to the memory location
  value := ptr^;      // Access the value through the pointer

  Writeln('Value pointed to by ptr: ', value);

  Dispose(ptr);       // Free the allocated memory
end.
```

### Safe Usage Tips

1. **Always Initialize Pointers**: Before using a pointer, make sure it points to valid memory. Uninitialized pointers can cause undefined behavior.

2. **Check for nil**: It’s good practice to check if a pointer is nil (i.e., not pointing to any memory) before using it:

```pascal linenums="1"
if ptr <> nil then
  Writeln(ptr^);
```

3. **Avoid Memory Leaks**: Always pair `New` with `Dispose` to prevent memory leaks. If you forget to free the allocated memory, it will not be available for other parts of your program or system.

4. **Don’t Use Freed Pointers**: After calling `Dispose`, the pointer still holds the address of the freed memory. Set it to `nil` to avoid accidental use:

```pascal linenums="1"
Dispose(ptr);
ptr := nil;
```

5. **Be Cautious with Pointer Arithmetic**: Although not commonly needed in high-level Pascal programming, pointer arithmetic (e.g., incrementing pointers) should be done carefully to avoid accessing invalid memory areas.
 
More info? See [Pointers](https://www.freepascal.org/docs-html/ref/refse15.html#x42-620003.4) and [Memory Management](https://wiki.freepascal.org/Memory_Management).


## Classes

Here is a simple example of creating a class. For mroe info, visit the official documentation; [Classes](https://www.freepascal.org/docs-html/ref/refch6.html#x69-930006). 

**Syntax**

```pascal linenums="1"
type
  TMyClass = class
  private
    // Private fields and methods
  protected
    // Protected fields and methods
  public
    // Public fields and methods
    constructor Create; // Constructor
    destructor Destroy; override; // Destructor
  end;
```

**Example**

```pascal linenums="1"
program ClassExample;

{$mode objfpc}{$H+}{$J-}

type
  // Define the class
  TPerson = class
  private
    FName: string;
    FAge: Integer;
  public
    constructor Create(const AName: string; AAge: integer);
    procedure DisplayInfo;
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

// Implementation of the constructor
constructor TPerson.Create(const AName: string; AAge: integer);
begin
  FName := AName;
  FAge := AAge;
end;

// Implementation of the method to display information
procedure TPerson.DisplayInfo;
begin
  WriteLn('Name: ', FName);
  WriteLn('Age: ', FAge);
end;

var
  Person: TPerson;

  { Main Block }
begin
  // Create an instance of TPerson
  Person := TPerson.Create('John Doe', 28);

  // Access properties
  Person.Name := 'Waldo Catto';
  Person.Age := 18;

  // Display information
  Person.DisplayInfo;

  // Free the memory used by the instance
  Person.Free;

  // Pause console
  WriteLn('Press enter key to quit');
  ReadLn;
end.
```

## Interfaces

Refer to the official doc [Interfaces](https://www.freepascal.org/docs-html/ref/refch7.html#x96-1200007) for more info.

- Interfaces can only be used in `delphi` or `objfpc` modes. 
- All parts of an `interface` are always `public`, so you can't hide them.
- Properties can only have methods to get or set their values. 
- You can't create interfaces directly. Instead, you need a `class` that uses the `interface`.
- You can only use calling convention modifiers in methods within an interface. You can't use special modifiers like `virtual`, `abstract`, `dynamic`, or `override` in an `interface`.

**Syntax**

1. Define the Interface: Use the `interface` keyword to define an interface, specifying the methods (and properties, if any) that any implementing class must provide.

2. Implement the Interface in a Class: Use the `class` keyword to define a class that implements the interface. The class **must provide concrete implementations** for all the methods and properties declared in the interface.


**Example**

This example defines a simple interface `IMyInterface` with one method `DoSomething`, and then implement this interface in a class `TMyClass`.

**1. Define the Interface**

```pascal linenums="1"
type
  IMyInterface = interface
    ['{12345678-1234-1234-1234-1234567890AB}'] // Unique identifier (GUID) for the interface
    procedure DoSomething;
  end;
```

**Step 2: Implement the Interface in a Class**

```pascal linenums="1"
type
  TMyClass = class(TInterfacedObject, IMyInterface)
  public
    procedure DoSomething;
  end;

procedure TMyClass.DoSomething;
begin
  WriteLn('Doing something...');
end;
```

**Step 3: Use the Interface and Class**

```pascal linenums="1"
var
  MyObject: IMyInterface;
begin
  MyObject := TMyClass.Create;
  MyObject.DoSomething;
end.
```

**Complete Example**

```pascal linenums="1"
program InterfaceExample;

{$mode objfpc}{$H+}{$J-}

type
  // Step 1: Define the Interface
  IMyInterface = interface
    ['{12345678-1234-1234-1234-1234567890AB}'] // Unique identifier (GUID) for the interface
    procedure DoSomething;
  end;

  // Step 2: Implement the Interface in a Class
  TMyClass = class(TInterfacedObject, IMyInterface)
  public
    procedure DoSomething;
  end;

procedure TMyClass.DoSomething;
begin
  WriteLn('Doing something...');
end;

var
  MyObject: IMyInterface;
begin
  // Step 3: Use the Interface and Class
  MyObject := TMyClass.Create;
  MyObject.DoSomething;
end.
```

- The GUID `['{12345678-1234-1234-1234-1234567890AB}']` is required for COM compatibility but can be a unique identifier in your application.
- `TInterfacedObject` is a base class that implements `IUnknown`, which is the ancestor of all interfaces. This ensures proper reference counting for memory management.

## More on Interfaces

### What is a GUID?

A GUID is like a super-unique name tag. Imagine you're at a huge event with thousands of people, and everyone needs to wear a name tag to avoid confusion. Each name tag has to be unique so that when someone calls out a name, only one person responds. That's what a GUID does for interfaces in programming.

### Why do we need a GUID for interfaces?

When we create interfaces in programming, we often have many different interfaces that might look similar. The GUID helps us keep track of each one and makes sure there's no mix-up. Here’s why this is important:

1. **Uniqueness**: Just like a unique name tag, a GUID makes sure that each interface is uniquely identified. No two interfaces will have the same GUID.

2. **Identification**: When your program is running, it might need to check if an object (a piece of data or a function) follows a certain set of rules (an interface). The GUID is used to ask,*"Do you follow these rules?"* and get a clear answer.

3. **Compatibility**: In complex programs or systems that involve many parts working together, like different pieces of software communicating with each other, the GUID ensures that they all understand each other correctly. It's like a universal language for interfaces.

### Example to Understand GUID

Imagine you're organizing a science fair. Each project needs a unique ID so judges know exactly which project they're looking at. Without unique IDs, two projects could have the same name, leading to confusion. GUIDs work the same way for interfaces in programming.

### Practical Example in Programming

Here's a simple example in Free Pascal:

1. Define the Interface with a GUID

```pascal linenums="1"
type
  IMyInterface = interface
    ['{12345678-1234-1234-1234-1234567890AB}'] // This is the GUID
    procedure DoSomething;
  end;
```

2. Implement the Interface in a Class

```pascal linenums="1"
type
  TMyClass = class(TInterfacedObject, IMyInterface)
  public
    procedure DoSomething;
  end;

procedure TMyClass.DoSomething;
begin
  WriteLn('Doing something...');
end;
```

3. Use the Interface and Class

```pascal linenums="1"
var
  MyObject: IMyInterface;
begin
  MyObject := TMyClass.Create;
  MyObject.DoSomething;
end.
```

### Breaking Down the Example

- **Define the Interface**: `IMyInterface` is like a rulebook that says any class that follows it must have a `DoSomething` procedure.
- **GUID**: `{12345678-1234-1234-1234-1234567890AB}` is a unique identifier for IMyInterface. It's like saying, *"This rulebook has a unique ID so there's no confusion."*
- **Implement the Interface**: `TMyClass` says, *"I follow the IMyInterface rulebook and provide a DoSomething procedure."*
- **Using the Interface**: The program creates an instance of `TMyClass` and calls `DoSomething` on it, knowing exactly which rules it's following because of the GUID.

### Summary

- GUIDs are unique identifiers that ensure interfaces are uniquely recognized.
- They help prevent confusion in large and complex systems.
- They allow programs to check if objects follow specific rules (interfaces) correctly.
- Think of a GUID as a unique fingerprint for an interface, ensuring it’s always identified correctly and uniquely in a program.


## Even More on Interfaces


### What is a Function?

Think of a function as a recipe. If you have a recipe for chocolate chip cookies, you follow those instructions every time you want cookies. You don’t need to worry about the recipe being mixed up with other recipes because you have the name of the recipe right there.

### What is an Interface?

An interface is like a contract or a blueprint that tells different objects (think of them as different people or tools) how they should behave. For instance, imagine you have a blueprint for different types of devices that can play music, like a smartphone, a tablet, or a speaker. Each of these devices follows the same set of instructions (the interface) for how to play music, but they might play it differently.

### Why Does an Interface Need a GUID?

**Unique Identification:**

- **Functions**: In a program, you call functions by their names. If you want to bake cookies, you just call the "cookie recipe" function. There's no need for a special identifier because each function name is unique within its context.
- **Interfaces**: Different interfaces might have similar methods, but they need a way to be uniquely identified. This is because many objects (devices) can follow the same interface (blueprint). The GUID acts like a unique serial number to make sure you’re dealing with the exact right blueprint.

**Multiple Implementations:**

- **Functions**: Each function is a specific set of instructions in your code. If you call a function, you're calling a specific set of instructions.
- **Interfaces**: An interface can be implemented by many different classes (objects). For example, you could have a `Player` interface for different types of media players. Each player (smartphone, tablet, speaker) will follow the same `Player` interface but might have different ways of playing the music. The GUID helps ensure that when you ask for a `Player`, you get the right kind of `Player`.

**Checking at Runtime:**

- **Functions**: When your program runs, it directly calls functions by their names. No extra checking is needed because you know exactly what function you're calling.
- **Interfaces**: Sometimes, you need to check if an object follows a particular interface, especially if you’re not sure what kind of object you have. The GUID helps you confirm that the object adheres to the right blueprint.

### Simple Analogy

Imagine you're at a huge convention where every booth has a unique ID number. Each booth might have a different type of product, but the unique ID ensures that when you ask for a specific type of product, you find the right booth.

- **Functions**: Like knowing the exact name of a recipe.
- **Interfaces**: Like having a unique ID for each type of product to make sure you get the right one.

### Example in Programming

Let's say you have an interface called `IDriveable` that any vehicle (like cars or bikes) should implement.

Interface Definition:

```pascal linenums="1"
type
  IDriveable = interface
    ['{11111111-1111-1111-1111-111111111111}'] // Unique ID
    procedure Drive;
  end;
  ```

Class Implementing the Interface:

```pascal linenums="1"
type
  TCar = class(TInterfacedObject, IDriveable)
  public
    procedure Drive;
  end;

procedure TCar.Drive;
begin
  WriteLn('Driving a car...');
end;

```

Using the Interface:

```pascal linenums="1"
var
  Vehicle: IDriveable;
  MyCar: TCar;
begin
  MyCar := TCar.Create;
  Vehicle := MyCar;
  Vehicle.Drive; // Calls the Drive method from TCar
end.
```

In this example, `IDriveable` has a unique `GUID`, so even if you have many different classes (like `TCar`, `TBike`) that implement IDriveable, the GUID ensures you’re interacting with the right interface.

### Summary

- **GUID for Interfaces**: Ensures each interface is uniquely identified, especially when dealing with multiple implementations.
- **Ordinary Functions**: Are unique by their names within their code context, so they don’t need an extra unique identifier.

The GUID is like a special label that makes sure you’re talking to the exact right set of instructions (interface) among many possibilities.


## Processing Text File

...




