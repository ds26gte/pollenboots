#lang pollen

◊title[#:tag "ffi"]{FFI Helpers}

There are a number of convenience functions that aren't native to the Pyret
runtime, but are often used in JavaScript code that interacts with the runtime.
Some of the most commonly used ones are documented here; this list is often
growing.

◊section[#:tag "ffi:equality"]{Equality}

The ffi exposes several utilities related to ◊secref["equality"].

◊pyret-block{
  FFI.equal :: PyretObject
}

The ◊pyret-id["Equal" "equality"] value.

◊pyret-block{
  FFI.unknown :: PyretObject
}

The ◊pyret-id["Unknown" "equality"] value.

◊pyret-block{
  FFI.notEqual :: PyretFunction
}

The ◊pyret-id["NotEqual" "equality"] constructor.

◊pyret-block{
  FFI.isEqual(Any) → JSBoolean
}

Checks if the given value is ◊pyret-id["Equal" "equality"].

◊pyret-block{
  FFI.isNotEqual(Any) → JSBoolean
}

Checks if the given value is an instance of ◊pyret-id["NotEqual" "equality"].

◊pyret-block{
  FFI.isUnknown(Any) → JSBoolean
}

Checks if the given value is a ◊pyret-id["Unknown" "equality"].

◊pyret-block{
  FFI.isEqualityResult(Any) → JSBoolean
}

Checks if the given value is an instance of a ◊pyret-id["EqualityResult" "equality"].

◊section[#:tag "ffi:exceptions"]{Exceptions}

FFI helpers provide the easiest way to programmatically throw Pyret exceptions
from JavaScript.  Most commonly, user-defined modules will simply throw
◊tt{MessageExceptions} that contain a string describing the error.

◊pyret-block{
  FFI.throwMessageException(PyretString) → Undefined
}

Throws an exception that Pyret recognizes and reports with a stack trace, using
the provided string as the message.

◊pyret-block{
  FFI.makeMessageException(PyretString) → Error
}

Sometimes its useful to ◊emph{create} an exception without actually throwing
it, like when using the ◊tt{error} callback of the ◊tt{Restarter} in
◊internal-id["Runtime" "pauseStack"].  This call creates a new exception object
without throwing it.

◊section[#:tag "ffi:lists"]{Lists}

Pyret lists are ubiquitous in Pyret's internals and libraries, and this library
provides a few conveniences for working with them.

◊pyret-block{
  FFI.makeList(JSArray) → List
}

Turns a JavaScript array into a Pyret ◊pyret-id["List" "lists"] with the same
elements in the same order.

◊pyret-block{
  FFI.toArray(List) → JSArray
}

Turns a Pyret ◊pyret-id["List" "lists"] with the same elements in the same
order.  For doing computationally heavy work, sometimes it is useful to convert
a Pyret ◊tt{List} to an array before processing it (and using JavaScript's
map/filter, etc.), since the Pyret version incurs more overhead.

◊pyret-block{
  FFI.isList(Any) → JSBoolean
}

Returns ◊tt{true} if the value is a Pyret ◊pyret-id["List" "lists"] and
◊tt{false} otherwise.

◊section{Other Data Helpers}

◊pyret-block{
  FFI.makeSome(Any) → Option
}

◊pyret-block{
  FFI.makeNone(Any) → Option
}

Create instances of ◊pyret-id["none" "option"] and ◊pyret-id["some" "option"]
from ◊secref["option"].

◊pyret-block{
  FFI.makeLeft(Any) → Either
}

◊pyret-block{
  FFI.makeRight(Any) → Either
}

Create instances of ◊pyret-id["left" "either"] and ◊pyret-id["right" "either"]
from ◊secref["either"].

◊pyret-block{
  FFI.cases(
    (Any -> JSBoolean)
    JSString
    Any
    Handlers
  )
  → Any
}

This call emulates the functionality of the ◊tt{cases} expression in a
JavaScript context.  It takes a predicate to check (usually a function like
◊tt{is-List}), a name for the predicate being checked, a ◊tt{data} value, and
an object containing handlers for its variants.  For example:

◊verbatim{
function sum(l) {
  cases(runtime.getField(lists, "is-List"), "List", l, {
    empty: function() { return "Empty"; },
    link: function(f, r) { return f + sum(r); }
  });
}
sum(runtime.ffi.makeList([1,2])) // is 3
}

The predicate check and name are solely for error reporting.
