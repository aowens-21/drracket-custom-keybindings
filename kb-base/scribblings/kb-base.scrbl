#lang scribble/manual

@(require (for-label racket
                     racket/gui/base
                     kb-base))

@title{Keybinding Base Language}

Welcome to this documentation for the kb-base language. This language is for writing keybinding behavior, and when
combined with macros (in ways that will be explained in this guide) one can extend DrRacket with new, macro-specific
keybindings.

@table-of-contents[]

@section{Getting Started with Keybindings}

This language is designed to be used in conjunction with Racket's @racket[syntax-property]s, to extend DrRacket
with new keybindings. To do that, macro authors need to follow a specific interface for macros they want to attach
keybindings to.

@subsection{A Simple Example}

What follows is a very simple keybinding which uses CTRL+a to insert the text "watermelon" into the buffer (the body
of the macro is unimportant for this example):

@racketblock[
 (define-syntax (my-macro stx)
   (syntax-property #'1
                    'keybinding-info
                    (vector "c:a"
                            (insert "watermelon")
                            "a-very-silly-keybinding"
                            'global)))     
]

@subsection{The 'keybinding-info Property}

There are four key pieces of information in the @racket['keybinding-info] syntax property. From the previous example
we have:

@racketblock[
 (vector "c:a"
         (insert "watermelon")
         "a-very-silly-keybinding"
         'global)
]

The first element of the vector is a string representing the key combination that should be mapped to your keybinding,
it follows the same conventions in Racket's @racket[keymap%].

The second element of the vector should be a program written in this keybinding language, you can see the full list
of operations later in this guide.

The third element of the vector is a name for your keybinding as a string.

The fourth element of the vector represents the active range for the keybinding inside the current editor window. It can be
one of: the symbol @racket['global], the symbol @racket['local], or a vector with two numbers that represent the start and end
positions of the active range. If @racket['global] is provided, the keybinding is active for the entire editor window. If
@racket['local] is provided, the keybinding is active inside the body of the macro usage.

@subsection{Looping Forms}

One desirable property of keybinding programs running inside your editor is that they should always terminate. For this reason
the keybinding language does not support a traditional looping construct like those you might have seen in imperative languages.
Instead, this language provides two forms: @racket[do-times] and @racket[count-iters], which are designed to work in conjunction
with one another to approximate looping an uncertain number of times. Let's look at the definitions of these two forms:

The purpose of @racket[do-times] is to perform some body expression a fixed number of times, specifically @racket[iter-count] number
of times. Alone this is not very expressive and it's easy to think of examples where you need looping but a known, fixed number of
iterations is not useful. This is where @racket[count-iters] comes in.

As we can see @racket[count-iters] returns a @racket[number?]. This number represents the number of times @racket[count-iters] iterated. @racket[count-iters]
gets a @racket[condition-expr], @racket[step-size-expr], and @racket[step-type], and uses these to do the following: take one step, increment the count, and
evaluate @racket[condition-expr]. If it evaluates to @racket[#t], @racket[count-iters] keeps going, otherwise if it evaluates to @racket[#f],
@racket[count-iters] stops and returns the number of times it iterated. If at any time @racket[count-iters] tries to take a step and it cannot make progress,
for example when we have hit one end of the buffer, the loop terminates and the count is returned regardless of the condition.

By this point you may be wondering what that @racket[non-modify-buffer-kb-expr?] contract means in the signature of @racket[count-iters]. The answer is simple,
the point of @racket[count-iters] is not to change anything in the current buffer, but rather to count something based on the current state. @racket[non-modify-buffer-kb-expr?]
means that @racket[condition-expr] can only be an expression that we know leaves the buffer alone (so things like @racket[insert] or @racket[set-position] are out). Similarly, when
@racket[count-iters] returns, the current position in the buffer is reset to what it was before the loop started. 

Combining these two constructs, we can take in some information at runtime about the editor's state and then use that in @racket[do-times] to loop
the correct number of times. We will now look at a more complex keybinding that requires the use of both @racket[count-iters] and @racket[do-times].

@section{Keybinding Language Reference}

@defmodule[kb-base]

This section describes every supported operation by the @racket[kb-base] language. Each operation
returns an @deftech{kb-expr?} that represents an expression interpreted by the @racket[kb-base] interpreter.
Similarly there is an @deftech{buffer-safe-kb-expr?} that represents an expression that doesn't change the
current editor buffer.

@defproc[(insert [str (or/c string? kb-expr?)])
         void?]{
 Inserts @racket[str] into the buffer at the current position.
}

@defproc[(insert-return) void?]{
 Inserts a newline at the current position.
}

@defproc*[([(delete [start-expr kb-expr?]
                    [end-expr kb-expr?])
            void?]
           [(delete [num-of-chars-to-delete kb-expr?])
            void?])]{
 The first form deletes the text starting at @racket[start-expr] and ending at @racket[end-expr]. The
 second form which just takes a @racket[num-of-chars-to-delete] starts at the current position and
 deletes @racket[num-of-chars-to-delete] characters following that position.
}

@defproc[(seq [expr1 kb-expr?]
              ...)
         any/c]{
 Sequences all the @racket[exprs] passed to it and takes the last return value as its value (similar to Racket's
 @racket[begin].
}

@defproc[(set-position [pos-expr kb-expr?])
         void?]{
 Sets the current position in the editor to the result of @racket[pos-expr].
}

@defproc[(get-position)
         exact-nonnegative-integer?]{
 Returns end position of the currently selected text or the current position if none is selected.
}

@defproc[(last-position)
         exact-nonnegative-integer?]{
 Returns the last position in the editor.
}

@defproc[(move-position [step-size-expr kb-expr?]
                        [step-type symbol?])
         void?]{
 Uses the same rules as @racket[move-position] (TODO: Add a link to the text% docs), to move according to the result of @racket[step-size-expr]]
 and @racket[step-type].

 Additionally, takes a type @racket['sexp], which moves over @racket[step-size-expr] s-expressions at a time.
}

@defproc[(forward-sexp)
         void?]{
 Jumps over the next sexp following the current position.
}

@defproc[(backward-sexp)
         void?]{
 Jumps backwards over the first sexp preceding the current position.
}

@defproc[(down-sexp)
         void?]{
 Moves into the next sexp following the current position.
}

@defproc[(up-sexp)
         void?]{
 Jumps out of the current sexp (the one the current position is in) leaving the current position at the start of that sexp.
}

@defproc[(get-forward-sexp)
         (or/c exact-nonnegative-integer? #f)]{
 Returns the starting position the next sexp following the current position, or @racket[#f] if one doesn't exist.
}

@defproc[(forward-sexp-exists?)
         boolean?]{
 Returns @racket[#t] if there is an sexp following the current position, @racket[#f] otherwise.
}

@defproc*[([(get-character)
            char?]
           [(get-character [pos kb-expr?])
            char?])]{
 The first form gets the character that immediately follows the current cursor position. If @racket[pos] is provided
 this function gets the character immediately following it.
}

@defproc*[([(get-forward-word)
            string?]
           [(get-forward-word [pos kb-expr?])
            string?])]{
 The first form gets the word that immediately follows the current cursor position. If @racket[pos] is provided
 this function gets the word immediately following it.
}

@defproc[(get-text [start-expr kb-expr?]
                   [end-expr kb-expr?])
         string?]{
 Gets the text between the result of @racket[start-expr] and @racket[end-expr] and returns it as a string.
}

@defproc[(do-times [iter-count kb-expr?]
                   [body kb-expr?])
         any/c]{
 Evaluates @racket[iter-count] which should return a @racket[number?] and executes @racket[body] that many times.
}

@defproc[(count-iters [condition-expr non-modify-buffer-kb-expr?]
                      [step-size-expr kb-expr?]
                      [step-type symbol?])
         number?]{
 Returns the number of iterations where each iteration evaluates @racket[condition-expr] and then does moves in the buffer
 according to @racket[step-size-expr] and @racket[step-type]. @racket[condition-expr] is special in that it is not allowed to
 be an expression that modifies the buffer (i.e. @racket[non-modify-buffer-kb-expr?]). The current cursor position is reset
 after the number of iterations has been counted.
}

@defproc[(seek-while [condition-expr non-modify-buffer-kb-expr?]
                     [step-size-expr kb-expr?]
                     [step-type symbol?])
         void?]{
 Uses @racket[step-size-expr] and @racket[step-type] to seek through the buffer while @racket[condition-expr] holds true.
}

@defform[(kb-let (let-clause ...+) body-expr)
         #:grammar [(let-clause [id val-expr])]]{
  For each @racket[let-clause], binds @racket[id] to the result of @racket[val-expr] in @racket[body-expr].
}

@defproc[(kb-set! [id symbol?]
                  [new-val-expr kb-expr?])
         void?]{
 First checks to make sure @racket[id] is bound, then overwrites its current value to the result of @racket[new-val-expr].
}

@defproc[(kb-if [condition-expr kb-expr?]
                [thn-expr kb-expr?]
                [else-expr kb-expr?])
         any/c]{
 Evaluates @racket[condition-expr] and evaluates @racket[thn-expr] if the result is anything other than @racket[#f], if the result
 is @racket[#f] it evaluates @racket[else-expr] instead.
}

@defproc[(kb-not [expr kb-expr?])
         boolean?]{
 Evaluates @racket[expr], which should produce a @racket[boolean?], and negates the result.
}

@defproc[(kb-equal? [expr1 kb-expr?]
                    [expr2 kb-expr?])
         boolean?]{
 Evaluates @racket[expr1] and @racket[expr2], and then compares their results using @racket[equal?].
}

@defproc[(add [num-expr1 kb-expr?]
              [num-expr2 kb-expr?])
         number?]{
 Adds the result of @racket[num-expr1] and @racket[num-expr2].
}

@defproc[(sub [num-expr1 kb-expr?]
              [num-expr2 kb-expr?])
         number?]{
 Subtracts the result of @racket[num-expr2] from the result of @racket[num-expr1].
}
