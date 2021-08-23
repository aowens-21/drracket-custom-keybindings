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
with new keybindings. To do that, macro authors need to follow a specific @racket[syntax-property] interface for macros they want to attach 
keybindings to.

@subsection{A Simple Example}

What follows is a very simple keybinding which uses CTRL+a to insert the text "watermelon" into the buffer (the body
of the macro is unimportant for this example), feel free to paste this into your DrRacket once you have the plugin installed
and try it out:

@racketblock[
 (define-syntax (my-macro stx)
   (syntax-property #'1
                    'keybinding-info
                    (make-kb "c:a"
                             (insert "watermelon")
                             "a-very-silly-keybinding"
                             'global
                             stx)))     
 ]

@subsection{The 'keybinding-info Property}

There are five key pieces of information in the @deftech{'keybinding-info} syntax property. From the previous example
we have:

@racketblock[
 (vector "c:a"
         (insert "watermelon")
         "a-very-silly-keybinding"
         'global
         (make-srcloc (syntax-source stx)
                      (syntax-line stx)
                      (+ 1 (syntax-column stx))
                      (+ 1 (syntax-position stx))
                      (syntax-span stx)))
 ]

The first element of the vector is a string representing the @deftech{key combination} that should be mapped to your keybinding,
it follows the same conventions in Racket's @racket[keymap%].

The second element of the vector should be a program written in this keybinding language, you can see the full list
of operations later in this guide.

The third element of the vector is a name for your keybinding as a string.

The fourth element of the vector represents the active range for the keybinding inside the current editor window. It can be
one of: the symbol @racket['global], the symbol @racket['local], or a vector with two numbers that represent the start and end
positions of the active range. If @racket['global] is provided, the keybinding is active for the entire editor window. If
@racket['local] is provided, the keybinding is active inside the body of the macro usage.

The fifth element is a @racket[srcloc] used by the @racket[drracket-custom-keybindings] plugin to correctly extract @tech{'keybinding-info}
properties from the expanded syntax in the editor.

@subsection{A More Involved Example}

Maybe inserting "watermelon" into the buffer isn't a great keybinding for your use case, and you want some more complex
keybindings related to your macro. Luckily this tool supports more than just simple fruit-related string insertion, so lets consider
a more interesting example. Maybe you're writing an @racket[cond] expression with two clauses, like the one below, and you want
to move some things from the first clause into the second @racket[else] clause. In this case we'll say everything after the
insertion point that is inside the clause should be moved, so the keybinding will work in relation to the current position (in this case
imagine the cursor is preceding the s-expression "(body-2)" in the first clause). We start with something like:

@racketblock[
 (cond [(some-condition-expr)
        (body-1)
        (body-2)
        (body-3)]
       [else
        (else-body)])]

and we want to end up with:

@racketblock[
 (cond [(some-condition-expr)
        (body-1)]
       [else
        (else-body)
        (body-2)
        (body-3)])
 ]

The idea of this keybinding is to basically loop over each of the expressions in the cond-clause following the current insertion point
until we hit the end the of the clause. At this point we can copy the text between the point where we started and where we ended and move
it to the next clause (following any sub-expressions in that clause). First off lets look at the macro we might write to attach this keybinding
to:

@racketblock[
 (define-syntax (my-cond stx)
   (syntax-property (...) ;; some cond implementation
                    'keybinding-info
                    (make-kb "c:b"
                             move-exprs-into-next-cond-clause ;; we'll fill this in next
                             "my-cond-keybinding"
                             'local
                             stx)))     
 ]

I've omitted some details including the implementation of @racket[my-cond] and exactly what @racket[move-exprs-into-next-cond-clause]
is referring to, but lets look at the other pieces of the @tech{'keybinding-info} property. The @tech{key combination} is pretty self-explanatory:
it means that the keybinding is activated by CTRL+b. The name is simply @racket["my-cond-keybinding"]. The activation range is the only
other interesting property, as it is now @racket['local] instead @racket['global], meaning this keybinding is only active inside a
usage of @racket[my-cond]. Although this keybinding may be useful in other contexts we are really designing it for @racket[my-cond], so it's
a good idea to make it @racket['local] to avoid conflicts with other keybindings. Now we can look at @racket[move-exprs-into-next-cond-clause]:

@racketblock[
 (define-for-syntax move-exprs-into-next-cond-clause
   (kb-let (['start-pos (get-position)])
           (seek-while (forward-sexp-exists?)
                       1
                       'sexp)
           (kb-let (['end-pos (get-position)]
                    ['text-to-move (get-text 'start-pos 'end-pos)])
                   (up-sexp)
                   (forward-sexp)
                   (down-sexp)
                   (seek-while (forward-sexp-exists?)
                               1
                               'sexp)
                   (insert-return)
                   (insert 'text-to-move)
                   (delete 'start-pos 'end-pos)
                   (set-position 'start-pos))))
 ]

This program starts off by storing its starting position so it knows where to begin copying the body
of the clause. Then it uses the @racket[seek-while] form to step over each subsequent s-expression until
it hits the end of the clause and stops. @racket[seek-while] can be thought of as a way to move over the buffer
until a condition returns false or it hits one end of the buffer. The rest of program stores the end position to copy,
moves to the next @racket[cond] clause, and pastes the correct text into it. Once this is done, the original text is deleted
and the position is reset to the start position. In the next section we'll see how to use @racket[kb-base]'s looping
forms to accomplish the same thing, and then in section 2 we'll see more examples of keybinding programs.

@subsection{Looping Forms}

The previous example could be accomplished using the looping forms @racket[do-times] and @racket[count-iters] instead of
@racket[seek-while] (indeed seek-while is just intended to be a nice way to seek through the buffer without use these looping forms).
First let's think about the reason for these two unconventional looping constructs.

One desirable property of keybinding programs running inside your editor is that they should always terminate. For this reason
the keybinding language does not support a traditional looping construct like those you might have seen in imperative languages.
Instead, this language provides two forms: @racket[do-times] and @racket[count-iters], which are designed to work in conjunction
with one another to approximate looping an uncertain number of times.

The purpose of @racket[do-times] is to perform some body expression a fixed number of times:

@racketblock[
 (test-kb-program (do-times 10 (insert "a")))
 ]

produces:

@racketblock[
 (values void 10 "aaaaaaaaaa")
 ]

While @racket[do-times] doesn't seem expressive, it is powerful enough to approximate loopping when combined with @racket[count-iters]. @racket[count-iters]
gets a @racket[condition-expr], @racket[step-size-expr], and @racket[step-type], and uses these to do the following: take one step, increment the count, and
evaluate @racket[condition-expr]. If it evaluates to @racket[#t], @racket[count-iters] keeps going, otherwise if it evaluates to @racket[#f],
@racket[count-iters] stops and returns the number of times it iterated. If at any time @racket[count-iters] tries to take a step and it cannot make progress,
for example when we have hit one end of the buffer, the loop terminates and the count is returned regardless of the condition.

@racketblock[
 (test-kb-program (count-iters (forward-sexp-exists?)
                               1
                               'sexp)
                  #:setup-proc (λ (editor)
                                 (send editor insert "() () ()")
                                 (send editor set-position 0)))
 ]

produces:

@racketblock[
 (values 3 0 "() () ()")
 ]

The idea is that @racket[count-iters] cannot change the buffer at all, meaning the position and content will stay the same no matter what. Combining
these two constructs, we can take in some information at runtime about the editor's state and then use that in @racket[do-times] to loop some unknown
number of times. We will now look at some more complex keybindings, some of which require the use of both @racket[count-iters] and @racket[do-times].
As a final example for this section, consider the following rewrite of the @racket[cond] example in the previous section:

@racketblock[
 (define-for-syntax move-exprs-into-next-cond-clause
   (kb-let (['start-pos (get-position)])
           (do-times (count-iters (forward-sexp-exists?)
                                  1
                                  'sexp)
                     (forward-sexp))
           (kb-let (['end-pos (get-position)]
                    ['text-to-move (get-text 'start-pos 'end-pos)])
                   (up-sexp)
                   (forward-sexp)
                   (down-sexp)
                   (do-times (count-iters (forward-sexp-exists?)
                                          1
                                          'sexp)
                             (forward-sexp))
                   (insert-return)
                   (insert 'text-to-move)
                   (delete 'start-pos 'end-pos)
                   (set-position 'start-pos))))
 ]

@section{Some Example Keybindings}

Often it is easier to get an understanding of how something works by looking at examples, so in this section we will
look at a few example keybindings made for existing forms in Racket.

@subsection{Example 1: Swapping @racket[cond] Branches}

First lets look at an example my-cond macro which we'll pretend behaves like @racket[cond]. Notice how
we can attach a keybinding using the @tech{'keybinding-info} property. The program @racket[swap-cond-branches]
will be defined later.

@racketblock[
 (define-syntax (my-cond stx)
   (syntax-property #'(cond ...)
                    'keybinding-info
                    (make-kb "c:space"
                             swap-cond-branches
                             "swap-cond-branches"
                             'local
                             stx)))     
 ]

This simple keybinding takes into account the cursor position inside a cond, and swaps the first branch
following the cursor with the one following it. For example, if we have this before the the keybinding:

@racketblock[
 (my-cond [(some-predicate? x) #t]
          [(some-other-predicate? x y) #f])
 ]

with the current cursor position preceding the first branch, then after the keybinding we'd have:

@racketblock[
 (my-cond [(some-other-predicate? x y) #f]
          [(some-predicate? x) #t])
 ]

Let's look at the full code for this keybinding program:

@racketblock[
 (define swap-cond-branches
   (seq (down-sexp)
        (kb-let (['first-sexp-start (get-position)]
                 ['first-sexp-end (seq (up-sexp)
                                       (forward-sexp)
                                       (sub (get-position) 1))]
                 ['second-sexp-start (seq (down-sexp)
                                          (get-position))]
                 ['second-sexp-end (seq (up-sexp)
                                        (forward-sexp)
                                        (sub (get-position) 1))]
                 ['first-sexp-content (get-text 'first-sexp-start 'first-sexp-end)]
                 ['second-sexp-content (get-text 'second-sexp-start 'second-sexp-end)])
                (set-position 'second-sexp-start)
                (delete 'second-sexp-start 'second-sexp-end)
                (insert 'first-sexp-content)
                (set-position 'first-sexp-start)
                (delete 'first-sexp-start 'first-sexp-end)
                (insert 'second-sexp-content)
                (set-position 'first-sexp-start)
                (up-sexp))))
 ]

The high level idea of this keybinding program is to start right before the opening parenthesis of the first branch you want to move,
then from there we copy the content from that branch and the one following it, then we just insert the content of the second branch
over the first and the content of the first over the second, effectively swapping the two branches.

The various bindings inside the @racket[kb-let] expression take care of getting the content, which gets bound to @racket['first-sexp-content]
and @racket['second-sexp-content]. Once we have these, the body of the let just takes care of moving to the correct positions, deleting the original
content and replacing it with the content we're swapping in.

More generally, you can read a keybinding program as sort of a sequence of steps you might take when making this code transformation manually, or put
another way you can walk through each instruction and how it will affect the buffer as you read the program. As you can see everything is quite low-level,
so the level of reasoning is in terms of very discrete small operations on the buffer like "insert" and "delete".

@subsection{Example 2: Converting Between Contract Forms}

Once again we'll look at the macro definition for our own version of @racket[->] called @racket[my->] with a keybinding attached.
The actual keybinding program is explained later in this example.

@racketblock[
 (define-syntax (my-> stx)
   (syntax-property #'(-> ...)
                    'keybinding-info
                    (make-kb "c:space"
                             arrow-to-arrow-star
                             "arrow-to-arrow-star-kb"
                             'local
                             stx)))     
 ]

Another code transformation one might like to make is updating the contract on a function they've written. Maybe initially
the function took no optional arguments and now it needs a different contract form to represent the optional argument's contract.
Remembering the syntax for the various contract combinators provided by Racket can be difficult, so the keybindings we see in this section
take one contract combinator and convert it to the other form automatically. Imagine, for example, you have the following
function:

@racketblock[
 (define/contract (my-func a b)
   (my-> number? string? boolean?)
   ...)
 ]

Converting this to the @racket[->*] syntax can be done easily with a keybinding, resulting in:

@racketblock[
 (define/contract (my-func a b)
   (->* (number? string?)
        ()
        boolean?)
   ...)
 ]

Let's look at the full code for this keybinding program:

@racketblock[
 (define arrow-to-arrow-star
   (seq (delete 4)
        (insert "->* ")
        (kb-let (['num-of-args (sub (count-iters (forward-sexp-exists?) 1 'sexp) 1)])
                (insert "(")
                (delete 1)
                (do-times 'num-of-args (move-position 1 'sexp))
                (insert ")")
                (insert-return)
                (insert "()")
                (insert-return))))
 ]

This keybinding should be invoked with the current position immediately preceding @racket[my->].
Using that position, we simply delete the name and insert the new one with a star. This is the call to
@racket[(delete 4)].

The next task is to use @racket[count-iters] to compute how many arguments the function takes, which we can do by iterating
one s-expression at a time and subtracting the total by 1 (the return value contract). Once we have this we use a @racket[do-times] to
wrap the arguments in one s-expression, insert an empty () for the optional paramters we'll expect the user to add, and end up with
these pieces of the @racket[->*] contract on their own line, where the last thing is the return type. We could easily decide to
insert less newlines or customize exactly how we want the @racket[->*] expression to look once it has been transformed.

@subsection{Example 3: Pattern Matching Code Generation}

This last example is for a macro like @racket[|#|lang plai]'s @racket[define-type], here is the macro with a keybinding
attached:

@racketblock[
 (define-syntax (my-define-type stx)
   (syntax-parse stx
     [(_ type-name
         [var-name (field-name field-c) field-clause ...]
         ...)
      (define first-var-pos (- (syntax-position (third (syntax->list stx))) 1))
      (syntax-property
       #'(define-type type-name [var-name (field-name field-c) field-clause ...] ...)
       'keybinding-info
       (make-kb "c:space"
                (gen-type-case (symbol->string (syntax-e #'type-name))
                               first-var-pos)
                "generate-type-case"
                'global
                stx))]))
 ]

This macro's definition warrants a little more explanation. We'll see a bit later on, but we need to pass to this keybinding
program two things: the position of the first variant's s-expression, and the type name for the @racket[define-type]. We can
get this information from the macro usage and it's passed to the call to @racket[gen-type-case].

Now lets look at what this keybinding should do. Given the following defined in our buffer:

@racketblock[
 (define-type Animal
   [dog (bark-volume number?)]
   [cat (favorite-treat string?)
    (coat-length string?)]
   [lizard (eats-bugs boolean?)])
 ]

We want to generate a @racket[type-case] that looks like this:

@racketblock[
 (type-case Animal ...
   [dog (bark-volume) ...]
   [cat (favorite-treat coat-length) ...]
   [lizard (eats-bugs) ...])
 ]

The idea of this keybinding is that it will use some information from the @racket[define-type] macro to get the
name of the type and the position where the first variant's sexpr starts. It will then use this information to construct
a type case from the definition. The full code is as follows, and we can walk through it in detail:

@racketblock[
 (define (gen-type-case type-name var-pos)
   (seq (set-position (last-position))
        (insert-return)
        (insert-return)
        (insert "(type-case ")
        (insert type-name)
        (insert " ...")
        (insert-return)
        (kb-let
         (['insert-posn (get-position)])
         (set-position var-pos)
         (do-times (count-iters (forward-sexp-exists?)
                                1
                                'sexp)
                   (down-sexp)
                   (kb-let
                    (['current-variant-name-posn (get-position)])
                    (set-position 'insert-posn)
                    (insert "[")
                    (insert (get-forward-word 'current-variant-name-posn))
                    (insert " (")
                    (kb-set! 'insert-posn (get-position))
                    (set-position 'current-variant-name-posn)
                    (forward-sexp)
                    (do-times (count-iters (forward-sexp-exists?)
                                           1
                                           'sexp)
                              (down-sexp)
                              (kb-let
                               (['current-field-name-posn (get-position)])
                               (set-position 'insert-posn)
                               (insert (get-forward-word 'current-field-name-posn))
                               (insert " ")
                               (kb-set! 'insert-posn (get-position))
                               (set-position 'current-field-name-posn)
                               (up-sexp)
                               (forward-sexp)))
                    (set-position 'insert-posn)
                    (delete (sub (get-position) 1) (get-position))
                    (insert ") ...]")
                    (insert-return)
                    (kb-set! 'insert-posn (get-position))
                    (set-position 'current-variant-name-posn)
                    (up-sexp)
                    (forward-sexp)))
         (set-position 'insert-posn)
         (seek-while (kb-not (kb-equal? (get-character) #\])) -1 'simple)
         (move-position 1 'simple)
         (insert ")"))))
 ]

This is definitely the most complex keybinding program we've looked at, and rather than walking through it line by line we can
see it as a few high level tasks. The first thing to notice is we take in @racket[type-name] and @racket[var-pos], which represent
the name of the type we'd like to generate the @racket[type-case] for and the starting position of the s-expression containing
the first variant in the type's definition. This is all information we can get from the macro we're attaching this keybinding program to.

The first part of the program sets up the start of the call to @racket[type-case], by inserting "(type-case <type-name> ..." where the "..."
is left for the user to fill in with an expression producing the correct type. The program then sets up a variable @racket['insert-posn], which
will be sort of a running track of where to insert text throughout the course of the program (in this case we start generating the @racket[type-case]
at the end of the buffer and we just update it each time we do an insert).

@section{Keybinding Language Reference}

@defmodule[kb-base]

@subsection{Operations}

This section describes every supported operation by the @racket[kb-base] language. Every operation
returns an @racket[kb-expr?] that represents an expression interpreted by the @racket[kb-base] interpreter.
Similarly there is an @racket[buffer-safe-kb-expr?] that represents an expression that doesn't change the
current editor buffer.

@defproc[(insert [str (or/c string? symbol? kb-expr?)])
         kb-expr?]{
 Inserts @racket[str] into the buffer at the current position.
}

@defproc[(insert-return) kb-expr?]{
 Inserts a newline at the current position.
}

@defproc[(delete [start-expr (or/c exact-nonnegative-integer? symbol? kb-expr?)]
                 [end-expr (or/c #f exact-nonnegative-integer? symbol? kb-expr?) #f])
         kb-expr?]{
 The first form deletes the text starting at @racket[start-expr] and ending at @racket[end-expr]. If @racket[end-expr]
 is @racket[#f], deletes @racket[start-expr] characters starting at the current position.
}

@defproc[(seq [expr1 (or/c string? char? number? boolean? symbol? kb-expr?)]
              ...)
         kb-expr?]{
 Sequences all the expressions passed to it and returns the value of the final one (similar to Racket's
 @racket[begin]).
}

@defproc[(set-position [pos-expr (or/c exact-nonnegative-integer? symbol? kb-expr?)])
         kb-expr?]{
 Sets the current position in the editor to the result of @racket[pos-expr].
}

@defproc[(get-position)
         kb-expr?]{
 Returns end position of the currently selected text or the current position if none is selected.
}

@defproc[(last-position)
         kb-expr?]{
 Returns the last position in the editor.
}

@defproc[(move-position [code (or/c 'up
                                    'down
                                    'left
                                    'right
                                    'home
                                    'end)]
                        [kind (or/c 'simple
                                    'word
                                    'line
                                    'page
                                    'sexp)])
         kb-expr?]{
 Uses the same rules as @racket[text%]'s move-position to move based on the passed @racket[code] and @racket[kind].

 Additionally, takes a kind @racket['sexp], which works with @racket['right] or @racket['left].
}

@defproc[(forward-sexp)
         kb-expr?]{
 Jumps over the next sexp following the current position.
}

@defproc[(backward-sexp)
         kb-expr?]{
 Jumps backwards over the first sexp preceding the current position.
}

@defproc[(down-sexp)
         kb-expr?]{
 Moves into the next sexp following the current position.
}

@defproc[(up-sexp)
         kb-expr?]{
 Jumps out of the current sexp (the one the current position is in) leaving the current position at the start of that sexp.
}

@defproc[(get-forward-sexp)
         kb-expr?]{
 Returns the starting position the next sexp following the current position, or @racket[#f] if one doesn't exist.
}

@defproc[(forward-sexp-exists?)
         kb-expr?]{
 Returns @racket[#t] if there is an sexp following the current position, @racket[#f] otherwise.
}

@defproc[(get-character [pos (or/c #f exact-nonnegative-integer? symbol? kb-expr?) #f])
         kb-expr?]{
 The first form gets the character that immediately follows the current cursor position. If @racket[pos] is not @racket[#f],
 returns the character after the position @racket[pos] in the current editor.
}

@defproc[(get-forward-word [pos (or/c #f exact-nonnegative-integer? symbol? kb-expr?) #f])
         kb-expr?]{
 The first form gets the word that immediately follows the current cursor position. If @racket[pos] is not @racket[#f],
 returns the word after the position @racket[pos] in the current editor.
}

@defproc[(get-text [start-expr (or/c exact-nonnegative-integer? symbol? kb-expr?)]
                   [end-expr (or/c exact-nonnegative-integer? symbol? kb-expr?)])
         kb-expr?]{
 Gets the text between the result of @racket[start-expr] and @racket[end-expr] and returns it as a string.
}

@defproc[(do-times [iter-count (or/c number? kb-expr?)]
                   [body kb-expr?])
         kb-expr?]{
 Evaluates @racket[iter-count] which should return a @racket[number?] and executes @racket[body] that many times.
}

@defproc[(count-iters [condition-expr (or/c boolean? symbol? buffer-safe-kb-expr?)]
                      [step-size-expr (or/c number? symbol? kb-expr?)]
                      [step-type (or/c 'simple
                                       'word
                                       'line
                                       'page
                                       'sexp)])
         kb-expr?]{
 Returns the number of iterations where each iteration evaluates @racket[condition-expr] and then does moves in the buffer
 according to @racket[step-size-expr] and @racket[step-type]. @racket[condition-expr] is special in that it is not allowed to
 be an expression that modifies the buffer (i.e. @racket[non-modify-buffer-kb-expr?]). The current cursor position is reset
 after the number of iterations has been counted.
}

@defproc[(seek-while [condition-expr (or/c boolean? symbol? buffer-safe-kb-expr?)]
                     [step-size-expr (or/c number? symbol? kb-expr?)]
                     [step-type (or/c 'simple
                                      'word
                                      'line
                                      'page
                                      'sexp)])
         kb-expr?]{
 Uses @racket[step-size-expr] and @racket[step-type] to seek through the buffer while @racket[condition-expr] holds true.
}

@defform[(kb-let (let-clause ...+) body-expr)
         #:grammar [(let-clause [id val-expr])]]{
 For each @racket[let-clause], binds @racket[id] to the result of @racket[val-expr] in @racket[body-expr].
}

@defproc[(kb-set! [id symbol?]
                  [new-val-expr (or/c string?
                                      number?
                                      char?
                                      boolean?
                                      symbol?
                                      kb-expr?)])
         kb-expr?]{
 First checks to make sure @racket[id] is bound, then overwrites its current value to the result of @racket[new-val-expr].
}

@defproc[(kb-if [condition-expr (or/c boolean? symbol? kb-expr?)]
                [thn-expr (or/c string? number? char? boolean? symbol? kb-expr?)]
                [else-expr (or/c string? number? char? boolean? symbol? kb-expr?)])
         kb-expr?]{
 Evaluates @racket[condition-expr] and evaluates @racket[thn-expr] if the result is anything other than @racket[#f], if the result
 is @racket[#f] it evaluates @racket[else-expr] instead.
}

@defproc[(kb-not [expr (or/c boolean? symbol? kb-expr?)])
         kb-expr?]{
 Evaluates @racket[expr], which should produce a @racket[boolean?], and negates the result.
}

@defproc[(kb-equal? [expr1 (or/c string? number? char? boolean? symbol? kb-expr?)]
                    [expr2 (or/c string? number? char? boolean? symbol? kb-expr?)])
         kb-expr?]{
 Evaluates @racket[expr1] and @racket[expr2], and then compares their results using @racket[equal?].
}

@defproc[(add [num-expr1 (or/c number? symbol? kb-expr?)]
              [num-expr2 (or/c number? symbol? kb-expr?)])
         kb-expr?]{
 Adds the result of @racket[num-expr1] and @racket[num-expr2].
}

@defproc[(sub [num-expr1 (or/c number? symbol? kb-expr?)]
              [num-expr2 (or/c number? symbol? kb-expr?)])
         kb-expr?]{
 Subtracts the result of @racket[num-expr2] from the result of @racket[num-expr1].
}

@subsection{Helpers}

This section documents helpers provided by the @racket[kb-base] language.

The @racket[kb-base] package also provides a few utilities to make testing easier. Since programs written in this
language run in the context of an editor window (specifically a @racket[racket:text<%>]), these test utilities
handle setting that editor up and getting results back from running programs.

@defproc[(make-kb [stroke string?]
                  [kb-program (or/c string? number? char? boolean? symbol? kb-expr?)]
                  [name string?]
                  [active-range (or/c 'local 'global (cons/c exact-nonnegative-integer?
                                                             exact-nonnegative-integer?))]
                  [stx syntax?])
         vector?]{
 A nicer way to construct @tech{'keybinding-info} properties. Generates a vector using the given
 information that corresponds to the content of an @tech{'keybinding-info} property. The last item in the vector
 is a @racket[srcloc] built from the passed @racket[stx].
}


@subsection{Testing Utilities}

@defmodule[kb-base/test-utils]

@defproc[(get-kb-program-result [editor (is-a?/c racket:text<%>)]
                                [kb-program (or/c string? number? char? boolean? symbol? kb-expr?)])
         (values any/c
                 exact-nonnegative-integer?
                 string?)]{
 Passes the given @racket[editor] and @racket[kb-program] to the @racket[kb-base] interpreter and returns three results:
 the value of the operation in @racket[kb-program], the current position after running @racket[kb-program], and the current
 text in @racket[editor] after running @racket[kb-program].
}

@defproc[(test-kb-program [kb-program (or/c string? number? char? boolean? symbol? kb-expr?)]
                          [test-proc (-> any/c
                                         exact-nonnegative-integer?
                                         string?
                                         any/c)
                           (λ (val pos text)
                             (values val pos text))]
                          [#:setup-proc setup-proc (-> (is-a?/c racket:text<%>)
                                                       any/c)])
         any/c]{
 Creates an empty @racket[racket:text%] object and uses that as the editor for a call to @racket[get-kb-program-result] with the provided
 @racket[kb-program]. If @racket[setup-proc] is provided, runs @racket[setup-proc] to set up some initial editor state before calling
 @racket[get-kb-program-result]. After @racket[get-kb-program-result] has returned, passes the resulting three values to @racket[test-proc], which
 by default wraps them in @racket[values] and returns them. @racket[test-proc] is useful in combination with @racket[rackunit] to test properties
 about the editor after @racket[kb-program] has run.
}
