" Vim syntax File
" Language: Structural Spec
" Author: Muralidaran Vijayaraghavan <vmurali@csail.mit.edu>
" Original Author: Richard Uhler <ruhler@csail.mit.edu>

" Case Sensitivity
" From reference guide (section 2.2):
"   Identifiers are case-sensitive: glurph, gluRph, and Glurph are three
"   distinct identifiers.
syn case match

" Keywords
"   keywords in the XXXGroup have yet to be classified
syn keyword StructuralSpec Action action endaction
syn keyword StructuralSpec bit Integer Bit Int UInt
syn keyword StructuralSpec clocked_by reset_by
syn keyword StructuralSpec default default_clock default_reset
syn keyword StructuralSpec Input Output
syn keyword StructuralSpec InputPulse OutputPulse
syn keyword StructuralSpec ConditionalInput ConditionalOutput
syn keyword StructuralSpec partinst include
syn keyword StructuralSpec let match matches
syn keyword StructuralSpec type numeric tagged
syn keyword StructuralSpec valueOf valueof void
syn keyword StructuralSpec mkConnection synthesize

syn keyword TypeClass Bits Eq Literal RealLiteral Arith Ord
syn keyword TypeClass Bounded Bitwise BitReduction BitExtend

syn keyword Conditional if else case endcase
syn keyword Repeat for
syn keyword Statement begin end
syn keyword Structure struct unition enum
syn keyword Typedef typedef
syn keyword Statement function endfunction
syn keyword Statement partition endpartition
syn keyword Statement port endport
syn keyword Statement atomic endatomic


" Single Line Comments
"   They start with //.
syn match Comment  =//.*=

" Block comments. /* ... */
syntax region Comment start="/\*" end="\*/"

" highlight links
hi def link Comment Comment
hi def link Conditional Conditional
hi def link Repeat Repeat
hi def link Statement Statement
hi def link Structure Structure
hi def link Typedef Typedef
hi def link StructuralSpec Special

" TODO: should this be in the syntax file?
set errorformat+=%EError:\ \"%f\"\\,\ line\ %l\\,\ column\ %c:\ (%.%#),%Z%m
set errorformat+=%WWarning:\ \"%f\"\\,\ line\ %l\\,\ column\ %c:\ (%.%#),%Z%m

" TODO
"   - Classify XXXGroup keywords into their appropriate groups.
"   - Highlight the #(...) notation somehow
"   - In // comment, ENTER, should keep us in // comment

