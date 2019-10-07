# Coeus Libraries

## `ast`

This library contains the Coeus AST definition and the corresponding parsing
logic. The are two variants of Coeus ASTs: one defined in `Ast.Coeus` and the
other one in `Ast.Ecoeus`. The `Ecoeus` variant has each AST node extended with
information such as variable types and subtree sizes, which makes it convenient
for subsequent operations to obtain those information. However, translating from
Coeus AST to ECoeus AST requires variable resolution and type checking. This is
not possible in the `ast` library and instead one should look for the
corresponding APIs in the `frontend` library.
	
## `cparser`

All files that are extracted from Compcert parser.

## `ctranslator`

Compcert-C-to-Coeus translation logic.

## `frontend`

Contains various AST simplifications and transformations.

## `prover`

This is where all proof rules / tactics are implemented. There's also
`Prover.StateEncoder` and `Prover.ActionEncoder`, where we turn proof states and
proof actions into feature vectors that can be recognized by our learning
engine.

Each proof rule (or tactic) is essentially a function from a prover config plus
one prover state to another prover state(see `prover/rule.ml`). Each rule can be
applicable to a given state or not: the reason why it is not applicable can
either be that the rule doesn't handle the corresponding AST node, or that the
current prover state has exceeded the resource limit specified in the prover
config. For a list of all candidate proof rules used in the prover, see
`prover/rules.ml`.

A prover state is essentially a list of proof goals, where each goal specifies
the AST nodes that need to be processed next, the current pre&post verification
conditions, and a bunch of other metadata that are related to conflict
analysis. A goal can be empty, where we have exhausted the AST nodes that need
to be processed. For empty goals, we use `ProverState.resolve` to remove them,
merge their corresponding verification conditions to a buffer
`ProverState.verif_state`, and activate another proof goal. There can be only
one active goal at a given point -- inactive goals will be kept in the back
until the current active goal gets resolved. Proof goals are either given
(i.e. from the input specification), or they can be added when certain proof
rule is applied. For example, loop sync rules would usually create a goal which
states that loop invariant is inductive, and another goal which states that loop
postcondition would imply the final postcondition. 


## `searcher`

This is where all the proof search strategies gets implemented.

In general, there are two types of search strategies we support: linear
strategies, and tree strategies. A linear strategy looks at one prover state,
and pick a single rule to apply next. This kind of strategy does not support
backtracking. A tree strategy maintains an additinal workset which keeps track
of where to backtrack, and it supports selecting / prioritizing multiple
candidate rules for a given state.

Conflict analysis is also implemented here, as it is only meaningful for
exhaustive tree strategies. The basic idea is to keep a cache of conflict specs,
where each spec keeps track of which rule should be avoided at which proof
step. Every time we want to pick a rule, we check if picking the rule would make
us run into the same situations that are stored in the conflict cache, and
backtrack immediately if we get a cache hit. Implementation of such analysis is
rather janky, as the feature was mostly added as an afterthought.

## `server`

This library contains logic that relate to communication with our training
engine. We use plain-text s-expression as our IPC protocol, and the message
format can be found in `server/protocolMessage`. The client can query for
general info such as training example count, feature vector size, benchmark
location, etc. It can also instruct the server to pick a benchmark and start a
rollout, after which the server would send back the current prover state plus
all available actions. The client can continue the process by repeatedly
specifying the next action to take, until all proof goals are resolved and the
server would send back a reward, indicating whether the rollout is successful or not.

## `solver`

This is where we send high-level verification conditions to our solver backend,
which gets forked as another process. Currently there are two CHC solvers we
use: one is our own hand-crafted houdini solver (see `Solver.Houdini`), and the
other one is the spacer solver bundled with Z3. Both backends have their own
pros&cons, so by default we try them both: houdini first (as it is usually
faster), and then spacer. Trying them both concurrently would be more desirable
but I have not figured out an easy way to implement that :(

## `util`

It is universally acknowledged that every project grown to a certain size must
contain a library called "util" or "etc" which holds code that is useful
everywhere but also hard to categorize into any other libraries.

## `verifier`

Defines the data format for Coeus' high-level verification conditions, and some
transformation / optimization passes on those verification conditions.
