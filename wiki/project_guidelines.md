# MS2: WIP

CS 3110 · TEAM PROJECT

[MS1 · Plan](https://canvas.cornell.edu/courses/90271/pages/ms1-startup) · [MS2 · Build](https://canvas.cornell.edu/courses/90271/pages/ms2-wip) · [MS3 · Release](https://canvas.cornell.edu/courses/90271/pages/ms3-release)

For MS2, get a small part of your program working, agree on how the rest will fit together, and plan how you’ll validate it. Use this [report template](https://canvas.cornell.edu/courses/90271/pages/ms2-report-template) to write your MS2 report. Aim for **about six pages** total. Start with your team name, members and NetIDs, project title, and a clickable repository URL. The sections below follow the report template.

Most development still lies ahead during MS3. A *modest* amount of useful, connected functionality is enough for full progress credit on MS2. Your grader will assess your progress, design, and validation by reading the report and selected code and tests.

## 01 What works so far

Begin with a brief description of the project you’re aiming for and any changes since MS1. At MS1, you chose a first piece to build and planned how everyone would help. For MS2, put that plan into practice: by the time you’re done with MS2, a user should be able to use a small part of your program, and your team should have learned from building it together. It’s okay if your plan evolves from what you wrote in MS1, as long as everyone has a clear role and the work is connected.

Explain what someone can do with the current version, which components work together, and what remains to be built. Think back to the examples in MS1. For the game, someone can now see the board, make a legal move, and see the board change: the input handling, game rules, and display work together. For the practice tool, someone can now load a few questions from a file and answer them: the file reader, question representation, and interaction work together. In each case, you’ve turned the first step you planned into something someone can use.

**Get a useful part of your program working across its components.** Choose something that exercises your project’s own behavior: opening an empty window or printing a welcome message won’t tell you much about whether the pieces fit. A collection of functions that teammates have tested separately also leaves that question unanswered. Point to the implementation and tests that support your account, using paths within the submitted ZIP.

## 02 Your design

Explain the main modules, what each takes care of, and how they cooperate. A small diagram can help us see the relationships, but a list of filenames by itself won’t explain the design.

Describe the important interfaces, including the types and operations other modules will rely on. You can show a short signature in the report or point to an `.mli` file. Distinguish interfaces you’ve implemented from ones you’re proposing. You don’t need to fill the repository with empty modules just to show that you’ve planned them.

Write down your design while you’re making these decisions together. The point is to give your teammates something they can use. If one person is writing a parser and another is using its output, they should be able to agree on the types and error behavior before both implementations are finished.

Explain a few choices that matter for your project. Perhaps the game rules should be separate from drawing the board, so you can test moves without opening a window. Perhaps a budget module should hide its representation, so other code can’t accidentally update a total without recording the expense.

Then explain what the early implementation taught you about the design, what still needs investigation, and how you’ll investigate it. If your project depends on something unfamiliar, try it early. Can you actually load the data you were planning to use? Can your graphics library handle the input you need to let someone make a move? Finding out now gives you time to change course. Include what you learned, especially if it changes what you’ll build next.

## 03 Your responsibilities and contributions

Update your responsibility table to explain what each person has done so far and what they will deliver next. For each major area, name a primary owner and a backup owner. Include paths to the code, tests, investigations, or documents that support your account.

Be specific enough that teammates can tell what they’re relying on. “Sam: backend” leaves a lot to guess. “Sam: load and save budgets, including malformed-file errors and persistence tests” gives the rest of the team something to plan around. One person can own several related modules, and people can work together on a component.

For each backup owner, describe how they’ve become familiar with the work and what they’ll do next. They might review the interface with the primary owner, work through tests, or help connect components.

**Every member should have actively contributed by MS2.** Implementation, investigation, testing, review, and integration can all be substantive contributions. If people worked together, explain each person’s part. Watching someone else work or agreeing with their choices is not enough. Describe any missed or reassigned responsibilities so we can understand the current state of the team.

## 04 Your remaining work

Plan the final stretch of development together. Which pieces need to come first? When will you connect the next ones? Write a short account of that plan, including dependencies and concrete work for every member.

## 05 Your validation

For each important part of your project’s scope, explain what should happen and how you’ll know the expected answers are right. Think about how a mistake would show up. A save-and-load test might reveal lost data; a test of an illegal move might reveal that your game can enter an impossible state. Choose checks that would help you find the mistakes you’re worried about. You can discuss related features together.

Describe the checks you’ve completed, why you chose them, and their actual results, including any failures. Point to the tests or other evidence in your submission. **Begin the automated test suite now**, with meaningful tests for behavior you’ve already implemented.

Then describe the checks you plan for behavior you haven’t built yet. Label the difference clearly so we can tell what you’ve checked and what you still intend to check. Make the plan concrete: “We’ll use OUnit” names a tool, but leaves the actual checks undecided. For example, one part of a budget planner’s validation discussion might read:

> **Category totals.** Our automated tests cover an empty budget and multiple expenses in one category; all pass on this submission (`test/budget_test.ml`). We worked out the expected totals by hand. Next we’ll test expenses spread across categories and the effect of deleting an expense once deletion is implemented. We also plan a manual check that changing a category filter updates the displayed total. That interface isn’t built yet.

For manual checks, describe the steps and what someone should expect to see; report what you observed for checks already completed. Decide which parts need a person to look, listen, or interact and which you can check automatically. Even a graphical application has behavior you can test without its display. If a planned component seems impossible to test automatically, revisit how you’ve divided its responsibilities!

As your code grows, Bisect coverage can help you spot places your tests never reach. Use that information to decide where another test would help; there’s no required coverage percentage. Build checking into the work as you go: add tests alongside new behavior, run them when you connect components, and record problems while they’re still fresh in your mind.

**Make your automated test suite runnable with `dune test`.** Give any additional commands needed for other checks, with enough detail that someone else could run them.

## 06 AI use and review

Include a short account of how your team has used AI so far and how you’ve reviewed and checked the work it helped produce. Give a concrete example where you can. Perhaps it suggested an interface, but the team changed the error type after considering what callers would need to know. Or it helped write tests whose expected answers you checked by hand. If you haven’t used AI, say so.

Then agree on how you’ll handle AI-assisted work as more code arrives. Who will review it? How will the person responsible for a component establish that they understand it and that it works? If you ask AI for tests, how will you check their expected answers independently of the implementation?

Use the AI agreements in your team contract, and revise them together if your experience suggests a change. Keep the source acknowledgements required by course policies in `AUTHORS.md`.

## 07 Submit your work

Upload these two files to the MS2 assignment on Gradescope by its posted deadline. **Only one person submits!** The submitter then adds all the other team members following [these instructions](https://guides.gradescope.com/hc/en-us/articles/21863861823373-Adding-Group-Members-to-a-Submission).

| File | What to include |
| --- | --- |
| `source.zip` | The current source, tests, build configuration, required small data files and assets, `AUTHORS.md`, and supporting files mentioned in the report. |
| `MS2Report.pdf` | Your report using the template above, with your team name, members and NetIDs, and a clickable repository URL. Keep the repository access arrangement you set up for MS1. |

**We’ll grade the code in the ZIP**, so you can keep developing your repository while we review MS2. Include all source files, and leave out generated build output, local OPAM switches, and Git metadata. For required data too large to upload, include a README explaining where to get the exact data your program uses.

## 08 Give each other feedback

Within **48 hours after submitting MS2**, each person should fill out the [preliminary peer-evaluation form](https://canvas.cornell.edu/courses/90271/files/16379656?wrap=1) for every teammate, including themselves. At your next team meeting, give each person the forms about them. Allow time to read and think about the feedback, then invite discussion at the following meeting. These forms stay within your team; don’t submit them to staff.

The form asks about team citizenship: how you’re working together. This is a chance to give honest feedback while there’s still time to act on it. If something needs to change, agree on a practical next step and update your team contract if useful. You’ll use the same citizenship criteria for the final peer evaluation.

## 09 How we’ll grade it

| Category | Weight | What we’re looking for |
| --- | --- | --- |
| Working progress | 20% | A useful part of the program connects meaningful components. Your account of what works so far is supported by the submitted implementation and tests, and clearly distinguishes completed from planned work. |
| Design and responsibilities | 50% | A sensible modular design with useful interface details, primary and backup owners, concrete contributions from every member so far, and a workable plan involving everyone in building and connecting the remaining pieces. |
| Validation | 30% | Concrete checks planned for the important parts of the final project, meaningful automated tests and actual results for existing behavior, and an account of how you review and check AI-assisted work. |
