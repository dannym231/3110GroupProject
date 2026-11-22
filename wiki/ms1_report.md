MS1 Report
Team name: Team 21 - Team Happy
Members and NetIDs: lo225, clf89, drm329, jg2575
Repository URL: https://github.com/dannym231/3110GroupProject
Our team contract
What we hope to get out of the project: As a team we hope to finish and complete a playable Monopoly that we can actually use and play together in full. We want to put an emphasis on polish and playability rather than the number of features, to make sure the game itself runs smoothly and is a positive user experience. We also want to get real practice working in a team environment. Communicating, delegating assignments, and merging pull requests to create a final product together is an important skill that we hope to learn.
Communication and meetings: We will have all communication through a shared iMessage group chat. Members will be expected to check text messages daily, and everyone should expect a reply to their message within 24 hours of when they sent it. We will meet once a week at Olin Library to go over progress. If someone can not make it, it must be directly communicated in the group chat so everyone is aware of the situation and in the loop.
Decisions: We discuss decisions in the group chat or at our weekly meeting, and everyone gets a chance to weigh in. If we haven’t agreed within 48 hours, we take a majority vote, and the owner of the affected area breaks a tie. Interface changes that affect another member’s module need that member’s approval. 
Commitments and problems: We track who is doing what as GitHub issues assigned to each member. If someone expects to miss a deadline, they post in the group chat at least 48 hours ahead, and their backup owner helps or takes over part of the work. If someone misses work without notice, we reach out within a day and agree on a new plan together. If a problem continues after that, we ask the professor for team coaching.


 
Review and integration: We will make PRs and have another member review the PR before merging into the main branch before actually committing. Before committing a PR, they must make sure that their code passes all test cases and doesn’t cause major issues to the project. Everyone must pull after a PR is merged to stay up-to-date. We will keep us all updated over messages on what features that we make and merge, and what work we are taking on.
AI use: We will use AI for design suggestions, writing tests, finding syntax errors, and simplifying code. The reviewer can ask the author to walk through any part they don’t understand. Anyone who uses AI-generated code must review it, test it, and be able to explain it before opening a PR, and note in the PR description which parts were AI-assisted. If we disagree about how much AI to use for a task, the area owner decides, within the course’s AI policy.
Revisiting the contract: We will renew the contract each milestone, and anyone can request an earlier review. Changes need everyone’s agreement and go in the contract file in the repo. 
Our project proposal
Our main idea for our project is implementing Monopoly as a multiplayer game. We will use the standard Monopoly rules that exist in the classic game, but allow for users to run the program on their own device and be able to play with other player(s) locally over LAN.
A game starts with one player hosting and the others joining over LAN, or with everyone playing on a single machine. On each turn, a player rolls the dice and moves around the board. Landing on an unowned property gives them a choice to buy it or pass; landing on an owned property means paying rent, which rises when the owner holds a full color set or has built houses and hotels. Players can also draw Chance and Community Chest cards that move them or change their money, pay taxes, and be sent to jail. A player who can’t pay what they owe goes bankrupt, and the last player remaining wins. Because we’re prioritizing polish over feature count, trading and auctions are stretch goals we’ll add only once the core game runs smoothly.
For the architecture, we will use the Model-View-Controller design pattern to allow for easy integration, and making sure that we’re able to switch between using the program locally vs connecting over a server/hosting a server. Our model will contain the current state of the board and the player states, then our controller will take the user input (from both incoming connections and also locally) and actually apply them to the board, and finally our view will actually display the current board state in a clean fashion. 
For our view, we will use a terminal interface using the ANSITerminal, allowing the user to interact with their keyboard primarily. This interface will be clean and informative, allowing the user to gain all necessary information about the current board state, but not throw all information at the user by using menus to organize actions. This interface will call functions within our turn engine, which will control the current state of the game and thus update our model which contains the cards, player states, money, and more game information. We will also try to avoid cyclic dependencies by making sure we follow the model-view-controller and thus allow for modularity within this group project.
Our backup idea.
If Monopoly turns out to not be as feasible as we thought, we will pivot to making a course schedule planner. The idea is that a student enters the possible courses they would like to take into the program and the program finds all combinations of discussions and lectures that could fit together without time conflicts. We will add features so the student can customize their preferences, such as avoiding classes before 9 a.m. or keeping Thursday afternoons free for meetings. The student can then see all possible options and lock in their favorite schedule. To make sure the project is self-contained we will have the planner read the course and section times from a data file which we will write instead of depending on a live course catalog. This backup plan doesn’t share the main uncertainty in our Monopoly plan, since it requires no networking, and it uses no external libraries. Its parts also divide easily among team members. This means that we can feel confident falling back on this and executing if our Monopoly plan fails.




Initial responsibilities
Area
Primary owner
Backup owner
What the owner will deliver
Board and properties, and cards & money
Daniel McCance
Charlie Foley
Define the JSON board format and a loader that rejects malformed files. Track ownership and compute base rent, monopolies, and houses. Load the Chance and Community Chest decks and apply their effects. Write the .mli spec and OUnit Tests for loading and rent and connect the board look ups to the turn engine. 





Turn Engine
Charlie Foley
Daniel McCance
Manages the flow of each turn: player order, dice rolls with a seeded random generator so games can be replayed, moving players, passing Go, doubles and jail rules, and checking for the end of the game. Sends each landing to the right module (property purchase or rent, cards, taxes). Checked with OUnit tests that use fixed dice rolls to confirm movement and turn order, and connected to the interface through a .mli that the display and the other modules call.





Server Host & Client
Luca Occhipinti
Jaiden Grimminck
Make a server host on the user’s device that can accept other incoming actions to interact with the current board state, and also create an internal interface to allow for the controller to feed any outputs towards any other output server. To gain the current state of the board, we will just use intermittent polling to update the current game state (if we have time, we will potentially upgrade this to SSE and/or websockets).
We will write tests to confirm that it successfully can start a server, connect to a server, perform different legal actions as well as deny any potential illegal actions another user might take.



Interface and AI player
Jaiden Grimminck
Luca Occhipinti
Make a clean interface, taking in the current state of the board, and actually displaying using the ANSITerminal library to do so.
Additionally, add controls to allow the player to interact with the menus and perform actions, as well as menus to connect to the server and host a server on a device, using the turn controller functions to actually control the state of the game. Also Adds a computer player that makes buy and build decisions using simple rules.
Finally, writes unit tests to confirm the functionality of the board, and to make sure that it renders different game states correctly.





Where we’ll start
What we’ll implement first: Two players on one machine take turns rolling and moving around a board loaded from JSON, buying unowned properties and paying rent on owned ones. The terminal shows each player’s position, money, and properties after every turn.
An uncertainty we’ll investigate early and how we’ll do it: Whether two machines can play together over LAN. We’ll build a small Unix socket prototype where one machine hosts, another joins, and the two exchange and display dice rolls. If it proves unreliable, we’ll fall back to local play on one machine.


What each member will actively contribute to the initial implementation or investigation:
Daniel: Board loader, rent lookup, buying and rent payment, and tests.
Charlie: Game-state interface, dice, movement, and turn order.
Luca: LAN host/join prototype.
Jaiden: ANSITerminal board display and terminal input.




