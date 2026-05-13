# purescript-report

First intended to visualize game statistics, now supports many things similar to `org-mode`.

* Load statistics from `json`, `text` or `dhall`;
    * Or define them with strict `PureScript` types;
* Edit all the values;
* Navigate groups and subjects easily;
* Regroup items by tag and have statistics for it;
* Store the state in the URL;
* Export to `org`, `json`, `dhall`, ...
* many more features...

In works / Planned:

* DONE Better keyboard navigation for editing values;
* Ability to add groups and values right in the UI and store them;
* DOING Load the data from `org-mode`;
* Dark mode;
* DONE Support `smos` format; [https://smos.online/];
* Terminal User Interface;
* Improve loading from other formats to not obligatorily be backed by types;
* Better examples, like tracking TV series or music discorgraphies etc...;

![Screen 1](/screens/screen-b01.png)
![Screen 2](/screens/screen-b02.png)
![Screen 3](/screens/screen-b03.png)
![Screen 4](/screens/screen-b04.png)
![Screen 5](/screens/screen-a01.png)
![Screen 6](/screens/screen-a05.png)
![Screen 7](/screens/screen-a07.png)
![Screen 8](/screens/screen-a08.png)
![Screen 9](/screens/screen-a09.png)

Keyboard:

* `<arrows>` when focused on report panel: select subjects, groups, items, decorators, ...
    * notice that `<arrow-up>` on groups navigates between groups, to go on the items-level again, press `<arrow-down>`
    * sometimes scroll isn't appropriately positioned according to selection, it is in the works
* `e` to edit selected item name or decorator
    * `<enter>` to accept
    * `<escape>` to cancel
* `r` to toggle read-only mode
* `s` to toggle if subject navigation is pinned
* `g` to toggle if groups navigation is pinned
* `p` to toggle progress plates on groups
* `x` to toggle `REP` export
* `f` to toggle `TEXT` export
* `q` to exit export
* `d` to toggle debug mode (show selection)


CLI:

```
sh ./run-cli.sh --help
sh ./run-cli.sh -i ./some/file.rep
sh ./run-cli.sh --file-in ./some/file.rep
sh ./run-cli.sh -i ./some/file.rep -f TEXT
sh ./run-cli.sh --convert -i ./some/file.dhall -f dhall -o ./some/file.json -t json
sh ./run-cli.sh -i ./test/games-samples/AstralChain.rep
sh ./run-cli.sh -i ./test/games-samples/AstralChain.dhall --from dhall
sh ./run-cli.sh -i ./test/games-samples/AstralChain.dhall --from dhall --to rep -o ./test/games-samples/AstralChain.rep
sh ./run-cli.sh -i ./test/games-samples/AstralChain.rep --from rep --to text
```