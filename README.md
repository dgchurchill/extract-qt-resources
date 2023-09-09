# Extract Qt Resources

Extract resources from a Qt binary.

## Examples

### List resources
```
$ ExtractQtResources.exe list Synergy
res/
res/res/
res/res/lang/
res/res/lang/gui_sq-AL.qm
res/res/lang/gui_ca-AD.qm
res/res/lang/gui_ja-JP.qm
...
```

### Extract resources

Resources will be written to the working directory.

:warning: No sanitization of file / directory names is done and files will be overwritten without warning,
so check with `list` first that the file / directory names look safe.

```
$ ExtractQtResources.exe extract Synergy
res/
res/res/
res/res/lang/
res/res/lang/gui_sq-AL.qm
res/res/lang/gui_ca-AD.qm
res/res/lang/gui_ja-JP.qm
...
```
