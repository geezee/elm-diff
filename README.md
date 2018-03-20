# elm-diff

This is a library to generate diffs. The current algorithm is a brute-force search
yet greedy. The algorithm favors deletes over inserts, and if there's a match
it will keep it.

It's still under development.

## Example

The diff between the strings `Hello world` and `HelL0, worLd!` is

```
Delete 'l' at 3
Insert 'L' at 3
Delete 'o' at 4
Insert '0' at 4
Insert ',' at 5
Delete 'l' at 9
Insert 'L' at 10
Insert '!' at 12
```

There is also a function to transform a string into another using a diff.

### See it in action

clone the package and run `elm reactor` and check it in the browser.
