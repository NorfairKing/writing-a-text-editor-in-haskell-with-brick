# Writing a text editor in Haskell with Brick

https://github.com/NorfairKing/writing-a-text-editor-in-haskell-with-brick

Preparations:

* On windows: Get the WSL
* install `stack`: `haskellstack.org`

# Why TUIs

- All in a terminal
- Less point-y click-y, more press-y button-y
- Looks old-schoo, very efficient
- Very light-weight

- Great to make something real and practical quickly


# Brick architecture

```
         Start
           |
           v
Event -> State -> Draw
  ^                |
  |                |
   \-    Brick  <-/
```


# Brick app

App

```
s: State
e: Custom events
n: Name of resources
```

``` haskell
data App s e n =
  { -- Draw the state
    appDraw :: s -> [Widget n]
  , [...]
  , -- Handle an event
    appHandleEvent :: s -> BrickEvent n e -> EventM n (Next s)
  , [...]
  , -- Style attributes
    appAttrMap :: s -> AttrMap
  }
```

# Cursor

**A cursor for an X is a data structure that represents both the entire X and also where you are looking with in X.**

. . .

Examples:

``` haskell
data ListCursor a = ListCursor [a] [a] -- Look between two elements
```

. . .

``` haskell
data NonEmptyListCursor a = NonEmptyListCursor [a] a [a] -- Look at an element
```

. . .

``` haskell
data TreeCursor a = [ ... ] -- It's complex
```


# Our cursor for a text editor

``` haskell
data TextFieldCursor
```

``` haskell
makeTextFieldCursor :: Text -> TextFieldCursor
rebuildTextFieldCursor :: TextFieldCursor -> Text

textFieldCursorSelectNextChar :: TextFieldCursor -> Maybe TextFieldCursor
textFieldCursorSelectPrevChar :: TextFieldCursor -> Maybe TextFieldCursor

textFieldCursorRemove :: TextFieldCursor -> Maybe (DeleteOrUpdate TextFieldCursor) 
```


# All relevant links

Starting-point: https://github.com/NorfairKing/tui-base.git
Following along: https://github.com/NorfairKing/writing-a-text-editor-in-haskell-with-brick

Cursor: https://hackage.haskell.org/package/cursor
Brick: https://hackage.haskell.org/package/brick
Drawing cursors: https://hackage.haskell.org/package/cursor-brick

Textfield Cursor: https://hackage.haskell.org/package/cursor-0.1.0.1/docs/Cursor-TextField.html
Drawing a Textfield cursor: https://hackage.haskell.org/package/cursor-brick-0.1.0.0/docs/Cursor-Brick-TextField.html

