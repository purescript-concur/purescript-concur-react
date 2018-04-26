module Test.Todos where

import Prelude hiding (div)

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, div, footer, h1, header, li, p', section, span, strong, text, ul)
import Concur.React.Widgets (displayButton, displayDoubleClickHandler, elEvent, textInput, wrapClickHandler, checkbox)
import Control.Alternative (empty)
import Control.Monad.IOSync (runIOSync')
import Control.Monad.State.Trans (StateT, get, lift, put, runStateT)
import Control.MultiAlternative (orr)
import Data.Array (concatMap, cons, drop, filter, intercalate, length, null, range, take, zip)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst, snd)
import React.DOM as D
import React.DOM.Props as P

todosWidget :: forall a. Widget HTML a
todosWidget = fst <$> runStateT widgetTodos startEntries

type Entry =
    { desc      :: String
    , completed :: Boolean
    }

data EntriesVisibility
  = All
  | Active
  | Completed
derive instance eqEntriesVisibility :: Eq EntriesVisibility

type EntriesList =
  { visibility :: EntriesVisibility
  , entries    :: Array Entry
  }

startEntries :: EntriesList
startEntries = { visibility: All, entries: [] }

entriesCompleted :: EntriesList -> Int
entriesCompleted = length <<< filter _.completed <<< _.entries

entriesLeft :: EntriesList -> Int
entriesLeft elist = length elist.entries - entriesCompleted elist

type EntriesWidget a = StateT EntriesList (Widget HTML) a

forever :: forall a b m. Monad m => m a -> m b
forever m = m >>= \_ -> forever m

widgetTodos :: forall a. EntriesWidget a
widgetTodos = forever $ div [ P.className "todomvc-wrapper" ]
  [ section [P.className "todoapp"] [widgetInput, widgetEntries, widgetControls]
  , lift $ footer [ P.className "info" ]
        [ p' [text "Double-click to edit a todo"]
        , p'
            [ text "Written by "
            , a [ P.href "https://github.com/ajnsit" ] [ text "Anupam Jain" ]
            ]
        , p'
            [ text "Part of "
            , a [ P.href "https://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]
  ]

widgetInput :: EntriesWidget Unit
widgetInput = header [ P.className "header" ]
  [ h1 [] [lift $ text "todos"]
  , do
      elist <- get
      s <- lift $ textInput [P.className "new-todo", P.placeholder "What needs to be done?", P.name "newTodo"] ""
      put $ elist { entries = cons {desc:s, completed:false} elist.entries }
  ]

classList :: Array (Maybe String) -> P.Props
classList = P.className <<< intercalate " " <<< concatMap (maybe [] (\s -> [s]))

opt :: String -> Boolean -> Maybe String
opt s p = if p then Just s else Nothing

widgetEntries :: EntriesWidget Unit
widgetEntries = do
  elist <- get
  section [ classList [Just "main", opt "hidden" (null elist.entries)] ]
    [ lift (allCompletedToggle (allCompleted elist)) >>= put <<< flip markAllComplete elist
    , ul [ P.className "todo-list" ] [elistToEntriesListWidget elist]
    ]
  where
    addChecked p l = cons (P.checked p) l
    allCompletedToggle checked = checkbox [P.className "toggle-all", P.name "toggle"] checked
    elistToEntriesListWidget elist = orr $ map (numberedEntryToWidget elist) $ filter (isEntryVisible elist.visibility <<< snd) $ zip (range 0 (length elist.entries)) elist.entries
    numberedEntryToWidget elist (Tuple i e) = do
      let l = elist.entries
      me <- lift $ widgetEntry e
      put $ elist { entries = case me of
                      Nothing -> take i l <> drop (i+1) l
                      Just e' -> take i l <> [e'] <> drop (i+1) l
                  }
    isEntryVisible Completed = _.completed
    isEntryVisible Active    = not <<< _.completed
    isEntryVisible All       = const true
    allCompleted elist = entriesLeft elist <= 0
    markAllComplete v elist = elist { entries = map (\e -> e {completed = v}) elist.entries }


widgetEntry :: Entry -> Widget HTML (Maybe Entry)
widgetEntry todo = go false
  where
    go editing = ego editing >>= either go pure
    ego editing = do
      li [ classList [ opt "completed" completed, opt "editing" editing ] ] $
        [ div [ P.className "view" ] $
            [ do
                c <- checkbox [P.className "toggle", P.name "toggle"] completed
                pure $ Right $ Just $ todo {completed = c}
            , Left (not editing) <$ displayDoubleClickHandler D.label [] (text desc)
            , Right Nothing <$ displayButton [P.className "destroy"] (text "")
            ]
        , if editing
            then map (\desc' -> Right $ Just $ todo { desc = desc' }) $
                   textInput [P.className "edit", P.name "title", P.value desc] ""
            else empty
        ]
    completed = todo.completed
    desc = todo.desc
    addChecked p l = cons (P.checked p) l
    wrapDoubleClick e props w = elEvent (\h -> P.onDoubleClick (const (runIOSync' (h unit)))) e props w

widgetControls :: EntriesWidget Unit
widgetControls = do
  elist <- get
  footer
      [ classList [Just "footer", opt "hidden" (null elist.entries)] ]
      [ widgetControlsCount , widgetControlsFilters , widgetControlsClear ]

widgetControlsCount :: forall a. EntriesWidget a
widgetControlsCount = do
  elist <- get
  span
      [ P.className "todo-count" ]
      [ strong [] [ lift $ text (show $ entriesLeft elist) ]
      , lift $ text $ (if entriesLeft elist == 1 then " item" else " items") <> " left"
      ]

widgetControlsFilters :: EntriesWidget Unit
widgetControlsFilters = ul [ P.className "filters" ] $ pure $ do
  elist <- get
  newVisibility <- lift $ visibilityWidget elist.visibility
  put $ elist { visibility = newVisibility }
  where
    visibilityWidget :: EntriesVisibility -> Widget HTML EntriesVisibility
    visibilityWidget visibility = intercalate (text " ") $
      [ visibilitySwap "#/" "All" All visibility
      , visibilitySwap "#/active" "Active" Active visibility
      , visibilitySwap "#/completed" "Completed" Completed visibility
      ]
    visibilitySwap :: String -> String -> EntriesVisibility -> EntriesVisibility -> Widget HTML EntriesVisibility
    visibilitySwap uri label visibility actualVisibility = wrapClickHandler D.li [] (const visibility) link
      where
        link = a [ P.href uri, classList [opt "selected" $ visibility == actualVisibility] ] [text label]

widgetControlsClear :: EntriesWidget Unit
widgetControlsClear = do
  elist <- get
  _ <- lift $ wrapClickHandler D.button
      [ classList [Just "clear-completed", opt "hidden" (entriesCompleted elist == 0)] ]
      (const unit)
      (text ("Clear completed (" <> show (entriesCompleted elist) <> ")"))
  put $ elist { entries = filter (not <<< _.completed) elist.entries }
