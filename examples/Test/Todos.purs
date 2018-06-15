module Test.Todos where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, footer, header, input, label, li, p', section, span, strong, text, ul)
import Concur.React.Props (_type, checked, classList, className, defaultValue, href, name, onChange, onClick, onDoubleClick, placeholder, onHandleEnter)
import Control.Alternative (empty)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State.Trans (StateT, get, lift, put, runStateT)
import Control.MultiAlternative (orr)
import Data.Array (cons, drop, filter, intercalate, length, null, range, take, zip)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(..), fst, snd)
import Prelude hiding (div)

todosWidget :: forall a. Widget HTML a
todosWidget = fst <$> runStateT widgetTodos startEntries

type Entry =
    { desc      :: String
    , completed :: Boolean
    }

data EntriesVisibility = All | Active | Completed
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

widgetTodos :: forall a. EntriesWidget a
widgetTodos = forever $ div [ className "todomvc-wrapper" ]
  [ section [className "todoapp"] [widgetInput, widgetEntries, widgetControls]
  , lift $ footer [ className "info" ]
        [ p' [text "Double-click to edit a todo"]
        , p'
            [ text "Written by "
            , a [ href "https://github.com/ajnsit" ] [ text "Anupam Jain" ]
            ]
        , p'
            [ text "Part of "
            , a [ href "https://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]
  ]

widgetInput :: EntriesWidget Unit
widgetInput = header [ className "header" ]$ pure do
  elist <- get
  s <- lift $ input [onHandleEnter, className "new-todo", placeholder "What needs to be done?", name "newTodo"] []
  put $ elist { entries = cons {desc:s, completed:false} elist.entries }

opt :: String -> Boolean -> Maybe String
opt s p = if p then Just s else Nothing

widgetEntries :: EntriesWidget Unit
widgetEntries = do
  elist <- get
  section [ classList [Just "main", opt "hidden" (null elist.entries)] ]
    [ lift (allCompletedToggle (allCompleted elist)) >>= put <<< flip markAllComplete elist
    , ul [ className "todo-list" ] [elistToEntriesListWidget elist]
    ]
  where
    addChecked p l = cons (checked p) l
    allCompletedToggle c = input [_type "checkbox", not c <$ onChange, className "toggle-all", name "toggle", checked c] []
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
        [ div [ className "view" ] $
            [ do
                c <- input [_type "checkbox", checked completed, not completed <$ onChange, className "toggle", name "toggle"] []
                pure $ Right $ Just $ todo {completed = c}
            , Left (not editing) <$ label [onDoubleClick] [text desc]
            , Right Nothing <$ button [onClick, className "destroy"] []
            ]
        , if editing
            then (\desc' -> Right $ Just $ todo { desc = desc' }) <$> input [onHandleEnter, className "edit", name "title", defaultValue desc] []
            else empty
        ]
    completed = todo.completed
    desc = todo.desc
    addChecked p l = cons (checked p) l

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
      [ className "todo-count" ]
      [ strong [] [ lift $ text (show $ entriesLeft elist) ]
      , lift $ text $ (if entriesLeft elist == 1 then " item" else " items") <> " left"
      ]

widgetControlsFilters :: EntriesWidget Unit
widgetControlsFilters = ul [ className "filters" ] $ pure $ do
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
    visibilitySwap uri label visibility actualVisibility = li [visibility <$ onClick] [link]
      where
        link = a [ href uri, classList [opt "selected" $ visibility == actualVisibility] ] [text label]

widgetControlsClear :: EntriesWidget Unit
widgetControlsClear = do
  elist <- get
  _ <- lift $ button [onClick, classList [Just "clear-completed", opt "hidden" (entriesCompleted elist == 0)]] [text ("Clear completed (" <> show (entriesCompleted elist) <> ")")]
  put $ elist { entries = filter (not <<< _.completed) elist.entries }
