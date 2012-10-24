import Graphics.UI.Gtk
import Control.Monad.IO.Class

main = do
   initGUI
   window <- windowNew
   button <- buttonNewWithLabel "Inc"
   containerAdd window button

   widgetShowAll window
   window `on` deleteEvent $ tryEvent (liftIO mainQuit)
   mainGUI
