# Tasks
- Check when we beat train speed record for first time. Shouldn't be for any speed.
- Check that active_station only works for 20 years per station
- Move block_map and track_graph to player
- First delivery: animation
- Train service inaugurated: no animation
  - Coal
  - service
  - inaugurated!
  - Coal picked up from Grafton Junction
  - Coal may be delivered to
  -          Grafton Woods
- Can't rebuild bridge under repair
- Use value manipulation to get to unseen overlays
- Missing:
  - Rate war info
  - driven out of town animation
  - Small things here and there (look through code)
- Straddle station updates so they're smaller and they don't seem as coordinated, like in game.
- Allow upgrading station while keeping station data
- Train records, first deliveries
- Hook up fiscal_period_end stuff
- Handle year end messages
- Stock broker: print ownership of AI stock
- Way we changed random things (e.g. track maintenance) isn't right
  - Need to scale up to more track than 100% of original game
- Check default state of development_map and block_map
- What happens when we delete a station and a train is heading there?
- Check computation of wait time
- F10 for survey
- cursor
- Refactor modal_menu: give it a 4th last type, move to its own module
  - Also the model_menu handler
- Check /2 for different things with money (e.g. pay)
- Check if we need to loop in _update_train (print out if we ever do multiple updates)
- Update_train: handle freight class priority movement
  - Data structure sorted by priority?
- Bug:
  - AIs don't seem to get created.
  - 2 stations: 1 station not being delivered to.
  - B and P in priority shipment are off left by one
  - Fancy paper: press any key is too high
  - Station screen has name in wrong place
  - Can't upgrade station
  - Animation: old timing issues
  - Delivery + priority pickup: no delivery message
  - Can't change light from f2/f3
  - Need better feedback for clicking on menu
  - Create a train with no station: msgbox to build a station first
  - New train doesn't pick up stuff from starting station
    - this may be normal
  - Clicking on map during train creation crashes
  - First engines use one-chimney img. Check in code.
  - Where does smoke originate? Check in code.
  - Smoke seems to go in different direction from game
  - joining a track from another track either refuses or crashes
  - Going into negative should be red money
