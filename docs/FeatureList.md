# Group 8 - Feature List
## Mandatory features
- `SEL` input shifted to different edge on `Mux` symbol 

- Symbol flip and rotation functionality 

    Flip and Rotate functionality includes labels and symbol titles. Functionality is binded to `Ctrl + R` and `Ctrl + F`. 

- Autoroute and partial autoroute - implemented between ports oriented in all directions (rotation and flip).

- Something fully working better than current jumps

## Additional features
- Changing mouse icons 

    When cursor hovers over draggable items e.g. Symbols and Wires, cursor becomes a hand symbol. When dragging, cursor becomes a clutched hand.    

- Directional arrows on Bus Extract, Bus Split

    These symbols, when rotated or flipped, are hard to differentiate thus directional arrows are added to aid in this process.

- Save function preserves rotation and flip 

    Original `Save` functionality did not preserve symbol rotation and flips. Code is modified to store the rotation and flip information of symbols. 

- Improvements on symbol UI
    - Larger symbols to account for symbol rotations
    - Removed `clk` label 
    - New names and labels for Gate components e.g. G3 -> AND3, G5 -> XOR5

- New components
  - `Mux4`: 
  - `Sign_Extend`: 
  - `UnSign_Extend`:
  - `Bus_Extract`:
  - 'Shift Register'

- Defaulting from partial routing to autorouting simplified and improved.

- `Crtl + A` to re-autoroute a Wire, and/or all wires connected to a Symbol (depending on what is selected before the KeyPress).

- Better removal of redundant segments - working along the whole wire, not just around the ports.

- Stickiness between parallel wires in a same net.
