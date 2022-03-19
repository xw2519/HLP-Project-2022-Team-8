# Central group work extension tracker

Tracks what extensions teammates are working on and what the current extensions under work are. 

# Templates 

## Proposing Extensions 

**General Notes**: 

When researching possible extensions, you can keep a list of possible extensions that would be interesting. 

The priority would be ones that would not take long and does not have too many dependencies i.e. ones that you could implement in your sections with minimum changes to other parts.

Modify this README.md file under [Other possible extensions](#other-possible-extensions) to contain:
- [*Name*]: *Extension description*

## Current Extension Being Worked On 

**General Notes**: 

If you are going to work on an extension, create a new branch from master **with the branch name format:** [*shortcode*: *short description*]

Modify this README.md file under [Current extensions being worked on](#current-extensions-being-worked-on) to contain:
#### *Name*
- *Extension being worked on*
  - Dependencies:
    - *Section*: *Name*
    - E.g. Symbols (Section 1): Xin
  - Files changes:
    - *Possible file changes*
    - E.g. Renderer.fs  


----
# Mandatory extensions 

## Symbols 

## BusWire 
  
----
# Other possible extensions

## Symbols 
- [Xin] Adding *clk* shape to custom generated symbols 
- [Xin] Automatic scaling of shapes based on the number of ports 
- [Xin] Components are renamed in the properties tab when their name is numeric
- [Xin] Changing mouse icons
- [Xin] Zoom to fit (Easy to do)

## BusWires

---- 
# Current extensions being worked on

## Symbols 

### **Leo**

### **Xin** 
- New shapes for new symbols
- Changing mouse icons
- Adding *clk* shape to custom generated symbols 

## BusWires

### **Bertil** 

### **Dom**

### **Tanguy** 

---- 
# Completed extensions

## Symbols 
- New names and labels for Gate components. (e.g. G3 -> AND3, G5 -> XOR5)
- Implemented Flip for ports
- Some reasonably competent way to flip components (possibly rotate them). 
- MUXes with select on bottom edge.
- Flips/rotates should keep port text legends nice 
- Symbol labels and component legends "adder" should always be horizontal
- Symbols resized to reduce squashed look 
- Removed *clk* label since symbol is already present 
- Added direction arrows on *Merge Bus* and *Split Bus* to aid differentiation when rotating 

## BusWires
- Something fully working better than current jumps. 
- Autoroute and partial autoroute - functionality as current Issie or better - but implemented between ports oriented in all directions. 
- Something fully working better than current jumps.
