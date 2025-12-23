# LUNLAND - Lunar Lander Simulation

A classic text-based lunar landing simulation game written in ANSI FORTRAN 77, featuring enhanced ASCII graphics, realistic physics, and three distinct landing outcomes.

[![FORTRAN 77](https://img.shields.io/badge/FORTRAN-77-blue.svg)](https://en.wikipedia.org/wiki/Fortran)
[![License](https://img.shields.io/badge/license-Educational-green.svg)](LICENSE)

## 📖 Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Game Physics](#game-physics)
- [Landing Outcomes](#landing-outcomes)
- [Installation & Compilation](#installation--compilation)
- [How to Play](#how-to-play)
- [Code Structure](#code-structure)
- [Customization Guide](#customization-guide)
- [ASCII Graphics Reference](#ascii-graphics-reference)
- [Technical Details](#technical-details)
- [Educational Value](#educational-value)
- [Contributing](#contributing)

---

## 🌙 Overview

**LUNLAND** is a faithful recreation of the classic 1969 lunar lander simulation, enhanced with detailed ASCII graphics. You are the pilot of a lunar module attempting to land on the moon's surface. Starting at 500 feet altitude with a downward velocity of 50 ft/sec and 120 units of fuel, you must carefully control your descent to achieve a soft landing.

**Author:** Mickey W. Lawless  
**Date:** December 21, 2025  
**Language:** ANSI FORTRAN 77  
**Compiler:** Lahey Personal FORTRAN 77 (compatible with gfortran)

---

## ✨ Features

- **Realistic Physics Simulation**
  - Accurate gravity and thrust calculations
  - Conservation of momentum
  - Real-time fuel consumption tracking
  
- **Enhanced ASCII Graphics**
  - Starfield with Earth and Moon in background
  - Detailed lunar surface with craters and rocks
  - Three distinct spacecraft states (intact, damaged, destroyed)
  - Dynamic altitude plot during descent
  
- **Intelligent Game Logic**
  - Exact touchdown time calculation using quadratic equations
  - Fractional-second precision for landing velocity
  - Automatic fuel depletion handling
  
- **Educational Design**
  - Demonstrates classic FORTRAN 77 programming techniques
  - Uses DATA statements and DO loops efficiently
  - Clear code structure for learning purposes

---

## ⚙️ Game Physics

### Fundamental Equations

The simulation uses the following physics model updated every second:

```fortran
V1 = V - B + 5.0        ! New velocity
F = F - B                ! Fuel consumption
H = H - 0.5 * (V + V1)  ! Height change (trapezoidal integration)
```

### Physics Parameters

| Parameter | Value | Description |
|-----------|-------|-------------|
| **Gravity** | 5.0 ft/sec² | Lunar gravity constant (downward) |
| **Thrust** | 1.0 ft/sec² per fuel unit | Engine efficiency |
| **Max Thrust** | 30.0 units/sec | Maximum burn rate |
| **Initial Height** | 500.0 feet | Starting altitude |
| **Initial Velocity** | 50.0 ft/sec | Initial descent rate |
| **Initial Fuel** | 120.0 units | Total fuel available |

### Touchdown Calculation

For precise landing velocity, the program solves:

```
If thrust ≠ 5.0:
  D = (-V + √(V² + H(10 - 2B))) / (5 - B)
  
If thrust = 5.0 (constant velocity):
  D = H / V
  
Final velocity: V1 = V + (5 - B) × D
```

---

## 🎯 Landing Outcomes

LUNLAND evaluates your landing based on touchdown velocity:

### 🏆 Perfect Landing (|V| < 0.01 ft/sec)

```
Velocity: 0.0 ft/sec
Result: "CONGRATULATIONS!!! A PERFECT LANDING!"
```

### ⚠️ Acceptable Landing (0.01 ≤ |V| < 2.0 ft/sec)

```
Velocity: 0.1 - 1.9 ft/sec
Result: "WELL......THAT WAS OK. BUT NOT SPECTACULAR..."
```

### 💥 Crash Landing (|V| ≥ 2.0 ft/sec)

```
Velocity: 2.0+ ft/sec
Result: "***** SORRY, BUT YOU BLEW IT KIDO!!!!!"
```

---

## 🛠️ Installation & Compilation

### Prerequisites

- FORTRAN 77 compiler (Lahey, gfortran, or compatible)
- Terminal with 70+ column width
- 24+ line display capability

### Compilation

#### Using GNU Fortran (gfortran)
```bash
gfortran -std=legacy -o lunland lunland.for
./lunland
```

#### Using Lahey FORTRAN 77
```bash
lf77 lunland.for
lunland
```

#### Using Intel Fortran
```bash
ifort -std77 -o lunland lunland.for
./lunland
```

### Running the Program

```bash
./lunland
```

---

## 🎮 How to Play

### Game Flow

1. **Start**: Program displays title and asks if you want instructions
2. **Launch**: Countdown begins, landing procedure initiated
3. **Descent**: Each second you receive status update showing:
   - Time elapsed
   - Current altitude (feet)
   - Current velocity (ft/sec, positive = downward)
   - Remaining fuel (units)
   - Visual altitude plot (asterisk shows your position)
4. **Input**: Enter thrust value (0-30) at the `?` prompt
5. **Landing**: When altitude reaches 0, touchdown sequence displays results

### Strategy Guide

#### Beginner Strategy
```
Time    Recommended Thrust
0-5     15 units (slow initial descent)
5-10    12 units (maintain controlled descent)
10-15   10 units (fine adjustments)
Final   Hover near 0 velocity, gentle touchdown
```

#### Advanced Strategy
- Calculate fuel efficiency: aim for ~1 ft/sec descent at 100 feet
- Reserve 20-30 units for final approach
- Practice "hovering" at low altitude before final descent
- Perfect landing requires velocity < 0.01 ft/sec

#### Common Mistakes
- ❌ Using max thrust (30) immediately - wastes fuel
- ❌ Not reserving fuel for final approach
- ❌ Letting velocity increase too much
- ❌ Overcompensating and ascending

---

## 📂 Code Structure

### Main Program: `LUNARL`

The primary program loop controls game flow:

```
LUNARL
├── Initialization
│   ├── Display title
│   ├── Call INSTRS (optional)
│   └── Set initial conditions
├── Main Game Loop (label 300)
│   ├── Call DSPSTA (display status)
│   ├── Read fuel input
│   ├── Physics calculations
│   ├── Check for touchdown
│   └── Check for fuel depletion
├── Out of Fuel Loop (label 350)
│   ├── Continue physics without input
│   └── Wait for touchdown
├── Touchdown Sequence (label 400)
│   ├── Calculate exact landing velocity
│   ├── Call DRAWLD (display graphics)
│   └── Display evaluation
└── Restart Option
```

### Subroutines

| Subroutine | Purpose | Key Features |
|------------|---------|--------------|
| **INSTRS** | Display game instructions | Multi-line formatted output |
| **DSPSTA** | Display status with altitude plot | Dynamic asterisk positioning |
| **DRAWLD** | Main graphics coordinator | Calls appropriate drawing subroutine |
| **DRWPRF** | Draw perfect landing scene | Intact spacecraft with banner |
| **DRWDMG** | Draw damaged landing scene | Tilted spacecraft with debris |
| **DRWCSH** | Draw crash landing scene | Explosion and wreckage |
| **CLRSCR** | Clear screen | Prints blank lines |

### Data Flow

```
User Input (Thrust)
    ↓
Physics Engine (LUNARL main loop)
    ↓
Status Display (DSPSTA)
    ↓
[Landing Detection]
    ↓
Graphics Selection (DRAWLD)
    ↓
Scene Drawing (DRWPRF/DRWDMG/DRWCSH)
    ↓
Final Evaluation
```

---

## 🎨 Customization Guide

### 1. Adjusting Initial Conditions

**Location:** `PROGRAM LUNARL`, lines 48-51

```fortran
C     Initialize variables
      T = 0
      H = 500.0      ! Starting height (feet)
      V = 50.0       ! Initial downward velocity (ft/sec)
      F = 120.0      ! Initial fuel (units)
```

**Customization Examples:**

| Difficulty | Height | Velocity | Fuel | Description |
|------------|--------|----------|------|-------------|
| Easy | 300.0 | 30.0 | 150.0 | Lower altitude, slower start, more fuel |
| Normal | 500.0 | 50.0 | 120.0 | Default settings |
| Hard | 750.0 | 70.0 | 100.0 | Higher altitude, faster start, less fuel |
| Expert | 1000.0 | 80.0 | 90.0 | Maximum challenge |

### 2. Modifying Physics Constants

**Location:** `PROGRAM LUNARL`, lines 70-71

```fortran
C     Physics calculations
      V1 = V - B + 5.0    ! Change 5.0 to adjust gravity
```

**Location:** Lines 66-68

```fortran
C     Limit fuel burn rate
      IF (B .LT. 0.0) B = 0.0
      IF (B .GT. 30.0) B = 30.0    ! Change 30.0 for max thrust
```

**Customization Examples:**

```fortran
! Low gravity mode (easier)
V1 = V - B + 3.0

! High gravity mode (harder)
V1 = V - B + 7.0

! Powerful engine (easier)
IF (B .GT. 50.0) B = 50.0

! Underpowered engine (harder)
IF (B .GT. 20.0) B = 20.0
```

### 3. Changing Landing Criteria

**Location:** `PROGRAM LUNARL`, lines 108-114

```fortran
C     Draw landing scene based on outcome
      LANDST = 0
      IF (ABS(V1) .LT. 0.01) THEN      ! Perfect landing threshold
         LANDST = 1
      ELSE IF (ABS(V1) .LT. 2.0) THEN  ! Acceptable landing threshold
         LANDST = 2
      ELSE                              ! Crash
         LANDST = 3
      END IF
```

**Customization Examples:**

```fortran
! Strict expert mode
IF (ABS(V1) .LT. 0.001) THEN    ! Near-zero velocity required
   LANDST = 1
ELSE IF (ABS(V1) .LT. 1.0) THEN
   LANDST = 2

! Forgiving beginner mode  
IF (ABS(V1) .LT. 0.1) THEN
   LANDST = 1
ELSE IF (ABS(V1) .LT. 5.0) THEN
   LANDST = 2

! Four-tier system
IF (ABS(V1) .LT. 0.01) THEN
   LANDST = 1  ! Perfect
ELSE IF (ABS(V1) .LT. 1.0) THEN
   LANDST = 2  ! Excellent
ELSE IF (ABS(V1) .LT. 3.0) THEN
   LANDST = 3  ! Good
ELSE
   LANDST = 4  ! Crash
```

### 4. Customizing Messages

**Location:** `PROGRAM LUNARL`, lines 118-135

```fortran
C     Display landing evaluation
      IF (LANDST .EQ. 1) THEN
         WRITE(*,430)
430      FORMAT(/,' CONGRATULATIONS!!!  A PERFECT LANDING!',/,
     >          ' YOUR LICENSE WILL BE RENEWED.........LATER.',/)
```

**Add Scoring System:**

```fortran
C     Calculate score based on fuel remaining and time
      SCORE = F * 10 + (500 - T * 10)
      WRITE(*,435) SCORE
435   FORMAT(/,' MISSION SCORE: ',I5,' POINTS',/)
```

### 5. Modifying the Altitude Plot

**Location:** `SUBROUTINE DSPSTA`, line 19

```fortran
C     Calculate plot position (29 to 80)
      PLTPOS = INT(H / 12.0) + 29    ! Change 12.0 to adjust scale
```

**Customization Examples:**

```fortran
! Wider plot range (more sensitive)
PLTPOS = INT(H / 8.0) + 29

! Narrower plot range (less sensitive)
PLTPOS = INT(H / 15.0) + 29

! Logarithmic scale (better for high altitudes)
PLTPOS = INT(LOG(H + 1.0) * 10.0) + 29
```

### 6. Modifying Starfield

**Location:** `SUBROUTINE DRAWLD`, lines 15-20

```fortran
      DATA STARX /5, 12, 20, 28, 35, 42, 50, 58, 65/
      DATA STARY /1, 2, 1, 2, 1, 2, 1, 2, 1/
```

**Add More Stars:**

```fortran
      INTEGER STARX(15), STARY(15)
      DATA STARX /5, 12, 20, 28, 35, 42, 50, 58, 65,
     >            8, 17, 33, 47, 55, 68/
      DATA STARY /1, 2, 1, 2, 1, 2, 1, 2, 1,
     >            1, 1, 2, 2, 1, 2/
```

### 7. Customizing Spacecraft Graphics

**Location:** `SUBROUTINE DRWPRF`, lines 12-14

```fortran
      DATA CRAFT /'     |     ', '     |     ', '    /-\    ',
     >            '    | O |  ', '   /=====\ ', '   | [o] | ',
     >            '   | USA | ', '  /===H===\', '    (V)   ',
     >            '  /|     |\', ' [==]   [==]', '  .:.   .:. '/
```

**Change USA to Another Country:**

```fortran
      DATA CRAFT /'     |     ', '     |     ', '    /-\    ',
     >            '    | O |  ', '   /=====\ ', '   | [o] | ',
     >            '   | EUR | ', '  /===H===\', '    (V)   ',
     >            '  /|     |\', ' [==]   [==]', '  .:.   .:. '/
```

**Change Banner Text:**

```fortran
      DATA BANNER /'*** EXCELLENT! **'/
```

### 8. Adding Sound Effects

**Location:** After touchdown in `PROGRAM LUNARL`, line 103

```fortran
C     Touchdown sequence
400   CONTINUE
      
      WRITE(*,'(A)') CHAR(7)    ! System beep/bell
      WRITE(*,410)
```

**Multiple Beeps for Crash:**

```fortran
      IF (LANDST .EQ. 3) THEN
         DO 405 I = 1, 3
            WRITE(*,'(A)') CHAR(7)
            CALL SLEEP(1)  ! Platform-dependent
405      CONTINUE
      END IF
```

### 9. Creating Custom Craters

**Location:** `SUBROUTINE DRAWLD`, lines 22-25

```fortran
      DATA CRATX /10, 25, 25, 45, 45, 60/
      DATA CRATY /19, 19, 18, 19, 18, 19/
      DATA CRATC /'o', 'O', '^', 'O', '^', 'o'/
```

**Add More Craters:**

```fortran
      INTEGER CRATX(10), CRATY(10)
      CHARACTER*1 CRATC(10)
      DATA CRATX /8, 10, 22, 25, 25, 42, 45, 45, 58, 60/
      DATA CRATY /19, 19, 19, 19, 18, 19, 19, 18, 19, 19/
      DATA CRATC /'o', 'o', 'O', 'O', '^', 'O', 'O', '^', 'o', 'o'/
```

### 10. Adjusting Screen Size

**Location:** Multiple subroutines

```fortran
C     Current: 24 rows x 70 columns
      CHARACTER*1 SCREEN(24,70)
      
C     Change to: 30 rows x 80 columns
      CHARACTER*1 SCREEN(30,80)
```

**Note:** You'll need to adjust surface drawing loops and spacecraft positions accordingly.

---

## 🖼️ ASCII Graphics Reference

### Perfect Landing Scene

```
    *            .                *              .           *         .
         .---.            ( )                        .
        /  o  \          / o \                                    *
        \___/           |_____|
                         \___/

                  o        ^       O        ^       o    ^    ^^
======================================================================
######################################################################
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


                    *** PERFECT! ***
                           |
                           |
                          /-\
                         |   |
                        /=====\
                        | [o] |
                        | USA |
                       /===H===\
                         (V)
                       /|     |\
                      [==]   [==]
                       .:.   .:.
```

**Elements:**
- `*` = Stars in background
- `.---.` / `/ o \` = Earth (left) and Moon (right)
- `======` = Lunar surface line
- `######` and `XXXX` = Subsurface layers
- `o`, `O`, `^` = Craters on surface
- `^^` = Rocks and boulders
- Spacecraft parts:
  - `|` = Radio antenna
  - `/-\` = Docking port
  - `| O |` = Window
  - `[o]` = Porthole
  - `USA` = Country marking
  - `===H===` = Descent stage with fuel tank
  - `(V)` = Engine nozzle
  - `[==]` = Landing footpads
  - `.:.` = Dust clouds

**Code Location:** `SUBROUTINE DRWPRF`

---

### Acceptable Landing Scene

```
    *       .         .                  *                 .         *
         .---.              ( )                    .
        /  o  \            / o \                                *
        \___/             |_____|
                           \___/

                  o        ^       O        ^       o    ^    ^^
======================================================================
######################################################################
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


                            .
                           /
                          /|
                         /===\
                        /[*] |
                        |USA |
                        \=~=/
                         (/)
                       /|~   |\
                      /==    ==\
                             [==]
                             .:.
                      -           o
```

**Damage Indicators:**
- `.` and `/` above spacecraft = Bent antenna
- `*` in window = Cracked porthole
- `~` in descent stage = Structural damage
- `(/)` = Bent engine nozzle
- Missing left footpad = Broken landing leg
- `-` and `o` on surface = Debris

**Code Location:** `SUBROUTINE DRWDMG`

---

### Crash Landing Scene

```
    *         .            *              .              *          .
         .---.              ( )                    .
        /  o  \            / o \                                *
        \___/             |_____|
                           \___/

                  o        ^       O        ^       o    ^    ^^
======================================================================
######################################################################
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


                     .  *  .  *  .
                    *  .  *  .  *
                   . *  BOOM! * .
                    *  .  *  .  *
                     . *  . *  .
                        ~}{~
                        }~{

                       \|/*\|/
                      /--XXX--\
                      |XXXXXXX|
                       \-X*X-/

                /-              \-
               |       [US]  [A]   |
                      (V)  ==

                        . : : .
                      (----------)

                o        ^         ^        o
                 .                        .
```

**Explosion Elements:**
- `.  *  .  *  .` = Explosion cloud expanding outward
- `BOOM!` = Explosion text
- `~}{~` and `}~{` = Fire and flames
- `\|/*\|/` = Radiating explosion energy
- `/--XXX--\` and `|XXXXXXX|` = Main wreckage pile
- `/-`, `\-`, `[US]`, `[A]` = Scattered debris (parts of "USA")
- `(V)` and `==` = Engine parts
- `. : : .` = Impact crater center
- `(----------)` = Crater rim
- `o` and `^` = Ejected rocks

**Code Location:** `SUBROUTINE DRWCSH`

---

### Graphics Data Structure

All graphics use DATA statements for easy modification:

```fortran
C     Example: Starfield definition
      DATA STARX /5, 12, 20, 28, 35, 42, 50, 58, 65/
      DATA STARY /1, 2, 1, 2, 1, 2, 1, 2, 1/

C     Example: Earth appearance
      DATA EARTH /'  .--. ', ' / o  \', ' \___/ '/

C     Example: Spacecraft structure
      DATA CRAFT /'     |     ', '     |     ', '    /-\    ',
     >            '    | O |  ', '   /=====\ ', '   | [o] | ',
     >            '   | USA | ', '  /===H===\', '    (V)   ',
     >            '  /|     |\', ' [==]   [==]', '  .:.   .:. '/
```

### Character Set Reference

| Character | Usage | Description |
|-----------|-------|-------------|
| `*` | Stars, explosion, damage | Bright point |
| `.` | Stars, dust, debris | Dim point |
| `:` | Dust, crater | Medium detail |
| `o`, `O` | Craters, portholes | Circular objects |
| `^` | Rocks, crater peaks | Pointed objects |
| `=` | Structure, surface | Horizontal lines |
| `#` | Subsurface | Dense material |
| `X` | Subsurface, wreckage | Very dense/destroyed |
| `/`, `\` | Angles, slopes | Diagonal lines |
| `|` | Vertical lines | Straight up/down |
| `[`, `]` | Containers | Brackets |
| `(`, `)` | Rounded objects | Parentheses |
| `{`, `}` | Fire, turbulence | Curly braces |
| `~` | Damage, waves, fire | Tilde |
| `-` | Horizontal, debris | Dash |

---

## 🔬 Technical Details

### Numerical Methods

**Trapezoidal Integration:**
```fortran
H = H - 0.5 * (V + V1)
```
Uses average of old and new velocity for more accurate position updates.

**Quadratic Solution for Touchdown:**
```fortran
D = (-V + SQRT(V*V + H*(10.0 - 2.0*B))) / (5.0 - B)
```
Solves the equation of motion to find exact collision time.

### Memory Usage

- Screen buffer: 24 × 70 = 1,680 characters
- DATA statements: ~500 bytes total
- Stack depth: Maximum 3 subroutine calls
- Total memory: < 5 KB

### Performance

- Graphics rendering: O(n²) where n = screen dimension
- Physics calculation: O(1) per iteration
- Typical game: 10-30 seconds, 10-30 iterations
- Screen updates: < 0.1 seconds per frame

### Portability Notes

**Character Set:**
- Uses CHAR(92) for backslash `\` (portable across systems)
- Uses CHAR(7) for system bell (optional sound)

**Screen Control:**
- CLRSCR uses blank lines (no ANSI escape codes)
- Compatible with any 70+ column terminal
- No OS-specific system calls

**I/O:**
- Standard FORTRAN 77 READ/WRITE
- No file I/O, purely interactive

---

## 📚 Educational Value

### Programming Concepts Demonstrated

1. **Structured Programming**
   - Main program with subroutines
   - Clear separation of concerns
   - Modular design

2. **Data Structures**
   - Arrays (CHARACTER*1 SCREEN)
   - DATA statements for initialization
   - Parallel arrays for coordinates

3. **Control Flow**
   - DO loops
   - IF-THEN-ELSE logic
   - GOTO for game loop (classic style)

4. **Numerical Computing**
   - Physics simulation
   - Integration methods
   - Root finding (quadratic formula)

5. **Text Processing**
   - FORMAT statements
   - String manipulation
   - Screen buffer management

### Physics Principles

- Kinematics (position, velocity, acceleration)
- Newton's laws of motion
- Conservation of momentum
- Fuel efficiency and thrust
- Orbital mechanics basics

### Problem-Solving Skills

- Resource management (fuel)
- Risk assessment (landing criteria)
- Real-time decision making
- Trade-off analysis (speed vs safety)

---

## 🤝 Contributing

Contributions are welcome! Areas for enhancement:

### Suggested Improvements

1. **Gameplay Features**
   - Multiple landing sites with different terrains
   - Scoring system based on fuel efficiency
   - High score table
   - Time trials mode
   - Variable gravity (different celestial bodies)

2. **Graphics Enhancements**
   - Color support using ANSI codes
   - Larger screen size (80×40)
   - Animated descent sequence
   - Multiple spacecraft designs
   - Weather effects (dust storms)

3. **Physics Improvements**
   - Wind simulation
   - Horizontal movement
   - Rotation control
   - More realistic fuel consumption

4. **Code Quality**
   - Add more comments
   - Error handling for invalid input
   - Input validation
   - Save/load game state

### How to Contribute

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

### Coding Standards

- Follow ANSI FORTRAN 77 standard
- Use meaningful variable names
- Add comments for complex logic
- Test with multiple compilers
- Maintain backward compatibility

---

## 📝 License

This code is provided as educational material for learning FORTRAN 77 and classic game programming techniques. Free to use for educational purposes.

---

## 🙏 Acknowledgments

- Original Lunar Lander game (1969)
- Classic BASIC and FORTRAN gaming community
- ASCII art pioneers
- FORTRAN 77 standard committee

---

## 📞 Contact

Mickey W. Lawless - December 21, 2025

For questions, suggestions, or bug reports, please open an issue on GitHub.

---

## 🚀 Version History

- **v1.0** (Dec 21, 2025)
  - Initial release
  - Three landing outcomes
  - Enhanced ASCII graphics
  - DATA statement graphics system
  - Complete documentation

---

## 🎓 Learning Resources

### FORTRAN 77 References
- [FORTRAN 77 Language Reference](https://www.fortran.com/)
- [Classic FORTRAN Programming Guide](http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/)

### Physics Resources
- [Lunar Landing Physics](https://en.wikipedia.org/wiki/Lunar_Lander_(video_game_genre))
- [Orbital Mechanics Basics](https://en.wikipedia.org/wiki/Orbital_mechanics)

### ASCII Art Resources
- [ASCII Art Archive](https://www.asciiart.eu/)
- [Text-based Game Design](https://en.wikipedia.org/wiki/Text-based_game)

---

**Happy Landing, Commander!** 🌙🚀

```
    *    .    *    .    *    .    *    .    *
         .---.              ( )        
        /  o  \            / o \       
        \___/             |_____|      
                           \___/       
```
