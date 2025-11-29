# VHDL Slot Machine


A digital slot machine implemented in **VHDL** as part of the *Digital System Fundamentals* course. This project runs on an FPGA and demonstrates practical digital design concepts such as finite state machines, display control, and modular hardware architecture.

---

## 📌 Overview

This project simulates a simple slot machine using hardware description logic. When the user activates the system, the slot machine cycles through symbols and displays the final randomized results on the 7-segment (or LED) display.

I was responsible for two major components:

* **Display System** – Designs and drives the output display modules.
* **Main Control Logic** – Implements the state machine and manages the overall flow between modules.

---

## 🎯 Features

* Fully hardware-based implementation using **VHDL**
* Modular design (control logic, display, randomization, timing)
* Finite State Machine (FSM) for game flow
* Random-like number generation for slot results
* LED/7-segment display driver
* Debounced push-button input handling

---

## 🎥 Demo

Watch the gameplay demo on YouTube:

[![Gameplay Demo](https://img.youtube.com/vi/kLzNHlMK-ao/0.jpg)](https://youtu.be/kLzNHlMK-ao)

---

## 🧩 System Architecture

**Main components include:**

* **Control FSM** – Manages IDLE, SPIN, and RESULT states
* **Randomizer Module** – Generates pseudo-random slot values
* **Display Controller** – Converts slot values to output signals
* **Clock Divider** – Adjusts the FPGA clock for human-visible display updates

---

## 🛠 Technologies & Tools

* **VHDL** (main implementation)
* **Xilinx ISE 14.7** (synthesis & FPGA programming)
* FPGA board (e.g., **Basys 3**, **Nexys**, or equivalent)

---

## 🙋‍♂️ My Contribution

I was responsible for the **core logic and video system**, specifically:

* **Display_VGA.vhd** — main gameplay logic, reel progression, win detection, and all VGA rendering
* **main_vga.vhd** — top-level integration of inputs, outputs, and subsystem wiring
* **debounce.vhd** — stable button/signal handling to avoid glitch inputs
* Integrating VGA timing, pixel drawing pipeline, and on-screen animations

---

### 🔗 Note on Missing Components

Only my part of the project is included in this repository.
My teammate handled the **coin/credit module**, which is essential to start the game (INSERT COIN logic).
My code expects this module to provide the credit signal, but it is not included here.

---

## 📬 Contact

Feel free to open an issue or message me if you want help understanding any module or adapting it to your own FPGA project.