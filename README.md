# ScaLCM

A Vibe coding Scala 3 port of Takeaki Uno's **LCM (Linear time Closed itemset Miner)** algorithm. This project supports mining **Closed**, **Frequent**, and **Maximal** itemsets from transactional databases.

## Features

* **Pure Scala 3 Implementation:** Built utilizing modern Scala 3 features (`enum`, braceless syntax, `@main` annotations).
* **Multiple Mining Modes:** Supports Closed Itemsets (`C`), Frequent Itemsets (`F`), and Maximal Itemsets (`M`).
* **Optimized Performance:** Implements direct array buffers, occurrence delivery, and transaction shrinking matching the efficiency of the original C codebase.
* **Flexible Output:** Supports stdout summary reporting or writing discovered itemsets directly to file.

## Prerequisites

* **Scala 3** (v3.10.1 or compatible)
* **sbt** (v2.x or sbt 1.10+)
* **Java JDK** (17 or newer)

## Project Structure

```text
ScaLCM/
├── .scalafmt.conf          # Scalafmt formatting rules
├── build.sbt               # sbt build definition & assembly config
├── project/
│   ├── build.properties    # sbt version pin
│   └── plugins.sbt         # sbt-assembly plugin definition
└── src/
    └── main/
        └── scala/
            └── lcm/
                ├── lcm.scala   # Main algorithm drivers & CLI
                └── utils.scala # Transactions, base state, and helper operations
