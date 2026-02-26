
## **Monte Carlo Simulation of a Simple Polymer Chain**

**Aim:**
The goal of this project is to implement a Monte Carlo (MC) simulation program to explore the conforma-
tional landscape of a linear polymer (polyethylene). The simulation must incorporate torsional energy ro-
tations and intramolecular Lennard-Jones interactions. Students will coordinate all tasks required to produce a complete and functional parallel MC code.

**Scientific goal:**
Develop a modular, well-documented Monte Carlo program capable of simulating a polyethylene model
in which each C–C bond can adopt a different torsional angle during the simulation. The initial version ofthe program should consider only carbon atoms (united-atom force field). In a second version, explicit
hydrogens must be included.

The simulation must:
- Generate an initial polymer conformation.
- Apply Monte Carlo moves that modify torsional states.
- Compute the energy using a Lennard-Jones potential between non-bonded carbon atoms.
- Perform Metropolis acceptance/rejection steps.
- Output structural and energetic observables.
- Provide post-processing tools and visualization of results.

**Parallelization requirements (OpenMP/MPI):**
1. Replica parallelism: Multiple independent replicas.
2. Parallel energy evaluation: Distribution of the energy calculation across nodes.
3. Mixed parallelism: Several replicas, each using parallel energy evaluation.

**Minimum required tools:**
- Git / GitHub (documentation, branches, commits, issue tracking).
- Makefiles (compilation, execution pipelines, figure generation).
- MPI (parallel version of the program).
- Optional Python scripts for post-processing and plotting.

------------
## Deliverables
### March 12th — Sequential Code Delivery

(To be submitted in Campus Virtual and GitHub)

**Evaluation criteria:**
- Code modularization.
- Well-indented and well-documented code.
- Proper documentation of the repository.
- Effective use of Makefiles to:
    - build the executable
    - run simulations
    - generate figures
- A results folder containing a simulation of a **500-carbon polymer at fixed temperature**, including:
    - Energy evolution
    - Torsion angle distributions
    - Radius of gyration and end-to-end distance
    - Monte Carlo trajectory

------------

### April 12th — Project Report Delivery (24h)

(To be submitted in Campus Virtual and GitHub)

**Memory format:**
- **Text:**
    - Must include:
        - Explanation of all implemented algorithms
        - Scalability study of the parallel code
        - Test cases
        - Data analysis
    - The manuscript must be delivered as a **PDF document.**

- **Code:**
    - A zip or tar.gz file containing:
        - All source code
        - Makefiles
        - Instructions for compilation and execution

    - Every subroutine must indicate the author. If shared, individual responsibilities must be explicitly
      stated.
    - All code must be properly documented.

- **Deliverable:**
    - PDF project report
    - .zip file containing all code and instructions

## Project Presentation (April 15th — To be confirmed)

- **Format:** Prezi, PowerPoint, PDF or similar.
- **Responsibilities:** Each student prepares their own slides, forming part of a single, integrated presenta-
tion.
- **Duration:**
    - 20–30 minutes total presentation
    - 15–30 minutes of questions from the instructor

## Evaluation Criteria

- Respect for all deadlines (late deliveries will be penalized).
- Correct and effective use of GitHub, Makefiles, and parallelization techniques in all deliverables.