The Project Leader (Arthur Murphy) is the "Gatekeeper" for this project and is the only one with contributor status (aside from the teacher). Therefore, teammates will be using a **Fork and Pull** workflow.

Here is a breakdown of how to handle the code and for the Project Leader to manage it.

---

## Do non Project Leaders need a role designation?

**No.** On GitHub, if you want someone to work on a repository without giving them "Write" access (the ability to push code directly to your branches), they don't need a specific role. They can interact with the project as "External Contributors."

_Plus, the Project Leader has not been given permissions to add collaborators._

---

## The Teammates' Workflow

Since non-Project Leaders don't have direct write access, they should follow these steps:

### 1. Fork the Repository

Instead of cloning the repo directly, they should go to [the project’s GitHub page](https://github.com/amu-upc/Project_2026_II) and click the **Fork** button (top right). This creates a personal copy of the project under *their* GitHub account.

### 2. Clone their Fork

They should clone their own copy to their local machine:
`git clone https://github.com/THEIR-USERNAME/Project_2026_II.git`

### 3. Create a Feature Branch

They should never work directly on the `main` branch. Before making changes, they should run:
`git checkout -b feature-description`

### 4. Commit and Push

After making their modifications:

* `git add .`
* `git commit -m "Explain what was changed"`
* `git push origin feature-description`

### 5. Open a Pull Request (PR)

Once they push to their fork, GitHub will show a yellow banner at the top of their profile saying **"Compare & pull request."** They click that, ensure the "base repository" is the **Project Leader's** repo, and hit submit.

---

## The Project Leader's Role as the Reviewer

Once teammates submit that Pull Request, it will show up in the **Pull Requests** tab of the main repository for the Project Leader to review.

* **Reviewing:** The Project Leader can click on "Files changed" to see exactly what they did.
* **Commenting:** The Project Leader can leave comments on specific lines of code if something looks off.
* **Approving:** Once it looks good, the Project Leader clicks **Review changes** -> **Approve**, and then the Project Leader (or the teacher) can hit **Merge pull request** to fold their code into the main project.

---

## Resyncing a Fork & Local Clone

If the main repository gets updated with new code, a teammate's fork will fall behind. You can sync from github.com on your branch by clicking `sync fork` (under the green "code" dropdown button). To then update their local repository clone:

1. **Check which remotes are configured:**
   
   `git remote -v`

2. **If there isn't an entry referencing the main _group_ repo, add it:**

   `git remote add main https://github.com/amu-upc/Project_2026_II.git`

3. **Fetch Changes:**
   
   `git fetch main`

4. **Pull down changes**
   
   `git pull main/master`

