# Demo Porject README

This is a demo README file. 


# Branching a project
Suppose we want to experiment with modifying this README file, or we wanted to change this file as part of a future update.  Either way, we can use *feature* or *topic* branches in order to separate out changes that we want to make **off of master**.

To see the existing branches, use the *git branch* command.

To create a new branch and simultaneously switch to it, use the *checkout* command with the *-b* (for *branch*) option.

**git checkout -b BRANCH_NAME** 

**NOTE:**  Branching can be done *after* you have started working on a new feature, and later decide to move it to a new branch before commiting your changes.

## Merging branches
### Fast Forward Merge
In order to switch branches, use the *checkout* command.

**git checkout master**

To merge the branch with master, **from the master** use the git merge command

**git merge update_README**

Once that is complete, if you type **git hist**, you will see that *HEAD*, *master*, and *update_README* all point to the same commit.

Now, we no longer need the updates branch.  The *-d* option of git branch will delete the unneeded branch.

**git branch -d update_README**

### Creating conflicts
This is bound to cause lots of trouble