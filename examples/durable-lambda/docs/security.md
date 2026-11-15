# Security

What a session of [Durable Lambda](../README.md) can and cannot get.

| Threat | Control |
| --- | --- |
| A session reads the provider key | Only the `llm` role can read the key. The worker has no SSM permission and no route to SSM. |
| A session calls another provider API | `llm` accepts two calls and only the models of the stack. |
| A session sends data to the internet | The network has no internet gateway and no NAT gateway. The resolver answers only for `amazonaws.com` names. Each endpoint accepts only the resources of this stack. |
| A session pushes to another repository | Only `repo` has a key. The key is a deploy key of one repository. |
| A session rewrites history | `repo` never uses force. A push that is not a fast-forward goes to a new branch. The ruleset "main is append-only" of GitHub refuses a deletion of `main` and a push to `main` that is not a fast-forward. |
| Private work becomes public | The repository is public, because GitHub permits a ruleset on a private repository only with a paid plan. Each commit that a session publishes is public. For private work, use such a plan and make the repository private. |
| A session makes GitHub run its code | `repo` refuses a push that changes `.github`. GitHub Actions is off for the repository. Do not turn on GitHub Actions in this repository: a workflow on `main` runs the code that a session wrote. |
| A session changes the release of the next worker | `sync` owns `releases/` and `CURRENT`. The worker has another user id on EFS and cannot write there. EFS permits a mount only to the roles of `worker` and `sync`, each through its own access point. The worker role cannot write `repo/` in the bucket. |
| A bad download gets into the image | Each download has a hash. The base image has a digest. Each file that `curl` downloads has a SHA-256, each Python package has a hash and each extension has a Git commit. The digest sets the Amazon Linux release, and `dnf` reads only the repository of that release. [`requirements/rpm.lock`](../requirements/rpm.lock) has the header SHA-256 of each installed RPM, and the build stops when one differs. The image has no Java and no Maven library. |
| A person that you do not know calls the stack | Nothing stops this person: the stack has no sign-in. Each person who has the address can start sessions and runs, use the provider key through `llm` and publish commits to the repository. Give the address only to people that you trust. For a stack that other people can find, put a sign-in before `front`. |
| A person decides at a gate of another person | Nothing stops this person: a gate has no sign-in. A gate is a step of a workflow, not a security control. |
| A false webhook | `front` checks the `X-Hub-Signature-256` of GitHub. |
| The credentials of the worker role leave the function | Each S3, SQS and Lambda permission of the worker role has the condition `aws:SourceVpc`. Outside the network of the stack, the credentials permit only a write to the log group of the worker. |
| A session deletes or overwrites data | The bucket keeps each old version of an object for 7 days. The worker can delete only `state/`, `relay/`, `workflows/`, `tmp/` and `work/`. |
| A stored answer or a stored file runs script in the browser | Each relayed answer has a `sandbox` content security policy, and `front` passes only a fixed list of answer headers. The API sends each attached file and each temporary file as plain text and as a download. |
| A flood of requests makes a large bill | The functions have a concurrency limit: 50 for `front`, 5 for `worker`, 20 for `llm`, 1 for `repo` and 1 for `sync`. A call cannot ask for more output tokens than the limit of the model. |

## The limit that you must know

Lambda does not let a function make a new kernel namespace, a seccomp filter or a
Landlock rule set. Thus the Vis jail cannot start in a function, and the worker runs
without it. The gateway and the tools of a session have no AWS credentials in their
environment. But the code of a session runs as the user of the worker. It can read
the temporary credentials of the worker role from the worker process. It can call the
Lambda Runtime API of the worker. It can read and change the data of the other
sessions of the stack. For this reason:

- the worker role permits only the bucket prefixes, the queue and the three functions that the worker needs;
- each secret is in a function that does not run the code of a session;
- one stack is for one team that shares its sessions. Give each tenant its own stack;
- the provider key has no spending limit in this stack. Set a budget for the key at the provider.

A session can also write in `work/` on EFS, which holds the project directory and the
work directory of each session. It cannot write a release.
