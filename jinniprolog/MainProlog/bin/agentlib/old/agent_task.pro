/*

agent interoperation protocol

design ideas: 

performatives = actions/queries about tasks/services
tasks/services: proceses, queries, updates, proxy tasks (i.e. ask B to do C)

form of a message:

performative(task)

performatves:

initiate(Task,TaskId): initiates a Task and get a TaskId for it

can_yo_do(CapabilityOrTask): queries about a capability, relevant for a task/service

get_status(TaskId,Status), where Status can be: unstarted, in_progress, finished, failed, garbage_collected, unknown

cancel(TaskId): cancels a tasks and frees/unlocks related resopurces

retry(TaskId): retries a task that has failed

get_result(TaskId,Result): retrives a result produced a task/services

an agent initiating a service becomes automatically an observer of the status of the service
and it should be ready to handle notifications about its completion

notification(TaskId,Status)

*/


