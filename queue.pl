% A queue data type in CHR with constant time operations.
% Needed since this is difficult to do with pure Prolog.

:- use_module(library(chr)).

:- chr_constraint init_queue/0, enqueue/2, dequeue/1,
		queue_element/3, queue_start/2, queue_end/2,
		dequeued_element/2.

% The queue is initialized with start=end, first-index=1 
init_queue(Q) :-
    queue_start(Q,0), queue_end(Q,0).

% Add element to end of queue
enqueue_element @
enqueue(Q, Elem), queue_end(Q,End) <=>
    NextEnd is End + 1
    |
    queue_element(Q,End,Elem),
    queue_end(Q,NextEnd).

% Extract the front-most element from queue.
dequeue_element @
dequeue(Q),queue_element(Q,Start,Elem),queue_start(Q,Start),queue_end(Q,End)<=>
    End > Start % The queue must have at least one element
    |
    NextStart is Start + 1,
    dequeued_element(Q,Elem), % Add the extracted element to the store
    queue_start(Q,NextStart).

% Dequeuing an element from empty queue has no effect.
dequeue(Q), queue_start(Q,StartEnd), queue_end(Q,StartEnd) <=> true.

%%% Testing

:- chr_constraint report_dequeued/0.

dequeued_element(Q,E) <=> 
    write('* dequeued element '),
    write(E),
    write(' , from queue:'),
    write(Q),
    nl. 


qtest :-
	init_queue(test),
	enqueue(test,elem1),
	enqueue(test,elem2),
	enqueue(test,elem3),
	dequeue(test),
	dequeue(test),
	dequeue(test),
	dequeue(test).
