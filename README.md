



<h3 align="center"></h3> Haskell Social Network Simulation </h3>

<div align="center">

  [![code coverage](coverage.svg "Code coverage")]()
</div>

---
## 🧐 About <a name = "about"></a>
The Haskell Social Network Simulation project aims to create a simulation of a social network using Haskell. The application spawns ten user threads, each representing a user in the social network. The users randomly select another user and send a random message to that user at random intervals. The project includes the definition of appropriate Haskell data types for users and messages. The main thread creates the users, spawns the user threads, and manages the simulation. The project also includes error handling, haddock-style comments, and a report detailing design decisions and challenges faced during development.

## Application Overview
The Haskell Social Network Simulation consists of several modules:

* User.hs: This module defines the User data type, representing a user in the social network. It includes fields such as username, messages received, messages sent, received messages, and sent messages.

* Message.hs: The Message module defines the Message data type, representing a message sent between users. It includes fields such as the sender, receiver, message content, time, and date.

* Main.hs: The Main module serves as the entry point of the application. It creates ten users, spawns user threads, manages the simulation, and displays the final count of messages received by each user.


## Design Choices
* User Data Type: The User data type includes fields such as username, messages received, messages sent, received messages, and sent messages. These fields provide a comprehensive representation of user-related information and enable tracking of message counts.

* Message Data Type: The Message data type includes fields such as sender, receiver, message content, time, and date. This representation captures all the necessary details of a message and facilitates easy tracking and display of messages.

* Concurrent Access and Data Consistency: To manage concurrent access to shared resources and ensure data consistency, MVars are used. The "total" MVar keeps track of the messages sent and stops sending messages once the limit of 100 messages is reached. The "wait" MVar is used to notify the end of the simulation.

* Memory Management: Haskell being a memory-managed language, memory utilization can be challenging when multiple threads are running concurrently. Careful consideration is given to memory management to prevent excessive memory usage.

## Challenges Faced
* Concurrent Access to Shared Resources: Managing concurrent access to shared resources posed a challenge, requiring the use of mechanisms to ensure safe and consistent access. The use of MVars helped synchronize access to shared data.

* Data Consistency: Ensuring data consistency when multiple threads access and modify shared data was a challenge. Proper synchronization techniques and data structures were employed to avoid race conditions and unpredictable data.

* Memory Management: Haskell's memory management posed challenges when dealing with multiple concurrent threads. Careful monitoring and control of memory usage were necessary to prevent memory leaks and excessive memory consumption.
  
## Extension of the Specification

As an extension to the given specification, an option to display a user's inbox and outbox was implemented. After the simulation is completed, the application prompts the user to choose a user ID and display either the inbox or outbox of that user. This feature provides users with the ability to view their messages and the total count of messages sent or received.

## User Manual
To compile and run the application, use the command "stack run" in the project directory.

To generate the Haddock documentation, use the following command:

``` stack build && cabal haddock --enable-documentation --open 
```
The generated Haddock documentation can be accessed through the provided file path.

Upon running the application, the simulation starts automatically. Once the simulation completes, the final count of messages received by each user is displayed.

To view a user's inbox or outbox, follow the prompts and enter the user ID. The application then prompts the user to choose whether to display the inbox or outbox of the selected user. The application then displays the messages along with the total count of messages sent or received by the user.

## Conclusion

The Haskell Social Network Simulation provides a realistic simulation of a social network using concurrent threads. The application successfully spawns user threads, simulates message sending between users, and tracks the count of messages received by each user. Design choices such as the User and Message data types, the use of MVars for concurrent access, and memory management considerations ensure the efficiency and reliability of the application. The extension to display a user's inbox and outbox adds an additional useful feature to the simulation. By following the provided user manual, users can effectively compile, run, and explore the functionalities of the application.