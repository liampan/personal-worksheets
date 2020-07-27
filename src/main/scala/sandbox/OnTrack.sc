import scala.concurrent.{ExecutionContext, Future}

object Test1 {
  // Given these functions:

  def f1: Future[Unit] = ???

  def f2: Future[Unit] = ???

  def f3: Future[Unit] = ???

  def f4: Future[Unit] = ???

  //Write code to execute them when:
  // there are no dependencies between the functions

  f1
  f2
  f3
  f4

  //f4 depends on f3 which depends on f2 which depends on f1

  implicit val ec: ExecutionContext = ???

  for {
    _ <- f1
    _ <- f2
    _ <- f3
    _ <- f4
  } yield ()

  // f4 depends on f3 and f2, and f3 and f2 both depend on f1

  for {
    _ <- f1
    _ <- {
      val startF2 = f2
      val startF3 = f3
      for {
        _ <- startF2
        _ <- startF3
      } yield ()
    }
    _ <- f4
  } yield ()
}


object Test2 {

  // Given two functions f1 and f2, implement f3 by composing f1 and f2
  val f1: (Int, Int) => Int = (a, b) => a + b
  val f2: Int => String = _.toString
  val f3: (Int, Int) => String = (f1.tupled andThen f2) (_, _)

}

import scala.annotation.tailrec

object Test3 {

  //Given a list Seq(1, 2, 3)
  //which represents the number 123, write a function to increment it by one without converting
  //  types. Your function should produce the expected result for the
  //following test cases:

  def increment(inputList: Seq[Int]): Seq[Int] = {
    @tailrec
    def incrementHelper(ints: Seq[Int], index: Int): Seq[Int] = {
      val newValueAtIndex = (ints(index) + 1) % 10
      val updatedList = ints.patch(index, Seq(newValueAtIndex), 1)
      if (newValueAtIndex == 0) incrementHelper(updatedList, index - 1)
      else updatedList
    }

    val listWithBuffer = 0 +: inputList
    val out = if (inputList.nonEmpty) incrementHelper(listWithBuffer, index = listWithBuffer.length - 1) else Seq.empty[Int]
    if (out.headOption.contains(0)) out.tail else out
  }

  //Consideration was made to reverse the list when incrementing to
  // improve efficiency of the algorithm, however to maintain readability
  // I opted to decrement the index instead.

  //  Nil => Nil
  assert(increment(Nil) == Nil)
  //  Seq(0) => Seq(1)
  assert(increment(Seq(0)) == Seq(1))
  //  Seq(1, 2, 3) => Seq(1, 2, 4)
  assert(increment(Seq(1, 2, 3)) == Seq(1, 2, 3))
  //  Seq(9, 9, 9) => Seq(1, 0, 0, 0)
  assert(increment(Seq(9, 9, 9)) == Seq(1, 0, 0, 0))


  // A 2nd implementation of increment using foldRight
  // In the end I preferred the readability of the tail recursive implementation
  def _increment(inputList: Seq[Int]): Seq[Int] =
    if (inputList.nonEmpty) {
      val listWithBuffer = 0 +: inputList
      val modified =
        listWithBuffer
          .zipWithIndex
          .foldRight((true, listWithBuffer)) {
            case ((current, index), state@(canChange, list)) =>
              if (canChange) {
                val newValueAtIndex = (current + 1) % 10
                val updatedList = list.updated(index, newValueAtIndex)
                (newValueAtIndex == 0, updatedList)
              } else
                state
          }._2
      if (modified.headOption.contains(0)) modified.tail else modified
    } else Seq.empty[Int]
}


trait Test4 {

  implicit val ec: ExecutionContext = ???

  /// Given the following function:
  def f[A](a: A): Future[A]

  //  Write a function `g` that safely handles calling f. The return type of `g` should be such that
  //    when f succeeds, g returns something very similar. Feel free to import an external library for
  //  the return type of g.

  def g[A](a: A): Future[Either[Throwable, A]] =
    f(a).map(Right.apply) recover {
    case t: Throwable => Left(t)
  }

  // I have made the assumption that g can return a Future monad of A.
  // This ensures the futures possible exceptions can be caught
  // and still return valid type.
  // Option could work here:  def g[A](a: A): Future[Option[A]]
  // however using the Either monad allows the throwable to be stored as a Left
  // meaning we don't lose information of why the future failed.

}

object Test5 {

  // Explain what the following code means:
  // Mention some advantages of the above code.
  trait MyAlg[F[_]] {
    def insertItSomewhere(someInt: Int): F[Unit]

    def doSomething(someInt: Int): F[Int]
  }

  // MyAlg is a typeclass which enables adding functionality
  // to existing classes where traditionally you wouldn't be able to (adhoc polymorphism)
  // MyAlg adds to the type constructor F[_]
  // the advantages of typeclasses include forcing returning current types
  // typeclasses also allow easier testing and refactoring as they
  // rely on the minimum amount of behaviour to complete a task.


}

object Test6 {

  trait MyAlg[F[_]] {
    def insertItSomewhere(someInt: Int): F[Unit]

    def doSomething(someInt: Int): F[Int]

  }

  import cats.Functor

  class MyProg[F[_]](alg: MyAlg[F]) {

    // Given the trait in Q5,
    //create a class `MyProg` abstract in type F that has MyAlg passed to it.
    //Implement the following method in the class:

    def checkThenAddIt(someInt: Int) =
      FFunctor.map(alg.doSomething(someInt))(alg.insertItSomewhere)

    //It should pass the result of `doSomething` to `insertItSomewhere`.
    //Feel free to add external imports.

    val FFunctor: Functor[F] = new Functor[F] {
      override def map[A, B](fa: F[A])(f: A => B): F[B] = ???
    }

  }


}

object Test7 {

  // How would you design a REST API for an address book?
  // What endpoints will it have (feel free to provide sample curl requests)?
  // How would you handle errors?

  /*
    The address book would work with a Contact model that would represent contacts in the address book
    Contacts would provide a name and email, this a model that could be updated depending on business
    requirements. All Contacts would be identified with an unique ID.
  */
  final case class Contact(id: Int, name: String, email: String)

  //    {
  //      "id": id,
  //      "name": name,
  //      "email": email
  //    }


  /*
  Endpoints:

  GET  /contacts
      returns a list of all known contacts

      example curl:
      curl -X GET http://localhost:9000/contacts

      responses:
      status: CREATED 200
      {
        "contacts" : [
          {
            "id" : 1,
            "name" : "john smith",
            "email" : "example@example.com"
          },
          {
            "id" : 2,
            "name" : "Jane smith",
            "email" : "example2@example.com"
          }
        ]
      }

  POST /contact
        Takes a JSON payload of contact details to create new contact,
        returns the newly created contact with its ID

        example curl:

        curl -d '{"name":"john smith", "email":"example@example.com"}' -H "Content-Type: application/json" -X POST http://localhost:9000/contact

        responses:
        when the json is read correctly and a new contact is created
        status: CREATED 201
        {
          "id" : 1,
          "name" : "john smith",
          "email" : "example@example.com"
        }

        when there is a problem with the clients request that
         means a contact can not be created
        status: BAD_REQUEST 400
        {
          "reason": "email contains invalid characters"
        }


  GET  /contact/:id
        Returns a single contact as JSON if a contact matches with requested ID from uri

        example curl:
        curl -X GET http://localhost:9000/contact/1

        responses:
        when the id matches a known contact
        status: OK 200
        {
          "id" : 1,
          "name" : "john smith",
          "email" : "example@example.com"
        }

        when the id does not match a known contact
        status: NOT_FOUND 404

  PUT  /contact/:id
        Takes a json payload of contact details to update a contact that matches an id,
        returns the updated contact. ID is not re-writeable.

        example curl:

        curl -d '{"name":"Jim smith", "email":"example@example.com"}' -H "Content-Type: application/json" -X PUT http://localhost:9000/contact/1

        responses:
        when the id matches a known contact and the json can be used to update the contact
        status: OK 200
        {
          "id" : 1,
          "name" : "Jim smith",
          "email" : "example@example.com"
        }

        when the id does not match a known contact but the request is parseable.
        status: NOT_FOUND 404

        when the body can not be used to update a contact
        status: BAD_REQUEST 400
        {
          "reason": "unable to parse JSON"
        }

  DELETE /contact/:id
        removes the contact from the address book that matches the id

        example curl:
        curl -X DELETE http://localhost:9000/contact/1

        responses:
        when the id matches a known contact and the contact is deleted
        status: ACCEPTED 202

        when the id does not match a known contact
        status: NOT_FOUND 404

   */
}