---
title: "Some useful types for database-using Rust web apps"
date: 2024-04-15T20:47:11-04:00
categories: ["Hacks"]
tags: ["rust"]
---
I've been writing a little web app in rust lately, and in it I decided to try to do it without an ORM. Instead, I modeled data access in a way that resembles the [Data Access Layer](https://en.wikipedia.org/wiki/Data_access_layer) pattern: You make a set of abstractions that maps the "business logic" to how the data is stored in the data store. Here are some types that I found useful in this journey so far.
<!--more-->
## The `IdType` trait

The first item is a very subtle little thing that I wasn't sure would work. But it does, and it's really pleasing! Introducing `IdType`, a trait that marks a type used for database identifiers. Say you have a struct `Bookmark` in sqlite that is has a `u64` as a primary key. What prevents you from passing accidentally just about any `u64` (say, a user ID) into a struct and reading any bookmark in the database? Right, that's why we make newtypes.

So you make a newtype that wraps `u64` and define your bookmark struct like so[^code-quality]:

[^code-quality]: Please note that approximately none of the code listed here will compile out of the box. Very sorry - this post is meant to provide a basis for a dialog with the rust compiler, not to be an entirely copy/pasteable framework; that would require a bit more boilerplate and wouldn't add much to the quality of the content. You are smart! You got this!

```rust
struct BookmarkId(u64);

pub struct Bookmark {
    pub id: BookmarkId,
    pub url: Url,
}
```

Sweet, but how do you create a new bookmark? Your database is what assigns these IDs, so do you make a second struct `BookmarkForInsertion` and sync struct fields? Or do you extract the ID fields and make a struct two layers deep? Pass all bookmark fields to an `add` function? All of these seemed unpleasant to me. Here's what I do instead:

```rust
pub trait IdType<T>: Copy + fmt::Display {
    type Id;

    /// Returns the inner ID.
    fn id(self) -> Self::Id;
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash, Debug, Clone, Copy, sqlx::Type)]
#[sqlx(transparent)]
#[serde(transparent)]
pub struct BookmarkId(i64);

impl IdType<BookmarkId> for BookmarkId {
    type Id = i64;

    fn id(self) -> Self::Id {
        self.0
    }
}
```

OK, so that's more complicated. What does that allow us to do? Let's update our Bookmark struct to use it:

```rust
pub struct Bookmark<ID: IdType<BookmarkId>> {
    pub id: ID,
    pub url: Url,
    // ...
}
```

So what this does is, now you can handle "existing" bookmarks like before, but also you can specify that a bookmark doesn't have an ID yet:

```rust
pub fn update_bookmark(bm: Bookmark<BookmarkId>) { ... }
// and
pub fn create_bookmark(bm: Bookmark<NoId>) -> Bookmark<BookmarkId> { ... }
```

What's more, the `IdTrait<T>` takes a type parameter that tells us what the expected ID type would be. That comes into play with the `NoId` type above: It's a little empty type that just says "I'm not an ID yet":

```rust
#[derive(PartialEq, Eq, Clone, Copy, Default, Serialize, Debug)]
pub struct NoId;

impl<T> IdType<T> for NoId {
    type Id = std::convert::Infallible;

    fn id(self) -> Self::Id {
        unreachable!("You mustn't try to access non-IDs.");
    }
}
```

Some neat things in this: One, `NoId` is a generic placeholder for all ID types - meaning a function signature can always take a struct representation of a database object that doesn't exist in the database yet. Neat thing two, the "inner" Id can not be retrieved from it. (It doesn't exist, after all!) It's [`convert::Infallible`](https://doc.rust-lang.org/std/convert/enum.Infallible.html), the "never" type, meaning any attempt at retrieving that ID will fail at compile time. The compiler won't let us look at the IDs of objects that haven't gotten any yet! One day, when the [`never` type](https://doc.rust-lang.org/std/primitive.never.html) is stabilized, we can use that. In the meantime, this is equivalent enough!

What's more, the NoId type tells serde to not expect an `id` field whenever you deserialize a Bookmark, say from JSON input on an API route:

```rust
/// NoId can be deserialized from any source, even if the field is not
/// present.
impl<'de> Deserialize<'de> for NoId {
    fn deserialize<D>(_deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(NoId)
    }
}
```

I didn't expect this pattern to work so well, but it's been extremely helpful in this CRUD app to not have to write multiple structs with the same (and then out-of-sync) fields in them; and it feels even better to do that with structures that feel like the business logic needs to feel - rather than how the database layout requires them to be.

But how do we use those structs? I thought we weren't making an ORM?

## The read-only/read-write transaction pattern

My app is a really reasonably "normal" one where a logged-in user makes a request, and that triggers one or multiple database operations, and if everything went right, commits any changes. That's a transaction! And what is a transaction other than a collection of business logic that gets applied to a data store?

Hence, the `Transaction` type. It wraps a lower-level (sqlite, in my case) Transaction handle that can't be retrieved (so code can't play with the database directly), and exposes methods that allow code to perform operations on the database:

```rust
struct Transaction {
    txn: sqlx::Transaction<'static, sqlx::sqlite::Sqlite>,
}

impl Transaction {
    /// Commit any changes made in the transaction.
    pub async fn commit(&mut self) { self.txn.commit().await; }

    /// Add a new bookmark and return its ID.
    pub async fn add_bookmark(
        &mut self,
        bm: Bookmark<NoId>,
    ) -> Result<BookmarkId, sqlx::Error> {
        // ...
    }

    /// Retrieve the bookmarks that belong to the current user.
    pub async fn list_bookmarks(
        &mut self
    ) -> Result<Vec<Bookmark<BookmarkId>>> {
        // ...
    }
}
```

So that's neat! But we can get something even neater. You may have seen this [article about running sqlite on a server](https://kerkour.com/sqlite-for-servers), and it recommends having two connection pools: One for read-only ops and one for read-write ops. Let's make a read-only and a read-write transaction that gets created from each of these two pools:

```rust
pub trait TransactionMode {}

pub struct ReadOnly {}
impl TransactionMode for ReadOnly {}

pub struct ReadWrite {}
impl TransactionMode for ReadWrite {}

pub struct Transaction<M: TransactionMode = ReadWrite> {
    txn: sqlx::Transaction<'static, sqlx::sqlite::Sqlite>,
    marker: PhantomData<M>,
}

impl Connection {
    pub async fn begin_for_user(
        &self,
        user: User<UserId>,
    ) -> Result<Transaction<ReadWrite>, sqlx::Error> {
        // ...
    }

    pub async fn begin_ro_for_user(
        &self,
        user: User<UserId>,
    ) -> Result<Transaction<ReadOnly>, RoTransactionError> {
        // ...
    }
}
```

So that gives us two methods - `Connection::begin` and `Connection::begin_ro`. And now, it's pretty easy to split that Transaction implementation into two blocks, one for the read-only operation and one for the read-write one:


```rust
impl Transaction<ReadWrite> {
    /// Commit any changes made in the transaction.
    pub async fn commit(&mut self) { self.txn.commit().await; }

    /// Add a new bookmark and return its ID.
    pub async fn add_bookmark(
        &mut self,
        bm: Bookmark<NoId>,
    ) -> Result<BookmarkId, sqlx::Error> {
        // ...
    }
}

impl<M: TransactionMode> Transaction<M> {
    /// Retrieve the bookmarks that belong to the current user.
    pub async fn list_bookmarks(
        &mut self,
        user_id: UserId,
    ) -> Result<Vec<Bookmark<BookmarkId>>> {
        // ...
    }
}
```

So the ReadWrite impl block looks reasonable, but why is the read-only block generic? That's because the method is available in both - `ReadWrite` *and* `ReadOnly` modes. You could also define methods that *aren't* available in read-write modes - say, if they're heavyweight enough that blocking your single write-capable connection with them would be wasteful. Then you write a `impl Transaction<ReadOnly>` block and the compiler will take care of the rest - any method defined on the "wrong" transaction type is definitely not callable - the compiler won't even be able to find it.

...but the compiler will tell you that you got the wrong mode. Here's how an error looks like if I accidentally call `.commit()` on a read-only transaction:

```console
error[E0599]: no method named `commit` found for struct `DbTransaction` in the current scope
  --> src/lz-web/src/ui.rs:51:9
   |
51 |     txn.commit();
   |         ^^^^^^ method not found in `DbTransaction`
   |
  ::: src/lz-web/src/db.rs:45:1
   |
45 | pub struct DbTransaction<M: lz_db::TransactionMode = lz_db::ReadOnly> {
   | --------------------------------------------------------------------- method `commit` not found for this struct
   |
   = note: the method was found for
           - `DbTransaction<ReadWrite>`
```

### A neat addition: Keeping extra data on the transaction

Every operation is made by a logged-in user, and so the transaction can encode *who* is making the request (since the authentication is checked as part of an [axum extractor](https://docs.rs/axum/latest/axum/extract/index.html)). That gives us the opportunity to _always_ know on whose behalf something is happening, and our data access methods can add restrictions to the query that ensures even faulty/manipulated input data doesn't touch another user's data!

```rust
pub struct Transaction<M: TransactionMode = ReadWrite> {
    txn: sqlx::Transaction<'static, sqlx::sqlite::Sqlite>,
    user: User<UserId>,  // <- this is new!
    marker: PhantomData<M>,
}
```

and... rework the methods that begin a transaction such that they require a username, and you can do stuff like this:

```rust
impl<M: TransactionMode> Transaction<M> {
    /// Retrieve the bookmarks that belong to the current user.
    pub async fn list_bookmarks(
        &mut self
    ) -> Result<Vec<Bookmark<BookmarkId>>> {
        // ...
        query_builder.push("WHERE user_id = ");
        query_builder.push_bind(self.user.id);
        // ...
    }
}
```

All that, together, feels pretty neat (and honestly, not allllll that "clever")! I have no doubt a sufficiently powerful ORM could have let me do these things too, with plugins and various other generics. But doing them this way feels somewhat more right - defining these structs and the logic operating on them allows for a lot of flexibility in coming up with efficient data representations & queries, while the various niceties that the language gives us (automatic json representation/parsing with serde! Transaction rollback on early-return!) make it feel really easy to write and maintain. I've been through a bunch of refactors of this app already, and the basic structure has held up pretty nicely.
