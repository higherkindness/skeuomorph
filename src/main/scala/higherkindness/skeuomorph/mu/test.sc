import higherkindness.skeuomorph.mu._
import MuF._
import SerializationType._
import Service._
import CompressionType._
List(
  Mu(TRecord(UserName,Some(foo.bar),List(),None,List(Field(value,List(),None,Some(Ascending),Mu(TString()))))),
  Mu(TRecord(Password,Some(foo.bar),List(),None,List(Field(hash,List(),None,Some(Ascending),Mu(TString())), Field(salt,List(),None,Some(Ascending),Mu(TString()))))),
  Mu(
    TRecord(LoginCredentials,Some(foo.bar),List(),None,
      List(Field(username,List(),None,Some(Ascending),Mu(TRecord(UserName,Some(foo.bar),List(),None,List(Field(value,List(),None,Some(Ascending),Mu(TString()))))))
        , Field(password,List(),None,Some(Ascending),Mu(TRecord(Password,Some(foo.bar),List(),None,List(Field(hash,List(),None,Some(Ascending),Mu(TString())), Field(salt,List(),None,Some(Ascending),Mu(TString())))))))))
)
