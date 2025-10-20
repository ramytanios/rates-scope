package lib

trait Underlying[T]:

  def forward(using Market[T]): Either[Error, Forward[T]]
