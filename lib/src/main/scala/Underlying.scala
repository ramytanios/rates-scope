package lib

trait Underlying:

  def forward(using Market): Either[Error, Forward]
