unit CurrentUser;

interface

type
  TCurrentUser = record
    UserID: Integer;
    Username: string;
    IsAdmin: Boolean;
  end;

var
  AppUser: TCurrentUser;

implementation

end.
