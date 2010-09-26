unit VForthModuleDateTime;
{$IFDEF fpc}{$MODE delphi}{$ENDIF}
interface

uses
  VForthModule,
  SysUtils,
  Math,
  VForth,
  DateUtils;

type
  EVForthModuleDateTimeError = class(Exception)

  end;

  TVForthModuleDateTime = class(TVForthModule, IVForthModule)
  private

  protected

  public
    procedure Register(AMachine: IVForthMachine); stdcall;
  end;

implementation

uses
  VForthAthom,
  VForthVariants;

{ TVForthModuleMath }

procedure VfNow(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushFloat(Now);
end;

procedure VfSleep(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  Sleep(AMachine.PopInt);
end;

procedure VfDate(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushString(DateToStr(AMachine.PopFloat));
end;

procedure VfTime(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushString(TimeToStr(AMachine.PopFloat));
end;

procedure VfDateTime(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushString(DateTimeToStr(AMachine.PopFloat));
end;

procedure VfYear(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushInt(YearOf(AMachine.PopFloat));
end;

procedure VfMonth(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushInt(MonthOf(AMachine.PopFloat));
end;

procedure VfDay(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushInt(DayOf(AMachine.PopFloat));
end;

procedure VfDayOfWeek(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushInt(DayOfTheWeek(AMachine.PopFloat));
end;

procedure VfDayOfMonth(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushInt(DayOfTheMonth(AMachine.PopFloat));
end;

procedure VfDayOfYear(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushInt(DayOfTheYear(AMachine.PopFloat));
end;

procedure VfMSecond(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushInt(MilliSecondOf(AMachine.PopFloat));
end;

procedure VfSecond(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushInt(SecondOf(AMachine.PopFloat));
end;

procedure VfMinute(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushInt(MinuteOf(AMachine.PopFloat));
end;

procedure VfHour(AMachine: IVForthMachine; AAthom: IVForthAthom; PAthomStr : PWideChar); stdcall;
begin
  AMachine.PushInt(HourOf(AMachine.PopFloat));
end;

procedure TVForthModuleDateTime.Register(AMachine: IVForthMachine);
begin
  AMachine.AddAthom(CreateVForthSystemAthom('now', Self, VfNow));
  AMachine.AddAthom(CreateVForthSystemAthom('sleep', Self, VfSleep));
  AMachine.AddAthom(CreateVForthSystemAthom('date', Self, VfDate));
  AMachine.AddAthom(CreateVForthSystemAthom('time', Self, VfTime));
  AMachine.AddAthom(CreateVForthSystemAthom('datetime', Self, VfDateTime));

  AMachine.AddAthom(CreateVForthSystemAthom('year', Self, VfYear));
  AMachine.AddAthom(CreateVForthSystemAthom('month', Self, VfMonth));
  AMachine.AddAthom(CreateVForthSystemAthom('day', Self, VfDay));
  AMachine.AddAthom(CreateVForthSystemAthom('DayOfWeek', Self, VfDayOfWeek));
  AMachine.AddAthom(CreateVForthSystemAthom('DayOfMonth', Self, VfDayOfMonth));
  AMachine.AddAthom(CreateVForthSystemAthom('DayOfYear', Self, VfDayOfYear));

  AMachine.AddAthom(CreateVForthSystemAthom('mSecond', Self, VfMSecond));
  AMachine.AddAthom(CreateVForthSystemAthom('second', Self, VfSecond));
  AMachine.AddAthom(CreateVForthSystemAthom('minute', Self, VfMinute));
  AMachine.AddAthom(CreateVForthSystemAthom('hour', Self, VfHour));
end;

end.
