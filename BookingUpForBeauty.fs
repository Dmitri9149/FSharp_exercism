module BookingUpForBeauty
// The following line is needed to use the DateTime type
open System
let schedule (appointmentDateDescription: string): DateTime = 
    DateTime.Parse(appointmentDateDescription)
let hasPassed (appointmentDate: DateTime): bool = 
    let compare = DateTime.Compare(appointmentDate, DateTime.Now )
    if compare < 0 then true
    else false
let isAfternoonAppointment (appointmentDate: DateTime): bool =
    if appointmentDate.Hour >= 12 && appointmentDate.Hour < 18 then true 
    else false  
let description (appointmentDate: DateTime): string = 
    "You have an appointment on " + appointmentDate.ToString() + "."
let anniversaryDate(): DateTime = (DateTime(DateTime.Now.Year, 9, 15))