# Enrollment Verification TODO's

## TODO

## FEEDBACK

## DONE
- [X] Can you place the Country dropdown below the Name field? The value selected for the Country will affect the address fields that are available.
- [X] Addresses is misspelled in the "Confirm addresses" modal. It's spelled adresses instead of addresses.
- [X] It looks like the values for the Address Type may be hardcoded. We should only include address types that apply to the student. For example, there shouldn't be any values for CX0001.
- [X] The address fields should not be pre-populated with values unless they come from the selected address type. The fields should be blank for CX0001 since that student does not have any addresses and no data is returned as part of the student_addresses collection.
- [X] The Copies field should be defaulted to 1 instead of 2.
- [X] Can you remove the Academic Institution dropdown from the page? We shouldn't have included it in the design since our convention is to limit data to the current institution (for most pages). I will request to have the back end limit the institutions to those where the student has data so there will situations where a student should not be able to submit data for this page for an institution. We will need to display a message to the student in that case letting them know they do not have access to the page for that institution. I will request the backend return the message.
- [X] For the Printing Date (displays when "Physical Version of Enrollment Verification mailed from Institution" is selected), we shouldn't allow students to select a date that is less than the current date.
- [X] There should be a way for students to remove the Address 2 field in case they select the + button by accident.
- [X] In the "Confirm addresses" modal, the address labels should come from the address labels collection under countries in the bootstrap data.
- [X] When there is more than one address in the "Confirm addresses" modal, the addresses should not display side-by-side: https://app.zeplin.io/project/5bf300f9e6db113f294195eb/screen/5d84ed1a63a5d01288864e56
- [X] When a user enters/selects data on the Enrollment Verification page and navigates away, we should display: https://app.zeplin.io/project/5bf300f9e6db113f294195eb/screen/5d84ed1998acc57892b04348. If that isn't possible, we should display the browser alert warning the student that they have unsaved data.
- [X] Before we break to the mobile view (each field on its on line), can you make it so the Printing Date field displays on the same line as the Academic Institution dropdown and Desired Term dropdown?
- [X] When a user selects a country, the address fields should be updated based on the address labels collection under countries in the bootstrap data.
- [X] In the confirm address modal, the address should display within the available space when only one address is specified. There is currently blank space to the right of the address.
- [X] The Return Home button doesn't work. I don't think this button is necessary though so I'm ok with it being removed.
- [X] Users should be able to select all of the terms for the Desired Term dropdown. The designs show an "All terms" option for the Desired Term dropdown. We should pass a blank term to the SubmitEnrlVerify service.
