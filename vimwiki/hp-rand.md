
  const submit = useCallback(
    async () => {
      try {
        // if (reportType === "physical") {
        //   const params: any = {
        //     institution: institution,
        //     print_dt: pSettings.date,
        //     include_current_program: pSettings.includeCurrentProg,
        //     include_earned_degrees: pSettings.includeEarnedDegrees,
        //     include_cum_and_term_gpa: pSettings.includeCumAndTermGpa,
        //     term: terms.find((t: any) => t.term_descr === pSettings.term)?.term,
        //     recipients: [
        //       ...addressData.map(
        //         (addr: Address) => ({properties: AddressToJson(addr)}))
        //     ]
        //   };
        //   const response = await postJSON(`SubmitEnrlVerify?online_print=N`,
        //       {body: JSON.stringify(params)});
        //   setSubmitted(true);
        //   setDoSubmit(false);
        //   return response;
        // } else {
        //   const params: any = {
        //     institution: institution,
        //     term: terms.find((t: any) => t.term_descr === pSettings.term)?.term,
        //     print_dt: "",
        //     include_current_program: pSettings.includeCurrentProg,
        //     include_earned_degrees: pSettings.includeEarnedDegrees,
        //     include_cum_and_term_gpa: pSettings.includeCumAndTermGpa,
        //     recipients: []
        //   };
        //   const response = await postJSON(`SubmitEnrlVerify?online_print=Y`,
        //       {body: JSON.stringify(params)});

        //   if (response.request_results.Success)
        //     window.open(response.url, "_blank");
        //   else {
        //     // TODO(Dustin): Handle failure
        //   }

        //   reset();
        //   setDoSubmit(false);
        //   return response;
        // }
      } catch(e) {
        // TODO(Dustin): Send failure notification
        console.log(e);
        return [];
      }
    }, []);
