##' Get Stocks CPC
##'
##' This function pulls all the cpc codes for the stocks module.
##'
##' @export
##'



getStocksCPC = function(){
  item <- ReadDatatable("fbs_stocks_comm_codes")[fbs_key == "measuredItemCPC", 
                                                 fbs_code]
}