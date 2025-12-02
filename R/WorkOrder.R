#' 序列号查询生成器界面
#'
#' @param colTitles  主页标题
#' @param widthRates 左右比例
#' @param func_left 左函数
#' @param func_right 右函数
#' @param tabTitle 标题
#' @param func_bottom  下面一栏
#'
#' @return 返回值
#' @import tsui
#' @export
#'
#' @examples
#' WorkOrderUI()
WorkOrderUI <- function(tabTitle ='Work Order',
                           colTitles =c('Filter Area','Operation Area','Result Area'),
                           widthRates =c(6,6,12),
                           func_left = WorkOrderUI_left,
                           func_right =WorkOrderUI_right,
                           func_bottom = WorkOrderUI_bottom
) {

  #三栏式设置，可以复制
  res = tsui::uiGen3(tabTitle = tabTitle,colTitles =colTitles,widthRates = widthRates,func_left = func_left,func_right = func_right,func_bottom = func_bottom )
  return(res)

}





#' 请输入文件
#'
#' @return 返回值
#' @export
#'
#' @examples
#' WorkOrderUI_left()
WorkOrderUI_left <- function() {


  res <- tagList(
    tsui::mdl_text2(id ='text_WorkOrder' ,label ='Work Order',value = '')








  )
  return(res)

}


#' 生成区域
#'
#' @return 返回值
#' @export
#'
#' @examples
#' WorkOrderUI_bottom()
WorkOrderUI_right <- function() {
  res <- tagList(

    shiny::actionButton(inputId = 'btn_WorkOrder_view',label = 'Query'),

    tsui::mdl_download_button(id = 'dl_WorkOrder',label = 'Download')




  )
  return(res)

}


#' 生成区域
#'
#' @return 返回值
#' @export
#'
#' @examples
#' WorkOrderUI_bottom()
WorkOrderUI_bottom <- function() {
  res <- tagList(
    tsui::uiScrollX(tsui::mdl_dataTable(id = 'WorkOrder_resultView',label ='结果显示' ))

  )
  return(res)

}
