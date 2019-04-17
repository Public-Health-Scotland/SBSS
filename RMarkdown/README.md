# How to Produce a Publication Report in Word using RMarkdown

Basic R markdown template for producing summary and full report. 

## Download the necessary files

To download the necessary files, click on *Clone or download -> Download ZIP* and save it to the folder of your choice. 
![Download zip example](https://github.com/NHS-NSS-transforming-publications/Images/blob/master/RMarkdown7.PNG)
* Go to the zip file, right click on it and choose *WinZip -> Extract to here*.
* You should see the files that are below.

	![Folder example](https://github.com/NHS-NSS-transforming-publications/Images/blob/master/RMarkdown_Basic2.png)

	* The ISD-NATIONAL-STATS-REPORT.Rmd file is the **RMarkdown** file that you open in RStudio, modify as desired, and 	run/knit to create a Word file.  The final Word file will have the exact same name as this file except for the 	extension, which will be .docx.  So in this case, the new file being created will be named ISD-NATIONAL-STATS-REPORT.docx
  	* The kitemark_tcm97-17949.jpg file is simply this image which RMarkdown will insert into the final MS Word Publications Report template:
  
  		![Kitemark](https://github.com/NHS-NSS-transforming-publications/RMarkdown_Basic/blob/master/kitemark_tcm97-17949.jpg)
	
  * The ISD-NATIONAL-STATS-REPORT_TEMPLATE.docx file is the file in which you set styles for headings and tables. The RMarkdown file will import the styles, but not the content of this file. You ideally shouldn’t need to modify this since we’ve already set the styles, but this is the file you would change if you wanted to change them.
  *	The Cover_Page.docx file is used to import a custom cover page and a custom footer into the user’s settings.  This file is no longer needed and can be erased after those steps are completed.

## One-time preparation steps for each user
Open the Cover_Page.docx file in Word.
* Save the Cover Page.
	* Press Ctrl + A to select all contents. Go to *Insert – Cover Page – Save Selection to Cover Page Gallery*. Give it a name (e.g. ISD_Publication_Report) and click OK.
  
	![Cover page example](https://github.com/NHS-NSS-transforming-publications/Images/blob/master/RMarkdown6.PNG)
	
* Save the footer:
	* Double click on the footer, and select the whole footer like this by pressing Ctrl + A.
	
  	![Footer example](https://github.com/NHS-NSS-transforming-publications/Images/blob/master/RMarkdown2.PNG)
	
  	* Then go to *Insert – Footer – Save Selection to Footer Gallery*. Give it a name (e.g. ISD_Publication_Footer) and click OK.
	
* Save the VBA macro:
	* Go to *View – Macros – View Macros*. Type the macro name you want to save this as (e.g. SetStyleOfTables) and click Create. It will open up the VBA developer window.
	* Copy the following code to the developer window and click the Save button to save the macro.
	
```vba
Sub SetStyleOfAllTablesAndPreserveAlignment()
 ' SetStyleOfAllTablesAndPreserveAlignment Macro
 
     For Each objTable In ActiveDocument.Tables
         
         '******This first section is for recording the old column alignments*****'
         numCols = objTable.Columns.Count 'first find the number of columns within the table.
         ReDim oldColumnAlignments(numCols) As Integer 'initialize an integer array of length 'numCols'.
         column_index = 0
         For Each tableColumn In objTable.Columns
             oldColumnAlignments(column_index) = tableColumn.Cells(1).Range.ParagraphFormat.Alignment
             column_index = column_index + 1
         Next tableColumn
         '************************************************************************'
        
        
         '-------This section changes the styles of the tables to what they should be. -------'
         objTable.Style = "ISD_Pubs_Tables"
         PreviousBookmarkID = objTable.Range.PreviousBookmarkID
         PreviousBookmarkName = ActiveDocument.Range.Bookmarks(PreviousBookmarkID)
         
         If PreviousBookmarkName = "glossary" Then
             objTable.Style = "Glossary_Style"
         End If
         
         If PreviousBookmarkName = "tableA" Then 'Change these as needed for each style type!
             objTable.Style = "TableA_Style"
         End If
         
         If PreviousBookmarkName = "tableB" Then
             objTable.Style = "TableB_Style"
         End If
         
         If PreviousBookmarkName = "tableC" Then
             objTable.Style = "TableC_Style"
         End If
         '------------------------------------------------------------------------------------'
         
         
         '^^^^This last section sets the alignments of each column of the table to what they were ^^^^'
         '^^^^before the style of the table was changed.^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'
         i = 0
         For Each tableColumn In objTable.Columns
             tableColumn.Select
             Selection.ParagraphFormat.Alignment = oldColumnAlignments(i)
             i = i + 1
         Next tableColumn
         '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'
         
         objTable.PreferredWidth = 100 'Sets the Preferred Table width to 100% of the width of the page.
         
     Next objTable
 
 End Sub
```

* These three steps of saving the VBA macro, saving the footer, and saving the cover page should only have to be done once ever for each user.  The macro, footer, and cover page should now be associated with the user’s Word setup and not with any particular Word file.  
* Close the Cover_Page.docx file in Word.
* Delete the file Cover_Page.docx as this is no longer needed

## One-time preparation steps for each publication report

When you are changing the RMarkdown template to make the Publication Report that you want to create, you will inevitably have to change many parts of the template.  For instance, you’ll have to change the wording, titles, plots, tables, images, as well as many other parts of the report.  The following two preparation steps are just a reminder to check or modify these things that might easily be overlooked, namely the location of the ‘Rate this Publication’ link and the possible commenting out of the “Early Access for Management Information” and “Early Access for Quality Assurance” sections.

*  Open the ISD-NATIONAL-STATS-REPORT.Rmd file in RStudio.
	* **Determine if notice of “Early Access for Management Information” and “Early Access for Quality Assurance” are needed within your publication report.**
		* In ISD-NATIONAL-STATS-REPORT.Rmd, within **Appendix 3 – Early Access Details**, not every publication will have the information for “Early Access for Management Information” and “Early Access for Quality Assurance”. So each team should judge for each publication if these sections are needed. If they are not, please comment them out (using Ctrl+Shift+C) within the RMarkdown script so that the text will not show in the final MS Word output.
		
```
**Early Access for Management Information**  
These statistics will also have been made available to those who needed access to 'management information', ie as part of the delivery of health and care:

<br>

**Early Access for Quality Assurance**  
These statistics will also have been made available to those who needed access to help quality assure the publication:
```


* Set the location of the 'Rate this publication' link within the RMarkdown file.

	![Rate publication link](https://github.com/NHS-NSS-transforming-publications/Images/blob/master/RMarkdown_Basic3.png)
	
	* Just set the location in the code above to wherever the link for rating the publication should go to.
* Again, these two steps just completed above only have to be performed once per publication, unless something would warrant a change in these areas of the publication in the future.


## Routine steps performed every time a publication is produced using RMarkdown
* Open the ISD-NATIONAL-STATS-REPORT.Rmd file in RStudio.
* Run/Knit the RMarkdown file.

	![Knit document example](https://github.com/NHS-NSS-transforming-publications/Images/blob/master/RMarkdown_Basic4.png)

* Open the MS Word document (ISD-NATIONAL-STATS-REPORT.docx) that was just created.
* Insert the Cover Page into the publication report. 
	* Open ISD-NATIONAL-STATS-REPORT.docx. Go to *Insert – Cover Page*. Scroll down to the general section and select the cover page template you saved previously. Notice now that the text “Information Services Division” appears much lower than it should. To fix that, go to *Page Layout – Margins – Custom Margins*. Set the “Top” number as 0.62 cm. Now it should be back in the proper location.
* Insert the Footer into the publication report. 
	* Go to *Insert – Footer*. Scroll down to the general section and select the footer template you saved previously. Now the footer has been fully inserted into the document.
* Set the Table Formatting Using the VBA Macro.
	* Go to *View – Macros – View Macros*. Select the macro you saved previously, and click Run. Now all the tables in the output document should be nicely formatted.
* Insert Table of Contents (TOC)
	* We need to insert the TOC manually as we cannot find a way to insert it on a specific page in the RMarkdown script. **Please note: this step should only be done after running the macro setting the table formats!  Otherwise the formatting of the tables won’t be in the correct order!**
	* Click on the end of last text line on the page “This is a National Publication” (page number 1).

	![National Statistics](https://github.com/NHS-NSS-transforming-publications/Images/blob/master/RMarkdown5.PNG)
	
	* Go to *Insert – Page Break*, so that a new blank page will be inserted.
	* Go to *References – Table of Contents*. Choose Built-in template Automatic Table 1. Now the TOC has been fully inserted.

Congratulations!  You have now completed creating a Word Publications National Stats Report Template from a RMarkdown template.  Feel free to play around with the RMarkdown file to see how the MS Word file changes in response to your modifications.

	



