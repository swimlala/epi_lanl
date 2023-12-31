CDF       
      	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       	DATE_TIME         N_PROF        N_PARAM       N_LEVELS  7   N_CALIB       	N_HISTORY            	   title         Argo float vertical profile    institution       BODC   source        
Argo float     history       06-May-2016 13:52:41Zcreation      
references        (http://www.argodatamgt.org/Documentation   comment       bThis netCDF file is generated using BODC's argoReader and netCDF writer software (argo@bodc.ac.uk)     user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    <H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    <X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    <\   REFERENCE_DATE_TIME                	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    <`   DATE_CREATION                  	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    <p   DATE_UPDATE                	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    <�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    <�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  <�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  <�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  =   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        =H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    =L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    =P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     =T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    =t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    =x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     =|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     =�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     =�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    =�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
_FillValue        A.�~       
resolution        >�E�vQ�        =�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    =�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�E�vQ�        =�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            =�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            =�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    >   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    >   VERTICAL_SAMPLING_SCHEME                   	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    >   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ?   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ?   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ?   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ?   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                    	valid_max         @�p        axis      Z      
_FillValue        G�O�   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�������     �  ?    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @          	valid_max         @D�        
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  G�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �         	valid_max         @D         
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  P�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  Y�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  [�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ^$   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                    	valid_max         @�p        conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�������     �  `\   PSAL_ADJUSTED            
      
   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @          	valid_max         @D�        conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  i8   TEMP_ADJUSTED            
      
   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �         	valid_max         @D         conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  r   PRES_ADJUSTED_QC         
         	long_name         quality flag   standard_name         PRES_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                 8  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   standard_name         PSAL_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                 8  }(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   standard_name         TEMP_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                 8  `   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         PRES_ADJUSTED_ERROR    units         decibar    conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�������     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         PSAL_ADJUSTED_ERROR    units         psu    conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  �t   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         TEMP_ADJUSTED_ERROR    units         degree_Celsius     conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    source_name       	PARAMETER      conventions       Argo reference table 3     
_FillValue                  `  �,   SCIENTIFIC_CALIB_EQUATION               	             	long_name         'Calibration equation for this parameter    source_name       SCIENTIFIC_CALIB_EQUATION      
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	             	long_name         *Calibration coefficients for this equation     source_name       SCIENTIFIC_CALIB_COEFFICIENT   
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	             	long_name         .Comment applying to this parameter calibration     source_name       SCIENTIFIC_CALIB_COMMENT   
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	            	long_name         Date of calibration    source_name       SCIENTIFIC_CALIB_DATE      conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     source_name       HISTORY_INSTITUTION    conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    source_name       HISTORY_STEP   conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    source_name       HISTORY_SOFTWARE   conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     source_name       HISTORY_SOFTWARE_RELEASE   conventions       Institution dependent      
_FillValue                    �4   HISTORY_REFERENCE                        	long_name         Reference of database      source_name       HISTORY_REFERENCE      conventions       Institution dependent      
_FillValue                 �  �P   HISTORY_DATE                     	long_name         #Date the history record was created    source_name       HISTORY_DATE   conventions       YYYYMMDDHHMISS     
_FillValue                  d  �   HISTORY_ACTION                       	long_name         Action performed on data   source_name       HISTORY_ACTION     conventions       Argo reference table 7     
_FillValue                    �t   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   source_name       HISTORY_PARAMETER      conventions       Argo reference table 3     
_FillValue                  p  ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   source_name       HISTORY_START_PRES     units         decibar    
_FillValue        G�O�        �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    source_name       HISTORY_STOP_PRES      units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    source_name       HISTORY_PREVIOUS_VALUE     
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   source_name       HISTORY_QCTEST     conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  p  �TArgo profile    3.1 1.2 19500101000000  20210226054841  20210226054841  1901875 Argo UK                                                         Brian King                                                      PSAL            TEMP            PRES               lA   BO  126294                          2C  D   NAVIS_EBR                       0651                            BGCi_SBE63 030916               869 @�䏀0��1   @�䏀0���C�f�A�@D�n��P1   GPS     Primary sampling: mixed                                                                                                                                                                                                                                            A   A   A   ����?�  @   @@  @�  @�  @�  @�  A   A  A   A0  A@  AP  A`  Ap  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B  B  B  B  B   B$  B(  B,  B0  B4  B8  B<  B@  BD  BH  BL  BP  BT  BX  B\  B`  Bd  Bh  Bl  Bp  Bt  Bx  B|  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C  C  C  C  C	  C  C  C  C  C  C  C  C  C  C  C  C!  C#  C%  C'  C)  C+  C-  C/  C1  C3  C5  C7  C9  C;  C=  C?  CA  CC  CE  CF�fCI  CK  CM  CO  CQ  CS  CU  CW  CY  C[  C]  C_  Ca  Cc  Ce  Cg  Ci  Ck  Cm  Co  Cq  Cs  Cu  Cw  Cy  C{  C}  C  C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C CÀ CĀ Cŀ Cƀ Cǀ CȀ Cɀ Cʀ Cˀ C̀ C̀ C΀ Cπ CЀ Cр CҀ CӀ CԀ CՀ Cր C׀ C؀ Cـ Cڀ Cۀ C܀ C݀ Cހ C߀ C�� C� C� C� C� C� C� C� C� C� C� C� C� C� C� C� C�� C� C� C� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D @ D � D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D	@ D	� D
@ D
� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D @ D � D!@ D!� D"@ D"� D#@ D#� D$@ D$� D%@ D%� D&@ D&� D'@ D'� D(@ D(� D)@ D)� D*@ D*� D+@ D+� D,@ D,� D-@ D-� D.@ D.� D/@ D/� D0@ D0� D1@ D1� D2@ D2� D3@ D3� D4@ D4� D5@ D5� D6@ D6� D7@ D7� D8@ D8� D9@ D9� D:@ D:� D;@ D;� D<@ D<� D=@ D=� D>@ D>� D?FfD?� D@@ D@� DA@ DA� DB@ DB� DC@ DC� DD@ DD� DE@ DE� DF@ DF��DG@ DG� DH@ DH� DI@ DI� DJ@ DJ� DK@ DK� DL@ DL� DM@ DM� DN@ DN� DO@ DO� DP@ DP� DQ@ DQ� DR@ DR� DS@ DS� DT@ DT� DU@ DU� DV@ DV� DW@ DW� DX@ DX� DY@ DY� DZ@ DZ� D[@ D[� D\@ D\� D]@ D]� D^9�D^� D_@ D_� D`@ D`� Da@ Da� Db@ Db� Dc@ Dc� Dd@ Dd� De@ De� Df@ Df� Dg@ Dg� Dh@ Dh� Di@ Di� Dj@ Dj� Dk@ Dk� Dl@ Dl� Dm@ Dm� Dn@ Dn� Do@ Do� Dp@ Dp� Dq@ Dq� Dr@ Dr� Ds@ Ds� Dt@ Dt� Du@ Du� Dv@ Dv� Dw@ Dw� Dx@ Dx� Dy@ Dy� Dz@ D{@ D|@ D}  BS�BJ�BO�BP�BO�BO�BO�BP�BP�BQ�BQ�BR�BQ�BR�BC�BR�BR�BQ�BQ�BQ�BP�BN�BM�BM�BL�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BJ�BK�BK�BK�BL�BJ�BJ�BH�BH�BG�BG�BG�BG�BF�BE�BE�BC�BA�B=qB<jB:^B9XB7LB33B/B+B'�B'�B%�B"�B �B�B�B{BPB1B��B��B�B�B�`B�HB�BB�)B�HB�sB�B�B�B��B��B��B��B��B��B��BBBBB��B��B�B�B�
B�B�B��B��B��B��B��B��B��B��B��BɺB�qB�dB��B��B��B��B��B�bB�JB�+B�B{�Bo�BZBS�BdZBVBP�B8RB!�B�B�B �B!�B�B�B.B6FB49B(�B�B$�B"�BbB��B�B��B��B%B��B�B�B�BB�/B�/BɺB�wBɺB�B+B{B�B�B�BJBB�B�B�ZB�#B�#B�B�B��B��BɺB��B�^B�9B�{B�B� B�B�B~�Bx�Bt�Bw�Bw�Bw�Bw�Br�Bp�Bq�Bq�Bp�Bo�Bn�BhsB`BB[#BO�BI�BF�BC�B@�B>wB:^B49B2-B/B)�B(�B(�B&�B#�B�B�B�B�B�BoB\BVBJB1BB  B  B��B��B��B��B��B�B�B�ZB�BB�;B�;B�5B�/B�B��B��BɺBǮBŢBƨB��B��B�;B��B+B.Be`Bk�Bt�Bw�Bv�Bu�BiyB^5BT�B=qB'�B��B�mB�sB�sB�sBDBoB�B��B}�Bm�Bo�B~�B��B�B�!B��B�bB�{B��B��B��B�B�!B�B��B�B��B��B��B�B�3B�B��B��B�hB�\B�PB�1B�=B�hB�\B�\B�PB�DB�%B� Bt�B^5BZBVBR�BM�BH�BF�BD�BA�B>wB7LB.B)�B)�B)�B'�B%�B"�B�B�B�B�B�B�B�BoB
=BB
��B
�mB
�;B
��B
��B
ŢB
ĜB
B
�wB
�dB
�?B
�-B
�-B
�'B
�!B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�\B
�1B
�1B
�+B
�1B
�+B
�+B
�+B
�B
�B
~�B
}�B
|�B
z�B
y�B
v�B
t�B
q�B
k�B
ffB
cTB
aHB
_;B
\)B
YB
XB
W
B
VB
S�B
R�B
R�B
R�B
Q�B
Q�B
O�B
M�B
L�B
J�B
F�B
>wB
7LB
49B
0!B
-B
%�B
�B
oB
PB
B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�`B	�NB	�;B	�#B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	ǮB	ŢB	��B	�jB	�dB	�XB	�LB	�LB	�LB	�LB	�LB	�LB	�FB	�LB	�LB	�RB	�RB	�dB	�jB	�qB	�qB	�qB	�3B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	�hB	�hB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��A���A���A���A��uA��A��PA��A���A���A�ĜA��^A���A�ĜA���A���A���A�ĜA��^A��-A��-A���A��+A�ffA�bNA�I�A�=qA�=qA�?}A�?}A�=qA�?}A�E�A�I�A�G�A�E�A�A�A�A�A�C�A�A�A�O�A�XA�?}A�+A�VA�%A�A�A�A���A���A��mA���A��FA���A�jA�?}A�33A�+A�A�ĜA�hsA�&�A��A��mA���A���A�|�A�^5A�"�A��7A�"�A��DA�oA���A�jA��A���A�|�A�C�A�A�;A�  A�JA��A�+A�5?A�7LA�9XA�9XA�9XA�9XA�9XA�=qA�1'A��A�A�;A�A|��A|��A|�HA|�/A|�A|��A|�A|{A{��A{A{��A{x�A{K�A{G�Az��Ay��Ay��Ax^5Aw�mAw�Av��Av�DAv �Au��Au�AuG�At��As��ArM�Aq�#Aq�mApĜApv�AnVAl�+Al-Al9XAlQ�AlI�Ak�7Aj��AkK�Ak\)Ak7LAj1'Ah��Ai`BAi7LAg�mAe�Ael�Aet�Ael�AfbAe�Ad��Ac�Ab��Ab1'Aa��A`(�A_VA_�hAa�PAb(�Ab�+Ab�\Ab�+Ab1Aa&�A`�DA^�HA^1'A]��A\�`A\�jA\r�A\I�A\E�A\JA[hsAZ��AZ-AYƨAW33AU�mAU��AVAU��AU��AU"�AT��AT��AT�uAT��AT�\AT$�AS�-AS�ASoAR��AR��ARn�AQx�AP��AP �AN�ANbNAN�AM�#AM�hAMdZAM%ALE�ALbAK�#AK`BAKXAKK�AK�AJ��AJVAJ  AI�AIdZAIC�AH��AH��AH�DAHffAHAG\)AG/AG�AF��AFjAF1'AE�AE�FAE��AEC�AD1AC�AC��AC��AC�hAC�AC&�ABA�AA��AA�AA;dA@��A@��A@�jAAoAA��AB�AC\)AE�hAH�AHz�AH�AIVAIVAH�AG�FAF��AF1'AD��ACoA?33A>bA>�A>{A>�A@A@~�A=�#A9�A6~�A4��A4z�A5G�A6�DA7C�A7
=A5VA4v�A4ffA4�\A4�A4ĜA4�HA4ȴA4A�A3�#A3��A3%A2bNA2bNA2ȴA2�HA2I�A1A0 �A/C�A.�A.z�A-��A-�7A-x�A-&�A,��A,�9A,jA+��A+l�A*1'A(1'A'�A'��A'G�A&~�A%�mA%�PA%`BA$�A$��A#�PA"9XA!��A!x�A ��A �RA ��A =qA+A��Av�AQ�A5?AbA�A��A��A�A�AjA|�A��A��A�AS�A�A�uA�mA��AbNAI�A$�A  A�mA��A�/A�jA�A�hAZA�AƨA�A	�wAffAbAJA$�AJA{A�A�
A?}A��AffAM�AA�FA\)A/A�RA\)Av�A�mA��AC�A �RA  �@��F@�\)@��@�5?@���@���@���@��7@�`B@��9@�b@��w@�C�@�J@���@��
@�~�@��@�Z@�h@�I�@�^@�dZ@�p�@�@���@�{@���@ו�@�@���@֗�@պ^@��`@ԋD@���@ҧ�@�p�@�;d@·+@�(�@��@�ƨ@�o@���@��H@Ƨ�@Ƈ+@�n�@�5?@ř�@�x�@�p�@�V@��@��`@���@ģ�@ě�@ēu@�z�@Ý�@��@§�@��T@�
=@�?}@��@��@��
@���@���@��
@��
@��
@���@��m@��@�(�@�I�@��u@�z�@��D@���@��@��@�9X@�b@�b@�|�@�33@�"�@��@��H@�ȴ@��@���@�$�@��T@�hs@��`@��@��`@�%@�V@��@�V@�%@��@��@��/@�C�@��+@��@��7@�7L@�j@���@��@���@��R@���@��#@�Q�@��y@���@���@�V@�^5@��@�^5@��-@���@�p�@���@��@�"�@�"�@�o@��@��@��@�o@�@��@�^5@�@�hs@�V@���@��@���@�Ĝ@��9@��@�j@�bN@�bN@�Z@�A�@�1'@�9X@�I�@�Q�@�r�@��@��u@��j@�Q�@�Ĝ@�\)@�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 �L��?�ff@33@S33@���@���@ə�@陚A��A��A$��A4��AD��AT��Ad��At��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B	33B33B33B33B33B33B!33B%33B)33B-33B133B533B933B=33BA33BE33BI33BM33BQ33BU33BY33B]33Ba33Be33Bi33Bm33Bq33Bu33By33B}33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�Bę�Bƙ�Bș�Bʙ�B̙�BΙ�BЙ�Bҙ�Bԙ�B֙�Bؙ�Bڙ�Bܙ�Bޙ�B���B♚B䙚B晚B虚BꙚB왚BB�B�B���B���B���B���B���CL�CL�CL�CL�C	L�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C!L�C#L�C%L�C'L�C)L�C+L�C-L�C/L�C1L�C3L�C5L�C7L�C9L�C;L�C=L�C?L�CAL�CCL�CEL�CG33CIL�CKL�CML�COL�CQL�CSL�CUL�CWL�CYL�C[L�C]L�C_L�CaL�CcL�CeL�CgL�CiL�CkL�CmL�CoL�CqL�CsL�CuL�CwL�CyL�C{L�C}L�CL�C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC¦fCæfCĦfCŦfCƦfCǦfCȦfCɦfCʦfC˦fC̦fCͦfCΦfCϦfCЦfCѦfCҦfCӦfCԦfCզfC֦fCצfCئfC٦fCڦfCۦfCܦfCݦfCަfCߦfC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC��fC�fC�fC�fC�fC�fC�fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD S3D �3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3D	S3D	�3D
S3D
�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3DS3D�3D S3D �3D!S3D!�3D"S3D"�3D#S3D#�3D$S3D$�3D%S3D%�3D&S3D&�3D'S3D'�3D(S3D(�3D)S3D)�3D*S3D*�3D+S3D+�3D,S3D,�3D-S3D-�3D.S3D.�3D/S3D/�3D0S3D0�3D1S3D1�3D2S3D2�3D3S3D3�3D4S3D4�3D5S3D5�3D6S3D6�3D7S3D7�3D8S3D8�3D9S3D9�3D:S3D:�3D;S3D;�3D<S3D<�3D=S3D=�3D>S3D>�3D?Y�D?�3D@S3D@�3DAS3DA�3DBS3DB�3DCS3DC�3DDS3DD�3DES3DE�3DFS3DF��DGS3DG�3DHS3DH�3DIS3DI�3DJS3DJ�3DKS3DK�3DLS3DL�3DMS3DM�3DNS3DN�3DOS3DO�3DPS3DP�3DQS3DQ�3DRS3DR�3DSS3DS�3DTS3DT�3DUS3DU�3DVS3DV�3DWS3DW�3DXS3DX�3DYS3DY�3DZS3DZ�3D[S3D[�3D\S3D\�3D]S3D]�3D^L�D^�3D_S3D_�3D`S3D`�3DaS3Da�3DbS3Db�3DcS3Dc�3DdS3Dd�3DeS3De�3DfS3Df�3DgS3Dg�3DhS3Dh�3DiS3Di�3DjS3Dj�3DkS3Dk�3DlS3Dl�3DmS3Dm�3DnS3Dn�3DoS3Do�3DpS3Dp�3DqS3Dq�3DrS3Dr�3DsS3Ds�3DtS3Dt�3DuS3Du�3DvS3Dv�3DwS3Dw�3DxS3Dx�3DyS3Dy�3DzS3D{S3D|S3D}33BS�BKnBPBQSBO�BO�BO(BP�BPBR0BQ�BSBQ�BR�BD^BR�BSUBR>BRBRpBR(BO�BN5BN�BM;BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BK�BJ�BK�BKbBK�BM�BK�BK�BIBH�BG�BG�BH
BG�BGaBF{BF�BD�BC0B>�B<�B:�B:�B9�B6uB1�B,�B(qB)+B'�B$B"IB!�B B�B�BqB �B��B��B�=B�:B�B�BܗB�B�B�B�B�bB��B��B��B��B��B��B�B�BB�B<BNBBڽBكB�+B�/B�XB�B�jB�B�LB΀BΡB͞B�MB��B�aB��B�ZB��B�:B��B�B�aB��B��B��B�hB�BsB[fBT<Bf�BWBVB<�B"�B�B�B �B#�BB&B-�B6�B6�B+�B�B%MB%�BB�2B�B��B��B=B  B�wB�B�B޾B�B�hB�vB�ZB�BUBIB�B�B�B�B�B��B��B�B۰B��B�xB�BՐB�mB�7B�B��B�B��B�mB�B�'B��B�ABzBt�Bw�Bw�Bw�Bx�Bs�Bq*Br�BrrBp�Bp<Bp�Bj�Ba�B^ BQTBJoBGVBDMB@�B?bB<,B4�B2�B0:B*%B)B)hB'�B$�B �ByBSB�B6BKB�B�B=B	�B|B >B �B��B�}B��B�TB��B�B�B�RB��B�IB�FB�eB�B�%B�zB�!B�fB�WBƔB�RB��BҪB� B�mB�B'�Bc�BjuBtcBw�Bw3Bx�Bk�B_�BX�BA�B1B��B�xB�wB�5B�#B
VB�B��B��B�DBntBm�B{�B��B��B��B�VB��B�B��B�B��B�FB�xB�B��B�B��B��B��B��B��B��B��B��B��B�[B��B�uB�|B�!B��B�B�B��B�BB�1By�B_GBZ�BV�BT�BO[BI�BG%BE�BBsBA'B:�B/(B*�B+GB*�B(DB&�B%vB B	B�B�B�B�B�B�B�B�B
�B
��B
�B
�,B
�B
�9B
�EB
��B
�FB
�&B
�]B
�}B
��B
��B
�jB
��B
�B
�jB
��B
�iB
��B
��B
�B
�sB
�"B
��B
�=B
�@B
��B
�lB
�#B
��B
�zB
��B
�_B
�B
~BB
}�B
{�B
z�B
wSB
vB
u3B
m�B
g�B
dB
bVB
`�B
]�B
Y�B
X�B
W�B
V�B
TRB
S/B
SB
SJB
R2B
R�B
P�B
NJB
M|B
L~B
LZB
@�B
9*B
6B
1QB
0�B
,�B
8B
�B
�B
iB
�B	��B	��B	�=B	�gB	��B	��B	�B	��B	�B	�UB	�=B	�'B	�PB	�pB	�cB	�/B	��B	�B	�)B	� B	�GB	�&B	�B	�EB	ҵB	�B	��B	�dB	��B	�)B	�B	�B	��B	��B	�B	��B	ˀB	�aB	��B	ɣB	��B	��B	��B	��B	�fB	�UB	�JB	�UB	�UB	�\B	�5B	�EB	�B	�)B	�B	��B	�]B	�cB	��B	��B	��B	�6B	�B	��B	�RB	��B	�$B	��B	��B	��B	�,B	�zB	�1B	�qB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�+B	��B	��B	�{B	�0B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�qB	��B	�{B	��B	�dB	��B	�1B	��B	��B	��B	��B	�TB	�)B	��B	��B	��B	��B	��B	��B	��B	��B	�^B	�B	�jB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�IB	��B	��B	��A���A���A���A��uA��A��PA��A���A���A�ĜA��^A���A�ĜA���A���A���A�ĜA��^A��-A��-A���A��+A�ffA�bNA�I�A�=qA�=qA�?}A�?}A�=qA�?}A�E�A�I�A�G�A�E�A�A�A�A�A�C�A�A�A�O�A�XA�?}A�+A�VA�%A�A�A�A���A���A��mA���A��FA���A�jA�?}A�33A�+A�A�ĜA�hsA�&�A��A��mA���A���A�|�A�^5A�"�A��7A�"�A��DA�oA���A�jA��A���A�|�A�C�A�A�;A�  A�JA��A�+A�5?A�7LA�9XA�9XA�9XA�9XA�9XA�=qA�1'A��A�A�;A�A|��A|��A|�HA|�/A|�A|��A|�A|{A{��A{A{��A{x�A{K�A{G�Az��Ay��Ay��Ax^5Aw�mAw�Av��Av�DAv �Au��Au�AuG�At��As��ArM�Aq�#Aq�mApĜApv�AnVAl�+Al-Al9XAlQ�AlI�Ak�7Aj��AkK�Ak\)Ak7LAj1'Ah��Ai`BAi7LAg�mAe�Ael�Aet�Ael�AfbAe�Ad��Ac�Ab��Ab1'Aa��A`(�A_VA_�hAa�PAb(�Ab�+Ab�\Ab�+Ab1Aa&�A`�DA^�HA^1'A]��A\�`A\�jA\r�A\I�A\E�A\JA[hsAZ��AZ-AYƨAW33AU�mAU��AVAU��AU��AU"�AT��AT��AT�uAT��AT�\AT$�AS�-AS�ASoAR��AR��ARn�AQx�AP��AP �AN�ANbNAN�AM�#AM�hAMdZAM%ALE�ALbAK�#AK`BAKXAKK�AK�AJ��AJVAJ  AI�AIdZAIC�AH��AH��AH�DAHffAHAG\)AG/AG�AF��AFjAF1'AE�AE�FAE��AEC�AD1AC�AC��AC��AC�hAC�AC&�ABA�AA��AA�AA;dA@��A@��A@�jAAoAA��AB�AC\)AE�hAH�AHz�AH�AIVAIVAH�AG�FAF��AF1'AD��ACoA?33A>bA>�A>{A>�A@A@~�A=�#A9�A6~�A4��A4z�A5G�A6�DA7C�A7
=A5VA4v�A4ffA4�\A4�A4ĜA4�HA4ȴA4A�A3�#A3��A3%A2bNA2bNA2ȴA2�HA2I�A1A0 �A/C�A.�A.z�A-��A-�7A-x�A-&�A,��A,�9A,jA+��A+l�A*1'A(1'A'�A'��A'G�A&~�A%�mA%�PA%`BA$�A$��A#�PA"9XA!��A!x�A ��A �RA ��A =qA+A��Av�AQ�A5?AbA�A��A��A�A�AjA|�A��A��A�AS�A�A�uA�mA��AbNAI�A$�A  A�mA��A�/A�jA�A�hAZA�AƨA�A	�wAffAbAJA$�AJA{A�A�
A?}A��AffAM�AA�FA\)A/A�RA\)Av�A�mA��AC�A �RA  �@��F@�\)@��@�5?@���@���@���@��7@�`B@��9@�b@��w@�C�@�J@���@��
@�~�@��@�Z@�h@�I�@�^@�dZ@�p�@�@���@�{@���@ו�@�@���@֗�@պ^@��`@ԋD@���@ҧ�@�p�@�;d@·+@�(�@��@�ƨ@�o@���@��H@Ƨ�@Ƈ+@�n�@�5?@ř�@�x�@�p�@�V@��@��`@���@ģ�@ě�@ēu@�z�@Ý�@��@§�@��T@�
=@�?}@��@��@��
@���@���@��
@��
@��
@���@��m@��@�(�@�I�@��u@�z�@��D@���@��@��@�9X@�b@�b@�|�@�33@�"�@��@��H@�ȴ@��@���@�$�@��T@�hs@��`@��@��`@�%@�V@��@�V@�%@��@��@��/@�C�@��+@��@��7@�7L@�j@���@��@���@��R@���@��#@�Q�@��y@���@���@�V@�^5@��@�^5@��-@���@�p�@���@��@�"�@�"�@�o@��@��@��@�o@�@��@�^5@�@�hs@�V@���@��@���@�Ĝ@��9@��@�j@�bN@�bN@�Z@�A�@�1'@�9X@�I�@�Q�@�r�@��@��u@��j@�Q�@�Ĝ@�\)@�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��<���<��v<���<���<�׵<���<��h<���<�� <���<��a<��j<��<�ۈ<��<���<���<���<��N<��<�0�<�2<��<���<���<���<��:<���<��]<��<��a<��<���<��*<���<�׍<��/<��i<��<��2<� \<�<�z<���<���<���<�س<���<���<��T<��<�#�<�4_<�m<�J�<��<��<�m=<�&�<� �<�!<��S<��S<�-%<�s�<�4�<�W<�qp<��<�lV<���<�y�<��f<���<�Va<�`�<��<��L<��u<���<��<��&<��<��<���<��<��8<�׉<��;<���<�׿<��<��<��<��<�+�<�#�<���<��<��{<�َ<�ڄ<��<��<�M<�%�<��J<���<���<� �<���<�ʣ<�1C<��<��<�s<�4.<�Eg<�^�<��(<�`�<�`n<�hO<�#�<��X<�Gf<�8O<��"<��c<�+<�C�<��3<�*�<��S<��<��|<��<�C�<��U<��<���<�
�<���<���<��k<��<�-�<�O�<��,<���<�K<� |<�ܩ<��K<��!<�7b<�d<�p2<�]�<���<�VO<�R<���<��T<���<�)M<��b<�q�<��<���<�M�<���<���<��<��<���<�� <�Z�<�X�<�i4<�/&<���<�<�<���<��}<�ر<��q<�9�<�+�<��]<�؍<��<<���<��<�$�<��8<��<� {<�ޯ<���<���<��<�=�<���<�TV<���<���<���<��<��<��^<��l<��`<�%:<��3<�ڛ<��<��<��<��<��<��M<��<��)<�<��k<��<�<�h�<��<��<��<��<��<��<��/<��j<��<��9<�A<��v<���<�؆<��<�
K<��~<�iU<���<��I<��O<��<��(<��&<��<��#<�%3<��L<��7<�M<��<��s<���<��<���<��\<�wc<��<�s�<�ǁ<��H<�؈<��#<�ר<�7�<���<���<��<���<��p<�
�<�l><�r�<�g�<��<�0<��<��M<��l<���<��2<�ׁ<���<�CI<�<��d<�Z <�g�<��X<��+<���<�L�<�T�<��<��<�+0<��<��@<��<��,<���<��<��o<���<�UF<�$�<��<���<�k<��<��<���<�`�<�p<��O<�"�<��<�c<�*C<� 
<�C<�<Y<��/<��<�E<�VB<�XE<��<���<���<��<<�A<��<��o<��1<���<��<�+�<���<��4<�	<<���<��<�A <��N<�u!<�$R<���<��<��D<��w<��+<��Z<���<���<�4o<��T<�8&<��<��Q<�n�<�e#<��<���<��<��Z<�׀<���<��y<�d<�<�<�=<��K<� <��<�F<��<�>�<�o�<�<�_�<��I<��<�R�<�d�<���<��(<��;<��<��5<�݅<���<���<���<�<�5<��a<���<��|<�R�<��<��S<��o<�,�<�֝<�7�<���<��<�Y�<�.<��<�ۺ<��<��<���<���<��<�)i<�%N<��H<�s<���<���<���<�$�<��<��k<���<�U<���<��g<��<��<��w<���<���<���<��<��(<��z<��:<���<���<���<���<��<�(�<��%<��r<�*�<�g<��<��<�><���<��V<��&<�ן<��#<��+<�ؑ<��)<��h<��9<��$<���<���<��?<��:<���<�`t<��N<�ݓ<���<��D<���<��p<�ަ<�چ<�ڻ<��^<��<���<��A<���<���<���<�ش<��<�׏<�נ<���<���<��D<���<��<��^<�#�<��<���<���<�#�<�#x<�f<��G<��<���<�'�<���<���<���<���<��<��~<�ߦ<��<�	�<��<��<�C<���<��q<��f<�ٴ<�ת<��&<��0<���<��#<�ڀ<�=<��<��<���<���<���<���<���<��<�ߍ<���<���<��6<��<���<���<�׃<��/<�׋<��<��<��G<��<��d<�J<��<���<�؂;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oPRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP, where dP is SURFACE PRESSURE (minus 5 dbar for Apf-5,7,8) from next cycle.                                                                                                                                                           TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt(sw_cndr(PSAL,TEMP,PRES),TEMP,PRES_ADJUSTED)                                                                                                                                                                                             N/A                                                                                                                                                                                                                                                             N/A                                                                                                                                                                                                                                                             PSAL_ADJUSTED = PSAL - dS                                                                                                                                                                                                                                        dP=-0.3                                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            N/A                                                                                                                                                                                                                                                             N/A                                                                                                                                                                                                                                                              ds=0                                                                                                                                                                                                                                                           Pressures adjusted using despiked reported SURFACE PRESSURE (1 dBar threshold) from the subsequent profile. The quoted error is 2.4 dBar.                                                                                                                       The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity adjusted for effects of pressure adjustment. The quoted error is max(0.01, 1xOW uncertainty) in PSS-78.                                                                                                                                                N/A                                                                                                                                                                                                                                                             N/A                                                                                                                                                                                                                                                             OWC(2018v01). Mapping scales LON 4/2 LAT 2/1 MAPSCALE_PHI 0.1/0.02. MAPSCALE_AGE 5/15. MAP_P_DELTA 100. Compared with CTD2019v01 and ARGO2020v01 ref. data.                                                                                                     202102191121102021022515391020210219112110202102191121102021022515391020210225153910BO  BO  BO  BO  BO  BO  BO  ARGQARGQARGQARGQARGQARSQARSQRTSPPREXRTQCRTQCSCUTnullOW  1.0 2.0 2.0 2.0 2.0 null0.1                                                                                                                                                                                                                                                                                                                                                                                                                                                                 20191016140638201910161406382019101614064020191016140645202007091437262021021911211020210225153910  CV  CV  QCP$QCP$QCP$IP  IP                                                                                                                  G�O�G�O�������������G�O�G�O�G�O�G�O�D}  D}  D}  G�O�G�O�G�� G�� G�� G�� G�� G�� G��                                 6389758         6389758         131072                                          