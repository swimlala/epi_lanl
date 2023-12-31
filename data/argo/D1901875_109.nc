CDF       
      	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       	DATE_TIME         N_PROF        N_PARAM       N_LEVELS  K   N_CALIB       	N_HISTORY            	   title         Argo float vertical profile    institution       BODC   source        
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
resolution        ?�������     	,  ?    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @          	valid_max         @D�        
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     	,  HL   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �         	valid_max         @D         
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     	,  Qx   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  Z�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  \�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  _<   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                    	valid_max         @�p        conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�������     	,  a�   PSAL_ADJUSTED            
      
   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @          	valid_max         @D�        conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     	,  j�   TEMP_ADJUSTED            
      
   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �         	valid_max         @D         conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     	,  s�   PRES_ADJUSTED_QC         
         	long_name         quality flag   standard_name         PRES_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                 L  }   PSAL_ADJUSTED_QC         
         	long_name         quality flag   standard_name         PSAL_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                 L  X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   standard_name         TEMP_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                 L  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         PRES_ADJUSTED_ERROR    units         decibar    conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�������     	,  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         PSAL_ADJUSTED_ERROR    units         psu    conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     	,  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         TEMP_ADJUSTED_ERROR    units         degree_Celsius     conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     	,  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    source_name       	PARAMETER      conventions       Argo reference table 3     
_FillValue                  `  �t   SCIENTIFIC_CALIB_EQUATION               	             	long_name         'Calibration equation for this parameter    source_name       SCIENTIFIC_CALIB_EQUATION      
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	             	long_name         *Calibration coefficients for this equation     source_name       SCIENTIFIC_CALIB_COEFFICIENT   
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	             	long_name         .Comment applying to this parameter calibration     source_name       SCIENTIFIC_CALIB_COMMENT   
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	            	long_name         Date of calibration    source_name       SCIENTIFIC_CALIB_DATE      conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     source_name       HISTORY_INSTITUTION    conventions       Argo reference table 4     
_FillValue                    �(   HISTORY_STEP                     	long_name         Step in data processing    source_name       HISTORY_STEP   conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    source_name       HISTORY_SOFTWARE   conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     source_name       HISTORY_SOFTWARE_RELEASE   conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      source_name       HISTORY_REFERENCE      conventions       Institution dependent      
_FillValue                 �  ��   HISTORY_DATE                     	long_name         #Date the history record was created    source_name       HISTORY_DATE   conventions       YYYYMMDDHHMISS     
_FillValue                  d  �X   HISTORY_ACTION                       	long_name         Action performed on data   source_name       HISTORY_ACTION     conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   source_name       HISTORY_PARAMETER      conventions       Argo reference table 3     
_FillValue                  p  ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   source_name       HISTORY_START_PRES     units         decibar    
_FillValue        G�O�        �H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    source_name       HISTORY_STOP_PRES      units         decibar    
_FillValue        G�O�        �d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    source_name       HISTORY_PREVIOUS_VALUE     
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   source_name       HISTORY_QCTEST     conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  p  ��Argo profile    3.1 1.2 19500101000000  20210226054700  20210226054700  1901875 Argo UK                                                         Brian King                                                      PSAL            TEMP            PRES               mA   BO  126569                          2C  D   NAVIS_EBR                       0651                            BGCi_SBE63 030916               869 @���c`1   @���c`�D&+j��g@D^�m\��1   GPS     Primary sampling: mixed                                                                                                                                                                                                                                            A   A   A   �L��?�  @   @@  @�  @�  @�  @�  A   A  A   A0  A@  AP  A`  Ap  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B  B  B  B  B   B$  B(  B,  B0  B4  B8  B<  B@  BD  BH  BL  BP  BT  BX  B\  B`  Bd  Bh  Bl  Bp  Bt  Bx  B|  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C  C  C�fC�fC	  C  C  C  C  C  C  C  C  C  C  C  C!  C#  C%  C'  C)  C+  C-  C/  C1  C3  C5  C7  C9  C;  C=  C?  CA  CC  CE  CG  CI  CK  CM  CO  CQ  CS  CU  CW  CY  C[  C]  C_  Ca  Cc  Ce  Cg  Ci  Ck  Cm  Co  Cq  Cs  Cu  Cw  Cy  C{  C}  C  C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C CÀ CĀ CŌ�Cƀ Cǀ CȀ Cɀ Cʀ Cˀ C̀ C̀ C΀ Cπ CЀ Cр CҀ CӀ CԀ CՀ Cր C׀ C؀ Cـ Cڀ Cۀ C܀ C݀ Cހ C߀ C�� C� C� C� C� C� C� C� C� C� C� C� C� C� C� C� C�� C� C� C� C� C�� C�� C�s3C�� C�� C�� C�� C�� C�� C�� C�� D @ D � D@ D� D@ D� D@ D� D@ D� D@ D� DFfD� D@ D� D@ D� D	@ D	� D
@ D
� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D@ D� D @ D � D!@ D!� D"@ D"� D#@ D#� D$@ D$� D%@ D%� D&@ D&� D'@ D'� D(@ D(� D)@ D)��D*@ D*� D+@ D+� D,@ D,� D-@ D-� D.@ D.� D/@ D/� D0@ D0� D1@ D1� D2@ D2� D3@ D3� D4@ D4� D5@ D5� D6@ D6� D7@ D7� D8@ D8� D9@ D9� D:@ D:� D;@ D;� D<@ D<� D=@ D=� D>@ D>� D?@ D?� D@@ D@� DA@ DA� DB@ DB� DC@ DC� DD@ DD� DE@ DE� DF@ DF� DG@ DG� DH@ DH� DI@ DI� DJ@ DJ� DK@ DK� DL@ DL� DM@ DM� DN@ DN� DO@ DO� DP@ DP� DQ@ DQ� DR@ DR� DS@ DS� DT@ DT� DU@ DU� DV@ DV� DW@ DW� DX@ DX� DY@ DY� DZ@ DZ� D[@ D[� D\@ D\� D]@ D]� D^@ D^� D_@ D_� D`@ D`� Da@ Da� Db@ Db� Dc@ Dc� Dd@ Dd� De@ De� Df@ Df� Dg@ Dg� Dh@ Dh� Di@ Di� Dj@ Dj� Dk@ Dk� Dl@ Dl� Dm@ Dm� Dn@ Dn� Do@ Do� Dp@ Dp� Dq@ Dq� Dr@ Dr� Ds@ Ds� Dt@ Dt� Du@ Du� Dv@ Dv� Dw@ Dw� Dx@ Dx� Dy@ Dy� Dz@ D{@ D|@ D}&fD�  D�vfD��fD��3D�  D�c3D�� D�� D�0 D�vfD�� D��fD�6fD�s3Dڣ3D��fD�  D�\�D�D�9�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�
B�B�NB�NB�NB�TB�NB�TB�TB�TB�NB�NB�NB�NB�NB�HB�HB�HB�HB�HB�BB�;B�/B�/B�/B�B�B�B�B�B�
B��B��B��BÖB�wB�-B��B��B��B��B��B�hB�PB�B~�Bz�By�By�Bx�Bw�Bv�Bv�Bu�Bt�Bs�Bp�Bp�Br�Bq�Bl�BjBffBe`BdZBbNB`BB_;B]/B\)B\)B\)B\)B[#BXBVBR�BO�BL�BH�BE�BA�B=qB9XB9XB6FB33B.B(�B$�B#�B"�B#�B"�B�B�B�BoBbBbBDBbBhBoB{B\B%BBBBBBBB��B��B��B�;B��B��B��B�B�/B�#B�B��B��B��B�B�B�B�)B�/B�B��B��B��B�
B�BB	7BB�B�B��B�B�B�B�`B��B�}B�dB�RB�FB�?B�!B�B��B��B��B�-B��B��B��B��B��B��B��B��B��B��B��B�oB�PB�7B�B�B�B~�Bv�Bq�Bp�Bo�Bm�BjBffBYBW
BO�BJ�BG�BD�BB�B9XB9XB<jB=qB;dB7LB5?B.B)�B$�B �B�B�BuBbBVBDB1B+BBBB  B
��B
��B
��B
�B
�B
�B
�B
�mB
�BB
�#B
��B
ȴB
ȴB
ǮB
ƨB
ÖB
��B
��B
�wB
�qB
�jB
�LB
�3B
�B
�B
�B
�B
��B
��B
��B
��B
��B
�{B
�bB
�VB
�JB
�1B
�B
{�B
x�B
v�B
u�B
u�B
s�B
r�B
q�B
n�B
hsB
gmB
e`B
bNB
^5B
\)B
XB
VB
R�B
M�B
I�B
C�B
;dB
5?B
1'B
-B
$�B
�B
�B
�B
�B
{B
oB
VB
JB
	7B
%B
B	��B	��B	�B	�B	�B	�B	�B	�B	�yB	�fB	�5B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	�qB	�jB	�RB	�FB	�?B	�3B	�!B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�!B	�B	�!B	�!B	�'B	�-B	�9B	�9B	�3B	�3B	�3B	�3B	�3B	�3B	�-B	�9B	�FB	�FB	�9B	�3B	�-B	�'B	�!B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�uB	�hB	�\B	�VB	�DB	�=B	�7B	�7B	�=B	�DB	�PB	�VB	�JB	�7B	�=B	�DB	�JB	�DB	�DB	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�?B	�RB	�qB	�^B	�RB	�LB	�LB	�LB	�RB	�^B	�qB	�wB	�qB	�qB	��B	��B	��B	ǮB	��B	�sB	��B	��B
+B
�B
+B
F�B
XB
o�B
�B
��B
�9B
ŢB
�
B
�/B
�B
��B+Ae|�Ae�Ael�Aex�Aep�AehsAedZAel�AehsAel�Aex�Ael�AeO�AeO�AeXAeXAeG�Ae7LAe/Ae&�Ae�AeoAd��Ad�Ad�`Ad�Ad�Ad��AdȴAd��Ad��Ad��Ad^5Ac�mAc��Ac�FAc�Ac�AchsAcXAcK�Ac;dAc+Ab��Ab�/Ab��AbĜAbĜAbĜAb��Ac
=AcVAcVAc%Ab��Ab��Ab��Ab��Ab��Ab�Ab�Ab�Ab�Ab�/Ab�Ab�Ab�`Ab�HAb��Ab��Ab��Ab�DAb^5Aa��Aa��Aa��Aa��Aa�^Aa�Aat�A`��A_�FA^=qA]p�A[AZQ�AYx�AX��AXQ�AXbAW��AV�AUl�AT �AS��ASS�AS
=AR��AR�RAR��AR��AR~�ARbNAR=qAQ��AQ�#AQ�#AQ�hAQ�AP�AP�\APbNAPE�AP �AO�mAOƨAO�7AO�7AO�AO�AOx�AO`BAO/AN��AN��ANI�AN�AM�AMhsAMVAL�uALQ�ALI�ALJAK�FAK"�AJ~�AJ9XAJ(�AJAI��AI�#AIC�AH~�AHQ�AH1AG��AG��AGS�AG�hAG��AG��AG�FAGG�AFr�AF=qAE�
AE�#AE�
AE��AE�FAE��AE&�AD��AD(�AB��AA�wAA\)AAG�AA`BAA`BAA�A@�`A@M�A@JA@bA@{A?�A?�
A@  A?��A?33A>��A>�\A>jA>��A@5?AA\)AA��AA/A?A?VA>M�A=G�A=�A<ĜA;��A9|�A7�A7A6�A69XA5��A4�`A4Q�A3��A2��A3�A3&�A1�#A0��A0�+A0��A0�A0�`A0VA/��A.�!A.-A-�mA-\)A,�9A,bNA+��A+�7A+hsA++A)�A)�7A)p�A)`BA)�A(�A'�A&bA%�#A$�RA$-A#�A#�wA#��A"�A"ffA"z�A"E�A!x�A �!A VA\)A�9A  A�A��A�Ax�A
=A�9A1'A�TA�AO�A�A�`A�!AVA�A;dA�uA �A�#A��A�RA�AVAG�A^5A1'A$�A��At�AA�A��A��A�A�A?}A^5A1'A �A{A��A%A
��A
v�A	�PA��A^5A$�A��AVAbNAC�AA�A��A�-Ap�A�/A�uA9XA
=A M�@��F@�C�@�@��@�1@��@�-@���@�\)@�v�@�9X@��@�r�@�dZ@�V@��y@�(�@�F@�l�@��@�-@�7L@���@�o@��@�`B@���@ܛ�@ڸR@ى7@�7L@�bN@�-@թ�@�%@���@�\)@�t�@��#@�r�@�dZ@�^5@ɩ�@�`B@���@�Q�@ǝ�@�;d@�v�@�I�@��T@��j@�(�@�n�@��h@�?}@��m@���@��7@�G�@�z�@��@��@��@��@���@��T@��@��T@��#@���@��7@�x�@���@��D@�1@�9X@���@�1@�9X@�bN@�bN@�I�@�  @��
@�ƨ@��P@�K�@�"�@�C�@�dZ@�;d@��\@��@�O�@���@���@�Z@��w@���@��@�K�@��+@�-@���@��@�V@� �@�V@��^@���@�?}@��@��9@�9X@�+@��+@��@���@��@�1@��
@���@�1@��@�C�@��+@�n�@��\@��+@�^5@�=q@��@���@��T@�@��^@�@��-@���@�X@��@�%@���@���@��@�1'@��@���@��@���@��
@�b@�9X@� �@��D@��/@���@��9@���@�bN@��m@�|�@�
=@���@��\@���@���@�ȴ@�o@���@�n�@��#@�`B@��@��@��u@�A�@�9X@�1'@�A�@�Q�@�Q�@�Q�@�j@��@�I�@� �@���@��y@�~�@�~�@��\@�v�@�-@�p�@���@�r�@�A�@�1'@�bN@�Q�@��@�ƨ@�ƨ@���@�|�@�t�@�S�@�S�@�K�@�33@��@�~�@�V@��@��@��#@���@�G�@���@��j@�bN@��
@��F@���@�S�@��@��@�o@�
=@�@���@�@�@�o@�33@�|�@���@�(�@�Z@�t�@�"�@���@�v�@�V@�V@�^5@�v�@�ff@�@��^@�hs@�p�@���@���@��Y@�W@v�c@sx@p@g��@^^5@VTa@Q��@Kt�@E�o@@�@<U2@9<6@6~�@5�^@3�@1��@0j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?�\?��H@-p�@mp�@��R@��R@ָR@��RA\)A\)A+\)A;\)AK\)A[\)Ak\)A{\)A��A��A��A��A��A��A��A��AŮAͮAծAݮA�A��A��A��B�
B�
B
�
B�
B�
B�
B�
B�
B"�
B&�
B*�
B.�
B2�
B6�
B:�
B>�
BB�
BF�
BJ�
BN�
BR�
BV�
BZ�
B^�
Bb�
Bf�
Bj�
Bn�
Br�
Bv�
Bz�
B~�
B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C��C��C�)C�)C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���D mqD �qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDs�D�qDmqD�qDmqD�qD	mqD	�qD
mqD
�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qD mqD �qD!mqD!�qD"mqD"�qD#mqD#�qD$mqD$�qD%mqD%�qD&mqD&�qD'mqD'�qD(mqD(�qD)mqD)�D*mqD*�qD+mqD+�qD,mqD,�qD-mqD-�qD.mqD.�qD/mqD/�qD0mqD0�qD1mqD1�qD2mqD2�qD3mqD3�qD4mqD4�qD5mqD5�qD6mqD6�qD7mqD7�qD8mqD8�qD9mqD9�qD:mqD:�qD;mqD;�qD<mqD<�qD=mqD=�qD>mqD>�qD?mqD?�qD@mqD@�qDAmqDA�qDBmqDB�qDCmqDC�qDDmqDD�qDEmqDE�qDFmqDF�qDGmqDG�qDHmqDH�qDImqDI�qDJmqDJ�qDKmqDK�qDLmqDL�qDMmqDM�qDNmqDN�qDOmqDO�qDPmqDP�qDQmqDQ�qDRmqDR�qDSmqDS�qDTmqDT�qDUmqDU�qDVmqDV�qDWmqDW�qDXmqDX�qDYmqDY�qDZmqDZ�qD[mqD[�qD\mqD\�qD]mqD]�qD^mqD^�qD_mqD_�qD`mqD`�qDamqDa�qDbmqDb�qDcmqDc�qDdmqDd�qDemqDe�qDfmqDf�qDgmqDg�qDhmqDh�qDimqDi�qDjmqDj�qDkmqDk�qDlmqDl�qDmmqDm�qDnmqDn�qDomqDo�qDpmqDp�qDqmqDq�qDrmqDr�qDsmqDs�qDtmqDt�qDumqDu�qDvmqDv�qDwmqDw�qDxmqDx�qDymqDy�qDzmqD{mqD|mqD}S�D�6�D��D��D�	�D�6�D�y�D�ƸD��D�F�D��D�ָD�D�MDԉ�Dڹ�D��D�6�D�s�D��RD�PRB��B�JB��B�#B�B�
B��B�B��B��B�BB�qB��B��B�B�@B�=B�B�"B�;B�B�QB�:B�B�&B� B�'B�B�B�BӸB�VB��BԂB�`B�8B��B�jB�HB�;B�SB�dB��B֓B�PB�'B�B��B֦B�^B�B�VB�|B�rB�yB�TB�VB�WB�gB�`B�SB�]B�B�^B�BB�B�iB�B�B��BݳB�3B��B��B�B�(B�xBؒBؙBؒBՆB�~B�kBƀB��B�2B��B�1B�;B�+B��B�B��B�tB|RB{:Bz�ByeBxGBv�BwgBv_Bu�Bt�BqMBqBt)Bs�Bm�Bk�Bg;Be�BeBcWBaB`,B]BB\@B\8B\xB\�B\BYQBW�BTZBPqBM�BIhBF�BB�B>B9rB9�B7"B4�B/�B)�B%B$0B"�B$5B$DB�BB6B�BvBxB
�BMB@B^BB`B�BBB&B?BNBkBB��B��B�RB�B�B�BѹB�BݸBۼBلBԤB��B��B�jB�:BعB�<B�B�
BчB�(B�rB��B�B)B
aB	LB��B��B�,B�#B�B�eB�B��B��B��B�B�B��B��B�cB�DB�WB�B�XB��B��B�qB�AB��B�EB�MB��B��B�LB��B�B�*B��B��B�kB��B�Bw�Bq�Bp�BpQBo#Bl�BjNBY�BY�BQSBKdBH9BEBD�B:/B9:B<�B?}B=aB8VB7�B/�B+�B&2B# B\B�B�BNB�BB�BB�B�B�B �B
��B
�B
�~B
��B
�]B
�1B
��B
�vB
�B
ߚB
�OB
�+B
��B
�#B
� B
ĿB
��B
��B
��B
�!B
��B
��B
�kB
��B
�9B
�0B
�B
��B
��B
�MB
�B
�<B
�B
�B
�=B
�GB
�B
�B
~�B
zB
w(B
v}B
wDB
t�B
s�B
t�B
p{B
i�B
hB
gB
c�B
_sB
]�B
YB
W�B
T�B
OB
L�B
FgB
=�B
6�B
2�B
1�B
(�B
gB
B
8B
�B
�B
OB
cB
�B

TB
	ZB
AB	�zB	�lB	�?B	��B	�B	�_B	�bB	��B	�B	�B	��B	� B	�kB	�QB	��B	�aB	ҮB	ѢB	��B	�zB	��B	��B	��B	�7B	�bB	��B	��B	��B	�B	��B	��B	��B	�4B	�yB	��B	��B	�B	�4B	��B	��B	�,B	�+B	�bB	�QB	�@B	��B	��B	��B	��B	�|B	�B	��B	�B	�CB	�cB	��B	�uB	�UB	��B	��B	�oB	�B	�B	��B	�0B	�B	�B	��B	��B	��B	��B	�OB	�-B	�bB	�B	�B	��B	�\B	��B	�1B	�AB	��B	��B	�B	��B	��B	�+B	��B	�TB	��B	��B	��B	�aB	��B	�PB	�B	�JB	�yB	�RB	�nB	�B	�YB	��B	�B	��B	��B	�SB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�;B	��B	��B	�~B	��B	��B	�eB	��B	�$B	�8B	�>B	�lB	��B	��B	��B	��B	�#B	��B	��B	��B	��B	�@B	��B	��B	��B	�oB	�$B	��B	�|B	�B	�B	��B	��B	�B	�
B	��B	��B	�ZB	�OB	��B	�B	��B	��B	��B	�#B	�mB	��B	��B	�fB	�)B	��B	��B	�	B	�~B	�1B	��B	�,B	�:B	�B	�:B	�B	�#B	�;B	�tB	��B	�QB	�`B	�IB	�5B	�WB	��B	�|B	�oB	��B	��B	�:B	�*B	�nB	�\B	�B	�!B	�"B	�"B	�$B	�B	�B	�B	��B	��B	��B	��B	�'B	��B	��B	��B	��B	��B	�XB	�PB	�HB	��B	��B	��B	��B	��B	�B	��B	��B	�'B	�B	��B	�B
QB
�B
+'B
F�B
X,B
o�B
�4B
��B
�HB
ůB
�B
�:B
�B
��B-Ae|�Ae�Ael�Aex�Aep�AehsAedZAel�AehsAel�Aex�Ael�AeO�AeO�AeXAeXAeG�Ae7LAe/Ae&�Ae�AeoAd��Ad�Ad�`Ad�Ad�Ad��AdȴAd��Ad��Ad��Ad^5Ac�mAc��Ac�FAc�Ac�AchsAcXAcK�Ac;dAc+Ab��Ab�/Ab��AbĜAbĜAbĜAb��Ac
=AcVAcVAc%Ab��Ab��Ab��Ab��Ab��Ab�Ab�Ab�Ab�Ab�/Ab�Ab�Ab�`Ab�HAb��Ab��Ab��Ab�DAb^5Aa��Aa��Aa��Aa��Aa�^Aa�Aat�A`��A_�FA^=qA]p�A[AZQ�AYx�AX��AXQ�AXbAW��AV�AUl�AT �AS��ASS�AS
=AR��AR�RAR��AR��AR~�ARbNAR=qAQ��AQ�#AQ�#AQ�hAQ�AP�AP�\APbNAPE�AP �AO�mAOƨAO�7AO�7AO�AO�AOx�AO`BAO/AN��AN��ANI�AN�AM�AMhsAMVAL�uALQ�ALI�ALJAK�FAK"�AJ~�AJ9XAJ(�AJAI��AI�#AIC�AH~�AHQ�AH1AG��AG��AGS�AG�hAG��AG��AG�FAGG�AFr�AF=qAE�
AE�#AE�
AE��AE�FAE��AE&�AD��AD(�AB��AA�wAA\)AAG�AA`BAA`BAA�A@�`A@M�A@JA@bA@{A?�A?�
A@  A?��A?33A>��A>�\A>jA>��A@5?AA\)AA��AA/A?A?VA>M�A=G�A=�A<ĜA;��A9|�A7�A7A6�A69XA5��A4�`A4Q�A3��A2��A3�A3&�A1�#A0��A0�+A0��A0�A0�`A0VA/��A.�!A.-A-�mA-\)A,�9A,bNA+��A+�7A+hsA++A)�A)�7A)p�A)`BA)�A(�A'�A&bA%�#A$�RA$-A#�A#�wA#��A"�A"ffA"z�A"E�A!x�A �!A VA\)A�9A  A�A��A�Ax�A
=A�9A1'A�TA�AO�A�A�`A�!AVA�A;dA�uA �A�#A��A�RA�AVAG�A^5A1'A$�A��At�AA�A��A��A�A�A?}A^5A1'A �A{A��A%A
��A
v�A	�PA��A^5A$�A��AVAbNAC�AA�A��A�-Ap�A�/A�uA9XA
=A M�@��F@�C�@�@��@�1@��@�-@���@�\)@�v�@�9X@��@�r�@�dZ@�V@��y@�(�@�F@�l�@��@�-@�7L@���@�o@��@�`B@���@ܛ�@ڸR@ى7@�7L@�bN@�-@թ�@�%@���@�\)@�t�@��#@�r�@�dZ@�^5@ɩ�@�`B@���@�Q�@ǝ�@�;d@�v�@�I�@��T@��j@�(�@�n�@��h@�?}@��m@���@��7@�G�@�z�@��@��@��@��@���@��T@��@��T@��#@���@��7@�x�@���@��D@�1@�9X@���@�1@�9X@�bN@�bN@�I�@�  @��
@�ƨ@��P@�K�@�"�@�C�@�dZ@�;d@��\@��@�O�@���@���@�Z@��w@���@��@�K�@��+@�-@���@��@�V@� �@�V@��^@���@�?}@��@��9@�9X@�+@��+@��@���@��@�1@��
@���@�1@��@�C�@��+@�n�@��\@��+@�^5@�=q@��@���@��T@�@��^@�@��-@���@�X@��@�%@���@���@��@�1'@��@���@��@���@��
@�b@�9X@� �@��D@��/@���@��9@���@�bN@��m@�|�@�
=@���@��\@���@���@�ȴ@�o@���@�n�@��#@�`B@��@��@��u@�A�@�9X@�1'@�A�@�Q�@�Q�@�Q�@�j@��@�I�@� �@���@��y@�~�@�~�@��\@�v�@�-@�p�@���@�r�@�A�@�1'@�bN@�Q�@��@�ƨ@�ƨ@���@�|�@�t�@�S�@�S�@�K�@�33@��@�~�@�V@��@��@��#@���@�G�@���@��j@�bN@��
@��F@���@�S�@��@��@�o@�
=@�@���@�@�@�o@�33@�|�@���@�(�@�Z@�t�@�"�@���@�v�@�V@�V@�^5@�v�@�ff@�@��^@�hs@�p�@���@���@��Y@�W@v�c@sx@p@g��@^^5@VTa@Q��@Kt�@E�o@@�@<U2@9<6@6~�@5�^@3�@1��@0j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��<�ח<��<��<���<���<���<���<���<��e<�ش<���<���<�ۺ<���<��h<��5<��r<��<���<��<��<���<��<��D<��<��1<���<��<��<��(<��<�d�<�ӱ<��!<��<��<�m<��<��<��<��M<��$<��<���<��<��h<��C<�َ<��w<��<��,<��<��v<�ߨ<���<��p<�۷<���<���<���<��<��M<��<��^<�ڽ<���<��<��<��<�S<��+<�-4<���<�:<��C<�ޤ<���<��v<���<��Q<���<��<��<��<��<���<�7�<�x�<���<���<���<�[�<��p<�X�<�o�<�c�<�%<���<��<���<��<�<��<�;<��<��<�t�<��<�Y�<�Y�<�8<�R<��<�1Z<��<�$�<��^<��<���<��<���<�)d<�P�<���<�j<���<�/�<�
{<�#8<�F<�O<��<���<��<�l=<��4<��<���<��<��<���<�q�<���<��M<�
�<��S<�޺<�8�<��,<�٩<��B<��<�/N<��(<� K<�*�<���<�ݞ<���<��<���<�1|<�${<��S<���<�J-<�0�<��<�״<�ݦ<��,<� �<�s4<��<��5<��9<��x<���<��-<�ޣ<��<�0�<��<���<��<�_<�1<��s<�DT<��<��R<���<�P;<��<�)�<��q<���<�[<�n�<�R�<��<�c<��J<�wB<�g/<�-�<��H<��R<�2!<��W<��<��A<���<���<�W�<�p�<��]<�b<�
<�g<��
<��<�q0<��<��b<��<��<�7�<��<��<�
�<��\<�%<�K�<�<�Ǉ<�t�<��<��X<���<� <��<��,<���<���<��<�3�<�R�<���<��"<�_�<�<X<���<�O�<�@�<�$g<�[�<�g<� e<� �<� [<��s<��<�&1<���<�P�<���<�I <�+<�+<�G
<��<�W`<�At<�Lu<�� <��8<��<�a�<�D�<��u<��<��<�
,<�<�N�<��<���<��9<���<�=�<��<�9�<��|<�0p<���<���<��<�"<��"<��<���<��1<�N
<��'<�<�}O<��<�2�<�4�<��j<�I�<�m<��1<�u�<�Qc<���<�-�<���<���<�X<��<���<�$�<�v�<���<�;�<���<��<��<��<�<�<�`0<���<�5I<�Y�<�=�<�@�<�@�<�x<��[<���<�K�<���<�d<��<��H<���<�M<�2�<��<�{�<�n�<�+=<��i<�C<�t<�)�<��<�>)<��%<�J�<��<�&�<�,�<�J<��,<��n<�u�<�	D<��z<�?z<�m�<�$b<���<��<��(<��j<��	<��<�ߕ<��2<��k<��<�	�<�<�<��$<��'<��:<�ג<��:<�ݲ<��<��e<���<��<��O<��<��<��d<���<��<�#�<� �<�"�<��Q<���<��<�$<��<��<���<�4�<��!<��X<���<��<�_�<�X�<�$Y<���<��I<���<��<�r<�t�<�*?<�tz<�W�<�?<���<��q<���<��`<�ہ<�AD<�2�<��<��<��L<��f<��W<��<��<��9<��a<��S<��X<��)<��<��<��B<��<��b<��)<��{<��d<��^<��W<��
<��Z<��Q<��P<��/<��V<��d<��F<���<��<��*<��d<�	=<�n<��<��7<��<��K<���<��<��
<��a<��<�5<��<��<��y<�R<��1<��[<��<���<��<��<��i<�ٱ<��<���<��_<��<�/*<�W<�ߒ<��<��<��<�3�<�%�<��A<��v<���<��9<��<���<���<��y<��f<��I<��<��<��v<��c<��<��X<��<���<��q<�� <��<��3<��#<��<��<��<��<���<���<���<��<��<��<��P<��N<�ߥ<���<��w<���<���<��<��k<�׷<��Q<�>�<��F<���<��z<��<��<��<���<��<���<���<��(<�ܵ<���<��h<��<��<��G<��t<�ߕ<��<��<���<��4<��H<���<��1<�߉<��G<�� <���<�ޯ<��6<��<��H;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oPRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP, where dP is SURFACE PRESSURE (minus 5 dbar for Apf-5,7,8) from next cycle.                                                                                                                                                           TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt(sw_cndr(PSAL,TEMP,PRES),TEMP,PRES_ADJUSTED)                                                                                                                                                                                             N/A                                                                                                                                                                                                                                                             N/A                                                                                                                                                                                                                                                             PSAL_ADJUSTED = PSAL - dS                                                                                                                                                                                                                                        dP=-0.71                                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            N/A                                                                                                                                                                                                                                                             N/A                                                                                                                                                                                                                                                              ds=0                                                                                                                                                                                                                                                           Pressures adjusted using despiked reported SURFACE PRESSURE (1 dBar threshold) from the subsequent profile. The quoted error is 2.4 dBar.                                                                                                                       The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity adjusted for effects of pressure adjustment. The quoted error is max(0.01, 1xOW uncertainty) in PSS-78.                                                                                                                                                N/A                                                                                                                                                                                                                                                             N/A                                                                                                                                                                                                                                                             OWC(2018v01). Mapping scales LON 4/2 LAT 2/1 MAPSCALE_PHI 0.1/0.02. MAPSCALE_AGE 5/15. MAP_P_DELTA 100. Compared with CTD2019v01 and ARGO2020v01 ref. data.                                                                                                     202102191121102021022515391020210219112110202102191121102021022515391020210225153910BO  BO  BO  BO  BO  BO  BO  ARGQARGQARGQARGQARGQARSQARSQRTSPPREXRTQCRTQCSCUTnullOW  1.0 2.0 2.0 2.0 2.0 null0.1                                                                                                                                                                                                                                                                                                                                                                                                                                                                 20191027080625201910270806252019102708062620191027080630202007091437262021021911211020210225153910  CV  CV  QCP$QCP$QCP$IP  IP                                                                                                                  G�O�G�O��L�;L�;L��G�O�G�O�G�O�G�O�D�9�D�9�D�9�G�O�G�O�G�4�G�4�G�4�G�4�G�4�G�4�G�4�                                6389758         6389758         131072                                          