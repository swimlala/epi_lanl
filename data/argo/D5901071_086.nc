CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:15Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               VA   AO  20111130141117  20190522121826  1727_5046_086                   2C  D   APEX                            2143                            040306                          846 @Ԅ�qf 1   @Ԅ�q�@7� ě���dhr� �1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @�33@�33A��AffA@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"fD"�fD#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��Dey�De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl� Dl��Dmy�Dm��Dn� DofDo�fDp  Dpy�Dq  Dq� Dr  Dr� Ds  Dsff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @@  @�33@�33A��AffA@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"fD"�fD#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��Dey�De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl� Dl��Dmy�Dm��Dn� DofDo�fDp  Dpy�Dq  Dq� Dr  Dr� Ds  Dsff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�33A�/A�&�A�"�A�$�A�(�A�-A�(�A�1'A�1'A�1'A���A��#AН�A�z�A�n�A�$�A�5?A�"�A�jA��Ḁ�A�t�A�A�A�-A��A�A���A��A��
A�ƨA˴9A˙�A�v�A�(�Aʴ9Aʉ7A�`BA�$�A�$�APA�t�A�9XA�VA�ffA�5?A���A�JA�O�A��
A�n�A�9XA���A��A���A�hsA�oA�ĜA�G�A���A�bA��A�O�A�5?A���A���A�ZA�oA���A��9A���A�M�A�bA�ffA���A��jA��A��A���A���A��A�33A�  A���A�I�A��A�-A�O�A�;dA��yA��!A�K�A���A�=qA�n�A�C�A�JA�^5A�^5A�K�A�jA�A�A��+A�JA���A�ZA��A�z�A�dZA�VA��/A��A�9XA�^5A���A��A��A�p�A��`A���A��TA��A��jA�1'A��A��wA�jA�ƨA�\)A�E�A���A�l�A�K�A��;A�(�A�A�+A~bA|�A{�AzbAx��Ax1'AwK�Au��At�Aq�#AoC�AnM�Al=qAi��Ag|�Ae"�Ac;dAbZAa�#A_hsA]�wA\A[x�AZ  AW��AU��AT��AS�ARbAP�AN�RALZAJA�AI�AH��AF�AE��AC��ACVABM�A@�A>�9A<��A9�A9`BA9
=A8�A81'A7%A5|�A4(�A3��A3/A2�A2�!A2$�A1K�A1%A0��A0(�A/K�A.��A.{A-C�A,�!A,E�A+S�A)�
A(��A($�A'��A'7LA&r�A$��A$A#�hA"ȴA!�A!��A �A ZA�A9XA��AS�AffA|�A��A�FA�`A�wA1'A\)A��A�`A�A|�A^5A�mA�PA33A�yA �A��Av�AA
ȴA
(�A	�#A	��A��A1'A��A"�A�uA�Az�A1'A  AƨA�A�A�9A/@�r�@��@��/@���@�v�@��@���@�?}@��9@� �@�ȴ@��@�@�J@���@�-@�I�@�&�@畁@�^5@���@�\)@�=q@��/@�I�@�dZ@�
=@��y@���@�\)@���@�~�@݉7@�Ĝ@�z�@�l�@�K�@ڟ�@�n�@�ff@ڇ+@�v�@��T@�x�@��@Ԭ@��
@���@д9@�C�@�1@���@�;d@�$�@�j@�@�@��@�j@��@��/@���@�@�{@���@�7L@�/@��@��D@�33@�n�@���@��@�l�@�
=@��\@��@�1'@�K�@�E�@��h@�?}@��j@�Q�@�9X@���@�;d@���@��\@�E�@���@��@��
@���@���@��+@���@�/@��@�bN@�S�@��^@��;@�\)@�ȴ@��\@�$�@�@�p�@��`@�1'@��w@�o@���@���@��\@�ff@�-@���@���@��T@��-@�7L@��@��@�?}@�(�@���@�/@�V@��u@�z�@��9@���@�Ĝ@�Ĝ@��j@��9@���@�%@�Z@�1'@��@�K�@��\@��@���@���@�@�G�@���@���@���@�j@�(�@���@�dZ@�"�@���@��R@�V@���@�E�@�=q@�$�@��@�7L@�%@�%@��`@���@�Q�@��@�ƨ@���@�+@���@���@��`@�1'@���@��F@���@���@�C�@�t�@�A�@�;d@��@���@��@���@��D@��@�9X@� �@���@���@�{@�M�@�S�@���@�b@�Z@��@�A�@���@��F@�|�@�"�@�v�@�V@�=q@�$�@��T@���@�hs@�G�@�?}@�%@��@��u@��;@���@��P@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�33A�/A�&�A�"�A�$�A�(�A�-A�(�A�1'A�1'A�1'A���A��#AН�A�z�A�n�A�$�A�5?A�"�A�jA��Ḁ�A�t�A�A�A�-A��A�A���A��A��
A�ƨA˴9A˙�A�v�A�(�Aʴ9Aʉ7A�`BA�$�A�$�APA�t�A�9XA�VA�ffA�5?A���A�JA�O�A��
A�n�A�9XA���A��A���A�hsA�oA�ĜA�G�A���A�bA��A�O�A�5?A���A���A�ZA�oA���A��9A���A�M�A�bA�ffA���A��jA��A��A���A���A��A�33A�  A���A�I�A��A�-A�O�A�;dA��yA��!A�K�A���A�=qA�n�A�C�A�JA�^5A�^5A�K�A�jA�A�A��+A�JA���A�ZA��A�z�A�dZA�VA��/A��A�9XA�^5A���A��A��A�p�A��`A���A��TA��A��jA�1'A��A��wA�jA�ƨA�\)A�E�A���A�l�A�K�A��;A�(�A�A�+A~bA|�A{�AzbAx��Ax1'AwK�Au��At�Aq�#AoC�AnM�Al=qAi��Ag|�Ae"�Ac;dAbZAa�#A_hsA]�wA\A[x�AZ  AW��AU��AT��AS�ARbAP�AN�RALZAJA�AI�AH��AF�AE��AC��ACVABM�A@�A>�9A<��A9�A9`BA9
=A8�A81'A7%A5|�A4(�A3��A3/A2�A2�!A2$�A1K�A1%A0��A0(�A/K�A.��A.{A-C�A,�!A,E�A+S�A)�
A(��A($�A'��A'7LA&r�A$��A$A#�hA"ȴA!�A!��A �A ZA�A9XA��AS�AffA|�A��A�FA�`A�wA1'A\)A��A�`A�A|�A^5A�mA�PA33A�yA �A��Av�AA
ȴA
(�A	�#A	��A��A1'A��A"�A�uA�Az�A1'A  AƨA�A�A�9A/@�r�@��@��/@���@�v�@��@���@�?}@��9@� �@�ȴ@��@�@�J@���@�-@�I�@�&�@畁@�^5@���@�\)@�=q@��/@�I�@�dZ@�
=@��y@���@�\)@���@�~�@݉7@�Ĝ@�z�@�l�@�K�@ڟ�@�n�@�ff@ڇ+@�v�@��T@�x�@��@Ԭ@��
@���@д9@�C�@�1@���@�;d@�$�@�j@�@�@��@�j@��@��/@���@�@�{@���@�7L@�/@��@��D@�33@�n�@���@��@�l�@�
=@��\@��@�1'@�K�@�E�@��h@�?}@��j@�Q�@�9X@���@�;d@���@��\@�E�@���@��@��
@���@���@��+@���@�/@��@�bN@�S�@��^@��;@�\)@�ȴ@��\@�$�@�@�p�@��`@�1'@��w@�o@���@���@��\@�ff@�-@���@���@��T@��-@�7L@��@��@�?}@�(�@���@�/@�V@��u@�z�@��9@���@�Ĝ@�Ĝ@��j@��9@���@�%@�Z@�1'@��@�K�@��\@��@���@���@�@�G�@���@���@���@�j@�(�@���@�dZ@�"�@���@��R@�V@���@�E�@�=q@�$�@��@�7L@�%@�%@��`@���@�Q�@��@�ƨ@���@�+@���@���@��`@�1'@���@��F@���@���@�C�@�t�@�A�@�;d@��@���@��@���@��D@��@�9X@� �@���@���@�{@�M�@�S�@���@�b@�Z@��@�A�@���@��F@�|�@�"�@�v�@�V@�=q@�$�@��T@���@�hs@�G�@�?}@�%@��@��u@��;@���@��P@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B(�B33BP�Bw�B��BB�B�NB�TB�`B�B�B�`B�;B�;B�fB�sB�sB�yB�yB�fB�TB�;B�5B�/B�NB�fB�mB�/B�/B�HB�NB�HB�;B�5B�B�B��B��B��B��B��BǮBŢBĜBȴB��B��B�jB�9B�B��B��B�?B�FB�'B�B��B��B�Bx�BjB]/BQ�BN�BE�B>wB5?B �BB��B�B�B�yB�/B��BŢB�qB�!B��B��B�\B~�BcTBQ�BD�B;dB0!B�BB
��B
�B
�#B
��B
�RB
�B
�{B
�7B
z�B
n�B
]/B
VB
J�B
@�B
5?B
0!B
(�B
�B
hB	��B	�B	�HB	��B	�XB	��B	�{B	�B	{�B	v�B	k�B	^5B	XB	S�B	K�B	A�B	8RB	2-B	)�B	"�B	�B	JB	  B��B��B�B�yB�ZB�5B�#B�B��B��BĜB�qB�XB�RB�XB�^B�RB�'B��B��B�B�B�B��B��B��B��B��B��B��B��B��B�oB�VB�=B�+B�%B�B�B�B�B�B~�B}�B�B�B�B� B|�B|�B|�B|�B}�B{�By�B|�B{�By�Bw�Bt�Bq�Bm�BgmBe`BbNBaHBaHB`BB_;B^5B]/B[#BZBXBW
BVBT�BS�BR�BQ�BP�BP�BP�BT�BZB[#B[#BZBZBYBT�BL�BE�BB�BA�BA�BA�B@�B?}B?}B?}B>wB=qB>wB@�BA�BA�BC�B?}B?}B<jB9XB9XB9XB9XB=qB?}BC�BM�BZBaHBiyBjBjBjBk�Bl�Bl�Bm�Bk�Bk�Bl�Bl�Bm�Bo�Bm�Bp�Br�Bo�BjBhsBdZB_;BaHBbNB]/BW
BT�BP�BO�BO�BO�BP�BQ�BR�BS�BS�BT�BT�BT�BT�BXBYB\)B^5BbNBcTBcTBffBjBm�Br�Bu�Bv�Bx�Bz�B{�B|�B~�B�B�B� B~�B� B�B�1B�7B�=B�PB�hB�{B��B��B��B��B��B�B�B�'B�3B�9B�LB�qB�}BƨBȴBȴBȴBɺB��B��B��B��B��B��B�/B�BB�`B�`B�B��B��B��B��B	  B	B	+B	+B		7B		7B	DB	VB	hB	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	&�B	)�B	,B	.B	0!B	1'B	33B	49B	49B	5?B	49B	6FB	=qB	?}B	@�B	B�B	C�B	C�B	C�B	B�B	B�B	A�B	@�B	@�B	?}B	>wB	?}B	@�B	B�B	B�B	D�B	G�B	I�B	L�B	W
B	^5B	ffB	z�B	|�B	}�B	}�B	~�B	{�B	|�B	}�B	}�B	|�B	}�B	�B	�+B	�PB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B-B:^BS�By�B��BƨB�B�TB�ZB�sB�B�B�mB�BB�BB�B�B�yB�B�B�yB�fB�NB�BB�;B�TB�yB�B�HB�;B�TB�NB�NB�HB�`B�5B�B�B�B��B��B��BȴBƨBŢBɺB��B��B�wB�RB�9B�B�B�LB�XB�3B�!B�B��B�B}�Bo�BaHBR�BR�BG�B@�B<jB-BB��B��B�B�B�TB�
BȴBÖB�9B��B��B�uB�+BgmBW
BF�B>wB7LB�B1BB
�B
�HB
��B
�wB
�-B
��B
�\B
� B
t�B
`BB
[#B
M�B
D�B
7LB
33B
.B
"�B
�B
%B	�B	�sB	�
B	��B	�-B	��B	�+B	}�B	~�B	q�B	cTB	ZB	XB	P�B	E�B	;dB	6FB	-B	&�B	�B	oB	B��B��B��B�B�B�BB�/B�5B�
B��B��B�}B�^B�XB�dB�wB�qB�?B�B��B�B�B�B�B��B��B��B��B��B��B��B��B�{B�hB�\B�=B�7B�+B�%B�1B�7B�B�B�B�B�B�B�B� B~�B}�B}�B� B}�B{�B~�B}�B|�B{�Bv�Bs�Br�BiyBgmBe`BcTBbNBaHB`BBaHB_;B_;B]/B[#BYBW
BVBW
BT�BS�BR�BR�BT�BYB[#B\)B\)B[#B\)B[#B[#BXBI�BD�BC�BD�BB�BA�B@�B@�B@�B@�B?}B>wB@�BC�BA�BE�BC�BA�B>wB;dB;dB:^B;dB>wB@�BD�BM�BZBaHBjBk�Bl�Bk�Bl�Bn�Bl�Bn�Bk�Bk�Bl�Bl�Bn�Bp�Bq�Bt�Bt�Bs�Bl�Bk�BjB`BBbNBdZBaHB\)B\)BR�BQ�BS�BO�BR�BS�BR�BS�BS�BT�BT�BVBXBZB[#B^5B`BBcTBdZBe`BiyBl�Bo�Br�Bv�Bw�By�Bz�B|�B~�B� B�B�B�B� B�B�%B�1B�7B�DB�VB�oB��B��B��B��B��B�B�B�!B�-B�9B�?B�RB�wB��BǮBȴBȴBȴB��B��B��B��B��B��B��B�/B�;B�mB�fB�yB��B��B��B��B	  B	B	+B	+B		7B		7B	DB	\B	hB	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	&�B	)�B	,B	/B	0!B	2-B	33B	49B	5?B	6FB	5?B	6FB	=qB	?}B	A�B	C�B	C�B	C�B	C�B	C�B	C�B	B�B	@�B	@�B	@�B	?}B	A�B	A�B	C�B	B�B	E�B	G�B	I�B	M�B	W
B	\)B	aHB	z�B	|�B	}�B	� B	�B	|�B	|�B	}�B	~�B	|�B	}�B	�B	�B	�JB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447042012010314470420120103144704  AO  ARGQ                                                                        20111130141117  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141117  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144704  IP                  G�O�G�O�G�O�                