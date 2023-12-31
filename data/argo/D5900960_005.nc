CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-01T23:12:40Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7T   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    88   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8<   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8D   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8H   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8P   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8X   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8`   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8d   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8l   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9l   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9x   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9|   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  AT   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  CL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  pl   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xD   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  z<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5900960 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130103207  20190523124442  1514_5041_005                   2C  D   APEX                            2041                            062805                          846 @��V/h`1   @��V/h`@6���R�c;;dZ�1   GPS     Primary sampling: averaged [2dbar-bin averaged]                                                                                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A˺^A˼jA˼jA˼jA˾wA�A�A�ĜA�ĜA�ĜA�A�A�ĜA�A���A���A˴9A˴9A˶FA˼jA���A�A˸RA˴9Aˣ�A˃A�`BA�(�A�  A��A��A��\A�ƨA�ffA���A�1A�ȴA���A�33A��A�1A��RA���A�{A���A���A�&�A��A�ƨA�p�A�"�A��#A�hsA�C�A�A��jA�`BA�  A��DA��A��A���A�~�A���A��DA�n�A�z�A��
A���A�/A��A�\)A�bA���A��A��\A���A���A�jA�VA���A��;A��A�;dA�-A���A��-A�1A�A���A��yA���A��A�A�^5A���A��+A���A���A�jA��A��A�%A��A�x�A��7A�7LA�VA�ZA��/A�7LA�  A��-A~  A|AzAx��Aw��Aw
=Au��As�#Arr�Ar9XArAqƨAoS�AnA�Am|�Alv�Ak�Aj�uAi�-Ag�PAet�Ad��Ad  Ab1'AaC�A`Q�A_��A^��A]�A\�/A[��A[oAY%AWp�AV�jAV{AS�AQ�AQO�AP^5AM�TAK��AJ�HAJz�AI?}AG��AG��AGK�AF�AD��AB5?A@��A?��A>�A<�`A;XA:��A9�
A8��A7dZA6{A4v�A4VA3�A3|�A2�yA2A�A0�9A0JA/l�A.��A-�mA+��A*jA)�#A)��A)VA($�A'��A'p�A%��A$��A$VA#��A#p�A"  A �jA��Ar�A��A�hA�HA{A��A�wAC�A�AĜAE�A��A�RA�A�jA1A��AK�A��Ar�A�AO�AI�A��A;dA �A
�uA
bA	�wAĜA�A�Av�A��A�DA��AVA ��A @���@���@�dZ@�-@�7L@�I�@���@�V@�z�@�@���@�ff@�j@�w@�|�@�33@ꟾ@� �@�;d@�=q@�9@���@�x�@�j@ߍP@�$�@�G�@�"�@�%@�S�@��@Ӿw@�^5@�Ĝ@��@�X@˕�@�5?@ȼj@�
=@��T@��/@�K�@�?}@��j@�33@��^@��9@�"�@�V@�33@���@��;@�n�@��h@���@���@���@�?}@�Ĝ@�z�@�$�@��@�j@�1'@��m@��@�C�@�ff@�n�@��\@��@�dZ@���@�\)@��+@���@�x�@�G�@��@�Ĝ@�Q�@��m@�C�@��R@�ff@��@���@�G�@��@���@� �@���@�l�@�dZ@�K�@���@��R@���@���@��\@��!@�t�@���@��
@�ƨ@��w@���@�S�@�
=@���@��@��y@���@��\@�G�@���@��`@��@��j@���@��`@���@���@��@�l�@�l�@�dZ@���@���@�
=@��@�=q@�hs@��u@�(�@�dZ@��!@��#@�&�@��j@�r�@�A�@�1@��m@���@���@��
@��
@��
@��
@��;@��@�j@��@�/@�hs@���@��-@���@��T@��@�5?@�v�@��+@���@���@���@���@��!@��!@���@�v�@���@��-@���@�p�@�/@��/@���@��@�I�@��@��@��@�dZ@�K�@�33@�ȴ@��+@�~�@�n�@�^5@�J@���@��@�X@��@��`@��D@�r�@�Z@�A�@�A�@��@�t�@�;d@�o@��@���@���@��\@��\@��+@���@��!@���@�n�@�^5@�E�@��@���@���@�hs@�G�@��@�V@�%@��`@��9@���@�(�@��F@���@�t�@�C�@��y@��!@�M�@�$�@�J@��@��#@���@�7L@�&�@���@���@�Ĝ@�j@�9X@�b@���@��
@��F@���@�;d@���@��@��\@�V@�5?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A˺^A˼jA˼jA˼jA˾wA�A�A�ĜA�ĜA�ĜA�A�A�ĜA�A���A���A˴9A˴9A˶FA˼jA���A�A˸RA˴9Aˣ�A˃A�`BA�(�A�  A��A��A��\A�ƨA�ffA���A�1A�ȴA���A�33A��A�1A��RA���A�{A���A���A�&�A��A�ƨA�p�A�"�A��#A�hsA�C�A�A��jA�`BA�  A��DA��A��A���A�~�A���A��DA�n�A�z�A��
A���A�/A��A�\)A�bA���A��A��\A���A���A�jA�VA���A��;A��A�;dA�-A���A��-A�1A�A���A��yA���A��A�A�^5A���A��+A���A���A�jA��A��A�%A��A�x�A��7A�7LA�VA�ZA��/A�7LA�  A��-A~  A|AzAx��Aw��Aw
=Au��As�#Arr�Ar9XArAqƨAoS�AnA�Am|�Alv�Ak�Aj�uAi�-Ag�PAet�Ad��Ad  Ab1'AaC�A`Q�A_��A^��A]�A\�/A[��A[oAY%AWp�AV�jAV{AS�AQ�AQO�AP^5AM�TAK��AJ�HAJz�AI?}AG��AG��AGK�AF�AD��AB5?A@��A?��A>�A<�`A;XA:��A9�
A8��A7dZA6{A4v�A4VA3�A3|�A2�yA2A�A0�9A0JA/l�A.��A-�mA+��A*jA)�#A)��A)VA($�A'��A'p�A%��A$��A$VA#��A#p�A"  A �jA��Ar�A��A�hA�HA{A��A�wAC�A�AĜAE�A��A�RA�A�jA1A��AK�A��Ar�A�AO�AI�A��A;dA �A
�uA
bA	�wAĜA�A�Av�A��A�DA��AVA ��A @���@���@�dZ@�-@�7L@�I�@���@�V@�z�@�@���@�ff@�j@�w@�|�@�33@ꟾ@� �@�;d@�=q@�9@���@�x�@�j@ߍP@�$�@�G�@�"�@�%@�S�@��@Ӿw@�^5@�Ĝ@��@�X@˕�@�5?@ȼj@�
=@��T@��/@�K�@�?}@��j@�33@��^@��9@�"�@�V@�33@���@��;@�n�@��h@���@���@���@�?}@�Ĝ@�z�@�$�@��@�j@�1'@��m@��@�C�@�ff@�n�@��\@��@�dZ@���@�\)@��+@���@�x�@�G�@��@�Ĝ@�Q�@��m@�C�@��R@�ff@��@���@�G�@��@���@� �@���@�l�@�dZ@�K�@���@��R@���@���@��\@��!@�t�@���@��
@�ƨ@��w@���@�S�@�
=@���@��@��y@���@��\@�G�@���@��`@��@��j@���@��`@���@���@��@�l�@�l�@�dZ@���@���@�
=@��@�=q@�hs@��u@�(�@�dZ@��!@��#@�&�@��j@�r�@�A�@�1@��m@���@���@��
@��
@��
@��
@��;@��@�j@��@�/@�hs@���@��-@���@��T@��@�5?@�v�@��+@���@���@���@���@��!@��!@���@�v�@���@��-@���@�p�@�/@��/@���@��@�I�@��@��@��@�dZ@�K�@�33@�ȴ@��+@�~�@�n�@�^5@�J@���@��@�X@��@��`@��D@�r�@�Z@�A�@�A�@��@�t�@�;d@�o@��@���@���@��\@��\@��+@���@��!@���@�n�@�^5@�E�@��@���@���@�hs@�G�@��@�V@�%@��`@��9@���@�(�@��F@���@�t�@�C�@��y@��!@�M�@�$�@�J@��@��#@���@�7L@�&�@���@���@�Ĝ@�j@�9X@�b@���@��
@��F@���@�;d@���@��@��\@�V@�5?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB:^B:^B9XB+B��B�sB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�`B�;B�5B�BɺB�qB�B��B��B��B�JB�B|�Bt�BcTBP�B1'B'�B\B�B�mB�TB��BƨB��B�oB�B}�By�Bl�BS�B@�B2-B'�B�B{BB
��B
�sB
�
B
ÖB
��B
��B
��B
�7B
t�B
`BB
K�B
?}B
/B
'�B
�B
�B
{B
B	��B	��B	��B	�B	�fB	�;B	�B	��B	��B	ȴB	��B	�LB	�B	��B	��B	��B	�hB	�JB	�7B	�B	�B	|�B	x�B	r�B	n�B	bNB	]/B	W
B	J�B	C�B	@�B	49B	$�B	�B	 �B	�B	�B	�B	{B	{B	oB		7B	  B��B��B�B�B�yB�`B�HB�#B��B��BƨB��B��B��BȴBÖB�^B�LB�?B�!B��B��B��B��B��B��B��B��B��B�PB�\B�oB�=B�+B�VB�+B� Bz�By�By�B{�Bv�Bs�Br�Bp�Bo�Bp�Bm�Bk�BiyBhsBe`BcTBe`B`BB^5BYBYB[#BT�BT�BS�BP�BN�BL�BJ�BF�BA�B=qB<jB9XB6FB49B2-B2-B0!B-B,B+B)�B(�B'�B&�B$�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{B{BuB{BuBoBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B"�B$�B&�B(�B+B.B1'B2-B5?B6FB9XB;dB9XB?}BF�BG�BM�BO�BO�BT�Be`BiyBjBq�Bx�B}�B�B�B�%B�%B�+B�1B�=B�JB�\B�oB�{B��B��B��B��B��B��B��B��B��B��B��B�B�'B�-B�9B�FB�^B��B�B�/B�;B�HB�HB�NB�TB�ZB�`B�fB�mB�fB�B�B��B��B��B��B	B	JB	bB	oB	�B	�B	�B	�B	�B	!�B	"�B	#�B	#�B	&�B	-B	0!B	33B	33B	33B	33B	5?B	6FB	7LB	8RB	;dB	=qB	>wB	>wB	?}B	@�B	A�B	F�B	L�B	VB	[#B	aHB	cTB	dZB	ffB	gmB	iyB	l�B	q�B	t�B	v�B	w�B	x�B	z�B	{�B	}�B	� B	�B	�B	�+B	�7B	�DB	�JB	�\B	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�9B	�FB	�LB	�RB	�^B	�qB	�qB	�qB	�wB	�}B	��B	��B	��B	B	ÖB	ÖB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�/B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�;B	�BB	�BB	�;B	�BB	�;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B;dB;dB;dB;dB;dB<jB<jB;dB;dB<jB;dB;dB;dB;dB;dB<jB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB2-B��B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B  B  B  BBBB  B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�mB�BB�;B�B��BB�'B�B�B��B�bB�B~�By�BgmBXB5?B,B�B��B�B�mB�B��B�B��B�+B�B{�Bt�BYBD�B5?B+B!�B�B1BB
�B
�)B
��B
��B
��B
��B
�VB
y�B
gmB
O�B
D�B
2-B
+B
 �B
�B
�B
B	��B	��B	��B	��B	�yB	�HB	�/B	�B	��B	��B	ǮB	�jB	�B	��B	��B	��B	�{B	�\B	�DB	�+B	�B	� B	z�B	w�B	q�B	dZB	^5B	]/B	L�B	D�B	B�B	8RB	(�B	�B	!�B	�B	�B	�B	�B	�B	�B	\B	B��B��B��B��B�B�mB�ZB�5B�B��BǮB��B��B��B��BǮB�jB�RB�LB�3B�B��B��B��B��B��B��B��B��B�bB�bB�uB�DB�=B�hB�=B�B{�Bz�B{�B}�By�Bv�Bt�Bp�Bp�Bq�Bo�Bm�Bl�BjBgmBdZBgmBdZBaHB[#BZB^5BVBVBW
BT�BP�BM�BL�BK�BF�B?}B>wB<jB9XB6FB49B49B2-B0!B.B-B+B+B(�B(�B'�B%�B#�B"�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B%�B'�B'�B+B-B/B2-B33B7LB8RB:^B;dB=qBB�BF�BG�BM�BO�BP�BVBe`BiyBjBp�Bx�B}�B�B�B�+B�+B�+B�7B�DB�JB�bB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�9B�FB�XB��B�B�/B�;B�HB�HB�NB�ZB�`B�`B�fB�mB�sB�B�B��B��B��B��B	B	PB	hB	oB	�B	�B	�B	�B	�B	!�B	#�B	$�B	$�B	&�B	.B	1'B	49B	49B	33B	33B	6FB	7LB	7LB	8RB	;dB	=qB	>wB	?}B	@�B	A�B	A�B	F�B	L�B	VB	[#B	aHB	cTB	e`B	ffB	gmB	hsB	l�B	q�B	t�B	v�B	w�B	x�B	z�B	|�B	}�B	�B	�B	�%B	�+B	�=B	�DB	�JB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�'B	�-B	�-B	�3B	�?B	�FB	�RB	�XB	�dB	�qB	�qB	�qB	�wB	�wB	��B	��B	B	B	ÖB	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�;B	�HB	�BB	�BB	�BB	�BB	�BB	�;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201112181657222011121816572220111218165722  AO  ARGQ                                                                        20111130103207  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130103207  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20111218165722  IP                  G�O�G�O�G�O�                