CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:57Z UW 3.1 conversion   
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g$   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135621  20190522121825  1727_5046_022                   2C  D   APEX                            2143                            040306                          846 @�2��_�1   @�2�/_�@7vE�����c���n�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBh  Bo��Bw��B��B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDr  Dr� Dr�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @y��@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BX  B`  Bg��Bo33Bw33B33B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B�B���B���B���C�fC  C�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��fC��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��D� D  Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D�3Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DG� DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Dg  Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dq� Dq��Dry�Dr� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ƨA���A���A���A���A���A�ĜAϴ9AύPA�S�A�M�A�S�A�bNA�dZA�l�A�n�A�p�A�v�A�x�A�|�AσAχ+A�~�A�l�A�bNA�XA�S�A�;dA��`A�Q�A�1'A�A�"�A�VA��A�ZA��TA��A�ZA���A��A��#A���A�VA��FA�VA��A��!A�ƨA�G�A���A��-A�33A�A�A�5?A� �A��A��7A�ĜA���A�A��mA�|�A�oA�C�A��^A�A�p�A��!A�VA�+A��TA��uA��A��A���A��;A�M�A���A��`A��A�E�A��^A��A���A���A�
=A�A�A�ĜA�;dA��A��A�A�M�A��HA���A�|�A���A��A�v�A���A��A�VA~�A{�mA{�-Az��Axz�AwXAvE�Au\)As��Aq�Ao��An��Am�TAm�Alr�AlAjr�Ai;dAhjAgl�Ae�;Ae"�Ad�jAc�wAb�AaA`�A_��A]��A\ffA[+AZ��AZ5?AY�^AX�!AX�AW�PAV�uAV�AU�^AT�\ARVAQp�AO�FAN�+AN1AM��AM��ALĜAI�FAGXAFVAE�ADI�ACO�AB�AA7LA@ZA@  A?��A?�A?O�A>ZA=K�A<�A<bNA<-A;��A;��A;K�A:�A:�!A9�^A8��A85?A7�PA6v�A6�A5�wA4��A4$�A3/A2�\A1t�A0�+A/
=A. �A,M�A+&�A*bA)7LA(�`A(I�A'|�A'&�A%ƨA%;dA$r�A#/A!&�AA�\A��AhsA&�A1'A�AƨA��A�DA-A33A��AO�A�9A��A�A9XA�TAȴA�A��A�A�AZAAA7LA
�+A	�-A~�AbA�A+A�yA�A�9AbA ^5@���@�bN@�\)@�n�@���@���@���@��^@�I�@��T@�ƨ@�D@�@�^@�j@��@䛦@�l�@�=q@���@ߝ�@�V@�?}@�b@۾w@ۅ@�E�@ؼj@�1@ְ!@Ձ@��@�1'@ӍP@�@�&�@Ѓ@��m@�~�@�Q�@�E�@ə�@ȃ@��
@�dZ@��@��#@��@�+@�+@�S�@�ƨ@þw@��/@��H@�Ĝ@�E�@��@��y@�ff@��#@��`@��@ǥ�@��H@�~�@�`B@�?}@�j@��H@��h@�(�@��@��h@�b@�J@��@���@�G�@��9@�Q�@��m@�l�@��H@�J@��@���@���@��#@��@�K�@�X@���@�v�@�hs@���@���@�Z@���@�@�5?@��@��+@���@���@��-@�+@�ȴ@���@��\@�$�@���@���@���@��m@���@�&�@��@��@� �@� �@��P@�ȴ@��7@���@�r�@�1@�K�@��@��@���@�@��@�M�@��@��7@�O�@���@��D@���@�ƨ@�ƨ@�l�@�;d@���@�5?@��+@��!@�5?@���@�x�@���@��D@��@���@�/@�^5@���@���@���@���@�dZ@�\)@�"�@�V@��#@��h@�X@��@�z�@��@��w@���@�C�@�"�@���@��\@�v�@�=q@���@���@��7@�O�@�7L@�G�@��@�bN@��@��F@���@��@���@�C�@���@�~�@�E�@��T@���@�p�@�O�@�G�@�X@�G�@��@�%@���@���@��D@�9X@� �@�b@��;@��w@�t�@��@���@��@���@���@��7@�p�@�G�@�?}@���@��9@��D@��w@�S�@��@��R@��y@�;d@�dZ@�l�@�S�@��@�ȴ@��!@��@��#@�@���@��711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ƨA���A���A���A���A���A�ĜAϴ9AύPA�S�A�M�A�S�A�bNA�dZA�l�A�n�A�p�A�v�A�x�A�|�AσAχ+A�~�A�l�A�bNA�XA�S�A�;dA��`A�Q�A�1'A�A�"�A�VA��A�ZA��TA��A�ZA���A��A��#A���A�VA��FA�VA��A��!A�ƨA�G�A���A��-A�33A�A�A�5?A� �A��A��7A�ĜA���A�A��mA�|�A�oA�C�A��^A�A�p�A��!A�VA�+A��TA��uA��A��A���A��;A�M�A���A��`A��A�E�A��^A��A���A���A�
=A�A�A�ĜA�;dA��A��A�A�M�A��HA���A�|�A���A��A�v�A���A��A�VA~�A{�mA{�-Az��Axz�AwXAvE�Au\)As��Aq�Ao��An��Am�TAm�Alr�AlAjr�Ai;dAhjAgl�Ae�;Ae"�Ad�jAc�wAb�AaA`�A_��A]��A\ffA[+AZ��AZ5?AY�^AX�!AX�AW�PAV�uAV�AU�^AT�\ARVAQp�AO�FAN�+AN1AM��AM��ALĜAI�FAGXAFVAE�ADI�ACO�AB�AA7LA@ZA@  A?��A?�A?O�A>ZA=K�A<�A<bNA<-A;��A;��A;K�A:�A:�!A9�^A8��A85?A7�PA6v�A6�A5�wA4��A4$�A3/A2�\A1t�A0�+A/
=A. �A,M�A+&�A*bA)7LA(�`A(I�A'|�A'&�A%ƨA%;dA$r�A#/A!&�AA�\A��AhsA&�A1'A�AƨA��A�DA-A33A��AO�A�9A��A�A9XA�TAȴA�A��A�A�AZAAA7LA
�+A	�-A~�AbA�A+A�yA�A�9AbA ^5@���@�bN@�\)@�n�@���@���@���@��^@�I�@��T@�ƨ@�D@�@�^@�j@��@䛦@�l�@�=q@���@ߝ�@�V@�?}@�b@۾w@ۅ@�E�@ؼj@�1@ְ!@Ձ@��@�1'@ӍP@�@�&�@Ѓ@��m@�~�@�Q�@�E�@ə�@ȃ@��
@�dZ@��@��#@��@�+@�+@�S�@�ƨ@þw@��/@��H@�Ĝ@�E�@��@��y@�ff@��#@��`@��@ǥ�@��H@�~�@�`B@�?}@�j@��H@��h@�(�@��@��h@�b@�J@��@���@�G�@��9@�Q�@��m@�l�@��H@�J@��@���@���@��#@��@�K�@�X@���@�v�@�hs@���@���@�Z@���@�@�5?@��@��+@���@���@��-@�+@�ȴ@���@��\@�$�@���@���@���@��m@���@�&�@��@��@� �@� �@��P@�ȴ@��7@���@�r�@�1@�K�@��@��@���@�@��@�M�@��@��7@�O�@���@��D@���@�ƨ@�ƨ@�l�@�;d@���@�5?@��+@��!@�5?@���@�x�@���@��D@��@���@�/@�^5@���@���@���@���@�dZ@�\)@�"�@�V@��#@��h@�X@��@�z�@��@��w@���@�C�@�"�@���@��\@�v�@�=q@���@���@��7@�O�@�7L@�G�@��@�bN@��@��F@���@��@���@�C�@���@�~�@�E�@��T@���@�p�@�O�@�G�@�X@�G�@��@�%@���@���@��D@�9X@� �@�b@��;@��w@�t�@��@���@��@���@���@��7@�p�@�G�@�?}@���@��9@��D@��w@�S�@��@��R@��y@�;d@�dZ@�l�@�S�@��@�ȴ@��!@��@��#@�@���@��711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB&�B&�B&�B%�B%�B(�B(�B,B33B;dB=qBJ�B^5B`BBn�Bu�B{�B�1B�\B��B�B�FB�FB�FB�LB�RB�LB�XB��B��B1BDB1B  B��B��B�B�B�sB�;B�B��B��B�B��B��B��B��B��B��BȴBĜB�qB�^B�^B�XB�FB�-B��B��B�{B�JB�+B�B}�Bx�Bs�Bn�BgmBbNB_;BYBJ�B@�B1'B!�B�BVB��B�TB�B��B�LB��B�%By�Bp�BdZBS�BB�B49B(�B!�B�B\B
��B
�B
ĜB
�qB
�FB
�B
��B
�hB
�%B
x�B
v�B
r�B
dZB
]/B
YB
T�B
L�B
?}B
6FB
0!B
,B
'�B
"�B
�B
uB
DB
B	��B	�B	�B	�mB	�NB	�/B	�B	��B	ÖB	�RB	�'B	��B	��B	��B	��B	��B	��B	�{B	�uB	�oB	�bB	�=B	{�B	t�B	gmB	`BB	\)B	YB	`BB	XB	<jB	$�B	�B	{B	VB	DB	%B	B	  B��B��B��B	%B	  B��B	B	B	+B	1B	1B	
=B	DB		7B	1B	B	  B��B��B��B��B�B�B�B�mB�NB�/B�B��B��BȴBĜBB��B�qB�^B�RB�3B�'B�B��B��B��B��B��B��B��B�oB�\B�PB�DB�=B�1B�B�B� B|�Bz�Bw�Bu�Bs�Bp�Bm�Bk�BhsBgmBffBffBdZBcTB`BB^5B\)B[#BYBT�BP�BM�BK�BH�BD�BC�BB�BA�B@�B?}B?}B=qB;dB9XB7LB49B6FB9XB9XB7LB8RB7LB8RB9XB:^B9XB:^B:^B<jB<jB;dB<jB=qB=qB<jB=qB<jB;dB:^B:^B:^B9XB8RB8RB7LB7LB7LB7LB6FB6FB5?B49B8RB=qB=qB@�BG�BM�B`BBo�B� B�VB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�?B�FB�LB�XB�^B�^B�jB�dB�^B�FB�'B�B��B��B��B��B��B�B��B�B�B�9B�RB��BÖB�jB�FB�FB�FB�FB�FB�FB�^B�qB��BÖB��B�B�;B�B�B�B�B�B�B�B�B�B�B�B�mB�sB�B��B��B��B��B��B��B��B��B	  B	B	B	+B	VB	{B	�B	�B	�B	�B	�B	�B	�B	#�B	)�B	33B	8RB	?}B	G�B	W
B	XB	YB	[#B	^5B	^5B	_;B	aHB	bNB	cTB	bNB	bNB	bNB	bNB	bNB	bNB	cTB	dZB	gmB	iyB	k�B	l�B	q�B	t�B	w�B	z�B	~�B	�B	�B	�B	�+B	�1B	�7B	�7B	�7B	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�3B	�9B	�RB	�^B	�dB	�jB	�qB	��B	B	ÖB	ÖB	ÖB	ĜB	ŢB	ǮB	ǮB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B&�B&�B&�B%�B%�B(�B(�B-B49B;dB=qBJ�B^5B`BBn�Bu�B{�B�1B�\B��B�B�FB�FB�FB�LB�RB�LB�jB�B%BhB\BhB+B��B��B��B�B�B�`B�;B�B�B�B�B�B�B��B��B��BɺB��BB�^B�dB�^B�XB�FB�B��B��B�\B�=B�1B�B{�Bv�Bq�BiyBcTB`BB_;BM�BE�B7LB%�B�B�BB�yB�B�BĜB��B�DB|�Bu�Bl�B\)BH�B8RB,B%�B�BuB%B
�BB
ȴB
ÖB
�^B
�?B
��B
��B
�PB
y�B
y�B
y�B
gmB
`BB
\)B
YB
R�B
F�B
9XB
2-B
/B
)�B
$�B
#�B
�B
VB
1B
B	��B	�B	�B	�`B	�HB	�B	��B	��B	�jB	�?B	�B	��B	��B	��B	��B	��B	��B	�{B	�uB	�uB	�\B	}�B	x�B	jB	aHB	]/B	YB	bNB	`BB	B�B	'�B	�B	�B	hB	VB		7B	B	B	  B	  B��B		7B	B��B	B	%B	1B		7B		7B	DB	JB	JB	DB	+B	B	  B��B��B��B��B�B�B�B�`B�NB�)B�B��B��BǮBÖBB��B�jB�qB�?B�9B�-B�B��B��B��B��B��B��B��B�uB�\B�JB�DB�=B�1B�B�B~�B|�By�Bv�Bv�Bs�Bo�Bo�BjBiyBgmBgmBffBe`BcTBbNB^5B\)B\)B\)BVBP�BM�BN�BI�BF�BD�BC�BC�BA�BA�B?}B>wB=qB;dB9XB9XB;dB;dB<jB:^B9XB:^B;dB<jB;dB<jB<jB<jB<jB=qB>wB>wB?}B>wB>wB=qB<jB<jB;dB;dB:^B:^B;dB:^B8RB9XB8RB7LB7LB7LB7LB9XB=qB=qB@�BG�BK�B]/Bl�B}�B�PB��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B�B�'B�FB�LB�RB�dB�jB�jB�wB�qB�jB�XB�?B�?B�B��B��B��B�B�B�B�B�B�3B�LB��BƨB��B�LB�FB�LB�LB�FB�LB�jB�}B��B��B��B�B�/B�B�B�B�B�B�B�B�B��B�B�B�sB�sB�B��B��B��B��B��B��B��B��B	B	B	B	1B	VB	{B	�B	�B	�B	�B	�B	�B	�B	"�B	)�B	33B	8RB	@�B	D�B	XB	XB	ZB	]/B	_;B	_;B	`BB	bNB	cTB	dZB	bNB	bNB	cTB	bNB	cTB	bNB	cTB	dZB	hsB	iyB	k�B	l�B	q�B	t�B	x�B	{�B	� B	�B	�B	�B	�+B	�7B	�=B	�7B	�=B	�VB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�3B	�?B	�XB	�^B	�dB	�qB	�}B	��B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ɺB	ȴB	ǮB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446412012010314464120120103144641  AO  ARGQ                                                                        20111130135621  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135621  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144641  IP                  G�O�G�O�G�O�                