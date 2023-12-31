CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:47Z UW 3.1 conversion   
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
_FillValue                 �  A@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ex   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  o    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �l   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143424  20190522121828  1727_5046_199                   2C  D   APEX                            2143                            040306                          846 @�Ή 1   @�Ϧ��@7	7KƧ��c�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @��@�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9y�D:  D:� D;  D;� D<  D<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� DcfDc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dry�Ds  Ds� DyS311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@33@y��@���@���A   A>ffA^ffA~ffA�33A�33A�33A�ffA�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bh  Bp  Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D�3Ds3D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Ds3D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$� D%  D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8�3D9s3D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=�3D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Dc  Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgs3Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Drs3Dr��Dsy�DyL�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�jA�l�A�n�A�p�A�n�A�n�A�n�A�n�A�p�A�t�A�z�A�z�A�|�A�x�A�r�A�ZA���A�dZA�A��DA�l�A�^5A�VA�M�A�K�A�G�A�C�A�A�A�7LA�%A���A��A��TA�ĜA��!A�~�A�M�A�7LA�VA�A�
=A���A��A�z�A�t�A�n�A�ffA�`BA�ZA�A�A�A���A��A�ZA�^5A�&�A��/A��A��!A�ƨA��A��A�VA��9A��DA���A�-A�O�A���A�?}A��A�9XA�VA�G�A��A��HA���A�G�A���A�ȴA�`BA�A��wA�I�A�ƨA�Q�A�7LA�A�O�A�r�A���A��A��+A��RA�E�A��9A�n�A���A���A�oA��\A�$�A��PA��TA�ZA�ĜA���A��DA��A�A�ĜA�9XA��;A�  A�M�A�|�A��A�A}��Az�Aw��At��As��Ar�\Ao�FAn5?Al�AjZAf��Aa�mA_`BA_oA^�DA]hsA[�-AY�mAV��AS|�AP��AM�7ALM�AJȴAHJAF��AE�ADbNAB9XA@��A@1A?�^A>�A<��A;%A:{A8�yA7�A65?A3��A2�HA1dZA1oA0^5A/?}A.ȴA-S�A,A+XA)�^A)�A(v�A'��A&��A&{A%O�A#hsA"-A Q�A�\A?}A^5A�;A��A�7At�AK�A�`A�^A��A�AQ�A|�AC�A�jA�hA�AZA��A�A?}A
=A��Ar�A�-A��A��A1A�#A�A
��A
�A	�-A	C�AffA��AQ�A��A33A��A��A�jA�-A�!A1Ahs@��@�ȴ@���@�"�@���@���@���@���@���@��@�C�@��H@��@���@��
@��@��@�@�@�F@��@�\)@��@��@��@�S�@��u@��@���@�@�hs@�j@�"�@��@� �@�1'@��;@ۅ@�C�@�E�@�&�@�9X@�"�@֧�@�n�@�{@�p�@�%@Լj@ԣ�@ԃ@Ӯ@���@��m@��@��;@���@�@ɡ�@�G�@��/@�I�@��
@�\)@�33@�S�@�^5@őh@�j@�33@�"�@��H@°!@�^5@��^@��`@��@��+@�M�@�{@�O�@�9X@���@��@�dZ@��@��@�x�@�O�@���@�  @�o@�-@�&�@��D@���@�t�@�"�@�@��!@��@�G�@�?}@���@�1'@��m@���@���@��+@�-@���@���@�j@�  @�|�@�;d@��#@�V@���@�Q�@���@�dZ@��!@�M�@��7@�V@���@�z�@��w@�|�@��@��@�p�@��@��9@��@�I�@��
@�\)@�
=@�=q@��^@���@���@�j@�9X@���@��F@�l�@��@��H@���@��R@���@�n�@�E�@�@���@�X@�?}@��@���@��@�Q�@�A�@� �@��@��@�;d@���@��!@��+@�n�@�^5@�-@��-@��7@�G�@��@�%@���@���@��/@���@���@���@��@�r�@�1'@�b@��@���@�V@���@��7@��j@�bN@�9X@��@���@�+@�
=@���@��@���@�^5@�V@�E�@�{@���@���@��7@�p�@�O�@���@�Ĝ@�z�@�A�@�  @�ƨ@��P@�ȴ@���@���@��+@�-@��@�@��h@�X@�G�@�7L@�%@�V@�Ĝ@��@��9@��j@��j@���@�(�@�1@��F@�|�@�+@�"�@�o@��@��y@���@��!@���@���@��\@�^5@�=q@��@��@���@��@�G�@�/@�%@��m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�jA�l�A�n�A�p�A�n�A�n�A�n�A�n�A�p�A�t�A�z�A�z�A�|�A�x�A�r�A�ZA���A�dZA�A��DA�l�A�^5A�VA�M�A�K�A�G�A�C�A�A�A�7LA�%A���A��A��TA�ĜA��!A�~�A�M�A�7LA�VA�A�
=A���A��A�z�A�t�A�n�A�ffA�`BA�ZA�A�A�A���A��A�ZA�^5A�&�A��/A��A��!A�ƨA��A��A�VA��9A��DA���A�-A�O�A���A�?}A��A�9XA�VA�G�A��A��HA���A�G�A���A�ȴA�`BA�A��wA�I�A�ƨA�Q�A�7LA�A�O�A�r�A���A��A��+A��RA�E�A��9A�n�A���A���A�oA��\A�$�A��PA��TA�ZA�ĜA���A��DA��A�A�ĜA�9XA��;A�  A�M�A�|�A��A�A}��Az�Aw��At��As��Ar�\Ao�FAn5?Al�AjZAf��Aa�mA_`BA_oA^�DA]hsA[�-AY�mAV��AS|�AP��AM�7ALM�AJȴAHJAF��AE�ADbNAB9XA@��A@1A?�^A>�A<��A;%A:{A8�yA7�A65?A3��A2�HA1dZA1oA0^5A/?}A.ȴA-S�A,A+XA)�^A)�A(v�A'��A&��A&{A%O�A#hsA"-A Q�A�\A?}A^5A�;A��A�7At�AK�A�`A�^A��A�AQ�A|�AC�A�jA�hA�AZA��A�A?}A
=A��Ar�A�-A��A��A1A�#A�A
��A
�A	�-A	C�AffA��AQ�A��A33A��A��A�jA�-A�!A1Ahs@��@�ȴ@���@�"�@���@���@���@���@���@��@�C�@��H@��@���@��
@��@��@�@�@�F@��@�\)@��@��@��@�S�@��u@��@���@�@�hs@�j@�"�@��@� �@�1'@��;@ۅ@�C�@�E�@�&�@�9X@�"�@֧�@�n�@�{@�p�@�%@Լj@ԣ�@ԃ@Ӯ@���@��m@��@��;@���@�@ɡ�@�G�@��/@�I�@��
@�\)@�33@�S�@�^5@őh@�j@�33@�"�@��H@°!@�^5@��^@��`@��@��+@�M�@�{@�O�@�9X@���@��@�dZ@��@��@�x�@�O�@���@�  @�o@�-@�&�@��D@���@�t�@�"�@�@��!@��@�G�@�?}@���@�1'@��m@���@���@��+@�-@���@���@�j@�  @�|�@�;d@��#@�V@���@�Q�@���@�dZ@��!@�M�@��7@�V@���@�z�@��w@�|�@��@��@�p�@��@��9@��@�I�@��
@�\)@�
=@�=q@��^@���@���@�j@�9X@���@��F@�l�@��@��H@���@��R@���@�n�@�E�@�@���@�X@�?}@��@���@��@�Q�@�A�@� �@��@��@�;d@���@��!@��+@�n�@�^5@�-@��-@��7@�G�@��@�%@���@���@��/@���@���@���@��@�r�@�1'@�b@��@���@�V@���@��7@��j@�bN@�9X@��@���@�+@�
=@���@��@���@�^5@�V@�E�@�{@���@���@��7@�p�@�O�@���@�Ĝ@�z�@�A�@�  @�ƨ@��P@�ȴ@���@���@��+@�-@��@�@��h@�X@�G�@�7L@�%@�V@�Ĝ@��@��9@��j@��j@���@�(�@�1@��F@�|�@�+@�"�@�o@��@��y@���@��!@���@���@��\@�^5@�=q@��@��@���@��@�G�@�/@�%@��m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�`B�`B�fB�`B�`B�`B�`B�`B�`B�`B�`B�`B�`B�`B�`B�mB�B�B/B8RB=qB?}BA�BA�BA�BA�BA�BA�BC�B@�B?}B>wBD�BE�BF�BK�BO�BR�BXBaHBm�Bt�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Br�Bp�Bo�Bq�Bv�Bw�B|�B�B}�Bn�BaHBS�BF�B)�BB�B�FB�!B�9B�3B�3B��BÖB�FB�B��B��B��B��B��B��B�oB�VB�%B~�Bx�Bu�Bo�BaHBM�B5?BB�#B�jB��B��B��B�JB|�B]/BS�BJ�B8RB+B{B  B
�B
�yB
�)B
�dB
�B
��B
��B
�hB
z�B
n�B
bNB
G�B
8RB
'�B
B	��B	�B	�HB	��B	ÖB	�FB	��B	��B	y�B	n�B	l�B	hsB	aHB	W
B	J�B	8RB	(�B	�B	bB		7B	B��B�B�B�yB�NB�5B�)B�B�B��BȴBŢB��B�qB�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�hB�\B�JB�=B�1B�%B�%B�%B�+B�+B�+B�+B�%B�B�B�B�B�B�B�B� B�B~�B}�B}�B}�B}�B{�B|�B{�B{�Bz�By�Bz�Bz�By�B{�Bz�Bz�By�Bx�Bw�Bw�Bv�Bu�Bu�Bt�Bs�Br�Bo�Bl�BiyBk�Bo�Bq�Bs�Bu�By�Bx�Br�Bn�BjBhsBhsBk�Bq�Bw�By�Bx�B{�B|�B~�B� B~�B}�Bw�Bq�Bk�BgmBe`Be`BgmBk�BiyBffBjBw�B�1B�=B�1B�%B�B�1B�DB�VB�\B�bB�\B�\B�\B�\B�VB�PB�JB�JB�\B�bB�{B��B��B��B��B��B��B��B��B��B�'B�B�B�B�3B�?B�?B�9B�9B�-B�'B�B�9BǮBȴB��B��B��B��B��B��B�
B�B�B�)B�;B�TB�fB�B�B�B�B��B��B��B��B��B��B	  B	B	%B	1B	JB	PB	\B	oB	�B	�B	�B	�B	�B	'�B	-B	.B	1'B	6FB	7LB	<jB	>wB	C�B	G�B	H�B	J�B	P�B	Q�B	T�B	[#B	^5B	`BB	bNB	cTB	cTB	dZB	ffB	hsB	m�B	q�B	w�B	y�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�B	�1B	�=B	�JB	�PB	�VB	�\B	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�9B	�9B	�?B	�FB	�FB	�FB	�FB	�FB	�LB	�^B	�dB	�dB	�dB	�wB	��B	��B	ƨB	ƨB	ŢB	ǮB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�/B	�5B	�5B	�5B	�/B	�)B	�5B	�BB	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�`B�`B�fB�`B�`B�`B�`B�`B�`B�`B�`B�`B�`B�`B�`B�mB�B�B/B8RB=qB?}BA�BA�BA�BA�BA�BA�BC�B@�B?}B>wBD�BE�BF�BK�BO�BR�BXBaHBm�Bt�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Br�Bp�Bo�Bq�Bv�Bw�B|�B�B}�Bn�BaHBS�BF�B)�BB�B�FB�!B�9B�3B�3B��BÖB�FB�B��B��B��B��B��B��B�oB�VB�%B~�Bx�Bu�Bo�BaHBM�B5?BB�#B�jB��B��B��B�JB|�B]/BS�BJ�B8RB+B{B  B
�B
�yB
�)B
�dB
�B
��B
��B
�hB
z�B
n�B
bNB
G�B
8RB
'�B
B	��B	�B	�HB	��B	ÖB	�FB	��B	��B	y�B	n�B	l�B	hsB	aHB	W
B	J�B	8RB	(�B	�B	bB		7B	B��B�B�B�yB�NB�5B�)B�B�B��BȴBŢB��B�qB�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�hB�\B�JB�=B�1B�%B�%B�%B�+B�+B�+B�+B�%B�B�B�B�B�B�B�B� B�B~�B}�B}�B}�B}�B{�B|�B{�B{�Bz�By�Bz�Bz�By�B{�Bz�Bz�By�Bx�Bw�Bw�Bv�Bu�Bu�Bt�Bs�Br�Bo�Bl�BiyBk�Bo�Bq�Bs�Bu�By�Bx�Br�Bn�BjBhsBhsBk�Bq�Bw�By�Bx�B{�B|�B~�B� B~�B}�Bw�Bq�Bk�BgmBe`Be`BgmBk�BiyBffBjBw�B�1B�=B�1B�%B�B�1B�DB�VB�\B�bB�\B�\B�\B�\B�VB�PB�JB�JB�\B�bB�{B��B��B��B��B��B��B��B��B��B�'B�B�B�B�3B�?B�?B�9B�9B�-B�'B�B�9BǮBȴB��B��B��B��B��B��B�
B�B�B�)B�;B�TB�fB�B�B�B�B��B��B��B��B��B��B	  B	B	%B	1B	JB	PB	\B	oB	�B	�B	�B	�B	�B	'�B	-B	.B	1'B	6FB	7LB	<jB	>wB	C�B	G�B	H�B	J�B	P�B	Q�B	T�B	[#B	^5B	`BB	bNB	cTB	cTB	dZB	ffB	hsB	m�B	q�B	w�B	y�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�B	�1B	�=B	�JB	�PB	�VB	�\B	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�9B	�9B	�?B	�FB	�FB	�FB	�FB	�FB	�LB	�^B	�dB	�dB	�dB	�wB	��B	��B	ƨB	ƨB	ŢB	ǮB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�/B	�5B	�5B	�5B	�/B	�)B	�5B	�BB	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�B	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201201031450352012010314503520120103145035  AO  ARGQ                                                                        20111130143424  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143424  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103145035  IP                  G�O�G�O�G�O�                