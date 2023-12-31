CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:52Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135441  20190522121825  1727_5046_001                   2C  D   APEX                            2143                            040306                          846 @������1   @����@@6�O�;dZ�c�?|�h1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DG��DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DUy�DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dy��D�&fD�  D��3D��3D�  D�3D�� D��fD��D�l�D��3D��fD�3D�` DڦfD�ٚD��D�\�D�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @9��@y��@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Ds3D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D�3Ds3D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(�3D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG�3DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUs3DU��DVy�DV��DWy�DW��DXy�DX��DY� DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dy�fD�#3D��D�� D�� D��D� D���D��3D��D�i�D�� D��3D� D�\�Dڣ3D��fD�	�D�Y�D�fD�ٚ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ĜA���A��7A��uA��A�n�A�K�A���A���A�~�A��wA��;A���A���A��wA���A�l�A�S�A�C�A�JA���A��PA�7LA���A��A�1'A��9A��A�jA�%A�K�A�jA�x�A�-A���A��+A�K�A��9A��TA�jA��A�bNA���A��A��-A�v�A��A���A���A�l�A�^5A�5?A��7A�M�A��;A�"�A��A��
A���A�\)A��FA�dZA���A�I�A�;dA�5?A�(�A���A�n�A��9A�{A�1A���A���A���A�%A���A��uA��^A�ZA��FA�A�  A���A��A��\A�Q�A��jA��A�G�A��A�S�A�  A�G�A�l�A�p�A��9A�n�A~�`A|��Aw�-AtM�Asp�As&�Ar�Ar��ArffAp�+Ao
=Am|�Ai��Af�Ae�-Adz�AcƨAbn�AbA�Aa��A`��A_p�A]��A\�\A[�^AZv�AY�AY33AX �AW/AVn�AU�AU��AT�HATjAS��AR��AP�AO�#AO&�AM��AK��AJ�`AJ=qAIoAG�#AFE�AC�A>9XA=XA<�A;�^A:�/A8�HA8(�A7hsA6v�A5�;A4�uA3A3�A2�yA2^5A1`BA1oA0�RA/�
A.��A-hsA,ZA+�PA*�/A*��A)�^A(��A'`BA'`BA&��A%��A$��A$jA#dZA"�A!/A�hA �A&�A(�Ar�A{A\)A�^A^5A�7AO�A&�A�A��A�A �AXA�FA{A
��A
�A	�#A	/A��A1Ap�A��A  A�A��A��A^5AC�A ��A Z@���@�S�@��@�@��9@�+@��h@��j@�A�@���@���@�@�~�@�@�  @�=q@�`B@�?}@�G�@�p�@�@�O�@���@��;@�@�  @�n�@�bN@�+@���@�;d@�5?@���@��@�Q�@�b@�;d@�E�@�&�@��/@܃@�"�@���@ڏ\@�r�@��H@��H@֏\@��@�@Ձ@ԋD@Ӯ@��m@�b@���@�33@ѡ�@���@�(�@��@��
@�\)@��@�@�x�@�Ĝ@�t�@Ɂ@�+@�Q�@Ý�@�5?@�z�@��-@���@�z�@� �@�K�@��j@�+@�^5@�`B@��@��y@��@�b@���@��H@��@��@�;d@��+@��T@�`B@��D@��@���@�~�@�$�@�@���@���@�?}@�7L@��@��@���@�Ĝ@��j@���@�z�@���@��y@���@�v�@�X@�`B@�$�@�V@���@�Z@�(�@�
=@���@��!@��\@�^5@�@�%@�I�@�K�@�I�@�A�@�9X@��m@���@��+@���@��u@���@��y@�J@�@�x�@�O�@�%@��@��j@��/@�&�@�x�@�G�@�/@��@��@���@���@��9@�9X@���@���@�S�@�33@�@��+@�@�&�@�bN@�Q�@�z�@�z�@��@�l�@�\)@���@�E�@�=q@��@�&�@�&�@��@���@���@��u@�bN@�;d@���@���@�$�@��@��@���@��-@���@���@���@�O�@�Ĝ@�A�@�l�@���@�S�@�;d@�K�@�S�@�l�@��P@��@�;d@��!@��R@���@�^5@�p�@�
=@��h@��@�hs@�/@�p�@�M�@�
=@��@�ȴ@���@�ȴ@�n�@���@��#@���@��@��9@��w@���@��@�t�@�S�@�
=@��H@���@���@�M�@�{@���@��@��T@��#@��7@�?}@��@l�@;d@K�@K�@�@~��@}�@|1@|�D@|��@}/@|��@|�@|(�@{��@{�
@{dZ@z��@z~�@z�\@z~�@z~�@zM�@y�@xr�@w;d@yG�@p��@nff@b�H@Z�@P��@JM�@EO�@>��@9�#@2�H@-�@*��@&��@!hs@V@l�@1@|�@
M�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ĜA���A��7A��uA��A�n�A�K�A���A���A�~�A��wA��;A���A���A��wA���A�l�A�S�A�C�A�JA���A��PA�7LA���A��A�1'A��9A��A�jA�%A�K�A�jA�x�A�-A���A��+A�K�A��9A��TA�jA��A�bNA���A��A��-A�v�A��A���A���A�l�A�^5A�5?A��7A�M�A��;A�"�A��A��
A���A�\)A��FA�dZA���A�I�A�;dA�5?A�(�A���A�n�A��9A�{A�1A���A���A���A�%A���A��uA��^A�ZA��FA�A�  A���A��A��\A�Q�A��jA��A�G�A��A�S�A�  A�G�A�l�A�p�A��9A�n�A~�`A|��Aw�-AtM�Asp�As&�Ar�Ar��ArffAp�+Ao
=Am|�Ai��Af�Ae�-Adz�AcƨAbn�AbA�Aa��A`��A_p�A]��A\�\A[�^AZv�AY�AY33AX �AW/AVn�AU�AU��AT�HATjAS��AR��AP�AO�#AO&�AM��AK��AJ�`AJ=qAIoAG�#AFE�AC�A>9XA=XA<�A;�^A:�/A8�HA8(�A7hsA6v�A5�;A4�uA3A3�A2�yA2^5A1`BA1oA0�RA/�
A.��A-hsA,ZA+�PA*�/A*��A)�^A(��A'`BA'`BA&��A%��A$��A$jA#dZA"�A!/A�hA �A&�A(�Ar�A{A\)A�^A^5A�7AO�A&�A�A��A�A �AXA�FA{A
��A
�A	�#A	/A��A1Ap�A��A  A�A��A��A^5AC�A ��A Z@���@�S�@��@�@��9@�+@��h@��j@�A�@���@���@�@�~�@�@�  @�=q@�`B@�?}@�G�@�p�@�@�O�@���@��;@�@�  @�n�@�bN@�+@���@�;d@�5?@���@��@�Q�@�b@�;d@�E�@�&�@��/@܃@�"�@���@ڏ\@�r�@��H@��H@֏\@��@�@Ձ@ԋD@Ӯ@��m@�b@���@�33@ѡ�@���@�(�@��@��
@�\)@��@�@�x�@�Ĝ@�t�@Ɂ@�+@�Q�@Ý�@�5?@�z�@��-@���@�z�@� �@�K�@��j@�+@�^5@�`B@��@��y@��@�b@���@��H@��@��@�;d@��+@��T@�`B@��D@��@���@�~�@�$�@�@���@���@�?}@�7L@��@��@���@�Ĝ@��j@���@�z�@���@��y@���@�v�@�X@�`B@�$�@�V@���@�Z@�(�@�
=@���@��!@��\@�^5@�@�%@�I�@�K�@�I�@�A�@�9X@��m@���@��+@���@��u@���@��y@�J@�@�x�@�O�@�%@��@��j@��/@�&�@�x�@�G�@�/@��@��@���@���@��9@�9X@���@���@�S�@�33@�@��+@�@�&�@�bN@�Q�@�z�@�z�@��@�l�@�\)@���@�E�@�=q@��@�&�@�&�@��@���@���@��u@�bN@�;d@���@���@�$�@��@��@���@��-@���@���@���@�O�@�Ĝ@�A�@�l�@���@�S�@�;d@�K�@�S�@�l�@��P@��@�;d@��!@��R@���@�^5@�p�@�
=@��h@��@�hs@�/@�p�@�M�@�
=@��@�ȴ@���@�ȴ@�n�@���@��#@���@��@��9@��w@���@��@�t�@�S�@�
=@��H@���@���@�M�@�{@���@��@��T@��#@��7@�?}@��@l�@;d@K�@K�@�@~��@}�@|1@|�D@|��@}/@|��@|�@|(�@{��@{�
@{dZ@z��@z~�@z�\@z~�@z~�@zM�@y�@xr�@w;d@yG�@p��@nff@b�H@Z�@P��@JM�@EO�@>��@9�#@2�H@-�@*��@&��@!hs@V@l�@1@|�@
M�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�3B�?B�LB�RB�RB�LB�XB�RB�LB�qB�RB��B��B��B��B�B�B��B��B��B��B��B��B�hB�DB�VB�JB�B{�Bx�Bw�Bt�Bn�BdZBYBP�BE�B49B%�B�BhB
=B��B�B�B�B�B�fB�B��B�dB��B��B�XB��B�B�BB��B�}B�qB�3B��B�VB�Bq�BaHB5?B�BDB
��B
��B
�^B
�?B
��B
��B
�hB
� B
}�B
`BB
P�B
,B
uB
DB
	7B
1B
+B
B	��B	�B	�;B	��B	�'B	�B	��B	��B	��B	�-B	�dB	�FB	�B	��B	��B	�{B	�PB	�7B	�B	~�B	{�B	y�B	w�B	u�B	s�B	q�B	o�B	jB	bNB	_;B	ZB	P�B	D�B	@�B	<jB	6FB	-B	"�B	B�)B��B��BɺB��B�jB�dB�XB�RB�LB�?B�3B�-B�'B�B�B�B�B�B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B�hB�DB�B� Bx�Bv�Bs�Bq�Bp�Bq�Bq�Bq�Bp�Bn�Bp�Bp�BgmBcTBaHB`BB`BB_;B]/B\)BZBXBVBS�BO�BJ�BH�BF�BE�BE�BD�BC�BC�BB�BA�B?}B>wB=qB<jB;dB:^B9XB7LB6FB49B6FBA�BI�BL�BN�BO�BR�BW
B\)BcTBdZBaHB`BB_;BbNBgmBo�Bu�Bu�Bu�Bt�Bs�Bs�Bs�Bu�Bu�Bt�By�Bz�By�B� B�1B�1B�DB�VB�VB�\B�hB��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B�hB�1B�B�B� B{�Bv�By�B{�B|�B|�B|�B{�B|�B|�B� B�=B�PB�\B�oB��B��B��B��B��B��B�B�B�B�B�'B�3B�?B�FB�LB�RB�RB�^B�^B�qB��BBBĜBȴB��B��B�B�#B�)B�#B�5B�sB�B�B��B�B�B�B��B	B	+B	PB	bB	bB	uB	bB	hB	�B	�B	�B	�B	�B	!�B	$�B	%�B	)�B	-B	2-B	6FB	9XB	:^B	:^B	<jB	A�B	F�B	J�B	K�B	M�B	P�B	P�B	Q�B	R�B	T�B	W
B	YB	ZB	]/B	_;B	bNB	dZB	dZB	cTB	cTB	jB	m�B	q�B	t�B	w�B	w�B	w�B	w�B	w�B	w�B	x�B	x�B	z�B	~�B	� B	� B	� B	�B	�+B	�7B	�=B	�PB	�PB	�PB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�bB	�bB	�{B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�!B	�'B	�'B	�'B	�'B	�-B	�-B	�3B	�?B	�^B	�qB	�}B	��B	��B	B	ÖB	ÖB	B	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�sB	��B
+B
�B
%�B
,B
2-B
9XB
@�B
G�B
I�B
N�B
R�B
W
B
]/B
bNB
iyB
m�B
r�B
w�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�9B�FB�XB�^B�dB�qB�dB�dB�XB��B�jB�B��B��B�B�B�B�B��B��B��B��B��B�uB�PB�hB�bB�B|�Bx�Bx�Bx�Bu�Bl�B]/BT�BM�B8RB(�B�BuBbB��B�B�B�B�B�B�#B��BĜB��B��B�XB��B��B�fB��B��B��B�jB��B�bB�PBs�Bm�B;dB�B\BB
�/B
�jB
�RB
��B
��B
��B
�B
�+B
ffB
]/B
5?B
�B
JB

=B
1B
1B

=B
B	��B	�B	ɺB	�?B	�!B	��B	��B	��B	�3B	�}B	�dB	�-B	��B	��B	��B	�\B	�JB	�1B	�B	~�B	{�B	x�B	x�B	u�B	t�B	r�B	q�B	ffB	bNB	`BB	W
B	G�B	B�B	@�B	9XB	1'B	+B	\B�5B�
B��B��BƨB�wB�qB�jB�^B�dB�RB�?B�3B�3B�-B�B�'B�'B�!B�B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B�\B�7B�+B�By�Bx�Bu�Br�Br�Br�Bq�Bq�Br�Bt�Br�Bl�BhsBe`BbNBaHBaHB_;B^5B\)BZBYBW
BS�BM�BI�BI�BF�BG�BE�BD�BD�BD�BC�BB�BA�B?}B=qB>wB<jB<jB9XB8RB7LB9XBC�BI�BL�BN�BO�BS�BXB^5BgmBhsBdZBdZBbNBbNBgmBq�Bw�Bv�Bv�Bu�Bu�Bu�Bu�Bv�Bv�Bw�Bz�B{�B}�B�B�1B�7B�JB�VB�\B�bB�oB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B��B��B��B��B��B�=B�B�B�B�By�B{�B}�B~�B�B�B}�B|�B|�B~�B�=B�VB�hB�uB��B��B��B��B��B��B�B�B�B�B�'B�3B�?B�FB�LB�RB�RB�dB�jB�wB��BÖBĜBĜBǮB��B��B�B�)B�5B�)B�5B�sB�B�B��B�B�B�B��B	B	1B	PB	oB	hB	�B	oB	oB	�B	�B	�B	�B	 �B	!�B	$�B	%�B	)�B	-B	2-B	6FB	9XB	:^B	:^B	<jB	B�B	G�B	J�B	K�B	N�B	P�B	P�B	R�B	S�B	VB	XB	YB	ZB	]/B	aHB	bNB	dZB	e`B	dZB	cTB	k�B	n�B	q�B	t�B	w�B	w�B	w�B	x�B	y�B	x�B	x�B	y�B	z�B	~�B	� B	� B	� B	�B	�+B	�=B	�DB	�VB	�VB	�PB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�bB	�bB	�{B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�'B	�-B	�!B	�'B	�'B	�'B	�-B	�-B	�-B	�3B	�FB	�^B	�qB	�}B	��B	��B	ÖB	ĜB	ĜB	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�sB	��B
+B
�B
%�B
,B
2-B
9XB
@�B
G�B
I�B
N�B
R�B
W
B
]/B
bNB
iyB
m�B
r�B
w�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446342012010314463420120103144634  AO  ARGQ                                                                        20111130135441  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135441  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144634  IP                  G�O�G�O�G�O�                