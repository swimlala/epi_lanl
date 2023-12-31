CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:06:41Z AOML 3.0 creation; 2016-05-31T21:48:59Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230641  20160531144859  5903738 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4053_7107_013                   2C  D   APEX                            5370                            041511                          846 @�K���1   @�K��RP@7��l�C��b�^5?}1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�fD�	�D�C3D�l�D���D��D�L�D���D��fD�  D�6fD�c3Dǳ3D�3D�@ Dڠ D��3D�  D�0 D� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BH  BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dy� D�gD�@ D�i�D�ٚD��D�I�D��gD��3D���D�33D�` Dǰ D�  D�<�Dڜ�D�� D���D�,�D�|�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�t�A�t�A�t�A�hsA�^5A�^5A�`BA��A��A��`A��#A���A�S�A�I�A�E�A�=qA�5?A�-A��A��A��A��A��A�(�A�1'A�5?A�9XA�C�A�E�A���A��A��A��A��A���A�1A�A�1A�;dA���A�+A���A��\A��/A�ĜA��7A�K�A�%A��#A���A�VA���A���A�1'A�{A��A�5?A�A�A�33A�
=A��A��-A��FA��RA�Q�A���A�^5A��A���A�$�A���A���A�n�A���A���A�{A�ĜA��A��+A��RA��A�33A��^A��7A��RA���A�l�A�bNA�n�A���A��A�r�A���A�?}A�ffA�t�A��;A�JA���A�z�A��A��A���A��yA�7LA��A�C�A�;dA�p�A��;A�7LA�JA��A���A���A��;A�ȴA��7A���A��A~�A{�Ay��Aw%Au�7AtZAr�9Ao�An��Am�Aj{Af��Ad�\Acl�Aa`BA^�A\��A\z�A]�-A]�AZ��AWhsAU��ATbNAS/AQ��AP{AM�AL�jAJ�\AIS�AH  AG?}AFr�AD�!AB �A@�!A?/A=O�A<(�A<bA:�jA9\)A8��A7G�A5��A4M�A3��A3��A2��A1�
A0�A.�HA-hsA,5?A+S�A*1'A(��A(1'A'�7A&��A&jA%��A$1A"�yA"9XA ��A��A�9A��A%A$�A�HAbA?}AjA�
A\)A��A�#AG�A��A�#A��AƨA\)A/A��A �A�7A�AffAbNA�-AhsAK�A�`A��AffA�
A
Q�A	t�A�yAbNAJA�A�AJA�A�\A�A��A�A�jA��A�\A�mAG�A ��A  �@�{@�b@�J@���@���@��7@�j@�\)@�hs@��@�1'@�Ĝ@�V@�M�@�+@�Z@�v�@�&�@��#@�X@�@�!@�9@�dZ@�@��D@�o@���@܋D@ڟ�@�bN@�A�@��@�G�@�b@ӥ�@�|�@��y@Ѳ-@ϝ�@�p�@̓u@ˮ@�@�n�@�@�/@�1'@�K�@��@ř�@�G�@�j@�dZ@°!@�$�@��7@�X@��@��@�ƨ@���@�"�@�v�@�V@���@��@���@�`B@���@��9@�M�@��w@�r�@�b@�;d@�|�@�1'@���@��P@�p�@��@��@��T@��/@�@��@�1@�"�@��@�@��\@�{@�`B@��@��`@��@�A�@�ƨ@��@�\)@�@���@���@��+@��#@��^@�/@�/@��@�&�@��9@��@��u@���@�/@�x�@�X@��/@��@�I�@���@��
@�|�@�K�@���@�M�@�5?@�@�@�O�@�%@���@���@��D@�1'@�b@��m@��P@�t�@�o@��!@�$�@�J@���@�7L@��j@��`@�j@��
@��P@�t�@�;d@���@���@��^@��7@�&�@�Ĝ@��u@�Q�@�  @�ƨ@���@�dZ@�K�@�K�@�;d@�dZ@�S�@��y@���@�V@�J@��7@��@��/@���@��u@��D@�r�@�bN@�Q�@�1'@���@��m@��w@���@�t�@��@��@�@��R@�v�@�M�@�{@���@�7L@��@���@��u@�z�@�bN@�A�@�1'@� �@�  @���@��
@���@��@��;@���@��7@��9@�33@�o@�C�@�o@�@���@�v�@�E�@�E�@�{@���@��-@�x�@�G�@�&�@��@��@���@��@���@�bN@�I�@�1'@�b@�  @���@��
@��w@��@�t�@�dZ@�S�@�S�@�"�@��D@xr�@p��@f��@]�h@Vȴ@O�@K@D��@?
=@9%@3t�@+��@(A�@#@@X@�/@X@�T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�t�A�t�A�t�A�hsA�^5A�^5A�`BA��A��A��`A��#A���A�S�A�I�A�E�A�=qA�5?A�-A��A��A��A��A��A�(�A�1'A�5?A�9XA�C�A�E�A���A��A��A��A��A���A�1A�A�1A�;dA���A�+A���A��\A��/A�ĜA��7A�K�A�%A��#A���A�VA���A���A�1'A�{A��A�5?A�A�A�33A�
=A��A��-A��FA��RA�Q�A���A�^5A��A���A�$�A���A���A�n�A���A���A�{A�ĜA��A��+A��RA��A�33A��^A��7A��RA���A�l�A�bNA�n�A���A��A�r�A���A�?}A�ffA�t�A��;A�JA���A�z�A��A��A���A��yA�7LA��A�C�A�;dA�p�A��;A�7LA�JA��A���A���A��;A�ȴA��7A���A��A~�A{�Ay��Aw%Au�7AtZAr�9Ao�An��Am�Aj{Af��Ad�\Acl�Aa`BA^�A\��A\z�A]�-A]�AZ��AWhsAU��ATbNAS/AQ��AP{AM�AL�jAJ�\AIS�AH  AG?}AFr�AD�!AB �A@�!A?/A=O�A<(�A<bA:�jA9\)A8��A7G�A5��A4M�A3��A3��A2��A1�
A0�A.�HA-hsA,5?A+S�A*1'A(��A(1'A'�7A&��A&jA%��A$1A"�yA"9XA ��A��A�9A��A%A$�A�HAbA?}AjA�
A\)A��A�#AG�A��A�#A��AƨA\)A/A��A �A�7A�AffAbNA�-AhsAK�A�`A��AffA�
A
Q�A	t�A�yAbNAJA�A�AJA�A�\A�A��A�A�jA��A�\A�mAG�A ��A  �@�{@�b@�J@���@���@��7@�j@�\)@�hs@��@�1'@�Ĝ@�V@�M�@�+@�Z@�v�@�&�@��#@�X@�@�!@�9@�dZ@�@��D@�o@���@܋D@ڟ�@�bN@�A�@��@�G�@�b@ӥ�@�|�@��y@Ѳ-@ϝ�@�p�@̓u@ˮ@�@�n�@�@�/@�1'@�K�@��@ř�@�G�@�j@�dZ@°!@�$�@��7@�X@��@��@�ƨ@���@�"�@�v�@�V@���@��@���@�`B@���@��9@�M�@��w@�r�@�b@�;d@�|�@�1'@���@��P@�p�@��@��@��T@��/@�@��@�1@�"�@��@�@��\@�{@�`B@��@��`@��@�A�@�ƨ@��@�\)@�@���@���@��+@��#@��^@�/@�/@��@�&�@��9@��@��u@���@�/@�x�@�X@��/@��@�I�@���@��
@�|�@�K�@���@�M�@�5?@�@�@�O�@�%@���@���@��D@�1'@�b@��m@��P@�t�@�o@��!@�$�@�J@���@�7L@��j@��`@�j@��
@��P@�t�@�;d@���@���@��^@��7@�&�@�Ĝ@��u@�Q�@�  @�ƨ@���@�dZ@�K�@�K�@�;d@�dZ@�S�@��y@���@�V@�J@��7@��@��/@���@��u@��D@�r�@�bN@�Q�@�1'@���@��m@��w@���@�t�@��@��@�@��R@�v�@�M�@�{@���@�7L@��@���@��u@�z�@�bN@�A�@�1'@� �@�  @���@��
@���@��@��;@���@��7@��9@�33@�o@�C�@�o@�@���@�v�@�E�@�E�@�{@���@��-@�x�@�G�@�&�@��@��@���@��@���@�bN@�I�@�1'@�b@�  @���@��
@��w@��@�t�@�dZ@�S�@�S�@�"�@��D@xr�@p��@f��@]�h@Vȴ@O�@K@D��@?
=@9%@3t�@+��@(A�@#@@X@�/@X@�T11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�;B�NB�NB�HB�B��B��B��BB	7B
=B	7B1B%BBBBBB%B1BDBJBVBhBhB
=B�NB��B��B�#B��B+BDBPBPBhB{B�B&�BP�BdZBaHB^5B\)B[#BW
BM�BW
B_;BW
BW
B]/BgmBl�Bo�Bw�Br�Bn�Bp�Bs�BjB[#BP�BE�B>wB33B;dBC�B;dB0!B(�B�B{BB��B
=B��B�BB��B��B�B��B�B��B�;BƨB��BiyB9XBhB��B�#B�?B�PBq�BO�B49B33B>wBB�B2-B�BoBB
��B
�B
�#B
ŢB
�}B
��B
��B
v�B
_;B
K�B
>wB
+B
�B
DB	��B	�B	�)B	��B	�}B	��B	��B	�DB	q�B	W
B	C�B	7LB	%�B	�B	DB	
=B	'�B	33B	�B��B�B�fB�BB�B��BB�wB�3B�'B�B�B�B��B�JB�%B�+B�7B�+B�PB�PB�1B�JB�+B� Bx�Bv�B�B�%B� Bs�Bk�BffBbNBaHB_;BVBO�BL�BJ�BH�BF�BF�BD�BE�BG�BI�BJ�BJ�BJ�BK�BK�BJ�BJ�BL�BM�BN�BN�BN�BN�BL�BK�BI�BJ�BK�BJ�BI�BI�BI�BI�BH�BG�BG�BH�BH�BF�BF�BC�BC�BB�B@�BA�BB�BC�B@�BB�BB�B>wB?}BD�BE�BE�BG�BP�BQ�BS�BT�BT�BS�BR�BO�BK�BI�BH�BD�B@�B=qB9XB8RBE�BO�BL�B[#BffBdZBbNB]/BgmBiyBhsBe`BbNB^5B\)BYBT�BQ�BXBVBP�BR�BQ�BO�BP�BR�BR�BS�BR�BR�BS�BS�BVBVBW
BW
B[#B]/B^5B_;B_;B`BBbNBdZBgmBgmBk�Bl�Bn�Bt�Bu�Bu�Bv�By�B}�B~�B~�B�B�B�B�DB�+B�B|�B�B�B�7B�hB�bB�B�B�7B�DB�7B�B|�Bz�Bz�B}�B�B�%B�DB�PB�\B�bB�hB�oB�{B��B��B��B��B��B��B�B�!B�?B�RB�^B��BB��BBȴB��B��B�B�B�B�B�
B�B�B�)B�;B�TB�mB�sB�yB�B�B�B��B��B��B��B��B	B	1B	JB	VB	\B	bB	uB	�B	�B	�B	�B	�B	�B	$�B	&�B	(�B	+B	,B	0!B	33B	7LB	;dB	<jB	>wB	@�B	B�B	C�B	D�B	G�B	I�B	K�B	O�B	T�B	XB	\)B	_;B	dZB	ffB	l�B	o�B	q�B	s�B	w�B	z�B	}�B	�B	�B	�+B	�1B	�=B	�JB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�^B	��B	�qB	�RB	�XB	�jB	�dB	�jB	�qB	�wB	��B	B	ÖB	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�;B	��B
B
bB
�B
%�B
-B
49B
:^B
@�B
E�B
J�B
O�B
T�B
YB
_;B
dZB
iyB
m�B
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�&B�&B�&B�EB�[B�[B�SB�B��B��B��BB	?B
EB	BB>B.B(BBBB*B-B;BNBUBdBqBsB
IB�[B�B��B�-B��B2BQBZBYBsB�B�B&�BP�BdgBaYB^CB\7B[/BWBM�BWB_IBWBWB]>Bg{Bl�Bo�Bw�Br�Bn�Bp�Bs�Bj�B[,BP�BE�B>�B3@B;qBC�B;pB0.B)B�B�B-B��B
JB��B�MB��B��B�B��B�B��B�EBƱB��Bi�B9aBsB��B�-B�IB�YBq�BO�B4CB3=B>�BB�B27B�ByBB
��B
�B
�-B
ŭB
��B
�
B
��B
v�B
_JB
K�B
>�B
+B
�B
VB	�B	�B	�<B	��B	��B	��B	��B	�\B	q�B	W"B	C�B	7cB	%�B	�B	_B	
WB	(B	3LB	�B��B�B�B�`B�:B��B®B��B�RB�FB�5B�6B�(B��B�kB�HB�LB�YB�OB�pB�qB�QB�jB�PB�"Bx�Bv�B�@B�HB�!Bs�Bk�Bf�BbqBalB_`BV(BPBL�BJ�BH�BF�BF�BD�BE�BG�BI�BJ�BJ�BJ�BK�BK�BJ�BJ�BL�BM�BN�BN�BN�BN�BL�BK�BI�BJ�BK�BJ�BI�BI�BI�BI�BH�BG�BG�BH�BH�BF�BF�BC�BC�BB�B@�BA�BB�BC�B@�BB�BB�B>�B?�BD�BE�BE�BG�BQBRBTBU"BUBTBSBPBK�BI�BH�BD�B@�B=�B9|B8xBE�BPBL�B[GBf�BdzBbpB]PBg�Bi�Bh�Be�BbrB^WB\KBY:BU BRBX5BV'BQBSBRBPBQ
BSBSBTBSBSBTBTBV)BV'BW-BW-B[IB]PB^ZB_^B_^B`gBbnBd{Bg�Bg�Bk�Bl�Bn�Bt�Bu�Bu�Bv�By�B~BBB�&B�,B�4B�fB�KB�.B}B�(B�/B�XB��B��B�:B�(B�ZB�gB�YB�?B}B{B{B~B�(B�EB�gB�pB�|B��B��B��B��B��B��B��B��B��B�B�"B�AB�^B�oB��B��B­B��B«B��B��B�B�1B�2B�.B�.B�(B�-B�;B�GB�VB�pB�B�B�B�B�B��B��B��B��B��B�B	.B	MB	bB	rB	vB	}B	�B	�B	�B	�B	�B	�B	�B	$�B	' B	)B	+B	,#B	0<B	3MB	7fB	;}B	<�B	>�B	@�B	B�B	C�B	D�B	G�B	I�B	K�B	O�B	UB	X(B	\BB	_UB	dsB	fB	l�B	o�B	q�B	s�B	w�B	z�B	~B	�B	�'B	�CB	�HB	�UB	�aB	�sB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�"B	�+B	�1B	�5B	�CB	�LB	�RB	�qB	��B	��B	�fB	�nB	�~B	�zB	��B	��B	��B	��B	¦B	êB	ĳB	ŸB	ƼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�OB	��B
!B
vB
�B
%�B
-"B
4LB
:pB
@�B
E�B
J�B
O�B
UB
Y(B
_LB
djB
i�B
m�B
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311448592016053114485920160531144859  AO  ARCAADJP                                                                    20140721230641    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230641  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230641  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531144859  IP                  G�O�G�O�G�O�                