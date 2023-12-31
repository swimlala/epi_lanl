CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-10-30T22:01:07Z AOML 3.0 creation; 2016-05-31T19:14:40Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20141030220107  20160531121440  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ]A   AO  4051_7090_093                   2C  D   APEX                            5368                            041511                          846 @���W�1   @��=��@4�\(��dn��O�;1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ]A   A   A   @���@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B:  B?��BG��BO��BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�fD��D�FfD���D��fD�fD�FfD��3D���D��fD�FfD��3D��3D�3D�L�Dډ�D�ٚD���D�0 D�s3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�fg@���@���A��A>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B9��B?34BG34BO34BW��B_��Bg��Bo34Bw��B��B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DI� DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dy� D�	�D�C3D��gD��3D�3D�C3D�� D�ٚD��3D�C3D�� D�� D� D�I�DچgD��gD���D�,�D�p D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1'A�9XA�;dA�7LA�=qA�?}A�7LA�9XA�;dA�=qA�E�A�Q�A�\)A�bNA�t�A㛦A㛦A㗍A�hA�+A�~�A�l�A�G�A�^Aݰ!AؾwA�+A�VA��HA��Aϣ�A�oA��HAǛ�A���A�r�A��A���A�VA��A�$�A���A��A���A���A��A��#A�\)A��A���A��7A�t�A��`A�ȴA���A��A��A��jA��A��DA�?}A�n�A��wA��A��A��A�VA�VA��A�S�A���A�ĜA��A���A��A��yA�l�A�%A�K�A��;A���A�S�A���A�-A��^A�"�A�G�A�A�"�A���A��A�`BA��
A�/A��mA��!A�jA���A�ZA�v�A�JA��jA���A�r�A�+A/A}�A{p�Az�`Az�AxI�At1ArJAp  An��Am�Alr�AkXAioAf�uAd�!Ad  Ac�Ab�HAa��A_�
A]�PA[VAY&�AX-AVZAS�#ARbNAQ;dAP�AN��AN1'AM�#AM7LAK�AJ�yAI+AF�AEdZAB��AAA@v�A?p�A?
=A>��A>�A=7LA;�7A:1'A8ZA5�A3��A3A2(�A0ȴA/�A/&�A.^5A-�7A,�9A*��A'%A$��A$v�A#p�A"�A!&�A ��AAƨA^5AG�A�!A-A�wAXA�`A�+AM�A
=AI�A{A  A��A�A�uA  A�A�RA(�Ax�Az�AƨAVAbNA��A��AA
E�A��A�hA�uAbA
=A��A�mA �A �@��y@���@��@��
@��@���@��m@띲@�t�@�
=@�v�@���@�
=@�{@�G�@���@�X@߮@���@���@�/@�b@��@�j@�t�@�o@��@ְ!@֏\@�{@�33@Гu@��@�33@���@�V@ͩ�@͑h@͑h@�`B@��@�Ĝ@� �@���@�;d@�=q@�Ĝ@���@��@��@���@Ý�@�dZ@�C�@�+@�@���@��@���@���@��P@���@�@��@�Ĝ@�z�@�9X@��F@�o@�=q@��@�@���@�/@��`@��j@��j@���@��@�C�@�@��y@��@�$�@���@�hs@�/@��@�%@���@��/@��9@��D@�I�@���@�
=@��@�o@�@�+@���@��@��-@�j@��
@���@�C�@�^5@��@���@��@���@��u@�I�@��@��@���@��w@��w@��w@��@�|�@�C�@�+@���@�E�@��@��h@�G�@���@��D@�Q�@��@��@���@�=q@���@��#@��-@�p�@��@��@�A�@���@��w@�dZ@�"�@�o@���@�@��^@�hs@�7L@��@���@�Ĝ@�z�@���@��@�t�@�\)@�+@���@�@���@�7L@���@���@��j@��u@�j@�9X@��@��w@��@�|�@�dZ@���@���@���@�|�@��@���@�;d@��@���@�@��\@�M�@��h@���@�A�@���@���@�|�@�;d@��@���@�^5@���@�?}@�/@�&�@�%@���@�bN@�1@���@���@��P@��@��P@�S�@�C�@�;d@�;d@�;d@�33@���@���@��R@���@��\@�E�@��@�@��#@���@�hs@�?}@�?}@�7L@�&�@���@�Ĝ@��@��u@�z�@�Z@�I�@�Q�@�Z@��@��m@��@��P@�\)@��@�M�@�5?@�@��-@�/@��9@�b@�  @��
@��w@��w@���@��@�t�@�S�@�"�@��!@�~�@�^5@�@���@���@���@�`B@�O�@��@�%@��P@�E�@}��@q��@i�@cC�@Z�H@R�!@Ko@C33@;�@49X@/��@*~�@%O�@��@7L@ff@@\)@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�1'A�9XA�;dA�7LA�=qA�?}A�7LA�9XA�;dA�=qA�E�A�Q�A�\)A�bNA�t�A㛦A㛦A㗍A�hA�+A�~�A�l�A�G�A�^Aݰ!AؾwA�+A�VA��HA��Aϣ�A�oA��HAǛ�A���A�r�A��A���A�VA��A�$�A���A��A���A���A��A��#A�\)A��A���A��7A�t�A��`A�ȴA���A��A��A��jA��A��DA�?}A�n�A��wA��A��A��A�VA�VA��A�S�A���A�ĜA��A���A��A��yA�l�A�%A�K�A��;A���A�S�A���A�-A��^A�"�A�G�A�A�"�A���A��A�`BA��
A�/A��mA��!A�jA���A�ZA�v�A�JA��jA���A�r�A�+A/A}�A{p�Az�`Az�AxI�At1ArJAp  An��Am�Alr�AkXAioAf�uAd�!Ad  Ac�Ab�HAa��A_�
A]�PA[VAY&�AX-AVZAS�#ARbNAQ;dAP�AN��AN1'AM�#AM7LAK�AJ�yAI+AF�AEdZAB��AAA@v�A?p�A?
=A>��A>�A=7LA;�7A:1'A8ZA5�A3��A3A2(�A0ȴA/�A/&�A.^5A-�7A,�9A*��A'%A$��A$v�A#p�A"�A!&�A ��AAƨA^5AG�A�!A-A�wAXA�`A�+AM�A
=AI�A{A  A��A�A�uA  A�A�RA(�Ax�Az�AƨAVAbNA��A��AA
E�A��A�hA�uAbA
=A��A�mA �A �@��y@���@��@��
@��@���@��m@띲@�t�@�
=@�v�@���@�
=@�{@�G�@���@�X@߮@���@���@�/@�b@��@�j@�t�@�o@��@ְ!@֏\@�{@�33@Гu@��@�33@���@�V@ͩ�@͑h@͑h@�`B@��@�Ĝ@� �@���@�;d@�=q@�Ĝ@���@��@��@���@Ý�@�dZ@�C�@�+@�@���@��@���@���@��P@���@�@��@�Ĝ@�z�@�9X@��F@�o@�=q@��@�@���@�/@��`@��j@��j@���@��@�C�@�@��y@��@�$�@���@�hs@�/@��@�%@���@��/@��9@��D@�I�@���@�
=@��@�o@�@�+@���@��@��-@�j@��
@���@�C�@�^5@��@���@��@���@��u@�I�@��@��@���@��w@��w@��w@��@�|�@�C�@�+@���@�E�@��@��h@�G�@���@��D@�Q�@��@��@���@�=q@���@��#@��-@�p�@��@��@�A�@���@��w@�dZ@�"�@�o@���@�@��^@�hs@�7L@��@���@�Ĝ@�z�@���@��@�t�@�\)@�+@���@�@���@�7L@���@���@��j@��u@�j@�9X@��@��w@��@�|�@�dZ@���@���@���@�|�@��@���@�;d@��@���@�@��\@�M�@��h@���@�A�@���@���@�|�@�;d@��@���@�^5@���@�?}@�/@�&�@�%@���@�bN@�1@���@���@��P@��@��P@�S�@�C�@�;d@�;d@�;d@�33@���@���@��R@���@��\@�E�@��@�@��#@���@�hs@�?}@�?}@�7L@�&�@���@�Ĝ@��@��u@�z�@�Z@�I�@�Q�@�Z@��@��m@��@��P@�\)@��@�M�@�5?@�@��-@�/@��9@�b@�  @��
@��w@��w@���@��@�t�@�S�@�"�@��!@�~�@�^5@�@���@���@���@�`B@�O�@��@�%@��P@�E�@}��@q��@i�@cC�@Z�H@R�!@Ko@C33@;�@49X@/��@*~�@%O�@��@7L@ff@@\)@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B
=B{B�BD�Bx�B{�B{�Bz�Bx�Bv�Bt�Bq�Bv�B��B�B�B�B�B��B��B��B��B��BƨB�XB�'B��B��BhsBe`B^5B\)B]/B^5B]/B\)BYBN�BD�B9XB:^B=qB?}BN�BQ�BN�BJ�BF�B49B#�B�B\B��B�BƨB�jBƨBƨB�XB�B��B�Bw�Bl�B_;BS�BJ�B=qB5?B/B'�B�BB�yB��BB�jB�B��B�DBjBI�B9XB!�B
�B
�;B
��B
ɺB
�dB
��B
�B
u�B
gmB
bNB
YB
K�B
@�B
;dB
5?B
%�B
bB
%B	��B	�B	�`B	�B	��B	�jB	��B	��B	��B	��B	�bB	�7B	�B	}�B	s�B	p�B	l�B	bNB	VB	M�B	G�B	C�B	?}B	<jB	:^B	7LB	0!B	&�B	�B	VB	%B��B��B�B�B�B�B�yB�fB�BB�#B��B��BȴBŢB��B�qB�^B�RB�FB�3B�B��B��B��B��B��B��B��B�{B�hB�VB�JB�=B�=B�7B�1B�+B�%B�B�%B�7B�DB�DB�DB�=B�7B�1B�+B�%B�B�B�B�B�B~�B|�B{�B~�B{�Bv�Br�Bp�Bq�Bo�Bn�Bn�Bn�Bn�Bm�Bl�Bk�Bl�BjBhsBu�Bw�By�B|�B|�B}�B�B�%B�+B�1B�7B�JB�PB�VB�VB�\B�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�-B�?B�FB�LB�^B�qBBǮB��B��B��B�B�B�B�B�/B�5B�BB�HB�NB�ZB�sB�B�B�B�B�B��B��B��B��B��B	B	B	B	B	B	
=B	DB	PB	PB	PB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	(�B	,B	0!B	5?B	5?B	7LB	:^B	?}B	@�B	@�B	A�B	E�B	H�B	I�B	L�B	N�B	O�B	R�B	T�B	VB	XB	YB	YB	YB	[#B	^5B	`BB	_;B	_;B	`BB	`BB	aHB	bNB	ffB	hsB	jB	l�B	o�B	u�B	w�B	z�B	{�B	|�B	~�B	�B	�B	�1B	�=B	�JB	�VB	�\B	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�9B	�?B	�?B	�FB	�LB	�RB	�^B	�dB	�jB	�wB	B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�5B	�BB	�BB	�BB	�BB	�NB	�TB	�`B	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
PB
PB
VB
bB
bB
hB
oB
{B
�B
 �B
�B
%�B
2-B
7LB
<jB
A�B
G�B
M�B
T�B
ZB
^5B
cTB
gmB
l�B
o�B
r�B
u�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B�B
IB�B�BD�Bx�B{�B{�Bz�Bx�Bv�Bt�Bq�Bv�B�B�B�#B�/B�&B�B��B��B�	B��BƷB�iB�:B�B��Bh�BejB^DB\5B]:B^BB]>B\7BY$BN�BD�B9gB:jB=|B?�BN�BQ�BN�BJ�BF�B4DB#�B�BfB��B�BƵB�vBƴBƳB�dB�B��B�Bw�Bl�B_HBTBJ�B=~B5FB/(B'�B�B"B�B��BB�uB�B��B�OBj�BI�B9aB!�B
��B
�HB
��B
��B
�qB
��B
�.B
u�B
g~B
b]B
Y%B
K�B
@�B
;sB
5OB
%�B
tB
7B	��B	�B	�tB	�1B	��B	�}B	�B	��B	��B	��B	�zB	�NB	�6B	~
B	s�B	p�B	l�B	beB	VB	M�B	G�B	C�B	?�B	<�B	:xB	7dB	08B	'B	�B	rB	BB�B��B��B�B��B�B�B�B�]B�>B�B��B��B��B��B��B�~B�pB�eB�RB�;B�B��B��B��B��B��B��B��B��B�wB�lB�^B�]B�YB�SB�MB�GB�@B�GB�XB�fB�fB�gB�\B�ZB�TB�NB�FB�@B�5B�,B�'B�)BB}B|BB|	Bv�Br�Bp�Bq�Bo�Bn�Bn�Bn�Bn�Bm�Bl�Bk�Bl�Bj�Bh�Bu�Bw�Bz B}B}B~B�)B�FB�MB�PB�YB�kB�pB�uB�uB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�/B�4B�?B�FB�MB�_B�cB�lB�~B��B¯B��B��B�B�B�"B�#B�!B�/B�MB�RB�`B�eB�lB�xB�B�B�B�B�B��B��B��B��B�B�B	%B	)B	/B	/B	.B	
YB	_B	jB	lB	jB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	)B	,!B	0=B	5[B	5WB	7dB	:xB	?�B	@�B	@�B	A�B	E�B	H�B	I�B	L�B	N�B	O�B	SB	UB	VB	X)B	Y2B	Y2B	Y0B	[<B	^MB	`[B	_SB	_SB	`YB	`ZB	a^B	bfB	fB	h�B	j�B	l�B	o�B	u�B	w�B	z�B	{�B	}B	B	�3B	�8B	�HB	�UB	�bB	�kB	�sB	�qB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�+B	�7B	�KB	�OB	�TB	�TB	�ZB	�bB	�hB	�sB	�|B	��B	��B	¥B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�+B	�0B	�KB	�UB	�VB	�VB	�YB	�bB	�iB	�uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B
 B
B
B
B
B
 B
 B
 B
'B
%B
%B
$B
.B
9B
>B
@B
DB
bB
bB
kB
vB
vB
|B
�B
�B
�B
 �B
�B
%�B
2?B
7]B
<|B
A�B
G�B
M�B
UB
Z-B
^GB
cfB
g}B
l�B
o�B
r�B
u�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214402016053112144020160531121440  AO  ARCAADJP                                                                    20141030220107    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141030220107  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141030220107  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121440  IP                  G�O�G�O�G�O�                