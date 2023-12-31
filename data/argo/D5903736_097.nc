CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-12-10T22:05:41Z AOML 3.0 creation; 2016-05-31T19:14:40Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20141210220541  20160531121440  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               aA   AO  4051_7090_097                   2C  D   APEX                            5368                            041511                          846 @�)�]��1   @�)�� @4�7KƧ��dl9XbN1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    aA   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�D��D�S3D�� D�� D�	�D�<�D�vfD�� D���D�FfD���D��fD�3D�FfD�|�D��fD��D�S3D� D�#3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @9��@y��@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bh  Bo��Bw��B��B���B���B�  B���B�fgB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D   D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dts4Dy4D�	�D�P D���D���D�gD�9�D�s3D���D���D�C3D��gD��3D� D�C3D�y�D��3D�gD�P D�|�D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��
A��#A��#A��A��A��A��/A��;A���A�ƨAѴ9AѰ!Aѩ�Aѝ�Aљ�Aї�Aѕ�AёhAэPAыDAч+Aч+AхA�~�A�~�A�|�A�t�A�p�A�l�A�ffA�K�A��A���A�-A���A�A���A��A���A��A���A��A��7A�z�A��A�E�A���A��A�Q�A�JA��TA�jA�t�A�l�A�bNA��/A�;dA�ZA�`BA�`BA�A��RA�M�A��A�I�A�(�A��A�%A�K�A�%A��A�v�A�1'A��A�-A��RA��/A�O�A��A�ƨA���A���A���A�\)A��yA���A�1'A��`A��!A��A�=qA�  A���A��7A���A�+A��A��/A�%A��A�+A���A�/A���A�I�A��wA�+A��FA�(�A�A|�A{O�AzQ�Ax�/Au
=As33Ap5?An~�Am��Al��AkC�Ai�mAioAhz�Ag�TAdĜAaƨA_x�A]�FA\v�AZ�9AV��AT^5AS�;ASAS�FASK�AQ�AP�9AO7LAN~�AL��AJ��AIAHjAGt�AF1'AD$�A@��A<�DA9�A8�A7?}A6�+A5�^A4=qA2�HA1
=A.��A,�/A+�A*^5A)�7A(�A'7LA%hsA$�\A#�A!��A!�A A�!A�A��AȴAZA�^A�A��A/A �A{A�AbA+A-AO�A�mAVAA%A�wA
�DA	�hA�An�Ap�A-A��A=qA�mA�An�A�FA?}A �A �@�V@�+@��-@���@��P@���@���@���@��@�@�+@�$�@�V@�  @��D@�;d@�@�O�@��@��;@�A�@��@�33@��#@�b@�P@柾@��@�b@�C�@�~�@��@��@�5?@���@�dZ@�+@�"�@�ȴ@ڏ\@�O�@���@֗�@�J@ՙ�@�z�@�"�@ёh@�A�@϶F@�dZ@��@�/@�b@�
=@ʸR@�~�@�$�@�@��@��
@�C�@���@ź^@�?}@ă@�ƨ@��
@��m@���@�"�@��H@°!@�@��@�V@�7L@��-@���@�p�@� �@�+@�
=@�C�@���@��+@��T@�&�@��@��\@�^5@�J@���@�?}@�O�@��j@���@��P@�S�@�V@�J@��-@�G�@���@���@�9X@��@���@��w@�S�@��+@�-@���@��@�?}@��@�j@�I�@�9X@��;@��w@���@�|�@��y@�5?@��@��7@�G�@�?}@�&�@��@��@�9X@��m@��@�S�@�"�@��@��P@�t�@���@���@�V@���@��@�bN@�A�@�S�@�;d@�;d@�\)@�K�@���@��+@�v�@�^5@�ff@��R@��y@��@���@�$�@��@��@��9@��m@��@���@��!@�ff@���@�?}@�%@��`@�Ĝ@�(�@��P@�;d@�
=@���@���@�ff@���@�ȴ@�~�@�5?@��@�l�@�ƨ@��!@��@��7@��@��D@�  @��
@��m@�l�@��\@�=q@��@��7@�G�@��`@��9@��j@��j@�Ĝ@��u@�z�@�j@��@��m@���@��@��+@�M�@�5?@�$�@�@��@��@���@��-@��@�?}@���@���@��u@�j@�Z@� �@�  @��
@���@�\)@�C�@�l�@�|�@�dZ@�@���@�E�@�J@��@���@�p�@��@��@�Ĝ@�Q�@�1@���@��m@�ƨ@���@�t�@�+@���@��@��\@�M�@��#@�@��-@���@�hs@�O�@��@�%@��/@���@�j@�(�@���@�dZ@��@��@��@�ȴ@��R@�Ĝ@��@x��@p�9@j��@co@[�m@S�
@L�/@D��@?�@:��@49X@.@*J@$�D@�@|�@��@�y@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��
A��#A��#A��A��A��A��/A��;A���A�ƨAѴ9AѰ!Aѩ�Aѝ�Aљ�Aї�Aѕ�AёhAэPAыDAч+Aч+AхA�~�A�~�A�|�A�t�A�p�A�l�A�ffA�K�A��A���A�-A���A�A���A��A���A��A���A��A��7A�z�A��A�E�A���A��A�Q�A�JA��TA�jA�t�A�l�A�bNA��/A�;dA�ZA�`BA�`BA�A��RA�M�A��A�I�A�(�A��A�%A�K�A�%A��A�v�A�1'A��A�-A��RA��/A�O�A��A�ƨA���A���A���A�\)A��yA���A�1'A��`A��!A��A�=qA�  A���A��7A���A�+A��A��/A�%A��A�+A���A�/A���A�I�A��wA�+A��FA�(�A�A|�A{O�AzQ�Ax�/Au
=As33Ap5?An~�Am��Al��AkC�Ai�mAioAhz�Ag�TAdĜAaƨA_x�A]�FA\v�AZ�9AV��AT^5AS�;ASAS�FASK�AQ�AP�9AO7LAN~�AL��AJ��AIAHjAGt�AF1'AD$�A@��A<�DA9�A8�A7?}A6�+A5�^A4=qA2�HA1
=A.��A,�/A+�A*^5A)�7A(�A'7LA%hsA$�\A#�A!��A!�A A�!A�A��AȴAZA�^A�A��A/A �A{A�AbA+A-AO�A�mAVAA%A�wA
�DA	�hA�An�Ap�A-A��A=qA�mA�An�A�FA?}A �A �@�V@�+@��-@���@��P@���@���@���@��@�@�+@�$�@�V@�  @��D@�;d@�@�O�@��@��;@�A�@��@�33@��#@�b@�P@柾@��@�b@�C�@�~�@��@��@�5?@���@�dZ@�+@�"�@�ȴ@ڏ\@�O�@���@֗�@�J@ՙ�@�z�@�"�@ёh@�A�@϶F@�dZ@��@�/@�b@�
=@ʸR@�~�@�$�@�@��@��
@�C�@���@ź^@�?}@ă@�ƨ@��
@��m@���@�"�@��H@°!@�@��@�V@�7L@��-@���@�p�@� �@�+@�
=@�C�@���@��+@��T@�&�@��@��\@�^5@�J@���@�?}@�O�@��j@���@��P@�S�@�V@�J@��-@�G�@���@���@�9X@��@���@��w@�S�@��+@�-@���@��@�?}@��@�j@�I�@�9X@��;@��w@���@�|�@��y@�5?@��@��7@�G�@�?}@�&�@��@��@�9X@��m@��@�S�@�"�@��@��P@�t�@���@���@�V@���@��@�bN@�A�@�S�@�;d@�;d@�\)@�K�@���@��+@�v�@�^5@�ff@��R@��y@��@���@�$�@��@��@��9@��m@��@���@��!@�ff@���@�?}@�%@��`@�Ĝ@�(�@��P@�;d@�
=@���@���@�ff@���@�ȴ@�~�@�5?@��@�l�@�ƨ@��!@��@��7@��@��D@�  @��
@��m@�l�@��\@�=q@��@��7@�G�@��`@��9@��j@��j@�Ĝ@��u@�z�@�j@��@��m@���@��@��+@�M�@�5?@�$�@�@��@��@���@��-@��@�?}@���@���@��u@�j@�Z@� �@�  @��
@���@�\)@�C�@�l�@�|�@�dZ@�@���@�E�@�J@��@���@�p�@��@��@�Ĝ@�Q�@�1@���@��m@�ƨ@���@�t�@�+@���@��@��\@�M�@��#@�@��-@���@�hs@�O�@��@�%@��/@���@�j@�(�@���@�dZ@��@��@��@�ȴ@��R@�Ĝ@��@x��@p�9@j��@co@[�m@S�
@L�/@D��@?�@:��@49X@.@*J@$�D@�@|�@��@�y@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BI�BH�BG�BF�BF�BG�BG�BG�BG�BG�BG�BH�BH�BH�BH�BH�BG�BG�BG�BF�BF�BE�BE�B@�B5?B&�B"�BA�B.B�B\BB��B�B�mB�BB�B��B��BÖB��B��BǮB�B��B��B�3B��B�B  B��B�yB�NB�)BȴB�FB�-B��B�7Bx�Bl�BffB`BBZBR�BJ�B0!B�BbB+B��B��B��B�B�fB�BB�B��B��BɺBȴBƨB��B�dB�9B��B�1Bn�B_;BM�B0!BB
�B
��B
ȴB
��B
�RB
�B
��B
�PB
�B
m�B
T�B
K�B
B�B
49B
oB
B	�B	�;B	�
B	��B	ŢB	�dB	�FB	�'B	��B	��B	�%B	z�B	p�B	iyB	^5B	J�B	>wB	<jB	;dB	:^B	7LB	0!B	,B	'�B	$�B	�B	�B	oB	hB	bB	1B��B�B�/B��B��B��BǮBĜB�}B�dB�FB�'B�B�B��B��B��B��B��B��B��B��B��B�{B�oB�hB�bB�\B�VB�JB�JB�DB�7B�1B�+B�%B�B�B�B�B~�B}�B}�B{�B{�Bz�Bz�Bz�By�Bx�Bx�Bx�Bx�By�Bz�B|�B|�B}�B|�B|�B~�B� B~�B�B�B�B�B�B�+B�%B�B�B�B�=B�uB�{B�{B��B��B��B�B�B�B�!B�3B�FB�LB�FB�9B�3B�3B�XB�XB�9B�FB�dB�qB�wB�}B��BŢBBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�)B�)B�5B�HB�HB�NB�NB�yB�B�B��B�B��B��B	  B	B	B��B	B	B	%B	%B		7B	
=B	PB	\B	hB	oB	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	"�B	"�B	#�B	&�B	+B	.B	0!B	2-B	33B	33B	33B	6FB	7LB	8RB	:^B	;dB	=qB	=qB	?}B	D�B	E�B	H�B	I�B	J�B	J�B	K�B	M�B	Q�B	S�B	XB	\)B	^5B	e`B	jB	k�B	l�B	m�B	n�B	m�B	o�B	p�B	r�B	v�B	w�B	x�B	z�B	|�B	� B	� B	�B	�1B	�7B	�\B	�hB	�{B	�uB	�{B	�{B	��B	�{B	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�9B	�?B	�LB	�^B	��B	ȴB	��B	ǮB	ĜB	ƨB	ǮB	ǮB	ǮB	ɺB	��B	��B	ɺB	ɺB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�TB	�ZB	�`B	�`B	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
	7B
1B
	7B

=B
DB
JB
PB
bB
�B
"�B
(�B
-B
2-B
7LB
<jB
A�B
G�B
K�B
O�B
VB
\)B
`BB
dZB
gmB
k�B
p�B
s�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BI�BH�BG�BF�BF�BG�BG�BG�BG�BG�BG�BH�BH�BH�BH�BH�BG�BG�BG�BF�BF�BE�BE�B@�B5MB&�B"�BA�B. B�BgB#B��B�B�yB�MB�'B�B��BÞB��B��BǷB�%B��B��B�<B��B��B B��B�B�XB�3BȾB�UB�4B��B�BBx�Bl�BfqB`IBZ$BR�BJ�B0+B�BmB4B�B��B��B�B�oB�NB�!B��B��B��BȾBƱB��B�pB�CB��B�;Bn�B_EBM�B0+BB
�B
�B
��B
��B
�aB
�"B
��B
�`B
�B
m�B
UB
K�B
B�B
4IB
~B
B	�B	�OB	�B	��B	ŶB	�yB	�ZB	�>B	�B	��B	�:B	z�B	p�B	i�B	^MB	J�B	>�B	<�B	;�B	:vB	7eB	0;B	,!B	(B	$�B	�B	�B	�B	�B	~B	NB�B�B�LB�
B��B��B��BļB��B��B�fB�FB�6B�)B�B�	B��B��B��B��B��B��B��B��B��B��B��B�B�wB�mB�iB�hB�ZB�TB�OB�HB�AB�4B�-B�+BB~B~B|B|
B{B{B{By�Bx�Bx�Bx�Bx�By�B{B}B}B~B}B}BB�#BB�.B�6B�;B�AB�@B�NB�FB�9B�5B�<B�_B��B��B��B��B��B��B�!B�2B�;B�AB�RB�dB�lB�fB�YB�RB�OB�vB�vB�XB�gB��B��B��B��B��B��B¯B��B��B��B�B��B��B��B��B��B��B�B�B�B��B��B��B��B�B�"B�.B�;B�FB�EB�TB�eB�fB�mB�mB�B��B��B��B��B��B��B	 B	%B	(B�B	%B	)B	@B	>B		RB	
UB	jB	xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	"�B	"�B	#�B	'B	+B	.0B	09B	2GB	3LB	3OB	3MB	6_B	7fB	8mB	:xB	;~B	=�B	=�B	?�B	D�B	E�B	H�B	I�B	J�B	J�B	K�B	M�B	RB	TB	X'B	\AB	^MB	exB	j�B	k�B	l�B	m�B	n�B	m�B	o�B	p�B	r�B	v�B	w�B	x�B	z�B	}B	�B	�B	�)B	�IB	�NB	�sB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�1B	�7B	�LB	�TB	�aB	�tB	��B	��B	��B	��B	ĲB	ƼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�&B	�,B	�2B	�*B	�2B	�7B	�=B	�=B	�BB	�MB	�JB	�JB	�QB	�WB	�]B	�bB	�iB	�pB	�sB	�uB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�
B	�B
 B
B
B
B
!B
'B
'B
(B
(B
,B
,B
3B
;B
7B
4B
=B
FB
	HB
GB
	KB

QB
YB
^B
cB
vB
�B
"�B
)B
-"B
2@B
7_B
<|B
A�B
G�B
K�B
O�B
VB
\:B
`UB
djB
g~B
k�B
p�B
s�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214402016053112144020160531121440  AO  ARCAADJP                                                                    20141210220541    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141210220541  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141210220541  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121440  IP                  G�O�G�O�G�O�                