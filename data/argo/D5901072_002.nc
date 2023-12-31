CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:40Z UW 3.1 conversion   
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143505  20190522121828  1728_5048_002                   2C  D   APEX                            2142                            040306                          846 @�3�+�1   @�3�?�@3C�
=p��c��x���1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@���A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$� D%  D%� D&  D&� D'  D'� D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_fD_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Dfy�Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw` Dy�fD�� D�I�D�� D���D���D�I�D�|�D��fD���D�0 D���DǠ D���D�6fD�ffD�Y�D��3D��D�VfD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@���@���AffA>ffA^ffA~ffA�33A�33A�  A�33A�33A�33A�33A�33B��B��B��B��B(  B/��B7��B?��BG��BO��BW��B_��Bg��Bp  Bw��B��B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC  C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk��Cm�fCo�fCr  Cs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
�3Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D$  D$y�D$��D%y�D%��D&y�D&��D'y�D'�3D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D=  D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D_  D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfs3Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��DwY�Dy� D���D�FfD�|�D��fD�ٚD�FfD�y�D��3D��D�,�D���Dǜ�D���D�33D�c3D�VfD�� D��D�S3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aԩ�Aԟ�Aԛ�Aԛ�Aԙ�Aԉ7A�|�A�G�A��A���AӶFAӕ�A���A�v�A�$�A���Aщ7A�\)A�=qA� �A�VA�A��A��;A���Aв-AС�AЇ+A�t�A�t�A�v�A�t�A�hsA�?}A��A�K�A���A� �AɍPA�M�A�G�A�A�A��A�?}Aú^AÝ�A�1'A�S�A�t�A�t�A��\A���A���A�z�A���A�I�A���A��A�ƨA��TA��A�~�A�(�A��A�+A��uA�/A���A��PA���A��A�A��A�A�A��A��A��mA���A�JA�/A�$�A�I�A�ffA�dZA���A�%A�\)A�&�A��DA��A��A�;dA�`BA��A�"�A�+A���A�I�A��+A��hA�;dA��A�=qA�ĜA�K�A��jA�ffA�t�A��wA��A���A�O�A���A���A�t�A��^A��-A�t�A�{A��
A�jA�hsA��\A�TA}�^A|��A|E�A{?}Ay�Ax�+Av-Ao�AjM�Ah��Ae�7A\~�AT�ANE�AJ�HAJ �AH��AF=qACK�AB��AB=qA@z�A=�mA:�uA8��A7�A7p�A4��A1�A0�A/&�A+�;A)7LA(��A(bNA'�A&Q�A%%A"��A!VAAjA;dAA�HA�TA�A�^A��A7LA��A��A`BA�A�
A�/A&�AQ�AS�A��Av�A�A�hA
�jA	�;A�/A�A�FAC�A�A{AG�A�/A�A�A�A��A
=A �A ��A Q�@�33@��
@���@�I�@�9X@� �@���@���@���@�  @�P@�"�@�^5@�hs@�@� �@��@�h@�9@��T@��/@�n�@�bN@�Z@�z�@��`@�~�@��y@��T@�D@��@�?}@��;@⟾@�E�@�  @ޗ�@�`B@�V@�z�@��m@ۥ�@�dZ@�{@أ�@��m@�K�@�
=@���@ְ!@և+@��@�x�@�/@�%@��`@ԣ�@�1@�+@�`B@�r�@Ѓ@�`B@�G�@�J@��@��m@�|�@�K�@�33@���@ְ!@�n�@�E�@�5?@�{@���@ՙ�@�p�@�G�@�/@�V@Դ9@���@�33@��H@�^5@��@с@�V@���@Л�@�j@�Q�@϶F@�dZ@�1'@�Z@�@�`B@��#@���@��@�33@�K�@�
=@��H@�ff@�J@�@Ų-@���@�33@��@�@���@��@�5?@�ff@���@�o@�V@�@�p�@��F@�n�@��7@��H@���@�S�@�^5@��@���@�Z@��P@�$�@�Ĝ@� �@�dZ@�+@��H@�=q@��u@��w@��@�E�@��#@�x�@�O�@�&�@��/@�Z@��;@�
=@�^5@���@�@���@���@��h@��7@��7@��@�X@��@�\)@�ȴ@���@�n�@�{@��#@�p�@�&�@��@�9X@�1@���@�C�@�$�@��@���@�j@�Q�@�A�@�(�@��@�b@�1@�  @��m@���@�dZ@��@��@��@�ȴ@���@��!@��+@�5?@�{@��@���@�@��^@��^@�x�@�%@��9@�j@�I�@� �@��;@��F@���@���@��P@�t�@�\)@�33@���@���@��@���@���@��^@�`B@�%@��/@��u@�Q�@���@���@�\)@��y@�ȴ@��R@��+@�-@���@���@���@��h@��@�hs@�?}@��@��j@�z�@�j@�Q�@�(�@�1@��@�;d@�ȴ@�~�@�-@��#@���@��7@�x�@�`B@�7L@���@��@�Q�@�9X@�b@��
@��F@���@�\)@��@���@�E�@��#@��h@�hs@�?}@��@�V@��/@���@���@���@���@��@��D@�o@�$�@��@zM�@r��@i�@_;d@Y7L@Qx�@K��@C�@:J@3�m@+@$��@ 1'@��@=q@{@-@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aԩ�Aԟ�Aԛ�Aԛ�Aԙ�Aԉ7A�|�A�G�A��A���AӶFAӕ�A���A�v�A�$�A���Aщ7A�\)A�=qA� �A�VA�A��A��;A���Aв-AС�AЇ+A�t�A�t�A�v�A�t�A�hsA�?}A��A�K�A���A� �AɍPA�M�A�G�A�A�A��A�?}Aú^AÝ�A�1'A�S�A�t�A�t�A��\A���A���A�z�A���A�I�A���A��A�ƨA��TA��A�~�A�(�A��A�+A��uA�/A���A��PA���A��A�A��A�A�A��A��A��mA���A�JA�/A�$�A�I�A�ffA�dZA���A�%A�\)A�&�A��DA��A��A�;dA�`BA��A�"�A�+A���A�I�A��+A��hA�;dA��A�=qA�ĜA�K�A��jA�ffA�t�A��wA��A���A�O�A���A���A�t�A��^A��-A�t�A�{A��
A�jA�hsA��\A�TA}�^A|��A|E�A{?}Ay�Ax�+Av-Ao�AjM�Ah��Ae�7A\~�AT�ANE�AJ�HAJ �AH��AF=qACK�AB��AB=qA@z�A=�mA:�uA8��A7�A7p�A4��A1�A0�A/&�A+�;A)7LA(��A(bNA'�A&Q�A%%A"��A!VAAjA;dAA�HA�TA�A�^A��A7LA��A��A`BA�A�
A�/A&�AQ�AS�A��Av�A�A�hA
�jA	�;A�/A�A�FAC�A�A{AG�A�/A�A�A�A��A
=A �A ��A Q�@�33@��
@���@�I�@�9X@� �@���@���@���@�  @�P@�"�@�^5@�hs@�@� �@��@�h@�9@��T@��/@�n�@�bN@�Z@�z�@��`@�~�@��y@��T@�D@��@�?}@��;@⟾@�E�@�  @ޗ�@�`B@�V@�z�@��m@ۥ�@�dZ@�{@أ�@��m@�K�@�
=@���@ְ!@և+@��@�x�@�/@�%@��`@ԣ�@�1@�+@�`B@�r�@Ѓ@�`B@�G�@�J@��@��m@�|�@�K�@�33@���@ְ!@�n�@�E�@�5?@�{@���@ՙ�@�p�@�G�@�/@�V@Դ9@���@�33@��H@�^5@��@с@�V@���@Л�@�j@�Q�@϶F@�dZ@�1'@�Z@�@�`B@��#@���@��@�33@�K�@�
=@��H@�ff@�J@�@Ų-@���@�33@��@�@���@��@�5?@�ff@���@�o@�V@�@�p�@��F@�n�@��7@��H@���@�S�@�^5@��@���@�Z@��P@�$�@�Ĝ@� �@�dZ@�+@��H@�=q@��u@��w@��@�E�@��#@�x�@�O�@�&�@��/@�Z@��;@�
=@�^5@���@�@���@���@��h@��7@��7@��@�X@��@�\)@�ȴ@���@�n�@�{@��#@�p�@�&�@��@�9X@�1@���@�C�@�$�@��@���@�j@�Q�@�A�@�(�@��@�b@�1@�  @��m@���@�dZ@��@��@��@�ȴ@���@��!@��+@�5?@�{@��@���@�@��^@��^@�x�@�%@��9@�j@�I�@� �@��;@��F@���@���@��P@�t�@�\)@�33@���@���@��@���@���@��^@�`B@�%@��/@��u@�Q�@���@���@�\)@��y@�ȴ@��R@��+@�-@���@���@���@��h@��@�hs@�?}@��@��j@�z�@�j@�Q�@�(�@�1@��@�;d@�ȴ@�~�@�-@��#@���@��7@�x�@�`B@�7L@���@��@�Q�@�9X@�b@��
@��F@���@�\)@��@���@�E�@��#@��h@�hs@�?}@��@�V@��/@���@���@���@���@��@��D@�o@�$�@��@zM�@r��@i�@_;d@Y7L@Qx�@K��@C�@:J@3�m@+@$��@ 1'@��@=q@{@-@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�!B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�hB
�B
~�B
|�B
�7B
�bB
��B
�-B
ǮBG�BaHB�PB��BB'�B!�B�B�B#�B$�BQ�B�hB��B��B�LB�LB�?B�wB��B�sB��B��B�BŢB�3B�B��B�DB{�B�B�oB��BÖB��B�9B��BN�B-B �B.B6FB8RB6FB�B�`B�qBɺB�jB�3B�B�B��B�hBy�Bp�BhsBaHBW
BK�B5?B-B�BB
�B
�yB
�5B
��B
ɺB
�^B
�B
��B
�B
n�B
^5B
XB
F�B
>wB
@�B
6FB
49B
/B
,B
#�B

=B	��B	��B	ŢB	�XB	>wB�B��B��B�jB�9B��B��B��B��B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�oB�{B�\B�oB��B�uB��B��B��B�hB�JB��B�=B�{B�1B�JB��B��B��B�bB��B�hB�oB�bB�hB�hB�oB�{B�VB�JB�DB�DB�DB�=B�=B�VB��B�uB��B�{B�{B�{B�{B�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�3B�^B�wB�3B�B��B��BĜB��B��B��B�B�fB��B��B	B�B�B��B	B��B	B��B��B	B	B	B	B	B	
=B	{B	oB	uB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	)�B	-B	8RB	W
B	dZB	gmB	hsB	hsB	hsB	iyB	k�B	l�B	m�B	n�B	o�B	p�B	q�B	s�B	u�B	u�B	u�B	w�B	w�B	w�B	x�B	u�B	s�B	q�B	p�B	o�B	o�B	n�B	r�B	p�B	e`B	iyB	\)B	VB	\)B	gmB	l�B	n�B	t�B	{�B	{�B	|�B	~�B	�B	�B	~�B	x�B	u�B	v�B	z�B	�%B	�=B	�oB	�{B	�hB	�DB	�7B	�+B	�B	� B	|�B	�7B	�hB	�uB	�{B	�bB	�\B	�hB	�{B	�oB	�VB	�oB	�oB	�hB	�bB	�{B	�bB	�\B	�uB	�uB	�oB	�uB	�uB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�XB	�9B	�XB	�LB	�^B	�RB	�RB	�XB	�^B	�dB	�dB	�jB	�jB	�qB	�wB	�}B	��B	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�/B	�;B	�BB	�HB	�NB	�TB	�TB	�ZB	�ZB	�fB	�`B	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
  B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
1B
	7B
oB
�B
�B
%�B
/B
33B
:^B
?}B
F�B
O�B
T�B
]/B
bNB
ffB
hsB
m�B
q�B
t�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�!B
�3B
�!B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
�1B
�B
� B
�=B
�oB
��B
�-B
��BH�BdZB�{B��B+B.B(�B �B"�B%�B"�BN�B�hB��B��B�XB�RB�RB��B��B�B��B��B�BȴB�?B�-B��B�PB~�B�B�hB��BȴBŢB�XB��BVB2-B#�B/B9XB;dB;dB)�B�B�}B��B��B�^B�B�B��B��B|�Br�Bk�BdZBZBR�B;dB2-B$�B	7B
��B
�B
�TB
��B
��B
B
�9B
��B
�JB
w�B
dZB
]/B
J�B
C�B
C�B
7LB
7LB
2-B
/B
)�B
�B

=B	��B	��B	��B	T�B��B�BÖB��B�qB�3B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B�uB��B��B��B��B��B��B�{B�\B��B�PB��B�7B�\B��B��B��B�uB��B�uB�uB�oB�{B�{B��B��B�\B�VB�JB�PB�VB�DB�DB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�9B�jB��B�9B��BB�BǮB��B��B��B��B�fB��B	B	B��B��B��B	B��B	%B	B��B	B	B	B	B	B	JB	�B	uB	{B	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	)�B	+B	0!B	VB	e`B	gmB	hsB	iyB	iyB	iyB	k�B	l�B	m�B	o�B	p�B	p�B	q�B	s�B	u�B	v�B	w�B	x�B	x�B	x�B	x�B	v�B	t�B	q�B	p�B	o�B	o�B	o�B	s�B	t�B	jB	l�B	]/B	T�B	[#B	gmB	l�B	n�B	u�B	{�B	|�B	}�B	� B	�B	�B	�B	z�B	v�B	v�B	z�B	�%B	�=B	�uB	��B	�oB	�DB	�=B	�=B	�+B	�B	z�B	�1B	�oB	�uB	��B	�hB	�bB	�uB	��B	�{B	�\B	�uB	�uB	�oB	�hB	��B	�oB	�hB	�{B	�{B	�uB	�uB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�XB	�?B	�^B	�XB	�jB	�XB	�XB	�XB	�^B	�dB	�dB	�jB	�jB	�qB	�wB	�}B	��B	ŢB	ĜB	ĜB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�/B	�;B	�BB	�HB	�TB	�ZB	�ZB	�`B	�ZB	�fB	�fB	�mB	�fB	�sB	�sB	�sB	�yB	�yB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
1B
	7B
oB
�B
�B
%�B
/B
33B
:^B
?}B
F�B
O�B
T�B
]/B
bNB
ffB
hsB
m�B
q�B
t�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<T��<#�
<#�
<�j<�9X<�o<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451532012011014515320120110145153  AO  ARGQ                                                                        20111130143505  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143505  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145153  IP                  G�O�G�O�G�O�                