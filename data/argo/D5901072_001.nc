CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:39Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143457  20190522121828  1728_5048_001                   2C  D   APEX                            2142                            040306                          846 @�3jPg?�1   @�3j����@3bI�^5�c��O�;d1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9� D:  D:� D;  D;� D;��D<� D=  D=� D>  D>� D?  D?�fD@fD@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dy��D�3D�L�D��3D�� D��3D�@ D��fD��3D���D�<�D�l�Dǣ3D�� D��DچfD�3D���D��D�L�D�Y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�B���B���B���B���B�  B�  B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCB  CC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW��CY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D�3Dy�D��D� D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D)  D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8�3D9y�D9��D:y�D:��D;y�D;�3D<y�D<��D=y�D=��D>y�D>��D?� D@  D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dy�3D�  D�I�D�� D���D�� D�<�D��3D�� D��fD�9�D�i�DǠ D���D�	�Dڃ3D� D���D��D�I�D�Vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�E�A�I�A�33A�A���A�-A�+A�=qA�bA��`AԾwA�jA�33A��A��A�~�A��/A��A��A��
Aѩ�Aћ�Aї�Aѕ�Aѕ�Aї�Aљ�Aї�Aї�AѓuAэPA�r�A�A�A�+A� �A��yAЧ�A�M�A�1A�Aʲ-Aɗ�A�E�AœuA�VA��A�x�A���A�?}A��A���A���A�XA�$�A��9A�l�A�I�A�33A���A�\)A�-A�{A�x�A�~�A� �A���A�O�A��HA�x�A�33A�v�A���A�7LA�ffA�r�A��A���A� �A�S�A��\A��A�`BA�1A�C�A��/A���A�dZA��jA��\A��\A���A�"�A���A�%A��A��TA���A�`BA�VA�33A�&�A�v�A�?}A�;dA���A�I�A��yA�t�A���A�/A��A�E�A�M�A�ZA�M�A���AzE�At�uAl�!Ai�
Ag��Af~�Ab�yA^r�A\ZAZ�AW|�AT�AR^5AP�+AOS�ANĜANbAM�AK�7AJ�AF��AE|�A@�`A>�\A>�A=��A=�A=;dA=�A<�A<��A<(�A;"�A9\)A8�jA7ƨA7
=A6bNA5l�A4jA/��A.�uA-�
A+�^A)�;A)O�A(�`A(�+A&��A$M�A"�A!A E�A��AJAA�A�A7LAjA��Az�A�PAVA|�AhsA9XA
=AM�A(�A��A&�A	�A�RA�A&�A�RA9XA�;A�A��AXAȴA=qA5?A�A  AhsAoA j@���@���@�dZ@�V@�@���@�{@��@�D@��
@�n�@�K�@�t�@��@�$�@�hs@���@�Z@��
@��@�ff@�X@�A�@�E�@��@�\)@��@�Z@ڟ�@��#@��`@�bN@ؼj@�b@ָR@�^5@���@�^5@Ցh@��#@�ƨ@��@�-@��#@١�@�G�@�z�@׾w@�
=@���@��@��@���@ӍP@�C�@�n�@�?}@��/@д9@�r�@϶F@�ff@͡�@�X@��`@�I�@��m@��H@�@�5?@�M�@�v�@ʏ\@��m@�K�@ҟ�@�K�@��@�5?@�M�@҇+@�=q@���@�/@���@Гu@���@ϝ�@�\)@�33@�"�@��@�
=@��@ΰ!@Ο�@�v�@�5?@���@�/@�r�@˝�@�+@ʟ�@�v�@��@�@�`B@���@���@�ȴ@�@��`@�V@��@� �@���@�M�@��@�J@��-@��7@�%@�Q�@���@�l�@�C�@�o@��+@�&�@��
@�t�@�"�@��R@���@��H@�;d@��h@�G�@�G�@�?}@�?}@�G�@��@��u@�9X@��@�;d@�v�@���@�O�@���@�bN@��
@�S�@�o@��!@�V@��@�hs@�?}@�Z@��w@�o@�-@�G�@��u@���@�;d@���@�=q@���@��h@�&�@�%@�p�@��h@�X@�x�@�hs@�7L@���@���@��@���@�j@��
@�l�@��H@�v�@�$�@��@��-@���@�p�@�p�@�7L@��u@�b@�C�@���@��@���@���@���@�~�@�ff@�=q@��@���@�7L@�r�@��;@���@�C�@�+@���@��H@���@�M�@�$�@�J@��@���@���@���@���@��h@�X@��@��/@���@��9@��@���@��u@�z�@�Z@�(�@�1@��@��@�t�@�;d@��y@���@�5?@��-@��h@��7@�p�@�?}@��`@���@��@�Z@�I�@�1'@���@�ƨ@�\)@�o@�v�@�$�@��@��-@��h@�hs@�O�@��@��D@�(�@���@��P@��P@��@�t�@�\)@�+@�v�@���@�?}@��j@�Q�@���@���@�C�@��@�hs@�@�~�@{C�@sS�@i�@b^5@[@PA�@H  @@1'@:��@5p�@-�-@%�T@5?@hs@�@n�@;d@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�E�A�I�A�33A�A���A�-A�+A�=qA�bA��`AԾwA�jA�33A��A��A�~�A��/A��A��A��
Aѩ�Aћ�Aї�Aѕ�Aѕ�Aї�Aљ�Aї�Aї�AѓuAэPA�r�A�A�A�+A� �A��yAЧ�A�M�A�1A�Aʲ-Aɗ�A�E�AœuA�VA��A�x�A���A�?}A��A���A���A�XA�$�A��9A�l�A�I�A�33A���A�\)A�-A�{A�x�A�~�A� �A���A�O�A��HA�x�A�33A�v�A���A�7LA�ffA�r�A��A���A� �A�S�A��\A��A�`BA�1A�C�A��/A���A�dZA��jA��\A��\A���A�"�A���A�%A��A��TA���A�`BA�VA�33A�&�A�v�A�?}A�;dA���A�I�A��yA�t�A���A�/A��A�E�A�M�A�ZA�M�A���AzE�At�uAl�!Ai�
Ag��Af~�Ab�yA^r�A\ZAZ�AW|�AT�AR^5AP�+AOS�ANĜANbAM�AK�7AJ�AF��AE|�A@�`A>�\A>�A=��A=�A=;dA=�A<�A<��A<(�A;"�A9\)A8�jA7ƨA7
=A6bNA5l�A4jA/��A.�uA-�
A+�^A)�;A)O�A(�`A(�+A&��A$M�A"�A!A E�A��AJAA�A�A7LAjA��Az�A�PAVA|�AhsA9XA
=AM�A(�A��A&�A	�A�RA�A&�A�RA9XA�;A�A��AXAȴA=qA5?A�A  AhsAoA j@���@���@�dZ@�V@�@���@�{@��@�D@��
@�n�@�K�@�t�@��@�$�@�hs@���@�Z@��
@��@�ff@�X@�A�@�E�@��@�\)@��@�Z@ڟ�@��#@��`@�bN@ؼj@�b@ָR@�^5@���@�^5@Ցh@��#@�ƨ@��@�-@��#@١�@�G�@�z�@׾w@�
=@���@��@��@���@ӍP@�C�@�n�@�?}@��/@д9@�r�@϶F@�ff@͡�@�X@��`@�I�@��m@��H@�@�5?@�M�@�v�@ʏ\@��m@�K�@ҟ�@�K�@��@�5?@�M�@҇+@�=q@���@�/@���@Гu@���@ϝ�@�\)@�33@�"�@��@�
=@��@ΰ!@Ο�@�v�@�5?@���@�/@�r�@˝�@�+@ʟ�@�v�@��@�@�`B@���@���@�ȴ@�@��`@�V@��@� �@���@�M�@��@�J@��-@��7@�%@�Q�@���@�l�@�C�@�o@��+@�&�@��
@�t�@�"�@��R@���@��H@�;d@��h@�G�@�G�@�?}@�?}@�G�@��@��u@�9X@��@�;d@�v�@���@�O�@���@�bN@��
@�S�@�o@��!@�V@��@�hs@�?}@�Z@��w@�o@�-@�G�@��u@���@�;d@���@�=q@���@��h@�&�@�%@�p�@��h@�X@�x�@�hs@�7L@���@���@��@���@�j@��
@�l�@��H@�v�@�$�@��@��-@���@�p�@�p�@�7L@��u@�b@�C�@���@��@���@���@���@�~�@�ff@�=q@��@���@�7L@�r�@��;@���@�C�@�+@���@��H@���@�M�@�$�@�J@��@���@���@���@���@��h@�X@��@��/@���@��9@��@���@��u@�z�@�Z@�(�@�1@��@��@�t�@�;d@��y@���@�5?@��-@��h@��7@�p�@�?}@��`@���@��@�Z@�I�@�1'@���@�ƨ@�\)@�o@�v�@�$�@��@��-@��h@�hs@�O�@��@��D@�(�@���@��P@��P@��@�t�@�\)@�+@�v�@���@�?}@��j@�Q�@���@���@�C�@��@�hs@�@�~�@{C�@sS�@i�@b^5@[@PA�@H  @@1'@:��@5p�@-�-@%�T@5?@hs@�@n�@;d@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
ɺB
ȴB
ƨB
ÖB
��B
��B
�}B
�qB
�}B
�}B
�}B
�}B
�wB
�wB
�}B
ÖB
ÖB
ÖB
ĜB
ŢB
ƨB
ɺB
��B
��B
��B
��B
��B
��B
��B
�
B
�B
�#B
�)B
�)B
�B
�#B
��B
ĜB
�qB
�XB
�FB
ɺB
��B
��B
�
B
�#BB0!BR�Bn�Bz�B�B�JB�9B�jBB�B�BB	7B�B(�B+B=qBG�BH�BN�BQ�BYBYBXBYB^5B^5B[#BZBYBZBVBP�BM�BH�BC�B?}B=qB9XB0!B�B\B��B�B�B�)B�XB��Bs�BVBE�B2-B	7B
�sB
�B
��B
�^B
��B
��B
��B
��B
�bB
�1B
u�B
I�B
;dB
�B	�B	�}B	�1B	x�B	l�B	VB	9XB	�B	DB	B��B�B�)B��B��BɺBɺBƨB��B�9B��B��B��B��B��B�uB�{B�uB�uB�uB�oB�bB�PB�\B�bB�oB��B��B��B�oB�DB�bB�hB�VB�hB�uB�oB�hB�uB��B�oB�hB�VB�PB�DB�+B�B�+B�=B�1B�uB�1B�%B�%B�\B��B�{B�hB�VB�VB�DB��B�{B�oB�\B�VB�\B�\B��B�uB��B�oB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BƨB��B��B��B��B��B��B��B�
B�B�;B�#B��B�dB�LB�3B�FB�B�B�B�!B�FB��B��BÖB��B�B��B�B�B		7B	PB	VB	PB	PB	VB	JB	DB	%B	B	%B	JB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	'�B	6FB	L�B	k�B	v�B	z�B	}�B	� B	�B	�B	�+B	�7B	�=B	�DB	�JB	�PB	�PB	�PB	�PB	�PB	�PB	�PB	�VB	�VB	�\B	�\B	�\B	�\B	�hB	�oB	�hB	�oB	�oB	�uB	�uB	�uB	�{B	�{B	�bB	�\B	�PB	�uB	��B	�uB	�uB	�oB	�uB	�{B	�uB	�uB	��B	��B	�uB	��B	��B	��B	��B	��B	��B	�uB	�oB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�FB	�9B	�-B	�-B	�LB	�XB	�LB	�LB	�qB	�LB	�LB	�RB	B	��B	�XB	�?B	�LB	�-B	�RB	�B	�!B	��B	��B	�B	��B	��B	�B	�?B	�9B	�RB	�dB	�dB	�wB	�}B	�}B	�wB	��B	��B	�qB	�jB	�^B	�^B	�^B	�XB	�XB	�XB	�XB	�^B	�XB	�RB	�RB	�FB	�FB	�FB	�FB	�LB	�LB	�RB	�RB	�XB	�XB	�^B	�wB	�qB	�wB	��B	��B	��B	B	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�)B	�;B	�BB	�HB	�ZB	�TB	�`B	�fB	�mB	�yB	�sB	�mB	�mB	�mB	�yB	�B	�yB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
VB
�B
�B
�B
%�B
-B
33B
;dB
A�B
G�B
M�B
Q�B
YB
aHB
hsB
l�B
p�B
s�B
v�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��B
ɺB
ǮB
ŢB
��B
��B
�wB
��B
��B
��B
��B
��B
��B
ÖB
ĜB
ĜB
ĜB
ĜB
ŢB
ƨB
ɺB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�#B
�/B
�/B
�B
�)B
�B
��B
��B
�wB
�qB
��B
��B
�B
�B
�TBJB6FBW
Bp�B{�B�B�bB�9B�jBÖB�)B��BBDB�B)�B0!B@�BI�BJ�BO�BT�B]/B[#B\)B^5B`BBaHB^5B`BB_;B_;BZBS�BR�BK�BE�BA�BB�BB�B7LB)�B{BB��B�B�`B��B��By�BZBI�B<jBuB
�B
�/B
�#B
�wB
�RB
��B
��B
��B
��B
�bB
|�B
P�B
>wB
.B	��B	��B	�VB	}�B	p�B	_;B	E�B	!�B	oB	
=B	B�B�NB�
B��B��B��B��BĜB�}B��B��B��B��B��B�{B��B�{B�{B�{B�{B�uB�hB�hB�oB�{B��B��B��B��B�\B�uB��B�{B�uB��B�{B��B��B��B��B�{B�hB�oB�hB�DB�DB�=B�JB�bB��B�=B�1B�=B�hB��B��B�oB�\B�bB�\B��B��B�uB�hB�bB�hB�uB��B��B��B�{B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BƨB��B��B��B��B��B��B�B�B�)B�HB�;B�B�}B�XB�FB�RB�!B�B�!B�!B�LB��B��BÖB��B�B��B��B�B		7B	VB	\B	VB	VB	\B	PB	VB		7B	%B	B	DB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	%�B	0!B	G�B	jB	w�B	{�B	}�B	� B	�B	�%B	�1B	�=B	�DB	�JB	�PB	�VB	�PB	�PB	�PB	�PB	�PB	�PB	�VB	�VB	�bB	�bB	�bB	�hB	�uB	�oB	�oB	�oB	�oB	�{B	�{B	�{B	��B	��B	�oB	�hB	�PB	�{B	��B	��B	�{B	�oB	�uB	��B	�uB	�{B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	�uB	�uB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�LB	�?B	�-B	�3B	�RB	�XB	�RB	�RB	�wB	�LB	�LB	�XB	ÖB	B	�XB	�FB	�RB	�3B	�XB	�B	�!B	�B	��B	�B	��B	��B	�B	�?B	�9B	�RB	�dB	�jB	�wB	�}B	�}B	�}B	��B	B	�wB	�qB	�dB	�^B	�dB	�XB	�XB	�XB	�^B	�dB	�^B	�XB	�XB	�FB	�FB	�FB	�FB	�LB	�LB	�RB	�RB	�XB	�^B	�dB	�}B	�qB	�}B	��B	��B	��B	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�)B	�;B	�BB	�HB	�ZB	�TB	�fB	�fB	�sB	�B	�sB	�mB	�mB	�mB	�B	�B	�yB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
VB
�B
�B
�B
%�B
-B
33B
;dB
A�B
H�B
M�B
Q�B
YB
aHB
hsB
l�B
p�B
s�B
v�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<�o<#�
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
<49X<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451522012011014515220120110145152  AO  ARGQ                                                                        20111130143457  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143457  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145152  IP                  G�O�G�O�G�O�                