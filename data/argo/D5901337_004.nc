CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:24Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112342  20190522121835  1901_5055_004                   2C  D   APEX                            2140                            040306                          846 @�5GC!�1   @�5H    @0/�;dZ�c)�Q�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�33B�  B�  B�  B�33B�  B�  B���B�33B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@�fDA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dv�3Dy��D��D�@ D�i�D���D�fD�FfD�p D�� D�� D��D�Y�D��fD���D�9�D�\�D�3D�� D��D�C3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B�  B�  B���B���B���B�  B���B���B���B�  B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B�  B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  D� D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@� D@��DAy�DA��DBy�DB��DCy�DC��DDy�DE  DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DJ  DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De�3Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dy�fD�	�D�<�D�ffD���D�3D�C3D�l�D���D���D�fD�VfD��3D��fD�6fD�Y�D� D���D��D�@ D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�ĜA۰!Aۡ�A���A�Q�Aا�Aװ!Aם�AׅA�jA�G�A�1'A� �A�JA�  A��A��HA���A֬A֓uAև+A�|�A�v�A�p�A�jA�dZA�`BA�ZA�S�A�Q�A�M�A�G�A�C�A�=qA�;dA�1'A� �Aէ�Aԟ�AуÁA��HA�?}AÁA��A��
A�
=A��A�O�A��A��A�O�A���A��A�-A�^5A��A���A�dZA���A�{A��-A�XA�JA��A��`A���A���A��A���A��^A��RA���A���A�jA�r�A��/A�&�A�33A���A��wA��#A�A���A���A���A�v�A�
=A��uA�jA��A��HA��RA�ƨA���A�v�A�  A�bNA�E�A{XAy
=AtI�An=qAj�jAd��A`�HA[��AW��ATr�AP�\AM��AI�mAD�\AA�TAA?}A=�A:�!A8(�A7x�A8��A8E�A5�;A4��A3�A2��A0�jA/O�A.��A-��A-?}A,��A*1A'"�A#oA"bNA"  A ��AO�AC�A��A|�A1A�FA��AZA�At�A  A�7A�mA �A��A��AC�A1A��AS�A�-AA{AbA�DA��A�/A��A��A7LA�A��A�mA�9AA�A�`A�AȴA%A	G�A	S�A
9XA
�\A
v�A
(�A�yA/A�\A-A  A�TAS�A�A��A�+AZA�A�mA��AoAz�AVA5?A��AS�A ��A ��A =q@��
@��@���@�~�@�9X@��R@�v�@�{@�/@��@�A�@���@�S�@�v�@�@�@��@�@�j@�@�33@��@�7@�r�@���@�@�b@�1@�ƨ@�ƨ@�@�bN@�P@�
=@�=q@�^@��@�?}@�Z@�(�@�I�@�Ĝ@�z�@��u@��m@�t�@�"�@ާ�@�{@�p�@�Z@ۮ@ڟ�@�$�@�7L@���@�  @�@ԓu@�r�@�r�@��;@�=q@���@�bN@�I�@�Q�@�1'@�ƨ@�l�@϶F@�
=@���@θR@�{@�%@̛�@�
=@�-@�Ĝ@��@Ǖ�@�t�@�\)@��@�ff@�J@ź^@�@���@Ų-@Ł@�G�@���@�Z@���@�t�@�t�@î@Ý�@î@�I�@�A�@��;@�ff@�=q@�@�5?@�@��T@�/@���@���@�Ĝ@�r�@���@��R@�n�@�E�@�5?@��#@�O�@�Ĝ@�Q�@���@���@�S�@�ff@�?}@��@�Q�@�b@�b@��w@�dZ@���@�n�@�$�@���@�Ĝ@��D@��m@�t�@�;d@��@�
=@��@���@�M�@��T@��7@�p�@��@���@�1@�"�@���@�n�@�-@���@���@��h@�X@��@���@�Ĝ@�Z@���@��F@�|�@�@���@�=q@�5?@��#@�hs@��9@�9X@�9X@� �@��F@�t�@�\)@�K�@��@��@��@��R@�^5@�{@�@�X@��@���@��@� �@���@���@�S�@��@�ff@�=q@�~�@���@��@��y@�ff@�?}@��@�t�@�K�@�\)@�C�@�@���@�@�hs@�?}@�Ĝ@�bN@�9X@��
@���@��;@���@���@��F@��@�33@���@�V@��#@���@�X@���@���@���@�bN@�1@��@��P@�l�@�S�@�"�@�
=@�@���@�V@�@��T@��7@�&�@��j@�z�@�A�@�  @���@���@��P@�\)@�+@��@��R@�n�@�=q@��@��7@�p�@�O�@��@���@��@��@� �@�  @��@��@�l�@��H@��\@�n�@�V@�-@���@�hs@��@��/@��@���@�Q�@x�`@k��@bJ@Z�!@S��@L(�@D�/@>@7�;@/�w@*J@$1@E�@n�@V@��@E�@
^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�ĜA۰!Aۡ�A���A�Q�Aا�Aװ!Aם�AׅA�jA�G�A�1'A� �A�JA�  A��A��HA���A֬A֓uAև+A�|�A�v�A�p�A�jA�dZA�`BA�ZA�S�A�Q�A�M�A�G�A�C�A�=qA�;dA�1'A� �Aէ�Aԟ�AуÁA��HA�?}AÁA��A��
A�
=A��A�O�A��A��A�O�A���A��A�-A�^5A��A���A�dZA���A�{A��-A�XA�JA��A��`A���A���A��A���A��^A��RA���A���A�jA�r�A��/A�&�A�33A���A��wA��#A�A���A���A���A�v�A�
=A��uA�jA��A��HA��RA�ƨA���A�v�A�  A�bNA�E�A{XAy
=AtI�An=qAj�jAd��A`�HA[��AW��ATr�AP�\AM��AI�mAD�\AA�TAA?}A=�A:�!A8(�A7x�A8��A8E�A5�;A4��A3�A2��A0�jA/O�A.��A-��A-?}A,��A*1A'"�A#oA"bNA"  A ��AO�AC�A��A|�A1A�FA��AZA�At�A  A�7A�mA �A��A��AC�A1A��AS�A�-AA{AbA�DA��A�/A��A��A7LA�A��A�mA�9AA�A�`A�AȴA%A	G�A	S�A
9XA
�\A
v�A
(�A�yA/A�\A-A  A�TAS�A�A��A�+AZA�A�mA��AoAz�AVA5?A��AS�A ��A ��A =q@��
@��@���@�~�@�9X@��R@�v�@�{@�/@��@�A�@���@�S�@�v�@�@�@��@�@�j@�@�33@��@�7@�r�@���@�@�b@�1@�ƨ@�ƨ@�@�bN@�P@�
=@�=q@�^@��@�?}@�Z@�(�@�I�@�Ĝ@�z�@��u@��m@�t�@�"�@ާ�@�{@�p�@�Z@ۮ@ڟ�@�$�@�7L@���@�  @�@ԓu@�r�@�r�@��;@�=q@���@�bN@�I�@�Q�@�1'@�ƨ@�l�@϶F@�
=@���@θR@�{@�%@̛�@�
=@�-@�Ĝ@��@Ǖ�@�t�@�\)@��@�ff@�J@ź^@�@���@Ų-@Ł@�G�@���@�Z@���@�t�@�t�@î@Ý�@î@�I�@�A�@��;@�ff@�=q@�@�5?@�@��T@�/@���@���@�Ĝ@�r�@���@��R@�n�@�E�@�5?@��#@�O�@�Ĝ@�Q�@���@���@�S�@�ff@�?}@��@�Q�@�b@�b@��w@�dZ@���@�n�@�$�@���@�Ĝ@��D@��m@�t�@�;d@��@�
=@��@���@�M�@��T@��7@�p�@��@���@�1@�"�@���@�n�@�-@���@���@��h@�X@��@���@�Ĝ@�Z@���@��F@�|�@�@���@�=q@�5?@��#@�hs@��9@�9X@�9X@� �@��F@�t�@�\)@�K�@��@��@��@��R@�^5@�{@�@�X@��@���@��@� �@���@���@�S�@��@�ff@�=q@�~�@���@��@��y@�ff@�?}@��@�t�@�K�@�\)@�C�@�@���@�@�hs@�?}@�Ĝ@�bN@�9X@��
@���@��;@���@���@��F@��@�33@���@�V@��#@���@�X@���@���@���@�bN@�1@��@��P@�l�@�S�@�"�@�
=@�@���@�V@�@��T@��7@�&�@��j@�z�@�A�@�  @���@���@��P@�\)@�+@��@��R@�n�@�=q@��@��7@�p�@�O�@��@���@��@��@� �@�  @��@��@�l�@��H@��\@�n�@�V@�-@���@�hs@��@��/@��@���@�Q�@x�`@k��@bJ@Z�!@S��@L(�@D�/@>@7�;@/�w@*J@$1@E�@n�@V@��@E�@
^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
hsB
hsB
hsB
gmB
ffB
dZB
dZB
cTB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
iyB
iyB
iyB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
hsB
iyB
iyB
iyB
hsB
gmB
aHB
W
B
:^B
[#B
jB
ZB
ffB
s�B
�B
�\B
�{B
�B
ȴB  B33B33B}�B�ZB��B{B$�B.BD�BD�B=qB9XB49B �B�B��B�jB��BȴB�BB�/B��B��B�ZB�)B�BɺB�RB��B�\Bl�BR�B1'BhB
��B
�5B
��B
�}B
B
��B
�#B
ƨB
�-B
�B
XB
(�B	�B	�wB	�{B	�B	_;B	8RB	"�B��B�HBÖB�LB�B�LB�^B��B�RB�!B�}BƨBɺB��B�B	B	�B	{B	hB	�B	�B	�B	�B	�B	�B	&�B	+B	�B��B�5B�`B�B�/B��BǮBĜBÖB�}B�dBĜB��B�fB�B	B	bB	�B	�B	 �B	A�B	I�B	B�B	9XB	-B	�B	oB	DB	�B	2-B	A�B	F�B	=qB	6FB	9XB	;dB	:^B	L�B	dZB	ffB	\)B	m�B	m�B	`BB	W
B	aHB	q�B	~�B	}�B	|�B	u�B	n�B	k�B	n�B	q�B	u�B	{�B	�B	�=B	�JB	�PB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�oB	��B	��B	��B	��B	��B	��B	�uB	��B	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�LB	�FB	�LB	�LB	�?B	�9B	�3B	�9B	�3B	�3B	�3B	�9B	�9B	�'B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�-B	�-B	�'B	�B	�B	�B	�B	�B	�B	�B	�B	�3B	�9B	�?B	�LB	�jB	�jB	�jB	�jB	�dB	�^B	�^B	�dB	�jB	��B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�#B	�/B	�5B	�5B	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�HB	�BB	�TB	�ZB	�TB	�ZB	�`B	�yB	�yB	�yB	�yB	�B	�yB	�B	�yB	�yB	�mB	�fB	�fB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
DB
JB
JB
JB
PB
PB
PB
VB
VB
\B
\B
\B
bB
bB
bB
oB
oB
oB
oB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
&�B
/B
5?B
:^B
C�B
L�B
N�B
Q�B
[#B
_;B
e`B
iyB
m�B
r�B
v�B
y�B
}�B
�B
�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
hsB
hsB
hsB
gmB
ffB
dZB
dZB
cTB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
iyB
iyB
iyB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
hsB
iyB
iyB
iyB
hsB
gmB
aHB
W
B
:^B
[#B
jB
ZB
ffB
s�B
�B
�\B
�{B
�B
ȴB  B33B33B}�B�ZB��B{B$�B.BD�BD�B=qB9XB49B �B�B��B�jB��BȴB�BB�/B��B��B�ZB�)B�BɺB�RB��B�\Bl�BR�B1'BhB
��B
�5B
��B
�}B
B
��B
�#B
ƨB
�-B
�B
XB
(�B	�B	�wB	�{B	�B	_;B	8RB	"�B��B�HBÖB�LB�B�LB�^B��B�RB�!B�}BƨBɺB��B�B	B	�B	{B	hB	�B	�B	�B	�B	�B	�B	&�B	+B	�B��B�5B�`B�B�/B��BǮBĜBÖB�}B�dBĜB��B�fB�B	B	bB	�B	�B	 �B	A�B	I�B	B�B	9XB	-B	�B	oB	DB	�B	2-B	A�B	F�B	=qB	6FB	9XB	;dB	:^B	L�B	dZB	ffB	\)B	m�B	m�B	`BB	W
B	aHB	q�B	~�B	}�B	|�B	u�B	n�B	k�B	n�B	q�B	u�B	{�B	�B	�=B	�JB	�PB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�oB	��B	��B	��B	��B	��B	��B	�uB	��B	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�LB	�FB	�LB	�LB	�?B	�9B	�3B	�9B	�3B	�3B	�3B	�9B	�9B	�'B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�-B	�-B	�'B	�B	�B	�B	�B	�B	�B	�B	�B	�3B	�9B	�?B	�LB	�jB	�jB	�jB	�jB	�dB	�^B	�^B	�dB	�jB	��B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�#B	�/B	�5B	�5B	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�HB	�BB	�TB	�ZB	�TB	�ZB	�`B	�yB	�yB	�yB	�yB	�B	�yB	�B	�yB	�yB	�mB	�fB	�fB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
DB
JB
JB
JB
PB
PB
PB
VB
VB
\B
\B
\B
bB
bB
bB
oB
oB
oB
oB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
&�B
/B
5?B
:^B
C�B
L�B
N�B
Q�B
[#B
_;B
e`B
iyB
m�B
r�B
v�B
z�B
}�B
�B
�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201201131251282012011312512820120113125128  AO  ARGQ                                                                        20111205112342  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112342  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125128  IP                  G�O�G�O�G�O�                