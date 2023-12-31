CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:54Z UW 3.1 conversion   
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135534  20190522121825  1727_5046_012                   2C  D   APEX                            2143                            040306                          846 @�&�kT�1   @�&�)���@8%�����c�\(�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ��B%33B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� D@��DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZy�D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dys3D�#3D�\�D��3D�� D��D�l�D��3D��fD�3D�c3D���D��fD�  D�i�Dڳ3D��D��D�@ D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B ffB$��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC  C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;��C=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@�3DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZs3DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds�3Dyl�D�  D�Y�D�� D���D��D�i�D�� D��3D� D�` D���D��3D��D�ffDڰ D��fD�fD�<�D�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A�A�A�A�  A���A���A���A��A��mA��#A̸RA̬A̙�A�n�A�ZA�9XA˰!A��HA�hsA���A�hsA��#A�VA��A��;A�n�A���A�&�A�/A�1A��A���A���A��A�&�A��/A��\A���A���A�I�A�ƨA�9XA��9A�ffA��!A��A�p�A���A�ȴA�jA���A��A�XA�G�A�~�A��HA�I�A�r�A�bNA�t�A���A�ffA��A���A�=qA�|�A��/A��A�ƨA���A��A�VA��7A��;A�K�A���A��A�v�A��/A��jA�z�A���A���A�|�A���A�bA� �A�ȴA�x�A�oA��HA�t�A�\)A�?}A�;dA�VA���A��/A��hA���A���A�z�A���A���A�XA���A��hA��A�$�A���A��^A��A�l�A�ffA� �A|jAz�yAy"�Au�
As�-Aq?}AoXAl �Ak|�AjI�Ah$�Ad��Ac�wAbr�Aa��AaK�A`A�A^�/A^r�A\9XAY�TAX�uAW��AVQ�AUO�AS��AS%AR��AR��AR^5AOAK�AK�AKAJ1AIdZAI
=AH�jAHA�AG\)AFn�AEp�AC�AA��A@��A@�\A?;dA=��A<r�A;?}A:E�A9�PA9�A7�^A5��A4n�A3��A1�
A1%A0��A0(�A/�TA/�PA/�A.�DA.E�A.A-�TA-�-A-;dA,�A,Q�A+�A*ȴA*$�A)�TA)l�A(�A(ZA'�#A&ĜA&E�A%��A$�jA#t�A"�yA"bNA!�FA!VA!A �A �9A��AI�A��A��A�wAhsA\)A/A�yA��A��A�7AA�RAVA`BA%A�+A�Ar�A�A�PA��AhsA9XAƨA`BA7LA
E�A	��A	A1'A
=Ar�A��A9XA��AȴA��AƨA bNA 5?A �@��w@�`B@��+@���@�33@�D@��H@�ȴ@�I�@�ƨ@��@�M�@��#@��T@��`@�@��@@���@�j@�
=@�+@�7@�j@��@�$�@�R@�hs@䛦@���@���@��@�&�@�  @ޏ\@��`@��y@�?}@��@�x�@�%@ؼj@��
@պ^@�1@ӝ�@��@�hs@ѡ�@�r�@υ@�V@���@ǍP@�  @°!@���@���@��@��T@�7L@��j@�Z@���@��w@�;d@�n�@�@��h@�7L@��@��m@�S�@�~�@�@�/@�%@��@�bN@�b@��
@��@�@�5?@�x�@�G�@��`@��@���@�;d@��@��R@�@�G�@�?}@��^@��^@��-@�@���@�7L@�r�@�+@��P@�(�@�7L@���@��#@�G�@��j@�1'@���@�ƨ@�t�@���@�=q@�?}@��/@��j@�z�@�9X@��@���@��@�`B@�Ĝ@�1'@�b@�1@���@��@��!@�M�@���@�O�@�7L@�/@��@��@��@���@���@�bN@�(�@�1@��
@��@���@��@�K�@�33@��H@�-@��@��j@�A�@�1@��@��F@�|�@��@�33@��y@�=q@��^@���@�hs@��D@�^5@��h@��#@�@��^@��/@��@��;@���@��@�$�@���@���@��^@���@��@�`B@���@��@���@��@��@�|�@�;d@�"�@��y@�n�@�@��^@�hs@���@���@�Ĝ@�r�@�r�@�b@�ƨ@�ƨ@��w@���@��@�S�@�+@��y@�
=@�+@��@��@��#@�%@��@�r�@���@�9X@�z�@�bN@�9X@� �@�1@�ƨ@�K�@�ȴ@�5?@��@��^@��@~5?@u�@t�j@gK�@]p�@V��@P  @I��@@�`@;�m@5?}@.�+@(r�@#o@�R@7L@�@�@`B@	&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A�A�A�A�  A���A���A���A��A��mA��#A̸RA̬A̙�A�n�A�ZA�9XA˰!A��HA�hsA���A�hsA��#A�VA��A��;A�n�A���A�&�A�/A�1A��A���A���A��A�&�A��/A��\A���A���A�I�A�ƨA�9XA��9A�ffA��!A��A�p�A���A�ȴA�jA���A��A�XA�G�A�~�A��HA�I�A�r�A�bNA�t�A���A�ffA��A���A�=qA�|�A��/A��A�ƨA���A��A�VA��7A��;A�K�A���A��A�v�A��/A��jA�z�A���A���A�|�A���A�bA� �A�ȴA�x�A�oA��HA�t�A�\)A�?}A�;dA�VA���A��/A��hA���A���A�z�A���A���A�XA���A��hA��A�$�A���A��^A��A�l�A�ffA� �A|jAz�yAy"�Au�
As�-Aq?}AoXAl �Ak|�AjI�Ah$�Ad��Ac�wAbr�Aa��AaK�A`A�A^�/A^r�A\9XAY�TAX�uAW��AVQ�AUO�AS��AS%AR��AR��AR^5AOAK�AK�AKAJ1AIdZAI
=AH�jAHA�AG\)AFn�AEp�AC�AA��A@��A@�\A?;dA=��A<r�A;?}A:E�A9�PA9�A7�^A5��A4n�A3��A1�
A1%A0��A0(�A/�TA/�PA/�A.�DA.E�A.A-�TA-�-A-;dA,�A,Q�A+�A*ȴA*$�A)�TA)l�A(�A(ZA'�#A&ĜA&E�A%��A$�jA#t�A"�yA"bNA!�FA!VA!A �A �9A��AI�A��A��A�wAhsA\)A/A�yA��A��A�7AA�RAVA`BA%A�+A�Ar�A�A�PA��AhsA9XAƨA`BA7LA
E�A	��A	A1'A
=Ar�A��A9XA��AȴA��AƨA bNA 5?A �@��w@�`B@��+@���@�33@�D@��H@�ȴ@�I�@�ƨ@��@�M�@��#@��T@��`@�@��@@���@�j@�
=@�+@�7@�j@��@�$�@�R@�hs@䛦@���@���@��@�&�@�  @ޏ\@��`@��y@�?}@��@�x�@�%@ؼj@��
@պ^@�1@ӝ�@��@�hs@ѡ�@�r�@υ@�V@���@ǍP@�  @°!@���@���@��@��T@�7L@��j@�Z@���@��w@�;d@�n�@�@��h@�7L@��@��m@�S�@�~�@�@�/@�%@��@�bN@�b@��
@��@�@�5?@�x�@�G�@��`@��@���@�;d@��@��R@�@�G�@�?}@��^@��^@��-@�@���@�7L@�r�@�+@��P@�(�@�7L@���@��#@�G�@��j@�1'@���@�ƨ@�t�@���@�=q@�?}@��/@��j@�z�@�9X@��@���@��@�`B@�Ĝ@�1'@�b@�1@���@��@��!@�M�@���@�O�@�7L@�/@��@��@��@���@���@�bN@�(�@�1@��
@��@���@��@�K�@�33@��H@�-@��@��j@�A�@�1@��@��F@�|�@��@�33@��y@�=q@��^@���@�hs@��D@�^5@��h@��#@�@��^@��/@��@��;@���@��@�$�@���@���@��^@���@��@�`B@���@��@���@��@��@�|�@�;d@�"�@��y@�n�@�@��^@�hs@���@���@�Ĝ@�r�@�r�@�b@�ƨ@�ƨ@��w@���@��@�S�@�+@��y@�
=@�+@��@��@��#@�%@��@�r�@���@�9X@�z�@�bN@�9X@� �@�1@�ƨ@�K�@�ȴ@�5?@��@��^@��@~5?@u�@t�j@gK�@]p�@V��@P  @I��@@�`@;�m@5?}@.�+@(r�@#o@�R@7L@�@�@`B@	&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�;Bp�B�B�wB��B��B��B�}B�?B��B��BȴB�#B�B�B��B�B�B�B�B�B�B�B�B�B�mB�TB�;B�5B�/B�5B�;B�BB�NB�TB�B�B�B�mB�fB�TB�/B��B�?B��B��B��B��B�uB�=Bz�Bu�Bt�Br�BjBbNB[#BQ�BI�BA�B?}B:^B1'B.B&�B�BB�B�#B��B�^B�-B�{Bp�B^5BI�BP�BO�B`BBp�B6FBPB
�B
�ZB
�B
�B
�NB
�B
�;B
��B
�^B
��B
�B
bNB
T�B
H�B
K�B
Q�B
=qB
�B
hB
B	�B	�)B	��B	�dB	��B	�9B	�LB	��B	�7B	�B	x�B	s�B	o�B	k�B	bNB	]/B	S�B	@�B	9XB	2-B	)�B	!�B	�B	�B	�B	�B	uB��B�B��B	%B	B	B	  B��B��B��B��B�B�yB�NB�5B�)B�B��B��B��BȴBƨBÖB�}B�qB�jB�XB�FB�9B�3B�3B�3B�-B�-B�'B�'B�-B�-B�-B�3B�9B�3B�-B�9B�?B�?B�?B�9B�9B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�hB�VB�JB�JB�JB�JB�DB�7B�B�B� B}�B{�Bz�Bx�Bv�Bs�Bn�Bk�BiyBgmBcTBbNB`BB_;B^5B\)BZBXBW
BS�BP�BM�BK�BI�BH�BG�BD�BD�BC�BB�B@�B=qB:^B8RB6FB49B8RBB�B[#B^5B]/B\)BdZBhsBk�Bm�Bn�Bq�Bw�Bx�By�Bx�Bw�By�B}�B�%B�bB��B��B��B��B��B��B��B��B��B��B��B�B�9B�LB�^B�^B�XB��BƨBBBÖBB��B�qB�FB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�-B�FB�^B�dB�jB�qB��BĜBƨBǮBȴB��B��B��B�#B�/B�5B�/B�TB�mB�B�B�B��B��B��B�B�B��B	  B	DB	
=B	
=B	DB	PB	hB	{B	�B	{B	�B	�B	�B	{B	{B	uB	uB	uB	�B	�B	�B	�B	�B	�B	�B	 �B	$�B	%�B	'�B	)�B	-B	.B	.B	.B	1'B	9XB	?}B	B�B	D�B	E�B	F�B	J�B	L�B	M�B	P�B	P�B	Q�B	Q�B	R�B	VB	\)B	aHB	dZB	e`B	gmB	jB	l�B	l�B	m�B	m�B	o�B	p�B	p�B	k�B	n�B	u�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�1B	�7B	�=B	�7B	�7B	�=B	�JB	�bB	�hB	�hB	�uB	�uB	�uB	�{B	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�XB	�^B	�^B	�^B	�^B	�^B	�XB	�LB	�FB	�FB	�LB	�RB	ƨB	�NB
%B
JB
hB
�B
 �B
'�B
2-B
8RB
?}B
G�B
N�B
T�B
\)B
cTB
jB
l�B
r�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�TBy�B�3B��BĜB�
B��BB�jB��B��BȴB�)B�B��B��B��B��B��B��B�B�B�B�B�B�yB�sB�NB�BB�BB�BB�BB�NB�ZB�mB�B�B�B�B�yB�mB�NB��B�RB��B��B��B��B��B�bB}�Bv�Bu�Bt�Bn�BdZB^5BT�BL�BC�BB�B=qB2-B/B)�B�B+B�B�5B�
B�jB�XB��Bv�Be`BJ�BQ�BO�B`BB~�B?}B{B
��B
�`B
�B
�B
�mB
�B
�NB
�
B
��B
�B
�DB
gmB
ZB
K�B
L�B
ZB
I�B
$�B
�B

=B	��B	�NB	��B	B	�B	�LB	�qB	��B	�JB	�B	z�B	t�B	r�B	o�B	cTB	cTB	ZB	D�B	<jB	6FB	-B	&�B	�B	�B	�B	�B	�B	+B�B��B		7B	%B	B	B	B	  B��B��B��B�B�fB�;B�BB�/B�B��B��B��BȴBȴBŢBB�}B�}B�XB�FB�?B�9B�9B�3B�3B�-B�-B�3B�3B�3B�?B�?B�9B�?B�FB�FB�LB�LB�FB�FB�?B�'B�B�B�B��B��B��B��B��B��B��B��B��B�{B�{B�oB�PB�JB�PB�PB�JB�JB�JB�B�B� B~�B{�Bz�By�Bx�Bs�Bp�Bm�Bl�BgmBdZBbNB`BBaHB^5B\)BZBZBVBS�BQ�BO�BJ�BI�BJ�BH�BE�BD�BC�BD�BA�B=qB:^B:^B7LB8RB@�B\)B_;B^5B]/BdZBjBm�Bn�Bo�Bs�By�Bz�Bz�Bz�By�B}�B}�B�B�oB��B��B��B��B��B��B��B��B��B��B��B�B�?B�RB�dB�qB�dB��BȴBÖBBŢBĜBĜB��B�dB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�!B�B�-B�LB�dB�jB�qB�wBBŢBǮBȴB��B��B��B��B�)B�;B�;B�/B�NB�mB�B�B�B��B��B��B��B�B��B��B	JB	DB	DB	JB	PB	hB	�B	�B	�B	�B	�B	�B	�B	�B	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	&�B	(�B	)�B	-B	.B	.B	/B	1'B	9XB	@�B	C�B	D�B	E�B	F�B	J�B	L�B	N�B	P�B	Q�B	R�B	S�B	S�B	W
B	]/B	aHB	e`B	ffB	hsB	jB	m�B	m�B	n�B	m�B	p�B	r�B	t�B	l�B	n�B	u�B	}�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�1B	�7B	�DB	�=B	�=B	�DB	�PB	�bB	�hB	�oB	�uB	�{B	�{B	�{B	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�XB	�^B	�^B	�^B	�dB	�dB	�^B	�RB	�FB	�LB	�RB	�RB	ƨB	�NB
%B
JB
hB
�B
 �B
'�B
2-B
8RB
?}B
H�B
N�B
T�B
\)B
cTB
jB
l�B
r�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446382012010314463820120103144638  AO  ARGQ                                                                        20111130135534  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135534  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144638  IP                  G�O�G�O�G�O�                