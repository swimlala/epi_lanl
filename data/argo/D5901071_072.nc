CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:11Z UW 3.1 conversion   
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
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               HA   AO  20111130140746  20190522121826  1727_5046_072                   2C  D   APEX                            2143                            040306                          846 @�r�ŀ 1   @�r�j1@@7{�l�C��dV�u1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D	��D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dry�Ds  Ds� Ds� Dy�fD�#3D�vfD���D�� D��D�\�D��fD��D��D�VfD���D��3D�fD�ffDڦfD�ٚD��D�\�D�D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC0  C1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	�3D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/� D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DW  DW� DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Dj� Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp�3Dqy�Dq��Drs3Dr��Dsy�DsٚDy� D�  D�s3D��fD���D��D�Y�D��3D��fD�fD�S3D���D�� D�3D�c3Dڣ3D��fD��D�Y�D�fD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A���A���A���A��uA���A���A���A���A���A��hA��PA��DA��+A��A��A��A�|�A�v�A�jA�=qA�-A�M�A�hsA���A���A�|�A� �A���A��A�t�A�l�A�-A���A��9A���A���A�n�A�VA�A�A�;dA�-A�$�A��mA�ƨA�Q�A�  A��A��A��;A���A�n�A�XA�9XA��#A�jA��HA�x�A��7A��A�  A�oA���A��7A��uA��FA��A�|�A�5?A�M�A��A�+A���A�x�A��;A�VA���A��uA�t�A��!A�A��A��A�=qA���A��A��#A��A���A���A�ĜA�
=A�~�A�\)A���A�
=A���A�
=A��/A�"�A�7LA�"�A��A�Q�A���A�XA��!A��`A��+A�/A�
=A| �A{�Az��Ay�-AvjAu�At��At~�As�Aq��ApjAoAmVAk�Aj��Aj��Ai��Aj�\Ai�mAg/Ae��AcG�AbffAa�^A`��A`1A_�PA_&�A^I�AZ�/AXA�AWVAUoAT1'AS�AR�AQ"�AP��AO�^AOK�AN�`AM��AL��AK�wAJ�/AI�AHz�AG�-AFȴAD-AB��AB �AA��AAoA@I�A?��A?"�A>z�A=G�A<�A;C�A9��A8�A8A�A6��A5��A5�A4A�A2ĜA1p�A/7LA.�A.�A-�A,�yA+��A*��A)��A(�!A'C�A&n�A%�A$VA#��A" �A �/A��A�AA�AƨA�AS�AA��A��A&�Ar�A�A  AG�A�AffAx�A��A�PA%A�+A�FA�9A�A33A�yAr�A��AS�A
=A�A1'A
�jA	\)A	&�A�/AE�AS�A�uA  A+A�AXA��A��A5?AVA =qA �@�t�@�ȴ@�hs@��@�{@�bN@�b@���@��!@��@�F@�r�@�v�@�\@��@�b@睲@�+@��@�33@◍@�^@�r�@�S�@�M�@ݩ�@ܼj@�33@�ȴ@�E�@ٲ-@�%@�I�@�t�@թ�@�;d@�M�@�`B@�dZ@�{@��@Ͳ-@���@�dZ@�V@��T@�7L@��/@�j@�1@�33@��@��H@ư!@�J@�X@�C�@�bN@���@�dZ@�@���@�Z@�b@��F@��@��H@�@��;@���@�%@��@�j@��;@���@��H@�n�@��@�@�5?@���@��`@��
@���@�S�@�v�@���@�&�@�bN@��@���@�C�@���@�{@���@�%@��@�bN@��P@��@��\@�n�@�M�@�E�@�@�z�@�Z@�1@�\)@��y@�ȴ@���@��R@�E�@�-@��T@���@��h@��@���@�Z@�1@��w@�|�@��y@�v�@�v�@�v�@���@�  @���@��@���@��@��h@��@��#@���@���@��7@���@���@�\)@��@�v�@���@��#@��^@���@�O�@��@�Ĝ@��9@��u@�I�@��m@��F@�t�@�dZ@�33@��y@��!@��+@�ff@�5?@�`B@��@���@��/@��@���@�|�@��P@���@��;@��@��P@�t�@�\)@�K�@��@�o@�ȴ@���@�=q@�{@��T@���@��@��
@���@�33@��@��+@�ff@�E�@�-@�{@���@�@��#@��-@���@���@��7@��7@��@��D@� �@�  @���@���@��P@�t�@�dZ@��@�
=@��y@��R@�~�@�E�@�5?@�{@�@��@��#@���@���@��-@�hs@�V@���@��`@�Ĝ@�  @�ƨ@��P@�t�@�o@��H@��@��@|�@q%@g�w@]�h@X�u@SC�@K@D�j@;�
@4z�@-�T@'+@"-@�@^5@��@G�@��@	%@v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A���A���A���A��uA���A���A���A���A���A��hA��PA��DA��+A��A��A��A�|�A�v�A�jA�=qA�-A�M�A�hsA���A���A�|�A� �A���A��A�t�A�l�A�-A���A��9A���A���A�n�A�VA�A�A�;dA�-A�$�A��mA�ƨA�Q�A�  A��A��A��;A���A�n�A�XA�9XA��#A�jA��HA�x�A��7A��A�  A�oA���A��7A��uA��FA��A�|�A�5?A�M�A��A�+A���A�x�A��;A�VA���A��uA�t�A��!A�A��A��A�=qA���A��A��#A��A���A���A�ĜA�
=A�~�A�\)A���A�
=A���A�
=A��/A�"�A�7LA�"�A��A�Q�A���A�XA��!A��`A��+A�/A�
=A| �A{�Az��Ay�-AvjAu�At��At~�As�Aq��ApjAoAmVAk�Aj��Aj��Ai��Aj�\Ai�mAg/Ae��AcG�AbffAa�^A`��A`1A_�PA_&�A^I�AZ�/AXA�AWVAUoAT1'AS�AR�AQ"�AP��AO�^AOK�AN�`AM��AL��AK�wAJ�/AI�AHz�AG�-AFȴAD-AB��AB �AA��AAoA@I�A?��A?"�A>z�A=G�A<�A;C�A9��A8�A8A�A6��A5��A5�A4A�A2ĜA1p�A/7LA.�A.�A-�A,�yA+��A*��A)��A(�!A'C�A&n�A%�A$VA#��A" �A �/A��A�AA�AƨA�AS�AA��A��A&�Ar�A�A  AG�A�AffAx�A��A�PA%A�+A�FA�9A�A33A�yAr�A��AS�A
=A�A1'A
�jA	\)A	&�A�/AE�AS�A�uA  A+A�AXA��A��A5?AVA =qA �@�t�@�ȴ@�hs@��@�{@�bN@�b@���@��!@��@�F@�r�@�v�@�\@��@�b@睲@�+@��@�33@◍@�^@�r�@�S�@�M�@ݩ�@ܼj@�33@�ȴ@�E�@ٲ-@�%@�I�@�t�@թ�@�;d@�M�@�`B@�dZ@�{@��@Ͳ-@���@�dZ@�V@��T@�7L@��/@�j@�1@�33@��@��H@ư!@�J@�X@�C�@�bN@���@�dZ@�@���@�Z@�b@��F@��@��H@�@��;@���@�%@��@�j@��;@���@��H@�n�@��@�@�5?@���@��`@��
@���@�S�@�v�@���@�&�@�bN@��@���@�C�@���@�{@���@�%@��@�bN@��P@��@��\@�n�@�M�@�E�@�@�z�@�Z@�1@�\)@��y@�ȴ@���@��R@�E�@�-@��T@���@��h@��@���@�Z@�1@��w@�|�@��y@�v�@�v�@�v�@���@�  @���@��@���@��@��h@��@��#@���@���@��7@���@���@�\)@��@�v�@���@��#@��^@���@�O�@��@�Ĝ@��9@��u@�I�@��m@��F@�t�@�dZ@�33@��y@��!@��+@�ff@�5?@�`B@��@���@��/@��@���@�|�@��P@���@��;@��@��P@�t�@�\)@�K�@��@�o@�ȴ@���@�=q@�{@��T@���@��@��
@���@�33@��@��+@�ff@�E�@�-@�{@���@�@��#@��-@���@���@��7@��7@��@��D@� �@�  @���@���@��P@�t�@�dZ@��@�
=@��y@��R@�~�@�E�@�5?@�{@�@��@��#@���@���@��-@�hs@�V@���@��`@�Ĝ@�  @�ƨ@��P@�t�@�o@��H@��@��@|�@q%@g�w@]�h@X�u@SC�@K@D�j@;�
@4z�@-�T@'+@"-@�@^5@��@G�@��@	%@v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBǮBǮBȴBǮBȴBǮBƨBǮBǮBǮBǮBǮBȴBȴBɺBɺBɺB��B��B��B�B!�BT�B�1B��B��B��B��B�-B��B�B�B�)B�5B�BB�TB�ZB�fB�fB�mB�fB�mB�fB�fB�B�B�B�B�B�B��B��B��B��B��B��B�B�B�TB�BB�B��B��B��BȴB�jB��B�wB�jB�qB�Bx�BdZB]/BT�BS�BR�BP�BN�BC�B+B�BVB%B��B�B�B�yB�)B��B�wB�B��B�BcTBR�B33B(�B&�B�B
��B
�`B
ɺB
��B
�dB
�-B
��B
�hB
�DB
�B
gmB
N�B
F�B
A�B
7LB
�B
�B
�B
uB
{B
  B	�B	�yB	�B	ɺB	��B	��B	��B	�B	�sB	��B	ȴB	�3B	��B	��B	��B	�B	�!B	�B	��B	��B	�PB	�%B	}�B	y�B	v�B	o�B	k�B	jB	gmB	e`B	cTB	_;B	YB	R�B	M�B	F�B	@�B	:^B	2-B	&�B	�B	�B	�B	�B	oB	\B	JB	1B	B��B��B�B�B�B�`B�HB�HB�5B�/B�B��B��BǮBĜB��B�dB�RB�?B�-B�B�B��B��B��B��B��B��B��B�uB�uB�oB�oB�bB�VB�JB�=B�1B�B�B� B� B~�B}�Bz�Bw�Bv�Bt�Br�Bq�Bo�Bn�Bm�Bk�BjBiyBhsBffBdZB`BB`BB_;B^5B\)BZBYBW
BT�BS�BS�BR�BQ�BO�BM�BM�BL�BK�BJ�BH�BH�BG�BH�BH�BG�BF�BE�BD�BD�BB�BB�BB�BA�B@�B@�B@�B@�B?}B?}B>wB>wB>wB>wB=qB=qB=qB<jB<jB;dB:^B9XB9XB9XB9XB9XB:^B;dB:^B:^B9XB:^B<jB=qB>wB>wB?}B?}BA�B@�B@�B?}B?}B?}BB�BJ�BJ�BK�BN�BP�BQ�BR�BR�BS�BS�BS�BW
B\)B]/B^5B_;BaHBaHBe`BiyBk�Bv�B}�B�B�%B�DB�JB�VB�\B�uB��B��B��B��B��B��B��B��B��B�B�B�9B�LB�RB�XB�^B�dB�^B�^B�XB�^B�dB�jB�}BŢBƨB��B��B��B��B�
B�#B�)B�BB�ZB�ZB�`B�fB�B�B��B��B	VB	bB	JB	
=B	+B	
=B	JB	bB	�B	�B	�B	�B	�B	�B	"�B	$�B	)�B	/B	2-B	49B	6FB	<jB	?}B	B�B	C�B	E�B	H�B	H�B	J�B	J�B	K�B	L�B	M�B	N�B	N�B	Q�B	XB	\)B	]/B	]/B	\)B	]/B	aHB	dZB	gmB	jB	m�B	m�B	l�B	l�B	o�B	q�B	s�B	v�B	w�B	y�B	z�B	{�B	{�B	{�B	{�B	{�B	� B	�%B	�7B	�=B	�DB	�JB	�JB	�PB	�\B	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�9B	�LB	�LB	�LB	�RB	�wB	��B	��B	B	ŢB	ƨB	ƨB	��B	�NB	��B
JB
�B
#�B
)�B
/B
7LB
9XB
?}B
G�B
N�B
T�B
YB
^5B
bNB
ffB
l�B
p�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BȴBǮBȴBǮBȴBǮBƨBǮBǮBǮBǮBǮBȴBȴBɺBɺBɺB��B��B��B�#B$�BXB�=B��B��B��B��B�FB��B�B�B�/B�;B�TB�`B�`B�mB�fB�mB�fB�mB�mB�B�B�B�B�B�B�B��B��B��B��B��B��B�B�B�`B�`B�)B��B��B��B��B��BĜB�}B��BÖB�RB� BiyB`BBXBW
BS�BQ�BS�BM�B33B�B{B
=B��B�B�B�B�NB��BB�FB��B�=BhsB^5B6FB)�B)�B!�B1B
�B
��B
ĜB
�qB
�FB
�!B
�uB
�VB
�\B
r�B
Q�B
H�B
D�B
A�B
#�B
�B
�B
�B
�B
B	��B	�B	�;B	��B	��B	��B	��B	�B	�B	��B	��B	�?B	�B	��B	�B	�B	�'B	�'B	�'B	��B	�bB	�DB	� B	{�B	y�B	s�B	m�B	m�B	hsB	gmB	ffB	bNB	]/B	VB	Q�B	J�B	C�B	>wB	;dB	,B	 �B	�B	�B	�B	{B	hB	\B	JB	%B	B��B��B�B�B�sB�TB�TB�NB�BB�#B��B��BɺBƨBÖB�wB�dB�RB�FB�'B�!B��B��B��B��B��B��B��B��B�{B�uB�uB�oB�bB�\B�PB�PB�1B�B�B�B�B�B~�By�Bx�Bw�Bu�Bt�Br�Bo�Bo�Bn�Bk�BjBjBhsBiyBdZBaHB`BB`BB^5B\)B[#BYBXBVBT�BS�BR�BR�BO�BN�BM�BL�BL�BK�BJ�BJ�BI�BI�BI�BG�BI�BI�BG�BH�BE�BD�BB�BB�BC�BC�BA�BA�BA�B@�B@�B?}B@�B@�B>wB=qB=qB=qB<jB<jB<jB=qB;dB;dB=qB<jB;dB;dB<jB<jB<jB=qB>wB?}B?}B@�BA�BB�B@�BA�B@�BA�BC�BG�BJ�BK�BM�BP�BQ�BR�BS�BS�BT�BVBW
BZB]/B^5B_;B`BBbNBbNBffBjBk�Bv�B~�B�B�1B�DB�PB�bB�bB�{B��B��B��B��B��B��B��B��B��B�B�!B�?B�RB�RB�XB�^B�dB�jB�^B�^B�dB�jB�jB�}BŢBǮB��B��B��B��B�
B�)B�/B�HB�`B�`B�fB�mB�B�B��B��B	bB	oB	VB	PB	1B	
=B	JB	\B	�B	�B	 �B	�B	�B	 �B	#�B	%�B	)�B	/B	2-B	5?B	7LB	=qB	?}B	B�B	D�B	F�B	H�B	I�B	J�B	J�B	L�B	M�B	M�B	N�B	O�B	S�B	YB	\)B	]/B	^5B	]/B	]/B	aHB	dZB	gmB	jB	m�B	m�B	l�B	l�B	o�B	q�B	t�B	v�B	w�B	y�B	z�B	{�B	|�B	}�B	{�B	|�B	�B	�+B	�7B	�=B	�DB	�JB	�JB	�PB	�\B	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�3B	�?B	�LB	�LB	�LB	�XB	�wB	��B	��B	B	ŢB	ƨB	ƨB	��B	�NB	��B
JB
�B
#�B
)�B
/B
7LB
9XB
?}B
G�B
N�B
T�B
YB
^5B
bNB
ffB
l�B
p�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446592012010314465920120103144659  AO  ARGQ                                                                        20111130140746  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140746  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144659  IP                  G�O�G�O�G�O�                