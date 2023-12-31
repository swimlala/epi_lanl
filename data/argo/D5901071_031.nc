CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:59Z UW 3.1 conversion   
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135747  20190522121825  1727_5046_031                   2C  D   APEX                            2143                            040306                          846 @�=�rX�1   @�=���@7�1&�x��c���S��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0ffB8ffB@  BH  BP  BW33B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Dz331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B33B��B'��B0  B8  B?��BG��BO��BV��B_��Bg��Bo��Bw��B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B˙�B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC|  C}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D  Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D s3D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DO  DOy�DP  DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dms3Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dss3Dz,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�"�A�JA��A��A��;A���Aϛ�AϑhAϋDAχ+AσA�~�A�|�A�z�A�v�A�r�A�p�A�n�A�l�A�jA�ffA�^5A�Q�A�E�A��A�S�A�G�A�9XA�1A���A�VA��hA��TA�9XA�"�A��A���A�K�A���A��FA�ffA�VA���A�`BA���A�E�A�A�v�A�oA���A�z�A��A���A�33A��A��FA���A�hsA�JA�A��+A�E�A��A��;A��A�|�A�O�A�JA���A���A�O�A�/A���A�ĜA���A�z�A�O�A���A���A��FA���A�n�A�I�A�(�A���A��/A���A��FA��uA���A�z�A�oA���A�r�A�$�A���A�I�A��A�bNA��A���A��A���A��DA�;dA��A�p�A��#A�^5A��!A��+A��uA��A���A��7A�XA�{A���A�t�A�%A��A�{A��PA��A�ƨA�-A��wA�`BA� �A�Q�A�bA�Q�A��^A�A�XA��!A�hsA�(�A�`BA�ȴA��A�&�A��-A~n�A|�DA{"�AyAv��Au��At1'Ar�\AlĜAj^5Ag33AdbNAc�^AbȴAa��A`M�A]�
A]&�A]A\�`A\�!A\M�A[VAX��AX�/AX�+AW�wAW\)AVjAU��AU�7AUdZAT��ASK�AQp�AP��AO�ANVAK�FAH��AF��AE;dADE�ABȴA@M�A?hsA>�\A=�A<�HA<�A<1'A;�hA:jA9A8(�A7��A7l�A6�A61'A57LA4I�A2��A2$�A0~�A/hsA.��A-�hA+�
A*��A)ƨA(�9A'/A%|�A$��A$��A$E�A"�A!�A ��An�AJA"�A`BA^5A�FA�A�A��AVA�TAr�A��AK�AA��A~�An�A��A\)AI�A�AoA
~�A	"�A�mAhsA��AAhsA�9A�+Ap�A�PA b@��/@�K�@���@�ff@��h@�o@�z�@�C�@홚@�A�@�%@��@�@�O�@�F@�v�@��@�b@ߕ�@��H@���@�&�@ݑh@�-@��#@ݑh@�?}@��/@�j@��m@�b@�A�@��;@�+@���@�x�@�E�@�S�@�o@�+@ڧ�@�$�@ٲ-@�Z@�o@ҧ�@�+@��;@ӍP@��@���@���@щ7@ϥ�@�C�@Χ�@��@͡�@�/@�I�@��y@ʧ�@��y@�v�@ɉ7@ȓu@�S�@Ƨ�@�@��@��y@�-@�J@��@���@�
=@��H@�v�@�?}@�Q�@�
=@�J@�@���@�I�@�ƨ@��y@���@��@��
@�
=@��H@��\@�n�@�ff@�^5@��@�p�@�V@��D@�I�@��@�@�7L@��`@�b@��
@���@��w@��+@��T@��-@���@�`B@��@�r�@���@�G�@�9X@�33@��@���@���@��+@�v�@�^5@�-@��@��@��-@��D@�o@�$�@�@�x�@���@�z�@� �@�|�@�ȴ@�@�X@�&�@��/@�z�@�1'@��@��F@�K�@��H@���@��#@�G�@� �@��@��P@�"�@�o@�@���@�v�@�E�@�{@�J@�@���@��@��@���@���@�O�@��@��/@��9@��D@��D@���@���@�S�@�C�@���@��!@���@��\@�n�@���@�x�@��9@�Z@� �@�  @�ƨ@��F@���@�K�@��@�
=@�
=@��@��!@�ff@�J@��^@��h@��@���@���@�Q�@�b@��m@��w@�\)@�"�@�o@��y@�ȴ@���@���@��+@�5?@��@�hs@�/@�V@���@��j@��D@�j@�$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�"�A�JA��A��A��;A���Aϛ�AϑhAϋDAχ+AσA�~�A�|�A�z�A�v�A�r�A�p�A�n�A�l�A�jA�ffA�^5A�Q�A�E�A��A�S�A�G�A�9XA�1A���A�VA��hA��TA�9XA�"�A��A���A�K�A���A��FA�ffA�VA���A�`BA���A�E�A�A�v�A�oA���A�z�A��A���A�33A��A��FA���A�hsA�JA�A��+A�E�A��A��;A��A�|�A�O�A�JA���A���A�O�A�/A���A�ĜA���A�z�A�O�A���A���A��FA���A�n�A�I�A�(�A���A��/A���A��FA��uA���A�z�A�oA���A�r�A�$�A���A�I�A��A�bNA��A���A��A���A��DA�;dA��A�p�A��#A�^5A��!A��+A��uA��A���A��7A�XA�{A���A�t�A�%A��A�{A��PA��A�ƨA�-A��wA�`BA� �A�Q�A�bA�Q�A��^A�A�XA��!A�hsA�(�A�`BA�ȴA��A�&�A��-A~n�A|�DA{"�AyAv��Au��At1'Ar�\AlĜAj^5Ag33AdbNAc�^AbȴAa��A`M�A]�
A]&�A]A\�`A\�!A\M�A[VAX��AX�/AX�+AW�wAW\)AVjAU��AU�7AUdZAT��ASK�AQp�AP��AO�ANVAK�FAH��AF��AE;dADE�ABȴA@M�A?hsA>�\A=�A<�HA<�A<1'A;�hA:jA9A8(�A7��A7l�A6�A61'A57LA4I�A2��A2$�A0~�A/hsA.��A-�hA+�
A*��A)ƨA(�9A'/A%|�A$��A$��A$E�A"�A!�A ��An�AJA"�A`BA^5A�FA�A�A��AVA�TAr�A��AK�AA��A~�An�A��A\)AI�A�AoA
~�A	"�A�mAhsA��AAhsA�9A�+Ap�A�PA b@��/@�K�@���@�ff@��h@�o@�z�@�C�@홚@�A�@�%@��@�@�O�@�F@�v�@��@�b@ߕ�@��H@���@�&�@ݑh@�-@��#@ݑh@�?}@��/@�j@��m@�b@�A�@��;@�+@���@�x�@�E�@�S�@�o@�+@ڧ�@�$�@ٲ-@�Z@�o@ҧ�@�+@��;@ӍP@��@���@���@щ7@ϥ�@�C�@Χ�@��@͡�@�/@�I�@��y@ʧ�@��y@�v�@ɉ7@ȓu@�S�@Ƨ�@�@��@��y@�-@�J@��@���@�
=@��H@�v�@�?}@�Q�@�
=@�J@�@���@�I�@�ƨ@��y@���@��@��
@�
=@��H@��\@�n�@�ff@�^5@��@�p�@�V@��D@�I�@��@�@�7L@��`@�b@��
@���@��w@��+@��T@��-@���@�`B@��@�r�@���@�G�@�9X@�33@��@���@���@��+@�v�@�^5@�-@��@��@��-@��D@�o@�$�@�@�x�@���@�z�@� �@�|�@�ȴ@�@�X@�&�@��/@�z�@�1'@��@��F@�K�@��H@���@��#@�G�@� �@��@��P@�"�@�o@�@���@�v�@�E�@�{@�J@�@���@��@��@���@���@�O�@��@��/@��9@��D@��D@���@���@�S�@�C�@���@��!@���@��\@�n�@���@�x�@��9@�Z@� �@�  @�ƨ@��F@���@�K�@��@�
=@�
=@��@��!@�ff@�J@��^@��h@��@���@���@�Q�@�b@��m@��w@�\)@�"�@�o@��y@�ȴ@���@���@��+@�5?@��@�hs@�/@�V@���@��j@��D@�j@�$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�JBjBhsB�B�=B�7B�DB�PB�oB��B��B�?BǮB��B��B�B�B�;B�TB�sB�B��BB+BDBVBoB�B�B!�B#�B&�B'�B)�B,B-B/B0!B0!B0!B1'B1'B1'B1'B2-B2-B33B33B33B49B5?B5?B6FB8RB9XB9XB9XB9XB9XB;dB=qB?}BA�B@�B=qB5?B1'B-B-B)�B$�B�B�BPB1B  B��B��B�B�B�mB�HB�B��BǮB�dB��B��B�JB�%B~�Bz�Br�BcTB49B�B��B�B�sB�B�}B��B�7B� Bk�B[#BS�BD�B(�B�B
��B
�B
�
B
�3B
�{B
y�B
[#B
Q�B
H�B
6FB
(�B
�B
PB	��B	��B	�B	�B	�RB	��B	��B	�bB	�uB	�oB	�1B	�B	~�B	~�B	� B	� B	�B	�B	�B	�B	� B	}�B	{�B	y�B	w�B	u�B	t�B	s�B	p�B	hsB	aHB	]/B	Q�B	C�B	,B	bB��B�B�`B�BƨB��B�dB�}BŢBǮBȴBȴBǮBƨBĜBÖBB��B�}B�wB�jB�dB�RB�'B�B��B��B��B��B��B��B�{B�hB�bB�\B�JB�7B�+B�B�B� B}�B~�Bx�Bs�Bp�Bo�Bm�Bk�BiyBe`BbNBbNBaHBaHBaHB`BB^5B]/B[#BZBYBVBS�BQ�BP�BN�BL�BJ�BI�BG�BB�B>wB:^B8RB5?B33B2-B/B-B,B+B)�B'�B&�B'�B%�B$�B$�B$�B#�B$�B#�B#�B#�B%�B,B6FB9XB8RB@�BC�BE�BG�BN�BR�BXBW
BZB\)Bk�Bw�Bv�Bx�Bz�B}�B~�By�Bm�Bm�Bv�B�B�B�%B�=B�\B�{B��B��B��B��B�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�?B�9B�9B�?B�FB�LB�^B�qB�qB�wB��BÖBĜBŢBŢBƨBƨB��B��B��B��B��B��B��B��B��B��B��B�B�/B�5B�5B�5B�BB�HB�NB�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B	B	B	B	+B	DB	uB	�B	�B	%�B	'�B	'�B	,B	.B	33B	5?B	7LB	;dB	>wB	?}B	@�B	@�B	B�B	F�B	I�B	M�B	N�B	N�B	Q�B	XB	YB	[#B	\)B	\)B	\)B	\)B	\)B	^5B	_;B	bNB	dZB	e`B	hsB	jB	jB	p�B	t�B	u�B	x�B	}�B	�B	�B	�B	�B	�B	�+B	�=B	�DB	�DB	�JB	�\B	�bB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�9B	�?B	�FB	�RB	�RB	�XB	�XB	�dB	�jB	�}B	��B	B	ÖB	ĜB	ƨB	ǮB	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Bq�B�1B�VB�JB�PB�\B�{B��B��B�^BɺB��B��B�B�)B�BB�`B�B��B��BB	7BPBbB{B�B!�B"�B$�B&�B(�B+B-B.B0!B1'B1'B1'B2-B2-B2-B2-B33B49B49B49B49B5?B6FB6FB8RB9XB9XB:^B:^B:^B:^B<jB>wB?}BA�BA�B@�B7LB33B/B.B+B&�B!�B�BVB
=BB��B��B�B�B�yB�TB�/B��B��B��B�B��B�VB�1B� B|�Bv�Bq�B>wB$�B��B��B�B�HB��B��B�DB�%Bo�B\)BXBK�B,B �B  B
��B
�HB
�dB
��B
�B
^5B
T�B
P�B
<jB
-B
#�B
{B
  B	��B	�B	�B	��B	�!B	��B	�uB	��B	��B	�PB	�PB	�B	� B	�B	�B	�B	�1B	�=B	�B	�B	� B	|�B	{�B	y�B	v�B	u�B	u�B	s�B	l�B	bNB	aHB	T�B	J�B	33B	�B	B�B�yB�;BɺBÖB�}B��BƨBɺB��B��B��BɺBƨBĜBĜBÖBB��B��B�qB�qB�9B�B�B�B��B��B��B��B��B�uB�hB�hB�oB�PB�DB�=B�B�B�B�Bz�Bx�Bu�Br�Bo�Bm�Bm�BjBffBcTBbNBbNBaHBbNB`BB`BB]/B\)B[#BZBXBS�BR�BQ�BN�BL�BJ�BK�BH�B>wB?}B;dB;dB6FB49B33B2-B.B.B-B-B(�B(�B'�B&�B&�B&�B%�B%�B$�B%�B$�B%�B+B7LB:^B9XBA�BD�BF�BG�BN�BS�BYBYB[#B[#BjBx�Bv�By�B{�B~�B�B�Bn�Bl�Bu�B�B�B�%B�=B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�FB�?B�?B�FB�LB�XB�jB�wB�wB�wBBÖBĜBŢBƨBǮBǮB��B��B��B��B��B��B��B��B��B��B�
B�#B�5B�5B�;B�;B�HB�TB�mB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B	B	B	B	1B	JB	{B	�B	�B	&�B	(�B	(�B	-B	/B	49B	6FB	8RB	<jB	?}B	A�B	A�B	B�B	C�B	F�B	J�B	M�B	N�B	N�B	R�B	XB	YB	[#B	\)B	\)B	\)B	\)B	\)B	^5B	`BB	bNB	dZB	e`B	hsB	jB	k�B	q�B	u�B	u�B	y�B	~�B	�B	�B	�B	�B	�B	�1B	�DB	�DB	�DB	�JB	�\B	�bB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�9B	�?B	�FB	�RB	�RB	�XB	�^B	�jB	�qB	��B	��B	B	ĜB	ĜB	ƨB	ǮB	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446442012010314464420120103144644  AO  ARGQ                                                                        20111130135747  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135747  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144644  IP                  G�O�G�O�G�O�                