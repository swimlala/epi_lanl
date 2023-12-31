CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:23Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112320  20190522121835  1901_5055_002                   2C  D   APEX                            2140                            040306                          846 @�46:�1   @�46β@ @0�hr�!�c%\(�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@�  @�  @���A   A@  A`  A���A�  A�  A�33A�33A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@ffBH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B���B���B���B���B�  B�33B�  B�  B�  C   C�fC  C  C  C
  C  C�fC  C  C  C�fC  C  C�C�C   C"  C$  C&  C(  C*�C,  C-�fC0  C2  C4�C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ�CL  CM�fCP  CR  CT  CU�fCX�CZ�C\  C^  C`  Cb�Cd  Cf  Ch  Cj�Cl�Cn�Cp  Cr  Ct  Cu�fCx�Cz  C|  C~�C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C��C��C��C��C�  C�  C��3C�  C��C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C��C�  C�  C��3C�  C��C��C�  C�  C��3C�  C��C��C��C��C��C�  C�  C�  C��3C�  C��C��C��C��C��C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3D � D  D� D  D� D��D� D  Dy�D  D�fD  Dy�D��Dy�D��D� D	fD	�fD
  D
� DfD� D  D�fD  Dy�D  D�fD��Dy�D  D� D  D� DfD�fD  D� D  D� D  D� DfDy�D  D� D  D� D  D� D  D� D  D�fD  Dy�D��Dy�D��Dy�D  D� D   D � D ��D!y�D"  D"�fD#fD#� D$  D$� D%  D%� D&  D&y�D&��D'y�D(  D(�fD)fD)� D*  D*y�D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/y�D0  D0� D1  D1y�D2  D2� D3fD3� D4  D4� D5  D5y�D6  D6� D6��D7� D8fD8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D=��D>� D?  D?� D?��D@� DAfDA� DB  DBy�DC  DC�fDDfDD�fDEfDE�fDFfDF�fDG  DGy�DG��DH� DI  DIy�DJ  DJ� DK  DK� DK��DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DRy�DS  DS� DT  DT� DT��DU� DV  DV� DWfDW� DX  DX� DYfDY� DZ  DZ� D[fD[� D\  D\y�D]  D]�fD^  D^� D_  D_� D`  D`�fDa  Day�Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg�fDh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du�fDv  Dv� DyffD�#3D�FfD�l�D�� D�� D�33D�ffD���D��3D�)�D�` Dǩ�D��fD�9�Dڀ D�� D���D�3D�Y�D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @,��@y��@���@���AffA>ffA^ffA�  A�33A�33A�ffA�ffA�33A�33A�33A�ffB��B��B��B��B'��B/��B7��B@  BG��BO��BX  B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�B���B���B���B���B���Bۙ�Bߙ�B㙚B癚B���B�  B���B���B���B���C��C�fC�fC�fC	�fC�fC��C�fC�fC�fC��C�fC�fC  C  C�fC!�fC#�fC%�fC'�fC*  C+�fC-��C/�fC1�fC4  C5�fC7�fC9�fC;�fC>  C?�fCA�fCC�fCE�fCG�fCJ  CK�fCM��CO�fCQ�fCS�fCU��CX  CZ  C[�fC]�fC_�fCb  Cc�fCe�fCg�fCj  Cl  Cn  Co�fCq�fCs�fCu��Cx  Cy�fC{�fC~  C�fC��3C��3C��fC��3C�  C��3C��fC��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��fC��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��fC��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��fC��3C�  C��3C��3C��fC��3C�  C��3C��fC��3C��3C��3C��3C�  C��3C��3C��fC��3C�  C�  C��3C��3C��fC��3C�  C�  C�  C�  C�  C��3C��3C��3C��fC��3C�  C�  C�  C�  C�  C�  C��3C��fC��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��fD y�D ��Dy�D��Dy�D�3Dy�D��Ds3D��D� D��Ds3D�3Ds3D�3Dy�D	  D	� D	��D
y�D  Dy�D��D� D��Ds3D��D� D�3Ds3D��Dy�D��Dy�D  D� D��Dy�D��Dy�D��Dy�D  Ds3D��Dy�D��Dy�D��Dy�D��Dy�D��D� D��Ds3D�3Ds3D�3Ds3D��Dy�D��D y�D �3D!s3D!��D"� D#  D#y�D#��D$y�D$��D%y�D%��D&s3D&�3D's3D'��D(� D)  D)y�D)��D*s3D*��D+y�D+��D,� D,��D-y�D-��D.y�D.��D/s3D/��D0y�D0��D1s3D1��D2y�D3  D3y�D3��D4y�D4��D5s3D5��D6y�D6�3D7y�D8  D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=�3D>y�D>��D?y�D?�3D@y�DA  DAy�DA��DBs3DB��DC� DD  DD� DE  DE� DF  DF� DF��DGs3DG�3DHy�DH��DIs3DI��DJy�DJ��DKy�DK�3DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRs3DR��DSy�DS��DTy�DT�3DUy�DU��DVy�DW  DWy�DW��DXy�DY  DYy�DY��DZy�D[  D[y�D[��D\s3D\��D]� D]��D^y�D^��D_y�D_��D`� D`��Das3Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dg� Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Du� Du��Dvy�Dy` D�  D�C3D�i�D���D���D�0 D�c3D���D�� D�&fD�\�DǦfD��3D�6fD�|�D��D�ٚD� D�VfD�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��mA��#A�ĜAծAթ�Aե�Aգ�A՟�A՝�A՛�Aՙ�Aՙ�A՗�A՗�AՕ�AՕ�AՓuAՑhAՑhAՕ�AՓuAՕ�A՗�AՕ�AՓuAՓuAՏ\AՉ7AՃAՃA�~�A�x�A�r�A�n�A�l�A�hsA�dZA�ZA�K�A�5?A���A�$�A�?}A�z�A�t�A�n�A���A���A�JA�ȴA���A�$�A���A�
=A���A�n�A�O�A��A��A��A�+A��uA�VA�A�A��
A�x�A��A�dZA�n�A�G�A�bNA�C�A�XA�{A�bA���A�VA�E�A�"�A�ffA�z�A�/A�t�A���A�VA��A��A���A�{A�p�A��HA��A��RA��A}�^Aw��AshsAp��An�HAlr�Ai��AgXAc�wA^�9A[G�AWC�AS|�AQAP�ANAJVAH��AF9XAEAD��AC�AB�DAB{A@�DA?l�A=ƨA<I�A;��A9A8E�A7;dA5�A3�hA1�A0bNA0bNA.��A,Q�A*ZA(I�A'C�A&ĜA%�A$E�A"��A!`BA bA33A;dA�`AA7LA7LAȴAoAA�A�-A�DA�A"�AQ�A^5A�A�/A�
Ap�A%A33A|�A�A��Ax�A�HA�mA?}A&�AM�AC�Az�A��A?}AȴA\)A��A
^5A
�uA
�A
��A
jA	��A	"�AS�A�jAr�AffAI�A�
AVAr�A �A~�AZA�A��A�hA�HA��A��A^5A5?A��A ��A ��@���@���@��-@��@���@�@�J@�$�@��#@�x�@���@���@���@���@�V@��7@��@�1@�dZ@�ff@�-@�9@��@@�33@�+@�Ĝ@ꗍ@�^@���@�@��H@�^@���@�|�@��@�bN@�|�@�@�7L@�&�@���@܋D@��@�\)@�"�@�ȴ@�J@ى7@���@�r�@���@���@�^5@��T@�O�@���@�9X@���@�t�@���@�ff@��@���@Ѳ-@�/@Ѓ@�9X@�b@Ͼw@��H@��@�X@���@�r�@�ƨ@��@�^5@�J@���@ɲ-@ɩ�@ə�@�x�@ȣ�@ǥ�@�"�@�v�@�@ũ�@��@�j@�bN@�A�@��m@Ý�@�t�@�o@\@��@��@��9@���@�l�@�33@�@�^5@��@�&�@���@�bN@��P@�;d@��@��R@��\@�=q@���@��@���@���@�Q�@�I�@�9X@� �@�(�@���@�ȴ@�$�@��@�J@�7L@��/@��D@�I�@���@�l�@��R@�v�@���@���@��+@��@�`B@�V@��j@���@��u@���@�r�@�1'@��F@�@�~�@�-@���@���@�O�@���@��9@�A�@���@�|�@�S�@�33@��@�"�@�+@��y@��\@�E�@�@��^@���@�G�@���@�1'@��@�"�@�M�@��T@���@�7L@�r�@��;@�\)@�+@��H@�~�@�-@�J@�V@��#@�x�@�%@�j@�I�@�1'@��w@���@�S�@���@���@���@�5?@��7@�?}@�/@�V@��9@�Z@�(�@��P@�l�@�l�@�
=@��\@�M�@���@���@��7@��@��9@��9@��@� �@��;@��w@�o@��!@���@��!@���@��+@�~�@��\@�ff@�$�@���@���@�O�@�V@���@�Q�@���@��;@��@�l�@�+@�@��@��!@��+@�M�@�$�@��@���@�hs@�G�@�/@��@�V@���@��@�z�@�j@�(�@���@��w@���@�l�@�C�@��y@�v�@�-@��T@�@���@�`B@�X@�7L@�%@��@�b@�@��@y��@n$�@d��@_;d@U�T@N@E��@>��@81'@1G�@+S�@$��@�y@�\@�-@hs@O�@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��mA��#A�ĜAծAթ�Aե�Aգ�A՟�A՝�A՛�Aՙ�Aՙ�A՗�A՗�AՕ�AՕ�AՓuAՑhAՑhAՕ�AՓuAՕ�A՗�AՕ�AՓuAՓuAՏ\AՉ7AՃAՃA�~�A�x�A�r�A�n�A�l�A�hsA�dZA�ZA�K�A�5?A���A�$�A�?}A�z�A�t�A�n�A���A���A�JA�ȴA���A�$�A���A�
=A���A�n�A�O�A��A��A��A�+A��uA�VA�A�A��
A�x�A��A�dZA�n�A�G�A�bNA�C�A�XA�{A�bA���A�VA�E�A�"�A�ffA�z�A�/A�t�A���A�VA��A��A���A�{A�p�A��HA��A��RA��A}�^Aw��AshsAp��An�HAlr�Ai��AgXAc�wA^�9A[G�AWC�AS|�AQAP�ANAJVAH��AF9XAEAD��AC�AB�DAB{A@�DA?l�A=ƨA<I�A;��A9A8E�A7;dA5�A3�hA1�A0bNA0bNA.��A,Q�A*ZA(I�A'C�A&ĜA%�A$E�A"��A!`BA bA33A;dA�`AA7LA7LAȴAoAA�A�-A�DA�A"�AQ�A^5A�A�/A�
Ap�A%A33A|�A�A��Ax�A�HA�mA?}A&�AM�AC�Az�A��A?}AȴA\)A��A
^5A
�uA
�A
��A
jA	��A	"�AS�A�jAr�AffAI�A�
AVAr�A �A~�AZA�A��A�hA�HA��A��A^5A5?A��A ��A ��@���@���@��-@��@���@�@�J@�$�@��#@�x�@���@���@���@���@�V@��7@��@�1@�dZ@�ff@�-@�9@��@@�33@�+@�Ĝ@ꗍ@�^@���@�@��H@�^@���@�|�@��@�bN@�|�@�@�7L@�&�@���@܋D@��@�\)@�"�@�ȴ@�J@ى7@���@�r�@���@���@�^5@��T@�O�@���@�9X@���@�t�@���@�ff@��@���@Ѳ-@�/@Ѓ@�9X@�b@Ͼw@��H@��@�X@���@�r�@�ƨ@��@�^5@�J@���@ɲ-@ɩ�@ə�@�x�@ȣ�@ǥ�@�"�@�v�@�@ũ�@��@�j@�bN@�A�@��m@Ý�@�t�@�o@\@��@��@��9@���@�l�@�33@�@�^5@��@�&�@���@�bN@��P@�;d@��@��R@��\@�=q@���@��@���@���@�Q�@�I�@�9X@� �@�(�@���@�ȴ@�$�@��@�J@�7L@��/@��D@�I�@���@�l�@��R@�v�@���@���@��+@��@�`B@�V@��j@���@��u@���@�r�@�1'@��F@�@�~�@�-@���@���@�O�@���@��9@�A�@���@�|�@�S�@�33@��@�"�@�+@��y@��\@�E�@�@��^@���@�G�@���@�1'@��@�"�@�M�@��T@���@�7L@�r�@��;@�\)@�+@��H@�~�@�-@�J@�V@��#@�x�@�%@�j@�I�@�1'@��w@���@�S�@���@���@���@�5?@��7@�?}@�/@�V@��9@�Z@�(�@��P@�l�@�l�@�
=@��\@�M�@���@���@��7@��@��9@��9@��@� �@��;@��w@�o@��!@���@��!@���@��+@�~�@��\@�ff@�$�@���@���@�O�@�V@���@�Q�@���@��;@��@�l�@�+@�@��@��!@��+@�M�@�$�@��@���@�hs@�G�@�/@��@�V@���@��@�z�@�j@�(�@���@��w@���@�l�@�C�@��y@�v�@�-@��T@�@���@�`B@�X@�7L@�%@��@�b@�@��@y��@n$�@d��@_;d@U�T@N@E��@>��@81'@1G�@+S�@$��@�y@�\@�-@hs@O�@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
]/B
^5B
_;B
]/B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
_;B
^5B
^5B
^5B
^5B
]/B
]/B
]/B
\)B
\)B
[#B
ZB
YB
W
B
R�B
J�B
33B
�B
!�B
+B
�B
/B
q�B
�'B
ȴB
�mB  B
��B
�mB
�NB
��BB
=B
��BoB�B"�B�B�B�BhB�BuBDBB+BB
��BJB�B%B
��B
�TB
�mB
��B
�B
�%B
t�B
gmB
[#B
C�B
'�B
+B	�B	�#B	��B	�}B	�FB	��B	�B	hsB	T�B	D�B	8RB	)�B	�B	oB	B�B�fB�BÖBŢB�}B�?B��B��B��B��B��B��B��B�{B��B��B��B��B�bB�=B�B�B�%B�1B�1B�JB�uB��B�uB�7B�=B�JB�hB�uB�uB�hB�bB��B��B�'B�9B�qB��B��B��B��B��B��B�#B��B	%B	JB	�B	%�B	(�B	2-B	9XB	8RB	?}B	I�B	XB	_;B	dZB	jB	gmB	dZB	_;B	^5B	ZB	W
B	S�B	P�B	N�B	H�B	I�B	D�B	M�B	W
B	aHB	^5B	[#B	VB	M�B	N�B	R�B	S�B	VB	W
B	W
B	W
B	]/B	ffB	jB	l�B	n�B	q�B	t�B	�B	�B	�B	�B	�B	�B	� B	v�B	p�B	iyB	p�B	v�B	�B	�1B	�JB	�\B	�VB	�JB	�DB	�7B	�1B	�1B	�7B	�=B	�JB	�DB	�PB	�JB	�7B	�+B	�+B	�+B	�%B	�B	�+B	�%B	�%B	�B	�%B	�1B	�+B	�B	�B	�B	�B	� B	� B	�B	�B	�DB	�PB	�bB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�?B	�LB	�XB	�^B	�XB	�RB	�RB	�dB	�jB	�jB	�wB	�qB	�qB	�wB	��B	��B	��B	B	ÖB	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�5B	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�ZB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
1B
	7B
	7B
	7B
	7B

=B

=B
DB
JB
JB
PB
PB
PB
PB
VB
VB
\B
\B
bB
bB
bB
hB
hB
hB
oB
oB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
%�B
.B
/B
49B
C�B
F�B
M�B
VB
YB
_;B
cTB
hsB
m�B
q�B
u�B
z�B
� B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
^5B
_;B
_;B
^5B
]/B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
_;B
^5B
^5B
^5B
^5B
]/B
]/B
]/B
\)B
\)B
[#B
ZB
ZB
YB
YB
dZB
YB
2-B
D�B
F�B
'�B
C�B
� B
�LB
�
B
��B\B
��B
�B
�BDB�B�B
=B�B(�B33B-B)�B$�B�B �B$�B#�B�B{BPBB�B)�BoBPB
��BB
��B
B
�bB
�B
y�B
k�B
T�B
D�B
�B
JB	�B	�HB	�)B	�)B	�wB	��B	�B	hsB	S�B	L�B	A�B	5?B	1'B	&�B	hB	B�B��B�B��B��B�RB�!B��B��B��B��B��B��B��B��B��B��B��B��B�bB�oB�{B��B�oB�hB��B�B��B��B�oB�oB��B��B��B��B��B��B��B�?B�^BB��B�#B�;B�
B��B��B�B��B	
=B	DB	oB	)�B	/B	5?B	;dB	7LB	=qB	G�B	ZB	cTB	iyB	r�B	n�B	q�B	ffB	e`B	`BB	\)B	YB	W
B	ZB	R�B	P�B	C�B	M�B	XB	e`B	cTB	cTB	aHB	Q�B	P�B	S�B	VB	ZB	]/B	[#B	XB	\)B	hsB	m�B	n�B	q�B	u�B	u�B	�B	�1B	�%B	�1B	�+B	�%B	�7B	~�B	x�B	iyB	q�B	u�B	� B	�1B	�PB	�hB	�hB	�\B	�PB	�JB	�DB	�DB	�JB	�VB	�\B	�\B	�bB	�bB	�JB	�7B	�7B	�DB	�PB	�=B	�DB	�=B	�DB	�7B	�DB	�JB	�PB	�JB	�1B	�+B	�+B	�B	�B	�B	�+B	�PB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�'B	�FB	�RB	�^B	�^B	�XB	�RB	�jB	�wB	�wB	�}B	��B	�wB	�qB	�wB	��B	B	B	ÖB	ĜB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�#B	�5B	�BB	�HB	�BB	�NB	�TB	�TB	�NB	�ZB	�ZB	�TB	�ZB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
1B

=B

=B

=B
	7B
DB

=B
JB
PB
PB
PB
VB
VB
VB
\B
\B
bB
bB
bB
hB
hB
oB
oB
hB
oB
oB
oB
uB
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
&�B
.B
/B
49B
C�B
F�B
N�B
VB
YB
_;B
cTB
hsB
m�B
q�B
u�B
z�B
� B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���=�P<�/=C�<�/<�C�<��
<e`B<#�
<e`B<u<u<#�
<#�
<e`B<u<���<�C�<49X<49X<#�
<�o<e`B<�o<e`B<#�
<49X<�C�<ě�<��
<T��<#�
<#�
<T��<�o<D��<�j<���<�/=#�
<�j<#�
<D��<�t�<�o<�C�<�`B<�j=o<��
<�1<�`B=�P=C�=�P<�`B<���<u<��
<�j<�1<�=\)<�h<�`B<ě�<u<u<�1<ě�<u<u<#�
<#�
<49X<#�
<#�
<D��<#�
<T��<49X<#�
<e`B<49X<49X<u<e`B<T��<#�
<#�
<u<��
<u<T��<#�
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
<T��<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250072012011312500720120113125007  AO  ARGQ                                                                        20111205112320  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112320  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125007  IP                  G�O�G�O�G�O�                