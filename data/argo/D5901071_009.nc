CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:53Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               	A   AO  20111130135521  20190522121825  1727_5046_009                   2C  D   APEX                            2143                            040306                          846 @�"�� 1   @�"�W:�@7�C��%�c�Z�11   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @,��@�  @�  A   A   A@  A^ffA�  A�  A�  A�  A���A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  BpffBx  B�  B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B���B���B�  B�33B�  B�  B�  B�  B�  B�  B�33B�33B���B���B�  C   C  C�fC�fC  C
  C  C  C�C  C  C  C�C�C�fC  C   C"  C$  C&�C(  C*  C,�C-�fC/�fC2  C4  C6  C7�fC9�fC<  C>  C?�fCB  CD  CE�fCG�fCJ  CL  CN  CO�fCQ�fCT  CV  CW�fCZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv�Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C��3C��C��C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C��C��C�  C�  C��3C�  C��C�  C��3C�  C��C�  C�  C��3C�  C�  C�  C��C��3C��3C��3C�  C�  C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� DfD� D  D� D��D� D  D� D  Dy�D  D� DfD�fD	fD	�fD
fD
� D
��Dy�D��D� D  D� DfD�fDfD�fD  D� D  D� DfD� D��Dy�D  D� D  D� D  Dy�D  D� DfD�fD��Dy�D  D� D  D� D  D� D��D� D  D� D  D� D fD �fD ��D!y�D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'  D'� D(  D(�fD)fD)� D)��D*y�D+  D+� D,  D,y�D-  D-� D-��D.� D.��D/y�D0fD0� D1  D1� D2  D2� D2��D3� D4  D4� D5  D5� D6  D6� D7  D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D?  D?� D@  D@�fDAfDA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DOy�DO��DPy�DQ  DQ�fDRfDR�fDS  DS� DTfDT� DT��DU� DV  DV� DW  DW� DX  DXy�DX��DYy�DZ  DZ� D[fD[� D\  D\�fD]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo�fDp  Dpy�Dp��Dq� Dr  Dr� DsfDsy�Dy��D�)�D�` D��3D���D�,�D�S3D���D�� D�#3D�ffD���D��D�,�D�VfDڌ�D���D��D�L�D�fD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@&ff@y��@���@���AffA>ffA\��A~ffA�33A�33A�33A�  A�33A�33A�33A�33B��B��B33B��B'��B/��B7��B?��BH  BO��BW��B_��Bg��Bp  Bw��B��B�  B���B���B�  B���B���B���B���B���B���B���B���B�  B�  B���B���BÙ�BǙ�B���B�  B���B���B���B���B���B���B�  B�  B�B���B���B���C�fC��C��C�fC	�fC�fC�fC  C�fC�fC�fC  C  C��C�fC�fC!�fC#�fC&  C'�fC)�fC,  C-��C/��C1�fC3�fC5�fC7��C9��C;�fC=�fC?��CA�fCC�fCE��CG��CI�fCK�fCM�fCO��CQ��CS�fCU�fCW��CY�fC[�fC]�fC`  Ca�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq��Cs�fCv  Cw�fCy�fC{�fC}�fC�fC��3C��fC��3C��3C��fC��fC��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��fC��3C��3C��3C��3C��3C��3C��fC��fC�  C�  C��3C��3C��3C��fC��3C�  C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C�  C��3C��fC��fC��3C�  C�  C��3C��3C��fC��3C�  C��3C��fC��3C�  C��3C��3C��fC��3C��3C��3C�  C��fC��fC��fC��3C��3C��3C��3C��fC�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C�  C��3C��3C�  C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��fC��fC��fC�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fD y�D ��Dy�D  Dy�D��Dy�D�3Dy�D��Dy�D��Ds3D��Dy�D  D� D	  D	� D
  D
y�D
�3Ds3D�3Dy�D��Dy�D  D� D  D� D��Dy�D��Dy�D  Dy�D�3Ds3D��Dy�D��Dy�D��Ds3D��Dy�D  D� D�3Ds3D��Dy�D��Dy�D��Dy�D�3Dy�D��Dy�D��Dy�D   D � D �3D!s3D!��D"y�D"��D#y�D#��D$y�D%  D%y�D%��D&y�D&��D'y�D'��D(� D)  D)y�D)�3D*s3D*��D+y�D+��D,s3D,��D-y�D-�3D.y�D.�3D/s3D0  D0y�D0��D1y�D1��D2y�D2�3D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>��D?y�D?��D@� DA  DAy�DA��DBy�DB��DCy�DC��DDy�DE  DEy�DE��DFy�DF��DGs3DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DM  DMy�DM��DNy�DN��DOs3DO�3DPs3DP��DQ� DR  DR� DR��DSy�DT  DTy�DT�3DUy�DU��DVy�DV��DWy�DW��DXs3DX�3DYs3DY��DZy�D[  D[y�D[��D\� D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�De  Dey�De�3Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djs3Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Do  Do� Do��Dps3Dp�3Dqy�Dq��Dry�Ds  Dss3Dy�3D�&fD�\�D�� D��fD�)�D�P D���D���D�  D�c3D��fD��fD�)�D�S3Dډ�D�ٚD�fD�I�D�3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�ĜA�ĜA�ĜA�ĜA˾wA�r�AƲ-A���A�ȴA�Q�AA�A��TA�9XA��A��A��FA�XA�ffA�-A�hsA�G�A���A�5?A�?}A�G�A�+A��mA���A�E�A��A���A�$�A�&�A��RA� �A��A��DA��A�p�A�E�A�t�A�G�A���A�`BA��/A�E�A��-A��A�dZA���A��A���A��`A��PA�9XA��mA�bA�r�A�$�A�S�A�ĜA��A��TA�G�A���A���A��9A��+A�  A�\)A�JA�r�A�x�A��!A��A�p�A�%A�S�A�(�A�bNA�A��HA��RA��A�+A��A��A��A�bA��mA�A��A�p�A�=qA��A���A�^5A�I�A�;dA�9XA�Q�A��A�1A���A��HA���A�C�A�{A�  A}S�Az��AyAw�-As�7Aq�hAq�ApVAl^5Aj�Aj{Ah��AgK�AfQ�AfM�AfZAf=qAc�Aa%A^��A\JAZ  AXz�AV��AU��AR��AO�TAN��AM�AK��AI`BAH1AEdZAD��AC��AC33AB��AA�TAA"�A@9XA>r�A<��A;p�A:-A9�A7��A6��A6 �A5�A5"�A4~�A3�#A3
=A2�A1\)A0��A/�A.z�A-�FA-�A+�
A*E�A)�A(�\A'�
A'K�A&��A&�!A&I�A%�hA$�A$I�A#��A"�A"ZA"{A!��A!;dA��A��A�;A��A{A�A"�A~�A�A1'A��A�A=qAXA��A�TAt�AC�A�A�HA��AAXA-A|�A�+A/A�AO�A
�RA	�^A	+Az�A  AhsA��A9XA33A-A�yA�!A��A r�A $�@�dZ@�z�@��!@��^@��@��@��@�ff@�(�@�5?@�x�@�Q�@�+@�v�@��`@�+@��@旍@�^@���@�  @�;d@�\@���@�G�@�z�@�t�@���@�n�@���@�r�@�ƨ@�C�@���@�~�@ٲ-@ى7@ش9@���@�"�@�n�@�z�@�1@��@��#@���@У�@�C�@��H@�~�@���@�p�@���@ˍP@�\)@ʇ+@ɩ�@Ɨ�@�  @Õ�@�;d@�=q@���@�M�@§�@���@��@�t�@�;d@��@���@�n�@���@���@�1'@��m@��!@�=q@���@�z�@��@�v�@�^5@�@�O�@�/@�%@��@���@�Ĝ@��j@�z�@��P@�n�@�1@���@��@���@���@�Z@��w@��w@��H@��@��@���@���@���@�E�@���@���@�G�@��@��/@�bN@���@�;d@�o@�
=@��H@��@���@���@�-@���@�{@�{@���@�7L@��j@�Ĝ@�/@��9@�~�@�M�@���@�33@��H@��\@�M�@�5?@�M�@�=q@�@��@��@��9@�A�@�b@��@�S�@�"�@��@��@�^5@�hs@�O�@�/@�Ĝ@���@��@�n�@��@��-@��h@�hs@�z�@���@���@�C�@��@��+@�@��^@���@�x�@�X@�7L@�V@���@���@��@�Z@��D@��D@�Q�@��m@���@���@��P@��P@���@��j@���@��;@�  @�%@�p�@��@��h@��@���@�$�@�r�@�1@�ƨ@�^5@��-@�=q@��@��`@���@��@���@�n�@��@��7@�x�@�`B@�`B@�O�@�?}@�/@�/@�&�@��@�%@�%@��@�Ĝ@�r�@�I�@�1@��w@���@��P@��@��H@�^5@�@��@��7@��@�9X@��F@��@�S�@�ȴ@�$�@��T@�-@���@�ȴ@��@�V@w�@kt�@`  @\9X@W��@T(�@N�R@I�#@@��@:J@2n�@-/@&V@  �@@��@��@+@
�H@  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�ĜA�ĜA�ĜA�ĜA˾wA�r�AƲ-A���A�ȴA�Q�AA�A��TA�9XA��A��A��FA�XA�ffA�-A�hsA�G�A���A�5?A�?}A�G�A�+A��mA���A�E�A��A���A�$�A�&�A��RA� �A��A��DA��A�p�A�E�A�t�A�G�A���A�`BA��/A�E�A��-A��A�dZA���A��A���A��`A��PA�9XA��mA�bA�r�A�$�A�S�A�ĜA��A��TA�G�A���A���A��9A��+A�  A�\)A�JA�r�A�x�A��!A��A�p�A�%A�S�A�(�A�bNA�A��HA��RA��A�+A��A��A��A�bA��mA�A��A�p�A�=qA��A���A�^5A�I�A�;dA�9XA�Q�A��A�1A���A��HA���A�C�A�{A�  A}S�Az��AyAw�-As�7Aq�hAq�ApVAl^5Aj�Aj{Ah��AgK�AfQ�AfM�AfZAf=qAc�Aa%A^��A\JAZ  AXz�AV��AU��AR��AO�TAN��AM�AK��AI`BAH1AEdZAD��AC��AC33AB��AA�TAA"�A@9XA>r�A<��A;p�A:-A9�A7��A6��A6 �A5�A5"�A4~�A3�#A3
=A2�A1\)A0��A/�A.z�A-�FA-�A+�
A*E�A)�A(�\A'�
A'K�A&��A&�!A&I�A%�hA$�A$I�A#��A"�A"ZA"{A!��A!;dA��A��A�;A��A{A�A"�A~�A�A1'A��A�A=qAXA��A�TAt�AC�A�A�HA��AAXA-A|�A�+A/A�AO�A
�RA	�^A	+Az�A  AhsA��A9XA33A-A�yA�!A��A r�A $�@�dZ@�z�@��!@��^@��@��@��@�ff@�(�@�5?@�x�@�Q�@�+@�v�@��`@�+@��@旍@�^@���@�  @�;d@�\@���@�G�@�z�@�t�@���@�n�@���@�r�@�ƨ@�C�@���@�~�@ٲ-@ى7@ش9@���@�"�@�n�@�z�@�1@��@��#@���@У�@�C�@��H@�~�@���@�p�@���@ˍP@�\)@ʇ+@ɩ�@Ɨ�@�  @Õ�@�;d@�=q@���@�M�@§�@���@��@�t�@�;d@��@���@�n�@���@���@�1'@��m@��!@�=q@���@�z�@��@�v�@�^5@�@�O�@�/@�%@��@���@�Ĝ@��j@�z�@��P@�n�@�1@���@��@���@���@�Z@��w@��w@��H@��@��@���@���@���@�E�@���@���@�G�@��@��/@�bN@���@�;d@�o@�
=@��H@��@���@���@�-@���@�{@�{@���@�7L@��j@�Ĝ@�/@��9@�~�@�M�@���@�33@��H@��\@�M�@�5?@�M�@�=q@�@��@��@��9@�A�@�b@��@�S�@�"�@��@��@�^5@�hs@�O�@�/@�Ĝ@���@��@�n�@��@��-@��h@�hs@�z�@���@���@�C�@��@��+@�@��^@���@�x�@�X@�7L@�V@���@���@��@�Z@��D@��D@�Q�@��m@���@���@��P@��P@���@��j@���@��;@�  @�%@�p�@��@��h@��@���@�$�@�r�@�1@�ƨ@�^5@��-@�=q@��@��`@���@��@���@�n�@��@��7@�x�@�`B@�`B@�O�@�?}@�/@�/@�&�@��@�%@�%@��@�Ĝ@�r�@�I�@�1@��w@���@��P@��@��H@�^5@�@��@��7@��@�9X@��F@��@�S�@�ȴ@�$�@��T@�-@���@�ȴ@��@�V@w�@kt�@`  @\9X@W��@T(�@N�R@I�#@@��@:J@2n�@-/@&V@  �@@��@��@+@
�H@  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBr�Bq�Bq�Bp�Bn�BjBbNB�1B�uB��B�9B�B��BdZB6FBO�B�DB�RB�dB��B�bBbNBXB]/BcTBo�Bv�B�B�hB�oB�bBĜB��BÖB�LBɺB��BǮB��B��B�
B�B�B�sB�sB�yB�mB�TB�fB�fB�#B�NB�ZB�NB�#B�
B�
B�B��BƨB�jB�-B��B��B��B�oB�1Bu�B`BBE�B:^B0!B)�B�BVB��B�ZB�
B��BɺB��B��B��B��B��B�JB{�Br�BcTBXBYBYBH�B,B�BVB
��B
�B
�`B
��B
��B
�!B
��B
��B
�hB
�B
q�B
`BB
F�B
49B
$�B
\B	��B	�B	�HB	��B	�!B	�B	��B	�\B	�7B	�B	t�B	n�B	m�B	o�B	p�B	l�B	`BB	K�B	;dB	,B	�B	�B	bB	B�B�;B�B��BɺB��B�qB�XB�RB�?B�3B�'B�B�B��B��B��B��B��B��B�uB�hB�bB�VB�PB�DB�=B�%B�bB�uB�oB�PB�7B�7B�+B�B�B� B�B�B�B� B}�B}�B}�B|�B|�B|�B|�B}�B}�B|�B{�B|�Bz�By�By�Bw�Bv�Bt�Br�Bp�Bn�Bn�Bm�Bk�BjBiyBiyBiyBhsBgmBffBdZBbNB`BB^5B\)BZBYBYBXBVBVBT�BT�BS�BR�BQ�BO�BO�BN�BO�BL�BK�BK�BJ�BH�BH�BG�BG�BE�BE�BD�BC�BC�BC�BB�BB�BC�BB�BC�BC�BD�BH�BH�BI�BJ�BL�BM�BN�BN�BO�BP�BQ�BQ�BQ�BT�BW
BXBXBXBZBZBZBZB[#BZB\)B[#B]/BaHBhsBhsBk�Bo�Bp�Bq�Bt�Bu�Bu�Bz�B�B�B}�B}�B�B�B�B�B�+B�DB�VB�=B�7B�=B�DB�PB�\B�hB�uB��B��B��B��B��B��B�B�?B�^BÖBǮBȴBɺB��B��B��BɺBȴBȴBŢB�}B�dB�^B�dB�dB�qBĜBɺB��B��B��B��B��B��B��B�B�#B�5B�;B�BB�`B�B�B�B�B��B��B	B	%B		7B	DB	VB	uB	�B	�B	�B	�B	 �B	!�B	 �B	"�B	)�B	/B	1'B	0!B	0!B	5?B	6FB	6FB	5?B	;dB	=qB	>wB	@�B	@�B	A�B	B�B	C�B	C�B	C�B	C�B	I�B	I�B	J�B	K�B	N�B	VB	W
B	YB	[#B	\)B	]/B	bNB	cTB	cTB	dZB	ffB	ffB	iyB	jB	jB	k�B	l�B	m�B	m�B	n�B	p�B	r�B	u�B	w�B	z�B	z�B	{�B	|�B	|�B	|�B	|�B	z�B	x�B	y�B	z�B	~�B	�+B	�JB	�PB	�PB	�PB	�=B	�B	�B	�B	~�B	~�B	~�B	�+B	�=B	�7B	�=B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�?B	�LB	�^B	�dB	�jB	�qB	�}B	�}B	��B	��B	��B	�}B	�qB	�qB	�qB	�jB	�dB	�dB	�jB	��B	ĜB	ƨB	ǮB	�
B	�yB	��B
B
PB
 �B
/B
8RB
<jB
C�B
H�B
M�B
Q�B
YB
[#B
`BB
e`B
k�B
n�B
s�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Br�Bq�Bq�Bp�Bo�Bs�B�1B��B��B��B�jB�LB�9Bx�B5?BG�B�1B�wBƨB�RB��Bl�B]/BbNBcTBo�Bx�B�%B�{B��B�uBȴB�/B��B�jB��B��BǮB��B��B�#B�HB�/B�B�B�B�B�B�B�B�ZB�sB�B�B�yB�)B�/B�BB�#B�BƨB�RB�B��B��B��B�{B�Bl�BL�BA�B6FB49B,B�B\B�B�;B�HB�HB�-B��B��B��B��B��B�B}�BiyBZB_;BffBT�B5?B0!B�B
=B
��B
��B
�NB
��B
�dB
�B
��B
��B
�hB
�B
v�B
VB
D�B
6FB
�B
+B
B	��B	��B	�FB	�LB	�dB	��B	�bB	�=B	|�B	s�B	n�B	q�B	w�B	~�B	t�B	\)B	M�B	:^B	+B	$�B	�B	�B	  B�sB�HB�BB�B��B��B�wB�wB�^B�RB�LB�9B�9B�-B�B��B��B��B��B��B��B�{B�uB�oB�bB�bB�DB��B��B��B�oB�\B�\B�bB�PB�1B�+B�%B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�B�B�B}�B{�Bz�Bz�B{�Bz�Bv�Bp�Bq�Br�Bp�Bn�Bl�Bk�BjBhsBiyBiyBhsBhsBdZBdZBcTBaHB_;B^5B\)B[#BZBYBYBYBW
BXBXBW
BS�BT�BS�BO�BO�BR�BN�BK�BJ�BI�BI�BI�BK�BI�BG�BF�BG�BG�BI�BJ�BJ�BM�BL�BK�BM�BM�BO�BP�BQ�BR�BS�BP�BS�BT�BW
BT�BYBZBZB[#BZB]/B]/BZB[#BZB\)B[#BaHBaHBhsBhsBm�Bq�Bs�Bq�Bt�Bu�Bu�B~�B�%B�JB�B}�B�B�B�B�B�+B�JB��B�VB�7B�DB�JB�PB�\B�hB�uB��B��B��B��B��B�B�B�FB�jBĜBȴBɺBɺB��B��B��B��B��B��B��BƨB�qB�^B�jB�jB�}BŢB��B��B��B��B��B��B��B�B�#B�)B�5B�BB�NB�`B�B�B�B�B��B��B	  B	B	
=B	DB	\B	�B	�B	�B	�B	�B	#�B	'�B	 �B	"�B	)�B	/B	1'B	0!B	0!B	5?B	6FB	6FB	5?B	;dB	=qB	>wB	@�B	A�B	B�B	C�B	C�B	C�B	E�B	E�B	J�B	J�B	L�B	N�B	Q�B	VB	YB	ZB	\)B	]/B	]/B	dZB	dZB	dZB	e`B	gmB	hsB	jB	k�B	k�B	l�B	l�B	n�B	n�B	o�B	q�B	r�B	u�B	w�B	{�B	z�B	{�B	|�B	|�B	}�B	|�B	� B	x�B	{�B	y�B	{�B	�%B	�JB	�PB	�PB	�PB	�=B	�7B	�B	�B	�B	� B	~�B	�1B	�PB	�=B	�7B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�FB	�RB	�^B	�dB	�jB	�}B	��B	��B	��B	B	ÖB	��B	�wB	�qB	�qB	�wB	�dB	�dB	�dB	�}B	ĜB	ƨB	ǮB	�
B	�yB	��B
B
PB
 �B
/B
8RB
<jB
C�B
H�B
M�B
Q�B
YB
[#B
`BB
e`B
k�B
n�B
t�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
=�P<u<#�
<#�
<#�
<#�
<�1<��
<#�
<#�
<#�
<#�
<49X<e`B<�t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<e`B<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<49X<D��<T��<D��<#�
<#�
<#�
<#�
<D��<T��<�o<#�
<#�
<T��<�j<#�
<#�
<#�
<#�
<#�
<D��<#�
<49X<#�
<#�
<#�
<T��<D��<#�
<�C�<�o<D��<49X<u<T��<D��<49X<#�
<#�
<#�
<D��<�o<�1<u<�o<�C�<�o<#�
<�o<ě�<49X<#�
<#�
<�9X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�t�<��
<�o<�t�<e`B<49X<49X<D��<��
<u<#�
<#�
<e`B<u<D��<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446362012010314463620120103144637  AO  ARGQ                                                                        20111130135521  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135521  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144637  IP                  G�O�G�O�G�O�                