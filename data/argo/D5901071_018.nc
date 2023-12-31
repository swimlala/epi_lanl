CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:56Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135602  20190522121825  1727_5046_018                   2C  D   APEX                            2143                            040306                          846 @�-�ʈ 1   @�-½�4@7�XbM��c�$�/�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A���A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� DfD�fD  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DOy�DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� Dj  Dj� Dj��Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�fDy��D�0 D�c3D�� D�� D�33D�vfD���D��3D�fD�Y�D���D���D�,�D�Y�DږfD�� D�)�D�\�D�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�33@���@���A��A<��A\��A|��A�33A�ffA�ffA�33A�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[�fC]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
��Dy�D�3Ds3D��Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN��DOl�DO�3DPs3DP�3DQs3DQ�3DRy�DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhy�Dh�3Dis3Di�3Djs3Dj��Dks3Dk�3Dll�Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3DsٚDy��D�)�D�\�D���D�ٚD�,�D�p D��3D���D� D�S3D��fD��fD�&fD�S3Dڐ D��D�#3D�VfD�3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�v�A�x�A�z�A�|�A�p�A�n�A�ffA�v�A�t�A�n�A�dZA�`BA�n�A�v�A�v�A�hsA�5?A�+A�A΍PA�VA��TA�A�A�VA�O�AǮA�ZA� �A��A���A�ZA��;A��A�hsA��A�x�A�A�A��uA��A�VA�ĜA���A�t�A���A��mA�G�A��A�bNA�{A��HA��+A��A�  A�t�A��7A�A�A���A�l�A���A��A�ƨA��+A�$�A�&�A�%A��A��mA�+A�I�A�5?A��
A��hA�?}A��A�\)A�1'A��yA��wA� �A�
=A�ƨA�
=A��-A��A��A���A��FA���A��RA��A�\)A���A�JA��jA�JA�bNA��mA�dZA���A�ĜA�n�A&�A}��A{�Az�/Ay��AwƨAv�AvJAu�hAsK�Apz�Aot�AnVAlJAk+Aj��AioAh �Aex�Ad�uAbr�Aa�#A`��A_�;A^��A]��A\��AZ5?AX��AX�AV�HAU%ATn�ATbAS\)ARn�AR{AQAQ
=AN�+AMhsAMK�ALr�AKVAJbAHAFE�AE�AE�AD�/ADn�AC�-ACp�AC\)AB��AA�A@��A?�
A?�A>9XA=��A=oA<jA:�DA9K�A7�#A5`BA3�A3A2VA1�A1�FA1t�A0��A/�TA.v�A+�mA+&�A*��A*^5A)"�A'��A'�PA&��A%�wA#�A#oA"ȴA"1'A �A��A��AVA�FA&�AjA�wAr�A��A��A�A�A��A��AI�A��AJAƨAx�A�AA�A?}Av�A�TA�Ar�AG�A
�+A	�A	��A	K�A�A��A�An�Ap�A��A1'A�A��A?}A �j@��m@���@��@�b@�\)@���@�ff@���@�X@��@��^@��j@�\)@�G�@�ƨ@���@�hs@�V@��@�@�dZ@��@�9@���@�t�@��@��@�Ĝ@� �@�+@�33@�|�@���@��T@���@�b@�o@ڗ�@�@���@�M�@�@��@�bN@��@��H@�^5@��m@�9X@��@��#@��@̬@�Q�@�M�@�{@�{@ɑh@�r�@�/@�/@���@�1'@�X@�M�@��@��H@�v�@�=q@���@�J@���@�;d@���@��`@�=q@�o@�z�@�(�@���@���@��+@�ff@��#@��h@���@��/@��+@��@�~�@���@�Z@�M�@�I�@���@�l�@�@�5?@��@�`B@��D@��;@�"�@��@���@�b@�@�p�@�G�@���@��@�j@�1'@��@��m@�b@���@���@�S�@�dZ@���@���@�-@���@�ƨ@�C�@��R@��\@�=q@��@�J@��@�-@�S�@��w@�Z@��@���@�p�@�E�@�n�@��!@�ȴ@��+@�^5@�=q@�$�@�@�`B@��@���@���@���@�t�@�G�@��m@�J@���@��@�`B@�hs@��h@�p�@�V@�I�@�dZ@�;d@��@�o@�33@��@�ȴ@��H@�\)@���@���@��F@��@�dZ@�\)@�C�@�S�@�K�@�K�@�33@�"�@�@��y@��H@��H@���@��\@�n�@�@��^@���@��h@��h@�O�@���@�Z@���@�dZ@��w@���@��@�|�@��@���@�ff@�V@�M�@��@���@��h@�`B@��@���@�r�@�bN@�1'@�b@��m@�ƨ@���@��@�\)@�C�@��@��@��+@��@���@���@�O�@���@�Z@���@��F@�|�@�t�@�dZ@�\)@�+@���@���@�v�@�V@�E�@��@�x�@�`B@�X@��@v�@n��@g|�@]p�@X�9@R-@I�#@C�m@>ff@:~�@2��@-�h@(bN@"�@�T@  @�@��@t�@A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�v�A�x�A�z�A�|�A�p�A�n�A�ffA�v�A�t�A�n�A�dZA�`BA�n�A�v�A�v�A�hsA�5?A�+A�A΍PA�VA��TA�A�A�VA�O�AǮA�ZA� �A��A���A�ZA��;A��A�hsA��A�x�A�A�A��uA��A�VA�ĜA���A�t�A���A��mA�G�A��A�bNA�{A��HA��+A��A�  A�t�A��7A�A�A���A�l�A���A��A�ƨA��+A�$�A�&�A�%A��A��mA�+A�I�A�5?A��
A��hA�?}A��A�\)A�1'A��yA��wA� �A�
=A�ƨA�
=A��-A��A��A���A��FA���A��RA��A�\)A���A�JA��jA�JA�bNA��mA�dZA���A�ĜA�n�A&�A}��A{�Az�/Ay��AwƨAv�AvJAu�hAsK�Apz�Aot�AnVAlJAk+Aj��AioAh �Aex�Ad�uAbr�Aa�#A`��A_�;A^��A]��A\��AZ5?AX��AX�AV�HAU%ATn�ATbAS\)ARn�AR{AQAQ
=AN�+AMhsAMK�ALr�AKVAJbAHAFE�AE�AE�AD�/ADn�AC�-ACp�AC\)AB��AA�A@��A?�
A?�A>9XA=��A=oA<jA:�DA9K�A7�#A5`BA3�A3A2VA1�A1�FA1t�A0��A/�TA.v�A+�mA+&�A*��A*^5A)"�A'��A'�PA&��A%�wA#�A#oA"ȴA"1'A �A��A��AVA�FA&�AjA�wAr�A��A��A�A�A��A��AI�A��AJAƨAx�A�AA�A?}Av�A�TA�Ar�AG�A
�+A	�A	��A	K�A�A��A�An�Ap�A��A1'A�A��A?}A �j@��m@���@��@�b@�\)@���@�ff@���@�X@��@��^@��j@�\)@�G�@�ƨ@���@�hs@�V@��@�@�dZ@��@�9@���@�t�@��@��@�Ĝ@� �@�+@�33@�|�@���@��T@���@�b@�o@ڗ�@�@���@�M�@�@��@�bN@��@��H@�^5@��m@�9X@��@��#@��@̬@�Q�@�M�@�{@�{@ɑh@�r�@�/@�/@���@�1'@�X@�M�@��@��H@�v�@�=q@���@�J@���@�;d@���@��`@�=q@�o@�z�@�(�@���@���@��+@�ff@��#@��h@���@��/@��+@��@�~�@���@�Z@�M�@�I�@���@�l�@�@�5?@��@�`B@��D@��;@�"�@��@���@�b@�@�p�@�G�@���@��@�j@�1'@��@��m@�b@���@���@�S�@�dZ@���@���@�-@���@�ƨ@�C�@��R@��\@�=q@��@�J@��@�-@�S�@��w@�Z@��@���@�p�@�E�@�n�@��!@�ȴ@��+@�^5@�=q@�$�@�@�`B@��@���@���@���@�t�@�G�@��m@�J@���@��@�`B@�hs@��h@�p�@�V@�I�@�dZ@�;d@��@�o@�33@��@�ȴ@��H@�\)@���@���@��F@��@�dZ@�\)@�C�@�S�@�K�@�K�@�33@�"�@�@��y@��H@��H@���@��\@�n�@�@��^@���@��h@��h@�O�@���@�Z@���@�dZ@��w@���@��@�|�@��@���@�ff@�V@�M�@��@���@��h@�`B@��@���@�r�@�bN@�1'@�b@��m@�ƨ@���@��@�\)@�C�@��@��@��+@��@���@���@�O�@���@�Z@���@��F@�|�@�t�@�dZ@�\)@�+@���@���@�v�@�V@�E�@��@�x�@�`B@�X@��@v�@n��@g|�@]p�@X�9@R-@I�#@C�m@>ff@:~�@2��@-�h@(bN@"�@�T@  @�@��@t�@A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�NB�yB�B��B��B�B�yB�B�B�B�B�B�B�B�B�`B�BB�B�B��B��BÖB�}B�qB�RB�FB�9B�-B�B��B��B�hB�Bz�Bv�Br�Bk�BcTBVBG�B6FB!�B�BVB%B��B�B�mB�NB�5B�B��B��B��B�3B��B��B��B��B�=By�BgmBR�B6FB�BoBB
��B
�B
�ZB
�B
��B
ƨB
�wB
�LB
�!B
��B
��B
�=B
~�B
t�B
l�B
ffB
_;B
W
B
Q�B
N�B
I�B
@�B
'�B
�B
{B

=B
B
B	��B	�B	�)B	��B	ȴB	ƨB	��B	ÖB	ĜB	�qB	�3B	��B	��B	�{B	�DB	�%B	�B	�B	�B	}�B	|�B	y�B	s�B	hsB	bNB	`BB	[#B	S�B	M�B	E�B	>wB	=qB	;dB	9XB	7LB	5?B	49B	2-B	.B	+B	%�B	"�B	�B	�B	�B	�B	bB		7B	B	  B�B�B�fB�NB�BB�;B�)B�B��BĜB�dB�LB�?B�3B�B�B�'B�B��B��B��B��B��B��B��B�uB�oB�\B�JB�DB�7B�B~�B|�By�Bx�Bw�Bu�Bs�Bo�Bn�Bm�Bl�BjBgmBffBe`BcTBbNB`BB]/B\)B\)B[#BZBYBXBS�BP�BN�BN�BL�BL�BL�BK�BK�BJ�BK�BK�BJ�BJ�BI�BI�BH�BF�BE�BE�BF�BD�BD�BD�BC�BC�BC�BB�B<jB:^B9XB:^B<jB@�B?}B=qB>wB?}BE�BH�BL�BQ�BR�BS�BT�BXBYB_;Bq�B�B�B�%B�B�B� B|�Bv�Bo�Bn�Br�Bt�Bt�Bu�By�By�Bx�Bx�B�B�\B�uB�uB�oB�DB�+B�%Bp�Bp�Bo�Bp�B�%B��B��B��B��B�B�XB�XB��B��B��B�B�B�#B�#B�#B�B��BĜBÖBŢB�qB�?B�B��B��B��B��B��B�B�!B�B�B�B�B��B��B��B��B��B�B�B�!B�?B�^B�jB�jB�jB�qBÖBƨBɺB��B��B��B��B��B��B��B�B�)B�)B�NB�B�B��B	  B	B	PB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	\B	PB	uB	"�B	�B	!�B	$�B	&�B	,B	0!B	2-B	49B	8RB	<jB	=qB	?}B	A�B	C�B	E�B	E�B	G�B	K�B	R�B	]/B	e`B	hsB	hsB	iyB	jB	m�B	r�B	s�B	s�B	u�B	v�B	x�B	y�B	z�B	z�B	{�B	|�B	�B	�1B	�JB	�PB	�VB	�VB	�\B	�bB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�?B	�XB	�jB	�wB	�}B	��B	B	B	ÖB	ÖB	ŢB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�#B	�mB	��B
B
uB
�B
'�B
0!B
9XB
=qB
C�B
G�B
O�B
T�B
[#B
`BB
cTB
ffB
jB
n�B
r�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�)B�fB�B��B��BB��B�B�B�B�B�B�B�B��B�B�yB�`B�/B�
B�B��BƨB��B��B�^B�LB�FB�9B�-B�-B��B��B�=B|�Bw�Bv�Bn�BjB]/BN�B@�B&�B�BbB	7BB��B�yB�TB�BB�)B�B��BǮB�XB��B��B��B��B�hB�Bm�B`BBA�B!�B�B	7B
��B
�B
�yB
�B
�
B
��B
��B
�^B
�?B
�B
��B
�\B
�B
z�B
o�B
jB
e`B
[#B
R�B
O�B
N�B
G�B
)�B
�B
�B
JB
%B
B	��B	��B	�;B	�B	��B	ȴB	ĜB	ƨB	ǮB	��B	�^B	��B	��B	��B	�hB	�1B	�B	�%B	�%B	~�B	}�B	|�B	{�B	l�B	cTB	cTB	`BB	XB	T�B	K�B	A�B	?}B	<jB	;dB	:^B	6FB	5?B	5?B	1'B	0!B	(�B	%�B	"�B	�B	�B	�B	�B	PB	1B	%B��B�B�sB�TB�HB�BB�5B�B��B��B�qB�RB�FB�FB�-B�!B�9B�-B�!B��B��B��B��B��B��B��B�{B�hB�\B�VB�VB�7B�B�B{�Bz�Bx�Bw�Bx�Br�Bo�Bn�Bn�Bm�Bk�BiyBgmBffBe`BdZB`BB^5B]/B\)B[#BZBZBW
BR�BP�BO�BP�BO�BM�BM�BM�BL�BM�BM�BK�BK�BJ�BJ�BI�BJ�BG�BG�BI�BH�BG�BG�BD�BD�BE�BF�B@�B=qB;dB<jB=qBB�BA�B?}B?}BA�BE�BH�BM�BS�BT�BVBW
BYB[#B`BBo�B�B�%B�%B�B�B�B|�B|�Br�Bp�Bt�Bu�Bu�Bx�Bz�By�By�Bz�B� B�\B�{B�{B��B�\B�JB�DBq�Bq�Bq�Bo�B�B��B��B��B��B�B�qB�^B�}BɺB��B�B�#B�)B�#B�)B�)B�BŢBÖBɺB��B�XB�9B��B��B��B��B��B�'B�'B�!B�!B�B�!B�B��B�B�B�B�B�B�'B�?B�^B�qB�jB�qB�qBĜBƨB��B��B��B��B��B��B��B��B�B�)B�)B�HB�B�B��B	  B	B	JB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	bB	JB	hB	$�B	"�B	"�B	%�B	&�B	,B	0!B	2-B	5?B	9XB	>wB	=qB	?}B	A�B	C�B	F�B	E�B	G�B	J�B	Q�B	]/B	e`B	iyB	hsB	iyB	jB	m�B	r�B	s�B	s�B	u�B	v�B	x�B	y�B	z�B	{�B	{�B	|�B	�B	�7B	�JB	�PB	�VB	�\B	�bB	�hB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�?B	�XB	�jB	�wB	�}B	��B	B	B	ÖB	ÖB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�#B	�sB	��B
B
uB
�B
'�B
0!B
9XB
>wB
C�B
G�B
O�B
VB
[#B
`BB
dZB
gmB
jB
n�B
r�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446402012010314464020120103144640  AO  ARGQ                                                                        20111130135602  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135602  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144640  IP                  G�O�G�O�G�O�                