CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:02Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               (A   AO  20111130135950  20190522121825  1727_5046_040                   2C  D   APEX                            2143                            040306                          846 @�I_\���1   @�I`��@7?;dZ��c��-V1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	y�D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DH  DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  DsffDy�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw��B33B���B���B���B���B�ffB���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5�fC7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	l�D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC��DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHl�DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De��Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3DsY�Dy��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��/A��A���A���A��A�"�A�  A��A̴9ȂhA̋DẢ7A̅A�n�A�ffA�hsA�dZA�bNA�`BA�^5A�\)A�XA�XA�VA�S�A�Q�A�Q�A�O�A�M�A�K�A�I�A�G�A�E�A�
=A�\)A���A�|�A�&�A�~�A���A�O�A�5?A��RA��PA�&�A�9XA�`BA��!A�x�A��A�?}A���A���A��!A���A�JA��A�;dA�5?A��A�A�A���A�n�A���A�A�`BA�v�A�O�A��A�%A�bNA���A��\A���A�A��\A���A��A�bA��
A��!A�bNA���A��A���A���A�A��A�M�A��A�VA��A��TA�E�A���A���A�%A�p�A�
=A��A�=qA��!A�-A}�wAz��Ayp�AxĜAx��Aw33AuƨAs�FAr1Ap�\An�Al�yAkp�AkAj��Aix�Af�!Ae�#Ae;dAd�9Ac��Ab=qA`��A_%A]�A[�
AZz�AY��AY�-AY
=AX5?AW��AW&�AV^5AU��AUhsAUAT�!AT�HAT~�AS�wAR��AQ;dAPE�AO
=AM+AL�yAK�TAJ�HAH��AG�FAG&�AF��AE`BADjAC33AA�PA@$�A?|�A?+A> �A<��A<��A;%A9��A8��A8^5A7�7A6�!A5O�A49XA3
=A1�A0{A/�A.(�A-��A-+A,A�A+�A*�A)?}A(��A(�A'/A%t�A$�A$ �A#K�A"ȴA"��A!�#A �AƨA��A$�A33A��AVAG�A�+A  A�;A��AVAQ�A�^A`BA�A�AbNAx�A7LA�A9XA�9A
=A �A��A��A^5A1AO�A
ffA	�wA	S�A�DAdZA�9AbA��A�;A�hA�AbNA1'A  A�^Al�A%A��A1'A��AhsA Z@�\)@��@�ȴ@��@��@�Z@��@�Ĝ@���@�J@�7L@��`@�ƨ@��@�+@�(�@柾@�x�@�%@���@�R@�X@ߕ�@�Ĝ@���@�o@�5?@�Z@��@�1'@�@�-@��/@���@�33@Ο�@���@̓u@�|�@���@�{@ɑh@�r�@ǍP@�
=@�$�@�x�@�Q�@�@�J@��@��F@�t�@��7@�33@��/@��P@�n�@�@��@���@��y@�^5@��#@��h@�z�@�@��`@�r�@��@�-@���@�ƨ@�|�@��@��@�@��7@�/@� �@��R@�M�@�x�@�Z@���@���@�;d@�ȴ@�M�@��-@�?}@���@�(�@�1@��w@�33@��R@�{@�ȴ@�@���@�@�x�@�hs@���@���@���@���@��!@�5?@��@�p�@�&�@���@���@�-@���@���@��-@�@�J@�n�@���@��R@���@���@��j@���@���@��R@�O�@�z�@���@��#@���@��@�x�@��@��`@�ȴ@�t�@�@��@�5?@��#@�E�@�$�@��@���@���@��h@�X@���@�r�@� �@��
@�\)@��y@���@�ff@�-@�J@���@���@�`B@�7L@�/@�/@��@�V@���@��j@�1'@��
@���@�S�@�o@��H@��@���@�^5@�=q@��@�$�@�J@��h@���@��@�7L@���@��/@�Ĝ@��9@��m@�|�@�l�@�dZ@�;d@�;d@�;d@�K�@��@���@���@��R@�ȴ@��R@���@�~�@�5?@�@��-@��-@��-@���@��7@�7L@��@���@��u@�Q�@� �@��@��;@��m@���@��w@�S�@�C�@�
=@�ȴ@��!@��\@��+@�n�@�^5@�M�@��@;d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��/A��A���A���A��A�"�A�  A��A̴9ȂhA̋DẢ7A̅A�n�A�ffA�hsA�dZA�bNA�`BA�^5A�\)A�XA�XA�VA�S�A�Q�A�Q�A�O�A�M�A�K�A�I�A�G�A�E�A�
=A�\)A���A�|�A�&�A�~�A���A�O�A�5?A��RA��PA�&�A�9XA�`BA��!A�x�A��A�?}A���A���A��!A���A�JA��A�;dA�5?A��A�A�A���A�n�A���A�A�`BA�v�A�O�A��A�%A�bNA���A��\A���A�A��\A���A��A�bA��
A��!A�bNA���A��A���A���A�A��A�M�A��A�VA��A��TA�E�A���A���A�%A�p�A�
=A��A�=qA��!A�-A}�wAz��Ayp�AxĜAx��Aw33AuƨAs�FAr1Ap�\An�Al�yAkp�AkAj��Aix�Af�!Ae�#Ae;dAd�9Ac��Ab=qA`��A_%A]�A[�
AZz�AY��AY�-AY
=AX5?AW��AW&�AV^5AU��AUhsAUAT�!AT�HAT~�AS�wAR��AQ;dAPE�AO
=AM+AL�yAK�TAJ�HAH��AG�FAG&�AF��AE`BADjAC33AA�PA@$�A?|�A?+A> �A<��A<��A;%A9��A8��A8^5A7�7A6�!A5O�A49XA3
=A1�A0{A/�A.(�A-��A-+A,A�A+�A*�A)?}A(��A(�A'/A%t�A$�A$ �A#K�A"ȴA"��A!�#A �AƨA��A$�A33A��AVAG�A�+A  A�;A��AVAQ�A�^A`BA�A�AbNAx�A7LA�A9XA�9A
=A �A��A��A^5A1AO�A
ffA	�wA	S�A�DAdZA�9AbA��A�;A�hA�AbNA1'A  A�^Al�A%A��A1'A��AhsA Z@�\)@��@�ȴ@��@��@�Z@��@�Ĝ@���@�J@�7L@��`@�ƨ@��@�+@�(�@柾@�x�@�%@���@�R@�X@ߕ�@�Ĝ@���@�o@�5?@�Z@��@�1'@�@�-@��/@���@�33@Ο�@���@̓u@�|�@���@�{@ɑh@�r�@ǍP@�
=@�$�@�x�@�Q�@�@�J@��@��F@�t�@��7@�33@��/@��P@�n�@�@��@���@��y@�^5@��#@��h@�z�@�@��`@�r�@��@�-@���@�ƨ@�|�@��@��@�@��7@�/@� �@��R@�M�@�x�@�Z@���@���@�;d@�ȴ@�M�@��-@�?}@���@�(�@�1@��w@�33@��R@�{@�ȴ@�@���@�@�x�@�hs@���@���@���@���@��!@�5?@��@�p�@�&�@���@���@�-@���@���@��-@�@�J@�n�@���@��R@���@���@��j@���@���@��R@�O�@�z�@���@��#@���@��@�x�@��@��`@�ȴ@�t�@�@��@�5?@��#@�E�@�$�@��@���@���@��h@�X@���@�r�@� �@��
@�\)@��y@���@�ff@�-@�J@���@���@�`B@�7L@�/@�/@��@�V@���@��j@�1'@��
@���@�S�@�o@��H@��@���@�^5@�=q@��@�$�@�J@��h@���@��@�7L@���@��/@�Ĝ@��9@��m@�|�@�l�@�dZ@�;d@�;d@�;d@�K�@��@���@���@��R@�ȴ@��R@���@�~�@�5?@�@��-@��-@��-@���@��7@�7L@��@���@��u@�Q�@� �@��@��;@��m@���@��w@�S�@�C�@�
=@�ȴ@��!@��\@��+@�n�@�^5@�M�@��@;d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B�dBǮBǮBƨBB�}B�}B��B��B�wB�qB�qB�qB�qB�qB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�qB�}B�}BƨB�HB�fB�#B�BB�)B�B��B��B��BŢB��B�dB�RB�FB�-B�!B�3B�B�B��B��B��B�%B�Bw�Bt�Bm�BaHB\)BVBK�B.B �B�BB��B�B�fB��B�^B�B��B��B�uB�hB�DB�B}�Bp�BT�B/BDB
�B
�HB
��B
ƨB
�dB
�9B
�!B
�B
��B
��B
��B
�7B
~�B
w�B
m�B
W
B
B�B
8RB
5?B
>wB
?}B
.B
#�B
�B
\B	��B	�B	�BB	�HB	�NB	��B	�jB	ÖB	�qB	�LB	�'B	��B	��B	�hB	�JB	�B	}�B	{�B	z�B	u�B	r�B	q�B	n�B	l�B	iyB	n�B	m�B	o�B	x�B	u�B	p�B	jB	gmB	hsB	_;B	S�B	[#B	YB	Q�B	B�B	<jB	7LB	7LB	2-B	+B	#�B	�B	uB	hB	\B	
=B	B��B��B�B�B�B�mB�HB�B��B��BŢB�}B�jB�XB�LB�9B�!B�B��B��B�B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�\B�VB�PB�PB�JB�DB�=B�7B�+B�B�B�B~�B{�Bx�Bw�Bv�Bt�Bq�Bl�BiyBhsBffBdZBcTBbNB`BB^5B\)BZBXBVBS�BS�BR�BQ�BP�BO�BN�BM�BL�BL�BK�BJ�BI�BH�BF�BD�BC�BA�B?}B>wB=qB<jB:^B8RB7LB7LB6FB5?B49B1'B0!B.B.B.B.B-B,B+B)�B(�B)�B)�B(�B'�B'�B&�B(�B(�B(�B)�B)�B)�B)�B)�B)�B,B,B-B.B0!B1'B1'B2-B33B49B6FB6FB6FB6FB5?B:^B?}BD�BG�BI�BJ�BM�BO�BQ�BS�BT�BS�BS�BVBXBXB[#B^5BdZBiyBjBk�Bn�Bo�Bo�Bo�Bv�Bz�B{�B� B�B�7B�DB�PB�\B�hB�{B��B��B��B��B��B��B��B�9B��B��BƨB��B��B�
B�)B�/B�5B�NB�mB�B�B�B��B	%B	PB	hB	oB	oB	�B	�B	�B	#�B	&�B	(�B	(�B	'�B	%�B	"�B	"�B	"�B	�B	�B	%�B	.B	/B	0!B	1'B	/B	:^B	F�B	L�B	O�B	P�B	O�B	Q�B	W
B	XB	ZB	\)B	]/B	\)B	\)B	^5B	aHB	cTB	e`B	hsB	k�B	n�B	p�B	q�B	r�B	u�B	w�B	{�B	}�B	~�B	� B	�B	�B	�B	�%B	�7B	�7B	�=B	�DB	�JB	�PB	�PB	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�-B	�3B	�9B	�3B	�9B	�9B	�?B	�LB	�LB	�LB	�dB	�jB	�qB	�wB	�}B	��B	B	ÖB	ŢB	ǮB	ǮB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�T1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B�dBȴBȴBǮBÖB�}B�}B��BB�wB�qB�qB�qB�qB�qB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�wB��BB��B�yB�B�NB�fB�TB�/B��B��B��B��BȴBB�dB�^B�RB�3B�LB�-B�B�B�B��B�+B�Bx�Bv�Bp�BffBbNB[#BW
B2-B#�B�BB��B��B�B�B�}B�FB��B��B�{B�uB�\B�1B�B{�BaHB>wB�B
��B
�fB
�#B
��B
�wB
�?B
�'B
�B
��B
��B
��B
�JB
�B
z�B
t�B
^5B
F�B
:^B
5?B
B�B
C�B
49B
(�B
�B
�B	��B	��B	�HB	�HB	�mB	�B	�}B	ŢB	�}B	�XB	�FB	��B	��B	�{B	�oB	�+B	� B	|�B	|�B	x�B	t�B	s�B	q�B	n�B	jB	o�B	n�B	o�B	z�B	x�B	t�B	o�B	k�B	m�B	e`B	T�B	_;B	]/B	XB	F�B	>wB	9XB	;dB	6FB	0!B	)�B	 �B	�B	uB	uB	VB	B	B��B�B�B�B�yB�`B�/B�
B��BɺB��B��B�^B�XB�LB�9B�B�B��B�!B�B�B��B��B��B��B��B��B��B��B��B��B�{B�hB�bB�hB�bB�VB�JB�DB�DB�7B�+B�B�B�B~�Bz�Bx�Bw�Bv�Bu�Bp�Bk�BiyBhsBffBdZBdZBcTB`BB^5B]/B\)BXBVBT�BS�BR�BR�BQ�BO�BN�BM�BM�BM�BK�BK�BI�BH�BH�BF�BE�BC�B@�B?}B=qB=qB<jB;dB9XB8RB6FB7LB6FB5?B33B1'B0!B/B0!B.B.B.B.B,B,B(�B,B+B,B+B+B,B,B+B+B+B,B,B-B-B.B0!B2-B2-B33B33B5?B6FB8RB8RB7LB7LB8RB>wBC�BF�BI�BJ�BL�BO�BP�BR�BT�BVBVBXBXBYB[#B]/BaHBffBjBk�Bl�Bo�Bp�Bp�Bq�Bx�B{�B|�B�B�%B�=B�JB�VB�bB�oB��B��B��B��B��B��B��B��B�3BB��BǮB��B��B�B�)B�5B�BB�ZB�sB�B�B�B��B	%B	PB	oB	uB	oB	�B	�B	�B	#�B	&�B	(�B	+B	)�B	'�B	$�B	#�B	%�B	�B	�B	$�B	.B	/B	1'B	33B	.B	7LB	E�B	M�B	O�B	P�B	P�B	Q�B	W
B	XB	[#B	\)B	]/B	\)B	\)B	_;B	bNB	dZB	ffB	iyB	l�B	n�B	q�B	q�B	s�B	v�B	x�B	{�B	}�B	~�B	� B	�B	�B	�B	�+B	�=B	�=B	�DB	�JB	�PB	�PB	�VB	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�!B	�!B	�'B	�-B	�3B	�9B	�3B	�?B	�?B	�?B	�LB	�LB	�LB	�jB	�qB	�qB	�}B	��B	��B	ÖB	ĜB	ŢB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�T1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<D��<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446472012010314464720120103144648  AO  ARGQ                                                                        20111130135950  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135950  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144648  IP                  G�O�G�O�G�O�                