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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               GA   AO  20111130140732  20190522121826  1727_5046_071                   2C  D   APEX                            2143                            040306                          846 @�qk����1   @�qlW:�@7ix����dV�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;�fD<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJy�DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Db��Dcy�Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Dh��Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dry�Ds  DsffDy�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @s33@���@���A��A<��A\��A|��A���A�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B6��B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG�fCI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Dy�D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D��Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$y�D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;y�D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJl�DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db��Dcl�Dc��Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh��Dil�Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq��Drl�Dr�3DsY�Dy�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�Q�A�K�A�VA�jA�l�A�ffA�hsA�hsA�hsA�jA�jA�jA�jA�hsA�jA�jA�jA�jA�jA�jA�ffA�^5A�33A��A�(�A�l�A�M�A�&�A���A�+A�`BA�  A�A�A�ȴA���A�JA��`A�ƨA�G�A��/A�|�A�VA�1'A���A�%A��;A���A�x�A�Q�A�JA�t�A�5?A�$�A��A��;A�z�A�ȴA�?}A��mA��RA���A�A�A��A�ĜA��!A���A���A�9XA���A�^5A�dZA�oA�oA��-A�hsA���A���A��A���A��FA�t�A�$�A��A��A��-A���A��A�^5A���A�C�A���A��7A�$�A��A�ĜA�r�A��uA��A��uA��`A�1'A���A�VA�XA��`A�JA� �A�n�A��A���A�VA��wA�ZA�=qA�A��hA/A|bAy��Au�TAp�+Ak�^Ai�7Ah�!AfAb�+AaAaoA`z�A`$�A_dZA_S�A^�A^�A^ �A]�A]7LA\bA[/AZ��AZv�AY�mAX~�AV�DAU�AT5?ARbNAP�ANbNAM��AM+AL�AJv�AHĜAG�
AGt�AFjAE�ADbNACS�ABjAAoA?|�A>Q�A=O�A< �A;+A:I�A9�A933A8bNA7�A5��A5�A57LA4Q�A4 �A3p�A1��A0bA.ĜA.�A-�
A-t�A-/A,�!A+hsA*��A)�mA)
=A'�A&�HA&��A&ffA%�wA$~�A#A"�`A"�A!�A!%A �!A�^AoA�!A�
A��AbA|�A^5A�hAQ�A|�Az�A��A�yA�PA�\A��A��A��A�FA?}A
�jA	�A�HA5?A�HA�A�A�uA�mA7LA��AI�A��AXA �A -@��@�I�@���@��T@�V@�z�@��@�S�@���@�{@��7@��@�b@��y@�%@@�?}@��@���@�
=@�~�@��@�R@噚@���@�@��y@�\@�v�@�@���@�ȴ@�^5@��@�%@��m@��y@���@�Q�@֟�@�X@��@�+@�M�@�@�&�@�Q�@�9X@��@ϝ�@�33@�ff@�/@��@�\)@�~�@���@�/@�Ĝ@�t�@��y@�
=@���@Ə\@�-@�x�@�o@�(�@��@��P@���@��@�7L@��j@��9@�1@��@�v�@��@���@��@���@�E�@�O�@�Q�@��P@�V@��@��^@�O�@���@�r�@��F@���@��\@���@�5?@���@�1'@��m@�S�@���@��+@��^@��@��;@��@�dZ@�33@��@�@��@��!@�^5@���@���@��7@���@�b@��@�ȴ@��@�5?@���@�?}@�V@��j@��@�hs@��m@���@�r�@��h@�@�O�@��;@��\@�ff@�hs@�`B@��@�I�@�;d@�C�@��;@� �@���@��#@�n�@�ff@��@��T@�hs@�j@��@�ff@�$�@�@���@�G�@���@��j@�A�@�b@��@�K�@�ȴ@��@��9@�S�@�o@�;d@���@���@�+@�o@�@�n�@��-@��@�r�@��@���@�ff@�M�@�@��@�7L@���@�`B@���@���@���@�z�@�9X@� �@��;@���@�ƨ@�K�@��@�~�@�=q@��@���@��@�Ĝ@��@�Q�@�A�@��@���@��
@���@�K�@���@���@��\@�v�@�n�@�E�@�@���@�`B@��@�V@��@��`@���@��9@���@�z�@�(�@��@��;@��
@�ƨ@���@�;d@��@�
=@�
=@�@�@��@��R@�A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Q�A�K�A�VA�jA�l�A�ffA�hsA�hsA�hsA�jA�jA�jA�jA�hsA�jA�jA�jA�jA�jA�jA�ffA�^5A�33A��A�(�A�l�A�M�A�&�A���A�+A�`BA�  A�A�A�ȴA���A�JA��`A�ƨA�G�A��/A�|�A�VA�1'A���A�%A��;A���A�x�A�Q�A�JA�t�A�5?A�$�A��A��;A�z�A�ȴA�?}A��mA��RA���A�A�A��A�ĜA��!A���A���A�9XA���A�^5A�dZA�oA�oA��-A�hsA���A���A��A���A��FA�t�A�$�A��A��A��-A���A��A�^5A���A�C�A���A��7A�$�A��A�ĜA�r�A��uA��A��uA��`A�1'A���A�VA�XA��`A�JA� �A�n�A��A���A�VA��wA�ZA�=qA�A��hA/A|bAy��Au�TAp�+Ak�^Ai�7Ah�!AfAb�+AaAaoA`z�A`$�A_dZA_S�A^�A^�A^ �A]�A]7LA\bA[/AZ��AZv�AY�mAX~�AV�DAU�AT5?ARbNAP�ANbNAM��AM+AL�AJv�AHĜAG�
AGt�AFjAE�ADbNACS�ABjAAoA?|�A>Q�A=O�A< �A;+A:I�A9�A933A8bNA7�A5��A5�A57LA4Q�A4 �A3p�A1��A0bA.ĜA.�A-�
A-t�A-/A,�!A+hsA*��A)�mA)
=A'�A&�HA&��A&ffA%�wA$~�A#A"�`A"�A!�A!%A �!A�^AoA�!A�
A��AbA|�A^5A�hAQ�A|�Az�A��A�yA�PA�\A��A��A��A�FA?}A
�jA	�A�HA5?A�HA�A�A�uA�mA7LA��AI�A��AXA �A -@��@�I�@���@��T@�V@�z�@��@�S�@���@�{@��7@��@�b@��y@�%@@�?}@��@���@�
=@�~�@��@�R@噚@���@�@��y@�\@�v�@�@���@�ȴ@�^5@��@�%@��m@��y@���@�Q�@֟�@�X@��@�+@�M�@�@�&�@�Q�@�9X@��@ϝ�@�33@�ff@�/@��@�\)@�~�@���@�/@�Ĝ@�t�@��y@�
=@���@Ə\@�-@�x�@�o@�(�@��@��P@���@��@�7L@��j@��9@�1@��@�v�@��@���@��@���@�E�@�O�@�Q�@��P@�V@��@��^@�O�@���@�r�@��F@���@��\@���@�5?@���@�1'@��m@�S�@���@��+@��^@��@��;@��@�dZ@�33@��@�@��@��!@�^5@���@���@��7@���@�b@��@�ȴ@��@�5?@���@�?}@�V@��j@��@�hs@��m@���@�r�@��h@�@�O�@��;@��\@�ff@�hs@�`B@��@�I�@�;d@�C�@��;@� �@���@��#@�n�@�ff@��@��T@�hs@�j@��@�ff@�$�@�@���@�G�@���@��j@�A�@�b@��@�K�@�ȴ@��@��9@�S�@�o@�;d@���@���@�+@�o@�@�n�@��-@��@�r�@��@���@�ff@�M�@�@��@�7L@���@�`B@���@���@���@�z�@�9X@� �@��;@���@�ƨ@�K�@��@�~�@�=q@��@���@��@�Ĝ@��@�Q�@�A�@��@���@��
@���@�K�@���@���@��\@�v�@�n�@�E�@�@���@�`B@��@�V@��@��`@���@��9@���@�z�@�(�@��@��;@��
@�ƨ@���@�;d@��@�
=@�
=@�@�@��@��R@�A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB@�B@�B@�BA�BA�B@�B@�B@�B@�B@�B@�B@�B@�B?}B@�BA�BA�BA�BB�BC�BF�BI�BT�B�B��B��B��B��B�B�BǮBƨBǮBǮB��B��B��B��B��B��B��B��B��B��B��B�5B�;B�5B�/B�#B��B��B��B��B�
B�/B�TB�sB�mB�yB�B�B�sB�`B�BB��BɺB��B��B��BȴBB�jB�9B�!B�B��B��B�DBy�BN�B1'B�BJBB�B�NB��B��B�9B��B�PB~�Bx�Bu�Bq�Bk�B_;BXBN�B6FB&�B�BPBB
��B
�B
�5B
��B
��B
�jB
�'B
��B
�DB
|�B
w�B
p�B
bNB
R�B
=qB
�B	��B	�B	ƨB	�jB	��B	�DB	�=B	�7B	�JB	�uB	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�{B	�+B	�B	x�B	jB	_;B	S�B	P�B	J�B	F�B	@�B	9XB	6FB	49B	0!B	,B	%�B	�B	�B	PB	B��B��B��B��B��B��B��B�B�B�B�B�yB�`B�TB�/B��B��BƨBĜBB��B�}B�jB�RB�FB�3B�'B�B�B�B�B��B��B��B��B��B��B��B��B��B�{B�oB�hB�bB�\B�VB�=B�+B�B�B� B�B}�Bx�Bs�Bm�BiyBgmBdZBcTB`BB^5B\)BZBXBW
BVBT�BR�BR�BQ�BP�BO�BO�BM�BL�BJ�BI�BH�BH�BH�BG�BG�BG�BH�BG�BG�BF�BE�BD�BC�BD�BD�BD�BC�BC�BB�B?}BB�BA�BA�BA�BA�BA�B@�B?}B>wB?}B?}B>wB>wB=qB=qB<jB<jB<jB;dB;dB;dB;dB;dB:^B;dB;dB;dB;dB:^B:^B:^B>wBB�BC�BC�BD�BF�BF�BG�BN�BQ�BR�BR�BR�BR�BT�BT�BVBYB[#B]/BbNBffBjBp�Bq�Br�Bq�Bq�Bq�Bt�Bu�Bw�Bx�B{�B{�B{�B|�B|�B}�B� B�B�%B�VB��B��B��B��B��B��B��B��B��B�B�B�!B�-B�3B�?B�?B�^BÖBɺB��B��B��B��B��B��B�
B�HB�B�sB�sB�sB�B�B�B�B��B	  B	B	  B��B��B��B��B��B	B��B��B��B	1B	JB	{B	�B	 �B	!�B	!�B	"�B	&�B	,B	0!B	49B	6FB	6FB	9XB	>wB	B�B	C�B	G�B	I�B	M�B	P�B	Q�B	S�B	S�B	T�B	XB	\)B	_;B	`BB	_;B	_;B	bNB	gmB	gmB	gmB	gmB	gmB	iyB	l�B	m�B	o�B	r�B	u�B	x�B	� B	�B	�B	�B	�B	�%B	�B	�7B	�=B	�=B	�JB	�\B	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�FB	�LB	�LB	�RB	�XB	�^B	�dB	�qB	�}B	��B	��B	��B	��B	ĜB	ŢB	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B@�B@�B@�BA�BA�B@�B@�B@�B@�B@�B@�B@�B@�B?}B@�BA�BA�BA�BB�BC�BF�BJ�BVB�B��B��B��B��B�B�)BɺBƨBǮBɺB��B��B�B��B��B��B��B��B��B��B�B�;B�BB�;B�5B�)B�B��B��B��B�B�;B�fB�B�yB�B�B�B�B�fB�fB�/B��B��B��B��B��BǮB��B�FB�'B�B��B��B�bB�+BYB<jB�B\B1B��B�B��BŢB�qB�B�{B�Bz�Bv�Bt�Bq�BbNB\)BYB>wB)�B�BbB+B  B
��B
�HB
��B
ÖB
��B
�LB
��B
�hB
}�B
z�B
v�B
jB
ZB
H�B
+B
B	�BB	ɺB	ĜB	�B	�VB	�JB	�DB	�PB	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�=B	�B	|�B	o�B	cTB	T�B	R�B	M�B	J�B	D�B	;dB	7LB	7LB	2-B	0!B	(�B	"�B	�B	hB	B	  B��B��B��B��B��B��B��B��B�B�B�B�fB�`B�NB�B��BȴBŢBÖBB��B��B�dB�XB�FB�?B�'B�B�B�!B�B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�uB�PB�DB�+B�B�B�B�B{�Bx�Br�Bo�BjBffBe`BdZB`BB^5B^5B[#BZBXBW
BT�BT�BS�BR�BQ�BQ�BP�BO�BO�BL�BJ�BJ�BI�BH�BI�BI�BI�BH�BH�BH�BH�BH�BH�BG�BF�BF�BE�BD�BD�BE�BE�BC�BC�BC�BB�BB�BA�B?}BB�B@�B@�B@�BA�B?}B?}B?}B?}B>wB=qB<jB<jB<jB<jB;dB;dB<jB<jB<jB;dB<jB<jB?}BD�BD�BD�BE�BH�BG�BG�BO�BR�BS�BT�BW
BXBVBVBXBZB]/B^5BbNBgmBl�Bp�Br�Bs�Br�Bt�Bt�Bv�Bw�By�Bz�B|�B{�B|�B}�B}�B� B�B�B�%B�\B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�3B�?B�FB�dBĜB��B��B��B��B��B��B�B�B�;B�B�yB�yB�sB�B�B�B�B�B	  B	B	B��B��B��B��B��B	+B��B��B��B	1B	DB	uB	�B	 �B	"�B	"�B	#�B	(�B	.B	2-B	5?B	6FB	7LB	:^B	?}B	C�B	D�B	H�B	I�B	N�B	Q�B	R�B	W
B	VB	VB	XB	\)B	_;B	aHB	_;B	_;B	cTB	hsB	hsB	hsB	hsB	iyB	jB	l�B	n�B	p�B	s�B	v�B	w�B	�B	�B	�B	�%B	�%B	�%B	�%B	�7B	�=B	�DB	�PB	�bB	�hB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�?B	�?B	�FB	�LB	�LB	�RB	�XB	�^B	�jB	�wB	�}B	��B	��B	��B	B	ĜB	ŢB	ŢB	ƨB	ƨB	ƨB	ǮB	ǮB	�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<u<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446582012010314465820120103144658  AO  ARGQ                                                                        20111130140732  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140732  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144658  IP                  G�O�G�O�G�O�                