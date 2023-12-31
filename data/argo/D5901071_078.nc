CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:12Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               NA   AO  20111130140913  20190522121826  1727_5046_078                   2C  D   APEX                            2143                            040306                          846 @�zyP��1   @�zy����@8����S��d�l�C�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D	  D	� D
  D
y�D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4�fD5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�3Dy� D�  D�i�D���D��3D�  D�ffD��3D��fD�3D�L�D���D���D�#3D�P Dڙ�D��D�&fD�ffD�3D�,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@y��@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C�fC��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D��Ds3D�3D	s3D	�3D
l�D
�3Ds3D�3Ds3D�3Dl�D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1y�D1�3D2s3D2�3D3s3D3�3D4y�D4��D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXy�DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�fDy�3D���D�c3D��fD���D��D�` D���D�� D��D�FfD��3D��fD��D�I�Dړ3D��3D�  D�` D��D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ƨAɲ-AɅA�n�A�`BA�Q�A�I�A�A�A�=qA��A��AȶFA�XA���A�M�A���A�$�AŋDA�VA�S�A�
=A�oA��
A��
A�&�A�K�A���A�=qA�ȴA��7A��A��A���A�dZA�t�A�p�A�Q�A�  A��A�
=A��uA�l�A�K�A��A�A�dZA��7A���A�XA�+A��9A���A�ȴA��9A�^5A��RA�7LA�%A���A��-A�  A��+A�7LA�M�A�JA�9XA��A���A�XA�oA��wA�E�A��+A�|�A�JA�l�A�33A��A�jA��uA�+A��A�z�A�"�A��;A�bNA��A�I�A��A���A�x�A�v�A�~�A��+A��!A�\)A�A��#A���A��A��7A��AƨA~M�A|�RA{\)A{oAz�Axn�Av��Av �At��AsdZAr�Ap�HAnn�Al��Ak33Aj$�Ail�Ah�!Agp�Ae�AdbAc�wAb��Aa�PA`��A`�A_�^A_&�A^1'A]�A\^5A[\)AYt�AWt�AV�`AV��AV5?AU/ATĜAT9XAS�ASVAQAO�#AM�mAMƨAMdZALz�AK;dAJ(�AH��AG�AFE�AE�;AE�AE��AD��AD�AD��AD1AA��A@��A?��A?/A>�RA>v�A=�A<�A;�7A9�;A9O�A8��A8�A7�A6�`A6bA4�uA3�A3+A2�A1A1��A1VA0  A/x�A/;dA.��A-�wA-7LA,��A,bNA+��A*��A)�;A)oA(��A(bNA(bA&��A%C�A$��A$ZA#�TA!O�AAC�AQ�AoAVA��A�AĜA`BA�A�`A�A��A�TA~�A�AbNA�hA
~�A	��A	\)A��A��A1A�PA��A �A��A|�AG�A z�@���@�C�@��T@�p�@�/@�1@���@��+@���@�S�@�v�@�$�@�hs@��/@�P@�hs@���@�V@�@�n�@�/@�Q�@��@�w@�^5@��@�n�@��@�O�@�(�@�-@�@�?}@܋D@�"�@ى7@� �@�ff@�(�@���@с@Ͼw@�@Χ�@�n�@�@�O�@�bN@��
@�K�@�V@�(�@Ų-@�  @�t�@�o@+@��@��@��`@�A�@���@��@�&�@��@�|�@���@�ff@��#@�G�@�j@�ȴ@�=q@�@��7@�%@��u@�b@��;@�|�@��R@��^@���@�%@�&�@�O�@��7@���@�@���@��9@��@�V@��m@��m@�A�@��@���@��@�Ĝ@�Ĝ@���@���@�@�ff@���@��+@��#@��@�9X@��@�
=@���@�@�`B@��/@�z�@�\)@���@��@�\)@�M�@�M�@�V@���@�hs@�X@�V@��@�1'@���@���@�"�@��H@���@��!@��!@�v�@�J@���@���@���@���@�p�@��@��7@�X@��@��u@�1'@�&�@�9X@��
@�ƨ@���@�G�@�^5@�^5@��y@�|�@��;@��@���@��@��`@�Ĝ@�bN@�(�@��@��w@�t�@�ff@���@��u@��@���@���@��9@���@�r�@���@�K�@��@�ȴ@���@�hs@�O�@��@���@�I�@�l�@�
=@���@��@��@��@��@��@��@��R@�=q@���@���@�`B@�r�@�9X@� �@��@���@�ƨ@�|�@�;d@���@�v�@���@��7@��@�`B@��j@�I�@��@�  @�ƨ@��@���@���@��@���@�I�@��@�C�@��\@��T@��h@�&�@���@��/@���@�j@�b@���@��;@�ƨ@��F@���@��@��@�l�@�C�@���@z�@n��@e�h@]�h@V�+@Ol�@I&�@@  @<Z@8Q�@1�@+��@%�@"~�@��@V@%@
��@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨAɲ-AɅA�n�A�`BA�Q�A�I�A�A�A�=qA��A��AȶFA�XA���A�M�A���A�$�AŋDA�VA�S�A�
=A�oA��
A��
A�&�A�K�A���A�=qA�ȴA��7A��A��A���A�dZA�t�A�p�A�Q�A�  A��A�
=A��uA�l�A�K�A��A�A�dZA��7A���A�XA�+A��9A���A�ȴA��9A�^5A��RA�7LA�%A���A��-A�  A��+A�7LA�M�A�JA�9XA��A���A�XA�oA��wA�E�A��+A�|�A�JA�l�A�33A��A�jA��uA�+A��A�z�A�"�A��;A�bNA��A�I�A��A���A�x�A�v�A�~�A��+A��!A�\)A�A��#A���A��A��7A��AƨA~M�A|�RA{\)A{oAz�Axn�Av��Av �At��AsdZAr�Ap�HAnn�Al��Ak33Aj$�Ail�Ah�!Agp�Ae�AdbAc�wAb��Aa�PA`��A`�A_�^A_&�A^1'A]�A\^5A[\)AYt�AWt�AV�`AV��AV5?AU/ATĜAT9XAS�ASVAQAO�#AM�mAMƨAMdZALz�AK;dAJ(�AH��AG�AFE�AE�;AE�AE��AD��AD�AD��AD1AA��A@��A?��A?/A>�RA>v�A=�A<�A;�7A9�;A9O�A8��A8�A7�A6�`A6bA4�uA3�A3+A2�A1A1��A1VA0  A/x�A/;dA.��A-�wA-7LA,��A,bNA+��A*��A)�;A)oA(��A(bNA(bA&��A%C�A$��A$ZA#�TA!O�AAC�AQ�AoAVA��A�AĜA`BA�A�`A�A��A�TA~�A�AbNA�hA
~�A	��A	\)A��A��A1A�PA��A �A��A|�AG�A z�@���@�C�@��T@�p�@�/@�1@���@��+@���@�S�@�v�@�$�@�hs@��/@�P@�hs@���@�V@�@�n�@�/@�Q�@��@�w@�^5@��@�n�@��@�O�@�(�@�-@�@�?}@܋D@�"�@ى7@� �@�ff@�(�@���@с@Ͼw@�@Χ�@�n�@�@�O�@�bN@��
@�K�@�V@�(�@Ų-@�  @�t�@�o@+@��@��@��`@�A�@���@��@�&�@��@�|�@���@�ff@��#@�G�@�j@�ȴ@�=q@�@��7@�%@��u@�b@��;@�|�@��R@��^@���@�%@�&�@�O�@��7@���@�@���@��9@��@�V@��m@��m@�A�@��@���@��@�Ĝ@�Ĝ@���@���@�@�ff@���@��+@��#@��@�9X@��@�
=@���@�@�`B@��/@�z�@�\)@���@��@�\)@�M�@�M�@�V@���@�hs@�X@�V@��@�1'@���@���@�"�@��H@���@��!@��!@�v�@�J@���@���@���@���@�p�@��@��7@�X@��@��u@�1'@�&�@�9X@��
@�ƨ@���@�G�@�^5@�^5@��y@�|�@��;@��@���@��@��`@�Ĝ@�bN@�(�@��@��w@�t�@�ff@���@��u@��@���@���@��9@���@�r�@���@�K�@��@�ȴ@���@�hs@�O�@��@���@�I�@�l�@�
=@���@��@��@��@��@��@��@��R@�=q@���@���@�`B@�r�@�9X@� �@��@���@�ƨ@�|�@�;d@���@�v�@���@��7@��@�`B@��j@�I�@��@�  @�ƨ@��@���@���@��@���@�I�@��@�C�@��\@��T@��h@�&�@���@��/@���@�j@�b@���@��;@�ƨ@��F@���@��@��@�l�@�C�@���@z�@n��@e�h@]�h@V�+@Ol�@I&�@@  @<Z@8Q�@1�@+��@%�@"~�@��@V@%@
��@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB`BB`BB`BBaHBaHBaHBbNBcTBbNBaHBaHB`BB^5B[#BZBZBYBYBZB[#BZBT�BS�BJ�B=qB5?B,B#�B�BPBB��B�B�ZB�)B��B��BŢB�dB�FB�-B�B�B�B�B�B��B�3B�!B�B��B��B��B��B��B�oB�=B{�BjB^5BT�BK�BD�B5?B$�B�BbB%BB��B��B��B�BB��B�wB�FB�3B��B��B�PB�1B�B~�By�Bu�Bl�BYBM�B9XB&�B{B%B
��B
�B
�BB
��B
ƨB
�^B
��B
��B
�bB
�B
y�B
o�B
ffB
`BB
]/B
W
B
N�B
G�B
C�B
;dB
5?B
/B
$�B
�B
bB

=B
%B
B	��B	��B	�B	�mB	�ZB	�;B	�B	��B	��B	��B	��B	ƨB	��B	�dB	�?B	�B	��B	��B	��B	��B	��B	��B	�{B	�\B	�=B	� B	v�B	p�B	n�B	jB	e`B	^5B	ZB	S�B	N�B	K�B	I�B	H�B	F�B	C�B	B�B	@�B	;dB	2-B	/B	,B	(�B	'�B	%�B	"�B	�B	�B	oB	\B	PB	
=B	%B	  B��B�B�B�B�B�sB�mB�ZB�HB�BB�;B�/B�#B�B��B��B��B��B��BɺBȴBƨBĜB�wB�^B�LB�?B�!B��B��B��B��B��B��B�uB�hB�PB�7B�%B�B}�Bx�Bs�Bp�Bn�Bl�BiyBgmBe`BcTBaHB_;B]/B\)BZBW
BT�BS�BR�BQ�BP�BO�BN�BN�BM�BL�BL�BK�BI�BJ�BJ�BJ�BJ�BI�BH�BE�BF�BE�BC�BB�BC�BC�BB�BA�B@�B?}B@�B?}B?}B>wB>wB=qB>wB<jB<jB<jB;dB:^B9XB;dB=qB>wB?}B@�B@�B@�B@�BA�B@�B@�B?}B@�BB�BE�BE�BE�BE�BH�BI�BR�BT�BVBW
B[#B\)BbNBbNBcTBdZBffBiyBn�Bo�Bo�Bp�Br�Bt�Bx�B|�B{�Bz�B}�B�+B�bB�oB�oB�uB�{B��B��B�{B�{B�oB��B��B��B��B��B��B��B��B�B�B�LB�wBĜB��B��B�B�5B�HB�HB�NB�TB�ZB�mB�mB�mB�fB�yB�B��B��B��B��B��B��B��B	B	
=B	\B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	!�B	#�B	%�B	'�B	+B	,B	-B	,B	,B	.B	5?B	5?B	5?B	:^B	>wB	K�B	W
B	ZB	^5B	cTB	ffB	l�B	o�B	o�B	p�B	q�B	s�B	t�B	w�B	w�B	v�B	s�B	q�B	w�B	z�B	~�B	�B	�B	�B	�B	�%B	�%B	�%B	�%B	�1B	�7B	�7B	�7B	�=B	�DB	�VB	�bB	�hB	�hB	�hB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�3B	�9B	�FB	ǮB	�sB	��B
\B
{B
�B
�B
"�B
-B
6FB
>wB
E�B
K�B
P�B
W
B
\)B
aHB
ffB
n�B
t�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B`BBaHB`BBaHBaHBaHBbNBcTBcTBbNBbNBbNBaHB^5B]/B^5B\)B[#B]/B`BB^5BVBXBQ�BA�B8RB2-B+B�BoBDB��B�B�mB�;B�B��B��B��B�RB�LB�!B�!B�B�!B�B�B�?B�'B�'B�B��B��B��B��B��B�hB�Bo�BbNBXBM�BJ�B=qB)�B�B{B	7BB  B��B��B�fB�B��B�LB�?B�B��B�\B�=B�B�Bz�Bw�Br�B\)BT�B?}B,B�BDB
��B
�B
�mB
��B
��B
��B
�B
��B
��B
�=B
~�B
t�B
jB
aHB
`BB
\)B
S�B
J�B
H�B
?}B
7LB
6FB
,B
�B
uB
JB
1B
B
B	��B	�B	�sB	�fB	�TB	�#B	�B	��B	��B	��B	ɺB	ÖB	�wB	�^B	�9B	��B	��B	��B	��B	��B	��B	��B	�hB	�VB	�%B	|�B	q�B	p�B	m�B	iyB	bNB	_;B	YB	Q�B	L�B	J�B	I�B	I�B	D�B	C�B	C�B	B�B	6FB	2-B	/B	+B	(�B	'�B	&�B	"�B	�B	{B	bB	\B	PB	
=B	B	  B��B�B�B�B�yB�B�sB�TB�HB�HB�HB�/B�B�B��B��B��B��B��BɺBǮBȴBB�jB�RB�LB�LB�B��B��B��B��B��B�{B�uB�hB�PB�7B�+B�B}�Bx�Bt�Bq�Bo�Bm�BiyBgmBe`BgmBbNB_;B^5B]/B\)BXBT�BT�BS�BQ�BQ�BO�BO�BO�BN�BM�BL�BM�BK�BK�BK�BK�BK�BK�BJ�BG�BH�BE�BD�BE�BD�BC�BD�BD�BB�BA�B@�BA�BA�B?}B>wB>wB?}B?}B?}B>wB:^B<jB=qB@�B?}B@�B@�BA�BB�BB�BB�BA�BB�BC�BD�BE�BF�BF�BF�BG�BJ�BL�BS�BVBW
BYB\)B^5BcTBcTBdZBe`BhsBk�Bo�Bp�Bp�Bq�Bs�Bu�By�B}�B}�B|�B~�B�+B�bB�oB�oB�uB�{B��B��B��B��B�{B��B��B��B��B��B��B��B��B�B�B�FB�wBĜB��B�
B�)B�BB�HB�NB�TB�ZB�`B�mB�yB�yB�yB�B�B��B��B��B��B��B��B��B	%B	
=B	bB	�B	�B	�B	�B	�B	�B	�B	"�B	!�B	!�B	#�B	%�B	'�B	+B	,B	.B	-B	-B	-B	7LB	6FB	5?B	:^B	<jB	I�B	W
B	YB	]/B	bNB	e`B	l�B	o�B	o�B	p�B	r�B	t�B	u�B	x�B	x�B	x�B	v�B	r�B	w�B	z�B	~�B	�B	�B	�B	�%B	�+B	�+B	�+B	�1B	�7B	�7B	�=B	�=B	�DB	�PB	�\B	�bB	�hB	�hB	�hB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�3B	�9B	�FB	ǮB	�sB	��B
\B
{B
�B
�B
"�B
-B
6FB
>wB
E�B
K�B
P�B
W
B
\)B
aHB
gmB
n�B
t�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447012012010314470120120103144701  AO  ARGQ                                                                        20111130140913  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140913  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144701  IP                  G�O�G�O�G�O�                