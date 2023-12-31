CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-01T23:12:39Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7T   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    88   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8<   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8D   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8H   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8P   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8X   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8`   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8d   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8l   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9l   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9x   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9|   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  AL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fh   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  h\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  p,   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  5900960 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130103148  20190523124442  1514_5041_002                   2C  D   APEX                            2041                            062805                          846 @��ӽ�?�1   @��ӽ�?�@6�KƧ��c<�1&�1   GPS     Primary sampling: averaged [2dbar-bin averaged]                                                                                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@y��@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dw�3Dxs3Dx�3Dys3Dy�3Dzs311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�  A�  A�  A���A�  A�  A�A�
=A�
=A�oA��A���A˝�A��A���A��A��`AʾwAʅA�l�A�^5A�A�A�  A���AɅA��AȁAǸRA���A��;A���A��A�oA�O�A��A��\A��A�(�A���A��mA��A�ƨA�r�A�+A�(�A�`BA�bA��A�ffA�l�A���A�(�A���A��TA��A���A�I�A��/A�A�&�A�ffA��mA��A��/A�x�A��/A�M�A��/A�oA��FA�?}A�?}A�
=A���A��TA���A��PA�VA�l�A�JA��A��A��
A�33A��-A�E�A���A���A���A��A���A�A�A�1'A�G�A�VA��A�%A�$�A��A�r�A�K�A�?}A�ZA��A���A�x�A�=qA�t�A�^5A�$�A�ffA�^5A���A�-A+A~  A}O�A{��AzZAyx�AxI�Av9XAt��AtbNAs�#Ar��Aop�Am�#Ak��Aj  Ai?}Ag�Ae?}Abz�A`�/A_�-A_G�A]�A[/AZA�AXĜAXJAU&�AQ|�AP1'AN�`AN�DANI�AMt�AL��AK�AK"�AJE�AI"�AH  AF�AFjAE�AEx�AD��ABA@ȴA@bNA?�7A>�A<5?A;�A9�A8��A8E�A7��A7�A61A4��A3�
A3?}A2�A2ZA1��A0��A/x�A. �A-7LA,�jA,�+A,v�A,Q�A,^5A,-A+�wA+
=A)��A)"�A(��A({A'
=A&M�A%�#A%�PA%VA$9XA"�A"  A!�7A!VA ��AO�AVA�A;dA�DA1'A�
A�!A^5A�^A33A
=A��A��AA��A�FA��A��A�RA  AhsAI�AAVA��A?}A	Al�A^5A��A��A-A�#A��A�#A �`@�t�@�Z@�{@���@��@�+@�`B@@��@�  @�V@�{@��@�@���@㝲@��@�1@ݙ�@ە�@�7L@���@�;d@�^5@�Ĝ@Ӯ@�ff@ϕ�@�$�@�G�@̼j@�z�@�=q@�G�@��;@�ȴ@Ų-@ċD@� �@���@�K�@�^5@��@�/@��@���@���@���@�v�@��j@�|�@�@�V@���@� �@�ƨ@��^@��@�@��@��@�bN@�Z@�I�@�(�@��@��\@��j@��@�
=@��y@��H@��@���@�;d@��m@��@��@���@�V@�5?@�hs@�I�@�+@��\@���@��`@�1'@�ƨ@�l�@�
=@��+@��@��T@��h@��@��@�1'@�Q�@�Q�@��D@���@�ȴ@�E�@��#@��@�(�@�1'@��P@�;d@�j@�1'@��m@�|�@��@��+@��T@��@��u@��@��;@�G�@���@�5?@�M�@���@�v�@�^5@�@��@�p�@�%@��@��/@���@��9@��@���@��@�j@�Q�@��
@��m@�j@���@��@�7L@��^@��@���@���@��7@��^@���@���@�J@�J@��@���@�@�@�@��-@���@��7@�x�@�`B@�`B@�X@��@��`@��9@�9X@�b@��@�ƨ@���@�|�@�dZ@�C�@��@��R@��\@�M�@�5?@��@��@���@��-@���@�p�@�O�@�7L@��@��@��@�bN@�A�@���@���@���@�l�@�C�@�
=@�ȴ@���@�V@�-@��@��#@���@��@�G�@�%@���@�r�@�Q�@�9X@���@��m@��w@��@�t�@�l�@�dZ@�33@���@��H@��@��!@�^5@�5?@��#@�x�@�G�@�&�@��/@�Q�@��@�l�@��@��@�v�@�^5@�E�@���@�@��h@�x�@�X@���@�z�@�Z@�9X@� �@��@�b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�  A�  A�  A���A�  A�  A�A�
=A�
=A�oA��A���A˝�A��A���A��A��`AʾwAʅA�l�A�^5A�A�A�  A���AɅA��AȁAǸRA���A��;A���A��A�oA�O�A��A��\A��A�(�A���A��mA��A�ƨA�r�A�+A�(�A�`BA�bA��A�ffA�l�A���A�(�A���A��TA��A���A�I�A��/A�A�&�A�ffA��mA��A��/A�x�A��/A�M�A��/A�oA��FA�?}A�?}A�
=A���A��TA���A��PA�VA�l�A�JA��A��A��
A�33A��-A�E�A���A���A���A��A���A�A�A�1'A�G�A�VA��A�%A�$�A��A�r�A�K�A�?}A�ZA��A���A�x�A�=qA�t�A�^5A�$�A�ffA�^5A���A�-A+A~  A}O�A{��AzZAyx�AxI�Av9XAt��AtbNAs�#Ar��Aop�Am�#Ak��Aj  Ai?}Ag�Ae?}Abz�A`�/A_�-A_G�A]�A[/AZA�AXĜAXJAU&�AQ|�AP1'AN�`AN�DANI�AMt�AL��AK�AK"�AJE�AI"�AH  AF�AFjAE�AEx�AD��ABA@ȴA@bNA?�7A>�A<5?A;�A9�A8��A8E�A7��A7�A61A4��A3�
A3?}A2�A2ZA1��A0��A/x�A. �A-7LA,�jA,�+A,v�A,Q�A,^5A,-A+�wA+
=A)��A)"�A(��A({A'
=A&M�A%�#A%�PA%VA$9XA"�A"  A!�7A!VA ��AO�AVA�A;dA�DA1'A�
A�!A^5A�^A33A
=A��A��AA��A�FA��A��A�RA  AhsAI�AAVA��A?}A	Al�A^5A��A��A-A�#A��A�#A �`@�t�@�Z@�{@���@��@�+@�`B@@��@�  @�V@�{@��@�@���@㝲@��@�1@ݙ�@ە�@�7L@���@�;d@�^5@�Ĝ@Ӯ@�ff@ϕ�@�$�@�G�@̼j@�z�@�=q@�G�@��;@�ȴ@Ų-@ċD@� �@���@�K�@�^5@��@�/@��@���@���@���@�v�@��j@�|�@�@�V@���@� �@�ƨ@��^@��@�@��@��@�bN@�Z@�I�@�(�@��@��\@��j@��@�
=@��y@��H@��@���@�;d@��m@��@��@���@�V@�5?@�hs@�I�@�+@��\@���@��`@�1'@�ƨ@�l�@�
=@��+@��@��T@��h@��@��@�1'@�Q�@�Q�@��D@���@�ȴ@�E�@��#@��@�(�@�1'@��P@�;d@�j@�1'@��m@�|�@��@��+@��T@��@��u@��@��;@�G�@���@�5?@�M�@���@�v�@�^5@�@��@�p�@�%@��@��/@���@��9@��@���@��@�j@�Q�@��
@��m@�j@���@��@�7L@��^@��@���@���@��7@��^@���@���@�J@�J@��@���@�@�@�@��-@���@��7@�x�@�`B@�`B@�X@��@��`@��9@�9X@�b@��@�ƨ@���@�|�@�dZ@�C�@��@��R@��\@�M�@�5?@��@��@���@��-@���@�p�@�O�@�7L@��@��@��@�bN@�A�@���@���@���@�l�@�C�@�
=@�ȴ@���@�V@�-@��@��#@���@��@�G�@�%@���@�r�@�Q�@�9X@���@��m@��w@��@�t�@�l�@�dZ@�33@���@��H@��@��!@�^5@�5?@��#@�x�@�G�@�&�@��/@�Q�@��@�l�@��@��@�v�@�^5@�E�@���@�@��h@�x�@�X@���@�z�@�Z@�9X@� �@��@�b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�LB�LB�LB�RB�LB�LB�LB�qB�^B��B�B��B$�B[#BffBp�Bw�Bv�Bu�Bw�By�By�Bt�Br�Bs�BiyBbNBR�BF�B-B{B1B��B�B�B�B�B�B�B�B�B��B��B��B%BBPB\B	7B��B��B��B��B��B��B��B��B��B�yB�5B�B�)B�BB�B��B��B�BɺB�RB�-B�B��B��B��B�PB~�Bm�BW
BA�B33B)�B�B��B�B�HB�BɺB�^B��B�uB�%Bw�Be`BM�BI�BA�B7LB+B�B\BuB�B1B
��B
�NB
��B
�jB
��B
�VB
�1B
� B
q�B
aHB
XB
N�B
H�B
@�B
:^B
0!B
%�B
�B
hB
+B
B
  B	��B	�HB	�
B	ȴB	�wB	�LB	�B	��B	�\B	�+B	�B	�B	�B	m�B	ffB	_;B	YB	B�B	33B	'�B	$�B	%�B	$�B	$�B	%�B	%�B	!�B	!�B	�B	oB	\B	JB	DB	%B	%B��B��B��B��B�B�B�`B�NB�;B�)B�B��B��BȴBÖB�wB�dB�LB�?B�B��B��B��B��B��B��B��B�B�RB�XB�FB�-B�B�B��B��B��B��B��B��B�uB�hB�PB�PB�DB�%B�B�B�B{�B}�Bv�Bu�Bm�Br�Bl�Bk�BiyBhsBffBffBffBe`B_;B]/B]/B[#BVBQ�BQ�BQ�BK�BF�BD�B?}B=qB<jB;dB:^B9XB7LB33B0!B-B,B-B+B(�B&�B#�B#�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuB�B{B�B�B{B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B!�B#�B&�B'�B'�B'�B)�B(�B-B1'B7LB9XB:^B;dB;dB:^B:^B:^B<jBA�BE�BK�BVB]/BaHBdZBo�B� B�%B�1B�PB�VB�VB�\B�oB�{B��B��B��B��B��B�B�B�!B�-B�9B�?B�qB�}BǮB��B��B�B�)B�BB�BB�;B�;B�ZB�fB�mB�sB��B��B��B��B��B��B��B��B��B��B	B	�B	�B	!�B	&�B	+B	/B	1'B	1'B	1'B	49B	8RB	9XB	:^B	=qB	>wB	?}B	A�B	B�B	J�B	P�B	S�B	XB	aHB	dZB	hsB	o�B	w�B	z�B	z�B	z�B	z�B	|�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�1B	�7B	�7B	�=B	�JB	�VB	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�9B	�FB	�LB	�LB	�RB	�XB	�^B	�dB	�wB	��B	��B	B	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�
B	�B	�#B	�#B	�#B	�/B	�;B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�HB	�HB	�NB	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�LB�LB�LB�RB�LB�LB�LB�qB�^B��B�B��B'�B\)BgmBq�Bw�Bw�Bv�Bx�By�Bz�Bu�Bs�Bu�Bk�BdZBVBI�B1'B�BPB��B��B��B�B�B�B��B��B��B��B��BB	7BBVBhBVB��B��B��B��B��B��B��B  BB�B�TB�/B�)B�BB��BB��B��B��B�XB�9B�!B��B��B��B�bB�Br�B[#BG�B8RB.B!�B��B�B�TB�#B��B�}B�B��B�=B|�Bn�BO�BK�BE�B;dB0!B�BbBuB�BJB
��B
�fB
�
B
B
��B
�oB
�DB
�B
w�B
dZB
ZB
Q�B
J�B
D�B
>wB
33B
)�B
"�B
{B
	7B
%B
B	��B	�`B	�/B	��B	��B	�dB	�9B	��B	�uB	�=B	�B	�B	�+B	o�B	iyB	`BB	^5B	H�B	5?B	+B	%�B	&�B	%�B	&�B	'�B	'�B	#�B	#�B	�B	�B	bB	PB	JB	1B	DB��B��B��B��B��B�B�yB�ZB�HB�5B�#B�B��B��BŢB�}B�jB�XB�RB�'B��B��B��B��B��B��B��B�B�^B�dB�XB�9B�B�B��B��B��B��B��B��B��B�{B�VB�VB�JB�7B�B�B�B}�B~�B{�Bw�Bn�Bs�Bm�Bk�Bk�BjBgmBhsBiyBhsBaHB_;B_;B]/BYBR�BS�BR�BO�BK�BH�BA�B?}B>wB=qB;dB:^B<jB6FB33B2-B0!B0!B/B+B)�B&�B'�B!�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B#�B$�B%�B)�B(�B(�B(�B)�B,B0!B49B9XB:^B;dB;dB;dB;dB;dB=qB?}BC�BF�BL�BVB]/BbNBdZBn�B� B�%B�=B�VB�VB�\B�hB�{B��B��B��B��B��B��B�B�B�'B�3B�?B�FB�qB��BǮB��B��B�B�5B�HB�BB�BB�BB�ZB�mB�sB�fB��B��B��B��B��B��B��B��B��B��B	B	�B	�B	!�B	&�B	+B	/B	1'B	2-B	2-B	5?B	8RB	9XB	:^B	=qB	?}B	?}B	A�B	C�B	J�B	Q�B	S�B	W
B	aHB	dZB	gmB	n�B	w�B	z�B	{�B	z�B	z�B	|�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�1B	�7B	�7B	�=B	�JB	�VB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�?B	�FB	�LB	�RB	�XB	�^B	�^B	�dB	�}B	��B	B	B	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�)B	�)B	�)B	�5B	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�TB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201112181657212011121816572120111218165721  AO  ARGQ                                                                        20111130103148  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130103148  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20111218165721  IP                  G�O�G�O�G�O�                