CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:04Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               /A   AO  20111130140132  20190522121825  1727_5046_047                   2C  D   APEX                            2143                            040306                          846 @�Re㾿�1   @�Rf�@
@7U\(��c�j~��#1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C3�fC5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg�fDh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dy��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @,��@s33@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'��B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3�3C5�3C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	��D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!y�D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgy�Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dly�Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dy��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AŃAŇ+AŋDAŋDAőhAŏ\AōPAœuAœuAŕ�Aŕ�Aŗ�Aŗ�Aŗ�Aŗ�Aŕ�Aŕ�Aŕ�Aŕ�Aŗ�Aŗ�Ař�Ař�Ař�Aś�Ař�Aś�Ař�Aŝ�Aş�Aş�Aŝ�Aš�Aŝ�Aŝ�Aŝ�Aş�Aŝ�Aś�Aś�AŅA���A´9A�Q�A�jA��A��A��A�M�A���A��mA��A�33A���A��yA�$�A�hsA�E�A�$�A��A�x�A��A���A��A�33A�$�A��A���A��jA��#A��A��A�A�n�A�\)A�5?A�O�A��A��RA�"�A�
=A�;dA�A�A���A��/A�+A�G�A�$�A��A��jA�{A��A���A��A�dZA���A�oA�A�A���A���A�r�A�`BA�jA�A��A�ȴA�ȴA�XA��`A�ĜA��PA���A�A}oA{��AyS�Aw��AvI�At�As/Ap��An(�Al^5AiAg�AeAcp�AaS�A`v�A_?}A\v�A[�hAZ�AZ  AX��AX^5AWt�AVjAU��AT��AT~�AS?}AR�/AR��AQ�AP�AN�`AL�AJ�jAI33AHr�AGl�AE��AEx�AES�AE%ADZAB�yAA�FA?�TA>=qA=x�A<bNA;�A;��A;+A:��A:$�A9�A9p�A8M�A6��A5�TA4�A4jA3��A29XA0�A/��A/�A-�A-
=A+�^A+7LA*r�A*E�A*1A)t�A);dA(�DA'��A'O�A&z�A%�hA%�A$��A$^5A$(�A#S�A"^5A!�PA �/A 5?A�
Ap�A&�A�
AAjA(�Ap�A��A��AbA�Ap�A;dAn�A�
A�AjA�A�FAG�A�A��A�A��A�A=qA��AZA��A
�A
9XA	&�An�A�AM�A�yA�AO�@��P@��@��@���@��F@��u@�\@�@��m@��@�^5@��/@�33@�^@�K�@旍@�v�@��@�V@�5?@�j@��m@ۅ@���@��@��@�l�@Չ7@�I�@�V@�&�@Л�@�"�@͙�@�bN@�v�@�p�@� �@�@ŉ7@�9X@�@��@�O�@�I�@��
@�33@���@��@���@�ƨ@�ȴ@�J@��@�Z@�;d@�=q@�V@�Q�@��@���@��-@��`@�ƨ@��-@�j@���@�-@���@���@���@�K�@�ff@���@���@�hs@��/@�A�@���@�\)@���@�ff@�^5@���@�hs@��@��@�bN@�(�@�9X@��F@�S�@�
=@�ȴ@�v�@�=q@�$�@��@�G�@�j@���@��@�ȴ@���@��@�V@��^@�X@�7L@�&�@�%@���@�Ĝ@�Ĝ@��@��@���@��P@�S�@�@���@��R@�n�@�E�@�J@��^@��`@�(�@�b@���@���@�t�@�
=@���@�^5@��@��h@�x�@�p�@�hs@�O�@���@���@��@�9X@��@���@��;@��
@��
@�  @�1@�1@���@��@��@�A�@�ƨ@�\)@�S�@�\)@�\)@�K�@�33@�"�@��@�
=@��H@��!@���@��+@�=q@���@�@��#@���@��-@�@��-@�x�@�G�@��@��@��
@��@�
=@��!@���@�n�@�$�@��@��T@�`B@�O�@�Ĝ@��j@��D@�b@��w@�K�@�;d@�+@�o@��@���@�M�@�-@�@��^@���@�?}@��@���@���@�z�@�Q�@�I�@�9X@�1'@�(�@� �@���@�\)@�C�@�o@��y@��!@��\@�v�@�^5@�5?@��@�{@�@���@���@��h@��@�`B@�7L@�V@��`@���@��D@}p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AŃAŇ+AŋDAŋDAőhAŏ\AōPAœuAœuAŕ�Aŕ�Aŗ�Aŗ�Aŗ�Aŗ�Aŕ�Aŕ�Aŕ�Aŕ�Aŗ�Aŗ�Ař�Ař�Ař�Aś�Ař�Aś�Ař�Aŝ�Aş�Aş�Aŝ�Aš�Aŝ�Aŝ�Aŝ�Aş�Aŝ�Aś�Aś�AŅA���A´9A�Q�A�jA��A��A��A�M�A���A��mA��A�33A���A��yA�$�A�hsA�E�A�$�A��A�x�A��A���A��A�33A�$�A��A���A��jA��#A��A��A�A�n�A�\)A�5?A�O�A��A��RA�"�A�
=A�;dA�A�A���A��/A�+A�G�A�$�A��A��jA�{A��A���A��A�dZA���A�oA�A�A���A���A�r�A�`BA�jA�A��A�ȴA�ȴA�XA��`A�ĜA��PA���A�A}oA{��AyS�Aw��AvI�At�As/Ap��An(�Al^5AiAg�AeAcp�AaS�A`v�A_?}A\v�A[�hAZ�AZ  AX��AX^5AWt�AVjAU��AT��AT~�AS?}AR�/AR��AQ�AP�AN�`AL�AJ�jAI33AHr�AGl�AE��AEx�AES�AE%ADZAB�yAA�FA?�TA>=qA=x�A<bNA;�A;��A;+A:��A:$�A9�A9p�A8M�A6��A5�TA4�A4jA3��A29XA0�A/��A/�A-�A-
=A+�^A+7LA*r�A*E�A*1A)t�A);dA(�DA'��A'O�A&z�A%�hA%�A$��A$^5A$(�A#S�A"^5A!�PA �/A 5?A�
Ap�A&�A�
AAjA(�Ap�A��A��AbA�Ap�A;dAn�A�
A�AjA�A�FAG�A�A��A�A��A�A=qA��AZA��A
�A
9XA	&�An�A�AM�A�yA�AO�@��P@��@��@���@��F@��u@�\@�@��m@��@�^5@��/@�33@�^@�K�@旍@�v�@��@�V@�5?@�j@��m@ۅ@���@��@��@�l�@Չ7@�I�@�V@�&�@Л�@�"�@͙�@�bN@�v�@�p�@� �@�@ŉ7@�9X@�@��@�O�@�I�@��
@�33@���@��@���@�ƨ@�ȴ@�J@��@�Z@�;d@�=q@�V@�Q�@��@���@��-@��`@�ƨ@��-@�j@���@�-@���@���@���@�K�@�ff@���@���@�hs@��/@�A�@���@�\)@���@�ff@�^5@���@�hs@��@��@�bN@�(�@�9X@��F@�S�@�
=@�ȴ@�v�@�=q@�$�@��@�G�@�j@���@��@�ȴ@���@��@�V@��^@�X@�7L@�&�@�%@���@�Ĝ@�Ĝ@��@��@���@��P@�S�@�@���@��R@�n�@�E�@�J@��^@��`@�(�@�b@���@���@�t�@�
=@���@�^5@��@��h@�x�@�p�@�hs@�O�@���@���@��@�9X@��@���@��;@��
@��
@�  @�1@�1@���@��@��@�A�@�ƨ@�\)@�S�@�\)@�\)@�K�@�33@�"�@��@�
=@��H@��!@���@��+@�=q@���@�@��#@���@��-@�@��-@�x�@�G�@��@��@��
@��@�
=@��!@���@�n�@�$�@��@��T@�`B@�O�@�Ĝ@��j@��D@�b@��w@�K�@�;d@�+@�o@��@���@�M�@�-@�@��^@���@�?}@��@���@���@�z�@�Q�@�I�@�9X@�1'@�(�@� �@���@�\)@�C�@�o@��y@��!@��\@�v�@�^5@�5?@��@�{@�@���@���@��h@��@�`B@�7L@�V@��`@���@��D@}p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�bB�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�VB�VB�VB�JB�=B��B��B��B�B�3B�9B�3B�'B�B�B�B�B�B�-B�9B�9B�FB�FB�?B�-B�-B�B��B��B��B�uB�+B{�Bl�BC�B6FB/B�BoB	7BB��B��B�sB�)B��BǮB�XB��B��B�Bn�Be`B\)BN�B@�B:^B0!B(�B �B�BoB
=B%B
��B
�B
�B
�HB
��B
ƨB
�?B
��B
��B
��B
�VB
�B
u�B
k�B
ZB
N�B
D�B
9XB
+B
uB	��B	�B	�/B	�B	��B	��B	ǮB	ÖB	�jB	�-B	�3B	�!B	�B	��B	��B	��B	��B	��B	�\B	�DB	�+B	�B	�B	� B	x�B	m�B	bNB	T�B	I�B	D�B	>wB	8RB	5?B	33B	1'B	,B	#�B	�B	�B	�B	uB	\B	JB	
=B	+B	B	B	B��B��B��B��B�B�B�B�`B�5B�B��B��B��B��BȴBƨBƨBĜBB��B�wB�jB�^B�FB�9B�3B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B�uB�bB�PB�DB�=B�7B�7B�+B�%B�B�B�B� B~�B|�Bz�Bx�Bw�Bu�Br�Bn�BjBgmBdZBbNB_;B\)BYBVBP�BM�BI�BE�BB�B@�B=qB>wB<jB8RB8RB6FB8RB8RB7LB5?B1'B/B/B/B-B)�B(�B)�B)�B)�B(�B(�B'�B(�B(�B)�B)�B,B,B+B,B,B-B.B/B0!B1'B1'B2-B2-B33B33B5?B7LB7LB7LB7LB9XB;dB=qB>wB@�BB�BD�BF�BI�BJ�BJ�BM�BP�BR�BS�BZB]/BaHBcTBdZBdZBgmBl�Bo�Bq�Bs�Bs�Bu�Bx�B|�B}�B~�B�B�%B�1B�1B�7B�=B�DB�\B�bB�oB�uB�uB�{B��B��B��B��B��B��B�B�FB��BBÖB��B��B�B�)B�)B�5B�BB�BB�BB�NB�`B�mB�yB�B�B�B�B��B��B	  B	  B	B	B	B	%B		7B	
=B	JB	\B	bB	uB	�B	�B	�B	�B	�B	"�B	&�B	(�B	,B	-B	/B	49B	7LB	>wB	H�B	K�B	L�B	O�B	W
B	]/B	`BB	cTB	hsB	hsB	iyB	jB	k�B	l�B	m�B	m�B	m�B	o�B	p�B	q�B	q�B	s�B	x�B	{�B	}�B	� B	�B	�%B	�%B	�%B	�+B	�1B	�1B	�7B	�=B	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�9B	�LB	�LB	�RB	�XB	�^B	�dB	�wB	��B	ĜB	ŢB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�)B	�)B	�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�bB�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�VB�VB�VB�\B��B��B�B�-B�?B�?B�LB�LB�9B�!B�!B�-B�-B�?B�?B�?B�XB�RB�LB�RB�XB�FB�!B�B��B��B��B�=B�Bx�BI�B8RB49B%�B�BDBBB��B�B�HB��B��BB��B��B�PBs�BiyBbNBVBD�B>wB33B-B%�B�B�BJBJB  B
��B
�B
�`B
�
B
��B
�jB
�B
��B
��B
��B
�+B
y�B
r�B
_;B
R�B
H�B
>wB
2-B
�B
B	��B	�NB	�/B	�)B	��B	��B	ǮB	ĜB	�?B	�FB	�-B	�!B	�B	��B	��B	��B	��B	�bB	�\B	�1B	�%B	�%B	�B	|�B	r�B	gmB	YB	K�B	G�B	B�B	9XB	6FB	49B	33B	0!B	&�B	#�B	�B	�B	�B	bB	PB	DB		7B	+B	B	B	B��B��B��B�B�B�B�yB�NB�B�B��B��B��B��BǮBǮBƨBÖBĜB��B�qB�qB�XB�FB�?B�-B�'B�'B�B�B��B��B��B��B��B��B��B��B��B��B�{B�uB�VB�JB�=B�=B�=B�1B�+B�B�B�B� B~�B}�Bz�By�Bz�Bt�Bq�Bl�BiyBgmBdZBbNB^5B[#B[#BT�BQ�BO�BJ�BF�BC�BB�B@�BA�B<jB9XB:^B:^B:^B:^B8RB49B33B0!B0!B/B0!B1'B-B+B+B+B+B)�B-B-B-B.B.B.B.B/B,B0!B0!B1'B2-B33B33B49B49B49B5?B6FB8RB8RB8RB9XB;dB=qB>wB@�BA�BD�BF�BH�BJ�BK�BL�BO�BR�BT�BW
B\)B_;BcTBdZBe`BffBiyBn�Bp�Br�Bt�Bt�Bv�By�B}�B~�B~�B�B�+B�7B�7B�=B�DB�JB�\B�hB�uB�{B�{B��B��B��B��B��B��B��B�B�LB��BBĜB��B�B�B�)B�)B�;B�BB�BB�BB�TB�fB�sB�B�B�B�B�B��B��B	B	B	B	B	B	+B	
=B	DB	PB	bB	hB	{B	�B	�B	�B	�B	�B	#�B	&�B	)�B	,B	-B	/B	49B	7LB	>wB	H�B	K�B	L�B	O�B	W
B	]/B	aHB	dZB	hsB	hsB	iyB	jB	k�B	l�B	m�B	m�B	n�B	p�B	p�B	q�B	r�B	t�B	x�B	{�B	}�B	� B	�B	�%B	�%B	�%B	�1B	�7B	�7B	�7B	�DB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�9B	�?B	�LB	�LB	�XB	�XB	�dB	�jB	�wB	B	ĜB	ŢB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446502012010314465020120103144650  AO  ARGQ                                                                        20111130140132  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140132  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144650  IP                  G�O�G�O�G�O�                