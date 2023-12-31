CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:13Z UW 3.1 conversion   
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
_FillValue                 �  A0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �x   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               PA   AO  20111130140938  20190522121826  1727_5046_080                   2C  D   APEX                            2143                            040306                          846 @�}�?�1   @�}����@8�E�����d����1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp� Dq  Dq�fDr  Dr� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�33@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B7��B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C�fC��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]�3C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6��D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Ddl�Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djy�Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm��Dns3Dn�3Dos3Do�3Dps3Dp�3Dqy�Dq�3Drs31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�O�A�G�A��HA�I�A�1A̴9A�|�A�M�A��A�
=A���A��mA���A�ĜA˾wAˣ�A�"�A��`A��AʅA��#A���A��#A�A���A�XA�hsA�r�A�r�A�G�A���A��uA�1A��/A�|�A��A�oA��!A�`BA��A�bNA�9XA��9A��`A�M�A�;dA��A���A�?}A��RA�`BA�A��A�+A�hsA��PA���A��DA���A�{A��DA�"�A���A��A�v�A�E�A�{A�ĜA���A��A�Q�A���A�I�A���A���A��;A�G�A��A�oA�`BA�K�A�{A��A��A��A�=qA���A��+A���A��wA�A��9A�7LA��A�jA��7A��HA�VA�bA��A��A��A��DA�bNA��;A��uA�"�A�%A�O�A��uA�$�A�A�VAK�A~��A~M�A|��AzE�Az{AzJAyXAw�Aw/Au�As7LAq
=Ap{An��Aj�jAh��Ae\)AbJA_��A]�-A\�HA\VA[33AX�AV�HAT�DAR�AR5?AQ�hAPjAO"�AN�+AN1AL��AK��AI�7AH��AHr�AGt�AG�AE33AC33AB�uABVABffAAt�A@(�A>�A>=qA>$�A=��A=�;A=C�A<��A<  A;��A:�A8��A8-A7dZA6z�A6��A6�!A6r�A5A4~�A3VA2M�A1��A1&�A0�A/�PA.�A-ƨA-A,jA+�mA*Q�A(n�A'��A't�A'VA&��A&I�A%S�A$�A#`BA"~�A!��A ��A ��A�^A/A=qA�FAG�A�A��A\)A�AM�A�A��A7LA33A��AC�A�A~�AoAJAVA$�A|�A9XA�A
��A
��A
n�A	�
A��A�A7LA��AI�A�PAE�A�TA�PA��A�;A �A @��@��@��@�G�@��@��u@�1'@��;@�?}@�C�@���@�X@���@�b@��m@�+@��@���@�Z@��@�{@�O�@�%@�u@���@�+@噚@��;@�5?@ᙚ@�I�@�ff@ݙ�@��/@�Q�@��@�-@���@�1'@��@�G�@�A�@ӶF@ӝ�@�-@�z�@��#@�I�@�9X@ˮ@�-@�l�@�I�@öF@�ȴ@�{@���@��@�G�@��@��/@�1'@�C�@��@�G�@���@��
@��H@�V@�$�@�&�@�Ĝ@�  @��@��@���@�j@�b@��
@�\)@�o@�;d@���@��w@��F@���@���@�\)@�K�@�33@��@���@��@���@�n�@��@���@���@���@���@���@��!@���@��@���@�@��@�?}@�V@��@���@���@��@�hs@��@���@��;@�dZ@�o@�~�@��^@��@��@��R@��#@��9@���@���@�n�@�5?@���@��j@�9X@��m@��;@��;@�l�@�l�@�1'@�bN@�j@�9X@�Q�@��;@���@�O�@��+@���@��@���@��H@��@��R@���@���@��@�@�@��7@��@�  @�Z@�j@�r�@�l�@��\@��H@���@�=q@�5?@��9@���@��H@���@�ȴ@��+@�M�@��+@��y@���@�@�
=@���@��+@���@��7@��@�7L@�Z@��m@��F@�dZ@��@���@��\@�M�@��@�E�@�ff@���@�&�@��/@�Q�@�+@�n�@�=q@�@�X@��@��@�Z@��@�ƨ@���@�;d@�v�@�$�@�{@��@��^@���@��;@�S�@�ȴ@�$�@���@��h@�`B@�V@��j@���@��u@��u@��@�r�@�j@�9X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�O�A�G�A��HA�I�A�1A̴9A�|�A�M�A��A�
=A���A��mA���A�ĜA˾wAˣ�A�"�A��`A��AʅA��#A���A��#A�A���A�XA�hsA�r�A�r�A�G�A���A��uA�1A��/A�|�A��A�oA��!A�`BA��A�bNA�9XA��9A��`A�M�A�;dA��A���A�?}A��RA�`BA�A��A�+A�hsA��PA���A��DA���A�{A��DA�"�A���A��A�v�A�E�A�{A�ĜA���A��A�Q�A���A�I�A���A���A��;A�G�A��A�oA�`BA�K�A�{A��A��A��A�=qA���A��+A���A��wA�A��9A�7LA��A�jA��7A��HA�VA�bA��A��A��A��DA�bNA��;A��uA�"�A�%A�O�A��uA�$�A�A�VAK�A~��A~M�A|��AzE�Az{AzJAyXAw�Aw/Au�As7LAq
=Ap{An��Aj�jAh��Ae\)AbJA_��A]�-A\�HA\VA[33AX�AV�HAT�DAR�AR5?AQ�hAPjAO"�AN�+AN1AL��AK��AI�7AH��AHr�AGt�AG�AE33AC33AB�uABVABffAAt�A@(�A>�A>=qA>$�A=��A=�;A=C�A<��A<  A;��A:�A8��A8-A7dZA6z�A6��A6�!A6r�A5A4~�A3VA2M�A1��A1&�A0�A/�PA.�A-ƨA-A,jA+�mA*Q�A(n�A'��A't�A'VA&��A&I�A%S�A$�A#`BA"~�A!��A ��A ��A�^A/A=qA�FAG�A�A��A\)A�AM�A�A��A7LA33A��AC�A�A~�AoAJAVA$�A|�A9XA�A
��A
��A
n�A	�
A��A�A7LA��AI�A�PAE�A�TA�PA��A�;A �A @��@��@��@�G�@��@��u@�1'@��;@�?}@�C�@���@�X@���@�b@��m@�+@��@���@�Z@��@�{@�O�@�%@�u@���@�+@噚@��;@�5?@ᙚ@�I�@�ff@ݙ�@��/@�Q�@��@�-@���@�1'@��@�G�@�A�@ӶF@ӝ�@�-@�z�@��#@�I�@�9X@ˮ@�-@�l�@�I�@öF@�ȴ@�{@���@��@�G�@��@��/@�1'@�C�@��@�G�@���@��
@��H@�V@�$�@�&�@�Ĝ@�  @��@��@���@�j@�b@��
@�\)@�o@�;d@���@��w@��F@���@���@�\)@�K�@�33@��@���@��@���@�n�@��@���@���@���@���@���@��!@���@��@���@�@��@�?}@�V@��@���@���@��@�hs@��@���@��;@�dZ@�o@�~�@��^@��@��@��R@��#@��9@���@���@�n�@�5?@���@��j@�9X@��m@��;@��;@�l�@�l�@�1'@�bN@�j@�9X@�Q�@��;@���@�O�@��+@���@��@���@��H@��@��R@���@���@��@�@�@��7@��@�  @�Z@�j@�r�@�l�@��\@��H@���@�=q@�5?@��9@���@��H@���@�ȴ@��+@�M�@��+@��y@���@�@�
=@���@��+@���@��7@��@�7L@�Z@��m@��F@�dZ@��@���@��\@�M�@��@�E�@�ff@���@�&�@��/@�Q�@�+@�n�@�=q@�@�X@��@��@�Z@��@�ƨ@���@�;d@�v�@�$�@�{@��@��^@���@��;@�S�@�ȴ@�$�@���@��h@�`B@�V@��j@���@��u@��u@��@�r�@�j@�9X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�bB�VB�JB�JB�DB�DB�=B�=B�=B�7B�7B�7B�7B�7B�7B�1B�B�B�B{�Bs�BbNBM�BG�BF�BD�BB�B>wB8RB1'B'�B �B�B�BuB��B�B��B�B�B�fB�)B��BɺBĜB�qB�RB�dB�RB�?B�^B�9B�!B�B��B��B�uB�VB�B~�By�Bt�Bo�BffBbNB_;B\)BXBT�BM�BB�B7LB2-B-B&�B�BuBPBB��B�B�;B�B�B��B�wB�3B��B��B�DBq�BdZBM�B1'B&�B�BbB	7BB
��B
��B
�mB
�;B
��B
��B
ŢB
�}B
�'B
��B
�{B
�bB
�7B
�B
|�B
x�B
t�B
jB
_;B
]/B
]/B
W
B
M�B
I�B
@�B
1'B
&�B
�B
hB	��B	�B	�/B	ɺB	�XB	�B	��B	��B	��B	�7B	w�B	e`B	[#B	VB	N�B	F�B	@�B	<jB	9XB	49B	0!B	%�B	'�B	2-B	'�B	(�B	�B	�B	bB	VB	\B	PB		7B	B	VB	�B	�B	�B	�B	bB	%B	B�B�mB�ZB�;B�BB�B��B��B��B�B�B�yB�fB�TB�BB�#B�B��B��B��B��BĜB�}B�wB�wB�qB�dB�RB�FB�'B�B�B��B��B��B��B��B��B��B��B��B�{B�oB�bB�PB�DB�7B�7B�1B�%B�B}�B}�Bv�Bs�Bq�Bn�Bl�BjBhsBgmBffBe`BcTBaHB_;B]/B\)B[#BYBYBXBVBS�BR�BP�BM�BL�BL�BL�BL�BL�BK�BK�BI�BI�BI�BJ�BJ�BI�BI�BI�BH�BH�BF�BF�BE�BE�BE�BE�BD�BD�BC�BC�BB�BC�BB�BB�BD�BD�BD�BD�BD�BB�BC�BC�B@�B@�B@�B@�B@�B@�BA�BF�BG�BG�BF�BD�BA�BB�BB�BB�BA�B@�BG�BJ�BK�BM�BP�BS�BW
BVBXBZB[#B`BBbNBffBgmBgmBdZBcTBk�Br�Bu�Bx�By�B}�B�7B�PB�VB�bB��B��B��B�B�B�B�B�'B�-B�-B�'B�!B�3B�qB�}B��BĜBɺB��B��B��B��B��B�B�
B�#B�HB�TB�`B�sB�yB�mB�sB�yB�B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	B	B	+B	bB	�B	�B	�B	 �B	$�B	&�B	1'B	7LB	@�B	A�B	E�B	T�B	XB	YB	[#B	[#B	\)B	]/B	^5B	^5B	]/B	\)B	aHB	ffB	hsB	jB	hsB	ffB	n�B	q�B	r�B	r�B	n�B	k�B	k�B	k�B	l�B	n�B	r�B	v�B	y�B	~�B	�B	�B	�B	�B	�%B	�+B	�+B	�+B	�7B	�DB	�JB	�JB	�DB	�JB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�91111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�bB�bB�\B�PB�PB�JB�DB�DB�DB�7B�7B�7B�7B�7B�=B�=B�%B�B�B~�B{�Bn�BVBL�BG�BG�BE�BA�B=qB7LB-B"�B�B�B�B+B�B��B��B�B�B�ZB�B��B��B��B�jB�wB�dB�LB�jB�RB�9B�B��B��B��B�oB�1B�B{�Bw�Bs�BhsBcTB`BB^5BYBXBR�BF�B9XB49B/B+B�B�BoB%B  B��B�HB�B�B��BB�XB��B��B��Bv�Bk�BVB49B+B�BoB
=B+B  B
��B
�B
�`B
��B
��B
ǮB
ŢB
�^B
��B
��B
�oB
�PB
�B
}�B
z�B
x�B
q�B
`BB
]/B
_;B
[#B
O�B
M�B
H�B
7LB
)�B
#�B
�B
B	��B	�sB	��B	��B	�'B	�B	�B	��B	�hB	�B	k�B	^5B	YB	S�B	K�B	B�B	>wB	>wB	9XB	7LB	(�B	(�B	5?B	(�B	.B	"�B	�B	hB	VB	oB	bB	PB	%B	VB	�B	�B	�B	�B	oB	+B	+B��B�yB�fB�NB�BB�B��B��B��B�B�B�B�sB�ZB�ZB�;B�#B�B��B��B��B��B��B�}B�}B�wB�jB�dB�XB�3B�'B�B�B��B��B��B��B��B��B��B��B��B�uB�oB�\B�bB�=B�7B�7B�DB�%B�B�By�Bv�Bt�Bp�Bp�Bl�BjBhsBgmBgmBffBdZBbNB_;B^5B^5B]/B[#BZBYBW
BW
BS�BT�BO�BM�BM�BM�BM�BL�BL�BN�BM�BL�BK�BK�BK�BJ�BK�BI�BJ�BI�BI�BG�BG�BF�BF�BF�BF�BF�BG�BE�BE�BE�BF�BF�BF�BE�BF�BF�BD�BD�BF�BA�BB�BA�B@�BB�BC�BE�BH�BG�BH�BH�BH�BF�BC�BD�BC�BD�BD�BH�BK�BL�BN�BR�BVBXBW
BYB\)B\)BaHBdZBgmBiyBiyBgmBe`Bk�Bs�Bv�By�Bz�B}�B�7B�PB�VB�bB��B��B��B�B�B�B�B�'B�3B�3B�-B�3B�?B�qB�}B��BĜB��B��B��B��B��B�B�
B�B�/B�NB�ZB�fB�yB�B�sB�sB�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	B	B	B	B	+B	\B	�B	�B	�B	 �B	%�B	%�B	0!B	5?B	C�B	A�B	C�B	T�B	XB	YB	[#B	[#B	]/B	]/B	_;B	_;B	^5B	]/B	aHB	ffB	hsB	l�B	iyB	ffB	o�B	r�B	r�B	u�B	q�B	k�B	k�B	k�B	m�B	n�B	r�B	v�B	x�B	� B	�B	�B	�B	�%B	�+B	�+B	�1B	�7B	�=B	�JB	�PB	�PB	�JB	�JB	�\B	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�91111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447022012010314470220120103144702  AO  ARGQ                                                                        20111130140938  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140938  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144702  IP                  G�O�G�O�G�O�                