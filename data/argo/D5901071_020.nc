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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  eP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135612  20190522121825  1727_5046_020                   2C  D   APEX                            2143                            040306                          846 @�/����1   @�/�s��@7�z�G��c�E����1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8�fD9fD9� D:  D:� D:��D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� DdfDd�fDefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dm� Dn  Dny�Do  Do� Dp  Dp�fDq  Dq� Dr  Dr� Ds  Dss3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'��B.��B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D��Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1��D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8y�D8��D9s3D9�3D:s3D:��D;l�D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc��Ddy�Dd��Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djy�Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dnl�Dn�3Dos3Do�3Dpy�Dp�3Dqs3Dq�3Drs3Dr�3Dsff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A�1A�bA�oA�oA��A��A�{A��A�{A�oA�oA�oA��A�{A�  AϸRAϕ�A�`BA�E�A��/A�G�A��Aǩ�A�?}A�bA�{A¬A���A�M�A�r�A���A��A���A�{A�?}A��!A���A�XA�p�A�O�A�&�A��+A�{A��FA��TA�XA�E�A��A�r�A�A��
A��A���A�=qA���A���A��FA�ȴA��A�t�A�A�{A�VA�K�A�l�A��A��A�=qA��DA�C�A�{A�t�A�~�A�VA���A� �A�I�A�;dA���A�9XA�\)A�=qA�?}A��yA��HA�C�A���A���A���A�dZA��A�x�A��\A��A~�9A|n�Az$�Ay�Aw�mAu�AudZAt��Aq�Ap  Ao��Ao7LAn�jAm��Am"�Al  Aj��Aj�+Aj5?AhbNAf��Af�\AfVAf$�Ae��Ad�Acp�Ab��Ab9XA`I�A^��A]�;A]p�A\�!AZ��AX��AYAX5?AW?}AUO�AS�AS`BAR��AQ�PAPAOC�AN��AMXALbNAK�AJv�AI�FAH��AG�#AF�RAE+AC�wAC?}AB��AB�jABQ�AA+A@n�A?p�A>ffA=��A=�PA=XA=/A<��A< �A;&�A:M�A9��A9/A9A8�A8VA7�;A6��A6{A5��A4�RA3�A3C�A2bA1C�A0~�A01A/�A/�A-�FA,ffA+x�A* �A)�;A(��A'x�A&=qA%XA$�9A"�A!\)A �uA��A��A�A��AJA\)A��AffA��A1'A�FA/A�#A9XAA1A��A33A�A9XA1AA�HA��Ax�A�AI�A;dA
M�A	��A��A�jAffAJAp�A�!AƨA��A�A�/AG�A 1@���@��T@���@�+@���@�
=@���@�^5@�9@� �@�!@��@�7L@��@�1@�ȴ@�Ĝ@��@��;@�|�@��@���@�z�@�dZ@�^@�  @ޏ\@�z�@ڇ+@ّh@ؼj@ָR@պ^@ԓu@� �@�K�@�=q@��T@�7L@�I�@�l�@�V@�z�@��@î@\@§�@+@��@�&�@��@�K�@�@�ȴ@��\@�E�@�ff@�@�^5@�S�@�r�@�Ĝ@ċD@�V@�%@�Z@�ȴ@�{@�`B@�j@���@�o@���@�v�@�G�@�9X@�+@���@�/@��!@�1@�x�@��@�&�@��/@��H@�M�@�+@���@���@�=q@�O�@�Ĝ@�9X@��@�ȴ@�@���@�p�@��D@��;@�+@��@���@��@��\@�-@���@�&�@��@�  @�;d@��+@��T@���@��7@�p�@��@�p�@�p�@�?}@�1@�|�@�33@�S�@�
=@�ȴ@���@���@�K�@�l�@�t�@�K�@��\@�~�@�J@��^@�p�@�hs@���@�S�@�hs@��@�1@�dZ@���@�o@��@�|�@��F@��+@�/@�bN@�ƨ@�C�@���@��@�+@�S�@�`B@���@�%@��9@�Q�@�9X@�r�@��9@�/@�$�@�ff@��+@��+@�V@���@��T@��#@���@�hs@�1'@�S�@�l�@�dZ@��@���@��!@��+@�v�@�V@�E�@�o@���@��@��@�S�@��@�v�@�{@��@���@��-@��7@�X@�V@��/@��@�I�@�I�@�(�@�b@���@��@�C�@��\@�{@���@��@��@��@���@�Ĝ@�Z@�Z@�9X@� �@�b@���@�ƨ@���@�t�@�+@��H@�ȴ@��\@�-@��@��@�{@�J@���@��#@��^@��@�p�@�G�@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�%A�1A�bA�oA�oA��A��A�{A��A�{A�oA�oA�oA��A�{A�  AϸRAϕ�A�`BA�E�A��/A�G�A��Aǩ�A�?}A�bA�{A¬A���A�M�A�r�A���A��A���A�{A�?}A��!A���A�XA�p�A�O�A�&�A��+A�{A��FA��TA�XA�E�A��A�r�A�A��
A��A���A�=qA���A���A��FA�ȴA��A�t�A�A�{A�VA�K�A�l�A��A��A�=qA��DA�C�A�{A�t�A�~�A�VA���A� �A�I�A�;dA���A�9XA�\)A�=qA�?}A��yA��HA�C�A���A���A���A�dZA��A�x�A��\A��A~�9A|n�Az$�Ay�Aw�mAu�AudZAt��Aq�Ap  Ao��Ao7LAn�jAm��Am"�Al  Aj��Aj�+Aj5?AhbNAf��Af�\AfVAf$�Ae��Ad�Acp�Ab��Ab9XA`I�A^��A]�;A]p�A\�!AZ��AX��AYAX5?AW?}AUO�AS�AS`BAR��AQ�PAPAOC�AN��AMXALbNAK�AJv�AI�FAH��AG�#AF�RAE+AC�wAC?}AB��AB�jABQ�AA+A@n�A?p�A>ffA=��A=�PA=XA=/A<��A< �A;&�A:M�A9��A9/A9A8�A8VA7�;A6��A6{A5��A4�RA3�A3C�A2bA1C�A0~�A01A/�A/�A-�FA,ffA+x�A* �A)�;A(��A'x�A&=qA%XA$�9A"�A!\)A �uA��A��A�A��AJA\)A��AffA��A1'A�FA/A�#A9XAA1A��A33A�A9XA1AA�HA��Ax�A�AI�A;dA
M�A	��A��A�jAffAJAp�A�!AƨA��A�A�/AG�A 1@���@��T@���@�+@���@�
=@���@�^5@�9@� �@�!@��@�7L@��@�1@�ȴ@�Ĝ@��@��;@�|�@��@���@�z�@�dZ@�^@�  @ޏ\@�z�@ڇ+@ّh@ؼj@ָR@պ^@ԓu@� �@�K�@�=q@��T@�7L@�I�@�l�@�V@�z�@��@î@\@§�@+@��@�&�@��@�K�@�@�ȴ@��\@�E�@�ff@�@�^5@�S�@�r�@�Ĝ@ċD@�V@�%@�Z@�ȴ@�{@�`B@�j@���@�o@���@�v�@�G�@�9X@�+@���@�/@��!@�1@�x�@��@�&�@��/@��H@�M�@�+@���@���@�=q@�O�@�Ĝ@�9X@��@�ȴ@�@���@�p�@��D@��;@�+@��@���@��@��\@�-@���@�&�@��@�  @�;d@��+@��T@���@��7@�p�@��@�p�@�p�@�?}@�1@�|�@�33@�S�@�
=@�ȴ@���@���@�K�@�l�@�t�@�K�@��\@�~�@�J@��^@�p�@�hs@���@�S�@�hs@��@�1@�dZ@���@�o@��@�|�@��F@��+@�/@�bN@�ƨ@�C�@���@��@�+@�S�@�`B@���@�%@��9@�Q�@�9X@�r�@��9@�/@�$�@�ff@��+@��+@�V@���@��T@��#@���@�hs@�1'@�S�@�l�@�dZ@��@���@��!@��+@�v�@�V@�E�@�o@���@��@��@�S�@��@�v�@�{@��@���@��-@��7@�X@�V@��/@��@�I�@�I�@�(�@�b@���@��@�C�@��\@�{@���@��@��@��@���@�Ĝ@�Z@�Z@�9X@� �@�b@���@�ƨ@���@�t�@�+@��H@�ȴ@��\@�-@��@��@�{@�J@���@��#@��^@��@�p�@�G�@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�sB�sB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�fB�TB�NB�;B�5B�5B�B�BBPBDB  B��B��B��B�B�B��B�B�B�B�sB�TB�)B�B�B��B��B��BɺBB�wB�9B�B��B��B��B�uB�JB�Bt�BZBM�BD�B8RB-B%�B�B{BJBB��B�B�BB��B�RB�-B�B��B��B�bB�1B}�Bm�B[#BA�B)�B�B
=B
��B
�/B
��B
ɺB
B
�}B
�^B
�?B
�B
��B
��B
�1B
y�B
e`B
ZB
S�B
I�B
B�B
8RB
�B
�B
�B
uB
hB
VB
hB
VB
	7B
+B
B	��B	�B	�fB	�ZB	�BB	�#B	�
B	��B	��B	ŢB	�LB	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�%B	~�B	}�B	y�B	s�B	m�B	jB	gmB	bNB	^5B	YB	VB	R�B	N�B	I�B	B�B	;dB	5?B	33B	2-B	0!B	.B	&�B	"�B	�B	�B	�B	�B	�B	�B	{B	hB	PB	
=B		7B	+B	%B	B	B��B��B��B��B�B�B�B�fB�TB�BB�5B�)B�B��B��BɺBŢBÖB�}B�^B�FB�3B�!B�B��B��B��B��B��B��B�oB�bB�bB�bB�PB�+B�B�B{�Bv�Bs�Bq�Bp�Bn�Bm�Bl�Bk�BiyBgmBffBe`BbNB_;B^5B]/B\)B[#BZBZBXBVBS�BQ�BO�BN�BK�BJ�BI�BH�BG�BE�BC�BB�B@�B?}B=qB?}B?}BA�BA�BC�BB�BA�B@�B@�B@�B@�BA�BA�BA�B?}B?}BF�BA�B=qB>wB<jB<jB;dB=qB<jB<jB<jB>wB@�BA�BA�BB�BA�B?}B;dB2-B.B0!B6FB7LB7LB8RB9XB9XB;dB=qB?}BC�BJ�BW
Bv�B�B�\B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�B�B��B��B��B��B�B�9B�9B�'B�9B�dB�LB�3B�B�B�B��B��B��B��B�B�B�B�B�B�B�B�B�!B�!B�!B�!B�'B�9B�LB�jB�}B��B��BBŢBƨBŢB��B��B��B��B�B�
B�B�B�B�ZB�fB�fB�sB�B�B��B��B��B��B	B	
=B	1B	+B	B	B	%B		7B	JB	hB	{B	{B	�B	�B	�B	�B	�B	&�B	(�B	-B	+B	-B	6FB	8RB	;dB	C�B	M�B	S�B	_;B	hsB	m�B	o�B	q�B	r�B	u�B	|�B	� B	� B	� B	}�B	� B	�B	�B	�B	�B	�B	�B	�%B	�7B	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�3B	�?B	�?B	�FB	�LB	�RB	�^B	�dB	�wB	��B	B	ĜB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�sB�sB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�mB�ZB�TB�BB�;B�NB��B��B+BVBuBDB  B��B��B��B��B��B�B�B�B�B�B�BB�5B�5B�
B��B��B��BŢBÖB�jB�'B��B��B��B��B�\B�VB~�B^5BQ�BI�B=qB/B)�B"�B�BbBB��B�B�TB�)B�qB�?B�3B�B��B�{B�PB�Bt�BffBL�B1'B�BbB
��B
�BB
�B
��B
ÖB
��B
�jB
�RB
�3B
��B
��B
�VB
� B
hsB
]/B
YB
K�B
D�B
B�B
�B
�B
�B
�B
uB
hB
�B
oB

=B
1B
	7B	��B	�B	�mB	�`B	�HB	�;B	�#B	�B	��B	��B	�jB	�B	��B	��B	�B	�B	��B	��B	��B	��B	�DB	� B	� B	~�B	x�B	p�B	l�B	k�B	e`B	aHB	[#B	XB	T�B	P�B	L�B	F�B	?}B	6FB	49B	33B	1'B	1'B	(�B	%�B	 �B	�B	�B	�B	�B	�B	�B	{B	bB	JB	DB	1B	+B	B	B	B��B��B��B�B�B�B�yB�`B�NB�;B�5B�)B�B��B��BƨBƨBÖB�wB�XB�?B�FB�!B��B��B��B��B��B��B�{B�oB�hB�oB�hB�1B�+B�B�Bz�Bv�Br�Bq�Bp�Bn�Bm�Bl�Bl�Bk�BgmBffBe`BbNBaHB_;B_;B\)B[#B[#BZBXBW
BT�BS�BP�BP�BN�BK�BJ�BI�BH�BG�BE�BC�BC�B?}B@�BA�BB�BB�BD�BC�BC�BC�BA�BA�BA�BB�BC�BC�BA�BB�BI�BC�B@�BA�B>wB>wB>wB?}B>wB=qB>wB@�BA�BB�BC�BD�BC�BB�BC�B7LB0!B0!B6FB8RB9XB:^B:^B:^B<jB=qB@�BC�BI�BR�Bu�B� B�\B��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�3B�'B�B�B��B��B��B�B�?B�LB�-B�3B�qB�^B�?B�B�B�B�B��B��B��B�B�B�B�B�B�B�B�!B�'B�'B�'B�'B�-B�?B�RB�qB�}B��B��BBŢBƨBƨB��B��B��B��B�
B�B�B�B�B�ZB�fB�fB�sB�B�B��B��B��B��B	B	PB	
=B	1B	%B	%B	%B		7B	JB	hB	�B	�B	�B	�B	�B	�B	�B	'�B	(�B	/B	,B	-B	7LB	9XB	;dB	C�B	M�B	S�B	^5B	hsB	m�B	o�B	q�B	s�B	u�B	|�B	� B	�B	�B	~�B	� B	�B	�B	�B	�B	�B	�B	�%B	�7B	�JB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�-B	�9B	�?B	�?B	�FB	�RB	�XB	�dB	�jB	�}B	��B	B	ĜB	ǮB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�#B	�)B	�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446402012010314464020120103144640  AO  ARGQ                                                                        20111130135612  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135612  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144640  IP                  G�O�G�O�G�O�                