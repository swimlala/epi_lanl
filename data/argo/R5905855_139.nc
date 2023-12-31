CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-12-04T09:42:36Z creation;2022-12-04T09:42:37Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20221204094236  20221204095810  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��5��%1   @�Ϡ"�@-W�O�;d�c��t�j1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���A�33B  B  B  B   B(ffB/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bw33B�  B�  B�  B�  B�ffB�  B�33B�  B�  B�  B�ffB�  B�ffB�  B�33B���B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�33B�  B���C   C  C  C  C  C
  C  C  C�C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @33@s33@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�33A���B33B33B33B33B'��B.��B6��B?33BG33BO33BW33B_33Bg33Bo33BvffB33B���B���B���B�  B���B���B���B���B���B�  B���B�  B���B���B�fgB���BÙ�BǙ�B˙�Bϙ�Bә�B���Bۙ�B�fgB㙚B癚B뙚BB���B���B�fgB���C��C��C��C��C	��C��C��C�gC��C�3C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM�gCO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٙC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٙC��fC��fC��fC��fC��3C��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D��Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dw�3Dxs3Dx�3Dys3Dy�3Dzs3Dz�3D{s3D{�3D|s3D|�3D}s3D}�3D~s3D~�3Ds3D�3D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D๚D���D�9�D�y�DṚD���D�9�D�y�D⹚D���D�9�D�y�D㹚D���D�9�D�y�D乚D���D�9�D�y�D幚D���D�9�D�y�D湚D���D�9�D�y�D繚D���D�9�D�y�D蹚D���D�9�D�y�D鹚D���D�9�D�y�D깚D���D�9�D�y�D빚D���D�9�D�y�D칚D���D�9�D�y�D���D���D�9�D�y�DD���D�9�D�y�D﹚D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D��411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�4A�6�A�>A�?�A�C�A�FtA�C�A�F�A�E�A�HKA�HKA�I�A�G�A�<�A�1'A�)�A�&�A�(�A�)_A�*eA�*�A�!�A�$tA�49A�8�A�AUA�WsAҌ�AҥA��A�r�A�V�A�M�A�kQA�~�A�U�A�v`A�P�A���A�%�A��A��A�� A�R�A��qA�aA��A�l�A�.A���A�J�A��A�9�A�\�A��}A���A�˒A���A�{JA�/�A�W
A��jA�'�A��uA�+A�@OA}.IA{��Ay�Ar�Am7LAi�2Ab��A`�"A_��A^��A]ݘA\ɆA[W?AW�zAV�XAT�jAS�9ARx�AQ�AP�AOzAM��AM7AM�AL�cALz�AG�4AA�<A>��A<�fA:�A:$tA8�A4eA2�A2�<A1U�A.~(A+��A*ƨA+�\A*��A*+A)�mA)\)A(k�A&V�A%QA$IRA!�IA�SA�A$tA�A�pA �Ay>A��AZ�A(�A��A0UA��A�dA�EA�HA�[A��A�A>�A�RAjA4A�PA��A�4A��A��A�A*�A�LAK^Ae�Al"A�A��A�A
��A
A�A
e,A
\�A
:�A	�]A	��A	�_A	=qA	�A�gA��A�AU2A��A�3A+kA�~ADgA/�A�oA�0A �A��AqA�AQA.�A��A kQ@�ff@��@�(�@���@�@���@��@���@�>�@��D@�?�@��@��H@�O�@���@�J�@��5@�@�PH@�=�@�o@� i@�g8@�V@@�P@�&@�9�@��H@�@�\�@�|@�R@�,=@��;@�S@��@�]d@�V@��,@�|�@�@㋬@��f@��@�خ@Ṍ@�,�@��@��A@ބ�@�:�@�)�@ݾw@���@�#:@��@�4n@�V@ؗ�@�@�O@�_p@�n�@��@�ݘ@�6z@��@�a|@�x@�(�@�Q�@�j�@���@���@�J#@��@̕@�(�@���@�c@��P@ʷ�@ʭ�@�Xy@ɜ�@��@�\�@��@ǿH@�Dg@Ə\@�@��&@ũ�@��@Ĉ�@��6@�^�@�n�@��@�g�@��@��5@��$@��D@���@��K@�;�@��@�@��@��@���@��I@��.@�{@��r@��D@��+@���@���@�+�@�7@���@���@�V@��F@�c @�J�@�b@��r@��@�hs@��)@�o@�O@�˒@��"@�j@���@�e@��@���@��h@���@��+@�l"@�c�@�V@�7�@��@��.@��;@��@�L0@��Q@�|�@�@�ߤ@��s@��X@��L@�r�@�>B@��^@�a�@��H@�q�@�5?@�	@��h@�V@���@�b@���@�n/@�+�@�a�@�8@�(�@��@�v�@�n�@�GE@��@�7L@��M@��1@�Q@��@��
@���@��0@��S@�*0@�֡@��j@��}@�~�@�U2@�/�@���@�y�@�%@��"@��@��@���@��+@��W@���@��X@���@�{J@�B�@��H@�y>@�J�@�E9@��/@���@�1@�zx@�%@��s@�Q�@��@�ϫ@��@�?}@��M@��s@�L0@��@���@��*@��:@�x@�A @�"�@�}V@�M@��@���@�~�@�e�@�8�@��@���@�u�@���@�F�@�%@���@���@�?@���@���@��=@�k�@�1�@�@���@�w�@�5?@�
�@���@�`B@��2@���@�V�@�@���@�l�@�C�@�"�@�
=@���@�q�@�?�@�
�@��[@���@�-w@���@���@�u%@�5?@���@��Q@��@�a�@�+�@���@��@��9@�|�@�5?@�
�@��'@�k�@�!-@��@���@��!@���@���@�|�@�]d@��D@��@�� @��-@�\�@��@��"@���@���@��2@�Ta@�_@���@��@��j@��:@�dZ@�=@�&@�&@��E@�tT@�0U@�	@���@�~�@�_p@�F@���@�GE@�	�@��@���@���@��4@�]�@�4�@�o@��$@�xl@�e�@�J�@�5?@�+k@�&�@�"h@��@��@C�@~�H@~�L@~1�@}@}G�@|�@|��@|g8@{��@z�"@z{@y��@yG�@x��@x�@x(�@w��@w˒@wƨ@w�w@w�@v�]@vZ�@v6�@u�>@u2a@t�@t7�@s��@s�@r��@r��@rR�@ru@q�z@qX@p��@p?�@p�@o��@oY@n�'@ni�@n;�@m�@mrG@m%@l�9@lK^@k�q@k�@jH�@i��@i8�@i�@h��@hz�@hXy@h"h@g�m@g��@g>�@g(@f�@fq�@f3�@e��@e�@e��@e#�@d��@dɆ@dM@d  @c�@@cl�@cH�@c.I@b�c@b��@bV@a�)@ac@a&�@`r�@`�@_�@_�@^��@^M�@^u@]��@]�3@]�@]c@]f�@]:�@\��@[X�@Z�'@Z��@Z1�@Y��@Y��@Y%F@Y�@Y�@X��@X�@X:�@X'R@X7@W�m@W�q@Wt�@V�'@V-@U��@Uo @T�5@T��@T�_@Tl"@T"h@S�@S��@S�@RW�@Q��@Qm]@P�$@PN�@Oخ@Og�@O�@Nh
@M��@M�@Mq@L��@L?�@Kƨ@J�B@J��@Jp;@J=q@I�.@I��@I%F@H�P@H�K@H��@G�;@GY@G�@F�6@F+k@F-@F&�@F@E��@E�3@Ec@E4@Dq@C�@C��@C˒@C��@C��@C�@B��@B	@A%F@@��@@_@@,=@?�V@?C�@>�c@>L0@=��@=�C@=\�@=�@<�z@<K^@<~@;��@;|�@;]�@;F�@:�R@:�@:��@:W�@:O@9�@9�M@9G�@9@8�@8Q�@7�w@7n/@7@6�}@6e@5�T@5\�@5L�@5B�@5(�@4�@4�I@4l"@4I�@4C-@46@3��@3�P@3�@2��@2��@2�<@2��@2Ta@2O@1��@1}�@17L@0��@0�@0��@0$@/˒@/��@/9�@.�@.�L@.��@.?@.4@-��@-��@-�C@-}�@-@,��@,�@,��@,?�@,"h@+�@+��@+��@+��@+\)@+=@*�8@*ȴ@*l�@*{@)�@)�H@)u�@)%F@(�@(�p@(�@(�z@(w�@(S�@(1'@(~@(	�@'�+@'��@'��@',�@&��@&ߤ@&�<@&�F@&_�@&?@&�@%��@%��@%[W@%@@$��@$�I@$9X@$$@$�@#��@#�:@#O@#.I@#�@"�2@"�@"v�@"d�@"5?@!�T@!�7@!@@ ��@ r�@ �@�a@��@b�@S�@1�@(@�s@��@J�@3�@�@w2@p�@^�@L�@2a@��@�z@[�@7@خ@�q@v`@�@҉@�!@�\@i�@+k@�@�@�@ԕ@k�@(�@��@bN@?�@~@�@�@�g@�@{J@_p@�@�@�@�B@�@p;@e@��@�n@p�@[W@A @+�@�@��@ѷ@�Y@2�@�@��@|�@e�@Z�@,�@��@��@��@^5@
�@��@c@[W@(�@֡@��@��@r�@:�@�m@�0@��@v`@H�@�@ں@ȴ@��@��@�x@z@8�@@�@��@�t@��@Vm@�@�@�@�?@��@@��@�	@{J@x@t�@X�@!-@o@
�M@
�@
�@
L0@
0U@	��@	��@	�@	��@	�~@	��@	|@	^�@	-w@	�@	�@�5@�@��@��@�o@`�@V�@Ft@9X@~@�+@� @��@�k@n/@&@�@��@�M@͟@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�4A�6�A�>A�?�A�C�A�FtA�C�A�F�A�E�A�HKA�HKA�I�A�G�A�<�A�1'A�)�A�&�A�(�A�)_A�*eA�*�A�!�A�$tA�49A�8�A�AUA�WsAҌ�AҥA��A�r�A�V�A�M�A�kQA�~�A�U�A�v`A�P�A���A�%�A��A��A�� A�R�A��qA�aA��A�l�A�.A���A�J�A��A�9�A�\�A��}A���A�˒A���A�{JA�/�A�W
A��jA�'�A��uA�+A�@OA}.IA{��Ay�Ar�Am7LAi�2Ab��A`�"A_��A^��A]ݘA\ɆA[W?AW�zAV�XAT�jAS�9ARx�AQ�AP�AOzAM��AM7AM�AL�cALz�AG�4AA�<A>��A<�fA:�A:$tA8�A4eA2�A2�<A1U�A.~(A+��A*ƨA+�\A*��A*+A)�mA)\)A(k�A&V�A%QA$IRA!�IA�SA�A$tA�A�pA �Ay>A��AZ�A(�A��A0UA��A�dA�EA�HA�[A��A�A>�A�RAjA4A�PA��A�4A��A��A�A*�A�LAK^Ae�Al"A�A��A�A
��A
A�A
e,A
\�A
:�A	�]A	��A	�_A	=qA	�A�gA��A�AU2A��A�3A+kA�~ADgA/�A�oA�0A �A��AqA�AQA.�A��A kQ@�ff@��@�(�@���@�@���@��@���@�>�@��D@�?�@��@��H@�O�@���@�J�@��5@�@�PH@�=�@�o@� i@�g8@�V@@�P@�&@�9�@��H@�@�\�@�|@�R@�,=@��;@�S@��@�]d@�V@��,@�|�@�@㋬@��f@��@�خ@Ṍ@�,�@��@��A@ބ�@�:�@�)�@ݾw@���@�#:@��@�4n@�V@ؗ�@�@�O@�_p@�n�@��@�ݘ@�6z@��@�a|@�x@�(�@�Q�@�j�@���@���@�J#@��@̕@�(�@���@�c@��P@ʷ�@ʭ�@�Xy@ɜ�@��@�\�@��@ǿH@�Dg@Ə\@�@��&@ũ�@��@Ĉ�@��6@�^�@�n�@��@�g�@��@��5@��$@��D@���@��K@�;�@��@�@��@��@���@��I@��.@�{@��r@��D@��+@���@���@�+�@�7@���@���@�V@��F@�c @�J�@�b@��r@��@�hs@��)@�o@�O@�˒@��"@�j@���@�e@��@���@��h@���@��+@�l"@�c�@�V@�7�@��@��.@��;@��@�L0@��Q@�|�@�@�ߤ@��s@��X@��L@�r�@�>B@��^@�a�@��H@�q�@�5?@�	@��h@�V@���@�b@���@�n/@�+�@�a�@�8@�(�@��@�v�@�n�@�GE@��@�7L@��M@��1@�Q@��@��
@���@��0@��S@�*0@�֡@��j@��}@�~�@�U2@�/�@���@�y�@�%@��"@��@��@���@��+@��W@���@��X@���@�{J@�B�@��H@�y>@�J�@�E9@��/@���@�1@�zx@�%@��s@�Q�@��@�ϫ@��@�?}@��M@��s@�L0@��@���@��*@��:@�x@�A @�"�@�}V@�M@��@���@�~�@�e�@�8�@��@���@�u�@���@�F�@�%@���@���@�?@���@���@��=@�k�@�1�@�@���@�w�@�5?@�
�@���@�`B@��2@���@�V�@�@���@�l�@�C�@�"�@�
=@���@�q�@�?�@�
�@��[@���@�-w@���@���@�u%@�5?@���@��Q@��@�a�@�+�@���@��@��9@�|�@�5?@�
�@��'@�k�@�!-@��@���@��!@���@���@�|�@�]d@��D@��@�� @��-@�\�@��@��"@���@���@��2@�Ta@�_@���@��@��j@��:@�dZ@�=@�&@�&@��E@�tT@�0U@�	@���@�~�@�_p@�F@���@�GE@�	�@��@���@���@��4@�]�@�4�@�o@��$@�xl@�e�@�J�@�5?@�+k@�&�@�"h@��@��@C�@~�H@~�L@~1�@}@}G�@|�@|��@|g8@{��@z�"@z{@y��@yG�@x��@x�@x(�@w��@w˒@wƨ@w�w@w�@v�]@vZ�@v6�@u�>@u2a@t�@t7�@s��@s�@r��@r��@rR�@ru@q�z@qX@p��@p?�@p�@o��@oY@n�'@ni�@n;�@m�@mrG@m%@l�9@lK^@k�q@k�@jH�@i��@i8�@i�@h��@hz�@hXy@h"h@g�m@g��@g>�@g(@f�@fq�@f3�@e��@e�@e��@e#�@d��@dɆ@dM@d  @c�@@cl�@cH�@c.I@b�c@b��@bV@a�)@ac@a&�@`r�@`�@_�@_�@^��@^M�@^u@]��@]�3@]�@]c@]f�@]:�@\��@[X�@Z�'@Z��@Z1�@Y��@Y��@Y%F@Y�@Y�@X��@X�@X:�@X'R@X7@W�m@W�q@Wt�@V�'@V-@U��@Uo @T�5@T��@T�_@Tl"@T"h@S�@S��@S�@RW�@Q��@Qm]@P�$@PN�@Oخ@Og�@O�@Nh
@M��@M�@Mq@L��@L?�@Kƨ@J�B@J��@Jp;@J=q@I�.@I��@I%F@H�P@H�K@H��@G�;@GY@G�@F�6@F+k@F-@F&�@F@E��@E�3@Ec@E4@Dq@C�@C��@C˒@C��@C��@C�@B��@B	@A%F@@��@@_@@,=@?�V@?C�@>�c@>L0@=��@=�C@=\�@=�@<�z@<K^@<~@;��@;|�@;]�@;F�@:�R@:�@:��@:W�@:O@9�@9�M@9G�@9@8�@8Q�@7�w@7n/@7@6�}@6e@5�T@5\�@5L�@5B�@5(�@4�@4�I@4l"@4I�@4C-@46@3��@3�P@3�@2��@2��@2�<@2��@2Ta@2O@1��@1}�@17L@0��@0�@0��@0$@/˒@/��@/9�@.�@.�L@.��@.?@.4@-��@-��@-�C@-}�@-@,��@,�@,��@,?�@,"h@+�@+��@+��@+��@+\)@+=@*�8@*ȴ@*l�@*{@)�@)�H@)u�@)%F@(�@(�p@(�@(�z@(w�@(S�@(1'@(~@(	�@'�+@'��@'��@',�@&��@&ߤ@&�<@&�F@&_�@&?@&�@%��@%��@%[W@%@@$��@$�I@$9X@$$@$�@#��@#�:@#O@#.I@#�@"�2@"�@"v�@"d�@"5?@!�T@!�7@!@@ ��@ r�@ �@�a@��@b�@S�@1�@(@�s@��@J�@3�@�@w2@p�@^�@L�@2a@��@�z@[�@7@خ@�q@v`@�@҉@�!@�\@i�@+k@�@�@�@ԕ@k�@(�@��@bN@?�@~@�@�@�g@�@{J@_p@�@�@�@�B@�@p;@e@��@�n@p�@[W@A @+�@�@��@ѷ@�Y@2�@�@��@|�@e�@Z�@,�@��@��@��@^5@
�@��@c@[W@(�@֡@��@��@r�@:�@�m@�0@��@v`@H�@�@ں@ȴ@��@��@�x@z@8�@@�@��@�t@��@Vm@�@�@�@�?@��@@��@�	@{J@x@t�@X�@!-@o@
�M@
�@
�@
L0@
0U@	��@	��@	�@	��@	�~@	��@	|@	^�@	-w@	�@	�@�5@�@��@��@�o@`�@V�@Ft@9X@~@�+@� @��@�k@n/@&@�@��@�M@͟@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B��B�B�B��B��B��B�B��B�7B�B�B��B��B��B�}B� B�4B��B��B�?B��B�B�RB��B�"B��B	B	,=B	0;B	\�B	��B	��B	�	B	�>B
}B
�B
	B
3�B
h
B
��B
�mB
ňB
��B
�NB
{�B
z�B
�B
��B
�B
�cB
�yB
x8B
p�B
oB
nB
�4B
�QB
�DB	B
�B
�MB
[B	�^B	�qB	āB	��B	��B	�B	�AB	ncB	G�B	<B	6+B	1[B	,qB	%zB	!B	�B	�B		7B	[B��B��B��B�BB�RB�B�|B�'B�WBޞBˬB��B��B�=B�sB��B�BB��B�B��B��B�B�rB�;B�B��B�B��B	oB	 B	�B	2�B	*�B	"�B	!B	�B	$B	*B	4B	8B	G�B	[#B	ZB	W�B	O\B	3hB	�B	B	)DB	5tB	7LB	:^B	;dB	JXB	WYB	d�B	v�B	sMB	rB	p;B	r�B	m)B	h>B	h$B	{dB	�B	�vB	��B	��B	��B	�B	��B	�MB	�/B	��B	��B	�B	��B	�vB	�bB	�HB	�@B	�FB	�,B	�fB	�B	��B	��B	�sB	�B	�*B	��B	��B	��B	��B	�yB	�fB	��B	��B	�tB	�4B	�kB	�TB	��B	�B	�B	�MB	�tB	�B	�B	��B	�8B	��B	�B	��B	�HB	��B	��B	�B	�cB	�.B	�.B	�iB	ðB	�gB	��B	�UB	ªB	�B	�MB	��B	ǔB	ʌB	��B	��B	̘B	̈́B	̈́B	�<B	�B	�B	͟B	̈́B	οB	�B	�(B	�\B	��B	�pB	�.B	�B	�B	ЗB	�4B	�:B	�B	�NB	��B	�VB	��B	�xB	�rB	�0B	�0B	�0B	�JB	�B	�6B	��B	͹B	�VB	�VB	�BB	�}B	�BB	ΥB	��B	��B	��B	�B	ҽB	��B	ӏB	өB	��B	ՁB	��B	ՁB	՛B	՛B	�MB	�mB	֡B	֡B	��B	�eB	�QB	��B	��B	�	B	یB	�]B	�xB	��B	��B	ݲB	��B	�OB	�!B	�pB	��B	�B	�@B	�B	�kB	�)B	�B	��B	�B	�IB	�cB	��B	�B	��B	��B	�'B	��B	�'B	�'B	��B	�aB	��B	��B	��B	��B	�B	�0B	��B	��B	��B	��B	�DB	��B	�>B	�DB	��B	�B
  B
 OB
 �B
;B
 B
 �B
 B	��B
  B	��B	�}B
 �B
;B
;B
B
 �B
 OB	��B
AB
�B
�B
�B
�B
�B
�B
�B
uB
�B
B
'B
�B
�B
	lB
	RB
)B
DB
)B

�B
B
�B
�B
JB
�B
�B
<B
"B
�B
"B
�B
�B
BB
BB
�B
\B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
B
B
,B
�B
MB
B
B
�B
�B
[B
�B
 B
hB
B
�B
hB
�B
oB
�B
�B
�B
aB
�B
{B
�B
B
MB
gB
�B
B
�B
�B
�B
yB
�B
yB
�B
�B
1B
B
QB
#B
�B
�B
�B
xB
B
dB
dB
dB
�B
�B
5B
jB
�B
�B
;B
pB
 B
 vB
 �B
!B
!�B
!�B
"NB
"NB
"hB
#TB
#�B
#�B
$@B
%,B
%�B
&fB
&�B
'B
'B
'8B
'RB
'mB
'�B
(�B
(�B
)DB
)DB
)�B
*B
*KB
*eB
+B
+6B
+�B
,"B
,qB
,qB
,WB
,�B
,�B
,�B
-�B
-�B
-�B
-�B
.IB
/ B
/OB
/OB
/5B
/�B
/�B
/�B
0oB
0�B
1AB
2B
2-B
2GB
2aB
2B
2aB
2|B
2�B
2�B
33B
2�B
2�B
3B
3�B
4nB
4�B
4�B
4�B
5B
5B
5�B
5�B
5�B
6�B
72B
7LB
7�B
7�B
7�B
7�B
7fB
7�B
7�B
7�B
88B
8RB
8�B
8�B
8�B
9>B
9XB
9XB
9�B
:^B
:�B
:�B
;B
;JB
;0B
<B
<B
<B
<B
;�B
<B
<PB
<�B
<�B
<�B
<�B
<�B
<�B
=VB
=�B
=�B
>(B
>BB
>(B
>BB
>]B
>�B
>�B
>�B
>�B
?B
?�B
?HB
?}B
?}B
?�B
?�B
?�B
@OB
@�B
AoB
A�B
A�B
B'B
BB
B�B
B�B
B�B
B�B
B�B
CaB
C{B
CaB
CaB
CaB
C�B
C�B
C�B
C�B
D3B
DgB
D�B
D�B
D�B
EB
E9B
E�B
E�B
E�B
E�B
FB
F?B
F�B
F�B
FYB
F?B
FtB
FtB
F�B
GB
G�B
HB
HfB
H�B
H�B
IB
IB
IB
J�B
J�B
J�B
J�B
J�B
J�B
K^B
K^B
K)B
K)B
KDB
K�B
K�B
K�B
L0B
L�B
L~B
L�B
MPB
M6B
MjB
M�B
NB
N"B
NVB
N�B
N�B
N�B
OB
PB
PB
PHB
P}B
P�B
P�B
P�B
PbB
Q B
Q B
QNB
QhB
QhB
Q�B
Q�B
R�B
SB
SuB
SuB
S[B
SB
SB
R�B
R�B
R�B
S[B
S�B
SuB
S�B
TaB
T�B
T�B
T�B
T�B
UMB
U�B
U�B
U�B
U�B
UgB
UgB
UgB
U�B
U�B
VB
VmB
V�B
V�B
V�B
V�B
W
B
W$B
W�B
W�B
X+B
XB
XEB
X_B
X�B
X�B
X�B
YeB
Y�B
Y�B
Y�B
ZkB
[qB
\�B
\�B
\xB
\�B
]dB
]�B
]�B
_!B
_�B
`\B
`�B
a-B
a�B
b4B
b4B
a�B
bhB
bhB
b�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
e,B
e,B
e,B
e�B
fB
f�B
f�B
f�B
gB
gmB
g�B
g�B
g�B
g�B
hsB
h�B
h�B
h�B
i*B
i_B
i*B
i�B
i�B
i�B
i�B
i�B
j0B
j�B
j�B
j�B
j�B
kQB
kkB
k�B
k�B
k�B
k�B
k�B
k�B
lWB
lWB
l�B
m)B
m)B
m]B
m�B
nB
nIB
n}B
n}B
n�B
n�B
n�B
oiB
o�B
o�B
pB
p!B
poB
p�B
p�B
p�B
qB
q'B
q[B
qvB
q�B
q�B
rB
r-B
rGB
rGB
r�B
r�B
sB
s3B
sMB
sMB
s�B
s�B
s�B
t9B
t�B
t�B
t�B
t�B
t�B
u%B
uZB
uZB
u�B
v`B
v�B
v�B
v�B
v�B
wB
w2B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y�B
y�B
y�B
zB
y�B
zB
z^B
zxB
z�B
z�B
{B
{JB
{dB
{dB
{dB
|B
|B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}<B
}VB
}VB
}�B
}�B
}�B
}�B
~B
~(B
~�B
.B
.B
cB
cB
}B
}B
�B
�B
�B
� B
�OB
��B
��B
��B
��B
��B
�B
� B
�UB
�oB
��B
��B
��B
��B
��B
��B
�AB
�[B
�uB
��B
�-B
�B
�aB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�MB
�MB
�gB
��B
��B
�B
�SB
�SB
��B
�mB
��B
�B
�?B
�?B
�?B
�YB
�YB
��B
��B
��B
��B
��B
�_B
�_B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�#B
�XB
�rB
��B
��B
�)B
�B
��B
��B
��B
�)B
��B
�B
�0B
�dB
��B
�jB
��B
�jB
�jB
�PB
�jB
�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B��B�B�B��B��B��B�B��B�7B�B�B��B��B��B�}B� B�4B��B��B�?B��B�B�RB��B�"B��B	B	,=B	0;B	\�B	��B	��B	�	B	�>B
}B
�B
	B
3�B
h
B
��B
�mB
ňB
��B
�NB
{�B
z�B
�B
��B
�B
�cB
�yB
x8B
p�B
oB
nB
�4B
�QB
�DB	B
�B
�MB
[B	�^B	�qB	āB	��B	��B	�B	�AB	ncB	G�B	<B	6+B	1[B	,qB	%zB	!B	�B	�B		7B	[B��B��B��B�BB�RB�B�|B�'B�WBޞBˬB��B��B�=B�sB��B�BB��B�B��B��B�B�rB�;B�B��B�B��B	oB	 B	�B	2�B	*�B	"�B	!B	�B	$B	*B	4B	8B	G�B	[#B	ZB	W�B	O\B	3hB	�B	B	)DB	5tB	7LB	:^B	;dB	JXB	WYB	d�B	v�B	sMB	rB	p;B	r�B	m)B	h>B	h$B	{dB	�B	�vB	��B	��B	��B	�B	��B	�MB	�/B	��B	��B	�B	��B	�vB	�bB	�HB	�@B	�FB	�,B	�fB	�B	��B	��B	�sB	�B	�*B	��B	��B	��B	��B	�yB	�fB	��B	��B	�tB	�4B	�kB	�TB	��B	�B	�B	�MB	�tB	�B	�B	��B	�8B	��B	�B	��B	�HB	��B	��B	�B	�cB	�.B	�.B	�iB	ðB	�gB	��B	�UB	ªB	�B	�MB	��B	ǔB	ʌB	��B	��B	̘B	̈́B	̈́B	�<B	�B	�B	͟B	̈́B	οB	�B	�(B	�\B	��B	�pB	�.B	�B	�B	ЗB	�4B	�:B	�B	�NB	��B	�VB	��B	�xB	�rB	�0B	�0B	�0B	�JB	�B	�6B	��B	͹B	�VB	�VB	�BB	�}B	�BB	ΥB	��B	��B	��B	�B	ҽB	��B	ӏB	өB	��B	ՁB	��B	ՁB	՛B	՛B	�MB	�mB	֡B	֡B	��B	�eB	�QB	��B	��B	�	B	یB	�]B	�xB	��B	��B	ݲB	��B	�OB	�!B	�pB	��B	�B	�@B	�B	�kB	�)B	�B	��B	�B	�IB	�cB	��B	�B	��B	��B	�'B	��B	�'B	�'B	��B	�aB	��B	��B	��B	��B	�B	�0B	��B	��B	��B	��B	�DB	��B	�>B	�DB	��B	�B
  B
 OB
 �B
;B
 B
 �B
 B	��B
  B	��B	�}B
 �B
;B
;B
B
 �B
 OB	��B
AB
�B
�B
�B
�B
�B
�B
�B
uB
�B
B
'B
�B
�B
	lB
	RB
)B
DB
)B

�B
B
�B
�B
JB
�B
�B
<B
"B
�B
"B
�B
�B
BB
BB
�B
\B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
B
B
,B
�B
MB
B
B
�B
�B
[B
�B
 B
hB
B
�B
hB
�B
oB
�B
�B
�B
aB
�B
{B
�B
B
MB
gB
�B
B
�B
�B
�B
yB
�B
yB
�B
�B
1B
B
QB
#B
�B
�B
�B
xB
B
dB
dB
dB
�B
�B
5B
jB
�B
�B
;B
pB
 B
 vB
 �B
!B
!�B
!�B
"NB
"NB
"hB
#TB
#�B
#�B
$@B
%,B
%�B
&fB
&�B
'B
'B
'8B
'RB
'mB
'�B
(�B
(�B
)DB
)DB
)�B
*B
*KB
*eB
+B
+6B
+�B
,"B
,qB
,qB
,WB
,�B
,�B
,�B
-�B
-�B
-�B
-�B
.IB
/ B
/OB
/OB
/5B
/�B
/�B
/�B
0oB
0�B
1AB
2B
2-B
2GB
2aB
2B
2aB
2|B
2�B
2�B
33B
2�B
2�B
3B
3�B
4nB
4�B
4�B
4�B
5B
5B
5�B
5�B
5�B
6�B
72B
7LB
7�B
7�B
7�B
7�B
7fB
7�B
7�B
7�B
88B
8RB
8�B
8�B
8�B
9>B
9XB
9XB
9�B
:^B
:�B
:�B
;B
;JB
;0B
<B
<B
<B
<B
;�B
<B
<PB
<�B
<�B
<�B
<�B
<�B
<�B
=VB
=�B
=�B
>(B
>BB
>(B
>BB
>]B
>�B
>�B
>�B
>�B
?B
?�B
?HB
?}B
?}B
?�B
?�B
?�B
@OB
@�B
AoB
A�B
A�B
B'B
BB
B�B
B�B
B�B
B�B
B�B
CaB
C{B
CaB
CaB
CaB
C�B
C�B
C�B
C�B
D3B
DgB
D�B
D�B
D�B
EB
E9B
E�B
E�B
E�B
E�B
FB
F?B
F�B
F�B
FYB
F?B
FtB
FtB
F�B
GB
G�B
HB
HfB
H�B
H�B
IB
IB
IB
J�B
J�B
J�B
J�B
J�B
J�B
K^B
K^B
K)B
K)B
KDB
K�B
K�B
K�B
L0B
L�B
L~B
L�B
MPB
M6B
MjB
M�B
NB
N"B
NVB
N�B
N�B
N�B
OB
PB
PB
PHB
P}B
P�B
P�B
P�B
PbB
Q B
Q B
QNB
QhB
QhB
Q�B
Q�B
R�B
SB
SuB
SuB
S[B
SB
SB
R�B
R�B
R�B
S[B
S�B
SuB
S�B
TaB
T�B
T�B
T�B
T�B
UMB
U�B
U�B
U�B
U�B
UgB
UgB
UgB
U�B
U�B
VB
VmB
V�B
V�B
V�B
V�B
W
B
W$B
W�B
W�B
X+B
XB
XEB
X_B
X�B
X�B
X�B
YeB
Y�B
Y�B
Y�B
ZkB
[qB
\�B
\�B
\xB
\�B
]dB
]�B
]�B
_!B
_�B
`\B
`�B
a-B
a�B
b4B
b4B
a�B
bhB
bhB
b�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
e,B
e,B
e,B
e�B
fB
f�B
f�B
f�B
gB
gmB
g�B
g�B
g�B
g�B
hsB
h�B
h�B
h�B
i*B
i_B
i*B
i�B
i�B
i�B
i�B
i�B
j0B
j�B
j�B
j�B
j�B
kQB
kkB
k�B
k�B
k�B
k�B
k�B
k�B
lWB
lWB
l�B
m)B
m)B
m]B
m�B
nB
nIB
n}B
n}B
n�B
n�B
n�B
oiB
o�B
o�B
pB
p!B
poB
p�B
p�B
p�B
qB
q'B
q[B
qvB
q�B
q�B
rB
r-B
rGB
rGB
r�B
r�B
sB
s3B
sMB
sMB
s�B
s�B
s�B
t9B
t�B
t�B
t�B
t�B
t�B
u%B
uZB
uZB
u�B
v`B
v�B
v�B
v�B
v�B
wB
w2B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y�B
y�B
y�B
zB
y�B
zB
z^B
zxB
z�B
z�B
{B
{JB
{dB
{dB
{dB
|B
|B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}<B
}VB
}VB
}�B
}�B
}�B
}�B
~B
~(B
~�B
.B
.B
cB
cB
}B
}B
�B
�B
�B
� B
�OB
��B
��B
��B
��B
��B
�B
� B
�UB
�oB
��B
��B
��B
��B
��B
��B
�AB
�[B
�uB
��B
�-B
�B
�aB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�MB
�MB
�gB
��B
��B
�B
�SB
�SB
��B
�mB
��B
�B
�?B
�?B
�?B
�YB
�YB
��B
��B
��B
��B
��B
�_B
�_B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�#B
�XB
�rB
��B
��B
�)B
�B
��B
��B
��B
�)B
��B
�B
�0B
�dB
��B
�jB
��B
�jB
�jB
�PB
�jB
�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221204094219  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20221204094236  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221204094237  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221204094237                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221204094237  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221204094237  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221204095810                      G�O�G�O�G�O�                