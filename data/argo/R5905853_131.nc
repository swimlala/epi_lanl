CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-09-10T06:48:11Z creation;2022-09-10T06:48:12Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220910064811  20220910070359  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @���1��1   @��U$��@-��-V�cw�
=p�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B^��Bh  Bp  Bx  B���B�33B���B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2�C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DU��DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@s33@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B^  Bg33Bo33Bw33B�fgB���B�fgB���B���B���B���B���B���B���B�fgB�34B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C�3C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/�gC1�gC3��C5��C7�3C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO�3CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co�gCq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC�ٙC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٙC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%y�D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU��DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dw�3Dxs3Dx�3Dys3Dy�3Dzs3Dz�3D{s3D{�3D|s3D|�3D}s3D}�3D~s3D~�3Ds3D�3D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�<�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D๚D���D�9�D�y�DṚD���D�9�D�y�D⹚D���D�9�D�y�D㹚D���D�9�D�y�D乚D���D�9�D�y�D幚D���D�9�D�y�D湚D���D�9�D�y�D繚D���D�9�D�y�D蹚D���D�9�D�y�D鹚D���D�9�D�y�D깚D���D�9�D�y�D빚D���D�9�D�y�D칚D���D�9�D�y�D���D���D�9�D�y�DD���D�9�D�y�D﹚D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�,�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AۭwA�xlA�n�A��AاRA؀�A�m�A�aA�PA�H�A�>wA�7�A�4nA�.�A�*�A�*0A�$@A�A�A��A��A��A�
�A�1A��A��A��A֧�Aϐ�A�=A�I�A�jA�{JA�:A���A�ffA�[WA�IA�:^A�4A��A��A��A��'A��CA�$�A�5�A���A�T�A�$A��*A�D�A��A��A�d�A�\�A�&�A�e�A�x�A��}A���A��qA��~A��)A�ٴA��,A��>A�a�A���A���A���A���A��2A�A�E�A���A��A���A�A�A���A�v`A�xA���A��A�<6A��A�oiA��A��vA�A}{�A|5�Av�nAr�bAl�AhRTAf��A_��A[�mAYL0AU��AS�AP
�AJ��AH��AH�AFϫAE�AC�YAA�A?�CA>�A=g8A;�A9��A7�:A6��A4�-A2�PA0�5A0"�A.j�A-�A,��A+�A(��A(A'kQA'A&�A$�jA$IRA#�2A"a|A ZA�A��A�AC�A�=A&A�?A-A�A��A��A�A��A��A�WAK^A�zA�*A�HA�A��A��A8AqA��A|�A�FA��A�Ap;A��A*0A�jA �A��A�AkQA�A{�A�rAK^A[�A
�cA
x�A
_�A
C�A	��A	  AE9A�A�@A@�A�A�9Ax�A�A��AbA��A`BAD�AFtAD�A�]Au%AK^AoA��AFtA�A�|A�A��Av`A�A �mA ��A �A _A /@��@��@�`�@�j@�p;@�b@�~�@�\)@�T�@��`@���@��@�H�@�5�@�~(@��q@��@�֡@��6@�"h@�֡@��+@�T�@��@��@��@�a@��@���@���@�;�@�B�@�e@�X@�9@�Q�@��D@�x�@�#�@�Ov@��@�k�@��@�h�@���@���@�O@���@�  @�@�Dg@ަL@ݩ�@��@�2�@���@��@�}�@؇+@�J�@�b@���@ד@���@�(�@��]@�O�@��@�o@ԝI@�1'@��@�خ@�X�@��@�Ĝ@�B[@�	@Ѿw@�E9@Б @�~@��z@�x�@��@κ�@�e�@��@�dZ@�c @˗$@�iD@�[W@��@��o@�RT@�q@�%@��@ȕ@���@ǜ�@�%@Ƙ_@��
@Ō~@�&�@���@�{@ä@@�O�@�@��*@�P�@�&�@�8�@���@�4�@���@���@���@��{@���@�w�@��@��f@�+�@��@��u@���@�~�@��]@���@�v�@�Ta@�:*@�%�@�4@��@�]�@��@���@��F@�7@��@@��@�s@��@�oi@�_@�D�@��A@�e�@��@��6@��z@�~(@�1�@��;@��7@�<6@�Y@��@�\�@��t@�=@�@�w�@��X@��]@��$@�z@�@�@��@��T@���@�|�@�)_@�͟@�z@�B[@�6�@�)�@�&�@��@��@���@�7L@�@�ȴ@��4@�Z@���@�Dg@���@��,@���@�bN@��@��D@���@��S@�J#@��H@��j@��_@�U2@�7@��@��[@�]�@���@�(�@��@��@�c@�(�@��x@�Q�@�1@��@��Q@�s@�&@���@�Xy@�  @��m@��C@��4@�=�@���@�e�@��]@��-@�2a@�u%@�1�@��]@��;@�f�@�7L@� i@��@�_@�6�@�-�@�7@��@�Z�@���@���@�0U@��t@�Q�@�*0@��@���@�GE@��@���@���@��P@�p�@�O@�%F@��c@�Ĝ@���@��@���@�j�@�33@�S@�ں@��<@�p;@�%�@��D@��C@�Vm@�,�@��8@��/@���@�V@�	@���@��'@�a�@�Dg@�-w@�֡@�z@�5?@��d@�o�@�&�@��@���@�_@�{@��Q@��@��)@�{�@�U2@�e@���@��@�/�@��[@���@�W�@��@��@��@�Dg@� \@�֡@��j@�� @�tT@�S�@� �@���@�b�@�33@��@��]@���@��@��D@�c @�>B@�1'@��@��@�N<@�)_@���@��F@�Ta@�(�@�+@�q@9�@~��@~��@~@}rG@|�u@{�@{s@z�c@z�+@y@y\�@y�@x�v@xI�@w��@wv`@w�@vv�@u�@u��@uu�@u=�@t�@tZ@s�+@s��@s|�@r�"@ru%@q�)@q�~@p�f@p_@o�+@o��@o�4@oU�@o)_@n��@m�Z@m��@m��@m��@l�@l��@l��@l*�@k��@k��@k]�@j�@j��@j$�@i��@i��@ic�@i*0@hѷ@hj@g�+@g��@g;d@f��@f�@fh
@e��@e�"@d��@dj@c�@cC�@b�1@bM�@a�d@a��@a^�@`ѷ@`|�@`%�@_�g@_�a@_��@_l�@_�@^d�@]�@]�@]hs@\�K@\��@\C-@[�@[@Z�@Z�!@ZR�@Yԕ@Y%F@X�p@X��@X�o@Xj@XH@Xx@W1�@V��@V͟@VZ�@U��@U�n@UVm@T�	@T��@TH@T�@S��@S�V@S�@R^5@R�@Q�@Q�H@Q��@Q!�@P�@O�@O~�@N�@Ni�@N�@M��@M+@L�.@LtT@K�@K�
@K"�@J�6@I�@I&�@H֡@H��@Hr�@HFt@H-�@G��@G�4@GiD@G]�@GY@Fa|@E�#@E�H@E��@Ek�@D��@C��@CX�@C6z@C�@B�s@BYK@B($@A�>@A�@A	l@@��@@/�@?��@?�@?33@>�@>��@>s�@>C�@>�@=��@=��@=c�@=8�@=&�@<�|@<��@<~(@<I�@<�@;��@;�P@;)_@:�!@:ff@:Z�@:L0@:$�@9��@9��@9`B@9B�@8��@8c�@8*�@7v`@7A�@7S@6z@5��@5IR@4�`@4��@4]d@4D�@4�@3�@2�r@2�@1�@1w2@10�@1%@0l"@/�g@/��@/�4@/@O@/)_@.��@.�\@.1�@-�)@-��@-j@-?}@,��@,�?@,��@,H@,M@+��@+��@+F�@*��@*h
@*@)��@)��@)IR@)�@(��@(2�@(�@'��@'_p@')_@&��@&��@%�T@%�S@%��@%��@%e,@%IR@%<6@%@@$�@$?�@$�@#��@#g�@#@"��@"{@!�7@!hs@!B�@ �9@ >B@�r@��@�g@�*@W?@,�@�@��@�h@z@J�@)�@��@�@T�@(�@�/@��@�.@I�@��@��@��@x@F�@��@��@�@}V@W�@L0@{@�=@S&@V@��@�_@��@c�@%�@�m@�a@�[@��@a@P�@@͟@z@Q@.�@�@��@��@{@J�@!�@�@�H@�'@��@��@J�@	l@�$@��@r�@M@7�@*�@�}@��@qv@/�@�@�@z@8�@�@�@�o@�@��@��@�M@k�@Vm@B�@!�@��@��@~@�+@�@��@��@y�@e�@A�@
=@�B@͟@�m@��@O@��@�@�^@rG@\�@?}@�@��@Ɇ@�z@c�@Q�@?�@"h@  @�W@�;@ݘ@�g@��@��@|�@s@K�@+@$t@S@
�c@
�<@
��@
c @
V@
J�@
1�@
�@	��@	�@	�S@	��@	��@	��@	�~@	zx@	w2@	[W@��@�@�@�@�D@u�@g8@D�@�@�;@�;@�*@�V@�@�@�@�$111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AۭwA�xlA�n�A��AاRA؀�A�m�A�aA�PA�H�A�>wA�7�A�4nA�.�A�*�A�*0A�$@A�A�A��A��A��A�
�A�1A��A��A��A֧�Aϐ�A�=A�I�A�jA�{JA�:A���A�ffA�[WA�IA�:^A�4A��A��A��A��'A��CA�$�A�5�A���A�T�A�$A��*A�D�A��A��A�d�A�\�A�&�A�e�A�x�A��}A���A��qA��~A��)A�ٴA��,A��>A�a�A���A���A���A���A��2A�A�E�A���A��A���A�A�A���A�v`A�xA���A��A�<6A��A�oiA��A��vA�A}{�A|5�Av�nAr�bAl�AhRTAf��A_��A[�mAYL0AU��AS�AP
�AJ��AH��AH�AFϫAE�AC�YAA�A?�CA>�A=g8A;�A9��A7�:A6��A4�-A2�PA0�5A0"�A.j�A-�A,��A+�A(��A(A'kQA'A&�A$�jA$IRA#�2A"a|A ZA�A��A�AC�A�=A&A�?A-A�A��A��A�A��A��A�WAK^A�zA�*A�HA�A��A��A8AqA��A|�A�FA��A�Ap;A��A*0A�jA �A��A�AkQA�A{�A�rAK^A[�A
�cA
x�A
_�A
C�A	��A	  AE9A�A�@A@�A�A�9Ax�A�A��AbA��A`BAD�AFtAD�A�]Au%AK^AoA��AFtA�A�|A�A��Av`A�A �mA ��A �A _A /@��@��@�`�@�j@�p;@�b@�~�@�\)@�T�@��`@���@��@�H�@�5�@�~(@��q@��@�֡@��6@�"h@�֡@��+@�T�@��@��@��@�a@��@���@���@�;�@�B�@�e@�X@�9@�Q�@��D@�x�@�#�@�Ov@��@�k�@��@�h�@���@���@�O@���@�  @�@�Dg@ަL@ݩ�@��@�2�@���@��@�}�@؇+@�J�@�b@���@ד@���@�(�@��]@�O�@��@�o@ԝI@�1'@��@�خ@�X�@��@�Ĝ@�B[@�	@Ѿw@�E9@Б @�~@��z@�x�@��@κ�@�e�@��@�dZ@�c @˗$@�iD@�[W@��@��o@�RT@�q@�%@��@ȕ@���@ǜ�@�%@Ƙ_@��
@Ō~@�&�@���@�{@ä@@�O�@�@��*@�P�@�&�@�8�@���@�4�@���@���@���@��{@���@�w�@��@��f@�+�@��@��u@���@�~�@��]@���@�v�@�Ta@�:*@�%�@�4@��@�]�@��@���@��F@�7@��@@��@�s@��@�oi@�_@�D�@��A@�e�@��@��6@��z@�~(@�1�@��;@��7@�<6@�Y@��@�\�@��t@�=@�@�w�@��X@��]@��$@�z@�@�@��@��T@���@�|�@�)_@�͟@�z@�B[@�6�@�)�@�&�@��@��@���@�7L@�@�ȴ@��4@�Z@���@�Dg@���@��,@���@�bN@��@��D@���@��S@�J#@��H@��j@��_@�U2@�7@��@��[@�]�@���@�(�@��@��@�c@�(�@��x@�Q�@�1@��@��Q@�s@�&@���@�Xy@�  @��m@��C@��4@�=�@���@�e�@��]@��-@�2a@�u%@�1�@��]@��;@�f�@�7L@� i@��@�_@�6�@�-�@�7@��@�Z�@���@���@�0U@��t@�Q�@�*0@��@���@�GE@��@���@���@��P@�p�@�O@�%F@��c@�Ĝ@���@��@���@�j�@�33@�S@�ں@��<@�p;@�%�@��D@��C@�Vm@�,�@��8@��/@���@�V@�	@���@��'@�a�@�Dg@�-w@�֡@�z@�5?@��d@�o�@�&�@��@���@�_@�{@��Q@��@��)@�{�@�U2@�e@���@��@�/�@��[@���@�W�@��@��@��@�Dg@� \@�֡@��j@�� @�tT@�S�@� �@���@�b�@�33@��@��]@���@��@��D@�c @�>B@�1'@��@��@�N<@�)_@���@��F@�Ta@�(�@�+@�q@9�@~��@~��@~@}rG@|�u@{�@{s@z�c@z�+@y@y\�@y�@x�v@xI�@w��@wv`@w�@vv�@u�@u��@uu�@u=�@t�@tZ@s�+@s��@s|�@r�"@ru%@q�)@q�~@p�f@p_@o�+@o��@o�4@oU�@o)_@n��@m�Z@m��@m��@m��@l�@l��@l��@l*�@k��@k��@k]�@j�@j��@j$�@i��@i��@ic�@i*0@hѷ@hj@g�+@g��@g;d@f��@f�@fh
@e��@e�"@d��@dj@c�@cC�@b�1@bM�@a�d@a��@a^�@`ѷ@`|�@`%�@_�g@_�a@_��@_l�@_�@^d�@]�@]�@]hs@\�K@\��@\C-@[�@[@Z�@Z�!@ZR�@Yԕ@Y%F@X�p@X��@X�o@Xj@XH@Xx@W1�@V��@V͟@VZ�@U��@U�n@UVm@T�	@T��@TH@T�@S��@S�V@S�@R^5@R�@Q�@Q�H@Q��@Q!�@P�@O�@O~�@N�@Ni�@N�@M��@M+@L�.@LtT@K�@K�
@K"�@J�6@I�@I&�@H֡@H��@Hr�@HFt@H-�@G��@G�4@GiD@G]�@GY@Fa|@E�#@E�H@E��@Ek�@D��@C��@CX�@C6z@C�@B�s@BYK@B($@A�>@A�@A	l@@��@@/�@?��@?�@?33@>�@>��@>s�@>C�@>�@=��@=��@=c�@=8�@=&�@<�|@<��@<~(@<I�@<�@;��@;�P@;)_@:�!@:ff@:Z�@:L0@:$�@9��@9��@9`B@9B�@8��@8c�@8*�@7v`@7A�@7S@6z@5��@5IR@4�`@4��@4]d@4D�@4�@3�@2�r@2�@1�@1w2@10�@1%@0l"@/�g@/��@/�4@/@O@/)_@.��@.�\@.1�@-�)@-��@-j@-?}@,��@,�?@,��@,H@,M@+��@+��@+F�@*��@*h
@*@)��@)��@)IR@)�@(��@(2�@(�@'��@'_p@')_@&��@&��@%�T@%�S@%��@%��@%e,@%IR@%<6@%@@$�@$?�@$�@#��@#g�@#@"��@"{@!�7@!hs@!B�@ �9@ >B@�r@��@�g@�*@W?@,�@�@��@�h@z@J�@)�@��@�@T�@(�@�/@��@�.@I�@��@��@��@x@F�@��@��@�@}V@W�@L0@{@�=@S&@V@��@�_@��@c�@%�@�m@�a@�[@��@a@P�@@͟@z@Q@.�@�@��@��@{@J�@!�@�@�H@�'@��@��@J�@	l@�$@��@r�@M@7�@*�@�}@��@qv@/�@�@�@z@8�@�@�@�o@�@��@��@�M@k�@Vm@B�@!�@��@��@~@�+@�@��@��@y�@e�@A�@
=@�B@͟@�m@��@O@��@�@�^@rG@\�@?}@�@��@Ɇ@�z@c�@Q�@?�@"h@  @�W@�;@ݘ@�g@��@��@|�@s@K�@+@$t@S@
�c@
�<@
��@
c @
V@
J�@
1�@
�@	��@	�@	�S@	��@	��@	��@	�~@	zx@	w2@	[W@��@�@�@�@�D@u�@g8@D�@�@�;@�;@�*@�V@�@�@�@�$111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
ÖB
�}B
�6B
��B
�	B
��B
��B
�RB
�RB
�lB
��B
��B
�8B
�RB
�8B
�8B
�B
��B
��B
��B
��B
�fB
��B
��B
�2B
��B
��B
��B"B7�BIB��B��BBB5�BF�B[�Bb�B�uB�B��B�=B�0BЗB� BңB�&B�B�(B�oB�wB�"B��B��B��B��B��B��B�fB|6Bn�B]/BC�BxB��B�,BƎB��B�B��B�BB�oBNVB$�B+B�B
��B
�vB
��B
��B
�B
��B
s�B
j�B
WYB
I�B
?�B
2aB
kB

�B
 B	�B	��B	�pB	��B	~BB	eB	OB	=�B	%zB	B	�B�$B��B��B��B�B�@B�BBۦB�?B՛B��BٚBخB�_B��B� B��B��B	VB	�B	"�B	*�B	0�B	-]B	-B	-)B	3�B	;B	:�B	:*B	=�B	7�B	/�B	1�B	5�B	<jB	HKB	RB	NB	J�B	GEB	I�B	ZB	`�B	`B	[WB	V�B	W�B	Y�B	\B	iB	��B	��B	wLB	s�B	v�B	{JB	|�B	�GB	��B	��B	�B	��B	�|B	�tB	�FB	��B	�aB	��B	�?B	��B	�B	�8B	�`B	��B	�fB	�jB	�iB	�{B	��B	�^B	��B	�^B	��B	��B	��B	�VB	��B	�(B	��B	�UB	�B	��B	��B	ƨB	ŢB	��B	�B	�KB	�B	��B	�=B	�	B	�JB	�0B	�DB	�#B	��B	�#B	�	B	�	B	�	B	�B	��B	̘B	�\B	�(B	ΊB	��B	�B	��B	��B	��B	�&B	��B	��B	�B	ּB	��B	�mB	�9B	��B	֡B	�B	�SB	�?B	ּB	�+B	��B	��B	��B	׍B	�YB	��B	�B	ٴB	ٚB	ٴB	�kB	ںB	��B	ۦB	یB	ۦB	��B	ܒB	��B	یB	�WB	�B	�)B	�dB	�B	��B	�B	�pB	��B	�CB	��B	��B	��B	�WB	یB	یB	�]B	��B	�B	� B	�B	�&B	�tB	��B	�B	�@B	�B	�2B	�B	�mB	��B	�B	�=B	�B	�5B	�B	�B	�B	�-B	�B	�MB	�B	�ZB	�B	�zB	��B	��B	��B	��B	�?B	�RB	��B	�	B	��B	��B	�>B	�XB	��B	�DB	�B	��B	��B	��B	�B	��B	��B	�jB	��B	�B	�B	��B	�"B	��B	�<B	��B	��B	�qB	�VB	�qB	��B	��B	�VB	��B	��B	��B	�]B	�BB	��B	��B	��B	��B	��B	�HB	��B	��B	��B
 B
 �B
UB
UB
;B
'B
AB
'B
uB
B
GB
B
�B
mB
+B
�B
�B
�B
�B
�B

#B

	B

�B
0B
B
�B
dB
B
�B
dB
�B
B
B
jB
�B
�B
�B
B
B
�B
�B
}B
}B
bB
�B
bB
}B
.B
�B
�B
�B
�B
HB
�B
�B
�B
�B
}B
�B
�B
�B
BB
BB
vB
�B
4B
4B
�B
NB
B
4B
�B
 B
�B
TB
B
B
�B
[B
uB
�B
[B
&B
[B
uB
�B
�B
�B
B
,B
�B
FB
FB
�B
�B
�B
mB
�B

B
�B
�B
?B
sB
�B
�B
�B
EB
B
�B
�B
7B
�B
qB
�B
�B
B
�B
�B
B
IB
dB
�B
�B
�B
5B
jB
5B
!B
;B
VB
�B
�B
 'B
 vB
 �B
 �B
!B
!�B
!�B
!�B
!�B
!�B
"NB
"hB
#:B
#�B
#�B
#�B
#�B
$tB
%FB
%`B
%�B
%�B
%�B
&LB
&�B
&�B
&�B
&�B
&�B
'B
(>B
(XB
(�B
(�B
(�B
)B
)yB
)_B
)�B
*0B
*B
*B
*B
+B
+QB
+QB
+QB
+�B
,B
,�B
-�B
.�B
/�B
/�B
/�B
0!B
0oB
0�B
0�B
0�B
0�B
0�B
0�B
1�B
2-B
1�B
2�B
3hB
4B
4TB
4�B
4�B
4�B
4�B
4�B
5tB
5�B
6FB
6�B
6�B
7fB
7�B
8RB
8lB
8�B
8�B
8�B
9	B
9$B
9$B
9XB
9�B
:B
9�B
:B
:DB
:^B
:�B
:�B
:�B
:�B
;0B
;JB
;JB
;�B
<PB
<PB
<PB
<PB
<B
<B
<B
<PB
<jB
=B
=�B
>�B
>�B
?}B
?�B
@4B
@B
@4B
@�B
@�B
AUB
A�B
A�B
BB
BB
B[B
B�B
B�B
B�B
C-B
C�B
C�B
DB
D�B
EB
ESB
EmB
E�B
FYB
F�B
F�B
G+B
G_B
G_B
G�B
H1B
H�B
IB
IB
H�B
I7B
I7B
J	B
JXB
JrB
JXB
J�B
J�B
J�B
K�B
K�B
K�B
K�B
LB
L~B
MB
M6B
MB
M6B
M6B
MB
MPB
NB
N"B
NB
N�B
N�B
N�B
OB
OvB
O�B
O�B
O�B
P.B
PB
P�B
P�B
QB
QB
QB
Q4B
Q�B
Q�B
Q�B
RoB
SB
S�B
S�B
S�B
T,B
T,B
T�B
T�B
T�B
U2B
UB
T�B
T{B
UB
U2B
UMB
UgB
U�B
VB
V9B
V�B
W�B
W�B
X+B
W�B
XB
XEB
X+B
XB
X�B
XEB
XEB
XyB
X�B
YeB
YeB
YB
Y�B
ZB
Z7B
ZB
ZQB
ZB
Z�B
[#B
[WB
[�B
[�B
[�B
[�B
\)B
\B
\)B
\CB
\�B
]/B
]IB
\�B
]/B
]IB
]IB
]�B
^OB
^�B
^jB
^jB
^�B
^�B
^�B
_;B
_!B
_�B
_�B
_�B
`�B
`�B
aB
abB
a�B
a�B
b�B
bhB
b�B
b�B
cB
cnB
c�B
dB
d@B
e,B
d�B
d�B
e`B
e�B
ezB
e�B
e�B
e�B
e�B
fB
fLB
f�B
f�B
f�B
gB
gRB
g8B
gRB
g�B
g�B
h>B
h�B
iB
i_B
iDB
iyB
i�B
jKB
jB
j�B
j�B
j�B
kB
kkB
k�B
k�B
l"B
lqB
l�B
l�B
l�B
mCB
mwB
m�B
m�B
nB
n/B
nIB
nB
nB
n}B
n}B
n�B
o B
o5B
o5B
oOB
p;B
p�B
p�B
p�B
p�B
p�B
qAB
qAB
q[B
qvB
q�B
q�B
q�B
rB
r-B
r�B
r�B
sB
shB
s�B
s�B
tTB
t�B
tnB
tTB
t�B
t�B
u?B
utB
u�B
u�B
vB
vB
vzB
v�B
v�B
v�B
w2B
wB
wB
wB
wfB
w�B
w�B
w�B
w�B
xRB
x8B
x�B
y>B
y�B
y�B
y�B
z*B
z�B
{0B
{�B
|�B
}B
}�B
}�B
}�B
~(B
~BB
~�B
~�B
HB
cB
}B
}B
}B
}B
�B
�B
� B
�OB
��B
��B
��B
�B
� B
� B
� B
� B
�;B
�UB
� B
�;B
�;B
�;B
�UB
�UB
�UB
��B
�oB
�B
��B
��B
��B
��B
�GB
�{B
��B
��B
��B
�B
�gB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�9B
�mB
�SB
�SB
��B
��B
�%B
�YB
�?B
�YB
��B
��B
�B
�+B
��B
��B
��B
��B
��B
��B
�1B
��B
��B
��B
��B
��B
�RB
�lB
�7B
�7B
�B
��B
��B
��B
��B
��B
��B
��B
�DB
�DB
��B
��B
��B
��B
��B
�xB
��B
�B
��B
�0B
�0B
�0B
�d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
ÖB
�}B
�6B
��B
�	B
��B
��B
�RB
�RB
�lB
��B
��B
�8B
�RB
�8B
�8B
�B
��B
��B
��B
��B
�fB
��B
��B
�2B
��B
��B
��B"B7�BIB��B��BBB5�BF�B[�Bb�B�uB�B��B�=B�0BЗB� BңB�&B�B�(B�oB�wB�"B��B��B��B��B��B��B�fB|6Bn�B]/BC�BxB��B�,BƎB��B�B��B�BB�oBNVB$�B+B�B
��B
�vB
��B
��B
�B
��B
s�B
j�B
WYB
I�B
?�B
2aB
kB

�B
 B	�B	��B	�pB	��B	~BB	eB	OB	=�B	%zB	B	�B�$B��B��B��B�B�@B�BBۦB�?B՛B��BٚBخB�_B��B� B��B��B	VB	�B	"�B	*�B	0�B	-]B	-B	-)B	3�B	;B	:�B	:*B	=�B	7�B	/�B	1�B	5�B	<jB	HKB	RB	NB	J�B	GEB	I�B	ZB	`�B	`B	[WB	V�B	W�B	Y�B	\B	iB	��B	��B	wLB	s�B	v�B	{JB	|�B	�GB	��B	��B	�B	��B	�|B	�tB	�FB	��B	�aB	��B	�?B	��B	�B	�8B	�`B	��B	�fB	�jB	�iB	�{B	��B	�^B	��B	�^B	��B	��B	��B	�VB	��B	�(B	��B	�UB	�B	��B	��B	ƨB	ŢB	��B	�B	�KB	�B	��B	�=B	�	B	�JB	�0B	�DB	�#B	��B	�#B	�	B	�	B	�	B	�B	��B	̘B	�\B	�(B	ΊB	��B	�B	��B	��B	��B	�&B	��B	��B	�B	ּB	��B	�mB	�9B	��B	֡B	�B	�SB	�?B	ּB	�+B	��B	��B	��B	׍B	�YB	��B	�B	ٴB	ٚB	ٴB	�kB	ںB	��B	ۦB	یB	ۦB	��B	ܒB	��B	یB	�WB	�B	�)B	�dB	�B	��B	�B	�pB	��B	�CB	��B	��B	��B	�WB	یB	یB	�]B	��B	�B	� B	�B	�&B	�tB	��B	�B	�@B	�B	�2B	�B	�mB	��B	�B	�=B	�B	�5B	�B	�B	�B	�-B	�B	�MB	�B	�ZB	�B	�zB	��B	��B	��B	��B	�?B	�RB	��B	�	B	��B	��B	�>B	�XB	��B	�DB	�B	��B	��B	��B	�B	��B	��B	�jB	��B	�B	�B	��B	�"B	��B	�<B	��B	��B	�qB	�VB	�qB	��B	��B	�VB	��B	��B	��B	�]B	�BB	��B	��B	��B	��B	��B	�HB	��B	��B	��B
 B
 �B
UB
UB
;B
'B
AB
'B
uB
B
GB
B
�B
mB
+B
�B
�B
�B
�B
�B

#B

	B

�B
0B
B
�B
dB
B
�B
dB
�B
B
B
jB
�B
�B
�B
B
B
�B
�B
}B
}B
bB
�B
bB
}B
.B
�B
�B
�B
�B
HB
�B
�B
�B
�B
}B
�B
�B
�B
BB
BB
vB
�B
4B
4B
�B
NB
B
4B
�B
 B
�B
TB
B
B
�B
[B
uB
�B
[B
&B
[B
uB
�B
�B
�B
B
,B
�B
FB
FB
�B
�B
�B
mB
�B

B
�B
�B
?B
sB
�B
�B
�B
EB
B
�B
�B
7B
�B
qB
�B
�B
B
�B
�B
B
IB
dB
�B
�B
�B
5B
jB
5B
!B
;B
VB
�B
�B
 'B
 vB
 �B
 �B
!B
!�B
!�B
!�B
!�B
!�B
"NB
"hB
#:B
#�B
#�B
#�B
#�B
$tB
%FB
%`B
%�B
%�B
%�B
&LB
&�B
&�B
&�B
&�B
&�B
'B
(>B
(XB
(�B
(�B
(�B
)B
)yB
)_B
)�B
*0B
*B
*B
*B
+B
+QB
+QB
+QB
+�B
,B
,�B
-�B
.�B
/�B
/�B
/�B
0!B
0oB
0�B
0�B
0�B
0�B
0�B
0�B
1�B
2-B
1�B
2�B
3hB
4B
4TB
4�B
4�B
4�B
4�B
4�B
5tB
5�B
6FB
6�B
6�B
7fB
7�B
8RB
8lB
8�B
8�B
8�B
9	B
9$B
9$B
9XB
9�B
:B
9�B
:B
:DB
:^B
:�B
:�B
:�B
:�B
;0B
;JB
;JB
;�B
<PB
<PB
<PB
<PB
<B
<B
<B
<PB
<jB
=B
=�B
>�B
>�B
?}B
?�B
@4B
@B
@4B
@�B
@�B
AUB
A�B
A�B
BB
BB
B[B
B�B
B�B
B�B
C-B
C�B
C�B
DB
D�B
EB
ESB
EmB
E�B
FYB
F�B
F�B
G+B
G_B
G_B
G�B
H1B
H�B
IB
IB
H�B
I7B
I7B
J	B
JXB
JrB
JXB
J�B
J�B
J�B
K�B
K�B
K�B
K�B
LB
L~B
MB
M6B
MB
M6B
M6B
MB
MPB
NB
N"B
NB
N�B
N�B
N�B
OB
OvB
O�B
O�B
O�B
P.B
PB
P�B
P�B
QB
QB
QB
Q4B
Q�B
Q�B
Q�B
RoB
SB
S�B
S�B
S�B
T,B
T,B
T�B
T�B
T�B
U2B
UB
T�B
T{B
UB
U2B
UMB
UgB
U�B
VB
V9B
V�B
W�B
W�B
X+B
W�B
XB
XEB
X+B
XB
X�B
XEB
XEB
XyB
X�B
YeB
YeB
YB
Y�B
ZB
Z7B
ZB
ZQB
ZB
Z�B
[#B
[WB
[�B
[�B
[�B
[�B
\)B
\B
\)B
\CB
\�B
]/B
]IB
\�B
]/B
]IB
]IB
]�B
^OB
^�B
^jB
^jB
^�B
^�B
^�B
_;B
_!B
_�B
_�B
_�B
`�B
`�B
aB
abB
a�B
a�B
b�B
bhB
b�B
b�B
cB
cnB
c�B
dB
d@B
e,B
d�B
d�B
e`B
e�B
ezB
e�B
e�B
e�B
e�B
fB
fLB
f�B
f�B
f�B
gB
gRB
g8B
gRB
g�B
g�B
h>B
h�B
iB
i_B
iDB
iyB
i�B
jKB
jB
j�B
j�B
j�B
kB
kkB
k�B
k�B
l"B
lqB
l�B
l�B
l�B
mCB
mwB
m�B
m�B
nB
n/B
nIB
nB
nB
n}B
n}B
n�B
o B
o5B
o5B
oOB
p;B
p�B
p�B
p�B
p�B
p�B
qAB
qAB
q[B
qvB
q�B
q�B
q�B
rB
r-B
r�B
r�B
sB
shB
s�B
s�B
tTB
t�B
tnB
tTB
t�B
t�B
u?B
utB
u�B
u�B
vB
vB
vzB
v�B
v�B
v�B
w2B
wB
wB
wB
wfB
w�B
w�B
w�B
w�B
xRB
x8B
x�B
y>B
y�B
y�B
y�B
z*B
z�B
{0B
{�B
|�B
}B
}�B
}�B
}�B
~(B
~BB
~�B
~�B
HB
cB
}B
}B
}B
}B
�B
�B
� B
�OB
��B
��B
��B
�B
� B
� B
� B
� B
�;B
�UB
� B
�;B
�;B
�;B
�UB
�UB
�UB
��B
�oB
�B
��B
��B
��B
��B
�GB
�{B
��B
��B
��B
�B
�gB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�9B
�mB
�SB
�SB
��B
��B
�%B
�YB
�?B
�YB
��B
��B
�B
�+B
��B
��B
��B
��B
��B
��B
�1B
��B
��B
��B
��B
��B
�RB
�lB
�7B
�7B
�B
��B
��B
��B
��B
��B
��B
��B
�DB
�DB
��B
��B
��B
��B
��B
�xB
��B
�B
��B
�0B
�0B
�0B
�d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220910064802  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220910064811  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220910064811  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220910064812                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220910154816  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220910154816  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220910070359                      G�O�G�O�G�O�                