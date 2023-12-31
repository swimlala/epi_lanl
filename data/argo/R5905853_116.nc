CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:43:37Z creation;2022-06-04T17:43:38Z conversion to V3.1      
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tH   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604174337  20220610141505  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               tA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @����'1   @��ωJ��@/�+I��co�l�C�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A   A   A>ffA^ffA�  A�  A�  A�  A�  A���A�  A���B ffB��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�ffB�33B���B�  B̙�B�  B�ffB�  B���B�  B�  B�  B�33BB���B�  B�  C   C  C  C  C�fC	�fC  C  C  C  C  C  C�fC  C�C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH33CI�fCK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�C3DÃ3D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @y��@���@���A��A;33A[33A|��A�ffA�ffA�ffA�ffA�33A�ffA�33A�32B��B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B�  B���B�fgBǙ�B�34Bϙ�B�  Bי�B�fgBߙ�B㙚B癚B���B�34B�fgB���B���B���C��C��C��C�3C	�3C��C��C��C��C��C��C�3C��C�gC�gC��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CH  CI�3CK�3CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR��DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dw�3Dxs3Dx�3Dys3Dy�3Dzs3Dz�3D{s3D{�3D|s3D|�3D}s3D}�3D~s3D~�3Ds3D�3D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�<�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�<�D�|�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D๚D���D�9�D�y�DṚD���D�9�D�y�D⹚D���D�9�D�y�D㹚D���D�9�D�y�D乚D���D�9�D�y�D幚D���D�9�D�y�D湚D���D�9�D�y�D繚D���D�9�D�y�D蹚D���D�9�D�y�D鹚D���D�9�D�y�D깚D���D�9�D�y�D빚D���D�9�D�y�D칚D���D�9�D�y�D���D���D�9�D�y�DD���D�9�D�y�D﹚D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A�ߤA���A���A�A���A���A���A��.A��.A���A��A�{AξwAΫAη�Aμ6Aα[A�i�A�dZA�`�A�QNA�L�A�N<A�OBA�OBA�PA�M�A�FA�B�A�C�A�C�A�A�A�B[A�CaA�?A�9�A�3�A�,�A�+6A�!�A�A�xA˒�A�ĜA�_pA��A���A�:*A��2A�7�A��xA��A���A�?HA���A�~A�,�A���A�ŢA�xA���A�xA��bA��(A�I�A��KA�dZA��A�*�A��A�2aA��&A�ߤA}M�Aw�Au4Ao{JAl� Ak�~Aj�QAj;�Ah�)AfsAe%FA_�]AWںAPw2AL�rAI?}AGS&AF�AE��AC�'AB�AA��A@��A>��A>��A?�#A=�A;rGA8A�A7OA6�A6xA5��A4HA2�UA2^�A1�A1��A1tTA0��A/҉A/kQA/E�A.�cA.҉A.r�A-h
A,��A,��A,_�A+��A*��A*xA*8�A)�WA)��A)kQA):*A(�KA(m�A'��A&�}A&cA&Z�A&DgA&~A%�rA%($A#|�A#E�A"��A!~�A��A��A3�A@A�}A_�A��A�pA]dA�YA�AA;A��A��AV�A/�A�A�[A��A��A��A+�A�wA!�A�mA��A�AAs�Af�A]�AF�A�AߤA��ARTA��Aq�A�A�Ag�A�1A�AJ�A�WA�kAZ�A"hA��A�A�A�xAIRAFtA8�A
�KA
��A
K^A	��A	�A	��A	��A	��A	MjA	�A�IAϫA_A��AE9A�=A"�Al"A��Ag8A�]A)_A�]A_�@���@�^5@���@�Z�@�b@��@�a�@�z�@���@���@�G�@�4n@�U�@�@�%@�@�p;@��@�@@�<6@�kQ@��Q@�J�@��T@�5�@�@���@��'@��[@ꤩ@�{@�C@�l�@�!�@�P@��@�,=@��N@��@�v`@�=@�R@�,�@�2�@�@�U�@�IR@��K@�(�@��@��@�6@���@�(@�q�@ۥ@�5�@��@ڑ @���@ٓ@�4�@��@�p;@��@��Q@�x@��[@օ�@���@�&@��s@Ԥ�@�1'@ӍP@�S@ҩ�@�oi@�Z�@�!@�^�@��@�d�@�@�`B@��@�#:@��j@͔�@�(�@��c@̬�@̌@���@��@���@���@�a�@�:�@���@�z@���@�F@�w�@�J@Ų-@�O@ĥz@�1�@��@�[W@��/@_@�~(@��@�Vm@��@���@�l�@�@�5�@��)@��_@�M�@��@���@�/@��R@��!@��L@�R�@��.@���@���@�O�@�6z@��@��}@��D@�v�@�Ta@�2�@�1@�@� i@��\@�[�@��@���@���@���@�T�@��@��~@�]�@�+@��B@�v�@��m@��=@�C@��5@��@���@��D@�u�@�Q�@�@��;@��@��/@��@��$@��O@��_@�n�@���@�@�Z@�?@���@��@�z�@���@�IR@��E@��Y@�@�@���@�E9@��8@��H@��o@�V�@�!@�dZ@��M@���@��@�$t@�ߤ@��<@�%�@���@���@�v�@��@��@�Z�@��@�A�@��@��~@�4�@���@��U@��x@��Y@�N�@�/�@���@��M@��@��6@�z�@�YK@�(�@�b@�1@���@���@��@��N@�n/@�C@��M@�~�@��@���@��Q@��a@��-@�;d@�ߤ@�tT@� �@�4@��D@��>@��@�o @���@��X@���@�c�@�Q@�J@��@�ϫ@��a@���@��@��\@�J�@�Ft@��@��W@���@��-@�}�@�A�@�*0@��f@���@��/@��,@��@��<@��h@�w�@� �@�ݘ@��[@�hs@�/@��@��I@�M@�l�@�%@�ѷ@���@��}@��L@���@�M@�P�@�6z@�;@�͟@��)@��?@���@��6@��@�q�@�d�@�B[@�b@���@�@��o@�_�@�$�@�f�@� \@�֡@���@�Z@���@��C@���@�k�@�L�@�.I@���@�.�@��@�˒@�x@�33@�/@��@���@�Ĝ@���@��@���@���@���@��"@�o�@�!�@�V@���@�͟@��'@��@�Q@��@�C@��@�C�@�0@_p@~�]@~��@~GE@}�T@}��@}�@}�@|�p@|�u@|	�@{��@{Mj@z�m@y�.@yN<@y�@x��@x7@w��@wW?@v��@vV@u��@uw2@u?}@tѷ@t!@s�{@sX�@rȴ@rM�@q��@q�@q��@q@q��@qe,@q�@p�|@p�@pH@o�a@oK�@n��@n-@m�o@m�@m��@m@l��@k˒@j��@i��@i�@ij@iS&@i�@h~(@h'R@h~@h7@h�@h!@hb@g�@gK�@f��@e�.@e?}@cO@b��@cg�@cƨ@c��@c�K@d�@d��@d�@c�;@b�H@ao @`��@`�9@` �@_�:@_�@^�@^�s@^��@^�@^��@^h
@^\�@^L0@^+k@]�#@]�C@]\�@\z�@\M@[�6@[�f@[qv@[S�@[�@Z��@Z�M@Z��@Z�H@Z�H@Z�s@ZR�@Yc�@X��@X�O@XH@W��@W�@W�@W i@V+k@U��@U��@U��@Uc�@T��@T��@T�@Ty>@T>B@S�@SH�@R��@R�H@R�,@R�\@R�@Q�^@Q�@QX@Q-w@P�@PbN@O��@O��@Oe�@N��@N4@M��@M��@MF@L��@L7�@K�0@K�:@KW?@K�@KS@J��@Jn�@J!�@I�^@I@IT�@Hq@G�@G�@H"h@G�g@G��@Gj�@GF�@F�"@F�X@F��@Fc @E�@E�@D]d@C�F@C=@C@B��@A�o@Ap�@A	l@@�O@@@?��@?!-@?�@>�y@>�6@>��@>�@=��@=u�@<�@<j@<V�@<<�@<%�@<�@;��@;)_@:҉@:��@:��@:H�@:$�@9�z@9��@9`B@9=�@9�@9q@9@9�@8��@8�@8?�@8�@7�@7��@7E9@6��@6��@6e@5x�@4�@4��@4�[@4�@4�@4�.@4M@41@3�@3�Q@3خ@3��@3P�@3�@3Y@3
=@2�@2��@2�@2��@2�r@2ff@2YK@2Ov@2@�@1��@1�X@1IR@1;@0�@0U2@07�@0,=@0x@/��@/4�@.�@.�m@.~�@-��@-ԕ@-�@-��@-��@-�M@-�@,�U@,Z@,	�@+��@+��@+dZ@+P�@+)_@*�\@*M�@)��@)��@)f�@)0�@(��@(�@(�?@(��@(��@(tT@(PH@(%�@(�@'ݘ@'��@'l�@'=@'�@&�@&�h@&�1@&^5@%��@%��@%��@%�h@%L�@%2a@%�@$�|@$�)@$�o@#��@#��@#_p@#J#@#=@#'�@#"�@#�@#�@"͟@"?@!�o@!�-@!p�@!7L@!%@ �e@ j@ `�@ N�@ 1'@�]@�@�@@,�@ff@B[@�@_@��@�z@��@x�@f�@^�@Y�@F@�I@	�@��@��@��@�$@RT@��@��@��@W�@+k@J@��@zx@X@F@A @8�@�@*�@��@��@��@Mj@'�@�@�@a|@;�@�N@�X@c�@&�@�@�@��@tT@K^@�@��@\)@'�@�@�@�c@҉@�1@B[@�@�j@L�@/@�@�@�@�K@��@tT@Q�@?�@:�@6@/�@*�@(�@M@�g@~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A�ߤA���A���A�A���A���A���A��.A��.A���A��A�{AξwAΫAη�Aμ6Aα[A�i�A�dZA�`�A�QNA�L�A�N<A�OBA�OBA�PA�M�A�FA�B�A�C�A�C�A�A�A�B[A�CaA�?A�9�A�3�A�,�A�+6A�!�A�A�xA˒�A�ĜA�_pA��A���A�:*A��2A�7�A��xA��A���A�?HA���A�~A�,�A���A�ŢA�xA���A�xA��bA��(A�I�A��KA�dZA��A�*�A��A�2aA��&A�ߤA}M�Aw�Au4Ao{JAl� Ak�~Aj�QAj;�Ah�)AfsAe%FA_�]AWںAPw2AL�rAI?}AGS&AF�AE��AC�'AB�AA��A@��A>��A>��A?�#A=�A;rGA8A�A7OA6�A6xA5��A4HA2�UA2^�A1�A1��A1tTA0��A/҉A/kQA/E�A.�cA.҉A.r�A-h
A,��A,��A,_�A+��A*��A*xA*8�A)�WA)��A)kQA):*A(�KA(m�A'��A&�}A&cA&Z�A&DgA&~A%�rA%($A#|�A#E�A"��A!~�A��A��A3�A@A�}A_�A��A�pA]dA�YA�AA;A��A��AV�A/�A�A�[A��A��A��A+�A�wA!�A�mA��A�AAs�Af�A]�AF�A�AߤA��ARTA��Aq�A�A�Ag�A�1A�AJ�A�WA�kAZ�A"hA��A�A�A�xAIRAFtA8�A
�KA
��A
K^A	��A	�A	��A	��A	��A	MjA	�A�IAϫA_A��AE9A�=A"�Al"A��Ag8A�]A)_A�]A_�@���@�^5@���@�Z�@�b@��@�a�@�z�@���@���@�G�@�4n@�U�@�@�%@�@�p;@��@�@@�<6@�kQ@��Q@�J�@��T@�5�@�@���@��'@��[@ꤩ@�{@�C@�l�@�!�@�P@��@�,=@��N@��@�v`@�=@�R@�,�@�2�@�@�U�@�IR@��K@�(�@��@��@�6@���@�(@�q�@ۥ@�5�@��@ڑ @���@ٓ@�4�@��@�p;@��@��Q@�x@��[@օ�@���@�&@��s@Ԥ�@�1'@ӍP@�S@ҩ�@�oi@�Z�@�!@�^�@��@�d�@�@�`B@��@�#:@��j@͔�@�(�@��c@̬�@̌@���@��@���@���@�a�@�:�@���@�z@���@�F@�w�@�J@Ų-@�O@ĥz@�1�@��@�[W@��/@_@�~(@��@�Vm@��@���@�l�@�@�5�@��)@��_@�M�@��@���@�/@��R@��!@��L@�R�@��.@���@���@�O�@�6z@��@��}@��D@�v�@�Ta@�2�@�1@�@� i@��\@�[�@��@���@���@���@�T�@��@��~@�]�@�+@��B@�v�@��m@��=@�C@��5@��@���@��D@�u�@�Q�@�@��;@��@��/@��@��$@��O@��_@�n�@���@�@�Z@�?@���@��@�z�@���@�IR@��E@��Y@�@�@���@�E9@��8@��H@��o@�V�@�!@�dZ@��M@���@��@�$t@�ߤ@��<@�%�@���@���@�v�@��@��@�Z�@��@�A�@��@��~@�4�@���@��U@��x@��Y@�N�@�/�@���@��M@��@��6@�z�@�YK@�(�@�b@�1@���@���@��@��N@�n/@�C@��M@�~�@��@���@��Q@��a@��-@�;d@�ߤ@�tT@� �@�4@��D@��>@��@�o @���@��X@���@�c�@�Q@�J@��@�ϫ@��a@���@��@��\@�J�@�Ft@��@��W@���@��-@�}�@�A�@�*0@��f@���@��/@��,@��@��<@��h@�w�@� �@�ݘ@��[@�hs@�/@��@��I@�M@�l�@�%@�ѷ@���@��}@��L@���@�M@�P�@�6z@�;@�͟@��)@��?@���@��6@��@�q�@�d�@�B[@�b@���@�@��o@�_�@�$�@�f�@� \@�֡@���@�Z@���@��C@���@�k�@�L�@�.I@���@�.�@��@�˒@�x@�33@�/@��@���@�Ĝ@���@��@���@���@���@��"@�o�@�!�@�V@���@�͟@��'@��@�Q@��@�C@��@�C�@�0@_p@~�]@~��@~GE@}�T@}��@}�@}�@|�p@|�u@|	�@{��@{Mj@z�m@y�.@yN<@y�@x��@x7@w��@wW?@v��@vV@u��@uw2@u?}@tѷ@t!@s�{@sX�@rȴ@rM�@q��@q�@q��@q@q��@qe,@q�@p�|@p�@pH@o�a@oK�@n��@n-@m�o@m�@m��@m@l��@k˒@j��@i��@i�@ij@iS&@i�@h~(@h'R@h~@h7@h�@h!@hb@g�@gK�@f��@e�.@e?}@cO@b��@cg�@cƨ@c��@c�K@d�@d��@d�@c�;@b�H@ao @`��@`�9@` �@_�:@_�@^�@^�s@^��@^�@^��@^h
@^\�@^L0@^+k@]�#@]�C@]\�@\z�@\M@[�6@[�f@[qv@[S�@[�@Z��@Z�M@Z��@Z�H@Z�H@Z�s@ZR�@Yc�@X��@X�O@XH@W��@W�@W�@W i@V+k@U��@U��@U��@Uc�@T��@T��@T�@Ty>@T>B@S�@SH�@R��@R�H@R�,@R�\@R�@Q�^@Q�@QX@Q-w@P�@PbN@O��@O��@Oe�@N��@N4@M��@M��@MF@L��@L7�@K�0@K�:@KW?@K�@KS@J��@Jn�@J!�@I�^@I@IT�@Hq@G�@G�@H"h@G�g@G��@Gj�@GF�@F�"@F�X@F��@Fc @E�@E�@D]d@C�F@C=@C@B��@A�o@Ap�@A	l@@�O@@@?��@?!-@?�@>�y@>�6@>��@>�@=��@=u�@<�@<j@<V�@<<�@<%�@<�@;��@;)_@:҉@:��@:��@:H�@:$�@9�z@9��@9`B@9=�@9�@9q@9@9�@8��@8�@8?�@8�@7�@7��@7E9@6��@6��@6e@5x�@4�@4��@4�[@4�@4�@4�.@4M@41@3�@3�Q@3خ@3��@3P�@3�@3Y@3
=@2�@2��@2�@2��@2�r@2ff@2YK@2Ov@2@�@1��@1�X@1IR@1;@0�@0U2@07�@0,=@0x@/��@/4�@.�@.�m@.~�@-��@-ԕ@-�@-��@-��@-�M@-�@,�U@,Z@,	�@+��@+��@+dZ@+P�@+)_@*�\@*M�@)��@)��@)f�@)0�@(��@(�@(�?@(��@(��@(tT@(PH@(%�@(�@'ݘ@'��@'l�@'=@'�@&�@&�h@&�1@&^5@%��@%��@%��@%�h@%L�@%2a@%�@$�|@$�)@$�o@#��@#��@#_p@#J#@#=@#'�@#"�@#�@#�@"͟@"?@!�o@!�-@!p�@!7L@!%@ �e@ j@ `�@ N�@ 1'@�]@�@�@@,�@ff@B[@�@_@��@�z@��@x�@f�@^�@Y�@F@�I@	�@��@��@��@�$@RT@��@��@��@W�@+k@J@��@zx@X@F@A @8�@�@*�@��@��@��@Mj@'�@�@�@a|@;�@�N@�X@c�@&�@�@�@��@tT@K^@�@��@\)@'�@�@�@�c@҉@�1@B[@�@�j@L�@/@�@�@�@�K@��@tT@Q�@?�@:�@6@/�@*�@(�@M@�g@~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	_�B	_�B	_pB	_pB	_;B	_;B	_pB	_VB	_�B	_pB	_pB	_B	_pB	aHB	a�B	a|B	a�B	a|B	cnB	cB	c B	c�B	d@B	d�B	e,B	e,B	e`B	e,B	d�B	dZB	d�B	d�B	dZB	dZB	d@B	dB	c�B	cTB	b�B	b�B	a�B	`BB	g�B	dtB	�	B	��B
�B
n�B
��B
��B
��B
�:B
�B
�B
�wB
�AB
�[B
�qB
��B
�B
��B
�OB
��B
�eB
��B
��B
m)B
\]B
F%B
=�B
/�B
�B	��B	�GB	��B	�CB	�kB	��B	rB	l"B	h$B	d�B	_�B	V�B	OvB	;B	7B��B�;B�B�0B�B��B��B�B��B	oB�BB	jB	AUB	D�B	;B	1vB	B�B	IRB	O�B	abB	z�B	��B	��B	��B	��B	��B	�TB	��B	�`B	�B	��B	��B	�gB	�,B	�=B	��B	�hB	��B	�;B	�B	�tB	�B	�	B	��B	�xB	�JB	�B
 �B
%B
�B
�B
1B
	�B

�B
VB
B
	B
%B
 �B	��B	�B	�0B	��B	��B	�B	�	B	��B	��B	�IB	�0B	�B	�*B	�eB	�B	�B	��B	�B	�B
�B
~B
&B
�B
�B
YB
sB
�B
�B
�B
�B
�B
�B
�B

B
�B
�B
�B
}B
�B
B
	B
�B
3B
3B
�B
YB
�B
�B
�B
EB
�B
 4B
 B	��B
�B
�B
�B
�B
�B
gB
�B
	RB

�B

�B
�B
DB
	�B

XB
	B
B
gB
3B
�B	�B	�B	��B	��B	�B	�FB	��B	�qB	�QB	��B	�2B	�B	��B	�B	�ZB	��B	�B	�bB	�	B	�KB	�7B	�B	��B	��B	��B	�B	�B	� B	��B	�B	�B	��B	�B	�B	�B	�B	�qB	�B	�B	�B	�zB	�$B	�WB	�B	�`B	��B	��B	�ZB	��B	��B	�QB	�QB	ڠB	یB	��B	�_B	��B	�=B	��B	ٴB	�B	ּB	��B	�B	�B	�SB	�9B	�mB	�?B	�?B	��B	��B	ּB	�
B	ּB	��B	ؓB	��B	�	B	�B	��B	�=B	��B	�QB	�B	ٴB	�B	�eB	�eB	�eB	چB	چB	ۦB	�/B	��B	ߊB	�B	�fB	�2B	�B	�B	��B	�*B	��B	��B	��B	�KB	�B	�B	� B	�UB	��B	��B	��B	�!B	�!B	��B	�vB	�hB	�ZB	�B	�TB	�B	��B	��B	�B	�nB	��B	�B	��B	��B	�tB	��B	�$B	��B	�$B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�jB	��B	�B	�6B	��B	��B	��B	��B	�PB	�"B	��B	��B	�B	��B	��B	�B	�(B	�HB	�.B	�.B	�HB	�}B	�}B	�}B	�}B	�HB	�}B
 �B
 �B
 �B
UB
�B
�B
�B
�B
{B
-B
�B
�B
MB
�B
�B
�B
B
3B
gB
SB
9B
mB
mB
SB
9B
�B
SB
B
9B
9B
�B
�B
B
mB
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B

#B

=B

�B

�B
B
�B
�B
�B
VB
�B
BB
vB
�B
�B
�B
�B
�B
�B
NB
4B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
FB
�B
�B
�B
2B
B
9B
�B
�B
�B
�B
�B
�B
yB
yB
�B
�B
�B
�B
B
eB
�B
�B
]B
B
dB
~B
�B
~B
~B
B
�B
!B
;B
�B
pB
�B
�B
 \B
!|B
!�B
"B
"4B
"B
"B
!�B
# B
#�B
#�B
$ZB
$�B
$tB
$tB
$ZB
$�B
$�B
$�B
$�B
$�B
$�B
%FB
&�B
&�B
&LB
&2B
'�B
'�B
(sB
(�B
(�B
)_B
)_B
)yB
)_B
)_B
)B
)*B
+QB
*�B
*�B
+�B
,�B
,qB
-CB
-CB
-)B
-)B
-)B
-B
.cB
.cB
.B
-�B
.B
.�B
/�B
/�B
/�B
/�B
/�B
.�B
.IB
-wB
-CB
-)B
-CB
.cB
.�B
.�B
/ B
/5B
/�B
0B
/�B
0B
0;B
0�B
1'B
1�B
2B
2GB
2�B
3hB
3�B
3�B
3�B
4�B
5�B
6�B
6�B
6�B
7fB
88B
8�B
8�B
9rB
9�B
9�B
:B
:*B
:*B
:*B
:xB
:�B
:�B
;dB
;�B
<B
;�B
;dB
;B
;0B
;0B
;�B
=B
<�B
<�B
=<B
=B
<�B
<�B
<�B
<�B
<�B
<�B
=B
=B
=B
<�B
<�B
<�B
<�B
<jB
;�B
<B
<�B
<�B
>�B
@�B
@OB
@�B
AB
CB
C{B
B�B
A�B
?�B
>BB
=�B
=�B
>(B
>wB
>]B
>wB
>wB
>wB
>�B
>�B
>�B
>�B
>�B
?HB
>�B
?HB
@ B
@B
@�B
AB
@�B
A B
A�B
A�B
A�B
A�B
BAB
B'B
BB
BuB
B�B
C-B
CGB
C�B
D�B
D�B
D�B
D�B
E�B
E9B
EB
E9B
ESB
E�B
E�B
E�B
E�B
E�B
F?B
G+B
G_B
G_B
GEB
G_B
G�B
HB
HB
HKB
H1B
H�B
H�B
H�B
H�B
IB
H�B
H1B
HKB
H�B
KB
KDB
J�B
J�B
K)B
L0B
L�B
M�B
M�B
M�B
M�B
NpB
O�B
O�B
O�B
O\B
O�B
Q�B
RB
RTB
RTB
R�B
SB
S[B
S�B
S�B
S�B
T{B
TaB
T�B
T�B
T�B
UB
U�B
U�B
VmB
V�B
VSB
V�B
V�B
W$B
W�B
W�B
X+B
XEB
XEB
X�B
YB
X�B
Y�B
ZB
ZB
ZB
ZkB
[#B
[WB
[�B
\)B
\xB
\xB
\�B
]B
]IB
]�B
^�B
^�B
^�B
^�B
^�B
_B
_VB
_pB
_pB
_pB
`vB
`�B
a-B
abB
b4B
b�B
b�B
b�B
b�B
b�B
b�B
c B
cTB
cnB
cnB
cTB
c�B
c�B
dB
c�B
dB
dB
dB
d&B
d@B
dZB
d�B
dtB
dtB
d�B
d�B
d�B
e`B
e�B
e�B
ffB
ffB
ffB
fLB
f�B
g8B
gRB
gRB
g�B
h$B
h>B
hXB
hXB
h>B
h>B
h�B
hsB
h�B
iB
iDB
iyB
i�B
i�B
i�B
j0B
j0B
j�B
j�B
j�B
kB
kQB
kQB
kkB
k�B
k�B
lB
l"B
l=B
l=B
lqB
l�B
l�B
l�B
l�B
m)B
m]B
mCB
m�B
m�B
n/B
n/B
nIB
n}B
n}B
n}B
n}B
ncB
n�B
oOB
o5B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p!B
p�B
p�B
p�B
q'B
qAB
qAB
qvB
q�B
q�B
q�B
q�B
q�B
rB
rB
r�B
sB
s3B
sMB
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
uB
u?B
u?B
uZB
uZB
u�B
u�B
vB
v+B
vzB
v�B
v�B
v�B
w2B
wfB
wfB
wfB
w2B
w�B
xRB
x�B
x�B
x�B
y	B
y$B
yXB
y�B
y�B
y�B
zxB
z�B
z�B
z�B
{B
{0B
{dB
{�B
{�B
{�B
|6B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}VB
}VB
}�B
~wB
~]B
~wB
~�B
~�B
~�B
~�B
~�B
B
.B
B
.B
.B
.B
.B
.B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	_�B	_�B	_pB	_pB	_;B	_;B	_pB	_VB	_�B	_pB	_pB	_B	_pB	aHB	a�B	a|B	a�B	a|B	cnB	cB	c B	c�B	d@B	d�B	e,B	e,B	e`B	e,B	d�B	dZB	d�B	d�B	dZB	dZB	d@B	dB	c�B	cTB	b�B	b�B	a�B	`BB	g�B	dtB	�	B	��B
�B
n�B
��B
��B
��B
�:B
�B
�B
�wB
�AB
�[B
�qB
��B
�B
��B
�OB
��B
�eB
��B
��B
m)B
\]B
F%B
=�B
/�B
�B	��B	�GB	��B	�CB	�kB	��B	rB	l"B	h$B	d�B	_�B	V�B	OvB	;B	7B��B�;B�B�0B�B��B��B�B��B	oB�BB	jB	AUB	D�B	;B	1vB	B�B	IRB	O�B	abB	z�B	��B	��B	��B	��B	��B	�TB	��B	�`B	�B	��B	��B	�gB	�,B	�=B	��B	�hB	��B	�;B	�B	�tB	�B	�	B	��B	�xB	�JB	�B
 �B
%B
�B
�B
1B
	�B

�B
VB
B
	B
%B
 �B	��B	�B	�0B	��B	��B	�B	�	B	��B	��B	�IB	�0B	�B	�*B	�eB	�B	�B	��B	�B	�B
�B
~B
&B
�B
�B
YB
sB
�B
�B
�B
�B
�B
�B
�B

B
�B
�B
�B
}B
�B
B
	B
�B
3B
3B
�B
YB
�B
�B
�B
EB
�B
 4B
 B	��B
�B
�B
�B
�B
�B
gB
�B
	RB

�B

�B
�B
DB
	�B

XB
	B
B
gB
3B
�B	�B	�B	��B	��B	�B	�FB	��B	�qB	�QB	��B	�2B	�B	��B	�B	�ZB	��B	�B	�bB	�	B	�KB	�7B	�B	��B	��B	��B	�B	�B	� B	��B	�B	�B	��B	�B	�B	�B	�B	�qB	�B	�B	�B	�zB	�$B	�WB	�B	�`B	��B	��B	�ZB	��B	��B	�QB	�QB	ڠB	یB	��B	�_B	��B	�=B	��B	ٴB	�B	ּB	��B	�B	�B	�SB	�9B	�mB	�?B	�?B	��B	��B	ּB	�
B	ּB	��B	ؓB	��B	�	B	�B	��B	�=B	��B	�QB	�B	ٴB	�B	�eB	�eB	�eB	چB	چB	ۦB	�/B	��B	ߊB	�B	�fB	�2B	�B	�B	��B	�*B	��B	��B	��B	�KB	�B	�B	� B	�UB	��B	��B	��B	�!B	�!B	��B	�vB	�hB	�ZB	�B	�TB	�B	��B	��B	�B	�nB	��B	�B	��B	��B	�tB	��B	�$B	��B	�$B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�jB	��B	�B	�6B	��B	��B	��B	��B	�PB	�"B	��B	��B	�B	��B	��B	�B	�(B	�HB	�.B	�.B	�HB	�}B	�}B	�}B	�}B	�HB	�}B
 �B
 �B
 �B
UB
�B
�B
�B
�B
{B
-B
�B
�B
MB
�B
�B
�B
B
3B
gB
SB
9B
mB
mB
SB
9B
�B
SB
B
9B
9B
�B
�B
B
mB
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B

#B

=B

�B

�B
B
�B
�B
�B
VB
�B
BB
vB
�B
�B
�B
�B
�B
�B
NB
4B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
FB
�B
�B
�B
2B
B
9B
�B
�B
�B
�B
�B
�B
yB
yB
�B
�B
�B
�B
B
eB
�B
�B
]B
B
dB
~B
�B
~B
~B
B
�B
!B
;B
�B
pB
�B
�B
 \B
!|B
!�B
"B
"4B
"B
"B
!�B
# B
#�B
#�B
$ZB
$�B
$tB
$tB
$ZB
$�B
$�B
$�B
$�B
$�B
$�B
%FB
&�B
&�B
&LB
&2B
'�B
'�B
(sB
(�B
(�B
)_B
)_B
)yB
)_B
)_B
)B
)*B
+QB
*�B
*�B
+�B
,�B
,qB
-CB
-CB
-)B
-)B
-)B
-B
.cB
.cB
.B
-�B
.B
.�B
/�B
/�B
/�B
/�B
/�B
.�B
.IB
-wB
-CB
-)B
-CB
.cB
.�B
.�B
/ B
/5B
/�B
0B
/�B
0B
0;B
0�B
1'B
1�B
2B
2GB
2�B
3hB
3�B
3�B
3�B
4�B
5�B
6�B
6�B
6�B
7fB
88B
8�B
8�B
9rB
9�B
9�B
:B
:*B
:*B
:*B
:xB
:�B
:�B
;dB
;�B
<B
;�B
;dB
;B
;0B
;0B
;�B
=B
<�B
<�B
=<B
=B
<�B
<�B
<�B
<�B
<�B
<�B
=B
=B
=B
<�B
<�B
<�B
<�B
<jB
;�B
<B
<�B
<�B
>�B
@�B
@OB
@�B
AB
CB
C{B
B�B
A�B
?�B
>BB
=�B
=�B
>(B
>wB
>]B
>wB
>wB
>wB
>�B
>�B
>�B
>�B
>�B
?HB
>�B
?HB
@ B
@B
@�B
AB
@�B
A B
A�B
A�B
A�B
A�B
BAB
B'B
BB
BuB
B�B
C-B
CGB
C�B
D�B
D�B
D�B
D�B
E�B
E9B
EB
E9B
ESB
E�B
E�B
E�B
E�B
E�B
F?B
G+B
G_B
G_B
GEB
G_B
G�B
HB
HB
HKB
H1B
H�B
H�B
H�B
H�B
IB
H�B
H1B
HKB
H�B
KB
KDB
J�B
J�B
K)B
L0B
L�B
M�B
M�B
M�B
M�B
NpB
O�B
O�B
O�B
O\B
O�B
Q�B
RB
RTB
RTB
R�B
SB
S[B
S�B
S�B
S�B
T{B
TaB
T�B
T�B
T�B
UB
U�B
U�B
VmB
V�B
VSB
V�B
V�B
W$B
W�B
W�B
X+B
XEB
XEB
X�B
YB
X�B
Y�B
ZB
ZB
ZB
ZkB
[#B
[WB
[�B
\)B
\xB
\xB
\�B
]B
]IB
]�B
^�B
^�B
^�B
^�B
^�B
_B
_VB
_pB
_pB
_pB
`vB
`�B
a-B
abB
b4B
b�B
b�B
b�B
b�B
b�B
b�B
c B
cTB
cnB
cnB
cTB
c�B
c�B
dB
c�B
dB
dB
dB
d&B
d@B
dZB
d�B
dtB
dtB
d�B
d�B
d�B
e`B
e�B
e�B
ffB
ffB
ffB
fLB
f�B
g8B
gRB
gRB
g�B
h$B
h>B
hXB
hXB
h>B
h>B
h�B
hsB
h�B
iB
iDB
iyB
i�B
i�B
i�B
j0B
j0B
j�B
j�B
j�B
kB
kQB
kQB
kkB
k�B
k�B
lB
l"B
l=B
l=B
lqB
l�B
l�B
l�B
l�B
m)B
m]B
mCB
m�B
m�B
n/B
n/B
nIB
n}B
n}B
n}B
n}B
ncB
n�B
oOB
o5B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p!B
p�B
p�B
p�B
q'B
qAB
qAB
qvB
q�B
q�B
q�B
q�B
q�B
rB
rB
r�B
sB
s3B
sMB
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
uB
u?B
u?B
uZB
uZB
u�B
u�B
vB
v+B
vzB
v�B
v�B
v�B
w2B
wfB
wfB
wfB
w2B
w�B
xRB
x�B
x�B
x�B
y	B
y$B
yXB
y�B
y�B
y�B
zxB
z�B
z�B
z�B
{B
{0B
{dB
{�B
{�B
{�B
|6B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}VB
}VB
}�B
~wB
~]B
~wB
~�B
~�B
~�B
~�B
~�B
B
.B
B
.B
.B
.B
.B
.B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104932  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174337  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174338  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174338                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024345  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024345  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                