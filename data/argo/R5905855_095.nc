CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:27:28Z creation;2022-06-04T19:27:29Z conversion to V3.1      
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
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
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604192728  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               _A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ٔ74���1   @ٔ7�ʆB@-�9Xb�d"�x���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B���B�  B�  B�ffB���B�ffB���B���B�ffB�  B�  B���B�  B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C�CL�C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C3��C5�fC7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV33CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{�fD|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D���D�@ Dƀ D��3D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dك3D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݃3D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@s33@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�33A�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B�fgB���B���B�fgB���B���B�  B�fgB�  B�fgB�fgB�  Bי�Bۙ�B�fgB㙚B癚B뙚BB�fgB���B���B���C��C��C��C��C	��C�gC�C�3C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5�3C7�3C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS�gCV  CW�3CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC�ٙC�ٙC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D��Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLy�DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dw�3Dxs3Dx�3Dys3Dy�3Dzs3Dz�3D{y�D{�3D|s3D|�3D}s3D}�3D~s3D~�3Ds3D�3D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�<�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D��gD�9�D�y�DƼ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�|�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�|�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D๚D���D�9�D�y�DṚD���D�9�D�y�D⹚D���D�9�D�y�D㹚D���D�9�D�y�D乚D���D�9�D�y�D幚D���D�9�D�y�D湚D���D�9�D�y�D繚D���D�9�D�y�D蹚D���D�9�D�y�D鹚D���D�9�D�y�D깚D���D�9�D�y�D빚D���D�9�D�y�D칚D���D�9�D�y�D���D���D�9�D�y�DD���D�9�D�y�D﹚D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�&g1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A؜�A،JA�rGA�tTA�tTA�s�A�zxA�{A�t�A�s�A�r|A�s�A�s�A�t�A�u%A�v�A�r�A�qvA�r�A�sA�s�A�t�A�u%A�uZA�v+A�wfA�xlA�x�A�yrA�y�A�j�A�gmA�lWA�k�A�c�A�J�A��fA�%�A�5A�/OA���Aҁ�A�� A���A��TA�$@A�A��VA�i�A�g�Ađ�A��LA��A��[A�ΥA�1�A�t�A��ZA���A���A��A��A��IA�x�A��wA���A���A��$A�t�A��A��uA��A��0A���A�y	A��A�T�A�[�A�B�A�ƨA�'A��iA��#A�%FA��bA��aA��A�xA�($A�R�A}��Ay��As��Ao��An+Al�.Aja�AdںA`�A]�AXɆAV�}ATԕAP��AKoiAIE�AH�AF/�AA�)A@��A?�A>�KA=$A<�A:&A8tTA7y�A6�A5�A5VmA1��A1&�A.�A-MjA,H�A+�$A*�]A*��A*.IA)J�A(�\A'��A'9�A'�A&�A&��A&l"A&?A&
�A%��A%(�A%k�A%1'A%�A%A%,=A#�A#ĜA#��A#��A#�}A#�A#��A#��A#�VA#�4A#e,A#H�A#$�A"��A"��A"&�A!��A!��A!�IA!-A �fA�A�A�@AjA�AK�A�}A�A��Aw2A��A��A�5A$A�AA A�A�4A�A�Af�A�A�A��A�A��A�oA%FA2aA1�A�AK^A��A9XA!�A��AoiA�jAv�AA
�[A
b�A
-�A	�VA	J�A��A� A�AAE9A(A�5AK^AqvA�ZAcA'RA!�A�AA�YA�A�,A��A��AiDA<6AMA��A��A�FA҉A"�A�jA}VAK�A��A�SA\�AخAoiA�sA0�A �IA b@�rG@��_@�j@��@��@�x@�s�@���@�p;@���@��@���@���@�:�@�+k@�	�@���@�P�@��@���@�0U@�x�@��@�N�@��@�~@��s@�$@�&@��P@��@�Ĝ@�7@��@��H@�B�@���@��@�>B@���@�t�@��@�$@�q@��@�ԕ@镁@�Y@�]d@���@���@�^�@���@�F@��@���@⻙@��@�r@�u%@�~@��6@�>�@୬@�@�c�@�.I@�kQ@ݓ@�'�@܈�@گO@�x@��'@׃{@�N<@�U�@�u�@ךk@׌~@�?�@�u�@Թ�@��A@��@Ѱ�@��H@��@�PH@���@�S�@�!�@�_@��@ˎ�@�?}@��B@�H@��@ɮ@ɗ$@�zx@Ȣ4@�]d@�1'@�e@���@��@Ɯx@ƋD@�A�@���@�ԕ@��@�n�@��z@ô�@�v`@�B�@��?@�h�@���@�^�@��@��v@���@�_�@���@��@���@�H@�Z@�bN@��V@���@��E@���@�#:@��N@�=@���@��@��@�.I@��"@�{�@�<�@���@��[@�6z@�V@�-@���@��X@�c�@� \@��p@�V@��Z@��q@��,@�B[@�s@��<@�B[@��@��E@�{�@�R�@�@���@���@�q@��K@��u@�.�@��*@�T�@���@���@�b@���@�IR@�ں@�PH@��@�ϫ@���@�C�@�#�@��@��P@��E@���@�?@�e@�~@��o@���@��	@�p�@�O�@�5�@�)_@��@��`@��b@�4n@��]@���@�|@�S&@�)_@��@��@�H�@�%�@��@��A@���@�Y�@�F@���@��@�h�@�1@���@���@�[W@���@��@�W�@��@���@�s@��@��1@�_�@�<�@��@��9@��k@�t�@�
=@���@�z�@�j@�H�@���@�dZ@�;d@��M@�V@��@��{@�Y@���@��A@�Xy@�+k@��^@��{@�1�@�ں@���@��F@�tT@�5?@���@���@�L�@�;@��)@��e@�,=@��@���@���@�x�@�[W@�J�@��@���@�~�@�S�@�6@��@��F@�K�@�S@��@���@��b@�$@�خ@���@�S�@�-w@��@��@���@�{�@���@��^@�zx@�X@��@���@�Ta@�
�@���@���@��@���@�n/@�C�@�@@��@���@�6@��>@���@���@���@�c@�u�@��5@���@��L@��@�� @���@��@���@���@�^5@��@� �@��w@�A�@��@�V@��r@��N@��*@���@�t�@�RT@��@��2@���@���@�bN@�.�@��@=@~ں@~�R@}�9@}Dg@}�@|�P@|�	@|�@|�p@|�I@|Q�@|/�@|�@{�@{�4@z�L@z@�@y�#@y \@x�O@x6@w��@w��@we�@v��@v�@v_�@u��@u&�@t�f@t�?@t7@s�q@s�k@sU�@sY@r�2@r�X@r�x@rv�@r?@q��@qY�@q(�@p�@p�D@pM@o��@o�:@o�{@oZ�@o�@o@n�@n��@n��@nu@m`B@l�@l|�@kt�@j�R@j?@je@i��@i��@i�@h��@g��@gZ�@f��@fL0@e�'@eVm@e�@d��@cخ@cx@c@O@c.I@b��@b+k@a�@ac@a�@`��@`I�@_�Q@_b�@_!-@^��@^l�@^4@]�@]!�@\�_@\q@\:�@\�@[�@[�	@[J#@[ i@Zߤ@Z_�@Y��@YY�@X�@X��@X_@W�@W�K@W@O@V�,@V�F@V3�@U��@U�S@Uw2@U�@TtT@T�@S�
@SiD@SMj@SC@R��@R_@Q��@P�@P$@O�K@O1�@N�8@N��@N.�@M��@M=�@M#�@M�@L�@LS�@K�]@K�@@KK�@K i@J��@Jh
@I�@I�@I��@I`B@H�@HI�@G�@Gy�@Fߤ@Fs�@F;�@E��@E��@ErG@D֡@DS�@D�@D  @C��@C�@C�}@CH�@C�@B��@B�<@B~�@BR�@B$�@B�@Be@B_@Aϫ@A��@Ae,@AQ�@A7L@@�`@@N�@?�&@?@O@?�@>��@>\�@=��@=��@=�"@=c�@=T�@=-w@<��@<Ĝ@<�9@<u�@<b@;˒@;E9@;(@:��@:�@:{�@9��@9^�@8�5@8�e@8oi@8@7o�@7,�@6��@6ں@6��@6d�@5��@5�S@5@5�@4�@4�u@4!@3خ@3��@3��@3RT@3)_@2�!@2�@1�@1u�@1L�@1%F@1#�@0��@0��@0Q�@0/�@0@/خ@/iD@/.I@.��@.�r@.+k@-ϫ@-�@-j@-=�@-V@,��@,��@,tT@,Xy@,b@+�@+�@+� @+�[@+\)@*�@*�'@*��@*�@*R�@*�@)��@)X@)IR@)<6@(�|@(��@(�_@(~(@(h�@(M@( �@'�m@'�6@'��@'o�@'J#@'.I@'�@&�x@&Ov@&@�@&8�@&O@%�@%�-@%��@%T�@%�@$��@$�j@$��@$c�@$"h@$~@$�@$�@$@$�@$1@#�]@#��@#��@#�}@#�w@#��@#Z�@#(@"��@"�2@"�@"�X@"�@"��@!��@!N<@!�@ ��@ �@ �z@ �@ oi@ 1@��@�@��@��@z@��@�t@�"@G�@@��@�9@l"@Z@/�@��@� @��@�$@X�@+@�@�@ߤ@�,@��@1�@�@�'@^�@A @-w@�@�I@h�@%�@ݘ@�:@t�@t�@X�@�,@\�@�.@��@�@�@�@N<@:�@5�@q@��@��@oi@V�@*�@�@��@s@�@��@��@��@�b@� @c @0U@�d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A؜�A،JA�rGA�tTA�tTA�s�A�zxA�{A�t�A�s�A�r|A�s�A�s�A�t�A�u%A�v�A�r�A�qvA�r�A�sA�s�A�t�A�u%A�uZA�v+A�wfA�xlA�x�A�yrA�y�A�j�A�gmA�lWA�k�A�c�A�J�A��fA�%�A�5A�/OA���Aҁ�A�� A���A��TA�$@A�A��VA�i�A�g�Ađ�A��LA��A��[A�ΥA�1�A�t�A��ZA���A���A��A��A��IA�x�A��wA���A���A��$A�t�A��A��uA��A��0A���A�y	A��A�T�A�[�A�B�A�ƨA�'A��iA��#A�%FA��bA��aA��A�xA�($A�R�A}��Ay��As��Ao��An+Al�.Aja�AdںA`�A]�AXɆAV�}ATԕAP��AKoiAIE�AH�AF/�AA�)A@��A?�A>�KA=$A<�A:&A8tTA7y�A6�A5�A5VmA1��A1&�A.�A-MjA,H�A+�$A*�]A*��A*.IA)J�A(�\A'��A'9�A'�A&�A&��A&l"A&?A&
�A%��A%(�A%k�A%1'A%�A%A%,=A#�A#ĜA#��A#��A#�}A#�A#��A#��A#�VA#�4A#e,A#H�A#$�A"��A"��A"&�A!��A!��A!�IA!-A �fA�A�A�@AjA�AK�A�}A�A��Aw2A��A��A�5A$A�AA A�A�4A�A�Af�A�A�A��A�A��A�oA%FA2aA1�A�AK^A��A9XA!�A��AoiA�jAv�AA
�[A
b�A
-�A	�VA	J�A��A� A�AAE9A(A�5AK^AqvA�ZAcA'RA!�A�AA�YA�A�,A��A��AiDA<6AMA��A��A�FA҉A"�A�jA}VAK�A��A�SA\�AخAoiA�sA0�A �IA b@�rG@��_@�j@��@��@�x@�s�@���@�p;@���@��@���@���@�:�@�+k@�	�@���@�P�@��@���@�0U@�x�@��@�N�@��@�~@��s@�$@�&@��P@��@�Ĝ@�7@��@��H@�B�@���@��@�>B@���@�t�@��@�$@�q@��@�ԕ@镁@�Y@�]d@���@���@�^�@���@�F@��@���@⻙@��@�r@�u%@�~@��6@�>�@୬@�@�c�@�.I@�kQ@ݓ@�'�@܈�@گO@�x@��'@׃{@�N<@�U�@�u�@ךk@׌~@�?�@�u�@Թ�@��A@��@Ѱ�@��H@��@�PH@���@�S�@�!�@�_@��@ˎ�@�?}@��B@�H@��@ɮ@ɗ$@�zx@Ȣ4@�]d@�1'@�e@���@��@Ɯx@ƋD@�A�@���@�ԕ@��@�n�@��z@ô�@�v`@�B�@��?@�h�@���@�^�@��@��v@���@�_�@���@��@���@�H@�Z@�bN@��V@���@��E@���@�#:@��N@�=@���@��@��@�.I@��"@�{�@�<�@���@��[@�6z@�V@�-@���@��X@�c�@� \@��p@�V@��Z@��q@��,@�B[@�s@��<@�B[@��@��E@�{�@�R�@�@���@���@�q@��K@��u@�.�@��*@�T�@���@���@�b@���@�IR@�ں@�PH@��@�ϫ@���@�C�@�#�@��@��P@��E@���@�?@�e@�~@��o@���@��	@�p�@�O�@�5�@�)_@��@��`@��b@�4n@��]@���@�|@�S&@�)_@��@��@�H�@�%�@��@��A@���@�Y�@�F@���@��@�h�@�1@���@���@�[W@���@��@�W�@��@���@�s@��@��1@�_�@�<�@��@��9@��k@�t�@�
=@���@�z�@�j@�H�@���@�dZ@�;d@��M@�V@��@��{@�Y@���@��A@�Xy@�+k@��^@��{@�1�@�ں@���@��F@�tT@�5?@���@���@�L�@�;@��)@��e@�,=@��@���@���@�x�@�[W@�J�@��@���@�~�@�S�@�6@��@��F@�K�@�S@��@���@��b@�$@�خ@���@�S�@�-w@��@��@���@�{�@���@��^@�zx@�X@��@���@�Ta@�
�@���@���@��@���@�n/@�C�@�@@��@���@�6@��>@���@���@���@�c@�u�@��5@���@��L@��@�� @���@��@���@���@�^5@��@� �@��w@�A�@��@�V@��r@��N@��*@���@�t�@�RT@��@��2@���@���@�bN@�.�@��@=@~ں@~�R@}�9@}Dg@}�@|�P@|�	@|�@|�p@|�I@|Q�@|/�@|�@{�@{�4@z�L@z@�@y�#@y \@x�O@x6@w��@w��@we�@v��@v�@v_�@u��@u&�@t�f@t�?@t7@s�q@s�k@sU�@sY@r�2@r�X@r�x@rv�@r?@q��@qY�@q(�@p�@p�D@pM@o��@o�:@o�{@oZ�@o�@o@n�@n��@n��@nu@m`B@l�@l|�@kt�@j�R@j?@je@i��@i��@i�@h��@g��@gZ�@f��@fL0@e�'@eVm@e�@d��@cخ@cx@c@O@c.I@b��@b+k@a�@ac@a�@`��@`I�@_�Q@_b�@_!-@^��@^l�@^4@]�@]!�@\�_@\q@\:�@\�@[�@[�	@[J#@[ i@Zߤ@Z_�@Y��@YY�@X�@X��@X_@W�@W�K@W@O@V�,@V�F@V3�@U��@U�S@Uw2@U�@TtT@T�@S�
@SiD@SMj@SC@R��@R_@Q��@P�@P$@O�K@O1�@N�8@N��@N.�@M��@M=�@M#�@M�@L�@LS�@K�]@K�@@KK�@K i@J��@Jh
@I�@I�@I��@I`B@H�@HI�@G�@Gy�@Fߤ@Fs�@F;�@E��@E��@ErG@D֡@DS�@D�@D  @C��@C�@C�}@CH�@C�@B��@B�<@B~�@BR�@B$�@B�@Be@B_@Aϫ@A��@Ae,@AQ�@A7L@@�`@@N�@?�&@?@O@?�@>��@>\�@=��@=��@=�"@=c�@=T�@=-w@<��@<Ĝ@<�9@<u�@<b@;˒@;E9@;(@:��@:�@:{�@9��@9^�@8�5@8�e@8oi@8@7o�@7,�@6��@6ں@6��@6d�@5��@5�S@5@5�@4�@4�u@4!@3خ@3��@3��@3RT@3)_@2�!@2�@1�@1u�@1L�@1%F@1#�@0��@0��@0Q�@0/�@0@/خ@/iD@/.I@.��@.�r@.+k@-ϫ@-�@-j@-=�@-V@,��@,��@,tT@,Xy@,b@+�@+�@+� @+�[@+\)@*�@*�'@*��@*�@*R�@*�@)��@)X@)IR@)<6@(�|@(��@(�_@(~(@(h�@(M@( �@'�m@'�6@'��@'o�@'J#@'.I@'�@&�x@&Ov@&@�@&8�@&O@%�@%�-@%��@%T�@%�@$��@$�j@$��@$c�@$"h@$~@$�@$�@$@$�@$1@#�]@#��@#��@#�}@#�w@#��@#Z�@#(@"��@"�2@"�@"�X@"�@"��@!��@!N<@!�@ ��@ �@ �z@ �@ oi@ 1@��@�@��@��@z@��@�t@�"@G�@@��@�9@l"@Z@/�@��@� @��@�$@X�@+@�@�@ߤ@�,@��@1�@�@�'@^�@A @-w@�@�I@h�@%�@ݘ@�:@t�@t�@X�@�,@\�@�.@��@�@�@�@N<@:�@5�@q@��@��@oi@V�@*�@�@��@s@�@��@��@��@�b@� @c @0U@�d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
��B
�1B
�KB
ٚB
ٴB
��B
�B
��B
ٴB
ٴB
ٴB
��B
ٚB
ٴB
�B
�B
�B
�B
�B
ٚB
�B
ٚB
�B
ٚB
ٚB
ٚB
ٚB
�B
�KB
�B
�B
�KB
�KB
�1B
��B
רB
�gB
�.B
�KB
�4B
��B
�IB
��B
��B
��B
�B
�KB
�B
�B�B_�B�)B��B��B��B��B��B�_B�=B�'B��B��BB��B�AB��B�&BĶB��B��B�B��B��B�!B�B}"Bi�BK�B,WB�B
�BB
�AB
�B
��B
�B
�B
�	B
f�B
&�B	�B	ڠB	��B	�B	�B	��B	�?B	i_B	M6B	>BB	1'B	(�B	!B	 vB	#�B	 �B	�B	�B	)�B	3�B	;dB	A�B	C�B	D�B	N�B	T�B	^�B	f2B	u%B	rGB	o�B	m�B	h�B	a�B	v�B	��B	��B	�CB	��B	��B	�[B	��B	�cB	��B	�B	��B	ŢB	��B	�B	ʦB	�1B	�NB	�AB	�'B	��B
B
�B
�B
]B
#nB
&�B
)*B
,�B
/�B
4�B
72B
8�B
9�B
:�B
<jB
=<B
>�B
?�B
@4B
@�B
A�B
GB
H�B
E�B
C�B
EmB
D3B
B[B
B[B
>]B
<jB
:�B
8B
4B
0B
0�B
2�B
1�B
2GB
3MB
2|B
*�B
B
qB
�B
�B
�B
�B
B
B
%B
�B
	�B
�B
mB
�B
�B
{B
�B
PB
JB
�B
jB
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
OB
B
IB
�B
�B
IB
�B
IB
�B
�B
�B
OB
�B
B
!B
�B
 vB
$@B
'�B
0�B
0oB
/OB
0�B
2�B
1�B
1[B
/B
-CB
+B
(�B
&�B
%�B
%�B
%,B
$ZB
$ZB
$tB
$ZB
$B
#:B
"�B
"B
!�B
!bB
!B
 �B
!�B
#:B
#B
"4B
"�B
"hB
!�B
!�B
!�B
 �B
 BB
�B
OB
�B
�B
B
�B
qB
kB
B
�B
[B
�B
�B
�B
�B
�B
�B
�B
&B
�B
�B
B
�B
4B
�B
�B
�B
"B
B
�B
jB
�B
�B
�B
�B
<B
(B
�B
�B
6B
0B
�B
�B
�B
�B
�B

#B
�B
B
�B
�B
�B
	�B
�B
JB
�B
�B
~B
�B
^B

=B
�B
mB
�B
�B
�B
-B
GB
B
�B
�B
�B
�B
9B
B
�B
gB
�B
GB
�B
�B
�B
aB
�B
�B
�B
{B
-B
aB
aB
B
GB
�B
gB
�B
�B
�B
{B
�B
�B
-B
-B
{B
B
 �B
 B
�B
B
B
�B
�B
�B
�B
9B
�B
gB
�B
+B
�B
_B
_B
zB
_B
KB
�B
1B
1B
�B
�B
�B
�B
B
�B
	�B

	B
	�B
	�B

�B

�B

�B
�B
~B
�B
�B
6B
6B
6B
B
�B
�B
�B
�B
�B
B
VB
�B
�B
�B
PB
dB
�B
6B
�B
�B
B
"B
pB
�B
�B
bB
�B
�B
HB
HB
HB
HB
bB
bB
�B
 B
hB
hB
�B
 B
oB
oB
�B
�B
�B
�B
,B
aB
B
B
�B
�B
�B
B
9B
mB
�B
�B
�B
�B
sB
YB
?B
YB
�B
�B
�B
sB
�B
?B
�B
+B
�B
sB
�B
_B
�B
�B
#B
qB
�B
�B
#B
�B
KB
B
kB
�B
	B
=B
=B
B
CB
�B
/B
IB
dB
~B
�B
B
5B
�B
VB
�B
pB
 B
 B
 �B
 �B
 �B
 �B
 �B
!|B
!�B
!�B
"4B
"4B
"NB
"�B
#:B
#TB
#nB
#�B
#�B
$ZB
$�B
%B
%,B
%B
$�B
%,B
%B
%�B
&LB
&fB
&�B
&�B
'B
'�B
'�B
($B
(sB
(XB
(sB
(�B
(�B
(�B
)B
(�B
)yB
)�B
*eB
*B
*�B
*�B
*�B
*B
+�B
+�B
+�B
,WB
,�B
-)B
-�B
-�B
-�B
.�B
.�B
.�B
/B
/�B
0B
0�B
0�B
1B
1�B
2|B
2|B
2�B
2�B
3MB
3�B
3�B
4B
4B
4B
4�B
4�B
4�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
6B
6FB
6`B
6zB
6�B
6zB
72B
7B
7fB
7�B
7�B
7�B
8B
8�B
8�B
8�B
8�B
8�B
9>B
9XB
9XB
9rB
:B
:*B
:^B
:xB
:�B
:�B
:�B
:�B
:�B
:�B
;dB
;B
;�B
;�B
;�B
<B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
="B
=qB
=�B
=�B
>(B
>�B
?B
?cB
?cB
?}B
?�B
@ B
?�B
@�B
@�B
@�B
A;B
A�B
AoB
A�B
B'B
BuB
B�B
B�B
B�B
C�B
C�B
C�B
DB
D�B
D�B
EB
E�B
FB
F%B
F�B
F�B
F�B
GEB
G_B
G�B
G�B
G�B
G�B
G�B
G�B
HB
H�B
H�B
H�B
H�B
IB
IRB
IlB
I�B
J	B
I�B
JXB
J�B
J�B
J�B
KDB
K^B
K^B
K�B
K�B
LJB
LdB
L�B
L�B
M6B
M�B
N<B
NVB
N�B
N�B
N�B
OB
N�B
N�B
O(B
O�B
PHB
PHB
PHB
PbB
Q B
Q4B
Q�B
QhB
Q�B
RB
R�B
R�B
RoB
RoB
R�B
S[B
R�B
R�B
S�B
TB
T,B
TaB
T�B
TFB
T,B
T�B
UgB
VB
VB
VB
VB
VB
V�B
V�B
V�B
W$B
W?B
WsB
W�B
WsB
W�B
W�B
W�B
W�B
XB
XB
XB
X_B
X�B
X�B
YB
YB
Y�B
ZB
Z7B
ZQB
Z�B
Z�B
Z�B
Z�B
[	B
[#B
[	B
[	B
[	B
[#B
[�B
\B
\B
\B
\CB
\CB
\�B
]IB
]~B
]�B
]/B
\�B
]IB
]~B
]�B
]�B
]�B
^OB
^�B
_;B
_VB
_VB
_�B
`�B
`�B
`�B
`�B
`�B
`�B
abB
a|B
a�B
a|B
a|B
bB
bhB
b�B
b�B
c B
c B
c:B
cnB
c�B
c�B
dB
dZB
d�B
d�B
e,B
eFB
e`B
ezB
e�B
e�B
e�B
e�B
fLB
ffB
ffB
ffB
ffB
f�B
gB
gRB
g8B
gRB
g�B
g�B
h$B
hXB
hXB
hXB
h�B
h�B
h�B
h�B
h�B
iB
i*B
i_B
i_B
i�B
i�B
i�B
i�B
i�B
jKB
jeB
jeB
jeB
jB
j�B
j�B
j�B
kB
kQB
kkB
k�B
k�B
k�B
k�B
k�B
k�B
lB
lB
lB
lB
lB
lB
l"B
l"B
l"B
l"B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
mB
m)B
mwB
m�B
m�B
m�B
m�B
m�B
nB
ncB
n�B
n�B
n�B
n�B
oOB
oiB
o�B
p!B
pB
pB
pUB
p�B
p�B
p�B
p�B
p�B
p�B
q'B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
r-B
raB
r|B
r�B
r�B
r�B
sMB
shB
s�B
s�B
s�B
tB
tB
t9B
t9B
uB
utB
u�B
u�B
u�B
u�B
u�B
vzB
vzB
vzB
v�B
v�B
v�B
v�B
wB
w2B
wLB
w�B
w�B
xB
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
��B
�1B
�KB
ٚB
ٴB
��B
�B
��B
ٴB
ٴB
ٴB
��B
ٚB
ٴB
�B
�B
�B
�B
�B
ٚB
�B
ٚB
�B
ٚB
ٚB
ٚB
ٚB
�B
�KB
�B
�B
�KB
�KB
�1B
��B
רB
�gB
�.B
�KB
�4B
��B
�IB
��B
��B
��B
�B
�KB
�B
�B�B_�B�)B��B��B��B��B��B�_B�=B�'B��B��BB��B�AB��B�&BĶB��B��B�B��B��B�!B�B}"Bi�BK�B,WB�B
�BB
�AB
�B
��B
�B
�B
�	B
f�B
&�B	�B	ڠB	��B	�B	�B	��B	�?B	i_B	M6B	>BB	1'B	(�B	!B	 vB	#�B	 �B	�B	�B	)�B	3�B	;dB	A�B	C�B	D�B	N�B	T�B	^�B	f2B	u%B	rGB	o�B	m�B	h�B	a�B	v�B	��B	��B	�CB	��B	��B	�[B	��B	�cB	��B	�B	��B	ŢB	��B	�B	ʦB	�1B	�NB	�AB	�'B	��B
B
�B
�B
]B
#nB
&�B
)*B
,�B
/�B
4�B
72B
8�B
9�B
:�B
<jB
=<B
>�B
?�B
@4B
@�B
A�B
GB
H�B
E�B
C�B
EmB
D3B
B[B
B[B
>]B
<jB
:�B
8B
4B
0B
0�B
2�B
1�B
2GB
3MB
2|B
*�B
B
qB
�B
�B
�B
�B
B
B
%B
�B
	�B
�B
mB
�B
�B
{B
�B
PB
JB
�B
jB
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
OB
B
IB
�B
�B
IB
�B
IB
�B
�B
�B
OB
�B
B
!B
�B
 vB
$@B
'�B
0�B
0oB
/OB
0�B
2�B
1�B
1[B
/B
-CB
+B
(�B
&�B
%�B
%�B
%,B
$ZB
$ZB
$tB
$ZB
$B
#:B
"�B
"B
!�B
!bB
!B
 �B
!�B
#:B
#B
"4B
"�B
"hB
!�B
!�B
!�B
 �B
 BB
�B
OB
�B
�B
B
�B
qB
kB
B
�B
[B
�B
�B
�B
�B
�B
�B
�B
&B
�B
�B
B
�B
4B
�B
�B
�B
"B
B
�B
jB
�B
�B
�B
�B
<B
(B
�B
�B
6B
0B
�B
�B
�B
�B
�B

#B
�B
B
�B
�B
�B
	�B
�B
JB
�B
�B
~B
�B
^B

=B
�B
mB
�B
�B
�B
-B
GB
B
�B
�B
�B
�B
9B
B
�B
gB
�B
GB
�B
�B
�B
aB
�B
�B
�B
{B
-B
aB
aB
B
GB
�B
gB
�B
�B
�B
{B
�B
�B
-B
-B
{B
B
 �B
 B
�B
B
B
�B
�B
�B
�B
9B
�B
gB
�B
+B
�B
_B
_B
zB
_B
KB
�B
1B
1B
�B
�B
�B
�B
B
�B
	�B

	B
	�B
	�B

�B

�B

�B
�B
~B
�B
�B
6B
6B
6B
B
�B
�B
�B
�B
�B
B
VB
�B
�B
�B
PB
dB
�B
6B
�B
�B
B
"B
pB
�B
�B
bB
�B
�B
HB
HB
HB
HB
bB
bB
�B
 B
hB
hB
�B
 B
oB
oB
�B
�B
�B
�B
,B
aB
B
B
�B
�B
�B
B
9B
mB
�B
�B
�B
�B
sB
YB
?B
YB
�B
�B
�B
sB
�B
?B
�B
+B
�B
sB
�B
_B
�B
�B
#B
qB
�B
�B
#B
�B
KB
B
kB
�B
	B
=B
=B
B
CB
�B
/B
IB
dB
~B
�B
B
5B
�B
VB
�B
pB
 B
 B
 �B
 �B
 �B
 �B
 �B
!|B
!�B
!�B
"4B
"4B
"NB
"�B
#:B
#TB
#nB
#�B
#�B
$ZB
$�B
%B
%,B
%B
$�B
%,B
%B
%�B
&LB
&fB
&�B
&�B
'B
'�B
'�B
($B
(sB
(XB
(sB
(�B
(�B
(�B
)B
(�B
)yB
)�B
*eB
*B
*�B
*�B
*�B
*B
+�B
+�B
+�B
,WB
,�B
-)B
-�B
-�B
-�B
.�B
.�B
.�B
/B
/�B
0B
0�B
0�B
1B
1�B
2|B
2|B
2�B
2�B
3MB
3�B
3�B
4B
4B
4B
4�B
4�B
4�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
6B
6FB
6`B
6zB
6�B
6zB
72B
7B
7fB
7�B
7�B
7�B
8B
8�B
8�B
8�B
8�B
8�B
9>B
9XB
9XB
9rB
:B
:*B
:^B
:xB
:�B
:�B
:�B
:�B
:�B
:�B
;dB
;B
;�B
;�B
;�B
<B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
="B
=qB
=�B
=�B
>(B
>�B
?B
?cB
?cB
?}B
?�B
@ B
?�B
@�B
@�B
@�B
A;B
A�B
AoB
A�B
B'B
BuB
B�B
B�B
B�B
C�B
C�B
C�B
DB
D�B
D�B
EB
E�B
FB
F%B
F�B
F�B
F�B
GEB
G_B
G�B
G�B
G�B
G�B
G�B
G�B
HB
H�B
H�B
H�B
H�B
IB
IRB
IlB
I�B
J	B
I�B
JXB
J�B
J�B
J�B
KDB
K^B
K^B
K�B
K�B
LJB
LdB
L�B
L�B
M6B
M�B
N<B
NVB
N�B
N�B
N�B
OB
N�B
N�B
O(B
O�B
PHB
PHB
PHB
PbB
Q B
Q4B
Q�B
QhB
Q�B
RB
R�B
R�B
RoB
RoB
R�B
S[B
R�B
R�B
S�B
TB
T,B
TaB
T�B
TFB
T,B
T�B
UgB
VB
VB
VB
VB
VB
V�B
V�B
V�B
W$B
W?B
WsB
W�B
WsB
W�B
W�B
W�B
W�B
XB
XB
XB
X_B
X�B
X�B
YB
YB
Y�B
ZB
Z7B
ZQB
Z�B
Z�B
Z�B
Z�B
[	B
[#B
[	B
[	B
[	B
[#B
[�B
\B
\B
\B
\CB
\CB
\�B
]IB
]~B
]�B
]/B
\�B
]IB
]~B
]�B
]�B
]�B
^OB
^�B
_;B
_VB
_VB
_�B
`�B
`�B
`�B
`�B
`�B
`�B
abB
a|B
a�B
a|B
a|B
bB
bhB
b�B
b�B
c B
c B
c:B
cnB
c�B
c�B
dB
dZB
d�B
d�B
e,B
eFB
e`B
ezB
e�B
e�B
e�B
e�B
fLB
ffB
ffB
ffB
ffB
f�B
gB
gRB
g8B
gRB
g�B
g�B
h$B
hXB
hXB
hXB
h�B
h�B
h�B
h�B
h�B
iB
i*B
i_B
i_B
i�B
i�B
i�B
i�B
i�B
jKB
jeB
jeB
jeB
jB
j�B
j�B
j�B
kB
kQB
kkB
k�B
k�B
k�B
k�B
k�B
k�B
lB
lB
lB
lB
lB
lB
l"B
l"B
l"B
l"B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
mB
m)B
mwB
m�B
m�B
m�B
m�B
m�B
nB
ncB
n�B
n�B
n�B
n�B
oOB
oiB
o�B
p!B
pB
pB
pUB
p�B
p�B
p�B
p�B
p�B
p�B
q'B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
r-B
raB
r|B
r�B
r�B
r�B
sMB
shB
s�B
s�B
s�B
tB
tB
t9B
t9B
uB
utB
u�B
u�B
u�B
u�B
u�B
vzB
vzB
vzB
v�B
v�B
v�B
v�B
wB
w2B
wLB
w�B
w�B
xB
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105248  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192728  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192729  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192729                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042737  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042737  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                