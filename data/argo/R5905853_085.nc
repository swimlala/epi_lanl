CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:38:01Z creation;2022-06-04T17:38:01Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604173801  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               UA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�y�+��1   @�y�eC!@.�;dZ��c�Z�11   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B���B�  B�  B�33B�ffB�ffB���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�fC	�fC�fC  C  C  C  C  C  C  C  CL�C�fC!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH33CI��CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv33Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|�fD}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�<�D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@s33@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW��B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B�fgB���B���B���B�fgBǙ�B˙�B���B�  B�  B�fgBߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C�3C	�3C�3C��C��C��C��C��C��C��C��C�C�3C!�3C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7�gC9��C;��C=��C?��CA��CC��CE��CH  CIfgCK�3CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cv  Cw�3Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٙC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��3C��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLy�DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dw�3Dxs3Dx�3Dys3Dy�3Dzs3Dz�3D{s3D{�3D|y�D|�3D}s3D}�3D~s3D~�3Ds3D�3D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�6gD�y�D๚D���D�9�D�y�DṚD���D�9�D�y�D⹚D���D�9�D�y�D㹚D���D�9�D�y�D乚D���D�9�D�y�D幚D���D�9�D�y�D湚D���D�9�D�y�D繚D���D�9�D�y�D蹚D���D�9�D�y�D鹚D���D�9�D�y�D깚D���D�9�D�y�D빚D���D�9�D�y�D칚D���D�9�D�y�D���D���D�9�D�y�DD���D�9�D�y�D﹚D���D�9�D�vgD�D���D�9�D�y�D�D���D�9�D�y�D�gD���D�9�D�y�D�D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�fg1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A̟!A̜�A̙�A̟�ḀFA̢hA̞�A̟�A̟VA̞OA̜A̙eA̘�A̚kA̛qA̜A̞�A̡�ḀFA̦LA̧A̧RḀ�A̡�ḀzẠ�A̜�A̞�A̍A�~�A�^A���A���A˱�A�_�A��A��mAʧ�A�s�A�E�Aɶ�A�T,A�[WAƘ+A�$�A��OAŕ�A�^�A�AĹ�A�I�A�4�A��PA�S[A��A�X�A���A�@�A�N�A���A���A��A�^�A��A��~A�MA���A�PA��	A��lA���A�S�A�d�A�FtA�\�A�ҽA�OBA�OvA�t�A���A�v+A�A��sA�L�A�b�A���A�H�A�P}A��FA���A�w2A�FA�MA�m]A�u�A��A�\�A�K�A��A�R�A��iA�QAw�At��Ar�An��Ai1�AfAep;Ae�Aa�~A^FA\7�AW��AT7�AP��AM��AL��AJ�+AF�PABB[A@��A@}�A@Q�A>A;PHA9�4A98�A8ȴA8m]A7��A6A�A5�PA5�A4�qA4IRA3ںA2��A1N�A0��A/�&A-�/A+��A*($A(iDA'�A'�AA'�A&�A%R�A&�A's�A&l�A&3�A$��A$�A!TaA \�A�KA��A /A�PA��A?A~AA\�A�A�A��A~(A�\A�RA��A�A��A�
A�^A|�A�ZArGA�A�/A4�A�A�Aq�A!�A�EA?�AK^A��AJAy>A�A�HAE9A��A��Ah
A$tA�A��A�A?}A��A�A��A8�A��A8�A8�A1�A��A}VA0UA��A3�A
��A
?�A
+A	�;A	 iAA AA�EA}VA6�A��A��A� A�ArGA4A��A��AZ�A ��A ��A ��A ��A y>A ]dA J�A 4@�W?@���@�m]@��1@��{@�n�@�dZ@�"h@�PH@��@��D@�O@�/�@�?}@�@�f�@��c@ꍹ@��@�iD@�}@�
�@��P@��@�zx@��@��@�C@�R@��@�hs@��f@��@�&@��c@�i�@�B�@�l�@݁�@�A @�ѷ@��@�)_@ڋD@��@�k�@ظR@��+@�X�@��@��Z@�RT@��@�"h@�/@�{�@��;@ѥ�@ї�@���@�c�@ϧ�@Ϊe@��Z@͓�@�t�@��@�_@�Z�@���@ʳh@��@�x�@�;d@��@��@�xl@�D�@�5?@� �@�@ǳ�@�]�@�/�@���@���@Ɨ�@�9X@��#@�H�@���@��@Ğ@��@�w2@��|@§@.@�I�@�x�@��@���@�Ov@��@��[@�x@��@��Y@��m@�e�@�N<@�4�@�*0@�
=@���@�7�@�$@��@�	�@��@��Q@���@���@�=�@�!�@��@��
@���@�oi@���@�rG@��@��F@��@���@��+@�h
@�˒@��@�s@�A�@��@�ȴ@�Xy@��+@���@�x@�+@�oi@�/�@�˒@�)_@��+@�Q�@��w@�Z@�qv@��@�_@��n@�RT@��4@�@��h@�V@��@�$�@��@�y�@��Y@�Ft@�*�@� �@��@���@��@��z@�n�@�N�@��@���@��@���@�PH@�@��6@��$@�Q�@���@���@��I@��@��]@���@���@�c@�O@���@��@��@�\�@���@�xl@��@�IR@�=�@���@�l�@���@�hs@�E9@��H@���@�%�@���@�x�@��@��h@�q@�6�@��T@��T@��@��9@��P@�8�@��@�@��	@���@���@�$@�x@�"�@��o@��@��)@�ƨ@��m@�ݘ@���@�4@��[@��O@�S�@��@��T@���@�1�@�Ĝ@��@�@�j�@�.I@���@�YK@��d@���@�U�@�+@��I@�j@�M@��&@��t@���@���@�{J@�G�@�%@��\@�J@��}@��~@�Z�@��K@��9@�u%@�*�@�
�@��;@���@���@�Y@��,@���@��A@�c @��@��@�o�@�!-@�Ɇ@���@�� @�R�@�J@���@��w@���@�x�@�e,@�4�@��@��8@���@��K@��B@���@�oi@�Q@��+@���@�)_@��}@�Q�@�!�@�@��@"�@~͟@~�R@~~�@}�@}p�@}&�@|�@|ѷ@|�D@|(�@{�g@{@z��@z=q@y�@y��@y}�@y+@x`�@x�@wA�@v�@vz@vB[@u�9@u|@uc�@u�@t�?@t�@t�@s��@s&@r�y@r��@r�@r}V@r8�@q�)@qX@pM@o{J@o�@n�h@ne@m��@l�K@l�@l�@k��@k\)@j�@jȴ@j�@jC�@i��@i8�@h��@hA�@h�@g�}@g|�@f��@fp;@fGE@e��@e&�@d��@d��@dy>@du�@d6@d~@d �@c��@c�f@cS�@c�@b�@bE�@a@a��@a:�@`�@`Ɇ@`�@`1'@_� @_~�@_/�@^ߤ@^ �@]�7@\�@\�j@[�;@[e�@[8@Z�"@Z�'@Z�L@Z�@Zs�@Z.�@Z@Y��@X�5@X]d@X7�@Xb@W˒@W�@@W8@V��@V:*@Ve@V
�@U�@Uj@U�@T��@TU2@T<�@T4n@S��@S�@S��@R�@RV@R
�@Q�t@Q#�@P��@P�_@PK^@P �@O�6@OH�@N�x@N($@M�@M�@L��@Lw�@L/�@K��@K��@K
=@J��@Jh
@JZ�@JB[@J�@I��@H��@Hy>@HZ@G��@G�w@G�@G|�@GP�@G/�@F�m@Fu%@FE�@F!�@E�@E�z@ErG@E2a@D��@D�z@D�@D~@C��@B��@BV@B$�@A�D@A��@A^�@@�5@@l"@?�@?�*@?iD@>��@>��@=�d@=&�@<�v@<m�@<G@;ݘ@;��@;\)@:��@:0U@9��@9��@9o @95�@8�z@8u�@8Z@8b@7�q@7dZ@76z@6�2@6�b@6i�@5�.@5/@4�@4�@4�9@4r�@44n@3�Q@3�0@3��@39�@2�@2��@2h
@2{@1�9@1��@1c@1�@0�@0?�@/��@/U�@/)_@/o@.�c@.�R@.u%@.GE@.1�@.�@-w2@-J�@-%F@,��@,Xy@,7�@,@+�
@+��@+�:@+t�@+�@*��@*��@*V@*	@)�@)�S@)k�@)N<@(��@(]d@(C-@'�]@'�&@'�Q@'��@'�g@'�w@'��@'�@'|�@''�@&��@&��@&{�@&Z�@&E�@&1�@&�@%�@%�'@%|@%=�@%@$�@$��@$��@$C-@#�m@#��@#�{@#\)@#@"�@"�@"i�@"$�@!ԕ@!��@!Y�@!:�@!%F@!!�@ ی@ �u@ <�@ *�@ �@ 1@�@�@˒@��@�@��@��@{@\�@V@�@�	@�	@�|@�K@�@��@�O@�D@u�@c�@[�@Ft@%�@��@��@��@X�@8@�@�s@��@� @E�@0U@-@#:@��@�'@�~@}�@k�@N<@�@ѷ@q@x@��@�f@{J@x@n/@_p@X�@S�@@O@�@�@�@҉@��@��@Z�@)�@J@ϫ@�'@s�@<6@4@��@ѷ@Ɇ@��@Q�@D�@-�@�@�@�K@��@�F@��@�P@n/@F�@@@�X@��@\�@GE@0U@��@�h@=�@q@�@~(@I�@@��@e�@@O@,�@!-@͟@��@��@�x@� @_�@�@u@�>@��@��@��@��@rG@8�@�@��@�/@��@r�@?�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A̟!A̜�A̙�A̟�ḀFA̢hA̞�A̟�A̟VA̞OA̜A̙eA̘�A̚kA̛qA̜A̞�A̡�ḀFA̦LA̧A̧RḀ�A̡�ḀzẠ�A̜�A̞�A̍A�~�A�^A���A���A˱�A�_�A��A��mAʧ�A�s�A�E�Aɶ�A�T,A�[WAƘ+A�$�A��OAŕ�A�^�A�AĹ�A�I�A�4�A��PA�S[A��A�X�A���A�@�A�N�A���A���A��A�^�A��A��~A�MA���A�PA��	A��lA���A�S�A�d�A�FtA�\�A�ҽA�OBA�OvA�t�A���A�v+A�A��sA�L�A�b�A���A�H�A�P}A��FA���A�w2A�FA�MA�m]A�u�A��A�\�A�K�A��A�R�A��iA�QAw�At��Ar�An��Ai1�AfAep;Ae�Aa�~A^FA\7�AW��AT7�AP��AM��AL��AJ�+AF�PABB[A@��A@}�A@Q�A>A;PHA9�4A98�A8ȴA8m]A7��A6A�A5�PA5�A4�qA4IRA3ںA2��A1N�A0��A/�&A-�/A+��A*($A(iDA'�A'�AA'�A&�A%R�A&�A's�A&l�A&3�A$��A$�A!TaA \�A�KA��A /A�PA��A?A~AA\�A�A�A��A~(A�\A�RA��A�A��A�
A�^A|�A�ZArGA�A�/A4�A�A�Aq�A!�A�EA?�AK^A��AJAy>A�A�HAE9A��A��Ah
A$tA�A��A�A?}A��A�A��A8�A��A8�A8�A1�A��A}VA0UA��A3�A
��A
?�A
+A	�;A	 iAA AA�EA}VA6�A��A��A� A�ArGA4A��A��AZ�A ��A ��A ��A ��A y>A ]dA J�A 4@�W?@���@�m]@��1@��{@�n�@�dZ@�"h@�PH@��@��D@�O@�/�@�?}@�@�f�@��c@ꍹ@��@�iD@�}@�
�@��P@��@�zx@��@��@�C@�R@��@�hs@��f@��@�&@��c@�i�@�B�@�l�@݁�@�A @�ѷ@��@�)_@ڋD@��@�k�@ظR@��+@�X�@��@��Z@�RT@��@�"h@�/@�{�@��;@ѥ�@ї�@���@�c�@ϧ�@Ϊe@��Z@͓�@�t�@��@�_@�Z�@���@ʳh@��@�x�@�;d@��@��@�xl@�D�@�5?@� �@�@ǳ�@�]�@�/�@���@���@Ɨ�@�9X@��#@�H�@���@��@Ğ@��@�w2@��|@§@.@�I�@�x�@��@���@�Ov@��@��[@�x@��@��Y@��m@�e�@�N<@�4�@�*0@�
=@���@�7�@�$@��@�	�@��@��Q@���@���@�=�@�!�@��@��
@���@�oi@���@�rG@��@��F@��@���@��+@�h
@�˒@��@�s@�A�@��@�ȴ@�Xy@��+@���@�x@�+@�oi@�/�@�˒@�)_@��+@�Q�@��w@�Z@�qv@��@�_@��n@�RT@��4@�@��h@�V@��@�$�@��@�y�@��Y@�Ft@�*�@� �@��@���@��@��z@�n�@�N�@��@���@��@���@�PH@�@��6@��$@�Q�@���@���@��I@��@��]@���@���@�c@�O@���@��@��@�\�@���@�xl@��@�IR@�=�@���@�l�@���@�hs@�E9@��H@���@�%�@���@�x�@��@��h@�q@�6�@��T@��T@��@��9@��P@�8�@��@�@��	@���@���@�$@�x@�"�@��o@��@��)@�ƨ@��m@�ݘ@���@�4@��[@��O@�S�@��@��T@���@�1�@�Ĝ@��@�@�j�@�.I@���@�YK@��d@���@�U�@�+@��I@�j@�M@��&@��t@���@���@�{J@�G�@�%@��\@�J@��}@��~@�Z�@��K@��9@�u%@�*�@�
�@��;@���@���@�Y@��,@���@��A@�c @��@��@�o�@�!-@�Ɇ@���@�� @�R�@�J@���@��w@���@�x�@�e,@�4�@��@��8@���@��K@��B@���@�oi@�Q@��+@���@�)_@��}@�Q�@�!�@�@��@"�@~͟@~�R@~~�@}�@}p�@}&�@|�@|ѷ@|�D@|(�@{�g@{@z��@z=q@y�@y��@y}�@y+@x`�@x�@wA�@v�@vz@vB[@u�9@u|@uc�@u�@t�?@t�@t�@s��@s&@r�y@r��@r�@r}V@r8�@q�)@qX@pM@o{J@o�@n�h@ne@m��@l�K@l�@l�@k��@k\)@j�@jȴ@j�@jC�@i��@i8�@h��@hA�@h�@g�}@g|�@f��@fp;@fGE@e��@e&�@d��@d��@dy>@du�@d6@d~@d �@c��@c�f@cS�@c�@b�@bE�@a@a��@a:�@`�@`Ɇ@`�@`1'@_� @_~�@_/�@^ߤ@^ �@]�7@\�@\�j@[�;@[e�@[8@Z�"@Z�'@Z�L@Z�@Zs�@Z.�@Z@Y��@X�5@X]d@X7�@Xb@W˒@W�@@W8@V��@V:*@Ve@V
�@U�@Uj@U�@T��@TU2@T<�@T4n@S��@S�@S��@R�@RV@R
�@Q�t@Q#�@P��@P�_@PK^@P �@O�6@OH�@N�x@N($@M�@M�@L��@Lw�@L/�@K��@K��@K
=@J��@Jh
@JZ�@JB[@J�@I��@H��@Hy>@HZ@G��@G�w@G�@G|�@GP�@G/�@F�m@Fu%@FE�@F!�@E�@E�z@ErG@E2a@D��@D�z@D�@D~@C��@B��@BV@B$�@A�D@A��@A^�@@�5@@l"@?�@?�*@?iD@>��@>��@=�d@=&�@<�v@<m�@<G@;ݘ@;��@;\)@:��@:0U@9��@9��@9o @95�@8�z@8u�@8Z@8b@7�q@7dZ@76z@6�2@6�b@6i�@5�.@5/@4�@4�@4�9@4r�@44n@3�Q@3�0@3��@39�@2�@2��@2h
@2{@1�9@1��@1c@1�@0�@0?�@/��@/U�@/)_@/o@.�c@.�R@.u%@.GE@.1�@.�@-w2@-J�@-%F@,��@,Xy@,7�@,@+�
@+��@+�:@+t�@+�@*��@*��@*V@*	@)�@)�S@)k�@)N<@(��@(]d@(C-@'�]@'�&@'�Q@'��@'�g@'�w@'��@'�@'|�@''�@&��@&��@&{�@&Z�@&E�@&1�@&�@%�@%�'@%|@%=�@%@$�@$��@$��@$C-@#�m@#��@#�{@#\)@#@"�@"�@"i�@"$�@!ԕ@!��@!Y�@!:�@!%F@!!�@ ی@ �u@ <�@ *�@ �@ 1@�@�@˒@��@�@��@��@{@\�@V@�@�	@�	@�|@�K@�@��@�O@�D@u�@c�@[�@Ft@%�@��@��@��@X�@8@�@�s@��@� @E�@0U@-@#:@��@�'@�~@}�@k�@N<@�@ѷ@q@x@��@�f@{J@x@n/@_p@X�@S�@@O@�@�@�@҉@��@��@Z�@)�@J@ϫ@�'@s�@<6@4@��@ѷ@Ɇ@��@Q�@D�@-�@�@�@�K@��@�F@��@�P@n/@F�@@@�X@��@\�@GE@0U@��@�h@=�@q@�@~(@I�@@��@e�@@O@,�@!-@͟@��@��@�x@� @_�@�@u@�>@��@��@��@��@rG@8�@�@��@�/@��@r�@?�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
NB
4B
 B
4B
�B
hB
4B
B
NB
B
�B
4B
B
.B
�B
B
B
�B
�B
�B
MB
2B
�B
YB
SB

B
KB
KB
�B
#nB
&2B
(�B
)�B
*KB
+�B
,�B
/ B
0�B
3MB
4B
8�B
GzB
Z�B
y�B
�HB
�jB
��B
��B
�@B
��B
�_B
�bB
�qB
��B
�GB
��B
�]B'B�B�B!bB,�B7�B\�Bi_Bk�Bj�B{�B�B�~B�2B��B�JB�B�xB�B�tB�B��B��BՁB�B�	B�EB�B��B��B�B��Bp;BlqBeFBZBA�B�B
�]B
�QB
�7B
�KB
��B
UB
�B	�,B	��B	��B	�4B	�tB	oOB	kB	ffB	VSB	AoB	7�B	!�B	oB	�B��B��B	 B	 iB��B��B��B�8B�B�cB��B�wB��B�HB	uB	B	�B	B	�B	,B	B	�B	 \B	+6B	2GB	FB	CB	9rB	8lB	BB	K�B	O�B	V�B	W�B	yrB	��B	�B	��B	��B	�MB	�xB	��B	��B	��B	�;B	��B	�5B	�B	��B	�B	�TB	�|B	�MB	��B	�B	�BB	��B	�(B	уB	��B	ޞB	�TB	�>B	��B	��B	�B	��B	��B	�B	�B	��B	�B	�B	�QB	�B	�dB	��B	�B	��B	ؓB	�dB	ּB	�B	چB	ބB	��B	�}B	�B	�B	�wB	�
B	�mB	�B	�LB	��B	�}B	��B	��B	�fB	��B	�8B	�LB	��B	�LB	��B	��B	��B	��B	��B	�B	�oB	�B	�;B	��B	�B	�B	��B	�B	�B	��B	��B	�KB	�_B	�/B	�B	�B	�@B	�RB	�
B	�mB	��B	�jB	߾B	�NB	��B	�TB	�VB	��B	�FB	��B	�0B	��B	ɠB	ɠB	�=B	�XB	ˬB	ˬB	�dB	�dB	�PB	�(B	��B	�.B	ѝB	�B	�B	�2B	�gB	�B	յB	�EB	��B	�5B	�\B	޸B	��B	��B	�kB	�]B	ڠB	�YB	�
B	��B	֡B	��B	خB	��B	�eB	�B	��B	��B	�WB	�=B	�eB	ؓB	��B	��B	��B	��B	��B	�1B	ٚB	��B	�'B	�;B	ݘB	�IB	�]B	�CB	��B	�B	�dB	��B	�IB	�!B	�-B	�bB	�|B	�B	�B	�:B	�B	�&B	��B	�B	�fB	�B	��B	�XB	�XB	��B	�eB	�B	�)B	�[B	�B	�B	��B	�B	��B	�FB	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�`B	��B	�%B	��B	��B	��B	�B	�nB	�B	�B	�B	��B	��B	�aB	��B	��B	� B	�cB	�cB	�B	�B	��B	�%B	��B	��B	��B	�DB	��B	��B	��B	�(B
 �B
�B
B
�B
B
�B
{B
B
{B
�B
�B	��B	��B	�DB	��B	�+B	��B	�`B	�`B	��B	�2B	�RB	�B	�8B	�>B	�DB	�*B	�xB	�*B	��B	��B	�dB	��B	��B	��B	�B	�HB	�.B	��B	��B	�HB	�HB	�}B	��B
 OB
 OB
B
�B
[B
[B
�B
�B
aB
B
�B
�B
�B
-B
gB
B
GB
B
�B
mB
MB
�B
�B
gB
�B
�B
�B
3B
9B
mB
B
B
�B
�B
�B
�B
B
�B
gB
%B
	B

�B
�B
�B
�B
TB
�B
�B
�B
B
�B
�B
B
�B
=B
�B
IB
IB
IB
�B
�B
B
5B
�B
�B
�B
!B
 'B
!�B
!|B
!�B
"B
"�B
#B
#B
#�B
#�B
$&B
#�B
$B
$�B
$�B
%�B
&fB
&LB
'RB
'8B
'mB
'mB
'�B
'�B
(sB
(sB
(sB
(sB
)_B
)�B
)DB
)�B
)_B
*KB
+B
*�B
+QB
+�B
+�B
,WB
,�B
-CB
-CB
-]B
-�B
-�B
-�B
.IB
.�B
.�B
/B
/ B
/B
0!B
0B
/�B
0;B
0UB
0�B
0�B
1vB
2B
2B
1�B
2GB
2aB
2aB
2|B
33B
3�B
3�B
49B
3hB
3�B
3�B
4B
4�B
4nB
5ZB
5�B
5%B
5ZB
5ZB
5�B
6B
6zB
6�B
6�B
72B
7�B
7�B
7�B
7�B
7fB
8�B
8lB
8�B
9XB
9rB
9�B
9XB
9�B
9�B
:*B
:^B
;JB
;�B
;�B
<PB
="B
=<B
?.B
?HB
?�B
@OB
@iB
@4B
@�B
@iB
@OB
@�B
A B
AUB
BB
B'B
BB
BAB
B�B
B�B
B�B
B�B
CGB
C-B
CGB
C�B
B�B
C�B
C�B
CGB
C�B
DgB
C�B
DMB
D�B
D�B
EB
D�B
FB
E�B
ESB
E�B
F�B
E�B
F�B
FB
F�B
G�B
G_B
HfB
HKB
IB
IB
IB
IlB
I�B
IlB
IRB
I�B
I�B
IlB
IB
J=B
J#B
J#B
J=B
JrB
JXB
J�B
KB
K�B
KxB
KxB
K�B
KDB
LJB
L0B
L�B
L�B
L�B
M6B
MPB
MjB
NB
NB
N"B
M�B
N�B
N�B
OB
O(B
N�B
O(B
O�B
P.B
P�B
PbB
P�B
QhB
QhB
Q�B
Q�B
Q�B
Q�B
R:B
R�B
R�B
RoB
RoB
R�B
S�B
S�B
S&B
S@B
S�B
SuB
S�B
TFB
T,B
T{B
UB
UB
T�B
U2B
T�B
UB
VB
V9B
VmB
VB
U�B
V�B
W$B
W�B
W�B
W�B
WsB
W�B
X_B
X�B
X�B
YB
YB
Y1B
ZB
Z�B
[=B
[=B
[�B
[�B
\B
[�B
\CB
\�B
\�B
]IB
\�B
]dB
^OB
^�B
^5B
^�B
^�B
_!B
_;B
_pB
`'B
`vB
`\B
`'B
aB
a-B
a-B
aHB
a�B
a�B
a�B
a�B
a�B
b�B
bhB
b�B
b�B
cB
c�B
c�B
c�B
dB
c�B
d@B
d�B
d�B
ezB
ezB
e`B
e�B
e�B
f2B
e�B
e�B
f�B
f�B
f�B
gB
f�B
gmB
gmB
g�B
g�B
g�B
g�B
h�B
h>B
h$B
h�B
h�B
iDB
i*B
i*B
iyB
jB
j0B
i�B
jKB
j�B
kB
jeB
j�B
j�B
j�B
kB
j�B
kB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
lB
l=B
lqB
l�B
l�B
l�B
l�B
mB
m)B
m�B
m�B
m�B
m�B
m�B
m�B
nIB
n�B
n�B
oOB
oiB
oiB
o�B
p!B
o�B
o�B
p!B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
qvB
q�B
rB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s3B
sMB
s3B
shB
shB
shB
s�B
s�B
s�B
tB
tB
t9B
tnB
t�B
t�B
t�B
uB
uB
t�B
u%B
u%B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
vzB
v�B
wLB
w2B
wfB
wLB
w�B
wfB
wfB
wfB
w�B
w�B
w�B
w�B
w�B
xB
xB
xRB
xlB
xlB
x�B
x�B
y	B
y>B
y>B
y�B
y�B
y�B
y�B
zB
zB
zB
zDB
zxB
zxB
zxB
zxB
z�B
z�B
z�B
z�B
{0B
{B
{dB
{�B
{�B
{�B
{�B
|6B
|�B
|�B
|�B
}"B
}�B
}�B
}�B
~]B
~�B
~�B
~�B
~�B
HB
HB
HB
cB
HB
�B
� B
� B
�B
�4B
�B
�4B
�OB
��B
��B
� B
�B
�;B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
NB
4B
 B
4B
�B
hB
4B
B
NB
B
�B
4B
B
.B
�B
B
B
�B
�B
�B
MB
2B
�B
YB
SB

B
KB
KB
�B
#nB
&2B
(�B
)�B
*KB
+�B
,�B
/ B
0�B
3MB
4B
8�B
GzB
Z�B
y�B
�HB
�jB
��B
��B
�@B
��B
�_B
�bB
�qB
��B
�GB
��B
�]B'B�B�B!bB,�B7�B\�Bi_Bk�Bj�B{�B�B�~B�2B��B�JB�B�xB�B�tB�B��B��BՁB�B�	B�EB�B��B��B�B��Bp;BlqBeFBZBA�B�B
�]B
�QB
�7B
�KB
��B
UB
�B	�,B	��B	��B	�4B	�tB	oOB	kB	ffB	VSB	AoB	7�B	!�B	oB	�B��B��B	 B	 iB��B��B��B�8B�B�cB��B�wB��B�HB	uB	B	�B	B	�B	,B	B	�B	 \B	+6B	2GB	FB	CB	9rB	8lB	BB	K�B	O�B	V�B	W�B	yrB	��B	�B	��B	��B	�MB	�xB	��B	��B	��B	�;B	��B	�5B	�B	��B	�B	�TB	�|B	�MB	��B	�B	�BB	��B	�(B	уB	��B	ޞB	�TB	�>B	��B	��B	�B	��B	��B	�B	�B	��B	�B	�B	�QB	�B	�dB	��B	�B	��B	ؓB	�dB	ּB	�B	چB	ބB	��B	�}B	�B	�B	�wB	�
B	�mB	�B	�LB	��B	�}B	��B	��B	�fB	��B	�8B	�LB	��B	�LB	��B	��B	��B	��B	��B	�B	�oB	�B	�;B	��B	�B	�B	��B	�B	�B	��B	��B	�KB	�_B	�/B	�B	�B	�@B	�RB	�
B	�mB	��B	�jB	߾B	�NB	��B	�TB	�VB	��B	�FB	��B	�0B	��B	ɠB	ɠB	�=B	�XB	ˬB	ˬB	�dB	�dB	�PB	�(B	��B	�.B	ѝB	�B	�B	�2B	�gB	�B	յB	�EB	��B	�5B	�\B	޸B	��B	��B	�kB	�]B	ڠB	�YB	�
B	��B	֡B	��B	خB	��B	�eB	�B	��B	��B	�WB	�=B	�eB	ؓB	��B	��B	��B	��B	��B	�1B	ٚB	��B	�'B	�;B	ݘB	�IB	�]B	�CB	��B	�B	�dB	��B	�IB	�!B	�-B	�bB	�|B	�B	�B	�:B	�B	�&B	��B	�B	�fB	�B	��B	�XB	�XB	��B	�eB	�B	�)B	�[B	�B	�B	��B	�B	��B	�FB	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�`B	��B	�%B	��B	��B	��B	�B	�nB	�B	�B	�B	��B	��B	�aB	��B	��B	� B	�cB	�cB	�B	�B	��B	�%B	��B	��B	��B	�DB	��B	��B	��B	�(B
 �B
�B
B
�B
B
�B
{B
B
{B
�B
�B	��B	��B	�DB	��B	�+B	��B	�`B	�`B	��B	�2B	�RB	�B	�8B	�>B	�DB	�*B	�xB	�*B	��B	��B	�dB	��B	��B	��B	�B	�HB	�.B	��B	��B	�HB	�HB	�}B	��B
 OB
 OB
B
�B
[B
[B
�B
�B
aB
B
�B
�B
�B
-B
gB
B
GB
B
�B
mB
MB
�B
�B
gB
�B
�B
�B
3B
9B
mB
B
B
�B
�B
�B
�B
B
�B
gB
%B
	B

�B
�B
�B
�B
TB
�B
�B
�B
B
�B
�B
B
�B
=B
�B
IB
IB
IB
�B
�B
B
5B
�B
�B
�B
!B
 'B
!�B
!|B
!�B
"B
"�B
#B
#B
#�B
#�B
$&B
#�B
$B
$�B
$�B
%�B
&fB
&LB
'RB
'8B
'mB
'mB
'�B
'�B
(sB
(sB
(sB
(sB
)_B
)�B
)DB
)�B
)_B
*KB
+B
*�B
+QB
+�B
+�B
,WB
,�B
-CB
-CB
-]B
-�B
-�B
-�B
.IB
.�B
.�B
/B
/ B
/B
0!B
0B
/�B
0;B
0UB
0�B
0�B
1vB
2B
2B
1�B
2GB
2aB
2aB
2|B
33B
3�B
3�B
49B
3hB
3�B
3�B
4B
4�B
4nB
5ZB
5�B
5%B
5ZB
5ZB
5�B
6B
6zB
6�B
6�B
72B
7�B
7�B
7�B
7�B
7fB
8�B
8lB
8�B
9XB
9rB
9�B
9XB
9�B
9�B
:*B
:^B
;JB
;�B
;�B
<PB
="B
=<B
?.B
?HB
?�B
@OB
@iB
@4B
@�B
@iB
@OB
@�B
A B
AUB
BB
B'B
BB
BAB
B�B
B�B
B�B
B�B
CGB
C-B
CGB
C�B
B�B
C�B
C�B
CGB
C�B
DgB
C�B
DMB
D�B
D�B
EB
D�B
FB
E�B
ESB
E�B
F�B
E�B
F�B
FB
F�B
G�B
G_B
HfB
HKB
IB
IB
IB
IlB
I�B
IlB
IRB
I�B
I�B
IlB
IB
J=B
J#B
J#B
J=B
JrB
JXB
J�B
KB
K�B
KxB
KxB
K�B
KDB
LJB
L0B
L�B
L�B
L�B
M6B
MPB
MjB
NB
NB
N"B
M�B
N�B
N�B
OB
O(B
N�B
O(B
O�B
P.B
P�B
PbB
P�B
QhB
QhB
Q�B
Q�B
Q�B
Q�B
R:B
R�B
R�B
RoB
RoB
R�B
S�B
S�B
S&B
S@B
S�B
SuB
S�B
TFB
T,B
T{B
UB
UB
T�B
U2B
T�B
UB
VB
V9B
VmB
VB
U�B
V�B
W$B
W�B
W�B
W�B
WsB
W�B
X_B
X�B
X�B
YB
YB
Y1B
ZB
Z�B
[=B
[=B
[�B
[�B
\B
[�B
\CB
\�B
\�B
]IB
\�B
]dB
^OB
^�B
^5B
^�B
^�B
_!B
_;B
_pB
`'B
`vB
`\B
`'B
aB
a-B
a-B
aHB
a�B
a�B
a�B
a�B
a�B
b�B
bhB
b�B
b�B
cB
c�B
c�B
c�B
dB
c�B
d@B
d�B
d�B
ezB
ezB
e`B
e�B
e�B
f2B
e�B
e�B
f�B
f�B
f�B
gB
f�B
gmB
gmB
g�B
g�B
g�B
g�B
h�B
h>B
h$B
h�B
h�B
iDB
i*B
i*B
iyB
jB
j0B
i�B
jKB
j�B
kB
jeB
j�B
j�B
j�B
kB
j�B
kB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
lB
l=B
lqB
l�B
l�B
l�B
l�B
mB
m)B
m�B
m�B
m�B
m�B
m�B
m�B
nIB
n�B
n�B
oOB
oiB
oiB
o�B
p!B
o�B
o�B
p!B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
qvB
q�B
rB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s3B
sMB
s3B
shB
shB
shB
s�B
s�B
s�B
tB
tB
t9B
tnB
t�B
t�B
t�B
uB
uB
t�B
u%B
u%B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
vzB
v�B
wLB
w2B
wfB
wLB
w�B
wfB
wfB
wfB
w�B
w�B
w�B
w�B
w�B
xB
xB
xRB
xlB
xlB
x�B
x�B
y	B
y>B
y>B
y�B
y�B
y�B
y�B
zB
zB
zB
zDB
zxB
zxB
zxB
zxB
z�B
z�B
z�B
z�B
{0B
{B
{dB
{�B
{�B
{�B
{�B
|6B
|�B
|�B
|�B
}"B
}�B
}�B
}�B
~]B
~�B
~�B
~�B
~�B
HB
HB
HB
cB
HB
�B
� B
� B
�B
�4B
�B
�4B
�OB
��B
��B
� B
�B
�;B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104919  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173801  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173801  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173801                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023809  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023809  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                