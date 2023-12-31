CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2021-11-30T09:42:50Z creation;2021-11-30T09:42:52Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ``   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20211130094250  20211130095247  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @٥��n]L1   @٥�З�&@3�KƧ��dT�t�j1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B���B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D	  D	� D
  D
� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D��D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D:��D;� D<  D<� D=  D=� D>  D>� D?fD?�fD@  D@� DA  DA� DB  DB� DC  DC� DD  DDy�DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DUy�DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Ddy�De  De�fDf  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq� Dr  Dr� Ds  Ds� Dt  Dt� DufDu�fDv  Dv� Dw  Dw� Dx  Dx� DyfDy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�<�D�|�D�� D�  D�@ D�|�D�� D�  D�@ D̀ D�� D�  D�@ D̀ Dͼ�D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�<�DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D��3D�  D�@ Dހ D�� D�  D�@ D�|�D߼�D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @H��@��H@��HAp�A%p�AEp�Aep�A��RA��RA��RA��RA¸RAҸRA�RA�RB ��B	\)B\)B\)B!\)B)\)B1\)B9\)BA\)BI\)BQ\)BY\)Ba\)Bi\)Bq\)By\)B��B��B��B��B��B��B��GB��B�z�B��GB��GB��B��B��B��B��B��BĮBȮB̮BЮB��GB�z�B�z�B�z�B�B�B�B�B��B��B��C W
CW
CW
CW
CW
C
W
CW
Cp�CW
CW
CW
CW
Cp�CW
CW
CW
C W
C"W
C$W
C&W
C(W
C*W
C,W
C.W
C0W
C2W
C4W
C6W
C8W
C:W
C<W
C>W
C@W
CBW
CDW
CFW
CHW
CJW
CLW
CNW
CPW
CRW
CTW
CVW
CXW
CZW
C\W
C^W
C`=pCbW
CdW
CfW
ChW
CjW
ClW
CnW
Cp=pCrW
CtW
CvW
Cx=pCzW
C|W
C~W
C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C��C�+�C�8RC�+�C��C��C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D�)D�D��D	�D	��D
�D
��D�D��D�D��D�D��D)D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D]D��D�D��D�D��D�D��D�D��D�D��D�D��D ]D �]D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)�)D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9�)D:�D:��D;]D;��D<�D<��D=�D=��D>�D>��D?)D?�)D@�D@��DA�DA��DB�DB��DC�DC��DD�DD�]DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU�]DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[)D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd]Dd�]De�De�)Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj�]Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq)Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du)Du�)Dv�Dv��Dw�Dw��Dx�Dx��Dy)Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D��D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D��D���D�
�D�J�D���D�ǮD�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D��D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D��D�G�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D��D�D�ND���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D��D��D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D��D�G�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�G�D���D���D�
�D�J�D��D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�D�ND��D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D�D���D�
�D�J�DÊ�D���D�
�D�J�DĊ�D���D�
�D�J�DŊ�D���D�
�D�J�DƊ�D���D�
�D�J�DǊ�D���D�
�D�J�DȊ�D���D�
�D�J�DɊ�D���D�
�D�G�Dʇ�D���D�
�D�J�Dˇ�D���D�
�D�J�D̊�D���D�
�D�J�D͊�D�ǮD�
�D�J�DΊ�D���D�
�D�J�Dϊ�D���D�
�D�G�DЊ�D���D�
�D�J�Dъ�D���D�
�D�J�DҊ�D���D�
�D�J�Dӊ�D���D�
�D�J�DԊ�D���D�
�D�J�DՊ�D���D�
�D�J�D֊�D���D�
�D�J�D׊�D���D�
�D�J�D؊�D���D�
�D�J�Dي�D���D�
�D�J�Dڊ�D���D�
�D�J�Dۊ�D���D�
�D�J�D܊�D���D�
�D�J�D݊�D��D�
�D�J�Dފ�D���D�
�D�J�D߇�D�ǮD�
�D�J�D���D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D��D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D���D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�D�J�D���D��G11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A֛�A֛�A֝�A֛�A֝�A֝�A֛�A֝�A֝�A֟�A֟�A֟�A֟�A֡�A֣�A֣�A֥�A֥�A֣�A֣�A֥�A֥�A֥�A֥�A֟�A։7A�1A�A�Aԡ�A��A�A���A��Aӣ�A�ffA�`BA�K�A�oA�bNA��A�ƨA�9XA���A���A�hsA���A�bNA�+A��A�O�A��uA���A��#A��;A���A���A�%A���A�^5A���A�dZA�"�A��7A��^A���A��A��RA�Q�A��A�7LA�  A�hsA���A�33A��A�XA��;A��mA�n�A��mA��uA�ZA��A���A��9A���A�{A�p�A�x�A��7A�A��hA��A�hsA�(�A�t�A��A�7LA�K�A��A��A�  A��A�(�A~��A{��Ax~�Au�-Aq�Ao�-Ao%Ak�TAiO�Agt�Af�9AfbNAd�AbVA`ĜA^��A]ƨAZ5?AV��AUt�ATE�ARM�APjAN�/AM�^AL�\AI��AI+AHr�AFȴAD��ACK�A@bNA>�A<n�A<�A;O�A9��A8�!A7|�A5/A2v�A0�A/l�A.^5A,�jA+��A*�A*  A)?}A(��A&�A$r�A#�A"��A"�+A"M�A"1A!ƨA ��A��A�PA�;Al�AG�A�^A|�A��A�A�A�A9XA��A��AG�A�`AE�A�PA/A��A�;AAQ�A\)AbA�A��A�wA
�9A�AVA�A��A�A9XAo@��@�Z@�x�@�"�@�z�@��@�$�@�`B@�I�@�"�@���@��@�@�w@�M�@��T@���@�hs@�1@��y@�Z@���@�E�@�&�@թ�@ԋD@ӶF@�$�@���@с@�A�@Η�@��@��m@�K�@���@�O�@�\)@���@�C�@���@�Ĝ@�1@��R@���@��@�r�@�b@��P@�"�@�E�@���@�7L@��u@���@�V@�X@��9@�r�@���@�K�@�C�@��@�V@�@��^@�hs@���@��9@�9X@��w@�l�@�o@��@��@�ȴ@���@�=q@�p�@���@���@��D@�r�@�A�@���@��H@�^5@�$�@��#@��^@�`B@���@��/@�z�@�bN@�I�@�1@��
@�S�@�@��!@�@�x�@��j@��@�Z@�I�@�9X@��w@���@�M�@�5?@�$�@��#@��h@�&�@��@�V@��`@�V@�ff@��u@��h@���@��7@��
@���@���@��@��m@��
@��F@�;d@�\)@�C�@�o@�ȴ@�M�@�5?@��@��@��7@��@�G�@�%@���@��@�\)@�t�@��P@���@��P@�t�@�C�@��@���@�^5@�-@���@���@���@���@�X@�7L@�&�@�V@��D@�1'@���@���@��P@��P@��@���@��@���@�^5@�-@��@��#@���@���@��-@���@��@�x�@���@�Q�@��@���@���@��@��D@�I�@�(�@��
@��F@�dZ@�
=@��H@���@��\@�n�@��@�{@�@��#@�@�hs@�7L@���@��@�Z@�Q�@�I�@�(�@���@��w@��P@�t�@�l�@�dZ@�
=@��+@�=q@�@��@��T@���@��-@��h@�hs@�?}@�&�@��@��@��9@�I�@��
@���@��@�t�@�l�@�S�@�+@�o@�
=@�
=@�@��y@��R@�ff@�-@���@��#@���@�hs@�7L@���@�Ĝ@�z�@�I�@�(�@�  @�ƨ@�l�@�+@���@���@���@��!@���@��+@�^5@�5?@��@��@�@��-@��h@�hs@�7L@�bN@�1@��;@��@��P@�|�@�\)@�33@�@��@���@�$�@��T@��^@��-@��7@�hs@��@���@��@�Q�@� �@�t�@�33@��@��y@��@�ȴ@���@�ff@�^5@�V@�V@�=q@�{@�@�G�@���@���@���@���@��@�Z@� �@\)@~ȴ@~ff@~@}O�@|��@|�D@|Z@|�@{�
@{t�@{33@z�\@y��@y�@xĜ@x�u@x �@w��@v�y@vE�@v@u�-@u�@t��@t�j@tj@st�@r�@r~�@q�^@p��@o�@oK�@o
=@n��@nE�@m@m/@l�/@lz�@lI�@l(�@k��@jn�@jJ@i��@ihs@i7L@h��@h �@g�w@g\)@f��@f��@fff@fE�@f5?@f{@e�-@e�h@e?}@d�j@d��@dZ@c��@c�@co@bJ@aX@a&�@a�@`��@`bN@_�;@_l�@^�@^v�@^{@]�h@]�@\�@\�D@\j@\j@\I�@\9X@[�m@Z��@Z-@Y��@Yx�@X�@X �@W�@WK�@V��@V$�@U�-@U�h@UO�@UV@T��@T�@S�m@St�@S33@So@R��@R��@R�!@Rn�@R�@Q��@Q�@Q��@Q�7@Q7L@P��@PA�@O��@O�@O��@O;d@N�@Nȴ@N��@N�+@Nff@NE�@N$�@M�@MO�@M�@L��@L�/@L�@LI�@K��@KdZ@J�@J��@J^5@JJ@I�#@I��@Ihs@I%@H�@HQ�@Hb@G��@G��@G\)@G
=@F�R@F��@F5?@E�@E�-@Ep�@D�@DZ@C��@Cƨ@C�F@C��@C�@Co@B��@B~�@A�^@Ahs@AG�@A�@@��@@�u@@b@?�@?|�@?\)@?K�@?;d@?
=@>ȴ@>5?@=��@=�@=p�@=?}@=/@<�@<Z@<9X@<�@;ƨ@;t�@;"�@:�H@:��@:�@9��@9x�@9hs@9X@97L@8��@8�`@8��@8Q�@8b@7�;@7�@7��@7��@7|�@7+@6ff@5�@5�-@5�h@5`B@5?}@5V@4��@4�@4Z@4(�@3��@3t�@3"�@3@2�\@2-@1�#@1�7@0��@0r�@0  @/��@/�w@/��@/l�@/�@.��@.�@.�@.�@.�@.ȴ@.��@.E�@-�@-@-��@-�@-`B@-?}@,�j@,9X@,1@+�F@+S�@*�@*��@*n�@*^5@*�@)��@)��@)�7@)G�@(��@(bN@'�;@'��@'�@'�P@'l�@';d@&�R@&�+@&$�@%�T@%��@%@%��@%?}@$z�@$9X@$(�@$1@#�m@#��@#33@"��@"��@"~�@"^5@"M�@"-@"�@"J@!�@!��@!&�@ �`@ Ĝ@ �u@ r�@ Q�@ 1'@��@�P@\)@K�@+@
=@��@�y@ȴ@ȴ@��@v�@E�@@�@��@��@��@��@�@��@��@z�@Z@��@�F@��@dZ@C�@33@33@33@33@"�@@�H@�\@��@�7@X@G�@7L@�@�@%@��@�u@A�@�w@�P@l�@\)@�@��@�y@ȴ@��@�+@�+@�+@�+@v�@ff@V@@@��@�@O�@�@��@�D@9X@�@��@�
@�
@��@S�@"�@�H@~�@^5@J@�@��@��@��@��@��@��@hs@�`@Ĝ@�9@��@�u@r�@1'@b@�@�;@�@|�@K�@+@�R@��@�+@v�@v�@V@@p�@/@��@�@�/@�/@��@��@z�@I�@�@1@�m@�F@��@dZ@C�@33@@
��@
��@
��@
�!@
��@
�\@
�\@
~�@
^5@	��@	X@	�@	%@�`@��@��@�@r�@A�@�@��@�@|�@;d@�y11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A֛�A֛�A֝�A֛�A֝�A֝�A֛�A֝�A֝�A֟�A֟�A֟�A֟�A֡�A֣�A֣�A֥�A֥�A֣�A֣�A֥�A֥�A֥�A֥�A֟�A։7A�1A�A�Aԡ�A��A�A���A��Aӣ�A�ffA�`BA�K�A�oA�bNA��A�ƨA�9XA���A���A�hsA���A�bNA�+A��A�O�A��uA���A��#A��;A���A���A�%A���A�^5A���A�dZA�"�A��7A��^A���A��A��RA�Q�A��A�7LA�  A�hsA���A�33A��A�XA��;A��mA�n�A��mA��uA�ZA��A���A��9A���A�{A�p�A�x�A��7A�A��hA��A�hsA�(�A�t�A��A�7LA�K�A��A��A�  A��A�(�A~��A{��Ax~�Au�-Aq�Ao�-Ao%Ak�TAiO�Agt�Af�9AfbNAd�AbVA`ĜA^��A]ƨAZ5?AV��AUt�ATE�ARM�APjAN�/AM�^AL�\AI��AI+AHr�AFȴAD��ACK�A@bNA>�A<n�A<�A;O�A9��A8�!A7|�A5/A2v�A0�A/l�A.^5A,�jA+��A*�A*  A)?}A(��A&�A$r�A#�A"��A"�+A"M�A"1A!ƨA ��A��A�PA�;Al�AG�A�^A|�A��A�A�A�A9XA��A��AG�A�`AE�A�PA/A��A�;AAQ�A\)AbA�A��A�wA
�9A�AVA�A��A�A9XAo@��@�Z@�x�@�"�@�z�@��@�$�@�`B@�I�@�"�@���@��@�@�w@�M�@��T@���@�hs@�1@��y@�Z@���@�E�@�&�@թ�@ԋD@ӶF@�$�@���@с@�A�@Η�@��@��m@�K�@���@�O�@�\)@���@�C�@���@�Ĝ@�1@��R@���@��@�r�@�b@��P@�"�@�E�@���@�7L@��u@���@�V@�X@��9@�r�@���@�K�@�C�@��@�V@�@��^@�hs@���@��9@�9X@��w@�l�@�o@��@��@�ȴ@���@�=q@�p�@���@���@��D@�r�@�A�@���@��H@�^5@�$�@��#@��^@�`B@���@��/@�z�@�bN@�I�@�1@��
@�S�@�@��!@�@�x�@��j@��@�Z@�I�@�9X@��w@���@�M�@�5?@�$�@��#@��h@�&�@��@�V@��`@�V@�ff@��u@��h@���@��7@��
@���@���@��@��m@��
@��F@�;d@�\)@�C�@�o@�ȴ@�M�@�5?@��@��@��7@��@�G�@�%@���@��@�\)@�t�@��P@���@��P@�t�@�C�@��@���@�^5@�-@���@���@���@���@�X@�7L@�&�@�V@��D@�1'@���@���@��P@��P@��@���@��@���@�^5@�-@��@��#@���@���@��-@���@��@�x�@���@�Q�@��@���@���@��@��D@�I�@�(�@��
@��F@�dZ@�
=@��H@���@��\@�n�@��@�{@�@��#@�@�hs@�7L@���@��@�Z@�Q�@�I�@�(�@���@��w@��P@�t�@�l�@�dZ@�
=@��+@�=q@�@��@��T@���@��-@��h@�hs@�?}@�&�@��@��@��9@�I�@��
@���@��@�t�@�l�@�S�@�+@�o@�
=@�
=@�@��y@��R@�ff@�-@���@��#@���@�hs@�7L@���@�Ĝ@�z�@�I�@�(�@�  @�ƨ@�l�@�+@���@���@���@��!@���@��+@�^5@�5?@��@��@�@��-@��h@�hs@�7L@�bN@�1@��;@��@��P@�|�@�\)@�33@�@��@���@�$�@��T@��^@��-@��7@�hs@��@���@��@�Q�@� �@�t�@�33@��@��y@��@�ȴ@���@�ff@�^5@�V@�V@�=q@�{@�@�G�@���@���@���@���@��@�Z@� �@\)@~ȴ@~ff@~@}O�@|��@|�D@|Z@|�@{�
@{t�@{33@z�\@y��@y�@xĜ@x�u@x �@w��@v�y@vE�@v@u�-@u�@t��@t�j@tj@st�@r�@r~�@q�^@p��@o�@oK�@o
=@n��@nE�@m@m/@l�/@lz�@lI�@l(�@k��@jn�@jJ@i��@ihs@i7L@h��@h �@g�w@g\)@f��@f��@fff@fE�@f5?@f{@e�-@e�h@e?}@d�j@d��@dZ@c��@c�@co@bJ@aX@a&�@a�@`��@`bN@_�;@_l�@^�@^v�@^{@]�h@]�@\�@\�D@\j@\j@\I�@\9X@[�m@Z��@Z-@Y��@Yx�@X�@X �@W�@WK�@V��@V$�@U�-@U�h@UO�@UV@T��@T�@S�m@St�@S33@So@R��@R��@R�!@Rn�@R�@Q��@Q�@Q��@Q�7@Q7L@P��@PA�@O��@O�@O��@O;d@N�@Nȴ@N��@N�+@Nff@NE�@N$�@M�@MO�@M�@L��@L�/@L�@LI�@K��@KdZ@J�@J��@J^5@JJ@I�#@I��@Ihs@I%@H�@HQ�@Hb@G��@G��@G\)@G
=@F�R@F��@F5?@E�@E�-@Ep�@D�@DZ@C��@Cƨ@C�F@C��@C�@Co@B��@B~�@A�^@Ahs@AG�@A�@@��@@�u@@b@?�@?|�@?\)@?K�@?;d@?
=@>ȴ@>5?@=��@=�@=p�@=?}@=/@<�@<Z@<9X@<�@;ƨ@;t�@;"�@:�H@:��@:�@9��@9x�@9hs@9X@97L@8��@8�`@8��@8Q�@8b@7�;@7�@7��@7��@7|�@7+@6ff@5�@5�-@5�h@5`B@5?}@5V@4��@4�@4Z@4(�@3��@3t�@3"�@3@2�\@2-@1�#@1�7@0��@0r�@0  @/��@/�w@/��@/l�@/�@.��@.�@.�@.�@.�@.ȴ@.��@.E�@-�@-@-��@-�@-`B@-?}@,�j@,9X@,1@+�F@+S�@*�@*��@*n�@*^5@*�@)��@)��@)�7@)G�@(��@(bN@'�;@'��@'�@'�P@'l�@';d@&�R@&�+@&$�@%�T@%��@%@%��@%?}@$z�@$9X@$(�@$1@#�m@#��@#33@"��@"��@"~�@"^5@"M�@"-@"�@"J@!�@!��@!&�@ �`@ Ĝ@ �u@ r�@ Q�@ 1'@��@�P@\)@K�@+@
=@��@�y@ȴ@ȴ@��@v�@E�@@�@��@��@��@��@�@��@��@z�@Z@��@�F@��@dZ@C�@33@33@33@33@"�@@�H@�\@��@�7@X@G�@7L@�@�@%@��@�u@A�@�w@�P@l�@\)@�@��@�y@ȴ@��@�+@�+@�+@�+@v�@ff@V@@@��@�@O�@�@��@�D@9X@�@��@�
@�
@��@S�@"�@�H@~�@^5@J@�@��@��@��@��@��@��@hs@�`@Ĝ@�9@��@�u@r�@1'@b@�@�;@�@|�@K�@+@�R@��@�+@v�@v�@V@@p�@/@��@�@�/@�/@��@��@z�@I�@�@1@�m@�F@��@dZ@C�@33@@
��@
��@
��@
�!@
��@
�\@
�\@
~�@
^5@	��@	X@	�@	%@�`@��@��@�@r�@A�@�@��@�@|�@;d@�y11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BQ�BQ�BQ�BP�BP�BQ�BP�BP�BP�BS�B`BB�B�BŢB�NB�fB�fB�mB�B�B�B�B�B�B�B�HB�5B��B��BB�B+B)�B49BA�B>wB<jB#�BB��BB��B��B��B��BB��B��B��B��B��B��B�B�sB�ZB�B�HB�;B�)B��B��B��BŢB�!B��B�{B�=B|�BiyB`BBVBL�BG�B<jB33B(�B �B
=B
��B
�HB
ɺB
�9B
�B
��B
�B
v�B
iyB
T�B
O�B
G�B
.B
�B
VB	��B	�B	�`B	�B	��B	��B	�jB	�^B	�?B	��B	��B	��B	�\B	�+B	n�B	ffB	bNB	YB	N�B	B�B	8RB	2-B	)�B	%�B	#�B	 �B	�B	DB	B��B�B�B�B�yB�TB�;B�
B��BŢB��B�qB�^B�FB�?B�3B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�B�-B�jBĜB��B�;B�;B�BB�HB�HB�5B�/B�)B�#B�#B�B�B�B�
B��B�
B�B��B��B��BɺBÖB�FB�B�B�B��B��B��B��B�\B�=B�%B�B� B}�B|�By�By�Bv�Bx�Bt�Bv�Bt�Br�Bv�Br�Bs�Bs�Bt�By�B�Bz�B{�B�B�%B�B�%B�=B�VB�\B�bB�hB��B��B��B��B��B��B��B��B��B�B�B�-B�3B�?B�LB�dB�qB��BBǮB��B��B�B�B�5B�NB�NB�TB�ZB�`B�mB�yB�B�B�B��B��B��B��B��B��B��B	B	1B	JB	PB	VB	VB	bB	�B	�B	!�B	"�B	$�B	$�B	(�B	-B	.B	33B	5?B	7LB	:^B	<jB	?}B	@�B	C�B	F�B	N�B	XB	YB	[#B	`BB	`BB	cTB	dZB	e`B	ffB	jB	n�B	o�B	p�B	t�B	w�B	y�B	}�B	�+B	��B	��B	��B	��B	��B	�B	�B	�'B	�!B	�'B	�!B	�-B	�FB	�RB	�XB	�dB	�wB	�wB	�}B	�}B	B	ÖB	ƨB	ƨB	ĜB	�wB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�HB	�NB	�NB	�TB	�TB	�ZB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B
DB
JB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
\B
bB
bB
hB
hB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
$�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
-B
-B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
E�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
E�B
E�B
E�B
F�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
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
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
|�B
{�B
|�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�B
�B
�%B
�%B
�%B
�%B
�+B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�7B
�1B
�1B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BP�BQ�BQ�BQ�BP�BP�BQ�BP�BP�BP�BS�B`BB�B�BŢB�NB�fB�fB�mB�B�B�B�B�B�B�B�HB�5B��B��BB�B+B)�B49BA�B>wB<jB#�BB��BB��B��B��B��BB��B��B��B��B��B��B�B�sB�ZB�B�HB�;B�)B��B��B��BŢB�!B��B�{B�=B|�BiyB`BBVBL�BG�B<jB33B(�B �B
=B
��B
�HB
ɺB
�9B
�B
��B
�B
v�B
iyB
T�B
O�B
G�B
.B
�B
VB	��B	�B	�`B	�B	��B	��B	�jB	�^B	�?B	��B	��B	��B	�\B	�+B	n�B	ffB	bNB	YB	N�B	B�B	8RB	2-B	)�B	%�B	#�B	 �B	�B	DB	B��B�B�B�B�yB�TB�;B�
B��BŢB��B�qB�^B�FB�?B�3B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�B�-B�jBĜB��B�;B�;B�BB�HB�HB�5B�/B�)B�#B�#B�B�B�B�
B��B�
B�B��B��B��BɺBÖB�FB�B�B�B��B��B��B��B�\B�=B�%B�B� B}�B|�By�By�Bv�Bx�Bt�Bv�Bt�Br�Bv�Br�Bs�Bs�Bt�By�B�Bz�B{�B�B�%B�B�%B�=B�VB�\B�bB�hB��B��B��B��B��B��B��B��B��B�B�B�-B�3B�?B�LB�dB�qB��BBǮB��B��B�B�B�5B�NB�NB�TB�ZB�`B�mB�yB�B�B�B��B��B��B��B��B��B��B	B	1B	JB	PB	VB	VB	bB	�B	�B	!�B	"�B	$�B	$�B	(�B	-B	.B	33B	5?B	7LB	:^B	<jB	?}B	@�B	C�B	F�B	N�B	XB	YB	[#B	`BB	`BB	cTB	dZB	e`B	ffB	jB	n�B	o�B	p�B	t�B	w�B	y�B	}�B	�+B	��B	��B	��B	��B	��B	�B	�B	�'B	�!B	�'B	�!B	�-B	�FB	�RB	�XB	�dB	�wB	�wB	�}B	�}B	B	ÖB	ƨB	ƨB	ĜB	�wB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�HB	�NB	�NB	�TB	�TB	�ZB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B
DB
JB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
\B
bB
bB
hB
hB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
$�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
-B
-B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
E�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
E�B
E�B
E�B
F�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
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
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
|�B
{�B
|�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�B
�B
�%B
�%B
�%B
�%B
�+B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�7B
�1B
�1B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20211130184052  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20211130094250  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20211130094251  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20211130094251  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20211130094251  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20211130094251  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20211130094251  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20211130094251  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20211130094252  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20211130094252                      G�O�G�O�G�O�                JA  ARUP                                                                        20211130095247                      G�O�G�O�G�O�                