CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-05-16T15:45:40Z creation;2022-05-16T15:45:48Z conversion to V3.1      
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
resolution        =���   axis      Z        h  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  op   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  sL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ۀ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ހ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20220516154540  20220516160235  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @�ϼ��k�1   @�Ͼ���R@3�A�7K��d_��-V1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A��A!��A@  A`  A�  A�  A�33A�33A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C�fC�fC
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:�C<�C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��D   D y�D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D   D � D!fD!�fD"  D"y�D#  D#� D$  D$� D%fD%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0�fD1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D?��D@� D@��DA� DBfDB� DC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DY��DZ� D[  D[y�D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Dfy�Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D��D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�3D�@ D�� D�� D�  D�@ D�� D���D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D���D���D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ Dă3D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǃ3D�� D���D�@ DȀ D�� D�  D�<�Dɀ D�� D�  D�<�Dʀ D�� D�  D�@ Dˀ D�� D���D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�C3D΃3D��3D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�C3DӃ3D��3D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�<�D�|�D�� D�3D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D��3D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ D�|�D�� D���D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D�|�D�� D�  D�@ D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@��HA
>A'
>AEp�Aep�A��RA��RA��A��A¸RAҸRA�RA�B\)B	\)B\)B\)B!\)B)\)B1\)B9\)BA\)BH��BQ\)BY\)Ba\)Bi\)Bq\)By\)B��B��B��B��B��B��B��B�z�B��B��B��B��B��B��B��B��B�z�BĮBȮB̮BЮBԮBخBܮB�B�B�B�B��GB��B��B��C W
CW
CW
C=pC=pC
W
CW
CW
CW
CW
CW
CW
CW
CW
Cp�CW
C W
C"W
C$W
C&W
C(W
C*W
C,W
C.W
C0W
C2p�C4W
C6W
C8W
C:p�C<p�C>W
C@W
CBW
CD=pCFW
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
C`W
CbW
CdW
CfW
ChW
CjW
ClW
CnW
CpW
CrW
CtW
CvW
CxW
CzW
C|W
C~W
C�+�C�+�C�8RC�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C��C��C�+�C�+�C�+�C�+�C�+�C��C��C��C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C��C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�8RD �D �]D�D��D�D��D�D��D�D��D�D��D�D��D�D�]D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D)D�)D�D��D�D��D�D��D�D��D�D��D�D��D)D��D�D��D�D��D�D��D�D��D�D��D)D��D�D��D�D��D�D��D �D ��D!)D!�)D"�D"�]D#�D#��D$�D$��D%)D%�)D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.�)D/�D/��D0�D0�)D1)D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=)D=��D>�D>��D?�D?��D@]D@��DA]DA��DB)DB��DC�DC��DD�DD��DE�DE�)DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ�)DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ]DZ��D[�D[�]D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df�]Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm)Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�J�D���D���D�
�D�J�D���D���D�
�D�G�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D��D��D�D�J�D���D���D�
�D�J�D���D�ǮD�
�D�ND���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�ND��D��D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�D�J�D���D���D�
�D�J�D���D�ǮD�
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
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D��D�J�D���D�ǮD��D�J�D���D��D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�ND���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�ND���D���D�
�D�J�D���D���D�
�D�G�D���D���D�
�D�J�D���D���D�
�D�J�D�D���D�
�D�J�DÊ�D���D�
�D�J�DĎD���D�
�D�J�DŊ�D���D�
�D�J�DƊ�D���D�
�D�J�DǎD���D��D�J�DȊ�D���D�
�D�G�DɊ�D���D�
�D�G�Dʊ�D���D�
�D�J�Dˊ�D���D��D�J�D̊�D���D�
�D�J�D͊�D���D�
�D�NDΎD��D�
�D�J�Dϊ�D���D�
�D�J�DЊ�D���D�
�D�J�Dъ�D���D�
�D�J�DҊ�D���D�
�D�NDӎD��D�
�D�J�DԊ�D���D�
�D�J�DՊ�D���D�
�D�G�Dև�D���D�D�J�D׊�D���D�
�D�J�D؊�D���D�
�D�J�Dي�D���D�
�D�J�Dڊ�D���D�
�D�J�Dۊ�D��D�
�D�J�D܊�D���D�
�D�J�D݊�D���D�
�D�J�Dއ�D���D��D�J�Dߊ�D���D�
�D�J�D���D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D��D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D�ǮD�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D���D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�D�J�D���D���D�
�D�J�D��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AЙ�A�`BA�1A�ĜAϮAϧ�Aϡ�Aϟ�Aϛ�Aϗ�AϓuAϏ\AύPAσA�v�A�n�A�S�A�
=A��;AμjAΡ�AΏ\A�hsA�\)A�oA���A�{ÃA��yA�O�A�VAɣ�Aȕ�AƶFA�n�A�hsA���A�?}A�$�A�?}A�x�A�A�1'A�9XA��A���A�;dA�+A��-A�A�Q�A�VA�9XA��A�I�A��A��A���A���A�r�A���A�r�A��!A�&�A��7A���A�  A��`A�oA��+A�;dA���A��wA��A���A��A���A���A� �A��HA�r�A��/A���A�1A��
A�z�A�M�A��A�VA�C�A�1'A� �A���A�VA�t�A��A���A�v�A��A�ȴA���A�-A���A��mA�|�A�hsA��A�A��wA�t�AC�Aw��At9XApjAl��AkG�AiAf��Ac\)A_�A\r�AX�AV(�AT^5AP��AOO�AL��AG�hAFQ�AD��AC|�AB�`ABJAAVA?"�A>^5A=�wA<�RA:�`A:ffA8�A7��A6�HA57LA4ĜA4 �A2��A0��A/+A-��A,ȴA+�7A*�+A)��A(��A'��A'%A&�A&$�A%
=A$v�A#VA!��A!dZA ��A �AdZA�A��AZA�
A�AhsA��A��A�A;dAv�A�wA�yA9XAhsAr�A��A|�At�A�/AffA��A�wA�7AS�AA�`A�A�^AhsA33A
�A
A�yA1A;dA^5AXA�AffAƨA��AƨA33A ��A J@��\@�Ĝ@��@�E�@���@� �@�o@�w@��@��@�V@�|�@�o@�ff@�J@�x�@�D@띲@�E�@��T@��@�C�@��@�n�@�{@���@�@��m@��@�7L@��u@߮@�@އ+@ݙ�@�A�@�t�@�C�@�+@��@���@�ȴ@ڗ�@�{@�hs@�j@ץ�@�E�@�  @�K�@��
@�+@���@�G�@�z�@���@�/@�Ĝ@�z�@ˍP@ʇ+@�p�@�Ĝ@��`@ȓu@���@ǝ�@�K�@��@�~�@���@Ł@�&�@��/@ċD@��y@���@�7L@��j@�9X@��@�=q@���@�X@���@�9X@� �@�A�@���@�+@�@���@�hs@�/@�Ĝ@�(�@���@��
@���@�
=@���@�M�@�@�x�@�?}@���@��u@��@��F@�;d@��@���@���@���@��`@�I�@� �@���@��;@��
@�+@��!@�n�@�{@�G�@��@��@�V@��@���@�Q�@���@���@�~�@��@��^@�X@��@�z�@�j@�Q�@� �@���@��
@��@�;d@�@��@���@�M�@��@�x�@���@��@� �@���@�;d@�
=@���@���@��+@�-@��@�7L@��@��@���@��j@���@�9X@�;d@�"�@��@��\@��+@�~�@��@��^@��7@�hs@�&�@��@���@��9@���@�z�@�  @���@�;d@���@�J@��^@��h@��9@�(�@� �@�b@�  @��@�@�v�@�V@�=q@�@��@��@��h@�X@��@���@�A�@��;@�ƨ@��w@���@�S�@�+@���@���@��!@��\@�ff@�5?@���@��^@��^@���@�`B@�?}@�%@��/@��@�Z@�1@��P@�t�@�dZ@��y@���@�v�@�v�@�ff@�E�@�5?@�=q@�J@���@��@��^@�G�@�&�@��@��@���@�A�@��m@��@���@���@���@��P@�dZ@�@���@�{@��#@���@���@�p�@�X@�G�@�&�@��/@���@�r�@� �@��@��F@�S�@��@���@���@�~�@�{@��h@�X@�7L@�%@��9@��D@��/@��@�`B@��/@��/@���@�%@�V@�V@�%@���@���@�bN@��@��
@�|�@��@��!@�-@��7@���@�Ĝ@���@�;@|�@\)@;d@~�@~ff@}��@}��@}�T@}�@}�@|��@|�@{t�@{C�@z�H@z�\@y��@yx�@y7L@x��@xĜ@x1'@w�@v��@vE�@u��@u�-@u��@u`B@tj@t�@s�
@s��@s33@so@r��@r^5@r=q@r�@q��@qX@p�@o�;@o��@o�P@ol�@o;d@o�@o
=@n�R@nv�@m�@mV@l�j@lI�@kƨ@k�F@kdZ@k"�@ko@ko@k@j�H@j�!@j��@j�\@j~�@j=q@i��@i��@i�@hĜ@h�u@hQ�@g��@g�w@g�w@g�@g��@g\)@g
=@fȴ@fE�@f$�@e��@ep�@e/@eV@eV@d�/@c�@b�!@b~�@bn�@b^5@bJ@a��@a��@aX@`��@_��@_\)@^�y@^v�@^@]O�@\�@\Z@\Z@\9X@[�@Z�@Z��@Z��@Z~�@Z^5@Z-@Y�7@Y&�@Y%@Y%@X�`@X�9@XA�@W�;@W�@W�@V�R@V��@Vv�@V{@U�@U��@UV@T�D@Sƨ@S�@SdZ@So@R��@R��@R��@R�@Q��@Qhs@P��@PbN@O��@O\)@O
=@Nv�@M��@M/@Lj@K��@KdZ@K@J��@J��@Jn�@JJ@I�@I��@H��@H��@H�@H�@HbN@G�w@GK�@G+@F�@F��@F$�@E��@E�h@E?}@D��@D�@C�F@B�@B~�@Bn�@BM�@A��@A%@@��@@bN@@A�@@ �@?�@?��@?�w@?�@?��@?|�@>�y@>��@>5?@=�T@=@=�-@=��@=�h@=�@=O�@=V@<��@<Z@<1@;�@:�@:~�@:^5@:J@9G�@97L@9�@9%@8Ĝ@8�9@8��@8r�@8A�@8 �@7�;@7��@7\)@7+@6��@6ȴ@6��@6ff@5�@5��@5��@5p�@5/@4�/@4�j@4I�@3�m@3��@333@2�H@2��@2�!@2�!@2��@2^5@2�@1��@1�^@1��@1�7@1x�@1G�@1&�@1�@0��@0Ĝ@0��@0�u@0�@0bN@0 �@/�;@/��@.��@.V@.@-?}@-V@,�j@,I�@+�
@+C�@+"�@+@*��@*~�@*M�@*�@)�#@)��@)�7@)7L@(Ĝ@(b@'�P@'\)@&�y@&��@&V@%�@%�T@%�T@%�T@%��@%�h@%`B@%?}@$��@$��@#��@#ƨ@#dZ@#"�@#o@#o@"�@"M�@"J@!�#@!�7@!X@!G�@!7L@ ��@ ��@ Q�@   @�@�P@l�@K�@�@�@��@5?@{@�h@`B@?}@�@V@��@�@I�@1@t�@�@��@n�@�@��@��@7L@��@�9@bN@b@�;@�@+@�@�+@ff@E�@$�@�T@@`B@V@�/@�@z�@Z@9X@�@�m@�@S�@C�@33@33@o@��@~�@^5@=q@-@-@��@�@�@�^@�7@�@��@Ĝ@�9@Q�@�;@�w@�w@�w@��@|�@l�@\)@;d@�@�R@5?@�@�T@��@�h@/@��@�/@��@�j@�j@�j@�j@�j@�j@�@��@��@�D@z�@Z@I�@(�@�@�@1@�m@�
@ƨ@ƨ@�
@�m@�
@ƨ@�F@��@��@��@t�@C�@"�@o@@
�@
��@
�\@
n�@
-@
J@	�@	�#@	�^@	hs@	X@	X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AЙ�A�`BA�1A�ĜAϮAϧ�Aϡ�Aϟ�Aϛ�Aϗ�AϓuAϏ\AύPAσA�v�A�n�A�S�A�
=A��;AμjAΡ�AΏ\A�hsA�\)A�oA���A�{ÃA��yA�O�A�VAɣ�Aȕ�AƶFA�n�A�hsA���A�?}A�$�A�?}A�x�A�A�1'A�9XA��A���A�;dA�+A��-A�A�Q�A�VA�9XA��A�I�A��A��A���A���A�r�A���A�r�A��!A�&�A��7A���A�  A��`A�oA��+A�;dA���A��wA��A���A��A���A���A� �A��HA�r�A��/A���A�1A��
A�z�A�M�A��A�VA�C�A�1'A� �A���A�VA�t�A��A���A�v�A��A�ȴA���A�-A���A��mA�|�A�hsA��A�A��wA�t�AC�Aw��At9XApjAl��AkG�AiAf��Ac\)A_�A\r�AX�AV(�AT^5AP��AOO�AL��AG�hAFQ�AD��AC|�AB�`ABJAAVA?"�A>^5A=�wA<�RA:�`A:ffA8�A7��A6�HA57LA4ĜA4 �A2��A0��A/+A-��A,ȴA+�7A*�+A)��A(��A'��A'%A&�A&$�A%
=A$v�A#VA!��A!dZA ��A �AdZA�A��AZA�
A�AhsA��A��A�A;dAv�A�wA�yA9XAhsAr�A��A|�At�A�/AffA��A�wA�7AS�AA�`A�A�^AhsA33A
�A
A�yA1A;dA^5AXA�AffAƨA��AƨA33A ��A J@��\@�Ĝ@��@�E�@���@� �@�o@�w@��@��@�V@�|�@�o@�ff@�J@�x�@�D@띲@�E�@��T@��@�C�@��@�n�@�{@���@�@��m@��@�7L@��u@߮@�@އ+@ݙ�@�A�@�t�@�C�@�+@��@���@�ȴ@ڗ�@�{@�hs@�j@ץ�@�E�@�  @�K�@��
@�+@���@�G�@�z�@���@�/@�Ĝ@�z�@ˍP@ʇ+@�p�@�Ĝ@��`@ȓu@���@ǝ�@�K�@��@�~�@���@Ł@�&�@��/@ċD@��y@���@�7L@��j@�9X@��@�=q@���@�X@���@�9X@� �@�A�@���@�+@�@���@�hs@�/@�Ĝ@�(�@���@��
@���@�
=@���@�M�@�@�x�@�?}@���@��u@��@��F@�;d@��@���@���@���@��`@�I�@� �@���@��;@��
@�+@��!@�n�@�{@�G�@��@��@�V@��@���@�Q�@���@���@�~�@��@��^@�X@��@�z�@�j@�Q�@� �@���@��
@��@�;d@�@��@���@�M�@��@�x�@���@��@� �@���@�;d@�
=@���@���@��+@�-@��@�7L@��@��@���@��j@���@�9X@�;d@�"�@��@��\@��+@�~�@��@��^@��7@�hs@�&�@��@���@��9@���@�z�@�  @���@�;d@���@�J@��^@��h@��9@�(�@� �@�b@�  @��@�@�v�@�V@�=q@�@��@��@��h@�X@��@���@�A�@��;@�ƨ@��w@���@�S�@�+@���@���@��!@��\@�ff@�5?@���@��^@��^@���@�`B@�?}@�%@��/@��@�Z@�1@��P@�t�@�dZ@��y@���@�v�@�v�@�ff@�E�@�5?@�=q@�J@���@��@��^@�G�@�&�@��@��@���@�A�@��m@��@���@���@���@��P@�dZ@�@���@�{@��#@���@���@�p�@�X@�G�@�&�@��/@���@�r�@� �@��@��F@�S�@��@���@���@�~�@�{@��h@�X@�7L@�%@��9@��D@��/@��@�`B@��/@��/@���@�%@�V@�V@�%@���@���@�bN@��@��
@�|�@��@��!@�-@��7@���@�Ĝ@���@�;@|�@\)@;d@~�@~ff@}��@}��@}�T@}�@}�@|��@|�@{t�@{C�@z�H@z�\@y��@yx�@y7L@x��@xĜ@x1'@w�@v��@vE�@u��@u�-@u��@u`B@tj@t�@s�
@s��@s33@so@r��@r^5@r=q@r�@q��@qX@p�@o�;@o��@o�P@ol�@o;d@o�@o
=@n�R@nv�@m�@mV@l�j@lI�@kƨ@k�F@kdZ@k"�@ko@ko@k@j�H@j�!@j��@j�\@j~�@j=q@i��@i��@i�@hĜ@h�u@hQ�@g��@g�w@g�w@g�@g��@g\)@g
=@fȴ@fE�@f$�@e��@ep�@e/@eV@eV@d�/@c�@b�!@b~�@bn�@b^5@bJ@a��@a��@aX@`��@_��@_\)@^�y@^v�@^@]O�@\�@\Z@\Z@\9X@[�@Z�@Z��@Z��@Z~�@Z^5@Z-@Y�7@Y&�@Y%@Y%@X�`@X�9@XA�@W�;@W�@W�@V�R@V��@Vv�@V{@U�@U��@UV@T�D@Sƨ@S�@SdZ@So@R��@R��@R��@R�@Q��@Qhs@P��@PbN@O��@O\)@O
=@Nv�@M��@M/@Lj@K��@KdZ@K@J��@J��@Jn�@JJ@I�@I��@H��@H��@H�@H�@HbN@G�w@GK�@G+@F�@F��@F$�@E��@E�h@E?}@D��@D�@C�F@B�@B~�@Bn�@BM�@A��@A%@@��@@bN@@A�@@ �@?�@?��@?�w@?�@?��@?|�@>�y@>��@>5?@=�T@=@=�-@=��@=�h@=�@=O�@=V@<��@<Z@<1@;�@:�@:~�@:^5@:J@9G�@97L@9�@9%@8Ĝ@8�9@8��@8r�@8A�@8 �@7�;@7��@7\)@7+@6��@6ȴ@6��@6ff@5�@5��@5��@5p�@5/@4�/@4�j@4I�@3�m@3��@333@2�H@2��@2�!@2�!@2��@2^5@2�@1��@1�^@1��@1�7@1x�@1G�@1&�@1�@0��@0Ĝ@0��@0�u@0�@0bN@0 �@/�;@/��@.��@.V@.@-?}@-V@,�j@,I�@+�
@+C�@+"�@+@*��@*~�@*M�@*�@)�#@)��@)�7@)7L@(Ĝ@(b@'�P@'\)@&�y@&��@&V@%�@%�T@%�T@%�T@%��@%�h@%`B@%?}@$��@$��@#��@#ƨ@#dZ@#"�@#o@#o@"�@"M�@"J@!�#@!�7@!X@!G�@!7L@ ��@ ��@ Q�@   @�@�P@l�@K�@�@�@��@5?@{@�h@`B@?}@�@V@��@�@I�@1@t�@�@��@n�@�@��@��@7L@��@�9@bN@b@�;@�@+@�@�+@ff@E�@$�@�T@@`B@V@�/@�@z�@Z@9X@�@�m@�@S�@C�@33@33@o@��@~�@^5@=q@-@-@��@�@�@�^@�7@�@��@Ĝ@�9@Q�@�;@�w@�w@�w@��@|�@l�@\)@;d@�@�R@5?@�@�T@��@�h@/@��@�/@��@�j@�j@�j@�j@�j@�j@�@��@��@�D@z�@Z@I�@(�@�@�@1@�m@�
@ƨ@ƨ@�
@�m@�
@ƨ@�F@��@��@��@t�@C�@"�@o@@
�@
��@
�\@
n�@
-@
J@	�@	�#@	�^@	hs@	X@	X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bl�Bk�BjBiyBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBgmBffBffBiyBl�Bn�Bq�Bw�B�=B�oB�qB��B1B'�B-B+B,B'�B �B\BBbB)�B0!B#�BoBPBhBoB�B�B!�B)�B=qBF�BE�BI�BK�BK�BN�BP�BP�BL�BL�BI�BF�BE�BB�BA�B@�B?}B>wB9XB5?B)�B�B
=BB��B�B�BB��BB�jB�XB�RB�?B�'B�B��B�\B{�Bk�B_;BD�B5?B%�B�B1BB
��B
�B
�TB
�;B
�B
��B
��B
�'B
��B
�uB
�PB
|�B
r�B
hsB
dZB
]/B
M�B
+B
	7B	��B	�B	��B	ŢB	�?B	��B	�=B	v�B	`BB	N�B	F�B	7LB	(�B	�B	PB	  B��B��B�B�B�B�sB�`B�BB�/B��B��B��B��B��B��BȴBƨBÖB�wB�'B�-B�FB�XB�XB�XB�FB�9B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�hB�hB�bB�VB�JB�DB�JB�VB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�B�B�B�B�B�B�B�'B�!B�9B�3B�3B�9B�9B�RB�RB�XB�RB�qB�qB�}B��B��BBŢB��B��B��B��B��B��B��B�B�
B�
B�/B�;B�)B�)B�`B�B�B�B�B�B�B��B��B��B	  B	B	B	
=B	VB	bB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	$�B	%�B	&�B	,B	49B	49B	49B	7LB	7LB	8RB	8RB	:^B	;dB	;dB	=qB	A�B	C�B	E�B	H�B	I�B	J�B	L�B	N�B	Q�B	R�B	VB	XB	YB	YB	_;B	dZB	k�B	m�B	q�B	x�B	y�B	~�B	�B	�B	�B	�1B	�7B	�7B	�7B	�7B	�DB	�JB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�9B	�?B	�?B	�LB	�RB	�jB	�qB	�qB	�wB	�}B	�}B	��B	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B

=B

=B
DB
DB
DB
JB
JB
JB
JB
VB
VB
\B
\B
bB
bB
bB
bB
hB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
%�B
%�B
#�B
"�B
!�B
!�B
"�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
+B
+B
+B
+B
,B
,B
,B
-B
-B
-B
-B
-B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
2-B
2-B
49B
5?B
5?B
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
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
>wB
>wB
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
cTB
bNB
cTB
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
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
gmB
hsB
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
iyB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
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
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
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
x�B
x�B
x�B
x�B
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
z�B
{�B
{�B
{�B
|�B
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
�B
�B
�B
�B
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
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�=B
�=B
�=B
�DB
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
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bl�Bk�BjBiyBhsBhsBhsBhsBhsBhsBhsBhsBhsBhsBgmBffBffBiyBl�Bn�Bq�Bw�B�=B�oB�qB��B1B'�B-B+B,B'�B �B\BBbB)�B0!B#�BoBPBhBoB�B�B!�B)�B=qBF�BE�BI�BK�BK�BN�BP�BP�BL�BL�BI�BF�BE�BB�BA�B@�B?}B>wB9XB5?B)�B�B
=BB��B�B�BB��BB�jB�XB�RB�?B�'B�B��B�\B{�Bk�B_;BD�B5?B%�B�B1BB
��B
�B
�TB
�;B
�B
��B
��B
�'B
��B
�uB
�PB
|�B
r�B
hsB
dZB
]/B
M�B
+B
	7B	��B	�B	��B	ŢB	�?B	��B	�=B	v�B	`BB	N�B	F�B	7LB	(�B	�B	PB	  B��B��B�B�B�B�sB�`B�BB�/B��B��B��B��B��B��BȴBƨBÖB�wB�'B�-B�FB�XB�XB�XB�FB�9B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�hB�hB�bB�VB�JB�DB�JB�VB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�B�B�B�B�B�B�B�'B�!B�9B�3B�3B�9B�9B�RB�RB�XB�RB�qB�qB�}B��B��BBŢB��B��B��B��B��B��B��B�B�
B�
B�/B�;B�)B�)B�`B�B�B�B�B�B�B��B��B��B	  B	B	B	
=B	VB	bB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	$�B	%�B	&�B	,B	49B	49B	49B	7LB	7LB	8RB	8RB	:^B	;dB	;dB	=qB	A�B	C�B	E�B	H�B	I�B	J�B	L�B	N�B	Q�B	R�B	VB	XB	YB	YB	_;B	dZB	k�B	m�B	q�B	x�B	y�B	~�B	�B	�B	�B	�1B	�7B	�7B	�7B	�7B	�DB	�JB	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�9B	�?B	�?B	�LB	�RB	�jB	�qB	�qB	�wB	�}B	�}B	��B	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B

=B

=B
DB
DB
DB
JB
JB
JB
JB
VB
VB
\B
\B
bB
bB
bB
bB
hB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
%�B
%�B
#�B
"�B
!�B
!�B
"�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
+B
+B
+B
+B
,B
,B
,B
-B
-B
-B
-B
-B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
2-B
2-B
49B
5?B
5?B
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
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
>wB
>wB
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
cTB
bNB
cTB
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
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
gmB
hsB
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
iyB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
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
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
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
x�B
x�B
x�B
x�B
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
z�B
{�B
{�B
{�B
|�B
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
�B
�B
�B
�B
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
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�=B
�=B
�=B
�DB
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
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20220517004206  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220516154540  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220516154545  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20220516154545  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20220516154546  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20220516154546  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20220516154547  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20220516154547  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20220516154548  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20220516154548                      G�O�G�O�G�O�                JA  ARUP                                                                        20220516160235                      G�O�G�O�G�O�                