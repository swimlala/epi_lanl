CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-01-04T10:01:06Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220104100106  20220104100106  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @ٯT$8+N1   @ٯT��9�@<�z�G��c�z�G�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   B   F   @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�Q�@�AA9AYAyA��HA��HA��HA��HA��HA��HA��HA��HBp�Bp�Bp�Bp�B&p�B.p�B6p�B>p�BFp�BNp�BVp�B^p�Bfp�Bnp�Bvp�B~p�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC�)C�)C�)C�)C	�)C�)C��C�)C�)C�)C�)C�)C�)C�)C�)C�)C!�)C#�)C%�)C'�)C)�)C+�)C-�)C/�)C1�)C3�)C5�)C7�)C9�)C;�)C=�)C?�)CA�)CC�)CE�)CG�)CI�)CK�)CM�)CO�)CQ�)CS�)CU�)CW�)CY�)C[�)C]�)C_�)Ca�)Cc�)Ce�)Cg�)Ci�)Ck�)Cm�)Co�)Cq�)Cs�)Cu�)Cw�)Cy�)C{�)C}�)C�)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��C��C��C��C��C��C��C��C��C��C��C��C��C��GC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D g
D �
Dg
D�
Dg
D�
Dg
D�Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
D	g
D	�
D
g
D
�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
Dg
D�
D g
D �
D!g
D!�
D"g
D"�
D#g
D#�
D$g
D$�
D%g
D%�
D&g
D&�
D'g
D'�
D(g
D(�
D)g
D)�
D*g
D*�
D+g
D+�
D,g
D,�
D-g
D-�
D.g
D.�
D/g
D/�
D0g
D0�
D1g
D1�
D2g
D2�
D3g
D3�
D4g
D4�
D5g
D5�
D6g
D6�
D7g
D7�
D8g
D8�
D9g
D9�
D:g
D:�
D;g
D;�
D<g
D<�
D=g
D=�
D>g
D>�
D?g
D?�
D@g
D@�
DAg
DA�
DBg
DB�
DCg
DC�
DDg
DD�
DEg
DE�
DFg
DF�
DGg
DG�
DHg
DH�
DIg
DI�
DJg
DJ�
DKg
DK�
DLg
DL�
DMg
DM�
DNg
DN�
DOg
DO�
DPg
DP�
DQg
DQ�
DRg
DR�
DSg
DS�
DTg
DT�
DUg
DU�
DVg
DV�
DWg
DW�
DXg
DX�
DYg
DY�
DZg
DZ�
D[g
D[�
D\g
D\�
D]g
D]�
D^g
D^�
D_g
D_�
D`g
D`�
Dag
Da�
Dbg
Db�
Dcg
Dc�
Ddg
Dd�
Deg
De�
Dfg
Df�
Dgg
Dg�
Dhg
Dh�
Dig
Di�
Djg
Dj�
Dkg
Dk�
Dlg
Dl�
Dmg
Dm�
Dng
Dn�
Dog
Do�
Dpg
Dp�
Dqg
Dq�
Drg
Dr�
Dsg
Ds�
Dtg
Dt�
Dug
Du�
Dvg
Dv�
Dwg
Dw�
Dxg
Dx�
Dyg
Dy�
Dzg
Dz�
D{g
D{�
D|g
D|�
D}g
D}�
D~g
D~�
Dg
D�
D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D���D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D��RD��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D³�D��D�3�D�s�Dó�D��D�3�D�s�Dĳ�D��D�3�D�s�Dų�D��D�3�D�s�DƳ�D��D�3�D�s�Dǳ�D��D�3�D�s�Dȳ�D��D�3�D�s�Dɳ�D��D�3�D�s�Dʳ�D��D�3�D�s�D˳�D��D�3�D�s�D̳�D��D�3�D�s�Dͳ�D��D�3�D�s�Dγ�D��D�3�D�s�Dϳ�D��D�3�D�s�Dг�D��D�3�D�s�Dѳ�D��D�3�D�s�Dҳ�D��D�3�D�s�Dӳ�D��D�3�D�s�DԳ�D��D�3�D�s�Dճ�D��D�3�D�s�Dֳ�D��D�3�D�s�D׳�D��D�3�D�s�Dس�D��D�3�D�s�Dٳ�D��D�3�D�s�Dڳ�D��D�3�D�s�D۳�D��D�3�D�s�Dܳ�D��D�3�D�s�Dݳ�D��D�3�D�s�D޳�D��D�3�D�s�D߳�D��D�3�D�s�D೅D��D�3�D�s�D᳅D��D�3�D�s�DⳅD��D�3�D�s�D㳅D��D�3�D�s�D䳅D��D�3�D�s�D峅D��D�3�D�s�D泅D��D�3�D�s�D糅D��D�3�D�s�D賅D��D�3�D�s�D鳅D��D�3�D�s�D곅D��D�3�D�s�D볅D��D�3�D�s�D쳅D��D�3�D�s�D���D��D�3�D�s�DD��D�3�D�s�DﳅD��D�3�D�s�D�D��D�3�D�s�D�D��D�3�D�s�D�D��D�3�D�s�D�D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���D��D�3�D�s�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A�I�A�S�A�S�A�M�A�M�A�M�A�M�A�G�A�E�A�I�A�VA�t�A��A�~�A�z�A�r�A�n�A�l�A�hsA�dZA�^5A�XA�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�VA�VA�VA�S�A�VA�XA�VA�VA�S�A�S�A�M�A�A�A�-A��mA�l�A��#A�bNA��+A�VA�bNA���A��A��/A�1A��\A�C�A�ƨA��A�M�A�{A���A�?}A��jA�-A�G�A�Q�A�A�O�A��7A�`BA���A� �A�%A�S�A�
=A�~�A��`A�K�A��9A�x�A�5?A��yA��yA�z�A���A�XA���A�ȴA�E�A���A�Q�A�A}C�A{Ay��Ax�Aw7LAv^5Au7LAs��Aq|�Ao+AmC�AkG�AkVAj5?Ai��Ai�Ai33Ah�`Ah$�AfM�AdVA`�A^VA]�7A[`BAZ5?AY�AXZAW�AU�AU7LAT(�AS33AR �AQ�AQl�AQAO�;AMl�AL�9AL-AK�wAKK�AJ�AI�AG�AE�AE��AE/AD�!ADbAB�\AA�^A@bNA>�\A>A�A>1'A=�
A=��A=C�A<��A;��A:��A9S�A8n�A8A7�
A7��A7\)A7
=A6�RA6A�A5��A3�mA2=qA17LA0��A/A/�A.�HA.�A-&�A,$�A+l�A+`BA*�A(��A'��A'��A'O�A&�!A%l�A$�A#A"�!A!�A!"�A jA�A�`A�A"�A�;AK�A�AbAA1'AƨAx�A�A9XA�mAhsA�A�!AI�A  AhsA��AƨAQ�A��AoAffAE�AbA�PA5?A��AC�A
��A	��A�\AM�A�AVA�A��A�A�9A(�AS�A%A��A  �@�33@�ȴ@�5?@���@�v�@�X@���@�l�@�E�@��9@�@��@�/@�@�A�@@�K�@�!@�M�@���@�|�@��@�7L@�\)@�h@�9X@�M�@�7L@�Ĝ@�z�@� �@۾w@��@�@�G�@��m@��@ղ-@��y@�{@��/@�33@�M�@�p�@�X@̬@��H@ɑh@��`@ǅ@�5?@�%@�r�@�;d@�5?@�O�@��@�9X@�ȴ@�V@�I�@��@��@�-@�x�@�V@��D@�b@�S�@�33@�
=@��@���@�=q@��T@� �@��@�~�@�J@�X@���@�I�@��@��@�5?@��^@�Z@��m@��F@���@�S�@���@�@��@���@���@���@��T@�x�@�%@��D@�A�@��@���@��
@��F@���@�l�@�C�@���@�M�@��@�&�@�Q�@� �@���@�;d@��y@��R@��+@�n�@���@�O�@��@��@��D@��@�ƨ@��P@�33@��+@��-@���@�A�@�dZ@���@�ȴ@�M�@��#@�p�@��@�Ĝ@�z�@�b@��
@��@�l�@��@���@�G�@��@�%@�Ĝ@�I�@�1@�K�@��!@�V@�=q@�E�@�=q@��#@��-@��@��9@���@��u@��D@�z�@�Z@��m@�S�@�+@��@�"�@�o@�n�@�p�@���@�9X@��@�33@��@�o@�ȴ@�ff@�5?@�$�@�J@���@�V@��/@��j@��u@�I�@� �@�1@��m@��w@���@�33@�n�@�@��h@�O�@�%@���@�r�@�Z@�I�@�9X@�9X@�9X@�1'@�(�@� �@��@��@�;@\)@~v�@~@}��@}��@}�@}?}@|��@|1@{��@{ƨ@{dZ@{@z�\@zJ@y��@yhs@x�9@xbN@w�;@w�P@w�P@wl�@w
=@v��@v�+@vE�@v$�@u�T@uV@t�@t�D@tz�@tz�@tz�@tj@tZ@t(�@s�
@s��@s��@s"�@r�H@r~�@q��@qG�@pĜ@pQ�@pA�@p1'@pb@o�@o|�@ol�@o+@n�@m��@m`B@l��@l�@k@j�\@j^5@j-@i��@i&�@h�@hQ�@h �@g|�@g�@f��@f�R@fE�@f{@ep�@e�@d��@d�@d�@d�@cƨ@c��@c�@ct�@c33@b�!@b�\@b=q@a7L@a%@`�9@`r�@` �@_��@_�w@_��@_;d@^��@^ff@^5?@]�T@]�h@]`B@]`B@]?}@]V@\�@\1@[��@[t�@[dZ@[C�@[33@[o@Z��@Z~�@Zn�@Z=q@ZJ@Yhs@Y�@Y%@XĜ@XQ�@X �@X  @W��@W�w@W��@W|�@W+@V�@V��@VE�@V5?@V@U�-@U�h@Up�@U`B@T�/@T��@TI�@S��@S33@S@R��@R~�@R^5@RM�@RJ@Q��@QX@P��@P�u@Pb@O��@O�w@N��@N�@Nv�@Nff@NE�@N@M�-@M�h@L��@L�D@L�@K��@Kƨ@K��@Ko@J��@J�!@J~�@JJ@I�7@IG�@I&�@H��@H�@G�@G�@GK�@F�y@F�R@F��@F�+@F�+@FE�@F$�@F$�@F$�@E�@E@Ep�@EO�@E�@D��@D�/@D��@DI�@C��@C��@Ct�@C33@B��@B�\@B~�@BM�@B�@BJ@A��@A&�@@��@@Q�@@ �@?�w@?�P@?\)@?�@>��@>��@>$�@=��@=`B@<��@<�@<��@<�D@<z�@<I�@<�@;�F@;"�@:�@:�!@:�\@:~�@:~�@:~�@:^5@9x�@9&�@8��@8�9@8A�@8 �@8 �@8b@8  @7�@7|�@7K�@7+@7�@6��@6ȴ@6��@6��@6��@6�+@6ff@65?@5�@5��@5��@5p�@5`B@5O�@5?}@4��@4�j@4��@4z�@4Z@41@3�
@3��@3dZ@3o@2��@2��@2n�@2=q@2J@1�@1�#@1��@1�^@1hs@0��@0�@0Q�@/��@/�P@/l�@/l�@/\)@/K�@/;d@.��@.ȴ@.��@.V@.$�@-�T@-@-O�@-V@,�j@,Z@,1@+��@+ƨ@+��@+dZ@+C�@+"�@*=q@)�@)�@)�@)��@)��@)x�@)X@(��@(Ĝ@(�9@(�9@(��@(�@(�@(r�@(bN@(A�@(1'@( �@(  @'��@'
=@&�R@&��@&��@&��@&��@&��@&��@&��@&��@&v�@&V@%��@%��@%O�@%?}@%V@$�/@$�@$��@$z�@$�@$1@$1@#��@#��@#t�@#S�@#C�@#"�@#o@"�H@"�\@"~�@"~�@"~�@"^5@"�@"J@!��@!�@!�@!�#@!��@!hs@!&�@ ��@ A�@�w@l�@\)@;d@
=@�y@ȴ@ff@�T@?}@�@��@��@I�@1@�F@dZ@�H@��@�!@��@~�@M�@�#@�^@�^@��@��@�7@hs@��@�@A�@1'@  @�@�;@�;@�;@�@l�@+@�y@�R@v�@$�@@p�@O�@�@�@�j@��@��@j@I�@9X@(�@�@1@�m@��@t�@t�@t�@dZ@33@33@�@��@=q@-@�@�#@x�@hs@7L@%@�9@�@bN@Q�@b@��@��@l�@+@�R@��@V@{@@�-@��@�@/@��@�@�/@�@j@(�@1@1@��@�m@ƨ@��@C�@"�@"�@@
��@
��@
�\@
M�@
-@
J@	�@	��@	��@	�7@	x�@	G�@	&�@�`@��@Ĝ@�@A�@b@��@|�@;d@�@��@��@�@�+@V@V@E�@5?@5?@5?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�K�A�I�A�S�A�S�A�M�A�M�A�M�A�M�A�G�A�E�A�I�A�VA�t�A��A�~�A�z�A�r�A�n�A�l�A�hsA�dZA�^5A�XA�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�VA�VA�VA�S�A�VA�XA�VA�VA�S�A�S�A�M�A�A�A�-A��mA�l�A��#A�bNA��+A�VA�bNA���A��A��/A�1A��\A�C�A�ƨA��A�M�A�{A���A�?}A��jA�-A�G�A�Q�A�A�O�A��7A�`BA���A� �A�%A�S�A�
=A�~�A��`A�K�A��9A�x�A�5?A��yA��yA�z�A���A�XA���A�ȴA�E�A���A�Q�A�A}C�A{Ay��Ax�Aw7LAv^5Au7LAs��Aq|�Ao+AmC�AkG�AkVAj5?Ai��Ai�Ai33Ah�`Ah$�AfM�AdVA`�A^VA]�7A[`BAZ5?AY�AXZAW�AU�AU7LAT(�AS33AR �AQ�AQl�AQAO�;AMl�AL�9AL-AK�wAKK�AJ�AI�AG�AE�AE��AE/AD�!ADbAB�\AA�^A@bNA>�\A>A�A>1'A=�
A=��A=C�A<��A;��A:��A9S�A8n�A8A7�
A7��A7\)A7
=A6�RA6A�A5��A3�mA2=qA17LA0��A/A/�A.�HA.�A-&�A,$�A+l�A+`BA*�A(��A'��A'��A'O�A&�!A%l�A$�A#A"�!A!�A!"�A jA�A�`A�A"�A�;AK�A�AbAA1'AƨAx�A�A9XA�mAhsA�A�!AI�A  AhsA��AƨAQ�A��AoAffAE�AbA�PA5?A��AC�A
��A	��A�\AM�A�AVA�A��A�A�9A(�AS�A%A��A  �@�33@�ȴ@�5?@���@�v�@�X@���@�l�@�E�@��9@�@��@�/@�@�A�@@�K�@�!@�M�@���@�|�@��@�7L@�\)@�h@�9X@�M�@�7L@�Ĝ@�z�@� �@۾w@��@�@�G�@��m@��@ղ-@��y@�{@��/@�33@�M�@�p�@�X@̬@��H@ɑh@��`@ǅ@�5?@�%@�r�@�;d@�5?@�O�@��@�9X@�ȴ@�V@�I�@��@��@�-@�x�@�V@��D@�b@�S�@�33@�
=@��@���@�=q@��T@� �@��@�~�@�J@�X@���@�I�@��@��@�5?@��^@�Z@��m@��F@���@�S�@���@�@��@���@���@���@��T@�x�@�%@��D@�A�@��@���@��
@��F@���@�l�@�C�@���@�M�@��@�&�@�Q�@� �@���@�;d@��y@��R@��+@�n�@���@�O�@��@��@��D@��@�ƨ@��P@�33@��+@��-@���@�A�@�dZ@���@�ȴ@�M�@��#@�p�@��@�Ĝ@�z�@�b@��
@��@�l�@��@���@�G�@��@�%@�Ĝ@�I�@�1@�K�@��!@�V@�=q@�E�@�=q@��#@��-@��@��9@���@��u@��D@�z�@�Z@��m@�S�@�+@��@�"�@�o@�n�@�p�@���@�9X@��@�33@��@�o@�ȴ@�ff@�5?@�$�@�J@���@�V@��/@��j@��u@�I�@� �@�1@��m@��w@���@�33@�n�@�@��h@�O�@�%@���@�r�@�Z@�I�@�9X@�9X@�9X@�1'@�(�@� �@��@��@�;@\)@~v�@~@}��@}��@}�@}?}@|��@|1@{��@{ƨ@{dZ@{@z�\@zJ@y��@yhs@x�9@xbN@w�;@w�P@w�P@wl�@w
=@v��@v�+@vE�@v$�@u�T@uV@t�@t�D@tz�@tz�@tz�@tj@tZ@t(�@s�
@s��@s��@s"�@r�H@r~�@q��@qG�@pĜ@pQ�@pA�@p1'@pb@o�@o|�@ol�@o+@n�@m��@m`B@l��@l�@k@j�\@j^5@j-@i��@i&�@h�@hQ�@h �@g|�@g�@f��@f�R@fE�@f{@ep�@e�@d��@d�@d�@d�@cƨ@c��@c�@ct�@c33@b�!@b�\@b=q@a7L@a%@`�9@`r�@` �@_��@_�w@_��@_;d@^��@^ff@^5?@]�T@]�h@]`B@]`B@]?}@]V@\�@\1@[��@[t�@[dZ@[C�@[33@[o@Z��@Z~�@Zn�@Z=q@ZJ@Yhs@Y�@Y%@XĜ@XQ�@X �@X  @W��@W�w@W��@W|�@W+@V�@V��@VE�@V5?@V@U�-@U�h@Up�@U`B@T�/@T��@TI�@S��@S33@S@R��@R~�@R^5@RM�@RJ@Q��@QX@P��@P�u@Pb@O��@O�w@N��@N�@Nv�@Nff@NE�@N@M�-@M�h@L��@L�D@L�@K��@Kƨ@K��@Ko@J��@J�!@J~�@JJ@I�7@IG�@I&�@H��@H�@G�@G�@GK�@F�y@F�R@F��@F�+@F�+@FE�@F$�@F$�@F$�@E�@E@Ep�@EO�@E�@D��@D�/@D��@DI�@C��@C��@Ct�@C33@B��@B�\@B~�@BM�@B�@BJ@A��@A&�@@��@@Q�@@ �@?�w@?�P@?\)@?�@>��@>��@>$�@=��@=`B@<��@<�@<��@<�D@<z�@<I�@<�@;�F@;"�@:�@:�!@:�\@:~�@:~�@:~�@:^5@9x�@9&�@8��@8�9@8A�@8 �@8 �@8b@8  @7�@7|�@7K�@7+@7�@6��@6ȴ@6��@6��@6��@6�+@6ff@65?@5�@5��@5��@5p�@5`B@5O�@5?}@4��@4�j@4��@4z�@4Z@41@3�
@3��@3dZ@3o@2��@2��@2n�@2=q@2J@1�@1�#@1��@1�^@1hs@0��@0�@0Q�@/��@/�P@/l�@/l�@/\)@/K�@/;d@.��@.ȴ@.��@.V@.$�@-�T@-@-O�@-V@,�j@,Z@,1@+��@+ƨ@+��@+dZ@+C�@+"�@*=q@)�@)�@)�@)��@)��@)x�@)X@(��@(Ĝ@(�9@(�9@(��@(�@(�@(r�@(bN@(A�@(1'@( �@(  @'��@'
=@&�R@&��@&��@&��@&��@&��@&��@&��@&��@&v�@&V@%��@%��@%O�@%?}@%V@$�/@$�@$��@$z�@$�@$1@$1@#��@#��@#t�@#S�@#C�@#"�@#o@"�H@"�\@"~�@"~�@"~�@"^5@"�@"J@!��@!�@!�@!�#@!��@!hs@!&�@ ��@ A�@�w@l�@\)@;d@
=@�y@ȴ@ff@�T@?}@�@��@��@I�@1@�F@dZ@�H@��@�!@��@~�@M�@�#@�^@�^@��@��@�7@hs@��@�@A�@1'@  @�@�;@�;@�;@�@l�@+@�y@�R@v�@$�@@p�@O�@�@�@�j@��@��@j@I�@9X@(�@�@1@�m@��@t�@t�@t�@dZ@33@33@�@��@=q@-@�@�#@x�@hs@7L@%@�9@�@bN@Q�@b@��@��@l�@+@�R@��@V@{@@�-@��@�@/@��@�@�/@�@j@(�@1@1@��@�m@ƨ@��@C�@"�@"�@@
��@
��@
�\@
M�@
-@
J@	�@	��@	��@	�7@	x�@	G�@	&�@�`@��@Ĝ@�@A�@b@��@|�@;d@�@��@��@�@�+@V@V@E�@5?@5?@5?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�7B�7B�uB��B��B��B��B��B�-BȴB��B��B�)B�TB�TB�ZB�fB�`B�TB�TB�TB�TB�TB�TB�TB�TB�TB�fB�TB�TB�TB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�`B�ZB�ZB�ZB�ZB�ZB�NB�;B�
B�9B_;B�B�bB�=B�Bs�BdZB`BBXBN�BK�BD�B?}B;dB8RB1'B+B#�B�BB�B�`B�)B��B�^B�B��B�%Bx�Br�BiyBR�BG�B>wB-B%�B!�BuB	7B  B�B�;B��B��BĜB�jB�?B��B��B�oB�=B�Bz�Bt�BiyB]/BL�BF�B;dB9XB7LB33B2-B0!B.B+B!�B�BPB��B��B�B�B�yB�fB�HB�;B�)B�B�B��B��B��B��BȴB�}B�XB�RB�FB�FB�FB�-B�B��B��B��B�qB�^B�'B�B��B��B�hB�bB�bB�Bn�Bk�BhsBdZBbNBbNBaHBaHBaHBaHB`BB_;B^5B\)BZBR�BN�BK�BI�BF�BD�BC�B?}B=qB8RB6FB5?B0!B-B)�B(�B&�B!�B�B�B�B{BoB\BPB
=B%BB  B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�BB��B
�B
�B
�B
�yB
�mB
�fB
�HB
�HB
�;B
�5B
�/B
�)B
�#B
�#B
�B
�
B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
ƨB
ȴB
ĜB
ÖB
��B
��B
��B
�}B
�wB
�qB
�qB
�dB
�dB
�^B
�^B
�XB
�XB
�RB
�RB
�RB
�LB
�FB
�9B
�LB
�?B
�9B
�9B
�9B
�3B
�9B
�3B
�3B
�3B
�3B
�3B
�9B
�3B
�3B
�3B
�3B
�3B
�9B
�3B
�9B
�FB
�LB
�LB
�LB
�RB
�XB
�jB
�jB
�wB
�}B
��B
��B
ÖB
ĜB
ƨB
ƨB
ƨB
ɺB
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�B
�B
�B
�B
�B
�B
�B
�BB
�NB
�ZB
�`B
�sB
�yB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��BBB+BJBbB{B�B�B�B�B�B�B�B�B�B�B �B!�B$�B%�B+B/B0!B1'B5?B6FB7LB8RB8RB<jB>wB@�BB�BB�BE�BG�BG�BI�BK�BO�BS�BW
B\)BaHBdZBffBhsBk�Bn�Bp�Br�Bt�Bv�Bv�Bx�B{�B�B�%B�1B�1B�=B�PB�\B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�9B�?B�LB�dB�jB��BÖBŢBɺB��B��B��B��B��B�
B�B�B�B�/B�NB�TB�`B�fB�yB�yB�B�B�B�B�B��B��B��BBB+B	7B
=B
=BDBDBDBDBDBJBJBJBVBbB�B�B�B�B�B�B�B �B �B �B"�B#�B%�B(�B+B-B/B0!B0!B1'B2-B33B49B6FB:^B;dB<jB=qBA�BB�BC�BC�BC�BD�BD�BD�BE�BG�BH�BH�BJ�BK�BM�BP�BP�BR�BS�BT�BT�BVBW
BW
BW
BXBYB^5BaHBcTBe`BiyBjBjBk�Bl�Bn�Bp�Bp�Bq�Bs�Bu�Bu�Bv�Bx�Bx�B{�B|�B}�B}�B~�B�B�B�B�B�B�B�%B�%B�+B�=B�DB�JB�PB�\B�bB�bB�bB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�!B�'B�'B�-B�3B�9B�9B�FB�FB�LB�XB�^B�dB�jB�jB�jB�qB�qB�}B�}B��B��BBÖBÖBŢBŢBŢBŢBƨBƨBǮBǮBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�
B�
B�B�B�B�B�B�B�#B�#B�#B�#B�#B�)B�/B�/B�/B�5B�5B�5B�;B�BB�HB�HB�HB�HB�NB�TB�TB�TB�TB�ZB�`B�mB�sB�sB�sB�sB�sB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBBBBBBB+B+B+B+B+B+B1B1B1B	7B	7B
=B
=B
=BDBDBJBPBPBPBPBVBVBVB\BbBhBhBhBhBhBhBoBoBuBuBuBuBuBuB{B{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B!�B!�B"�B"�B"�B"�B"�B#�B$�B$�B%�B%�B%�B&�B&�B'�B'�B(�B(�B(�B(�B)�B)�B+B+B+B+B+B+B+B,B-B-B-B.B.B.B.B.B.B.B/B/B/B0!B0!B0!B1'B1'B2-B2-B33B33B33B33B33B33B33B49B49B49B49B5?B5?B5?B5?B5?B5?B5?B6FB7LB7LB7LB7LB8RB8RB8RB8RB9XB9XB9XB9XB:^B:^B:^B;dB;dB<jB<jB<jB=qB=qB=qB=qB>wB>wB>wB>wB>wB?}B?}B@�B@�B@�B@�B@�B@�BA�BA�BA�BA�BB�BB�BB�BB�BC�BC�BC�BC�BC�BD�BD�BD�BD�BE�BE�BE�BE�BE�BF�BF�BG�BG�BG�BH�BH�BH�BH�BI�BI�BI�BI�BI�BI�BI�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B�7B�7B�uB��B��B��B��B��B�-BȴB��B��B�)B�TB�TB�ZB�fB�`B�TB�TB�TB�TB�TB�TB�TB�TB�TB�fB�TB�TB�TB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�`B�ZB�ZB�ZB�ZB�ZB�NB�;B�
B�9B_;B�B�bB�=B�Bs�BdZB`BBXBN�BK�BD�B?}B;dB8RB1'B+B#�B�BB�B�`B�)B��B�^B�B��B�%Bx�Br�BiyBR�BG�B>wB-B%�B!�BuB	7B  B�B�;B��B��BĜB�jB�?B��B��B�oB�=B�Bz�Bt�BiyB]/BL�BF�B;dB9XB7LB33B2-B0!B.B+B!�B�BPB��B��B�B�B�yB�fB�HB�;B�)B�B�B��B��B��B��BȴB�}B�XB�RB�FB�FB�FB�-B�B��B��B��B�qB�^B�'B�B��B��B�hB�bB�bB�Bn�Bk�BhsBdZBbNBbNBaHBaHBaHBaHB`BB_;B^5B\)BZBR�BN�BK�BI�BF�BD�BC�B?}B=qB8RB6FB5?B0!B-B)�B(�B&�B!�B�B�B�B{BoB\BPB
=B%BB  B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�BB��B
�B
�B
�B
�yB
�mB
�fB
�HB
�HB
�;B
�5B
�/B
�)B
�#B
�#B
�B
�
B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
ƨB
ȴB
ĜB
ÖB
��B
��B
��B
�}B
�wB
�qB
�qB
�dB
�dB
�^B
�^B
�XB
�XB
�RB
�RB
�RB
�LB
�FB
�9B
�LB
�?B
�9B
�9B
�9B
�3B
�9B
�3B
�3B
�3B
�3B
�3B
�9B
�3B
�3B
�3B
�3B
�3B
�9B
�3B
�9B
�FB
�LB
�LB
�LB
�RB
�XB
�jB
�jB
�wB
�}B
��B
��B
ÖB
ĜB
ƨB
ƨB
ƨB
ɺB
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�B
�B
�B
�B
�B
�B
�B
�BB
�NB
�ZB
�`B
�sB
�yB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��BBB+BJBbB{B�B�B�B�B�B�B�B�B�B�B �B!�B$�B%�B+B/B0!B1'B5?B6FB7LB8RB8RB<jB>wB@�BB�BB�BE�BG�BG�BI�BK�BO�BS�BW
B\)BaHBdZBffBhsBk�Bn�Bp�Br�Bt�Bv�Bv�Bx�B{�B�B�%B�1B�1B�=B�PB�\B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�9B�?B�LB�dB�jB��BÖBŢBɺB��B��B��B��B��B�
B�B�B�B�/B�NB�TB�`B�fB�yB�yB�B�B�B�B�B��B��B��BBB+B	7B
=B
=BDBDBDBDBDBJBJBJBVBbB�B�B�B�B�B�B�B �B �B �B"�B#�B%�B(�B+B-B/B0!B0!B1'B2-B33B49B6FB:^B;dB<jB=qBA�BB�BC�BC�BC�BD�BD�BD�BE�BG�BH�BH�BJ�BK�BM�BP�BP�BR�BS�BT�BT�BVBW
BW
BW
BXBYB^5BaHBcTBe`BiyBjBjBk�Bl�Bn�Bp�Bp�Bq�Bs�Bu�Bu�Bv�Bx�Bx�B{�B|�B}�B}�B~�B�B�B�B�B�B�B�%B�%B�+B�=B�DB�JB�PB�\B�bB�bB�bB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�!B�'B�'B�-B�3B�9B�9B�FB�FB�LB�XB�^B�dB�jB�jB�jB�qB�qB�}B�}B��B��BBÖBÖBŢBŢBŢBŢBƨBƨBǮBǮBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�
B�
B�B�B�B�B�B�B�#B�#B�#B�#B�#B�)B�/B�/B�/B�5B�5B�5B�;B�BB�HB�HB�HB�HB�NB�TB�TB�TB�TB�ZB�`B�mB�sB�sB�sB�sB�sB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBBBBBBB+B+B+B+B+B+B1B1B1B	7B	7B
=B
=B
=BDBDBJBPBPBPBPBVBVBVB\BbBhBhBhBhBhBhBoBoBuBuBuBuBuBuB{B{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B!�B!�B"�B"�B"�B"�B"�B#�B$�B$�B%�B%�B%�B&�B&�B'�B'�B(�B(�B(�B(�B)�B)�B+B+B+B+B+B+B+B,B-B-B-B.B.B.B.B.B.B.B/B/B/B0!B0!B0!B1'B1'B2-B2-B33B33B33B33B33B33B33B49B49B49B49B5?B5?B5?B5?B5?B5?B5?B6FB7LB7LB7LB7LB8RB8RB8RB8RB9XB9XB9XB9XB:^B:^B:^B;dB;dB<jB<jB<jB=qB=qB=qB=qB>wB>wB>wB>wB>wB?}B?}B@�B@�B@�B@�B@�B@�BA�BA�BA�BA�BB�BB�BB�BB�BC�BC�BC�BC�BC�BD�BD�BD�BD�BE�BE�BE�BE�BE�BF�BF�BG�BG�BG�BH�BH�BH�BH�BI�BI�BI�BI�BI�BI�BI�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.39 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220104100106                              AO  ARCAADJP                                                                    20220104100106    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220104100106  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220104100106  QCF$                G�O�G�O�G�O�C000            