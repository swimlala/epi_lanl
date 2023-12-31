CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-12-20T09:37:47Z creation;2019-12-20T09:37:48Z conversion to V3.1;2022-08-02T05:11:40Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ad   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ϥ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191220093747  20220818091504  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL                A   JA  A30_8420_032                    2C  D   APEX                            8420                            2.11.2                          846 @�����S�1   @������ @-��Ϫ͟�cMԕ*�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B���B�  B�  B���B���B���B���B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C�fC  C
  C�C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(�C*� C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@ff@k�@�
=@��A�
A;\)A[\)Az�HA���A��A��
A�A��A�  A�A��B��BffB��BB&�RB.�RB6�B>�
BF�
BN�HBV�HB^��Bg�Bn�
Bv��B~��B�=qB�� B��B�Q�B�Q�B�.B�L�B��B�p�B�u�B�u�B�z�B�p�B�ffB�W
B�aHBÙ�B�W
B�Q�B�ffB�k�B�u�B�k�B�p�B�u�B�u�B�ffB�ffB�aHB�z�B��{B��C�RC��C��C�RC	��C�C�C��C�RC��C��C�3C�3C�RC��C��C!�RC#�qC%� C'ǮC*5�C+�
C-�3C/��C1�RC3��C5�RC7��C9�3C;�3C=��C?�3CA�CC��CE� CG��CI�3CK�CM��CO�3CQ�3CS�RCU�RCW� CY�{C[��C]��C_��Ca��Cc��Ce�RCg��Ci��Ck��Cm��Co��Cq��Cs�Cu�3Cw�3Cy��C{��C}��C��C��qC���C���C�ٚC��)C���C�ٚC�ٚC��
C�ٚC��RC��{C���C��HC�޸C�ٚC�ٚC��qC�޸C��)C��qC�� C�޸C���C���C��RC���C��qC��C��C��)C�� C��qC���C�ٚC���C�޸C�޸C���C���C���C��)C��qC�޸C��qC�ٚC���C�ٚC�ٚC��qC�� C��qC��)C�޸C��)C��RC��)C�� C��qC��RC�ٚC���C�޸C��qC��qC��)C�ٚC�޸C�޸C�� C��C�޸C���C��)C���C�ٚC��)C��qC�޸C�� C�� C��qC�ٚC��)C��)C��)C��HC��qC��)C��)C��qC��HC��HC��)C��qC��fC��C��RC�ٚC���C�ٚC��)C��)C�ٚC���C��qC��qC��qC��)C��)C���C��RC��)C��qC��)C��RC��
C���C��)C�ٚC���C��)C�޸C�޸C��)C���C��)C���D k�D �Dl�D�DmqD�qDn�D�\DnD��Dl�D�qDn�D�Dp D�DmqD�\D	p�D	�D
l)D
�qDo\D�Dn�D�Dn�D�Dp D�Dn�D�DnD�qDmqD��Dj�D�qDp�D�HDn�D�)DmqD�Do\D�Dl�D�Dl)D��Dl�D�DnD��Dl)D�Dp D�Dl)D�=Dl)D�qDj�D��D n�D ��D!o\D!�D"o\D"�qD#nD#�D$n�D$�D%n�D%�D&l�D&�D'n�D'�)D(l)D(�D)nD)��D*nD*�D+l)D+��D,mqD,��D-nD-� D.nD.�D/n�D/��D0l�D0�qD1nD1�D2mqD2�D3n�D3�D4p�D4�D5k�D5�D6nD6�qD7nD7�\D8nD8�D9mqD9��D:l�D:�qD;n�D;�D<l�D<�D=l)D=��D>nD>�qD?l�D?��D@k�D@�DAk�DA�DBq�DB�\DCl�DC�DDp DD�DEl�DE�\DFp DF�\DGo\DG�DHl)DH�DIqHDI�DJnDJ�DKj�DK��DLn�DL�\DMmqDM��DNj�DN�DOn�DO�DPl�DP�)DQo\DQ�DRl�DR�\DSqHDS� DTnDT�DUo\DU�qDVp DV��DWnDW�DXp DX� DYnDY�qDZmqDZ�D[o\D[�D\mqD\�)D]nD]� D^n�D^��D_n�D_�\D`n�D`�Dan�Da�qDbl)Db�DcnDc�qDdn�Dd�Del�De�)DfnDf� Dgo\Dg�Dhl�Dh��DinDi�DjnDj�Dko\Dk� DlmqDl��DmmqDm�)Dnl)Dn�qDol�Do��DpnDp�\Dqn�Dq�)Drl)Dr��Dsn�Ds�HDtp�Dt�\DumqDu�\DvnDv�qDwnDw�\DxqHDx� DynDy�qDzl)Dz�D{n�D{�D|l�D|�)D}nD}�D~n�D~�qDnD�D�7
D�w
D��\D���D�8RD�x�D���D��D�5�D�vD��fD��
D�8 D�w\D���D��
D�6�D�w\D��
D���D�5�D�v�D��\D��
D�6�D�vfD���D��fD�7
D�w\D�� D�� D�8 D�w�D���D��fD�7\D�w�D���D��fD�6D�vD��fD��\D�7\D�vfD��D��
D�7�D�w\D��D���D�6D�vD���D���D�6fD�vfD���D���D�7\D�v�D���D��\D�7
D�v�D��fD��fD�6fD�w\D���D��D�5�D�u�D��qD��fD�7\D�w\D���D��
D�7�D�w
D��D��\D�7
D�w
D��\D���D�6D�u�D��\D���D�7�D�w\D��\D���D�6�D�w
D���D���D�7
D�w
D���D��
D�7\D�w\D��
D��D�6D�vfD���D��\D�7\D�w\D���D���D�6fD�vfD���D���D�6fD�v�D��fD��D�6fD�w\D��\D���D�6�D�w
D��
D��
D�7�D�xRD���D���D�6D�v�D��\D���D�6�D�w\D�� D��\D�6�D�vfD��fD��D�6fD�vfD���D��fD�6fD�w�D���D���D�6D�w
D��
D���D�6�D�w�D���D��\D�7\D�v�D��\D���D�8 D�v�D���D��D�7
D�x D���D���D�7�D�v�D���D��D�6�D�vD��qD��fD�8RD�w
D��D���D�7�D�xRD��\D��D�6fD�w
D���D���D�7
D�vD���D���D�7\D�w
D��
D��fD�5�D�uqD��
D�� D�7\D�w�D���D���D�6fD�vD���D��\D�7
D�vfD��fD���D�7�D�w�D��
D���D�7\D�w
D��\D��\D�6�D�w�D���D���D�7
D�w�D��\D��
D�7\D�w
D��
D���D�6�D�vD��fD��
D�6�D�u�D���D�� D�8RD�x D��\D���D�7\D�w
D��fD��\D�8 D�w
D���D���D�7
D�w\D���D��
D�7
D�vDµ�D���D�6D�w\Dö�D��D�7\D�xRDĸ�D���D�7
D�w\DŶ�D��fD�6�D�vDƶfD���D�6�D�w
DǷ
D��fD�7\D�w�Dȷ\D�� D�7�D�v�DɶfD���D�6fD�v�Dʷ�D���D�6�D�w�D˷�D��
D�6fD�w
D̷\D��
D�6fD�v�Dͷ\D���D�6fD�v�Dη\D���D�7�D�x D϶�D��D�7
D�x Dз�D��
D�7
D�vDѵqD���D�7�D�w�Dҷ�D���D�7�D�vfDӶ�D��\D�7�D�w\DԷ\D���D�6fD�u�DնfD��D�7\D�v�DֵqD���D�8RD�xRD׸ D��\D�7\D�w\Dط�D���D�7\D�v�DٶD��D�7
D�x Dڸ D�� D�8RD�w\D۶fD���D�7
D�w\Dܷ�D��RD�7\D�vfDݶD��
D�7
D�w\D޷
D��\D�7\D�w�D߸ D��
D�7
D�v�D�
D��\D�7�D�xRD� D��\D�7
D�uqD�fD��\D�7\D�w\D㷮D�� D�8 D�xRD两D���D�7\D�w\D� D���D�6�D�vfD涸D���D�6�D�w�D緮D��
D�6fD�u�D趸D��
D�7\D�x D鷮D�� D�7
D�w
D귮D��
D�6fD�v�D붸D��D�6�D�w�D�\D��\D�8 D�w�D��
D��\D�7
D�w\D�
D��fD�6fD�w
D� D���D�5�D�v�D�
D���D�7
D�xRD�
D��
D�7\D�w\D�
D���D�6fD�w
D�
D��fD�6fD�w
D��
D��\D�7�D�w\D��\D��
D�7\D�w�D���D��
D�7�D�w
D��fD��D�5�D�u�D��D��D�6fD�v�D��
D��\D�8 D�w\D��\D��\D�6fD�u�D��fD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�o A�n�A�s�A�tTA�t�A�u�A�v�A�v�A�w�A�w�A�s�A�s�A�y�A�r|A�]�A�&�A���A��A�c�A�@A�8�A�/�A�-�A�(�A�"4A��A��A��A�چAԽ<AԪ�AԠ�AԎ"A���A���A�3�Aǀ4A�1[A��A�_A�qA��GA�B�A��A��)A�|�A�h
A�>�A���A�QNA�ɆA�DA��A�9$A�v�A���A���A�w2A�e�A���A���A��rA�iA�0�A��A�c�A���A��xA��A�rGA�H�A���A��A�Q�A���A�s�A��_A�A�A��FA�tA�N<Ay��Ax:�Ax��AuخAoc AiJ�Ahc�Ae&Ac�A]h
AY��AV��AS��AN^5AM�[AN$AM�AK�MAJ��AI��AIxAG�ADhsAB�A?��A=OA=��A;�A:1A6E9A4bNA3��A1��A0c A.�CA-�UA+qA*Y�A&��A%+kA$A�A$�A#��A#*0A#�A"��A"=�A!TaA �<A g8A��A8AK�AGA�?A`�A�>A��AqvA��A&A��A2�AkQA�6Am]AĜAz�A�A�tAl�Ax�AaAA A��A2aA�'AN<AGA��AffA��AH�AoiAP�A��A��Al"A^�A&A�AA��AĜA�	AU�A
֡A
�hA
u%A
N�A
JA	�A	VmA��AN<A�dA}VAT�A"hA��A0UA��A�A�XA($A�5A�@AV�AJ#AeA�$A33A�A��A��A��A ��A Mj@���@���@�J�@�N�@�
=@�Q@��@���@�ϫ@��O@�g8@�/�@��~@�YK@�G@��@���@��@�x�@�O@�7L@�@�#:@��N@�>�@���@��@��n@�`B@엍@�&�@��A@�@�e�@���@�7@�IR@�i�@�iD@��@�M�@��@�X@�l�@�(�@���@��/@�@�@���@��@�4@�@�@߷@�iD@�33@މ�@�%�@�_p@�6z@��@�Q�@�b@۟V@ڵ@�0U@٨X@��@؎�@��g@�t�@��v@�R�@��@�b�@�;@Ԅ�@�7�@��@��}@ӑh@�m]@�F�@��@ҷ�@҅�@�#:@��m@р4@�?}@�@д9@�c�@��@��@΄�@��@��`@�,=@˙�@�Y@�n�@Ɋ�@�%F@���@���@ȳh@�U2@�˒@�qv@�ѷ@Ơ�@�g8@�-@�$@��@��@��9@ť@��@ė�@�c�@���@�hs@�\)@�"�@�Ft@�@���@���@��x@�a|@���@���@�1�@���@�D�@��F@�@���@�u�@�B[@��"@�_�@�e@��@�ݘ@���@�:�@�V@���@�{@���@���@�rG@�\�@�+k@��@�x@�u@��
@�zx@�;d@�V@���@��B@���@���@�!�@�͟@�/�@���@�@���@��Y@�Q@�9X@�$@�J@���@���@�T�@�2a@���@�h
@�$@���@�P�@�ѷ@�U2@���@���@���@�w2@� i@���@�i�@�!@��H@�Z�@�:�@��]@��,@��6@�xl@��@���@���@�Vm@�ߤ@��$@��1@���@���@�^�@�-w@��_@�O@��9@��@@�[W@�+@�ѷ@���@���@�v�@�Xy@�8�@�6@�ԕ@�6z@��@���@�J@�33@���@�-@��P@�V@��]@���@�@��;@���@�^�@���@���@��D@�l�@�E�@�0U@� �@���@���@�\)@��2@�v�@���@��g@��@��@��@�͟@���@�1�@��W@���@�rG@�P�@��f@��$@���@�M@�@���@���@��@�33@�(@��@���@���@�D�@��+@��X@�9�@��2@���@�kQ@�5?@��@��+@���@�o @�!-@��@��@��!@�z@�c�@��>@���@�L�@�@���@���@�R�@� �@��@��X@���@�f�@�.I@���@���@��@��b@���@�i�@�Q@��@��^@�dZ@�H�@� \@��@���@�~(@�V@��@���@�ԕ@�w2@��@�B[@��@��D@��z@�g�@��H@���@���@�i�@�U2@��@�ԕ@��k@�rG@�@���@�y>@�($@���@���@�&@��s@���@�c @�I�@�	@�&@�@�f@&@~��@~W�@~�@}�-@}(�@|u�@|/�@|�@{��@{W?@z�@z��@y�)@yB�@x�u@xb@w� @w|�@vxl@v$�@u��@u:�@t��@t�z@tU2@s��@s�V@sU�@r�R@rYK@r!�@q�@q+@p�/@p��@pV�@p1@oݘ@o��@o�@n��@n��@nGE@m��@m��@m<6@lm�@k�@k�@j�'@ju%@j?@i�@i�'@i=�@i2a@h��@hѷ@h��@htT@h�@g�{@gX�@g.I@f�]@fl�@f�@eY�@d�`@d�u@dPH@c�;@c�K@c��@cn/@c1�@b��@a��@`��@`�p@`|�@`  @_��@^��@^��@^H�@]�T@]�H@]��@]:�@\�@\w�@\m�@\A�@[J#@Z��@Z��@Z�F@Z��@Zc @Z?@Z�@Y�9@Y�S@Y�@X�z@Xx@Wخ@W��@W�@Wo@V�L@Ve@U�^@U��@UA @T�j@TK^@T'R@T�@S�Q@SF�@R��@R_�@R_@Q��@Q \@P��@P@O�@O�@N��@NZ�@M�#@M|@M�@L��@L?�@K��@Kg�@K8@K�@J��@Je@I�z@Ia�@I�@H�O@H<�@H@G�
@G~�@F��@F��@E��@Ej@E�@D��@Dѷ@D`�@C�@C��@C"�@Bn�@B
�@A��@A��@A�@A�M@AIR@@��@@u�@@�@?�w@?U�@?@O@?$t@>��@=�@<�5@<�9@<`�@<%�@;�m@;��@;=@;�@:ߤ@:��@:H�@9ԕ@9a�@8��@8��@8'R@7�}@7o�@76z@6�6@6GE@5�j@5T�@4�@4l"@4�@3�@3�}@3��@36z@2��@2��@2V@2H�@1��@1��@1��@1�M@1j@1N<@1V@0��@0y>@07�@0!@0$@0!@0~@0M@/33@.��@.Ov@-�@-��@-B�@,֡@,��@,��@,y>@,,=@,$@+�W@+e�@+8@+"�@+�@*�@*�R@*��@*h
@*-@)�@)�@(��@(�z@(��@(�o@(g8@'�A@'��@']�@'�@&�B@&�@&�\@&i�@&5?@%�@%��@%k�@%2a@$�@$�u@$Xy@$"h@#�A@#�*@#��@#t�@#X�@#�@#@"�@"��@"�r@"L0@"_@!��@!Q�@!+�@!V@ �@ �@ 2�@ �@�@W?@33@�@��@xl@&�@�X@�@�@�p@��@y>@c�@1@�@��@;d@�@��@��@l�@:*@+k@�@$�@
�@�@c@<6@��@�@�z@�Y@l"@>B@�@�@�r@�m@�g@K�@�@�@�@��@e@�#@��@�@��@B�@�/@�[@�@u�@?�@	�@��@�$@;d@�s@��@��@p;@c @GE@@�j@��@��@p�@S&@IR@F@�@�@��@~(@~(@u�@D�@:�@1'@��@��@iD@C�@"�@�@�@�@�@?@&�@	@�@�@�@�@x�@:�@�@�|@��@�e@Xy@,=@�}@�@�{@b�@"�@
��@
��@
E�@	��@	rG@	N<@	A @	�@�@��@��@��@�D@j@C-@"h@  @�r@ݘ@�P@;d@1�@�@�x@��@�\@�\@?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�o A�n�A�s�A�tTA�t�A�u�A�v�A�v�A�w�A�w�A�s�A�s�A�y�A�r|A�]�A�&�A���A��A�c�A�@A�8�A�/�A�-�A�(�A�"4A��A��A��A�چAԽ<AԪ�AԠ�AԎ"A���A���A�3�Aǀ4A�1[A��A�_A�qA��GA�B�A��A��)A�|�A�h
A�>�A���A�QNA�ɆA�DA��A�9$A�v�A���A���A�w2A�e�A���A���A��rA�iA�0�A��A�c�A���A��xA��A�rGA�H�A���A��A�Q�A���A�s�A��_A�A�A��FA�tA�N<Ay��Ax:�Ax��AuخAoc AiJ�Ahc�Ae&Ac�A]h
AY��AV��AS��AN^5AM�[AN$AM�AK�MAJ��AI��AIxAG�ADhsAB�A?��A=OA=��A;�A:1A6E9A4bNA3��A1��A0c A.�CA-�UA+qA*Y�A&��A%+kA$A�A$�A#��A#*0A#�A"��A"=�A!TaA �<A g8A��A8AK�AGA�?A`�A�>A��AqvA��A&A��A2�AkQA�6Am]AĜAz�A�A�tAl�Ax�AaAA A��A2aA�'AN<AGA��AffA��AH�AoiAP�A��A��Al"A^�A&A�AA��AĜA�	AU�A
֡A
�hA
u%A
N�A
JA	�A	VmA��AN<A�dA}VAT�A"hA��A0UA��A�A�XA($A�5A�@AV�AJ#AeA�$A33A�A��A��A��A ��A Mj@���@���@�J�@�N�@�
=@�Q@��@���@�ϫ@��O@�g8@�/�@��~@�YK@�G@��@���@��@�x�@�O@�7L@�@�#:@��N@�>�@���@��@��n@�`B@엍@�&�@��A@�@�e�@���@�7@�IR@�i�@�iD@��@�M�@��@�X@�l�@�(�@���@��/@�@�@���@��@�4@�@�@߷@�iD@�33@މ�@�%�@�_p@�6z@��@�Q�@�b@۟V@ڵ@�0U@٨X@��@؎�@��g@�t�@��v@�R�@��@�b�@�;@Ԅ�@�7�@��@��}@ӑh@�m]@�F�@��@ҷ�@҅�@�#:@��m@р4@�?}@�@д9@�c�@��@��@΄�@��@��`@�,=@˙�@�Y@�n�@Ɋ�@�%F@���@���@ȳh@�U2@�˒@�qv@�ѷ@Ơ�@�g8@�-@�$@��@��@��9@ť@��@ė�@�c�@���@�hs@�\)@�"�@�Ft@�@���@���@��x@�a|@���@���@�1�@���@�D�@��F@�@���@�u�@�B[@��"@�_�@�e@��@�ݘ@���@�:�@�V@���@�{@���@���@�rG@�\�@�+k@��@�x@�u@��
@�zx@�;d@�V@���@��B@���@���@�!�@�͟@�/�@���@�@���@��Y@�Q@�9X@�$@�J@���@���@�T�@�2a@���@�h
@�$@���@�P�@�ѷ@�U2@���@���@���@�w2@� i@���@�i�@�!@��H@�Z�@�:�@��]@��,@��6@�xl@��@���@���@�Vm@�ߤ@��$@��1@���@���@�^�@�-w@��_@�O@��9@��@@�[W@�+@�ѷ@���@���@�v�@�Xy@�8�@�6@�ԕ@�6z@��@���@�J@�33@���@�-@��P@�V@��]@���@�@��;@���@�^�@���@���@��D@�l�@�E�@�0U@� �@���@���@�\)@��2@�v�@���@��g@��@��@��@�͟@���@�1�@��W@���@�rG@�P�@��f@��$@���@�M@�@���@���@��@�33@�(@��@���@���@�D�@��+@��X@�9�@��2@���@�kQ@�5?@��@��+@���@�o @�!-@��@��@��!@�z@�c�@��>@���@�L�@�@���@���@�R�@� �@��@��X@���@�f�@�.I@���@���@��@��b@���@�i�@�Q@��@��^@�dZ@�H�@� \@��@���@�~(@�V@��@���@�ԕ@�w2@��@�B[@��@��D@��z@�g�@��H@���@���@�i�@�U2@��@�ԕ@��k@�rG@�@���@�y>@�($@���@���@�&@��s@���@�c @�I�@�	@�&@�@�f@&@~��@~W�@~�@}�-@}(�@|u�@|/�@|�@{��@{W?@z�@z��@y�)@yB�@x�u@xb@w� @w|�@vxl@v$�@u��@u:�@t��@t�z@tU2@s��@s�V@sU�@r�R@rYK@r!�@q�@q+@p�/@p��@pV�@p1@oݘ@o��@o�@n��@n��@nGE@m��@m��@m<6@lm�@k�@k�@j�'@ju%@j?@i�@i�'@i=�@i2a@h��@hѷ@h��@htT@h�@g�{@gX�@g.I@f�]@fl�@f�@eY�@d�`@d�u@dPH@c�;@c�K@c��@cn/@c1�@b��@a��@`��@`�p@`|�@`  @_��@^��@^��@^H�@]�T@]�H@]��@]:�@\�@\w�@\m�@\A�@[J#@Z��@Z��@Z�F@Z��@Zc @Z?@Z�@Y�9@Y�S@Y�@X�z@Xx@Wخ@W��@W�@Wo@V�L@Ve@U�^@U��@UA @T�j@TK^@T'R@T�@S�Q@SF�@R��@R_�@R_@Q��@Q \@P��@P@O�@O�@N��@NZ�@M�#@M|@M�@L��@L?�@K��@Kg�@K8@K�@J��@Je@I�z@Ia�@I�@H�O@H<�@H@G�
@G~�@F��@F��@E��@Ej@E�@D��@Dѷ@D`�@C�@C��@C"�@Bn�@B
�@A��@A��@A�@A�M@AIR@@��@@u�@@�@?�w@?U�@?@O@?$t@>��@=�@<�5@<�9@<`�@<%�@;�m@;��@;=@;�@:ߤ@:��@:H�@9ԕ@9a�@8��@8��@8'R@7�}@7o�@76z@6�6@6GE@5�j@5T�@4�@4l"@4�@3�@3�}@3��@36z@2��@2��@2V@2H�@1��@1��@1��@1�M@1j@1N<@1V@0��@0y>@07�@0!@0$@0!@0~@0M@/33@.��@.Ov@-�@-��@-B�@,֡@,��@,��@,y>@,,=@,$@+�W@+e�@+8@+"�@+�@*�@*�R@*��@*h
@*-@)�@)�@(��@(�z@(��@(�o@(g8@'�A@'��@']�@'�@&�B@&�@&�\@&i�@&5?@%�@%��@%k�@%2a@$�@$�u@$Xy@$"h@#�A@#�*@#��@#t�@#X�@#�@#@"�@"��@"�r@"L0@"_@!��@!Q�@!+�@!V@ �@ �@ 2�@ �@�@W?@33@�@��@xl@&�@�X@�@�@�p@��@y>@c�@1@�@��@;d@�@��@��@l�@:*@+k@�@$�@
�@�@c@<6@��@�@�z@�Y@l"@>B@�@�@�r@�m@�g@K�@�@�@�@��@e@�#@��@�@��@B�@�/@�[@�@u�@?�@	�@��@�$@;d@�s@��@��@p;@c @GE@@�j@��@��@p�@S&@IR@F@�@�@��@~(@~(@u�@D�@:�@1'@��@��@iD@C�@"�@�@�@�@�@?@&�@	@�@�@�@�@x�@:�@�@�|@��@�e@Xy@,=@�}@�@�{@b�@"�@
��@
��@
E�@	��@	rG@	N<@	A @	�@�@��@��@��@�D@j@C-@"h@  @�r@ݘ@�P@;d@1�@�@�x@��@�\@�\@?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�yB�yB�_B�B׍B�sB�sB�+B�KB��B�kB�kB�B�qBݲB�B�B�B�B�PB��B�VB��B��B�qB�<B��B�PB��B��B	  B	B	�B	~�B	��B
WsB
mCB
��B
��B
�B
��B
��B
�QB
�NB
��B
��B
ÖB
��B
�8B
�RBTB1�BD�BU�Ba�B�KB��B��B��B�cB� B^�BPBc BDgB%B?�BC-BB'B:B'�B
̈́B
e�B
V9B
G+B
?HB
#TB
 �B
�B
"�B	�HB	��B	��B	��B	�CB	z*B	]�B	VmB	GB	4�B	]B	�B�B	zB��B	PB	8RB	D�B	l�B	}VB	z�B	��B	�FB	��B	��B	p�B	^�B	kB	\�B	H�B	#:B	,B	%B	4�B	5�B	3�B	1[B	.B	*KB	)yB	*KB	-CB	.cB	2�B	6+B	7fB	;�B	EB	I�B	O�B	c�B	l�B	o�B	pB	p!B	s�B	��B	��B	�fB	�KB	�rB	�B	��B	�}B	�HB	��B	��B	��B	�2B	�SB	�B	��B	�WB	�'B	��B	�iB	��B	��B	��B	��B	��B	��B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	�*B	�*B	�0B	��B	��B	�BB	��B	��B	��B	�B	�B	�UB	�3B	�mB	�%B	ƨB	��B	��B	��B	�zB	�1B	ȚB	�lB	�#B	��B	��B	��B	��B	̳B	�dB	�B	��B	��B	ϫB	�(B	�vB	�B	�B	ϫB	�HB	�VB	�vB	��B	��B	͹B	��B	�JB	��B	��B	��B	��B	��B	�NB	ѝB	� B	�&B	�uB	�,B	�2B	�9B	�9B	�$B	��B	�+B	ٚB	��B	ٚB	ٚB	�B	ؓB	�B	�kB	��B	ۦB	�WB	ܬB	ܒB	��B	�dB	��B	��B	��B	�5B	��B	�B	��B	��B	�OB	ބB	߾B	�\B	��B	��B	�5B	߾B	��B	�HB	�hB	�NB	�:B	�B	��B	�B	�B	�`B	�`B	��B	��B	�B	�2B	�RB	�B	��B	��B	��B	�
B	��B	�B	��B	�$B	��B	��B	�*B	�DB	��B	�yB	�DB	�B	�DB	��B	�0B	�yB	�eB	�B	�B	��B	�"B	�)B	�B	�B	�oB	�B	��B	�[B	�vB	��B	�aB	�aB	�B	��B	��B	�B	��B	��B	��B	�hB	��B	��B	�TB	��B	�B	�B	�ZB	�tB	��B	��B	��B	��B	��B	��B	��B	�B	�lB	�	B	��B	��B	��B	��B	�xB	��B	�B	�dB	�dB	��B	��B	��B	�PB	�<B	�qB	��B	�B	��B	��B	��B	�B	�B	�B	�cB	��B
  B
 B	��B	��B
�B
�B
'B
�B
aB
�B
MB
�B
�B
B
B
B
�B
�B
YB
?B
tB
�B
�B
B
�B
B
�B
	RB
	�B
	7B
	B
�B
fB
�B

XB
DB
xB
�B
�B
dB
�B
B
�B
�B
B
<B
�B
<B
�B
�B
�B
B
(B
�B
�B
4B
�B
�B
hB
�B
hB
hB
�B
�B
�B
�B
�B
�B
&B
&B
B
aB
2B
gB
SB
�B
9B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
�B

B
sB
YB
�B
�B
�B
sB
�B
�B
yB
KB
B
�B
�B
�B
	B
�B
�B
�B
�B
�B
OB
B
B
VB
 �B
 �B
!B
!B
!B
!-B
!|B
"hB
"�B
"�B
#�B
$B
$@B
$�B
$�B
$�B
$�B
%,B
%B
$�B
$�B
%FB
%`B
$�B
&fB
&B
&LB
&�B
&�B
'B
'RB
'�B
(�B
)*B
)B
)*B
)_B
)�B
*eB
*eB
*B
*�B
*�B
*�B
+QB
+6B
,"B
,=B
,�B
,�B
,�B
-�B
-�B
-�B
.B
-�B
./B
.�B
/OB
.�B
.�B
.�B
.�B
.IB
.B
.cB
.�B
.�B
/5B
/�B
/OB
/ B
.�B
/�B
/�B
/�B
0!B
/�B
0�B
0�B
1'B
1[B
1[B
1�B
1�B
1�B
1�B
2GB
2�B
2�B
33B
3hB
3�B
4nB
4nB
4TB
4�B
4�B
4�B
5%B
5?B
5tB
5�B
6+B
6+B
6zB
7fB
7fB
7�B
8B
8B
8RB
8�B
9	B
9	B
9>B
9�B
9�B
:B
:^B
:xB
:�B
:xB
:�B
:�B
:�B
;0B
;JB
;�B
<B
<B
<6B
<6B
<PB
="B
=qB
>B
>BB
>wB
>�B
>�B
>�B
?.B
>�B
?.B
?.B
?cB
?�B
?�B
@OB
@iB
@iB
@�B
AB
A;B
A�B
B'B
BAB
B[B
B�B
B�B
B�B
B�B
B�B
CGB
DB
D�B
D�B
D�B
EB
EmB
FB
E�B
F?B
F%B
F?B
FYB
F�B
GzB
G_B
GEB
GEB
HfB
HfB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
IRB
I�B
I�B
J	B
J	B
J	B
JXB
JrB
J�B
J�B
J�B
J�B
K)B
KDB
K)B
K)B
K)B
K�B
K�B
K�B
LB
K�B
LdB
L~B
L�B
MB
MB
M�B
M�B
NVB
N�B
N�B
N�B
OBB
O�B
P.B
P.B
P.B
P�B
P�B
P�B
QB
QhB
Q�B
Q�B
Q�B
RB
RTB
R�B
R�B
S[B
S�B
S�B
S�B
S�B
TFB
T�B
T�B
T�B
U�B
U�B
U�B
V9B
VB
VB
V9B
V�B
V�B
W
B
W?B
WsB
WsB
WsB
W�B
X_B
X�B
X�B
YKB
YKB
Y�B
Y�B
Z7B
ZQB
ZQB
ZQB
Z�B
Z�B
Z�B
[	B
[�B
[�B
[�B
\]B
\�B
\�B
]/B
]�B
^5B
^jB
^�B
^�B
^�B
^�B
^�B
_;B
_pB
_�B
`\B
`�B
a|B
bhB
b�B
b�B
cB
cB
c:B
cnB
cnB
c:B
c:B
c B
c B
cB
b�B
d&B
dZB
dZB
d@B
dB
d@B
d�B
d�B
d�B
d�B
d�B
eFB
e�B
eFB
eB
e,B
e�B
e�B
ffB
f�B
f�B
f�B
ffB
f�B
f�B
g8B
g8B
g8B
g8B
g�B
g�B
h
B
hXB
hsB
hsB
h�B
h�B
h�B
iB
i_B
i�B
i�B
i�B
j0B
jKB
jeB
jB
j�B
j�B
j�B
kB
k6B
k6B
kkB
k�B
k�B
k�B
k�B
lWB
l�B
l�B
l�B
l�B
l�B
m]B
m]B
m�B
n/B
n/B
n�B
n�B
n�B
oB
o�B
p;B
p;B
pUB
p�B
p�B
p�B
qB
q'B
q�B
q�B
q�B
q�B
r-B
rGB
r|B
r�B
r�B
s3B
shB
sMB
sB
sMB
s�B
s�B
s�B
tB
tB
tB
t9B
tTB
t9B
tTB
tB
uZB
utB
utB
utB
u�B
vzB
v�B
w�B
xlB
x�B
x�B
y>B
y>B
y$B
y>B
y>B
y>B
yrB
y�B
yXB
y>B
y	B
y>B
yXB
yXB
y�B
zDB
zxB
z�B
z�B
z�B
z�B
{JB
{�B
|B
|�B
}<B
}qB
}qB
}qB
}�B
}�B
}�B
}�B
~B
~B
~(B
~(B
~BB
~BB
~(B
~(B
~�B
~wB
~�B
~�B
~�B
~�B
~�B
�B
�B
�B
� B
�iB
�iB
��B
��B
��B
� B
� B
� B
��B
�oB
��B
�B
�uB
��B
��B
�GB
�aB
�GB
�GB
�{B
��B
�3B
�gB
��B
�B
�B
�B
�SB
��B
��B
��B
��B
�tB
�YB
�tB
�?B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BخBخBؓB�EB��BרBרB�_B�B�BڠBڠB�QB��B�jB��B�GB�PB�jB��B��B��B��B��B��B��B��B��B�VB�wB	 �B	GB	
=B	�B	�B
]�B
vFB
�kB
�IB
�B
�/B
��B
�B
��B
��B
�bB
��B
�B
�B
�QBgB2�BFBW�Bd@B��B�_B�`B��B��B��BabBR�Bh�BGzB&LBB[BF�BD�B@OB49B
��B
h>B
W�B
IB
C�B
(>B
B
B
+kB	ؓB	�DB	��B	�&B	�B	�iB	_�B	ZkB	KB	:�B	!B	
�B��B	dB�wB	�B	9�B	FtB	n/B	~�B	|PB	��B	��B	��B	��B	s�B	_�B	m�B	`B	L�B	%�B	B	'�B	6�B	7�B	5�B	4TB	0oB	.IB	+�B	+�B	-�B	/OB	33B	6�B	8B	<�B	F?B	J�B	P�B	d�B	nIB	qB	p�B	q�B	u�B	��B	�%B	�B	�RB	�^B	�B	��B	��B	�4B	�TB	��B	�MB	��B	��B	��B	��B	��B	��B	��B	�UB	�iB	��B	�!B	�;B	��B	��B	��B	�"B	��B	�GB	�?B	��B	�6B	�B	�B	�B	��B	��B	��B	��B	�]B	��B	��B	�BB	��B	�HB	��B	� B	�'B	ĶB	��B	ƨB	�_B	��B	�KB	�1B	��B	��B	�B	��B	ʦB	�=B	�dB	��B	̳B	�jB	�B	�B	ΥB	�(B	�bB	�B	�B	�jB	�pB	ЗB	� B	�B	�.B	бB	ѝB	�"B	�jB	�B	οB	�bB	��B	�NB	ѝB	��B	�oB	�@B	өB	��B	ԯB	յB	֡B	��B	׍B	�_B	��B	�B	�7B	��B	�B	ٚB	�KB	��B	�#B	ܬB	�)B	��B	�B	��B	�B	��B	�5B	�5B	�OB	��B	�pB	�pB	�pB	�VB	�B	�;B	�'B	��B	�vB	ބB	��B	�'B	�\B	��B	��B	��B	��B	�B	�tB	�B	�,B	��B	��B	�zB	�B	�B	��B	��B	�B	�$B	�
B	�$B	�XB	�
B	��B	�$B	�B	�B	�yB	�yB	��B	�0B	��B	�B	�B	��B	�B	��B	�0B	�6B	�=B	�"B	�B	��B	��B	�}B	��B	�B	�'B	�AB	��B	��B	�GB	�B	��B	��B	��B	��B	��B	�B	�B	�hB	��B	�B	�nB	��B	��B	��B	�?B	��B	��B	�`B	�B	�LB	��B	��B	�B	��B	��B	��B	��B	��B	��B	�DB	�DB	�dB	�B	��B	��B	��B	�B	�6B	�jB	��B	��B	��B	��B	��B	�B	�B	�HB	�HB	�cB	�}B	��B
 B
 OB
 �B
 4B
 OB
[B
'B
�B
aB
�B
�B
�B
B
�B
9B
SB
SB
SB
%B
�B
�B
�B
EB
_B
zB
KB
�B
	B
	�B
	�B
	�B
	�B
	B
�B
	B

�B
�B
�B
B
�B
�B
B
jB
<B
<B
pB
�B
�B
�B
VB
\B
BB
\B
�B
bB
4B
�B
�B
�B
�B
�B
�B
�B
�B
�B
:B
oB
[B
@B
�B
�B
�B
B
�B
�B
�B
�B
�B
9B
B
�B
�B
9B
�B
�B
$B

B
?B
$B
sB
�B
�B
+B
B
+B
�B
YB
+B
�B
B
�B
7B
�B
QB
=B
CB
B
IB
B
B
�B
VB
VB
�B
 �B
!HB
!bB
!|B
!|B
!|B
!�B
"�B
#TB
#:B
#�B
$ZB
$�B
%B
%B
%B
%,B
%zB
%FB
%FB
%`B
%�B
%�B
%`B
&�B
&�B
&�B
&�B
'B
'RB
'�B
(>B
(�B
)_B
)_B
)yB
)�B
)�B
*�B
*�B
*�B
*�B
*�B
+B
+�B
+�B
,qB
,qB
,�B
,�B
-]B
-�B
-�B
.IB
.IB
.cB
.�B
/OB
/�B
/B
.�B
/B
/iB
.�B
.IB
.�B
/ B
/ B
/�B
0!B
/�B
/iB
/5B
/�B
0;B
0;B
0�B
0oB
0�B
1'B
1�B
1�B
1�B
2B
2-B
2-B
2GB
2�B
3B
33B
3�B
3�B
49B
4�B
4�B
4�B
4�B
4�B
5%B
5tB
5�B
5�B
6+B
6zB
6�B
6�B
7�B
7�B
8B
8RB
8RB
8�B
8�B
9XB
9XB
9�B
:B
:*B
:xB
:�B
:�B
:�B
:�B
:�B
;B
;JB
;B
;�B
<6B
<6B
<�B
<�B
<�B
<�B
=�B
=�B
>]B
>�B
>�B
>�B
?B
?HB
?cB
?HB
?}B
?cB
?�B
@ B
@4B
@�B
@�B
@�B
AB
AUB
A�B
BAB
BuB
BuB
B�B
B�B
B�B
B�B
CB
CaB
C�B
DgB
D�B
EB
E9B
ESB
E�B
F?B
F?B
FtB
FtB
F�B
F�B
GB
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
IB
IB
I7B
I�B
I�B
J#B
J=B
J=B
JrB
J�B
J�B
J�B
KB
J�B
K)B
K^B
K�B
K^B
K^B
KxB
K�B
K�B
L0B
L~B
LJB
L�B
L�B
MB
MPB
M�B
M�B
N<B
N�B
N�B
N�B
OBB
O�B
PB
P}B
P}B
P}B
P�B
P�B
QNB
QNB
Q�B
Q�B
R:B
R B
RoB
R�B
S&B
S&B
S�B
S�B
TFB
T,B
TFB
T�B
T�B
T�B
UMB
U�B
VB
V9B
VSB
VSB
VSB
V�B
W
B
W$B
W?B
WsB
W�B
W�B
W�B
X+B
X�B
Y1B
YKB
Y�B
YB
Y�B
ZB
ZkB
Z�B
Z�B
Z�B
Z�B
[=B
[#B
[WB
\B
\)B
\CB
\�B
\�B
]IB
]�B
]�B
^�B
^�B
^�B
_B
^�B
^�B
_;B
_�B
_�B
`'B
`�B
`�B
a�B
b�B
b�B
c:B
cTB
cTB
c�B
c�B
c�B
cnB
cnB
cTB
cTB
c:B
c:B
dtB
d�B
d�B
dtB
dtB
dtB
d�B
d�B
d�B
d�B
e,B
e�B
e�B
e�B
e`B
e`B
fLB
f2B
f�B
f�B
f�B
f�B
f�B
gB
gB
gmB
gmB
gmB
g�B
g�B
h$B
hXB
h�B
h�B
h�B
h�B
h�B
iB
iDB
i�B
i�B
i�B
jB
jeB
j�B
j�B
j�B
kB
k6B
kB
k6B
kkB
k�B
k�B
k�B
k�B
lB
lWB
l�B
l�B
l�B
l�B
mB
mCB
m�B
m�B
n/B
n}B
n}B
n�B
n�B
oB
o�B
pB
poB
p�B
p�B
p�B
p�B
p�B
qAB
qvB
q�B
q�B
q�B
r-B
raB
r|B
r�B
r�B
r�B
s�B
s�B
s�B
sMB
s�B
s�B
tB
tB
tTB
tTB
tTB
tTB
tnB
tnB
t�B
tnB
u�B
u�B
u�B
u�B
vB
v�B
w2B
w�B
x�B
x�B
y$B
yXB
yrB
yXB
y�B
yrB
y�B
y�B
y�B
y�B
y�B
yXB
yrB
yrB
y�B
z*B
z�B
z�B
z�B
z�B
z�B
{B
{dB
|B
|PB
|�B
}qB
}�B
}�B
}�B
}�B
}�B
~(B
~BB
~BB
~BB
~]B
~]B
~]B
~]B
~]B
~wB
~�B
~�B
~�B
B
~�B
.B
.B
�B
� B
� B
�OB
��B
��B
��B
��B
�;B
�oB
�oB
�UB
��B
��B
��B
�[B
��B
�-B
�B
��B
��B
�{B
�aB
��B
�B
��B
��B
��B
�SB
�9B
�9B
��B
��B
��B
��B
�?B
��B
��B
��B
�tB
�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<F@�<4;@<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.28(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202001101525062020011015250620200110152506202207271134022022072711340220220727113402202207271536292022072715362920220727153629  JA  ARFMdecpA30a                                                                20191220093713  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191220093747  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191220093747  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191220093748  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191220093748  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191220093748  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191220093748                      G�O�G�O�G�O�                JA  ARUP                                                                        20191220095411                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20191220000000  CF  PSAL_ADJUSTED_QC@Q�@}p�G�O�                JM  ARCAJMQC2.0                                                                 20200110062506  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200110062506  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023402  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063629  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091504                      G�O�G�O�G�O�                