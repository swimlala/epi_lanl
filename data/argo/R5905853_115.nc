CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:43:25Z creation;2022-06-04T17:43:26Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174325  20220610141505  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               sA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��L|5�1   @��L�io@/��z�H�ch�\)1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A���A�  B   BffB��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�ffB���B���B�  B�33B�ffB�33B���B���B�  B�  B�  B�  B�  Bԙ�B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
�C  C  C  C�C  C  C�fC�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DSy�DT  DT� DU  DUy�DU��DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@q�@���@���Az�A<z�A\z�A|z�A�=qA�=qA�=qA�=qA�
>A�
>A�=qA�=qB�B�RB�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��\B��\B��\B��\B�\)B��\B���B�\)B�\)B��\B�B���B�B�(�B�\)B��\BÏ\BǏ\Bˏ\BϏ\B�(�B�\)Bڏ\Bߏ\B�\B�\B�\B�\B�\B��\B��\B��\CǮCǮCǮC�HC	�HCǮCǮCǮC�HCǮCǮC�C�CǮCǮCǮC!ǮC#ǮC%ǮC'ǮC)ǮC+ǮC-ǮC/ǮC1ǮC3ǮC5�HC7�HC9�C;ǮC=ǮC?ǮCAǮCCǮCEǮCGǮCIǮCKǮCMǮCOǮCQǮCS�CUǮCWǮCYǮC[ǮC]ǮC_ǮCaǮCc�HCeǮCgǮCiǮCkǮCmǮCoǮCqǮCsǮCuǮCwǮCyǮC{ǮC}ǮCǮC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��
C��
C���C���C���C���C���C���C���C���C���C���C���C��
C���C��C��C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��
C���C���C���C���C���C���C���C���C���C���C���C���C���C���D q�D ��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��DxRD�RDq�D��D	q�D	��D
q�D
��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D�Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D q�D ��D!q�D!��D"q�D"��D#q�D#��D$q�D$��D%q�D%��D&q�D&��D'q�D'��D(q�D(��D)q�D)��D*q�D*��D+q�D+��D,q�D,��D-q�D-��D.q�D.��D/q�D/��D0q�D0��D1q�D1��D2q�D2��D3q�D3��D4q�D4��D5q�D5��D6q�D6��D7q�D7��D8q�D8��D9q�D9��D:q�D:��D;q�D;��D<q�D<��D=q�D=��D>q�D>��D?q�D?��D@q�D@��DAq�DA��DBq�DB��DCq�DC��DDq�DD��DEq�DE��DFq�DF��DGq�DG��DHq�DH��DIq�DI��DJq�DJ��DKq�DK��DLq�DL��DMxRDM��DNq�DN��DOq�DO��DPq�DP��DQq�DQ��DRq�DR��DSk�DS��DTq�DT��DUk�DU�DVq�DV��DWq�DW��DXq�DX��DYxRDY��DZq�DZ��D[q�D[��D\q�D\��D]q�D]��D^q�D^��D_q�D_��D`q�D`��Daq�Da��Dbq�Db��Dcq�Dc��Ddq�Dd��Deq�De��Dfq�Df��Dgq�Dg��Dhq�Dh��Diq�Di��Djq�Dj��Dkq�Dk��Dlq�Dl��Dmq�Dm��Dnq�Dn��Doq�Do��Dpq�Dp��Dqq�Dq��Drq�Dr��Dsq�Ds��Dtq�Dt��Duq�Du��Dvq�Dv��Dwq�Dw��Dxq�Dx��Dyq�Dy��Dzq�Dz��D{q�D{��D|q�D|��D}q�D}��D~q�D~��Dq�D��D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D��)D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D�)D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�u�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�֡A��KA��?A��yA���A���A���A���A��gA��A���A���A��[A��NA���A�خA���A��EAξwAξ�Aκ^Aι$AΰUAΒ�A�w2A�tTA�o A�m)A�j�A�e�A�c�A�]dA�UgA��A��A�?�A�-AɲaA��A��AĄMAïOA�A±�A�T,A��$A�_A��A���A�?}A��A�o�A��A�
=A���A��A��A��8A�!A�w�A��HA���A���A���A�3�A�A��|A��.A��A��A�>BA���A�ԕA���A���A�T,A�u�A��A���A�a|A�yA}#:Ay��AwsAs-Ap�Aoo�Am�AkL�Ai]dAg,=Ad~�A_=�A\�4A[�A[��AX�xAU:*AQGAO$tANg�AK7AF͟ACeA?��A<��A;bNA8/A6D�A5֡A5O�A4|A3�"A2یA2CA0�A.M�A+�|A+G�A)�HA)T�A)FtA)(�A(ĜA(DgA'�'A&�zA'ѷA)�+A)FtA(SA'��A'��A'�A'"�A&uA$GEA#K�A"�gA"�HA"�A!�]A!0�A �A jA A1Aa�A�A��AzAs�AԕA��AخA��Ap�A�A�zA�oAl�A	lA��ADgA��A�A�[Ah
A-�A�dAqvA&�A��AxA��A8A�?A˒A�A�<A�A�A�4A�AA*0A�A��A�CA[WA�A�EAiDA��A��A)_An�A
qA
4A
S�A	�IA	�A	'RA��A�A�AxA>BA�tA�AZ�A�+AbA�A��A�eA��A��A��A!�A E�@���@�6@��@��n@�2a@�,=@��y@��@���@�7@��k@�W?@�*0@��@���@�O@���@�|@��@�H�@��+@�J�@�u@�!�@�˒@�iD@�/�@�`�@��@���@�^@�'@�T�@�B[@�
�@��@��]@�dZ@��@�c�@�M@�7L@�~(@���@��@�>�@�u�@��@���@�^@�q@���@��)@��@�ƨ@��@�?}@ފr@��z@�\�@�Y@��v@�GE@��@ن�@؎�@�v�@�V�@֑�@ռ�@��P@�B[@��@ӥ�@���@�@��@�u�@�c�@�H�@��@Ϣ�@�X�@�ی@�r�@�U2@�9X@��@͘�@��M@���@˅�@�J#@���@�q�@�8�@��r@��@��N@�zx@���@��?@Ȯ}@�p;@�J@ǀ4@�`B@�"�@��c@���@�v�@�ݘ@�0�@�l"@�y�@��X@�C-@��@���@�Y�@�.I@�Ĝ@�~�@�Ta@�,=@���@�qv@�.I@��m@���@�9�@���@�'R@���@�|@�@�v�@��)@�w2@�A @��H@�V�@�{@��{@�%@��@��!@�Ov@�Q�@�&@��@��r@�M@��@���@��@�Z@� �@���@��h@���@���@�:�@��z@�YK@�
�@��D@���@��n@�K�@��4@�c�@�"h@�خ@��~@���@��@�~(@�M�@�x@��;@��{@��}@��@���@�<6@��E@���@�W�@��+@���@�J�@��@�ߤ@��m@��L@��A@�+k@��f@��@���@��u@�~�@�Z�@��>@��@���@�d�@��@���@���@��y@��b@�s�@�H�@�@�f�@��@��@��F@�4n@��]@��z@�g�@��@��@�"h@��d@���@�9�@��u@��@�a�@���@���@�Xy@��=@�x@��M@�
=@��@�:*@��r@��@��@��}@���@�U�@�T�@�Y�@�W?@�6z@�C@��y@��p@��m@�� @��@��}@���@�^5@�6�@��.@��F@��P@��@�hs@�RT@�/@�Y@��@��@��@��!@�V@��+@��z@���@�e,@�A�@���@�Xy@�J@���@��z@��X@��	@�e,@�.I@���@���@�8�@��@��w@���@��@���@�`B@�8�@���@���@��/@��,@��@��p@��<@���@��A@�`�@�
�@���@�G�@��@��@��F@�ff@�*�@��)@��#@���@�>�@�	l@��,@��<@���@�B[@�"h@��@���@���@���@�T�@�#�@��6@�3�@���@��g@�^�@���@�҉@��@�l�@��D@��@��@�'�@��@���@���@�C-@�M@��@���@�u�@���@�g8@��@E9@~�b@}��@}G�@}�@|�j@|�@|	�@zTa@x�@xl"@w�W@wU�@v��@u��@u�N@u\�@s��@siD@rں@r	@q#�@p�@pM@o�@oS�@oP�@oo@n��@n�X@n�+@nB[@m�@m�@m�@l'R@k|�@j�M@j��@jl�@j+k@i��@i�@h-�@g��@g�{@gY@f�@e@du�@d6@c��@cqv@c(@b��@b#:@a��@a�'@aO�@a@@`z�@_��@_K�@^ߤ@^�H@^ں@^�b@^�1@^�+@^e@]��@]f�@\bN@[�+@[خ@[�6@[�[@[iD@[.I@Z͟@Zz@ZB[@Z)�@Y�D@Y�n@Y=�@Yq@X�P@X�@X@Wt�@WP�@W!-@V�H@V��@V8�@V �@Uԕ@UV@T�v@T�@T�@T�@S�0@Sx@So@R_�@Ru@Q|@P��@PV�@O�r@O�@@N��@Nv�@N3�@M@Mu�@Me,@MB�@M/@L��@Ke�@J�"@JkQ@JJ@Iԕ@I�"@H�	@HU2@H�@G� @G�q@F��@E�9@Em]@EN<@E/@E+�@E&�@E#�@D�5@D�D@D�@C��@Cqv@C i@Bxl@B	@B�@A�o@A��@AG�@@��@@ی@@��@@��@@��@@N�@?�W@?��@?�	@?'�@>�R@>�+@>kQ@>@�@>4@=�@=�H@=��@=w2@=+�@<��@<D�@;�4@;RT@;�@:�]@:�\@:kQ@:1�@9�X@9^�@9*0@8�|@8�v@8�E@8�p@8�@8]d@8�@7�{@7J#@7.I@7�@6�@6l�@6YK@66�@6�@5�.@5�z@5u�@5�@4��@42�@3�V@3O@3"�@2��@2}V@2;�@2
�@1��@1*0@0��@0ی@0�[@0ѷ@0֡@0�E@0�@0�_@0	�@/�P@/+@.�@.�6@.��@.�6@.��@.z@.W�@.�@-�@-<6@-+@,��@,�z@,l"@,N�@+ݘ@+�g@+�g@+��@+dZ@+K�@+�@*�8@*�s@*��@*��@*H�@*($@*�@*u@)�@)x�@(�P@(�e@(��@(I�@'�w@'�P@'/�@&�@&�]@&�@&d�@&E�@&8�@&�@%��@%��@%�M@%X@$��@$�9@$|�@$S�@$>B@$1'@$!@$�@$�@#��@#��@#�F@#iD@#>�@#.I@#o@"�@"�@!�d@!�=@ �@ �@ ��@ �e@ oi@  �@�:@$t@�@�8@��@i�@�@�@@�C@Vm@�@�)@�@�o@-�@��@{J@W?@$t@�@�@͟@M�@�T@��@�@��@�@u�@[�@-�@�r@ƨ@s@H�@=@�@��@�h@��@s�@-@��@��@�"@c@w2@L�@&�@�|@�E@��@��@�@��@�@�Y@U2@ �@�@�F@�	@s@$t@�M@�@l�@Ov@@�@��@B�@*0@q@@�@�.@m�@A�@��@�@]�@;d@(@��@�y@��@�m@��@�h@�b@�@��@�@��@�N@��@J�@-w@ \@��@*�@(�@�@��@˒@��@�@e�@4�@C@
�@
�B@
� @
��@
V@
O@	�@	�@	��@	��@	�^@	��@	��@	u�@	�@�5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�֡A��KA��?A��yA���A���A���A���A��gA��A���A���A��[A��NA���A�خA���A��EAξwAξ�Aκ^Aι$AΰUAΒ�A�w2A�tTA�o A�m)A�j�A�e�A�c�A�]dA�UgA��A��A�?�A�-AɲaA��A��AĄMAïOA�A±�A�T,A��$A�_A��A���A�?}A��A�o�A��A�
=A���A��A��A��8A�!A�w�A��HA���A���A���A�3�A�A��|A��.A��A��A�>BA���A�ԕA���A���A�T,A�u�A��A���A�a|A�yA}#:Ay��AwsAs-Ap�Aoo�Am�AkL�Ai]dAg,=Ad~�A_=�A\�4A[�A[��AX�xAU:*AQGAO$tANg�AK7AF͟ACeA?��A<��A;bNA8/A6D�A5֡A5O�A4|A3�"A2یA2CA0�A.M�A+�|A+G�A)�HA)T�A)FtA)(�A(ĜA(DgA'�'A&�zA'ѷA)�+A)FtA(SA'��A'��A'�A'"�A&uA$GEA#K�A"�gA"�HA"�A!�]A!0�A �A jA A1Aa�A�A��AzAs�AԕA��AخA��Ap�A�A�zA�oAl�A	lA��ADgA��A�A�[Ah
A-�A�dAqvA&�A��AxA��A8A�?A˒A�A�<A�A�A�4A�AA*0A�A��A�CA[WA�A�EAiDA��A��A)_An�A
qA
4A
S�A	�IA	�A	'RA��A�A�AxA>BA�tA�AZ�A�+AbA�A��A�eA��A��A��A!�A E�@���@�6@��@��n@�2a@�,=@��y@��@���@�7@��k@�W?@�*0@��@���@�O@���@�|@��@�H�@��+@�J�@�u@�!�@�˒@�iD@�/�@�`�@��@���@�^@�'@�T�@�B[@�
�@��@��]@�dZ@��@�c�@�M@�7L@�~(@���@��@�>�@�u�@��@���@�^@�q@���@��)@��@�ƨ@��@�?}@ފr@��z@�\�@�Y@��v@�GE@��@ن�@؎�@�v�@�V�@֑�@ռ�@��P@�B[@��@ӥ�@���@�@��@�u�@�c�@�H�@��@Ϣ�@�X�@�ی@�r�@�U2@�9X@��@͘�@��M@���@˅�@�J#@���@�q�@�8�@��r@��@��N@�zx@���@��?@Ȯ}@�p;@�J@ǀ4@�`B@�"�@��c@���@�v�@�ݘ@�0�@�l"@�y�@��X@�C-@��@���@�Y�@�.I@�Ĝ@�~�@�Ta@�,=@���@�qv@�.I@��m@���@�9�@���@�'R@���@�|@�@�v�@��)@�w2@�A @��H@�V�@�{@��{@�%@��@��!@�Ov@�Q�@�&@��@��r@�M@��@���@��@�Z@� �@���@��h@���@���@�:�@��z@�YK@�
�@��D@���@��n@�K�@��4@�c�@�"h@�خ@��~@���@��@�~(@�M�@�x@��;@��{@��}@��@���@�<6@��E@���@�W�@��+@���@�J�@��@�ߤ@��m@��L@��A@�+k@��f@��@���@��u@�~�@�Z�@��>@��@���@�d�@��@���@���@��y@��b@�s�@�H�@�@�f�@��@��@��F@�4n@��]@��z@�g�@��@��@�"h@��d@���@�9�@��u@��@�a�@���@���@�Xy@��=@�x@��M@�
=@��@�:*@��r@��@��@��}@���@�U�@�T�@�Y�@�W?@�6z@�C@��y@��p@��m@�� @��@��}@���@�^5@�6�@��.@��F@��P@��@�hs@�RT@�/@�Y@��@��@��@��!@�V@��+@��z@���@�e,@�A�@���@�Xy@�J@���@��z@��X@��	@�e,@�.I@���@���@�8�@��@��w@���@��@���@�`B@�8�@���@���@��/@��,@��@��p@��<@���@��A@�`�@�
�@���@�G�@��@��@��F@�ff@�*�@��)@��#@���@�>�@�	l@��,@��<@���@�B[@�"h@��@���@���@���@�T�@�#�@��6@�3�@���@��g@�^�@���@�҉@��@�l�@��D@��@��@�'�@��@���@���@�C-@�M@��@���@�u�@���@�g8@��@E9@~�b@}��@}G�@}�@|�j@|�@|	�@zTa@x�@xl"@w�W@wU�@v��@u��@u�N@u\�@s��@siD@rں@r	@q#�@p�@pM@o�@oS�@oP�@oo@n��@n�X@n�+@nB[@m�@m�@m�@l'R@k|�@j�M@j��@jl�@j+k@i��@i�@h-�@g��@g�{@gY@f�@e@du�@d6@c��@cqv@c(@b��@b#:@a��@a�'@aO�@a@@`z�@_��@_K�@^ߤ@^�H@^ں@^�b@^�1@^�+@^e@]��@]f�@\bN@[�+@[خ@[�6@[�[@[iD@[.I@Z͟@Zz@ZB[@Z)�@Y�D@Y�n@Y=�@Yq@X�P@X�@X@Wt�@WP�@W!-@V�H@V��@V8�@V �@Uԕ@UV@T�v@T�@T�@T�@S�0@Sx@So@R_�@Ru@Q|@P��@PV�@O�r@O�@@N��@Nv�@N3�@M@Mu�@Me,@MB�@M/@L��@Ke�@J�"@JkQ@JJ@Iԕ@I�"@H�	@HU2@H�@G� @G�q@F��@E�9@Em]@EN<@E/@E+�@E&�@E#�@D�5@D�D@D�@C��@Cqv@C i@Bxl@B	@B�@A�o@A��@AG�@@��@@ی@@��@@��@@��@@N�@?�W@?��@?�	@?'�@>�R@>�+@>kQ@>@�@>4@=�@=�H@=��@=w2@=+�@<��@<D�@;�4@;RT@;�@:�]@:�\@:kQ@:1�@9�X@9^�@9*0@8�|@8�v@8�E@8�p@8�@8]d@8�@7�{@7J#@7.I@7�@6�@6l�@6YK@66�@6�@5�.@5�z@5u�@5�@4��@42�@3�V@3O@3"�@2��@2}V@2;�@2
�@1��@1*0@0��@0ی@0�[@0ѷ@0֡@0�E@0�@0�_@0	�@/�P@/+@.�@.�6@.��@.�6@.��@.z@.W�@.�@-�@-<6@-+@,��@,�z@,l"@,N�@+ݘ@+�g@+�g@+��@+dZ@+K�@+�@*�8@*�s@*��@*��@*H�@*($@*�@*u@)�@)x�@(�P@(�e@(��@(I�@'�w@'�P@'/�@&�@&�]@&�@&d�@&E�@&8�@&�@%��@%��@%�M@%X@$��@$�9@$|�@$S�@$>B@$1'@$!@$�@$�@#��@#��@#�F@#iD@#>�@#.I@#o@"�@"�@!�d@!�=@ �@ �@ ��@ �e@ oi@  �@�:@$t@�@�8@��@i�@�@�@@�C@Vm@�@�)@�@�o@-�@��@{J@W?@$t@�@�@͟@M�@�T@��@�@��@�@u�@[�@-�@�r@ƨ@s@H�@=@�@��@�h@��@s�@-@��@��@�"@c@w2@L�@&�@�|@�E@��@��@�@��@�@�Y@U2@ �@�@�F@�	@s@$t@�M@�@l�@Ov@@�@��@B�@*0@q@@�@�.@m�@A�@��@�@]�@;d@(@��@�y@��@�m@��@�h@�b@�@��@�@��@�N@��@J�@-w@ \@��@*�@(�@�@��@˒@��@�@e�@4�@C@
�@
�B@
� @
��@
V@
O@	�@	�@	��@	��@	�^@	��@	��@	u�@	�@�5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	:B	:B	 B	B	 B	B	 B	B	�B	B	�B	B	�B	�B	�B	B	�B	�B	�B	hB	�B	�B	hB	B	�B	4B	B	B	 B	�B	�B	�B	�B	HB	9B	C�B	O\B	M�B	\B	��B
#�B
��B
�tB
��B
�ZB@�BH�Bm�BwfB|B�'B��B��B��B��B�B�nB��B�5B��B��B�Bo�Bj�B\xB:*B$�BTB
�zB
��B
�B
�BB
�uB
p�B
JXB
 vB
	B
GB
 �B	�(B	��B	�B	�B	�uB	��B	�tB	�bB	��B	�hB	�B	|�B	p�B	`�B	I�B	9XB	4�B	33B	'RB	oB	�B��B	 OB�B�B	�B�B�B�*B�B��B�GB��B	�B	�B	�B	xB	#TB	"B	 \B	#�B	$�B	&2B	)�B	-�B	5tB	@iB	E�B	S�B	n}B	�B	��B	�
B	�HB	��B	��B	�ZB	�$B	��B	�hB	�TB	�fB	�
B	�XB	�0B	��B	�)B	�B	�*B	��B	�kB	�B	�KB	�B	�eB	�B	��B	�QB	�CB	�)B	��B	��B	�]B	��B	��B	�CB	�iB	��B	�B	�TB	�B	�tB	�9B	��B	�lB	��B	��B	��B	�%B	��B	�B	�B	��B	��B	�B	��B	��B	�B	�tB	�%B	�?B	��B	�B	��B	�B	�&B	�IB	�#B	��B	��B	��B	��B	��B	�vB	�B	�;B	��B	�+B	�YB	ԕB	ΊB	�"B	҉B	�?B	�
B	�aB	��B	өB	�uB	өB	өB	�B	уB	��B	ϑB	ϫB	�pB	�<B	�\B	ΥB	�VB	�(B	�BB	��B	ΊB	�"B	�B	�VB	ΥB	�pB	��B	�<B	��B	��B	� B	�HB	�bB	ЗB	��B	�TB	ҽB	ҽB	�oB	� B	�:B	��B	�hB	��B	ҽB	�oB	уB	��B	�@B	��B	��B	��B	ԕB	޸B	�B	�CB	�CB	�B	ߊB	�B	߾B	��B	��B	��B	޸B	߾B	��B	��B	��B	�B	��B	�B	�B	�B	� B	�B	�tB	�nB	�nB	�tB	�tB	�,B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�0B	�B	�B	�B	�B	�B	�eB	�eB	��B	�6B	�B	��B	��B	�B	�}B	�iB	��B	�B	�5B	��B	� B	��B	� B	�iB	�B	��B	�B	��B	��B	�B	�/B	��B	�/B	��B	��B	��B	��B	�IB	��B	�oB	�'B	�B	�GB	�B	�-B	�9B	��B	��B	��B	�?B	�?B	�%B	�B	��B	��B	��B	��B	�2B	��B	�RB	�*B	�xB	��B	�qB	��B	�PB	�B	��B	�B	�B	�qB	�BB	��B	��B	�]B
 �B
 B
 �B
 �B
 �B
 B
;B
oB
oB
UB
 B
 �B	��B	�cB	�}B	��B	��B
  B
 �B
oB
;B
 B
 iB	��B	�HB
  B
 �B
�B
B
�B
B
B
�B
�B
�B
{B
aB
aB
�B
�B
�B
�B
3B
B
�B
�B
B
�B
�B
�B
�B
�B
�B
B
B
fB
fB
fB
	lB

#B

�B
�B
0B
dB
�B
PB
"B
�B
vB
.B
.B
B
�B
:B
:B
hB
�B
B
BB
�B
�B
�B
�B
TB
@B
[B
[B
�B
�B
2B
MB
�B
�B
B
�B
B
SB
�B
�B
B
�B
�B
�B
B
/B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
 BB
 �B
!B
!-B
!HB
!|B
!|B
!�B
"B
"4B
"4B
#TB
#:B
#nB
#TB
#TB
#�B
#�B
#�B
$@B
$ZB
$tB
$ZB
$tB
$@B
$tB
$�B
$�B
$tB
$�B
%B
%FB
%`B
%�B
&2B
&2B
&�B
&2B
&�B
'B
'mB
'�B
($B
(sB
(�B
)*B
)DB
)�B
)�B
)�B
)�B
)�B
)�B
*KB
)�B
)�B
)�B
(�B
'�B
'�B
(�B
)B
+kB
+�B
,B
,WB
+�B
+�B
,�B
,�B
-�B
./B
.�B
.�B
/ B
./B
./B
.�B
/5B
/�B
0B
/�B
0;B
0�B
1�B
1B
1'B
1[B
1�B
2�B
3�B
4�B
4�B
4�B
5�B
6FB
6�B
6�B
7�B
8RB
8�B
9rB
;�B
<�B
<�B
<�B
=B
>B
>]B
>�B
>�B
?B
?}B
?�B
?�B
@ B
@B
?�B
?�B
@4B
A B
B'B
A�B
BuB
B�B
C�B
C�B
CGB
C-B
C�B
C�B
EmB
E�B
E�B
E�B
E�B
F�B
GB
FYB
E�B
D�B
D�B
D�B
D�B
DgB
DMB
D3B
C�B
C�B
CGB
CB
CB
B�B
CB
CB
CB
CaB
C�B
C�B
C�B
C�B
DB
D3B
C�B
DB
DB
D�B
ESB
E�B
E�B
FB
FB
FtB
FtB
FtB
GzB
GEB
GzB
G�B
G�B
HKB
H�B
H�B
H�B
H�B
I�B
JXB
J�B
J�B
J�B
K�B
K�B
K�B
LdB
L~B
L~B
L~B
LJB
L�B
M�B
M6B
L�B
M�B
NpB
N�B
O�B
O�B
PB
O�B
O�B
PHB
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q B
QB
QNB
Q�B
Q�B
RTB
RTB
R�B
R�B
SuB
S�B
S�B
T,B
TaB
T�B
T�B
UMB
U�B
U�B
U�B
U�B
U�B
U�B
VB
V9B
VmB
V�B
V�B
W$B
W�B
W�B
X�B
YB
Y�B
Y�B
ZQB
Z�B
Z�B
Z�B
[=B
[qB
[�B
[�B
[�B
[�B
[�B
[�B
\)B
\CB
]dB
]�B
]�B
]�B
^�B
_VB
_�B
`\B
`\B
`vB
`�B
abB
a�B
a�B
a�B
cB
c:B
c B
cB
cB
c B
c B
c:B
c�B
d&B
dZB
d&B
d@B
d&B
d@B
dZB
d�B
e,B
e�B
f2B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
g�B
g�B
g�B
h$B
h$B
h$B
h�B
h�B
h�B
h�B
h�B
h�B
iDB
i*B
iDB
i�B
i�B
i�B
jB
j0B
jB
jeB
j�B
kB
k6B
k6B
kkB
k�B
k�B
lqB
l�B
l�B
l�B
mB
mB
mB
mB
mCB
mCB
mwB
m�B
nB
n/B
ncB
n}B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o5B
oiB
oOB
oiB
oOB
pUB
p;B
poB
p�B
p�B
p�B
p�B
qB
qvB
q�B
rB
rB
r-B
raB
r�B
r�B
sB
r�B
r�B
s3B
sMB
s�B
s�B
s�B
tB
t9B
tnB
t�B
t�B
t�B
t�B
t�B
u?B
utB
uZB
u?B
v`B
vzB
vzB
v�B
v�B
v�B
w2B
wLB
wLB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
xB
xlB
x�B
y$B
y>B
yrB
yrB
y�B
y�B
y�B
zB
y�B
y�B
y�B
zB
zDB
zxB
z�B
z�B
z�B
z�B
z�B
{0B
{0B
{�B
{�B
{B
|B
|PB
|�B
|�B
|�B
|�B
}B
}B
}<B
}�B
}�B
~(B
~BB
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
}B
�B
�B
�B
�B
� B
�OB
�OB
�B
�B
�oB
�;B
�oB
��B
��B
��B
��B
�'B
�'B
�AB
�[B
��B
��B
��B
�B
�aB
�GB
�aB
��B
��B
�{B
��B
�aB
��B
�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	:B	:B	 B	B	 B	B	 B	B	�B	B	�B	B	�B	�B	�B	B	�B	�B	�B	hB	�B	�B	hB	B	�B	4B	B	B	 B	�B	�B	�B	�B	HB	9B	C�B	O\B	M�B	\B	��B
#�B
��B
�tB
��B
�ZB@�BH�Bm�BwfB|B�'B��B��B��B��B�B�nB��B�5B��B��B�Bo�Bj�B\xB:*B$�BTB
�zB
��B
�B
�BB
�uB
p�B
JXB
 vB
	B
GB
 �B	�(B	��B	�B	�B	�uB	��B	�tB	�bB	��B	�hB	�B	|�B	p�B	`�B	I�B	9XB	4�B	33B	'RB	oB	�B��B	 OB�B�B	�B�B�B�*B�B��B�GB��B	�B	�B	�B	xB	#TB	"B	 \B	#�B	$�B	&2B	)�B	-�B	5tB	@iB	E�B	S�B	n}B	�B	��B	�
B	�HB	��B	��B	�ZB	�$B	��B	�hB	�TB	�fB	�
B	�XB	�0B	��B	�)B	�B	�*B	��B	�kB	�B	�KB	�B	�eB	�B	��B	�QB	�CB	�)B	��B	��B	�]B	��B	��B	�CB	�iB	��B	�B	�TB	�B	�tB	�9B	��B	�lB	��B	��B	��B	�%B	��B	�B	�B	��B	��B	�B	��B	��B	�B	�tB	�%B	�?B	��B	�B	��B	�B	�&B	�IB	�#B	��B	��B	��B	��B	��B	�vB	�B	�;B	��B	�+B	�YB	ԕB	ΊB	�"B	҉B	�?B	�
B	�aB	��B	өB	�uB	өB	өB	�B	уB	��B	ϑB	ϫB	�pB	�<B	�\B	ΥB	�VB	�(B	�BB	��B	ΊB	�"B	�B	�VB	ΥB	�pB	��B	�<B	��B	��B	� B	�HB	�bB	ЗB	��B	�TB	ҽB	ҽB	�oB	� B	�:B	��B	�hB	��B	ҽB	�oB	уB	��B	�@B	��B	��B	��B	ԕB	޸B	�B	�CB	�CB	�B	ߊB	�B	߾B	��B	��B	��B	޸B	߾B	��B	��B	��B	�B	��B	�B	�B	�B	� B	�B	�tB	�nB	�nB	�tB	�tB	�,B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�0B	�B	�B	�B	�B	�B	�eB	�eB	��B	�6B	�B	��B	��B	�B	�}B	�iB	��B	�B	�5B	��B	� B	��B	� B	�iB	�B	��B	�B	��B	��B	�B	�/B	��B	�/B	��B	��B	��B	��B	�IB	��B	�oB	�'B	�B	�GB	�B	�-B	�9B	��B	��B	��B	�?B	�?B	�%B	�B	��B	��B	��B	��B	�2B	��B	�RB	�*B	�xB	��B	�qB	��B	�PB	�B	��B	�B	�B	�qB	�BB	��B	��B	�]B
 �B
 B
 �B
 �B
 �B
 B
;B
oB
oB
UB
 B
 �B	��B	�cB	�}B	��B	��B
  B
 �B
oB
;B
 B
 iB	��B	�HB
  B
 �B
�B
B
�B
B
B
�B
�B
�B
{B
aB
aB
�B
�B
�B
�B
3B
B
�B
�B
B
�B
�B
�B
�B
�B
�B
B
B
fB
fB
fB
	lB

#B

�B
�B
0B
dB
�B
PB
"B
�B
vB
.B
.B
B
�B
:B
:B
hB
�B
B
BB
�B
�B
�B
�B
TB
@B
[B
[B
�B
�B
2B
MB
�B
�B
B
�B
B
SB
�B
�B
B
�B
�B
�B
B
/B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
 BB
 �B
!B
!-B
!HB
!|B
!|B
!�B
"B
"4B
"4B
#TB
#:B
#nB
#TB
#TB
#�B
#�B
#�B
$@B
$ZB
$tB
$ZB
$tB
$@B
$tB
$�B
$�B
$tB
$�B
%B
%FB
%`B
%�B
&2B
&2B
&�B
&2B
&�B
'B
'mB
'�B
($B
(sB
(�B
)*B
)DB
)�B
)�B
)�B
)�B
)�B
)�B
*KB
)�B
)�B
)�B
(�B
'�B
'�B
(�B
)B
+kB
+�B
,B
,WB
+�B
+�B
,�B
,�B
-�B
./B
.�B
.�B
/ B
./B
./B
.�B
/5B
/�B
0B
/�B
0;B
0�B
1�B
1B
1'B
1[B
1�B
2�B
3�B
4�B
4�B
4�B
5�B
6FB
6�B
6�B
7�B
8RB
8�B
9rB
;�B
<�B
<�B
<�B
=B
>B
>]B
>�B
>�B
?B
?}B
?�B
?�B
@ B
@B
?�B
?�B
@4B
A B
B'B
A�B
BuB
B�B
C�B
C�B
CGB
C-B
C�B
C�B
EmB
E�B
E�B
E�B
E�B
F�B
GB
FYB
E�B
D�B
D�B
D�B
D�B
DgB
DMB
D3B
C�B
C�B
CGB
CB
CB
B�B
CB
CB
CB
CaB
C�B
C�B
C�B
C�B
DB
D3B
C�B
DB
DB
D�B
ESB
E�B
E�B
FB
FB
FtB
FtB
FtB
GzB
GEB
GzB
G�B
G�B
HKB
H�B
H�B
H�B
H�B
I�B
JXB
J�B
J�B
J�B
K�B
K�B
K�B
LdB
L~B
L~B
L~B
LJB
L�B
M�B
M6B
L�B
M�B
NpB
N�B
O�B
O�B
PB
O�B
O�B
PHB
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q B
QB
QNB
Q�B
Q�B
RTB
RTB
R�B
R�B
SuB
S�B
S�B
T,B
TaB
T�B
T�B
UMB
U�B
U�B
U�B
U�B
U�B
U�B
VB
V9B
VmB
V�B
V�B
W$B
W�B
W�B
X�B
YB
Y�B
Y�B
ZQB
Z�B
Z�B
Z�B
[=B
[qB
[�B
[�B
[�B
[�B
[�B
[�B
\)B
\CB
]dB
]�B
]�B
]�B
^�B
_VB
_�B
`\B
`\B
`vB
`�B
abB
a�B
a�B
a�B
cB
c:B
c B
cB
cB
c B
c B
c:B
c�B
d&B
dZB
d&B
d@B
d&B
d@B
dZB
d�B
e,B
e�B
f2B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
g�B
g�B
g�B
h$B
h$B
h$B
h�B
h�B
h�B
h�B
h�B
h�B
iDB
i*B
iDB
i�B
i�B
i�B
jB
j0B
jB
jeB
j�B
kB
k6B
k6B
kkB
k�B
k�B
lqB
l�B
l�B
l�B
mB
mB
mB
mB
mCB
mCB
mwB
m�B
nB
n/B
ncB
n}B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o5B
oiB
oOB
oiB
oOB
pUB
p;B
poB
p�B
p�B
p�B
p�B
qB
qvB
q�B
rB
rB
r-B
raB
r�B
r�B
sB
r�B
r�B
s3B
sMB
s�B
s�B
s�B
tB
t9B
tnB
t�B
t�B
t�B
t�B
t�B
u?B
utB
uZB
u?B
v`B
vzB
vzB
v�B
v�B
v�B
w2B
wLB
wLB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
xB
xlB
x�B
y$B
y>B
yrB
yrB
y�B
y�B
y�B
zB
y�B
y�B
y�B
zB
zDB
zxB
z�B
z�B
z�B
z�B
z�B
{0B
{0B
{�B
{�B
{B
|B
|PB
|�B
|�B
|�B
|�B
}B
}B
}<B
}�B
}�B
~(B
~BB
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
}B
�B
�B
�B
�B
� B
�OB
�OB
�B
�B
�oB
�;B
�oB
��B
��B
��B
��B
�'B
�'B
�AB
�[B
��B
��B
��B
�B
�aB
�GB
�aB
��B
��B
�{B
��B
�aB
��B
�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104931  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174325  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174326  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174326                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024333  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024333  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                