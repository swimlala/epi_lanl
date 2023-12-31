CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-11-14T06:43:41Z creation;2022-11-14T06:43:42Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20221114064341  20221114070522  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @���,�W1   @��ȥ�}�@./��v��c��\(��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A!��A@  A`  A�  A�  A�  A���A���A�  A�33A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx��B~ffB�  B���B�  B�  B�  B�  B�  B�  B�  B�ffB�ffB���B�  B�  B�  B�  B�ffB�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C�C  C  C�fC
  C  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(�C)��C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dny�Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@q�@���@���A{A<z�A\z�A|z�A�=qA�=qA�
>A�
>A�=qA�p�A�p�A�=qB�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B}�B��\B�\)B��\B��\B��\B��\B��\B��\B��\B���B���B�\)B��\B��\B��\B��\B���B���B�\)BϏ\Bӏ\B׏\Bۏ\Bߏ\B�\B�\B�\B�\B�\B��\B��\B�C�HCǮCǮC�C	ǮCǮCǮCǮCǮCǮCǮCǮC�CǮCǮCǮC!ǮC#ǮC%ǮC'�HC)�{C+�C-ǮC/ǮC1ǮC3ǮC5ǮC7ǮC9ǮC;ǮC=ǮC?ǮCAǮCCǮCEǮCGǮCIǮCKǮCMǮCOǮCQǮCSǮCUǮCW�HCYǮC[ǮC]ǮC_ǮCaǮCcǮCeǮCgǮCiǮCkǮCmǮCoǮCqǮCsǮCuǮCwǮCyǮC{ǮC}ǮCǮC���C���C���C���C��
C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��
C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��
C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D q�D ��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D	q�D	��D
q�D
��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D q�D ��D!q�D!��D"q�D"��D#q�D#��D$q�D$��D%q�D%��D&q�D&��D'q�D'��D(q�D(��D)q�D)��D*q�D*��D+q�D+��D,q�D,��D-q�D-��D.q�D.��D/q�D/��D0q�D0��D1q�D1��D2q�D2��D3q�D3��D4q�D4��D5q�D5��D6q�D6��D7q�D7��D8q�D8��D9q�D9��D:q�D:��D;q�D;��D<q�D<��D=q�D=��D>q�D>��D?q�D?��D@q�D@��DAq�DA��DBq�DB��DCq�DC��DDq�DD��DEq�DE��DFq�DF��DGq�DG��DHq�DH�RDIq�DI��DJq�DJ��DKq�DK��DLq�DL��DMq�DM��DNq�DN��DOq�DO��DPq�DP��DQq�DQ��DRq�DR��DSq�DS��DTq�DT��DUq�DU��DVq�DV��DWq�DW��DXq�DX��DYq�DY��DZq�DZ��D[q�D[��D\q�D\��D]q�D]��D^q�D^��D_q�D_��D`q�D`��Daq�Da��Dbq�Db��Dcq�Dc��Ddq�Dd��Deq�De��Dfq�Df��Dgq�Dg��Dhq�Dh��Diq�Di��Djq�Dj��Dkq�Dk��Dlq�Dl��Dmq�Dm��Dnk�Dn��Doq�Do��Dpq�Dp��Dqq�Dq��Drq�Dr��Dsq�Ds��Dtq�Dt��Duq�Du��Dvq�Dv��Dwq�Dw��Dxq�Dx��Dyq�Dy��Dzq�Dz��D{q�D{��D|q�D|��D}q�D}��D~q�D~��Dq�D��D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�<)D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�e�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aٳ�Aٰ�Aپ�A���A��OA��Aٿ}A��aA��mAٸ�Aٵ?Aٶ�Aٳ�Aٟ�A٠�A�l"A�aA�PHA�O�A�OBA�MA�L�A�K�A�K^A�HKA�HKA�H�A�JXA�FtA�H�A�<jA�(A���Aח�A�N�A�Q�A�cA��A��;A�U�A�NpA�A͆�A��A��+A�Z�A�YA��cA�ÖA���A��A��A�ƨA�\]A���A�U�A� �A�$@A��tA��%A�&�A�h>A���A�	�A�E�A�^jA���A��A��DA���A���A�SA���A�
�A�ÖA��1A���A�ޞA���A��<A���A�c A�r|A���A��A{MAw�At�Ar�}Aqu�Ao�Ah|�Ad�>AcC-Aa|A`�cA\ \ATy>ARkQAP�`AOuAL}�AK�AH��AE�\A?XA=��A9��A7��A6p;A5FtA3'RA2��A2�A1-�A/ѷA/:�A/�A-�QA,x�A*��A)-�A(J#A'ƨA$[�A#%FA"U�A"��A"��A"��A# iA#1�A#l"A#!A"�qA"OvA!�PA �sA��A�LAaAߤA(�AɆA�fAZ�A�A.IA"�A��A��A��A*�A��A�LA#:A�$A��AIRA_pAm�A�A33A{JA�&A$tA��A�A�Ao�AW?A��AZA6�A�}A�A��A�AF�A��A/A

=A	h�A	C�A	D�A��A��A<6A(A��Af�A��A��A��A�AjAS�A>�A5�A7A��A�PA�6A��A�A��A��A ��A �@�c�@��@��?@��@�C�@�M�@�J@���@���@�B�@�ff@���@�C�@��@��@��8@��A@�T�@��@��@@�4@��@�6@�D�@��@�j�@�ߤ@�J@��@���@�Ɇ@�|�@�P�@��@��K@�j@��+@�g�@�5�@��?@�H@��@�T�@��@�ی@��+@߸�@�8@��@��@ݓ@���@ܿ�@܁o@�Z�@�C�@۫�@�7L@�
=@ڴ9@��&@���@�U2@�8@��j@��P@��M@�m�@�|@Ұ�@���@�&�@��,@�M@�m]@��@Ό@�YK@͆�@�@�ѷ@��@���@�(@���@���@�A�@ʎ�@�0�@ʞ�@�GE@��]@���@ȑ�@�L�@�Y@��@�=@ǋ�@Ǖ�@�o�@�Dg@ƪe@�M@�y�@��@���@Ĺ�@Ĝx@�~�@�7�@���@�.I@¶�@�V@��@���@��~@�F�@�S@�ی@��@�_�@�@��@�y�@�)_@���@�Ta@���@�zx@��@��@��@�|@�P�@�S@��@�c @��D@�hs@���@�W�@�~@���@��3@��P@���@�E�@��V@�}�@�N<@���@��$@�\�@�@���@�9�@��@���@��s@�Ta@�J@�~�@��@��U@���@���@�h�@�e@��@�\�@�)_@��,@��@�Xy@�.�@��@���@��@��#@���@���@�A�@�,�@�C@��@���@��@��#@���@�e�@�?}@�7L@��)@��o@��@���@�t�@�4@��]@�a|@��g@��k@���@�c�@�ߤ@�|�@�&�@���@���@�~�@�Dg@�ȴ@��@���@�_p@��@�:�@��.@���@���@�+@���@�H@��@���@�^�@�@�Ĝ@�� @�xl@�a|@�Ta@�e@��@��m@���@�8�@�u%@�$�@��g@���@�?}@�$t@� \@��<@�l�@��@���@�a@�E9@�4�@�1�@��@���@��y@��)@��@�:*@��@��@�ݘ@��'@�j�@�4�@��@�Ɇ@���@���@�v�@�%�@��a@���@�dZ@�Vm@�E9@�)_@��@�[�@��@���@�|@�2a@�V@��j@��@��@�u%@�!@��9@��~@�W?@�8�@��@��M@���@���@��I@�O@���@�y�@�P�@�V@���@���@�c�@�A�@�ϫ@�rG@�q@��P@��R@�d�@�@���@�c@�[W@��@��@��_@�a|@�<�@�4@���@���@��@��y@��]@��j@���@�_�@���@�ԕ@���@�}�@�<6@��@���@�xl@�Z�@�)�@��@��@���@�u�@��"@���@��@��@�m�@�($@���@�33@��@��@��`@��h@�~�@�-@خ@�{@
=@~�@}��@{�m@y�@x��@xq@x,=@wb�@w
=@v�s@vn�@u�@u@u \@u:�@tѷ@t�_@tc�@s��@s��@s�@r�\@r$�@q`B@q\�@q+�@p��@pQ�@p@o� @o��@oiD@o&@n��@n�X@n�x@nW�@m?}@lA�@k��@k�@j��@ju@i}�@i+�@h��@hy>@hx@gy�@g�@f�]@f��@fȴ@fȴ@f��@fR�@f�@e��@e��@e@d��@d~@c��@c�@cF�@b�8@b�s@b�R@bq�@b$�@a��@a5�@`�	@`�9@`>B@_{J@_Y@_Y@_�@_@^�@^?@]�@\�f@\r�@\Ft@\7@[��@[8@Zs�@Z!�@Y�@Y�@Y4@Y+@X�	@X��@Xj@X,=@W˒@WJ#@V��@V1�@V!�@U�9@Uhs@U<6@U�@T�`@T��@T/�@S�&@SZ�@Rߤ@R��@R��@RkQ@RH�@Ru@Q��@Q8�@P��@P�u@P��@P|�@P�@O��@Og�@O�@N��@NGE@M�>@MQ�@M�@L֡@Ll"@Lx@K�A@K��@K��@K.I@J�s@Ju%@Je@I�d@I#�@H�O@H"h@H  @G��@G"�@F��@F��@Fxl@F �@Ek�@EG�@E%F@D��@DS�@D�@C�;@C��@Cv`@C�@B��@Bq�@A��@A�7@AO�@A+�@@�5@@��@@g8@@b@?��@?x@?�@>ں@>҉@>YK@=�X@=V@<�$@<h�@<@;��@;��@;a@:��@:͟@:��@:��@:Q@:8�@9�Z@9�^@9��@9^�@95�@9�@8��@8~@7��@7X�@6�B@6V@68�@6	@5�9@5�@4H@3�F@3��@333@2�X@2Ov@1�=@1/@0�9@0�D@0h�@0Ft@0�@/�@/�@/8@/33@/&@.�@.d�@.Ta@.?@.+k@-�D@-�C@-u�@,�@,j@,I�@,,=@+�;@+�}@+��@+a@*ߤ@*��@*($@)�>@)L�@(�5@(�$@(Xy@(7@(�@'�]@'��@'b�@')_@'�@&��@&��@&v�@&$�@%��@%��@%j@%4@%�@$��@$N�@$/�@$'R@$�@#��@#��@#�P@#o�@#P�@#�@"�@"kQ@"{@!�@!-w@!�@ ��@ ��@ ��@ g8@ 1@�@�:@Z�@+@��@�@�!@��@W�@�@u@��@�H@�'@hs@7L@ \@��@��@D�@6@�+@ƨ@�@qv@F�@�@ں@�@GE@	@��@ϫ@��@L�@4@�@�@�D@?�@�]@��@Z�@9�@�@��@�@�A@J�@$�@�@�o@�@�@��@�h@u�@L�@�@��@��@��@�I@��@�Y@*�@�W@��@v`@v`@g�@e�@a@_p@]�@)_@�@��@�+@h
@@�@�Z@��@��@�@��@p�@@�z@-�@�@��@�@�w@\)@)_@�@�@��@�H@�6@Z�@5?@!�@@�@��@`B@N<@(�@�@֡@�$@��@�I@�D@��@`�@M@H@<�@1'@,=@b@��@�@�&@�V@t�@H�@�@
��@
�'@
��@
�R@
��@
}V@
H�@
#:@
#:@
+k@
O1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aٳ�Aٰ�Aپ�A���A��OA��Aٿ}A��aA��mAٸ�Aٵ?Aٶ�Aٳ�Aٟ�A٠�A�l"A�aA�PHA�O�A�OBA�MA�L�A�K�A�K^A�HKA�HKA�H�A�JXA�FtA�H�A�<jA�(A���Aח�A�N�A�Q�A�cA��A��;A�U�A�NpA�A͆�A��A��+A�Z�A�YA��cA�ÖA���A��A��A�ƨA�\]A���A�U�A� �A�$@A��tA��%A�&�A�h>A���A�	�A�E�A�^jA���A��A��DA���A���A�SA���A�
�A�ÖA��1A���A�ޞA���A��<A���A�c A�r|A���A��A{MAw�At�Ar�}Aqu�Ao�Ah|�Ad�>AcC-Aa|A`�cA\ \ATy>ARkQAP�`AOuAL}�AK�AH��AE�\A?XA=��A9��A7��A6p;A5FtA3'RA2��A2�A1-�A/ѷA/:�A/�A-�QA,x�A*��A)-�A(J#A'ƨA$[�A#%FA"U�A"��A"��A"��A# iA#1�A#l"A#!A"�qA"OvA!�PA �sA��A�LAaAߤA(�AɆA�fAZ�A�A.IA"�A��A��A��A*�A��A�LA#:A�$A��AIRA_pAm�A�A33A{JA�&A$tA��A�A�Ao�AW?A��AZA6�A�}A�A��A�AF�A��A/A

=A	h�A	C�A	D�A��A��A<6A(A��Af�A��A��A��A�AjAS�A>�A5�A7A��A�PA�6A��A�A��A��A ��A �@�c�@��@��?@��@�C�@�M�@�J@���@���@�B�@�ff@���@�C�@��@��@��8@��A@�T�@��@��@@�4@��@�6@�D�@��@�j�@�ߤ@�J@��@���@�Ɇ@�|�@�P�@��@��K@�j@��+@�g�@�5�@��?@�H@��@�T�@��@�ی@��+@߸�@�8@��@��@ݓ@���@ܿ�@܁o@�Z�@�C�@۫�@�7L@�
=@ڴ9@��&@���@�U2@�8@��j@��P@��M@�m�@�|@Ұ�@���@�&�@��,@�M@�m]@��@Ό@�YK@͆�@�@�ѷ@��@���@�(@���@���@�A�@ʎ�@�0�@ʞ�@�GE@��]@���@ȑ�@�L�@�Y@��@�=@ǋ�@Ǖ�@�o�@�Dg@ƪe@�M@�y�@��@���@Ĺ�@Ĝx@�~�@�7�@���@�.I@¶�@�V@��@���@��~@�F�@�S@�ی@��@�_�@�@��@�y�@�)_@���@�Ta@���@�zx@��@��@��@�|@�P�@�S@��@�c @��D@�hs@���@�W�@�~@���@��3@��P@���@�E�@��V@�}�@�N<@���@��$@�\�@�@���@�9�@��@���@��s@�Ta@�J@�~�@��@��U@���@���@�h�@�e@��@�\�@�)_@��,@��@�Xy@�.�@��@���@��@��#@���@���@�A�@�,�@�C@��@���@��@��#@���@�e�@�?}@�7L@��)@��o@��@���@�t�@�4@��]@�a|@��g@��k@���@�c�@�ߤ@�|�@�&�@���@���@�~�@�Dg@�ȴ@��@���@�_p@��@�:�@��.@���@���@�+@���@�H@��@���@�^�@�@�Ĝ@�� @�xl@�a|@�Ta@�e@��@��m@���@�8�@�u%@�$�@��g@���@�?}@�$t@� \@��<@�l�@��@���@�a@�E9@�4�@�1�@��@���@��y@��)@��@�:*@��@��@�ݘ@��'@�j�@�4�@��@�Ɇ@���@���@�v�@�%�@��a@���@�dZ@�Vm@�E9@�)_@��@�[�@��@���@�|@�2a@�V@��j@��@��@�u%@�!@��9@��~@�W?@�8�@��@��M@���@���@��I@�O@���@�y�@�P�@�V@���@���@�c�@�A�@�ϫ@�rG@�q@��P@��R@�d�@�@���@�c@�[W@��@��@��_@�a|@�<�@�4@���@���@��@��y@��]@��j@���@�_�@���@�ԕ@���@�}�@�<6@��@���@�xl@�Z�@�)�@��@��@���@�u�@��"@���@��@��@�m�@�($@���@�33@��@��@��`@��h@�~�@�-@خ@�{@
=@~�@}��@{�m@y�@x��@xq@x,=@wb�@w
=@v�s@vn�@u�@u@u \@u:�@tѷ@t�_@tc�@s��@s��@s�@r�\@r$�@q`B@q\�@q+�@p��@pQ�@p@o� @o��@oiD@o&@n��@n�X@n�x@nW�@m?}@lA�@k��@k�@j��@ju@i}�@i+�@h��@hy>@hx@gy�@g�@f�]@f��@fȴ@fȴ@f��@fR�@f�@e��@e��@e@d��@d~@c��@c�@cF�@b�8@b�s@b�R@bq�@b$�@a��@a5�@`�	@`�9@`>B@_{J@_Y@_Y@_�@_@^�@^?@]�@\�f@\r�@\Ft@\7@[��@[8@Zs�@Z!�@Y�@Y�@Y4@Y+@X�	@X��@Xj@X,=@W˒@WJ#@V��@V1�@V!�@U�9@Uhs@U<6@U�@T�`@T��@T/�@S�&@SZ�@Rߤ@R��@R��@RkQ@RH�@Ru@Q��@Q8�@P��@P�u@P��@P|�@P�@O��@Og�@O�@N��@NGE@M�>@MQ�@M�@L֡@Ll"@Lx@K�A@K��@K��@K.I@J�s@Ju%@Je@I�d@I#�@H�O@H"h@H  @G��@G"�@F��@F��@Fxl@F �@Ek�@EG�@E%F@D��@DS�@D�@C�;@C��@Cv`@C�@B��@Bq�@A��@A�7@AO�@A+�@@�5@@��@@g8@@b@?��@?x@?�@>ں@>҉@>YK@=�X@=V@<�$@<h�@<@;��@;��@;a@:��@:͟@:��@:��@:Q@:8�@9�Z@9�^@9��@9^�@95�@9�@8��@8~@7��@7X�@6�B@6V@68�@6	@5�9@5�@4H@3�F@3��@333@2�X@2Ov@1�=@1/@0�9@0�D@0h�@0Ft@0�@/�@/�@/8@/33@/&@.�@.d�@.Ta@.?@.+k@-�D@-�C@-u�@,�@,j@,I�@,,=@+�;@+�}@+��@+a@*ߤ@*��@*($@)�>@)L�@(�5@(�$@(Xy@(7@(�@'�]@'��@'b�@')_@'�@&��@&��@&v�@&$�@%��@%��@%j@%4@%�@$��@$N�@$/�@$'R@$�@#��@#��@#�P@#o�@#P�@#�@"�@"kQ@"{@!�@!-w@!�@ ��@ ��@ ��@ g8@ 1@�@�:@Z�@+@��@�@�!@��@W�@�@u@��@�H@�'@hs@7L@ \@��@��@D�@6@�+@ƨ@�@qv@F�@�@ں@�@GE@	@��@ϫ@��@L�@4@�@�@�D@?�@�]@��@Z�@9�@�@��@�@�A@J�@$�@�@�o@�@�@��@�h@u�@L�@�@��@��@��@�I@��@�Y@*�@�W@��@v`@v`@g�@e�@a@_p@]�@)_@�@��@�+@h
@@�@�Z@��@��@�@��@p�@@�z@-�@�@��@�@�w@\)@)_@�@�@��@�H@�6@Z�@5?@!�@@�@��@`B@N<@(�@�@֡@�$@��@�I@�D@��@`�@M@H@<�@1'@,=@b@��@�@�&@�V@t�@H�@�@
��@
�'@
��@
�R@
��@
}V@
H�@
#:@
#:@
+k@
O1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
nIB
n�B
nB
n/B
nIB
nIB
ncB
n}B
n/B
nB
n/B
nB
nB
m�B
m�B
mwB
m)B
mB
mB
m)B
mCB
m]B
mwB
ncB
o5B
oiB
o�B
o�B
p�B
r�B
��B
��Bv+B�9B��B�VB�BB�!B�0B��B��B�7BخB��B��B��B� B�B�B	�BB�B-�B1[B'�B$&B#�BOBeB�B�B�B�B�eB�BƨB�UB�FB�zB��B��B��BoOBK�B-]BpB
޸B
��B
��B
�QB
��B
b�B
M�B
5�B
�B	�yB	�9B	�B	��B	��B	��B	��B	p!B	h$B	\�B	UgB	FB	 B	�B	�B	�B	#B	G+B	HfB	7�B	hB	;B�B��B�AB�B�KB��B��B�DB�aB�9B�zB�B�MB��B�B	MB	"�B	�B	B	!�B	;B	@�B	DB	K�B	T�B	]�B	a�B	f�B	nIB	o5B	h$B	d@B	b�B	ffB	w�B	�GB	�uB	zDB	o�B	ffB	YB	L~B	D�B	E�B	E�B	CGB	K�B	Z7B	[qB	i�B	n�B	m]B	j�B	d&B	c B	^B	[�B	YKB	QhB	I�B	J�B	L~B	Q B	X�B	jB	p�B	��B	�zB	�7B	� B	�$B	�OB	�'B	�'B	�CB	�IB	��B	�B	�-B	��B	�B	��B	�?B	��B	�UB	�B	��B	�}B	��B	��B	��B	��B	�QB	�B	��B	�+B	��B	�B	��B	��B	�B	�B	�	B	�^B	��B	�VB	��B	�XB	��B	�qB	�}B	��B	��B	��B	��B	��B	��B	��B	�B	�PB	��B	�qB	� B	��B	�B	��B	B	��B	�gB	�aB	�SB	�KB	�B	�=B	�=B	�7B	�1B	��B	ƨB	�tB	�?B	��B	ȚB	�lB	�XB	�DB	��B	��B	�^B	�B	�pB	�(B	ЗB	ӏB	ԕB	��B	ՁB	��B	�$B	�
B	�B	�QB	�	B	�7B	ؓB	��B	ԯB	ϑB	�.B	��B	��B	�:B	��B	��B	�NB	�4B	ңB	�@B	��B	��B	�B	��B	ӏB	ԕB	��B	�KB	��B	چB	��B	��B	�NB	�B	�B	�8B	��B	�B	�B	�B	��B	��B	�B	�B	�B	�|B	�hB	�FB	�FB	�+B	�+B	�zB	��B	��B	��B	�rB	��B	�XB	�	B	�rB	�DB	��B	�xB	��B	�dB	�dB	�B	��B	��B	�6B	��B	�PB	��B	�B	�VB	��B	�]B	�]B	�B	�(B	�BB	�BB	�(B	�BB	�wB	��B	��B	��B
 B
  B	��B
 B
 B
oB
uB
AB
uB
'B
�B
GB
{B
�B
�B
�B
�B
3B
B
�B
MB
3B
�B
�B
B
�B
�B
�B
tB
?B
?B
�B
B
+B
EB
+B
+B
B
+B
+B
�B
zB
�B
+B
fB
�B
KB
�B
	7B
	�B

	B

#B
	�B
	�B
	�B
	lB
	�B

�B

#B
	�B
^B
�B
�B
�B
DB
B
�B
6B
�B
�B
�B
�B
DB
)B
)B
DB
�B
dB
6B
�B
�B
6B
�B
"B
�B
�B
�B
�B
(B
BB
(B
�B
�B
\B
�B
.B
B
B
�B
�B
�B
�B
�B
�B
�B
B
9B
�B
�B
�B
�B
�B
?B
$B
YB
�B
_B
yB
�B
yB
B
KB
�B
B
�B
�B
�B
�B
=B
�B
�B
�B
�B
�B
�B
)B
�B
dB
dB
�B
B
B
�B
�B
�B
�B
;B
�B
 BB
 �B
 �B
!bB
!|B
!|B
!bB
!�B
"�B
#�B
$�B
%�B
%FB
%`B
$�B
%`B
%`B
&�B
'B
'�B
'�B
'�B
(XB
(�B
(�B
)_B
)yB
*B
*KB
*B
*�B
*�B
+B
+�B
+�B
,WB
,qB
,qB
,�B
,�B
,�B
-�B
-�B
-�B
.B
.}B
.�B
/iB
/iB
/�B
/�B
/�B
0B
/�B
0�B
0�B
0�B
0�B
1vB
1[B
1�B
1�B
2aB
2�B
3MB
4B
4nB
4�B
5tB
5tB
5ZB
5�B
5�B
6zB
6`B
4�B
4TB
4B
49B
4�B
5?B
6�B
8B
9XB
9�B
;B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
=<B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>B
>B
>]B
?.B
?�B
?}B
?�B
@B
?}B
>�B
>�B
=�B
=VB
=VB
=�B
=�B
>]B
>�B
>]B
>BB
?cB
@OB
A;B
A�B
D�B
EB
EB
ESB
E�B
F�B
F�B
G+B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
HKB
H�B
H�B
IB
IlB
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L~B
L�B
L�B
MB
MB
M�B
NB
N<B
NpB
N�B
N�B
N�B
N�B
OBB
O(B
O\B
O\B
O�B
P}B
P�B
P}B
P�B
QB
QB
Q B
Q4B
QhB
Q�B
Q�B
RTB
R�B
R�B
R�B
R�B
R�B
S&B
SuB
S�B
TB
TB
TB
S�B
TFB
T�B
T�B
T�B
T�B
UMB
UgB
U�B
U�B
VB
VSB
V�B
V�B
V�B
V�B
V�B
V�B
W$B
W?B
WsB
W�B
XB
XyB
XyB
X_B
Y1B
YKB
Y1B
YB
Y�B
ZQB
ZQB
ZkB
Z�B
Z�B
[#B
[#B
[WB
[�B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
]B
]IB
]dB
]�B
]�B
^B
^jB
^�B
^OB
^�B
_!B
_�B
_�B
_�B
`'B
`\B
`\B
`vB
`�B
`�B
`�B
aB
aB
a-B
abB
a|B
a�B
a�B
a�B
a�B
a�B
bhB
bhB
b�B
b�B
cTB
cTB
cTB
cTB
c�B
d�B
d�B
d�B
d�B
eB
e,B
e�B
fB
f�B
f�B
f�B
f�B
f�B
f�B
gRB
g�B
g�B
g�B
h$B
hsB
h�B
hsB
h�B
h�B
h�B
iB
i�B
i�B
jB
j0B
jB
jeB
jB
j�B
kB
k6B
k�B
k�B
l"B
lWB
l�B
l�B
l�B
l�B
l�B
m]B
mwB
m�B
m�B
m�B
m�B
n/B
nIB
n}B
n�B
n�B
o B
oOB
pB
pUB
poB
poB
p�B
p�B
qB
qB
q'B
qAB
qvB
q�B
q�B
q�B
rB
raB
r�B
r�B
r�B
sB
shB
s�B
tB
t9B
t�B
t�B
t�B
uB
t�B
uB
u%B
uZB
uZB
utB
u�B
u�B
u�B
u�B
u�B
u�B
v+B
vzB
vzB
v�B
v�B
v�B
v�B
wB
wLB
wLB
w�B
w�B
xB
xB
xlB
x�B
x�B
y	B
yXB
y>B
yrB
y�B
y�B
y�B
z*B
zB
z*B
z^B
z�B
z�B
z�B
z�B
{B
{0B
{JB
{0B
{JB
{JB
{dB
{B
|B
|6B
|6B
|PB
|PB
|PB
|jB
|�B
|�B
}"B
}qB
}�B
}�B
~BB
~�B
~�B
~�B
HB
HB
�B
� B
� B
�B
��B
��B
��B
��B
��B
��B
�;B
��B
�[B
�AB
�[B
�[B
��B
��B
�GB
�aB
�{B
�{B
��B
��B
�B
�3B
�MB
�MB
��B
��B
�B
�B
�SB
�mB
��B
��B
��B
��B
�B
��B
�?B
�YB
�YB
�YB
�YB
�YB
�tB
�tB
�tB
��B
��B
�tB
��B
��B
��B
�+B
�+B
�EB
�EB
�EB
�zB
��B
��B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
nIB
n�B
nB
n/B
nIB
nIB
ncB
n}B
n/B
nB
n/B
nB
nB
m�B
m�B
mwB
m)B
mB
mB
m)B
mCB
m]B
mwB
ncB
o5B
oiB
o�B
o�B
p�B
r�B
��B
��Bv+B�9B��B�VB�BB�!B�0B��B��B�7BخB��B��B��B� B�B�B	�BB�B-�B1[B'�B$&B#�BOBeB�B�B�B�B�eB�BƨB�UB�FB�zB��B��B��BoOBK�B-]BpB
޸B
��B
��B
�QB
��B
b�B
M�B
5�B
�B	�yB	�9B	�B	��B	��B	��B	��B	p!B	h$B	\�B	UgB	FB	 B	�B	�B	�B	#B	G+B	HfB	7�B	hB	;B�B��B�AB�B�KB��B��B�DB�aB�9B�zB�B�MB��B�B	MB	"�B	�B	B	!�B	;B	@�B	DB	K�B	T�B	]�B	a�B	f�B	nIB	o5B	h$B	d@B	b�B	ffB	w�B	�GB	�uB	zDB	o�B	ffB	YB	L~B	D�B	E�B	E�B	CGB	K�B	Z7B	[qB	i�B	n�B	m]B	j�B	d&B	c B	^B	[�B	YKB	QhB	I�B	J�B	L~B	Q B	X�B	jB	p�B	��B	�zB	�7B	� B	�$B	�OB	�'B	�'B	�CB	�IB	��B	�B	�-B	��B	�B	��B	�?B	��B	�UB	�B	��B	�}B	��B	��B	��B	��B	�QB	�B	��B	�+B	��B	�B	��B	��B	�B	�B	�	B	�^B	��B	�VB	��B	�XB	��B	�qB	�}B	��B	��B	��B	��B	��B	��B	��B	�B	�PB	��B	�qB	� B	��B	�B	��B	B	��B	�gB	�aB	�SB	�KB	�B	�=B	�=B	�7B	�1B	��B	ƨB	�tB	�?B	��B	ȚB	�lB	�XB	�DB	��B	��B	�^B	�B	�pB	�(B	ЗB	ӏB	ԕB	��B	ՁB	��B	�$B	�
B	�B	�QB	�	B	�7B	ؓB	��B	ԯB	ϑB	�.B	��B	��B	�:B	��B	��B	�NB	�4B	ңB	�@B	��B	��B	�B	��B	ӏB	ԕB	��B	�KB	��B	چB	��B	��B	�NB	�B	�B	�8B	��B	�B	�B	�B	��B	��B	�B	�B	�B	�|B	�hB	�FB	�FB	�+B	�+B	�zB	��B	��B	��B	�rB	��B	�XB	�	B	�rB	�DB	��B	�xB	��B	�dB	�dB	�B	��B	��B	�6B	��B	�PB	��B	�B	�VB	��B	�]B	�]B	�B	�(B	�BB	�BB	�(B	�BB	�wB	��B	��B	��B
 B
  B	��B
 B
 B
oB
uB
AB
uB
'B
�B
GB
{B
�B
�B
�B
�B
3B
B
�B
MB
3B
�B
�B
B
�B
�B
�B
tB
?B
?B
�B
B
+B
EB
+B
+B
B
+B
+B
�B
zB
�B
+B
fB
�B
KB
�B
	7B
	�B

	B

#B
	�B
	�B
	�B
	lB
	�B

�B

#B
	�B
^B
�B
�B
�B
DB
B
�B
6B
�B
�B
�B
�B
DB
)B
)B
DB
�B
dB
6B
�B
�B
6B
�B
"B
�B
�B
�B
�B
(B
BB
(B
�B
�B
\B
�B
.B
B
B
�B
�B
�B
�B
�B
�B
�B
B
9B
�B
�B
�B
�B
�B
?B
$B
YB
�B
_B
yB
�B
yB
B
KB
�B
B
�B
�B
�B
�B
=B
�B
�B
�B
�B
�B
�B
)B
�B
dB
dB
�B
B
B
�B
�B
�B
�B
;B
�B
 BB
 �B
 �B
!bB
!|B
!|B
!bB
!�B
"�B
#�B
$�B
%�B
%FB
%`B
$�B
%`B
%`B
&�B
'B
'�B
'�B
'�B
(XB
(�B
(�B
)_B
)yB
*B
*KB
*B
*�B
*�B
+B
+�B
+�B
,WB
,qB
,qB
,�B
,�B
,�B
-�B
-�B
-�B
.B
.}B
.�B
/iB
/iB
/�B
/�B
/�B
0B
/�B
0�B
0�B
0�B
0�B
1vB
1[B
1�B
1�B
2aB
2�B
3MB
4B
4nB
4�B
5tB
5tB
5ZB
5�B
5�B
6zB
6`B
4�B
4TB
4B
49B
4�B
5?B
6�B
8B
9XB
9�B
;B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
=<B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>B
>B
>]B
?.B
?�B
?}B
?�B
@B
?}B
>�B
>�B
=�B
=VB
=VB
=�B
=�B
>]B
>�B
>]B
>BB
?cB
@OB
A;B
A�B
D�B
EB
EB
ESB
E�B
F�B
F�B
G+B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
HKB
H�B
H�B
IB
IlB
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L~B
L�B
L�B
MB
MB
M�B
NB
N<B
NpB
N�B
N�B
N�B
N�B
OBB
O(B
O\B
O\B
O�B
P}B
P�B
P}B
P�B
QB
QB
Q B
Q4B
QhB
Q�B
Q�B
RTB
R�B
R�B
R�B
R�B
R�B
S&B
SuB
S�B
TB
TB
TB
S�B
TFB
T�B
T�B
T�B
T�B
UMB
UgB
U�B
U�B
VB
VSB
V�B
V�B
V�B
V�B
V�B
V�B
W$B
W?B
WsB
W�B
XB
XyB
XyB
X_B
Y1B
YKB
Y1B
YB
Y�B
ZQB
ZQB
ZkB
Z�B
Z�B
[#B
[#B
[WB
[�B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
]B
]IB
]dB
]�B
]�B
^B
^jB
^�B
^OB
^�B
_!B
_�B
_�B
_�B
`'B
`\B
`\B
`vB
`�B
`�B
`�B
aB
aB
a-B
abB
a|B
a�B
a�B
a�B
a�B
a�B
bhB
bhB
b�B
b�B
cTB
cTB
cTB
cTB
c�B
d�B
d�B
d�B
d�B
eB
e,B
e�B
fB
f�B
f�B
f�B
f�B
f�B
f�B
gRB
g�B
g�B
g�B
h$B
hsB
h�B
hsB
h�B
h�B
h�B
iB
i�B
i�B
jB
j0B
jB
jeB
jB
j�B
kB
k6B
k�B
k�B
l"B
lWB
l�B
l�B
l�B
l�B
l�B
m]B
mwB
m�B
m�B
m�B
m�B
n/B
nIB
n}B
n�B
n�B
o B
oOB
pB
pUB
poB
poB
p�B
p�B
qB
qB
q'B
qAB
qvB
q�B
q�B
q�B
rB
raB
r�B
r�B
r�B
sB
shB
s�B
tB
t9B
t�B
t�B
t�B
uB
t�B
uB
u%B
uZB
uZB
utB
u�B
u�B
u�B
u�B
u�B
u�B
v+B
vzB
vzB
v�B
v�B
v�B
v�B
wB
wLB
wLB
w�B
w�B
xB
xB
xlB
x�B
x�B
y	B
yXB
y>B
yrB
y�B
y�B
y�B
z*B
zB
z*B
z^B
z�B
z�B
z�B
z�B
{B
{0B
{JB
{0B
{JB
{JB
{dB
{B
|B
|6B
|6B
|PB
|PB
|PB
|jB
|�B
|�B
}"B
}qB
}�B
}�B
~BB
~�B
~�B
~�B
HB
HB
�B
� B
� B
�B
��B
��B
��B
��B
��B
��B
�;B
��B
�[B
�AB
�[B
�[B
��B
��B
�GB
�aB
�{B
�{B
��B
��B
�B
�3B
�MB
�MB
��B
��B
�B
�B
�SB
�mB
��B
��B
��B
��B
�B
��B
�?B
�YB
�YB
�YB
�YB
�YB
�tB
�tB
�tB
��B
��B
�tB
��B
��B
��B
�+B
�+B
�EB
�EB
�EB
�zB
��B
��B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221114064341  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221114064341  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221114064342  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221114064342                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221114154347  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221114154347  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221114070522                      G�O�G�O�G�O�                