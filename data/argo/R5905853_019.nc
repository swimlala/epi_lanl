CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:25:58Z creation;2022-06-04T17:25:59Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604172558  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @����u�1   @��S�@./��-V�c���n�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  Ba33Bf��Bp  Bx  B�  B�ffB�  B���B�  B�33B�ffB�  B�  B�  B�  B�  B�33B�ffB���B���B���B�  B�33B�  B�  B�  B�33B���B�  B�33B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  CL�C�fC  C�fC  C  C�fC�fC  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^�C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D   D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D��3D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @
>@mp�@��R@��RA��A;\)A[\)A{\)A��A��A��A��AͮAݮA��A��B�
B�
B�
B�
B&�
B.�
B6�
B>�
BF�
BN�
BV�
B`
=Be��Bn�
Bv�
B~�
B���B�k�B�8RB�k�B���B���B�k�B�k�B�k�B�k�B�k�B���B���B�8RB�8RB�8RB�k�BǞ�B�k�B�k�B�k�Bמ�B�8RB�k�B㞸B�k�B�8RB�k�B�k�B�k�B�k�B�k�C��C��C��C��C	��C�C�)C��C�)C��C��C�)C�)C��C��C��C!��C#��C%�]C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY�)C[��C]�]C_�]Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D mqD �qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qD	mqD	�qD
mqD
�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDgD�qDmqD�qDmqD�qDmqD�qD gD �qD!mqD!�qD"mqD"�qD#mqD#�qD$mqD$�qD%mqD%�qD&mqD&�qD'mqD'�qD(mqD(�qD)mqD)�qD*mqD*�qD+mqD+�qD,mqD,�qD-mqD-�qD.mqD.�qD/mqD/�qD0mqD0�qD1mqD1�qD2mqD2�qD3mqD3�qD4mqD4�qD5mqD5�qD6mqD6�qD7mqD7�qD8mqD8�qD9mqD9�qD:mqD:�qD;mqD;�qD<mqD<�qD=mqD=�qD>mqD>�qD?mqD?�qD@mqD@�qDAmqDA�qDBmqDB�qDCmqDC�qDDmqDD�qDEmqDE�qDFmqDF�qDGmqDG�qDHmqDH�qDImqDI�qDJmqDJ�qDKmqDK�qDLmqDL�qDMmqDM�qDNmqDN�qDOmqDO�qDPmqDP�qDQmqDQ�qDRmqDR�qDSmqDS�qDTmqDT�qDUmqDU�qDVmqDV�qDWmqDW�qDXmqDX�qDYmqDY�qDZmqDZ�qD[mqD[�qD\mqD\�qD]mqD]�qD^mqD^�qD_mqD_�qD`mqD`�qDamqDa�qDbmqDb�qDcmqDc�qDdmqDd�qDemqDe�qDfmqDf�qDgmqDg�qDhmqDh�qDimqDi�qDjmqDj�qDkmqDk�qDlmqDl�qDmmqDm�qDnmqDn�qDomqDo�qDpmqDp�qDqmqDq�qDrmqDr�qDsmqDs�qDtmqDt�qDumqDu�qDvmqDv�qDwmqDw�qDxmqDx�qDymqDy�qDzmqDz�qD{mqD{�qD|mqD|�qD}mqD}�qD~mqD~�qDmqD�qD�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�3�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�3�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D¶�D���D�6�D�v�Dö�D���D�6�D�v�DĶ�D���D�6�D�v�DŶ�D���D�6�D�v�Dƶ�D���D�6�D�v�DǶ�D���D�6�D�v�Dȶ�D���D�6�D�v�Dɶ�D���D�6�D�v�Dʶ�D���D�6�D�v�D˶�D���D�6�D�v�D̶�D���D�6�D�v�DͶ�D���D�6�D�v�Dζ�D���D�6�D�v�D϶�D���D�6�D�v�Dж�D���D�6�D�v�DѶ�D���D�6�D�v�DҶ�D���D�6�D�v�DӶ�D���D�6�D�v�DԶ�D���D�6�D�v�Dն�D���D�6�D�v�Dֶ�D���D�6�D�v�D׶�D���D�6�D�v�Dض�D���D�6�D�v�Dٹ�D���D�6�D�v�Dڶ�D���D�6�D�v�D۶�D���D�6�D�v�Dܶ�D���D�6�D�v�Dݶ�D���D�6�D�v�D޶�D���D�6�D�v�D߶�D���D�6�D�v�DමD���D�6�D�v�DᶸD���D�6�D�v�DⶸD���D�6�D�v�D㶸D���D�6�D�v�D䶸D���D�6�D�v�D嶸D���D�6�D�v�D涸D���D�6�D�v�D綸D���D�6�D�v�D趸D���D�6�D�v�D鶸D���D�6�D�v�D궸D���D�6�D�v�D붸D���D�6�D�v�D춸D���D�6�D�v�D���D���D�6�D�v�DD���D�6�D�v�DﶸD���D�6�D�v�D�D���D�6�D�v�D�D���D�6�D�v�D�D���D�6�D�v�D�D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�@�A�L�A�"�A�)_A�$@A���A���A�p�A�Y�A� \A�A�A��A��cA৻A��A�s�A��A�	Aݍ�A�2�A���Aܵ�A��A�{A��+AڶAڎ�Aڄ�A�S�A�)�A�DA٣�A���A�y�AѸ�A�CaA˔{A�u�A��A��A�� A�(�A�D�A�-�A�!A��A��XA��(A�A��#A��UA���A�
rA�D3A�DA�A�qA��A��A���A��UA��2A�aHA���A�{A���A�]dA�B�A�l"A�;A��A��)A�  A�y�A��&A���A~-wAtd�Ap�-Al�Ah��Ag}�Af��AeqAb($A`/�A^HA\hsAY+kAUA AQ�#APS&AOg8AL+AI%ADߤAAl�A@�A?	A=E�A9��A76A5�A4��A4oiA3'�A1jA0�=A/ĜA/-A.��A-��A-g8A,�A+�$A*��A*�A(*�A'YA&��A&��A%RTA#n�A"qvA ��An/A,�A:�A#:A�AĜA~�A[WA,=A�UA�dA��AYKA4AIRA�AYA�.A�A��A�&A�tA��A�kAv�AV�A-�A�9A4A�PATaA�A_pA�ADgA�A��A��A!�A��A%ASA��AqvA6�A��AQ�A�A��A�oA��A�9A:*A
��A
�*A
aA
4A	�'A��A#:A��A�A_pA�.A�8Az�A��A��An/Au%AoiAA�A�yAa�A�A�A�mAxlA+kA�FAA �A �hA M@��?@��@�(�@��@���@�iD@�#�@��@�u�@��@���@��+@�~@��Q@�1�@��@�C@���@���@���@�4@�4@�;�@��@���@�$@��@��8@�<�@���@�O�@��@�kQ@陚@�\�@��@�k@�҉@�[�@�� @���@�@�ߤ@��@�9�@�;�@�_p@�x@�o @�@��u@�_�@��@߬q@޶�@�h
@�/�@ݢ�@ܻ�@�"h@��@�W?@��`@ڴ9@ڎ�@���@سh@���@�|�@�&@ּj@�v�@�I�@�@ո�@�F@�]d@�!�@�PH@�1@��@Ӡ�@���@���@Ӯ�@�+@�l"@���@ч�@�4@Е�@�b@�qv@�~�@�f�@�~(@��d@ͱ[@�C�@̟�@�'R@�J�@ʬ@ʄ�@�^5@���@��@�1@ɸ�@�@@ȧ�@�~(@�_@���@ǌ~@�B�@ƪe@�^5@�Xy@� �@Ż0@Ő�@�T�@�/�@���@ď\@�M@��}@û0@èX@�l�@�+�@���@�O@�@�a@�L�@�=�@��@��@���@�l"@�G@�y�@���@��@�c�@���@���@���@�g8@���@���@�ی@�Ta@��@���@�b�@�ѷ@�H�@���@�RT@��K@���@�Ov@���@���@�w2@�8�@��@�Ĝ@�R�@�/�@�1'@�"h@���@�g�@��@�\�@��@�خ@�Y�@�8@��@�H@��d@�zx@���@�K^@��@��^@��@��4@���@���@�}V@�?@�  @��~@��@��x@�`�@�D�@�M@��o@��@��@�u@���@��E@��g@��$@���@��@��F@��	@��@��e@�p;@�g8@�[�@��@��9@���@�n/@�;@�Ɇ@��'@���@�{�@��.@��@��j@���@�#�@���@��?@��@�h
@��@��S@�o @�e�@�Z�@��@�H@��@��@���@�S@���@���@�_@�	@���@���@�g�@�"�@��2@�� @��@��7@�(�@���@���@�_�@�*�@�J@���@��@��'@�x�@�Vm@�;@�_�@�O@��T@���@�X@�%@���@���@��z@�i�@��@��C@�k�@�9�@� \@���@���@�z�@�Z�@�%�@��+@��}@�|�@�4�@��j@���@���@��A@�d�@�S�@� �@�
=@��s@�Ft@�e@���@��d@��P@�A�@��I@�bN@�Q@�D�@�1@�ƨ@�@��0@��[@���@�_p@��@�p;@�M@��;@���@�dZ@�(�@��"@���@�B[@��@��@���@�w2@�C�@�33@���@���@�oi@�C-@�6�@��@���@�f�@�o@��@�d�@���@��@�X�@���@��b@���@�:�@خ@X�@@~�B@~	@}7L@|�@|N�@|@{��@z��@y��@yc@yV@x�f@xx@w��@w_p@v�@u?}@t��@tI�@s�;@r�\@rYK@r�@q��@q&�@p�I@pS�@p$@o��@o>�@n��@n��@nu%@n_@m��@mu�@mJ�@m	l@l�O@lFt@lb@k�m@k�0@k�@k�a@k�@k��@k�P@j�@jz@jQ@j@iԕ@i��@iT�@i�@h��@h�@h�?@h��@h�u@h��@hw�@hb@g��@gj�@g=@g
=@f҉@f6�@e�@e�9@e�X@e�7@eT�@eq@d��@d%�@cb�@cC@b�y@b�'@b�6@bd�@b
�@a�@aY�@a+@`�@`�9@`�@`PH@_��@^��@^8�@]�3@]��@]k�@]G�@]�@\֡@\��@\�I@\�Y@\:�@[��@[�@[�@[��@[E9@[S@Z҉@Z{�@Y�^@YB�@X�P@XɆ@X��@W�@W�V@WZ�@V�@VE�@U��@UN<@T�o@T7@S�@S�:@SJ#@R�m@R	@Q��@Q5�@P�@P[�@PM@Oݘ@O�k@O4�@Nߤ@N�+@N@MVm@L��@LQ�@K�A@K9�@J�@J;�@J($@I��@I�@H��@G�@GX�@G33@G"�@G+@F�M@F)�@E�@D��@Dw�@D4n@C�f@Bȴ@Bh
@B�@Azx@Ak�@AN<@AY�@A5�@A�@@��@@�@@?�@@x@?��@?.I@>ȴ@>�1@>@�@=��@=�@=��@=s�@=q@<֡@<Ĝ@<�_@<bN@<6@;�q@:��@:J�@:)�@:@9�>@9Dg@9@8�@8��@7�@7�@@7]�@7S�@7E9@71�@6l�@5��@5�@5N<@5�@4�E@4�D@4:�@4!@4 �@4�@3�+@3�{@3@2H�@2�@1��@1�@0y>@0bN@0/�@0�@/�@/�@/l�@.�@.��@.z@.L0@-�Z@-�>@-��@-�M@-N<@,��@,��@,:�@+��@+U�@*��@*�@*Ov@*{@)�@)��@)��@)\�@)?}@)�@(�@(�O@(�_@(��@(~(@(e�@([�@(~@(�@'g�@&�@&�@&�L@&+k@%�@%�@%�@%�S@%��@%�S@%p�@%�@$�O@$��@$��@$]d@$7@$@#��@#˒@#�q@#��@#E9@"�H@"J�@"!�@!�@!��@!�~@!T�@!N<@!5�@ ��@ �9@ �@ ��@ ~(@ "h@ �@ �@��@ƨ@�$@j�@+@�@�H@��@_�@@�@��@�h@��@f�@<6@�@��@�[@�e@?�@*�@��@�*@{J@F�@�@��@��@xl@1�@#:@e@�T@��@��@`B@�@�|@�K@�@�v@�E@Ĝ@��@��@��@w�@r�@M@"h@��@��@��@@O@�'@n�@)�@�N@�@��@hs@0�@�@�@��@Ɇ@��@PH@�A@��@�q@��@�4@K�@.I@�@��@�@�6@\�@{@�@ϫ@��@�@�~@0�@�|@֡@�j@��@�@�e@��@c�@ �@��@��@�f@dZ@.I@Y@��@�B@ȴ@�'@�@��@��@�@Z�@V@B[@:*@4@u@��@�@�d@��@��@��@c@p�@rG@m]1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�@�A�L�A�"�A�)_A�$@A���A���A�p�A�Y�A� \A�A�A��A��cA৻A��A�s�A��A�	Aݍ�A�2�A���Aܵ�A��A�{A��+AڶAڎ�Aڄ�A�S�A�)�A�DA٣�A���A�y�AѸ�A�CaA˔{A�u�A��A��A�� A�(�A�D�A�-�A�!A��A��XA��(A�A��#A��UA���A�
rA�D3A�DA�A�qA��A��A���A��UA��2A�aHA���A�{A���A�]dA�B�A�l"A�;A��A��)A�  A�y�A��&A���A~-wAtd�Ap�-Al�Ah��Ag}�Af��AeqAb($A`/�A^HA\hsAY+kAUA AQ�#APS&AOg8AL+AI%ADߤAAl�A@�A?	A=E�A9��A76A5�A4��A4oiA3'�A1jA0�=A/ĜA/-A.��A-��A-g8A,�A+�$A*��A*�A(*�A'YA&��A&��A%RTA#n�A"qvA ��An/A,�A:�A#:A�AĜA~�A[WA,=A�UA�dA��AYKA4AIRA�AYA�.A�A��A�&A�tA��A�kAv�AV�A-�A�9A4A�PATaA�A_pA�ADgA�A��A��A!�A��A%ASA��AqvA6�A��AQ�A�A��A�oA��A�9A:*A
��A
�*A
aA
4A	�'A��A#:A��A�A_pA�.A�8Az�A��A��An/Au%AoiAA�A�yAa�A�A�A�mAxlA+kA�FAA �A �hA M@��?@��@�(�@��@���@�iD@�#�@��@�u�@��@���@��+@�~@��Q@�1�@��@�C@���@���@���@�4@�4@�;�@��@���@�$@��@��8@�<�@���@�O�@��@�kQ@陚@�\�@��@�k@�҉@�[�@�� @���@�@�ߤ@��@�9�@�;�@�_p@�x@�o @�@��u@�_�@��@߬q@޶�@�h
@�/�@ݢ�@ܻ�@�"h@��@�W?@��`@ڴ9@ڎ�@���@سh@���@�|�@�&@ּj@�v�@�I�@�@ո�@�F@�]d@�!�@�PH@�1@��@Ӡ�@���@���@Ӯ�@�+@�l"@���@ч�@�4@Е�@�b@�qv@�~�@�f�@�~(@��d@ͱ[@�C�@̟�@�'R@�J�@ʬ@ʄ�@�^5@���@��@�1@ɸ�@�@@ȧ�@�~(@�_@���@ǌ~@�B�@ƪe@�^5@�Xy@� �@Ż0@Ő�@�T�@�/�@���@ď\@�M@��}@û0@èX@�l�@�+�@���@�O@�@�a@�L�@�=�@��@��@���@�l"@�G@�y�@���@��@�c�@���@���@���@�g8@���@���@�ی@�Ta@��@���@�b�@�ѷ@�H�@���@�RT@��K@���@�Ov@���@���@�w2@�8�@��@�Ĝ@�R�@�/�@�1'@�"h@���@�g�@��@�\�@��@�خ@�Y�@�8@��@�H@��d@�zx@���@�K^@��@��^@��@��4@���@���@�}V@�?@�  @��~@��@��x@�`�@�D�@�M@��o@��@��@�u@���@��E@��g@��$@���@��@��F@��	@��@��e@�p;@�g8@�[�@��@��9@���@�n/@�;@�Ɇ@��'@���@�{�@��.@��@��j@���@�#�@���@��?@��@�h
@��@��S@�o @�e�@�Z�@��@�H@��@��@���@�S@���@���@�_@�	@���@���@�g�@�"�@��2@�� @��@��7@�(�@���@���@�_�@�*�@�J@���@��@��'@�x�@�Vm@�;@�_�@�O@��T@���@�X@�%@���@���@��z@�i�@��@��C@�k�@�9�@� \@���@���@�z�@�Z�@�%�@��+@��}@�|�@�4�@��j@���@���@��A@�d�@�S�@� �@�
=@��s@�Ft@�e@���@��d@��P@�A�@��I@�bN@�Q@�D�@�1@�ƨ@�@��0@��[@���@�_p@��@�p;@�M@��;@���@�dZ@�(�@��"@���@�B[@��@��@���@�w2@�C�@�33@���@���@�oi@�C-@�6�@��@���@�f�@�o@��@�d�@���@��@�X�@���@��b@���@�:�@خ@X�@@~�B@~	@}7L@|�@|N�@|@{��@z��@y��@yc@yV@x�f@xx@w��@w_p@v�@u?}@t��@tI�@s�;@r�\@rYK@r�@q��@q&�@p�I@pS�@p$@o��@o>�@n��@n��@nu%@n_@m��@mu�@mJ�@m	l@l�O@lFt@lb@k�m@k�0@k�@k�a@k�@k��@k�P@j�@jz@jQ@j@iԕ@i��@iT�@i�@h��@h�@h�?@h��@h�u@h��@hw�@hb@g��@gj�@g=@g
=@f҉@f6�@e�@e�9@e�X@e�7@eT�@eq@d��@d%�@cb�@cC@b�y@b�'@b�6@bd�@b
�@a�@aY�@a+@`�@`�9@`�@`PH@_��@^��@^8�@]�3@]��@]k�@]G�@]�@\֡@\��@\�I@\�Y@\:�@[��@[�@[�@[��@[E9@[S@Z҉@Z{�@Y�^@YB�@X�P@XɆ@X��@W�@W�V@WZ�@V�@VE�@U��@UN<@T�o@T7@S�@S�:@SJ#@R�m@R	@Q��@Q5�@P�@P[�@PM@Oݘ@O�k@O4�@Nߤ@N�+@N@MVm@L��@LQ�@K�A@K9�@J�@J;�@J($@I��@I�@H��@G�@GX�@G33@G"�@G+@F�M@F)�@E�@D��@Dw�@D4n@C�f@Bȴ@Bh
@B�@Azx@Ak�@AN<@AY�@A5�@A�@@��@@�@@?�@@x@?��@?.I@>ȴ@>�1@>@�@=��@=�@=��@=s�@=q@<֡@<Ĝ@<�_@<bN@<6@;�q@:��@:J�@:)�@:@9�>@9Dg@9@8�@8��@7�@7�@@7]�@7S�@7E9@71�@6l�@5��@5�@5N<@5�@4�E@4�D@4:�@4!@4 �@4�@3�+@3�{@3@2H�@2�@1��@1�@0y>@0bN@0/�@0�@/�@/�@/l�@.�@.��@.z@.L0@-�Z@-�>@-��@-�M@-N<@,��@,��@,:�@+��@+U�@*��@*�@*Ov@*{@)�@)��@)��@)\�@)?}@)�@(�@(�O@(�_@(��@(~(@(e�@([�@(~@(�@'g�@&�@&�@&�L@&+k@%�@%�@%�@%�S@%��@%�S@%p�@%�@$�O@$��@$��@$]d@$7@$@#��@#˒@#�q@#��@#E9@"�H@"J�@"!�@!�@!��@!�~@!T�@!N<@!5�@ ��@ �9@ �@ ��@ ~(@ "h@ �@ �@��@ƨ@�$@j�@+@�@�H@��@_�@@�@��@�h@��@f�@<6@�@��@�[@�e@?�@*�@��@�*@{J@F�@�@��@��@xl@1�@#:@e@�T@��@��@`B@�@�|@�K@�@�v@�E@Ĝ@��@��@��@w�@r�@M@"h@��@��@��@@O@�'@n�@)�@�N@�@��@hs@0�@�@�@��@Ɇ@��@PH@�A@��@�q@��@�4@K�@.I@�@��@�@�6@\�@{@�@ϫ@��@�@�~@0�@�|@֡@�j@��@�@�e@��@c�@ �@��@��@�f@dZ@.I@Y@��@�B@ȴ@�'@�@��@��@�@Z�@V@B[@:*@4@u@��@�@�d@��@��@��@c@p�@rG@m]1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B�DB�0B�B��B�>B�rB�rB�DB�dB��B	�B	B	B	�B	�B	%B	8�B	>(B	A�B	FYB	S�B	ezB	p!B	r�B	q[B	q�B	v�B	vFB	x�B	��B	��B	wfB	u�B	��B	��B	ΊB	��B

�B
5%B
KxB
^OB
rB
y�B
u�B
s�B
v�B
��B
�B
��B%zBE�Bh�B}<B��B��B��B�_B�B�EB� B{Bt9B]IB;B!�B�B�B
��B
�vB
�6B
�B
N�B
�B	�	B	��B	PbB	F�B	=qB	0UB	)yB	&2B	OB	�B	�B	#B	�B	GB��B�]B�B�B��BοB�iB��B�aB�)B��B�NB�fB��B�6B��B�IB�B��BªB�7B�NB�B�|B	
=B	B	 �B��B��B	 OB	yB	+�B	:*B	2�B	,�B	)_B	 �B	)�B	1B	<�B	H�B	V�B	b�B	g�B	l"B	qvB	ncB	_;B	R B	oB	{�B	|�B	�DB	��B	�8B	��B	�"B	�B	ǔB	ΊB	ЗB	�B	�VB	͟B	�~B	��B	ʌB	�=B	�RB	ȚB	ĶB	��B	�B	�RB	��B	�B	��B	�<B	�B	�jB	�B	�bB	уB	�oB	��B	ԯB	��B	�B	өB	��B	�aB	՛B	��B	�YB	ՁB	�[B	��B	�[B	�[B	��B	��B	�B	��B	��B	ЗB	��B	ԕB	��B	՛B	�YB	ںB	�B	�\B	�ZB	�B	�B	��B	��B	�B	�B	��B	ٚB	�B	��B	�	B	��B	�OB	ݲB	�xB	֡B	ңB	�YB	� B	�nB	�B	��B	�B	�B	��B	�FB	�nB	� B	�TB	�B	��B	�NB	��B	�B	�nB	�LB	�LB	��B	��B	�B	�B	�LB	�ZB	�B	�zB	�B	�B	��B	��B	��B	�QB	�0B	�eB	�CB	�B	�UB	�B	��B	�oB	�B	�]B	�B	��B	�)B	��B	�B	�OB	�B	��B	�wB	�B	�WB	�B	�QB	�WB	�IB	�B	�B	��B	�B	��B	��B	�B	�[B	�TB	�B	��B	�B	�B	��B	�B	�FB	�B	�hB	�B	�-B	�AB	��B	�iB	��B	��B	��B	�B	�9B	�B	�tB	�?B	�TB	��B	�B	�3B	�nB	�TB	�tB	��B	�B	��B	�B	�B	�%B	�B	�B	�9B	�B	�B	�TB	�B	��B	�hB	��B	�?B	�B	�B	�B	��B	��B	��B	�B	�LB	��B	�tB	��B	��B	�B	��B	�DB	��B	��B	��B	�xB	��B	��B	��B	��B	�DB	��B	�$B	�>B	��B	�	B	��B	��B	�6B	�VB	��B
 �B
 B	��B
 B
UB
�B
[B
AB
AB
AB
'B
uB
GB
�B
�B
�B
B
�B
SB
�B
�B
fB
�B
�B
KB
zB
�B
B
{B
-B
GB
�B
�B
SB
zB
�B
B
gB
�B
�B
�B
MB
�B
�B
�B
�B
_B
�B
	�B

�B
	B
�B

	B
�B
�B
�B
�B
TB
�B
�B
�B
�B
�B
{B
{B
aB
�B
�B
�B
gB
�B
�B
$B
?B
+B
KB
�B
�B
�B
B
B
B
�B
B
�B
B
eB
1B
1B
�B
QB
kB
kB
7B
�B
QB
QB
QB
B
7B
kB
#B
=B
�B
�B
dB
�B
�B
�B
�B
�B
B
�B
IB
OB
�B
B
pB
 B
 \B
 �B
!�B
"NB
# B
#�B
$&B
%B
$�B
$�B
$tB
$�B
$�B
$�B
%B
%,B
%�B
&�B
'RB
'�B
(
B
'�B
'�B
'RB
'B
&�B
&�B
&2B
&LB
&�B
&B
&�B
&�B
'�B
(>B
'�B
'mB
'�B
'RB
*eB
+�B
,B
,WB
,WB
,�B
-)B
-]B
.}B
.�B
/5B
/OB
/�B
/�B
/�B
0;B
0oB
0�B
0�B
0�B
1B
1AB
1'B
1�B
1�B
2aB
2GB
2GB
2�B
2�B
2�B
3hB
2�B
3�B
4B
4B
4�B
4�B
5ZB
5tB
5�B
6+B
6`B
6zB
6`B
6�B
7�B
7�B
7�B
7�B
7�B
8lB
8�B
9$B
9$B
9	B
9�B
9�B
9�B
:B
;0B
;dB
;dB
;�B
<�B
<jB
<�B
=B
=<B
=qB
=�B
=qB
=�B
=�B
>BB
>BB
>BB
>�B
>�B
>�B
>�B
?.B
?cB
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?cB
@iB
@B
@�B
@�B
@�B
@�B
@�B
A B
A B
A B
@�B
@�B
A;B
A;B
AB
AUB
A�B
A�B
A�B
BB
A�B
B�B
B�B
BAB
B�B
B�B
C-B
B�B
CGB
C�B
DgB
D�B
D�B
D�B
D�B
EB
EB
E9B
E�B
EmB
E�B
E�B
E�B
E�B
F?B
F�B
G+B
GB
G�B
G�B
G�B
H1B
G�B
H�B
G�B
HB
HKB
H�B
H�B
H�B
H�B
H�B
H�B
IB
IRB
I�B
J	B
J=B
J=B
J�B
J�B
J�B
J�B
K^B
K�B
K�B
L�B
MB
MPB
M�B
MPB
MB
L�B
L�B
L�B
MB
L~B
L�B
L�B
L�B
MB
M�B
M�B
NB
NVB
OB
OvB
O�B
O�B
PHB
P�B
Q B
Q B
P�B
QB
Q�B
R�B
R�B
R�B
S&B
R�B
S[B
S�B
S�B
R�B
S@B
S�B
TB
T�B
T{B
T�B
U�B
UB
U�B
VmB
V�B
W?B
X�B
XEB
W�B
W�B
XyB
X�B
YeB
Y�B
Z7B
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[WB
Z�B
[�B
[#B
[�B
[�B
[=B
[	B
\)B
\)B
\B
[�B
[�B
\CB
]/B
]/B
^B
]�B
^B
]�B
_�B
_�B
`'B
`'B
`B
_�B
_;B
^�B
_�B
_VB
_pB
_�B
`B
`\B
`vB
`B
_�B
`B
`�B
`\B
`�B
`�B
`�B
`�B
a-B
a�B
a�B
a�B
bB
bhB
bNB
bNB
b�B
b�B
b�B
cnB
b�B
c�B
c�B
dtB
dZB
d�B
d�B
e�B
e�B
d�B
e�B
e�B
e�B
e�B
e�B
fLB
f�B
f�B
f�B
f2B
f�B
ffB
g8B
gB
f�B
gmB
g�B
h>B
g�B
g�B
h$B
h�B
h
B
h$B
h�B
i�B
i�B
iDB
i�B
i�B
i�B
i�B
i�B
i�B
j0B
j0B
jB
kB
k6B
kkB
k�B
k�B
l"B
k�B
k�B
l=B
l"B
l�B
l"B
lqB
l�B
mB
l�B
l�B
m)B
mCB
m]B
mwB
mwB
mwB
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o5B
o5B
oiB
o�B
p;B
pB
poB
poB
p�B
p�B
q[B
qAB
q[B
qvB
q�B
q�B
q�B
q�B
q�B
raB
raB
r�B
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
s3B
s3B
sB
s�B
s�B
s�B
t�B
tTB
t�B
t�B
utB
u?B
u�B
u�B
u�B
v`B
vFB
vzB
v+B
v�B
v�B
wB
wfB
wLB
w�B
w�B
w�B
w�B
xB
xRB
x8B
xRB
x�B
y$B
y	B
y>B
y�B
y�B
y	B
y�B
y�B
y�B
zB
z*B
zB
yrB
y�B
z*B
zDB
z�B
{B
zxB
{dB
{�B
{�B
{�B
|B
{�B
|B
{0B
|B
|6B
{�B
|�B
|PB
|�B
|�B
|�B
|�B
}B
|�B
}"B
}"B
}"B
}<B
}qB
}�B
}VB
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B�DB�0B�B��B�>B�rB�rB�DB�dB��B	�B	B	B	�B	�B	%B	8�B	>(B	A�B	FYB	S�B	ezB	p!B	r�B	q[B	q�B	v�B	vFB	x�B	��B	��B	wfB	u�B	��B	��B	ΊB	��B

�B
5%B
KxB
^OB
rB
y�B
u�B
s�B
v�B
��B
�B
��B%zBE�Bh�B}<B��B��B��B�_B�B�EB� B{Bt9B]IB;B!�B�B�B
��B
�vB
�6B
�B
N�B
�B	�	B	��B	PbB	F�B	=qB	0UB	)yB	&2B	OB	�B	�B	#B	�B	GB��B�]B�B�B��BοB�iB��B�aB�)B��B�NB�fB��B�6B��B�IB�B��BªB�7B�NB�B�|B	
=B	B	 �B��B��B	 OB	yB	+�B	:*B	2�B	,�B	)_B	 �B	)�B	1B	<�B	H�B	V�B	b�B	g�B	l"B	qvB	ncB	_;B	R B	oB	{�B	|�B	�DB	��B	�8B	��B	�"B	�B	ǔB	ΊB	ЗB	�B	�VB	͟B	�~B	��B	ʌB	�=B	�RB	ȚB	ĶB	��B	�B	�RB	��B	�B	��B	�<B	�B	�jB	�B	�bB	уB	�oB	��B	ԯB	��B	�B	өB	��B	�aB	՛B	��B	�YB	ՁB	�[B	��B	�[B	�[B	��B	��B	�B	��B	��B	ЗB	��B	ԕB	��B	՛B	�YB	ںB	�B	�\B	�ZB	�B	�B	��B	��B	�B	�B	��B	ٚB	�B	��B	�	B	��B	�OB	ݲB	�xB	֡B	ңB	�YB	� B	�nB	�B	��B	�B	�B	��B	�FB	�nB	� B	�TB	�B	��B	�NB	��B	�B	�nB	�LB	�LB	��B	��B	�B	�B	�LB	�ZB	�B	�zB	�B	�B	��B	��B	��B	�QB	�0B	�eB	�CB	�B	�UB	�B	��B	�oB	�B	�]B	�B	��B	�)B	��B	�B	�OB	�B	��B	�wB	�B	�WB	�B	�QB	�WB	�IB	�B	�B	��B	�B	��B	��B	�B	�[B	�TB	�B	��B	�B	�B	��B	�B	�FB	�B	�hB	�B	�-B	�AB	��B	�iB	��B	��B	��B	�B	�9B	�B	�tB	�?B	�TB	��B	�B	�3B	�nB	�TB	�tB	��B	�B	��B	�B	�B	�%B	�B	�B	�9B	�B	�B	�TB	�B	��B	�hB	��B	�?B	�B	�B	�B	��B	��B	��B	�B	�LB	��B	�tB	��B	��B	�B	��B	�DB	��B	��B	��B	�xB	��B	��B	��B	��B	�DB	��B	�$B	�>B	��B	�	B	��B	��B	�6B	�VB	��B
 �B
 B	��B
 B
UB
�B
[B
AB
AB
AB
'B
uB
GB
�B
�B
�B
B
�B
SB
�B
�B
fB
�B
�B
KB
zB
�B
B
{B
-B
GB
�B
�B
SB
zB
�B
B
gB
�B
�B
�B
MB
�B
�B
�B
�B
_B
�B
	�B

�B
	B
�B

	B
�B
�B
�B
�B
TB
�B
�B
�B
�B
�B
{B
{B
aB
�B
�B
�B
gB
�B
�B
$B
?B
+B
KB
�B
�B
�B
B
B
B
�B
B
�B
B
eB
1B
1B
�B
QB
kB
kB
7B
�B
QB
QB
QB
B
7B
kB
#B
=B
�B
�B
dB
�B
�B
�B
�B
�B
B
�B
IB
OB
�B
B
pB
 B
 \B
 �B
!�B
"NB
# B
#�B
$&B
%B
$�B
$�B
$tB
$�B
$�B
$�B
%B
%,B
%�B
&�B
'RB
'�B
(
B
'�B
'�B
'RB
'B
&�B
&�B
&2B
&LB
&�B
&B
&�B
&�B
'�B
(>B
'�B
'mB
'�B
'RB
*eB
+�B
,B
,WB
,WB
,�B
-)B
-]B
.}B
.�B
/5B
/OB
/�B
/�B
/�B
0;B
0oB
0�B
0�B
0�B
1B
1AB
1'B
1�B
1�B
2aB
2GB
2GB
2�B
2�B
2�B
3hB
2�B
3�B
4B
4B
4�B
4�B
5ZB
5tB
5�B
6+B
6`B
6zB
6`B
6�B
7�B
7�B
7�B
7�B
7�B
8lB
8�B
9$B
9$B
9	B
9�B
9�B
9�B
:B
;0B
;dB
;dB
;�B
<�B
<jB
<�B
=B
=<B
=qB
=�B
=qB
=�B
=�B
>BB
>BB
>BB
>�B
>�B
>�B
>�B
?.B
?cB
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?cB
@iB
@B
@�B
@�B
@�B
@�B
@�B
A B
A B
A B
@�B
@�B
A;B
A;B
AB
AUB
A�B
A�B
A�B
BB
A�B
B�B
B�B
BAB
B�B
B�B
C-B
B�B
CGB
C�B
DgB
D�B
D�B
D�B
D�B
EB
EB
E9B
E�B
EmB
E�B
E�B
E�B
E�B
F?B
F�B
G+B
GB
G�B
G�B
G�B
H1B
G�B
H�B
G�B
HB
HKB
H�B
H�B
H�B
H�B
H�B
H�B
IB
IRB
I�B
J	B
J=B
J=B
J�B
J�B
J�B
J�B
K^B
K�B
K�B
L�B
MB
MPB
M�B
MPB
MB
L�B
L�B
L�B
MB
L~B
L�B
L�B
L�B
MB
M�B
M�B
NB
NVB
OB
OvB
O�B
O�B
PHB
P�B
Q B
Q B
P�B
QB
Q�B
R�B
R�B
R�B
S&B
R�B
S[B
S�B
S�B
R�B
S@B
S�B
TB
T�B
T{B
T�B
U�B
UB
U�B
VmB
V�B
W?B
X�B
XEB
W�B
W�B
XyB
X�B
YeB
Y�B
Z7B
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[WB
Z�B
[�B
[#B
[�B
[�B
[=B
[	B
\)B
\)B
\B
[�B
[�B
\CB
]/B
]/B
^B
]�B
^B
]�B
_�B
_�B
`'B
`'B
`B
_�B
_;B
^�B
_�B
_VB
_pB
_�B
`B
`\B
`vB
`B
_�B
`B
`�B
`\B
`�B
`�B
`�B
`�B
a-B
a�B
a�B
a�B
bB
bhB
bNB
bNB
b�B
b�B
b�B
cnB
b�B
c�B
c�B
dtB
dZB
d�B
d�B
e�B
e�B
d�B
e�B
e�B
e�B
e�B
e�B
fLB
f�B
f�B
f�B
f2B
f�B
ffB
g8B
gB
f�B
gmB
g�B
h>B
g�B
g�B
h$B
h�B
h
B
h$B
h�B
i�B
i�B
iDB
i�B
i�B
i�B
i�B
i�B
i�B
j0B
j0B
jB
kB
k6B
kkB
k�B
k�B
l"B
k�B
k�B
l=B
l"B
l�B
l"B
lqB
l�B
mB
l�B
l�B
m)B
mCB
m]B
mwB
mwB
mwB
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o5B
o5B
oiB
o�B
p;B
pB
poB
poB
p�B
p�B
q[B
qAB
q[B
qvB
q�B
q�B
q�B
q�B
q�B
raB
raB
r�B
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
s3B
s3B
sB
s�B
s�B
s�B
t�B
tTB
t�B
t�B
utB
u?B
u�B
u�B
u�B
v`B
vFB
vzB
v+B
v�B
v�B
wB
wfB
wLB
w�B
w�B
w�B
w�B
xB
xRB
x8B
xRB
x�B
y$B
y	B
y>B
y�B
y�B
y	B
y�B
y�B
y�B
zB
z*B
zB
yrB
y�B
z*B
zDB
z�B
{B
zxB
{dB
{�B
{�B
{�B
|B
{�B
|B
{0B
|B
|6B
{�B
|�B
|PB
|�B
|�B
|�B
|�B
}B
|�B
}"B
}"B
}"B
}<B
}qB
}�B
}VB
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104850  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172558  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172558  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172559                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022607  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022607  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                