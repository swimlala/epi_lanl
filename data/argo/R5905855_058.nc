CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:20:46Z creation;2022-06-04T19:20:47Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604192046  20220610151509  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               :A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�76Ϳ��1   @�77-�c�@-������cj$�/�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�ffB�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�ffB���B�  C   C  C  C  C  C
  C�C33C  C�fC�C��C�fC  C  C�fC   C"  C$  C%�fC(  C*33C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^�C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�3D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@xQ�@�(�@�(�A{A>{A^{A~{A��
A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�\)B�(�B�B��\B��\B��\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�(�B�(�B��\B�B�C�HC�HC�HC�HC	�HC��C{C�HCǮC��C�CǮC�HC�HCǮC�HC!�HC#�HC%ǮC'�HC*{C+�HC-�HC/ǮC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[��C]��C_�HCa�HCcǮCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD~�D�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF��DGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp��DqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D���D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�?\D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��)D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��\D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�+6A�+A�%�A�(�A�'�A��A�!�A�!�A��A��A�JA�xA�
�A�
�A�
=A�
	A��A�%A��A��A��A�A��]A��A���A���A��fA��A���A֟VAց;A�C�A� �Aպ^A�>�Aԗ�A�3�A̹�A��A�X�A�MA�5?A��\A�[�A��A���A�2�A��gA���A���A��+A��:A���A�g�A�TaA�9�A���A��A�A��RA�"hA��dA��A��VA���A�m)A���A��A��mA�|A�IA��~A�I�A�'A���A��A���A��&A���A���A�.�A��A�F�A��A�+�A��A~ȴAy;�Au
=Ap�{Am=Ak�Ag�Ac��Ab�A_�]A\��AZ�AW@AS�AM\�AIxAE�	AB�zA?�A=W�A:+A82aA7�A6k�A4�QA34nA0D�A.2aA-`�A,��A,qA+�QA*/A(��A'��A',=A&W?A$��A#`BA"J#A!zA �A �1A �SA ��A��As�AP�AA�A�A�vA�A=qA'RA�A3�A��A�A�A�YA��A�hA��A��A�A�|Am]Al�A~�AuA��A �A
=A�Ah�A�BAe�AVA�[Au%A�fA��A:�Aw�A��AoiA�APHA&�AN�A�]A�Ax�A!�A�	A4A`BA��A��A:*A�3A�KA �A�A.IAS�Al"A�A�A�CA|A/�A�'AxlA�AzA%Ac�A&A+Al"A�A��A�A��AѷAl"AG�A
��A
n�A
JA	ȴA��AoiAA�&AxA��AP�Al�Ah�A�@��L@���@�5�@��@�A @���@�1@�ԕ@�L0@�҉@���@��@��@��@�!@�m�@�u@���@�S�@��@�B[@���@�@@�s@�#�@�v�@�@�s@�֡@�
�@��@�-@�~�@�ѷ@�G@��@��@�=q@���@�8@��@�L@�&�@߁�@��|@ޣ�@�l"@��@��@�,=@�@�,�@�dZ@��]@׌~@֘_@�x�@ԗ�@��@ӗ�@�iD@�A�@�p;@ъ	@�Z�@�Mj@Ε�@�:*@�ԕ@̓{@��`@̔F@�C-@��+@˵t@�'�@�1@��@ȃ�@�J@�j�@��]@ƝI@�a|@��@��@ř�@�l�@�4@��?@�4n@�
�@ø�@Å�@�^�@�8@��@���@I@�'R@��)@���@�P�@��@�;@�u%@��@�B�@��@��v@�z�@��@��^@��'@�v`@�S@��@�(�@��F@�@O@�ی@��\@�V@���@�33@�ѷ@�C-@��@�`B@���@���@�{@�s�@��@���@�3�@�|@��2@�w�@�M@��Z@��K@��h@�#�@�^5@���@�dZ@�4�@��@�<�@��Z@��S@�-w@���@��@��@�4@��@��@��o@�M�@���@�{J@��@��U@���@�q�@�Ta@�1�@�#:@�	�@��;@�|�@�4@�ں@��@�@��P@��f@��5@��y@���@���@�h�@�	@�e@�7@��@���@�/�@�	l@���@��e@��@�Z@�	@��@��S@��@�L�@�=�@���@���@�q�@�C�@�e@���@�Y�@�;@��[@��$@���@�u%@�%�@�	@��3@��f@�t�@�L�@��H@�v�@�M@��Z@���@�ԕ@���@�l�@��@��@�!�@��j@�a@�o@��p@���@�M@�ƨ@�J�@�ی@���@�V@�{@���@���@��"@�7L@���@��@��#@���@�y�@�\)@��f@��@�ی@���@�xl@�Ft@��@��@�J#@��	@��s@�u%@�}V@��9@���@�e�@�4n@���@�v`@�8�@��@���@�>B@�	�@�G@��d@��~@�v`@�`B@�>�@��@��@�]d@�Ov@��@��)@�ݘ@��6@���@�<6@��@��@�@���@�֡@��+@�6@��[@��~@���@���@���@�|�@�8�@�{@��]@���@���@��4@�j�@�=�@���@��/@��r@�B[@��@���@��@�~�@�Mj@�%@���@�M�@��A@���@��k@�hs@�2a@��,@��@�kQ@�PH@�	@���@�s�@�"�@���@��!@��F@�r�@�+k@�q@W?@~�8@~kQ@~@}�~@}N<@|�[@|A�@|�@{dZ@{�@z�x@zv�@zGE@y��@y�@yk�@yV@x�@x��@x<�@w��@w��@w�4@w\)@v�]@vxl@v�@u�@u \@t�e@t'R@s�
@s�@r�r@rB[@q��@q4@p�v@p~(@pG@oqv@o�@n�\@n;�@m�@mQ�@m�@l�D@k�r@k�{@k(@jc @j@i��@i:�@h��@h/�@g��@gdZ@g�@f��@f$�@e�C@eA @d��@d`�@d'R@dG@c�k@c@O@bߤ@b��@b�b@bTa@a��@aN<@a%F@a�@`��@_�K@_g�@_C�@_+@_$t@_�@^��@^W�@^=q@]��@]@\M@\1@[��@[o�@[S�@[H�@Z�y@ZH�@Y�j@Y��@Y!�@X�p@X��@X�.@Xq@XA�@W�A@W��@Wy�@W+@W@V�B@VQ@V-@U�z@U7L@T��@T@Sݘ@S�@S@R�@Q��@Q?}@P��@PA�@O��@N�}@NE�@N@Mj@M@L�5@L�.@K��@K��@Kqv@KK�@J��@J��@J��@J	@I�@I��@Ihs@IJ�@I*0@IV@H��@HV�@H4n@G��@G{J@GY@F�,@Fi�@F{@E�t@EDg@D��@D �@C�Q@C� @Cj�@CC@B�@B�L@BL0@A��@Aa�@AA @@�/@@|�@@Z@@"h@?ƨ@?dZ@>�H@>i�@>�@=��@=�n@=�@==�@<�P@<��@<��@<j@<9X@<(�@<�@;�g@;n/@:�@:�A@::*@9��@9O�@8�@8�4@8�@7�k@7�@6�B@6�L@6kQ@6M�@60U@5�.@5��@5��@5J�@5�@4�	@4�@4�p@4��@4�o@4x@3�m@3�w@3��@3��@3x@3Z�@3C@2��@2u@1ϫ@1��@1��@1��@1��@1Vm@14@1@@0�f@0�@0PH@/�+@/�:@/]�@/A�@/�@.�B@.M�@.
�@-�=@-5�@-�@,�@,�@,(�@+��@+�f@+Mj@+@*�@*�1@*�@)�>@)�@)w2@)+@(��@(��@(M@(G@'ݘ@'�}@'�	@'K�@'�@&�X@&��@&_�@&3�@%�@%��@%|@%u�@%f�@$�@$�@$bN@$,=@#��@#�K@#�P@#8@#S@"�"@"��@"~�@"E�@"�@!��@!2a@!@!�@ ��@ ��@ tT@ @˒@x@&@��@�F@O@��@�@�S@F@!�@!�@@��@��@֡@��@��@��@bN@�@ƨ@��@33@�B@�6@J�@@�@�S@��@^�@B�@�@�@`�@%�@$@ �@�@��@W?@�@�@�m@~�@L0@0U@J@ϫ@��@��@�~@Y�@0�@@@��@��@�@�o@  @�@�6@�$@J#@9�@S@͟@�L@z@xl@R�@�@�)@��@�7@��@|@N<@�@�`@��@��@��@��@m�@A�@1@�@�@��@4�@�"@�h@��@+k@�)@�@�z@��@J�@%@��@��@�I@�_@oi@>B@b@��@�a@��@��@Z�@'�@�@ i@
ߤ@
�r@
=q@	��@	�@	��@	��@	c�@	N<@	+@�p@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�+6A�+A�%�A�(�A�'�A��A�!�A�!�A��A��A�JA�xA�
�A�
�A�
=A�
	A��A�%A��A��A��A�A��]A��A���A���A��fA��A���A֟VAց;A�C�A� �Aպ^A�>�Aԗ�A�3�A̹�A��A�X�A�MA�5?A��\A�[�A��A���A�2�A��gA���A���A��+A��:A���A�g�A�TaA�9�A���A��A�A��RA�"hA��dA��A��VA���A�m)A���A��A��mA�|A�IA��~A�I�A�'A���A��A���A��&A���A���A�.�A��A�F�A��A�+�A��A~ȴAy;�Au
=Ap�{Am=Ak�Ag�Ac��Ab�A_�]A\��AZ�AW@AS�AM\�AIxAE�	AB�zA?�A=W�A:+A82aA7�A6k�A4�QA34nA0D�A.2aA-`�A,��A,qA+�QA*/A(��A'��A',=A&W?A$��A#`BA"J#A!zA �A �1A �SA ��A��As�AP�AA�A�A�vA�A=qA'RA�A3�A��A�A�A�YA��A�hA��A��A�A�|Am]Al�A~�AuA��A �A
=A�Ah�A�BAe�AVA�[Au%A�fA��A:�Aw�A��AoiA�APHA&�AN�A�]A�Ax�A!�A�	A4A`BA��A��A:*A�3A�KA �A�A.IAS�Al"A�A�A�CA|A/�A�'AxlA�AzA%Ac�A&A+Al"A�A��A�A��AѷAl"AG�A
��A
n�A
JA	ȴA��AoiAA�&AxA��AP�Al�Ah�A�@��L@���@�5�@��@�A @���@�1@�ԕ@�L0@�҉@���@��@��@��@�!@�m�@�u@���@�S�@��@�B[@���@�@@�s@�#�@�v�@�@�s@�֡@�
�@��@�-@�~�@�ѷ@�G@��@��@�=q@���@�8@��@�L@�&�@߁�@��|@ޣ�@�l"@��@��@�,=@�@�,�@�dZ@��]@׌~@֘_@�x�@ԗ�@��@ӗ�@�iD@�A�@�p;@ъ	@�Z�@�Mj@Ε�@�:*@�ԕ@̓{@��`@̔F@�C-@��+@˵t@�'�@�1@��@ȃ�@�J@�j�@��]@ƝI@�a|@��@��@ř�@�l�@�4@��?@�4n@�
�@ø�@Å�@�^�@�8@��@���@I@�'R@��)@���@�P�@��@�;@�u%@��@�B�@��@��v@�z�@��@��^@��'@�v`@�S@��@�(�@��F@�@O@�ی@��\@�V@���@�33@�ѷ@�C-@��@�`B@���@���@�{@�s�@��@���@�3�@�|@��2@�w�@�M@��Z@��K@��h@�#�@�^5@���@�dZ@�4�@��@�<�@��Z@��S@�-w@���@��@��@�4@��@��@��o@�M�@���@�{J@��@��U@���@�q�@�Ta@�1�@�#:@�	�@��;@�|�@�4@�ں@��@�@��P@��f@��5@��y@���@���@�h�@�	@�e@�7@��@���@�/�@�	l@���@��e@��@�Z@�	@��@��S@��@�L�@�=�@���@���@�q�@�C�@�e@���@�Y�@�;@��[@��$@���@�u%@�%�@�	@��3@��f@�t�@�L�@��H@�v�@�M@��Z@���@�ԕ@���@�l�@��@��@�!�@��j@�a@�o@��p@���@�M@�ƨ@�J�@�ی@���@�V@�{@���@���@��"@�7L@���@��@��#@���@�y�@�\)@��f@��@�ی@���@�xl@�Ft@��@��@�J#@��	@��s@�u%@�}V@��9@���@�e�@�4n@���@�v`@�8�@��@���@�>B@�	�@�G@��d@��~@�v`@�`B@�>�@��@��@�]d@�Ov@��@��)@�ݘ@��6@���@�<6@��@��@�@���@�֡@��+@�6@��[@��~@���@���@���@�|�@�8�@�{@��]@���@���@��4@�j�@�=�@���@��/@��r@�B[@��@���@��@�~�@�Mj@�%@���@�M�@��A@���@��k@�hs@�2a@��,@��@�kQ@�PH@�	@���@�s�@�"�@���@��!@��F@�r�@�+k@�q@W?@~�8@~kQ@~@}�~@}N<@|�[@|A�@|�@{dZ@{�@z�x@zv�@zGE@y��@y�@yk�@yV@x�@x��@x<�@w��@w��@w�4@w\)@v�]@vxl@v�@u�@u \@t�e@t'R@s�
@s�@r�r@rB[@q��@q4@p�v@p~(@pG@oqv@o�@n�\@n;�@m�@mQ�@m�@l�D@k�r@k�{@k(@jc @j@i��@i:�@h��@h/�@g��@gdZ@g�@f��@f$�@e�C@eA @d��@d`�@d'R@dG@c�k@c@O@bߤ@b��@b�b@bTa@a��@aN<@a%F@a�@`��@_�K@_g�@_C�@_+@_$t@_�@^��@^W�@^=q@]��@]@\M@\1@[��@[o�@[S�@[H�@Z�y@ZH�@Y�j@Y��@Y!�@X�p@X��@X�.@Xq@XA�@W�A@W��@Wy�@W+@W@V�B@VQ@V-@U�z@U7L@T��@T@Sݘ@S�@S@R�@Q��@Q?}@P��@PA�@O��@N�}@NE�@N@Mj@M@L�5@L�.@K��@K��@Kqv@KK�@J��@J��@J��@J	@I�@I��@Ihs@IJ�@I*0@IV@H��@HV�@H4n@G��@G{J@GY@F�,@Fi�@F{@E�t@EDg@D��@D �@C�Q@C� @Cj�@CC@B�@B�L@BL0@A��@Aa�@AA @@�/@@|�@@Z@@"h@?ƨ@?dZ@>�H@>i�@>�@=��@=�n@=�@==�@<�P@<��@<��@<j@<9X@<(�@<�@;�g@;n/@:�@:�A@::*@9��@9O�@8�@8�4@8�@7�k@7�@6�B@6�L@6kQ@6M�@60U@5�.@5��@5��@5J�@5�@4�	@4�@4�p@4��@4�o@4x@3�m@3�w@3��@3��@3x@3Z�@3C@2��@2u@1ϫ@1��@1��@1��@1��@1Vm@14@1@@0�f@0�@0PH@/�+@/�:@/]�@/A�@/�@.�B@.M�@.
�@-�=@-5�@-�@,�@,�@,(�@+��@+�f@+Mj@+@*�@*�1@*�@)�>@)�@)w2@)+@(��@(��@(M@(G@'ݘ@'�}@'�	@'K�@'�@&�X@&��@&_�@&3�@%�@%��@%|@%u�@%f�@$�@$�@$bN@$,=@#��@#�K@#�P@#8@#S@"�"@"��@"~�@"E�@"�@!��@!2a@!@!�@ ��@ ��@ tT@ @˒@x@&@��@�F@O@��@�@�S@F@!�@!�@@��@��@֡@��@��@��@bN@�@ƨ@��@33@�B@�6@J�@@�@�S@��@^�@B�@�@�@`�@%�@$@ �@�@��@W?@�@�@�m@~�@L0@0U@J@ϫ@��@��@�~@Y�@0�@@@��@��@�@�o@  @�@�6@�$@J#@9�@S@͟@�L@z@xl@R�@�@�)@��@�7@��@|@N<@�@�`@��@��@��@��@m�@A�@1@�@�@��@4�@�"@�h@��@+k@�)@�@�z@��@J�@%@��@��@�I@�_@oi@>B@b@��@�a@��@��@Z�@'�@�@ i@
ߤ@
�r@
=q@	��@	�@	��@	��@	c�@	N<@	+@�p@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	āB	�MB	ĜB	āB	āB	ĜB	ĶB	ĜB	�gB	āB	�B	�B	�B	�B	�9B	�B	�9B	�mB	�SB	żB	��B	��B	ƎB	��B	��B	�_B	�EB	�7B	�=B	�0B	�B	�hB	��B	՛B	��B	�PB	��B	�2B	�oB	�-B	��B
�B
�B
.B
2B
#TB
1[B
<6B
?cB
R�B
^�B
h�B
kQB
w�B
}qB
��B
�B
��B
�AB�B-�BX�Bh�Bs�Bs�BcnB8�B3hB8B72B�B{B�B
�B
��B
w�B
Z�B
S�B
E�B
*eB
	�B	��B
�B
B
EB	��B	��B	�'B	��B	�^B	z�B	n/B	`�B	O�B	G+B	@4B	4nB	+kB	!�B	�B	�B��B�B��B�\B��B�GB��B�7B��B޸BߊB�-B�$B�%B��B��B�B��B�4B�B��B�6B�B��B�}B	�B	�B	
�B	�B	,�B	F�B	M6B	WsB	]�B	k�B	kB	g�B	e�B	e�B	i_B	d�B	O�B	K)B	XEB	qvB	x�B	~�B	�GB	��B	��B	��B	�B	��B	�#B	�WB	��B	�B	��B	��B	�DB	��B	��B	�	B	��B	��B	�B	��B	ΥB	�RB	��B
uB
hB
�B
�B

�B
�B
	lB
�B
*�B
1AB
2B
&fB
1B
 B
�B
B
$�B
(�B
5�B
B�B
>wB
5�B
1[B
2�B
33B
2�B
<�B
E�B
GEB
EB
D3B
B�B
@B
>�B
@�B
B�B
A�B
A�B
B�B
BB
B�B
@�B
B�B
B'B
AUB
@iB
>wB
9XB
0oB
+�B
"hB
B
�B
<B
NB
�B
'B	�MB	�B	�VB	�B	��B	�B	�B	�pB	ۦB	ڠB	�7B	ڠB	�B	�)B	ܒB	��B	��B	�~B	�VB	ߤB	�B	��B	ߤB	�;B	޸B	޸B	��B	�BB	�B	��B	�HB	�-B	�bB	��B	�HB	�hB	�B	�B	�B	�HB	��B	�B	�\B	ߤB	��B	��B	�HB	�BB	�bB	�B	ۦB	��B	��B	�(B	�B	ՁB	ևB	�{B	ּB	�sB	ٚB	�WB	��B	�QB	��B	�qB	ܬB	�B	�B	�bB	�B	�B	�:B	�:B	� B	�B	�B	�B	�B	�B	�B	�B	�fB	�B	�mB	�mB	�B	�mB	�mB	�>B	��B	�DB	�*B	�DB	�yB	�B	�B	��B	��B	�B	��B	�QB	�B	�B	�6B	�B	�CB	�)B	�)B	�wB	��B	�IB	�B	�}B	�}B	� B	�B	��B	�[B	�B	��B	�|B	�|B	�B	�MB	�B	�nB	��B	�?B	�B	�B	�?B	��B	�B	�B	�FB	�B	�LB	�B	��B	��B	��B	��B	�$B	�rB	�xB	��B	��B	�dB	�JB	�dB	�B	��B	��B	��B	��B	�<B	�"B	�"B	��B	��B	��B	�}B	��B	�}B	��B	��B
 B
 OB
 B
[B
[B
[B
GB
�B
B
�B
aB
�B
tB
+B
zB
�B
	RB
	�B
	�B
	�B
	lB
�B
�B
dB
�B
�B
�B
(B
\B
BB
(B
�B
�B
B
�B
�B
�B
}B
�B
�B
�B
hB
�B
�B
�B
 B
oB
�B
�B
�B
�B
�B
&B
�B
�B
�B
�B
�B
�B
�B
�B
[B
{B
�B
�B
�B
B
�B
{B
,B
�B
�B
2B
MB
�B
�B
yB
�B
�B
�B
+B
B
�B
=B
�B
]B
�B
�B
B
5B
jB
�B
�B
�B
�B
dB
�B
5B
 �B
!B
!HB
!|B
!�B
!�B
!�B
!HB
 �B
 \B
 BB
 BB
 vB
 vB
 �B
 �B
 �B
 vB
 �B
 \B
 �B
 �B
 vB
 'B
 \B
 �B
!B
!�B
"�B
$�B
$�B
$�B
$�B
$B
#�B
#�B
%FB
$�B
$�B
$�B
%FB
%�B
&2B
&B
&2B
&�B
&fB
&�B
'�B
(>B
(�B
)DB
*B
*�B
*�B
*�B
*�B
+6B
+�B
+�B
,�B
,�B
,�B
,�B
-B
.B
.cB
.�B
.}B
.�B
/B
/iB
0B
0UB
0oB
0�B
0�B
1B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
3hB
3�B
4TB
4�B
4�B
5tB
5�B
6B
5�B
5�B
5�B
6B
6`B
6�B
7fB
7�B
7�B
7�B
7�B
7�B
7�B
8B
8�B
8�B
9$B
9�B
9�B
:DB
:�B
:xB
:�B
;B
;B
;0B
;dB
;�B
;�B
<B
<PB
<jB
<�B
<�B
="B
=<B
=�B
>(B
>�B
>�B
>�B
?B
?HB
?}B
?�B
@B
@B
@iB
@�B
A B
AoB
A�B
A�B
BAB
BAB
B�B
B�B
CB
B�B
B�B
CB
C�B
C�B
C�B
C�B
C�B
D�B
E9B
ESB
EmB
ESB
ESB
E�B
E�B
EmB
E�B
F%B
F�B
F�B
G+B
GB
GB
F�B
GEB
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
IB
IlB
IlB
I�B
I�B
I�B
J	B
J�B
KB
KDB
KB
KB
K)B
KxB
K�B
LdB
LJB
LB
L0B
LJB
L�B
L�B
M6B
M�B
M�B
N"B
NVB
N<B
N�B
O(B
O\B
O\B
OvB
O�B
O�B
O�B
PHB
P�B
P�B
P�B
P�B
Q B
Q B
Q4B
Q�B
Q�B
Q�B
Q�B
R B
R:B
RoB
R�B
R�B
R�B
S[B
S�B
S�B
S�B
TFB
T{B
T�B
T�B
T�B
UgB
U�B
UgB
U�B
VB
VB
VB
VmB
V�B
V�B
WYB
WsB
W�B
W�B
W�B
W�B
X+B
X_B
XyB
X�B
X�B
X�B
X�B
X�B
Y1B
Y�B
ZQB
ZQB
Z�B
Z�B
[#B
[	B
[�B
[�B
\xB
\�B
\�B
\�B
\�B
\�B
\�B
]B
]IB
]�B
]�B
]�B
]�B
]�B
]�B
^B
^jB
^jB
^�B
^�B
^�B
^�B
^�B
^�B
_;B
_�B
`'B
`\B
`vB
`vB
`�B
`�B
`�B
`�B
`�B
aB
abB
a�B
bB
b4B
b4B
b4B
bhB
b�B
b�B
cTB
c�B
c�B
c�B
d@B
d�B
d�B
d�B
eB
eFB
eFB
ezB
e�B
e�B
e�B
f2B
f�B
f�B
f�B
gB
g�B
g�B
gmB
g�B
g�B
g�B
h>B
hXB
h�B
h�B
iB
iB
iDB
i*B
i*B
i�B
jB
i�B
jeB
jeB
jB
j�B
kB
k6B
k6B
kQB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m]B
m�B
nB
n/B
n�B
n}B
oB
oiB
o�B
oiB
o�B
p!B
p!B
p!B
p!B
p;B
p;B
p;B
pUB
poB
poB
poB
p�B
p�B
qB
q[B
q�B
q�B
r-B
rGB
r|B
r�B
r�B
r�B
r�B
r�B
s3B
s�B
tB
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
u%B
utB
utB
u�B
u�B
u�B
u�B
v+B
v`B
v�B
v�B
v�B
v�B
v�B
wLB
xB
xB
xB
x8B
x8B
x8B
xlB
x�B
x�B
y$B
y	B
y>B
yrB
y�B
y�B
zB
y�B
y�B
zDB
z^B
z�B
z�B
z�B
z�B
z�B
z�B
{dB
{B
{B
{B
{�B
|B
|PB
|�B
|�B
}"B
}qB
}VB
}qB
}�B
~B
~BB
~wB
~�B
~�B
~�B
}B
}B
�B
�B
�B
� B
�OB
��B
� B
�B
� B
� B
�oB
��B
�B
�B
�[B
�'B
�[B
��B
�uB
�uB
�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	āB	�MB	ĜB	āB	āB	ĜB	ĶB	ĜB	�gB	āB	�B	�B	�B	�B	�9B	�B	�9B	�mB	�SB	żB	��B	��B	ƎB	��B	��B	�_B	�EB	�7B	�=B	�0B	�B	�hB	��B	՛B	��B	�PB	��B	�2B	�oB	�-B	��B
�B
�B
.B
2B
#TB
1[B
<6B
?cB
R�B
^�B
h�B
kQB
w�B
}qB
��B
�B
��B
�AB�B-�BX�Bh�Bs�Bs�BcnB8�B3hB8B72B�B{B�B
�B
��B
w�B
Z�B
S�B
E�B
*eB
	�B	��B
�B
B
EB	��B	��B	�'B	��B	�^B	z�B	n/B	`�B	O�B	G+B	@4B	4nB	+kB	!�B	�B	�B��B�B��B�\B��B�GB��B�7B��B޸BߊB�-B�$B�%B��B��B�B��B�4B�B��B�6B�B��B�}B	�B	�B	
�B	�B	,�B	F�B	M6B	WsB	]�B	k�B	kB	g�B	e�B	e�B	i_B	d�B	O�B	K)B	XEB	qvB	x�B	~�B	�GB	��B	��B	��B	�B	��B	�#B	�WB	��B	�B	��B	��B	�DB	��B	��B	�	B	��B	��B	�B	��B	ΥB	�RB	��B
uB
hB
�B
�B

�B
�B
	lB
�B
*�B
1AB
2B
&fB
1B
 B
�B
B
$�B
(�B
5�B
B�B
>wB
5�B
1[B
2�B
33B
2�B
<�B
E�B
GEB
EB
D3B
B�B
@B
>�B
@�B
B�B
A�B
A�B
B�B
BB
B�B
@�B
B�B
B'B
AUB
@iB
>wB
9XB
0oB
+�B
"hB
B
�B
<B
NB
�B
'B	�MB	�B	�VB	�B	��B	�B	�B	�pB	ۦB	ڠB	�7B	ڠB	�B	�)B	ܒB	��B	��B	�~B	�VB	ߤB	�B	��B	ߤB	�;B	޸B	޸B	��B	�BB	�B	��B	�HB	�-B	�bB	��B	�HB	�hB	�B	�B	�B	�HB	��B	�B	�\B	ߤB	��B	��B	�HB	�BB	�bB	�B	ۦB	��B	��B	�(B	�B	ՁB	ևB	�{B	ּB	�sB	ٚB	�WB	��B	�QB	��B	�qB	ܬB	�B	�B	�bB	�B	�B	�:B	�:B	� B	�B	�B	�B	�B	�B	�B	�B	�fB	�B	�mB	�mB	�B	�mB	�mB	�>B	��B	�DB	�*B	�DB	�yB	�B	�B	��B	��B	�B	��B	�QB	�B	�B	�6B	�B	�CB	�)B	�)B	�wB	��B	�IB	�B	�}B	�}B	� B	�B	��B	�[B	�B	��B	�|B	�|B	�B	�MB	�B	�nB	��B	�?B	�B	�B	�?B	��B	�B	�B	�FB	�B	�LB	�B	��B	��B	��B	��B	�$B	�rB	�xB	��B	��B	�dB	�JB	�dB	�B	��B	��B	��B	��B	�<B	�"B	�"B	��B	��B	��B	�}B	��B	�}B	��B	��B
 B
 OB
 B
[B
[B
[B
GB
�B
B
�B
aB
�B
tB
+B
zB
�B
	RB
	�B
	�B
	�B
	lB
�B
�B
dB
�B
�B
�B
(B
\B
BB
(B
�B
�B
B
�B
�B
�B
}B
�B
�B
�B
hB
�B
�B
�B
 B
oB
�B
�B
�B
�B
�B
&B
�B
�B
�B
�B
�B
�B
�B
�B
[B
{B
�B
�B
�B
B
�B
{B
,B
�B
�B
2B
MB
�B
�B
yB
�B
�B
�B
+B
B
�B
=B
�B
]B
�B
�B
B
5B
jB
�B
�B
�B
�B
dB
�B
5B
 �B
!B
!HB
!|B
!�B
!�B
!�B
!HB
 �B
 \B
 BB
 BB
 vB
 vB
 �B
 �B
 �B
 vB
 �B
 \B
 �B
 �B
 vB
 'B
 \B
 �B
!B
!�B
"�B
$�B
$�B
$�B
$�B
$B
#�B
#�B
%FB
$�B
$�B
$�B
%FB
%�B
&2B
&B
&2B
&�B
&fB
&�B
'�B
(>B
(�B
)DB
*B
*�B
*�B
*�B
*�B
+6B
+�B
+�B
,�B
,�B
,�B
,�B
-B
.B
.cB
.�B
.}B
.�B
/B
/iB
0B
0UB
0oB
0�B
0�B
1B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
3hB
3�B
4TB
4�B
4�B
5tB
5�B
6B
5�B
5�B
5�B
6B
6`B
6�B
7fB
7�B
7�B
7�B
7�B
7�B
7�B
8B
8�B
8�B
9$B
9�B
9�B
:DB
:�B
:xB
:�B
;B
;B
;0B
;dB
;�B
;�B
<B
<PB
<jB
<�B
<�B
="B
=<B
=�B
>(B
>�B
>�B
>�B
?B
?HB
?}B
?�B
@B
@B
@iB
@�B
A B
AoB
A�B
A�B
BAB
BAB
B�B
B�B
CB
B�B
B�B
CB
C�B
C�B
C�B
C�B
C�B
D�B
E9B
ESB
EmB
ESB
ESB
E�B
E�B
EmB
E�B
F%B
F�B
F�B
G+B
GB
GB
F�B
GEB
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
IB
IlB
IlB
I�B
I�B
I�B
J	B
J�B
KB
KDB
KB
KB
K)B
KxB
K�B
LdB
LJB
LB
L0B
LJB
L�B
L�B
M6B
M�B
M�B
N"B
NVB
N<B
N�B
O(B
O\B
O\B
OvB
O�B
O�B
O�B
PHB
P�B
P�B
P�B
P�B
Q B
Q B
Q4B
Q�B
Q�B
Q�B
Q�B
R B
R:B
RoB
R�B
R�B
R�B
S[B
S�B
S�B
S�B
TFB
T{B
T�B
T�B
T�B
UgB
U�B
UgB
U�B
VB
VB
VB
VmB
V�B
V�B
WYB
WsB
W�B
W�B
W�B
W�B
X+B
X_B
XyB
X�B
X�B
X�B
X�B
X�B
Y1B
Y�B
ZQB
ZQB
Z�B
Z�B
[#B
[	B
[�B
[�B
\xB
\�B
\�B
\�B
\�B
\�B
\�B
]B
]IB
]�B
]�B
]�B
]�B
]�B
]�B
^B
^jB
^jB
^�B
^�B
^�B
^�B
^�B
^�B
_;B
_�B
`'B
`\B
`vB
`vB
`�B
`�B
`�B
`�B
`�B
aB
abB
a�B
bB
b4B
b4B
b4B
bhB
b�B
b�B
cTB
c�B
c�B
c�B
d@B
d�B
d�B
d�B
eB
eFB
eFB
ezB
e�B
e�B
e�B
f2B
f�B
f�B
f�B
gB
g�B
g�B
gmB
g�B
g�B
g�B
h>B
hXB
h�B
h�B
iB
iB
iDB
i*B
i*B
i�B
jB
i�B
jeB
jeB
jB
j�B
kB
k6B
k6B
kQB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m]B
m�B
nB
n/B
n�B
n}B
oB
oiB
o�B
oiB
o�B
p!B
p!B
p!B
p!B
p;B
p;B
p;B
pUB
poB
poB
poB
p�B
p�B
qB
q[B
q�B
q�B
r-B
rGB
r|B
r�B
r�B
r�B
r�B
r�B
s3B
s�B
tB
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
u%B
utB
utB
u�B
u�B
u�B
u�B
v+B
v`B
v�B
v�B
v�B
v�B
v�B
wLB
xB
xB
xB
x8B
x8B
x8B
xlB
x�B
x�B
y$B
y	B
y>B
yrB
y�B
y�B
zB
y�B
y�B
zDB
z^B
z�B
z�B
z�B
z�B
z�B
z�B
{dB
{B
{B
{B
{�B
|B
|PB
|�B
|�B
}"B
}qB
}VB
}qB
}�B
~B
~BB
~wB
~�B
~�B
~�B
}B
}B
�B
�B
�B
� B
�OB
��B
� B
�B
� B
� B
�oB
��B
�B
�B
�[B
�'B
�[B
��B
�uB
�uB
�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105239  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192046  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192047  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192047                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042055  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042055  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151509                      G�O�G�O�G�O�                