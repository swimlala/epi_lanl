CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-01-13T15:43:16Z creation;2023-01-13T15:43:17Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230113154316  20230113162614  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�ێ���1   @���ax:@.�5?|��d$�/�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw33B�  B�  B�  B�  B�  B�33B�  B�33B���B���B�ffB���B���B�  B���B���B�  B�  B�33B�  B�  B�33B���B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C�C
L�C��C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@+�@xQ�@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bv�RB�B�B�B�B�B���B�B���B��\B�\)B�(�B��\B��\B�B��\B��\B�B�B���B�B�B���B׏\B�B�B�B�B�\B�B�B�B�B�C�HC�HC�HC��C
.C�C�HC�HCǮC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5ǮC7ǮC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ��CS��CU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��qC��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD~�D�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr��DsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�x�D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�\D��\D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��)D�?\D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�m]A�z�A�.AІ%AЊ	AЌ~AЍ�AЎ�AЍ�AЎVAЏ(AЎ�AЎ�AЏ�AБ�AВoAД�AДFAЕAЕ�AЖAЗYAЗ�AИ�AК�AМ�AМ�AЛ�AО�AПVAП�AП�AР\AС�AС�AЛ	AГ�A��BA΍A�l"A�^jA���A���A���A�\]A�9�A���A��A�@A�"�A�1�A�\�A�z�A�x�A���A��\A���A�VmA���A��NA��-A�m)A�y	Ay}VAs�PAq�AAm��AkrGAj�cAf<�Aa
�AZ��AZ3�AU��AQ� AN�yAL6�AH�FAE��ADQ�AB�AAd�A@t�A>�,A;j�A:\�A8��A6��A4��A3X�A2,=A0�A0!�A/W?A.m]A- �A+y>A*o A)��A)V�A(��A&�!A%��A%�PA%�PA%A!��A��A��A}�A�A ��A!&�A!_A!ffA"(A"�A!��A!s�A @A/A�|A�/A��AA��AO�A��Ay�A_A�)A�A6A��A6AC�AɆA:�AOvA \AیA;dAi�A�A�ASA�A�A:*A�A�A�A��A��A�`APHAa|A�gA�AA��A&�A
�1A
'RA
k�A
s�A	�A	8�A��A��A�OA�A	�AخA��AN�AMA��A��A4�A�TA/�A�|A��AOvA ��A H�@��N@���@��@�zx@���@�i�@�O@��j@�w2@���@���@��v@���@�9�@��@��|@��E@�c @�@�@�9@�$@�#:@�c @���@�x@�/�@�;@�B[@ꠐ@�5�@��@��@��@�u@��r@琗@��@掊@�Q@���@�!�@� @���@オ@�@ᴢ@��@�zx@��@��Z@�-@�:�@੓@�C-@���@��@��}@�<�@��3@ۏ�@�Dg@�֡@ڳh@�C-@�G@لM@؍�@ؐ.@�Ov@��>@�c�@���@��@ׄM@�@��?@֟�@�m�@��@�m]@�%F@���@Ԍ@��@ӊ�@���@��a@�\�@д9@�p;@�{@�G@���@��@�@�tT@�E�@��@͢�@�f�@�4�@��@�M@���@�a@��@ʏ\@��@ɢ�@�Z�@�!-@�V�@ǎ"@�J�@��@�1�@�%F@�7�@ë�@�m]@��@�S@���@£�@�u�@�b@�iD@�$t@��E@�_@�خ@�n/@��@��I@��@���@��k@�s�@�hs@�e�@�&�@�u%@��K@�>�@��]@�}V@� �@�s@���@��4@�%�@��@��@��0@��M@��@�RT@��@��M@���@�r�@���@��@�r�@�l"@�>B@�M@�
�@��@��@�4�@���@���@��@� �@��@���@�1�@�~@���@���@�x�@�H�@�C�@�8@�1�@�!-@���@�q�@�M@���@�Mj@���@�c @�A�@���@��f@�o@��@�I�@���@��@��o@�#:@��W@��@�U�@�@���@��Y@�Q@�6@�+k@�'R@�M@��@���@�@��$@��\@�U2@�=q@��@���@���@�O�@��@��5@�֡@��[@�Ĝ@��U@��@���@���@�]d@��@�[W@�@O@��@��c@�҉@�~�@�@�j�@���@��B@�($@��>@��T@��>@��z@��	@�t�@�>�@��@��9@�� @�c�@��@���@���@�E9@�@@���@���@���@��@�;�@���@�t�@�!-@���@�Ta@��a@�\�@�2a@�@@��8@��M@��E@��}@��@�(�@���@�T�@�8�@���@��A@�_@���@�o @�Z�@�A�@�;d@�.I@� \@��@��@��2@��_@�	@���@�|@�0�@�@��D@�1'@��@��f@���@���@�bN@�C�@�6�@� �@��@�\�@�#�@�@��j@�K^@��}@��:@�33@��X@��\@�C-@�˒@��[@���@���@�e,@�S&@�IR@�4�@���@���@�:*@��@��r@��@���@���@��@�2a@��@���@�҉@�
�@�˒@���@�c@�Dg@�;@�j@���@�ԕ@�Dg@�&�@���@��}@���@�xl@�)�@�W@�0@l�@�@S@~��@~�c@~��@~{@}�@}o @|��@|��@|l"@|/�@{�m@{�@z��@y��@y[W@y<6@x��@x�@vE�@u7L@tɆ@t��@tu�@tA�@s�@s�@r�@r��@r0U@q��@q?}@p�/@p|�@pS�@o˒@n�@n8�@m�N@m�n@m��@m�~@mj@m�@l*�@k�Q@k��@k!-@j��@j�@i�@i��@iVm@h��@h'R@g�4@f��@f;�@eԕ@e��@e\�@d�P@dZ@c��@cg�@c)_@c�@b��@b��@b;�@a��@`��@`z�@` �@_�A@_��@_(@^��@^+k@^@]}�@\�@\��@\�@[�@[{J@[1�@Z��@YDg@XbN@W�&@W��@WO@W/�@V��@V}V@U�@Uzx@U%@T�)@Th�@T@S�6@S��@S=@S�@R�+@R!�@R
�@Q�N@Q�S@QT�@Q�@P��@P�@P��@P$@P�@P@OZ�@O!-@O�@N��@N�@Mhs@M-w@M;@LN�@K�0@K�{@KMj@K�@K@J�@Jq�@J6�@J
�@I�@Ix�@IVm@I%@Hѷ@H~(@H7�@G�+@G�a@GU�@F��@Fl�@F	@E��@E�@Dc�@C�r@C�@B�"@B��@B_�@B.�@A��@A�z@A��@A��@A \@@��@@�@@7�@@�@@G@?�g@?�F@?|�@?P�@?>�@?�@>��@>��@>YK@=��@=c�@<֡@<�@;˒@;\)@;J#@:��@:�1@:1�@9�@9�"@9��@9x�@9A @9�@8�v@8��@8"h@7˒@7��@7X�@79�@6ߤ@6d�@6J@5�3@5�^@5s�@4�f@4�K@4�@3�+@3��@3o�@34�@2��@2��@2�@1�@1�@0_@09X@/�W@/��@/{J@/S@.i�@.&�@-�-@-��@-s�@-�@,�	@,��@,�@,7�@,-�@,1@+��@+{J@+�@*Z�@*.�@*
�@)�3@)��@)�@(�v@(�.@(�@'��@'�0@'iD@'(@&��@&�R@&��@&Ov@&�@%��@%��@%�@%F@$�P@$�?@$��@$Xy@$*�@$�@#�@#dZ@"�c@"�B@"v�@"O@!�D@!@!��@!}�@!j@!Q�@!#�@ ��@ q@ %�@�+@��@�@@E9@�M@�,@��@{�@\�@3�@
�@��@��@�-@T�@�@��@>B@�@�;@��@~�@iD@E9@�@ں@�@��@�@�1@��@�\@ff@@ԕ@@�@�=@u�@8�@;@ی@�_@|�@K^@�@M@b@G@��@��@�k@l�@U�@+@�@�\@s�@ff@i�@i�@i�@a|@.�@@�#@��@�C@�X@��@w2@a�@^�@IR@V@�@��@�z@�@��@��@��@�@q@c�@N�@b@�@��@y�@W?@�@��@��@z@R�@:*@?@@�@8�@3�@@��@ϫ@�-@�=@x�@\�@8�@@@��@�9@bN@-�@~@�@��@�@�q@�P@y�@iD@P�@�2@�<@��@O@�@�3@�X@f�@:�@�@�@�@�p@��@��@j@I�@	�@�[@'�@
�2@
͟@
��@
�@
�@
�@
�1@
��@
��@
{�@
Q@
4@	�9@	@	��@	��@	O�@	V@�f@�|@�@�D@h�@Z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�m]A�z�A�.AІ%AЊ	AЌ~AЍ�AЎ�AЍ�AЎVAЏ(AЎ�AЎ�AЏ�AБ�AВoAД�AДFAЕAЕ�AЖAЗYAЗ�AИ�AК�AМ�AМ�AЛ�AО�AПVAП�AП�AР\AС�AС�AЛ	AГ�A��BA΍A�l"A�^jA���A���A���A�\]A�9�A���A��A�@A�"�A�1�A�\�A�z�A�x�A���A��\A���A�VmA���A��NA��-A�m)A�y	Ay}VAs�PAq�AAm��AkrGAj�cAf<�Aa
�AZ��AZ3�AU��AQ� AN�yAL6�AH�FAE��ADQ�AB�AAd�A@t�A>�,A;j�A:\�A8��A6��A4��A3X�A2,=A0�A0!�A/W?A.m]A- �A+y>A*o A)��A)V�A(��A&�!A%��A%�PA%�PA%A!��A��A��A}�A�A ��A!&�A!_A!ffA"(A"�A!��A!s�A @A/A�|A�/A��AA��AO�A��Ay�A_A�)A�A6A��A6AC�AɆA:�AOvA \AیA;dAi�A�A�ASA�A�A:*A�A�A�A��A��A�`APHAa|A�gA�AA��A&�A
�1A
'RA
k�A
s�A	�A	8�A��A��A�OA�A	�AخA��AN�AMA��A��A4�A�TA/�A�|A��AOvA ��A H�@��N@���@��@�zx@���@�i�@�O@��j@�w2@���@���@��v@���@�9�@��@��|@��E@�c @�@�@�9@�$@�#:@�c @���@�x@�/�@�;@�B[@ꠐ@�5�@��@��@��@�u@��r@琗@��@掊@�Q@���@�!�@� @���@オ@�@ᴢ@��@�zx@��@��Z@�-@�:�@੓@�C-@���@��@��}@�<�@��3@ۏ�@�Dg@�֡@ڳh@�C-@�G@لM@؍�@ؐ.@�Ov@��>@�c�@���@��@ׄM@�@��?@֟�@�m�@��@�m]@�%F@���@Ԍ@��@ӊ�@���@��a@�\�@д9@�p;@�{@�G@���@��@�@�tT@�E�@��@͢�@�f�@�4�@��@�M@���@�a@��@ʏ\@��@ɢ�@�Z�@�!-@�V�@ǎ"@�J�@��@�1�@�%F@�7�@ë�@�m]@��@�S@���@£�@�u�@�b@�iD@�$t@��E@�_@�خ@�n/@��@��I@��@���@��k@�s�@�hs@�e�@�&�@�u%@��K@�>�@��]@�}V@� �@�s@���@��4@�%�@��@��@��0@��M@��@�RT@��@��M@���@�r�@���@��@�r�@�l"@�>B@�M@�
�@��@��@�4�@���@���@��@� �@��@���@�1�@�~@���@���@�x�@�H�@�C�@�8@�1�@�!-@���@�q�@�M@���@�Mj@���@�c @�A�@���@��f@�o@��@�I�@���@��@��o@�#:@��W@��@�U�@�@���@��Y@�Q@�6@�+k@�'R@�M@��@���@�@��$@��\@�U2@�=q@��@���@���@�O�@��@��5@�֡@��[@�Ĝ@��U@��@���@���@�]d@��@�[W@�@O@��@��c@�҉@�~�@�@�j�@���@��B@�($@��>@��T@��>@��z@��	@�t�@�>�@��@��9@�� @�c�@��@���@���@�E9@�@@���@���@���@��@�;�@���@�t�@�!-@���@�Ta@��a@�\�@�2a@�@@��8@��M@��E@��}@��@�(�@���@�T�@�8�@���@��A@�_@���@�o @�Z�@�A�@�;d@�.I@� \@��@��@��2@��_@�	@���@�|@�0�@�@��D@�1'@��@��f@���@���@�bN@�C�@�6�@� �@��@�\�@�#�@�@��j@�K^@��}@��:@�33@��X@��\@�C-@�˒@��[@���@���@�e,@�S&@�IR@�4�@���@���@�:*@��@��r@��@���@���@��@�2a@��@���@�҉@�
�@�˒@���@�c@�Dg@�;@�j@���@�ԕ@�Dg@�&�@���@��}@���@�xl@�)�@�W@�0@l�@�@S@~��@~�c@~��@~{@}�@}o @|��@|��@|l"@|/�@{�m@{�@z��@y��@y[W@y<6@x��@x�@vE�@u7L@tɆ@t��@tu�@tA�@s�@s�@r�@r��@r0U@q��@q?}@p�/@p|�@pS�@o˒@n�@n8�@m�N@m�n@m��@m�~@mj@m�@l*�@k�Q@k��@k!-@j��@j�@i�@i��@iVm@h��@h'R@g�4@f��@f;�@eԕ@e��@e\�@d�P@dZ@c��@cg�@c)_@c�@b��@b��@b;�@a��@`��@`z�@` �@_�A@_��@_(@^��@^+k@^@]}�@\�@\��@\�@[�@[{J@[1�@Z��@YDg@XbN@W�&@W��@WO@W/�@V��@V}V@U�@Uzx@U%@T�)@Th�@T@S�6@S��@S=@S�@R�+@R!�@R
�@Q�N@Q�S@QT�@Q�@P��@P�@P��@P$@P�@P@OZ�@O!-@O�@N��@N�@Mhs@M-w@M;@LN�@K�0@K�{@KMj@K�@K@J�@Jq�@J6�@J
�@I�@Ix�@IVm@I%@Hѷ@H~(@H7�@G�+@G�a@GU�@F��@Fl�@F	@E��@E�@Dc�@C�r@C�@B�"@B��@B_�@B.�@A��@A�z@A��@A��@A \@@��@@�@@7�@@�@@G@?�g@?�F@?|�@?P�@?>�@?�@>��@>��@>YK@=��@=c�@<֡@<�@;˒@;\)@;J#@:��@:�1@:1�@9�@9�"@9��@9x�@9A @9�@8�v@8��@8"h@7˒@7��@7X�@79�@6ߤ@6d�@6J@5�3@5�^@5s�@4�f@4�K@4�@3�+@3��@3o�@34�@2��@2��@2�@1�@1�@0_@09X@/�W@/��@/{J@/S@.i�@.&�@-�-@-��@-s�@-�@,�	@,��@,�@,7�@,-�@,1@+��@+{J@+�@*Z�@*.�@*
�@)�3@)��@)�@(�v@(�.@(�@'��@'�0@'iD@'(@&��@&�R@&��@&Ov@&�@%��@%��@%�@%F@$�P@$�?@$��@$Xy@$*�@$�@#�@#dZ@"�c@"�B@"v�@"O@!�D@!@!��@!}�@!j@!Q�@!#�@ ��@ q@ %�@�+@��@�@@E9@�M@�,@��@{�@\�@3�@
�@��@��@�-@T�@�@��@>B@�@�;@��@~�@iD@E9@�@ں@�@��@�@�1@��@�\@ff@@ԕ@@�@�=@u�@8�@;@ی@�_@|�@K^@�@M@b@G@��@��@�k@l�@U�@+@�@�\@s�@ff@i�@i�@i�@a|@.�@@�#@��@�C@�X@��@w2@a�@^�@IR@V@�@��@�z@�@��@��@��@�@q@c�@N�@b@�@��@y�@W?@�@��@��@z@R�@:*@?@@�@8�@3�@@��@ϫ@�-@�=@x�@\�@8�@@@��@�9@bN@-�@~@�@��@�@�q@�P@y�@iD@P�@�2@�<@��@O@�@�3@�X@f�@:�@�@�@�@�p@��@��@j@I�@	�@�[@'�@
�2@
͟@
��@
�@
�@
�@
�1@
��@
��@
{�@
Q@
4@	�9@	@	��@	��@	O�@	V@�f@�|@�@�D@h�@Z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�BB�qB�B�"B��B��B��B��B��B�B�"B�"B�<B�<B�"B�<B�"B�"B�<B�<B�<B�"B�<B�<B�<B�<B�VB�VB�VB�VB�qB�qB��B�VB�qB��B��B	EB	�B	�B
B
�SB
�B
�oB
�B
��B
��B
�_B
��B
�vB
kQB
RB
N<B
%,B
9B
FB
�B
�B
B	�B	ԯB	�zB	�B	��B	��B	��B	�B	t�B	o�B	WYB	9�B	5B	�B		lB�-B��B�5B�B�B�5B�B�WB�7B�?B�B��BŢB��B�wB��B�B��BӏB�
B�yB��B�B��B�}BϑBѝB�B�bB�B҉B� BǔBªB�#BچB	B	)�B	A�B	V�B	k6B	��B	��B	��B	��B	��B	�AB	��B	��B	�>B	��B	��B	��B	�B	�mB	ۦB	ۦB	�B	�CB	�1B	��B	��B	�WB	ּB	�B	ƨB	��B	��B	�;B	�(B	�}B	��B	�9B	�B	�HB	��B	�0B	��B	��B	��B	ȀB	�7B	��B	�rB	�B	��B	�B	�RB	�B	�B	�VB	�FB	�NB	� B	ѝB	��B	՛B	��B	�@B	�fB	�"B	�IB	�B	�AB	��B	�eB	��B	�B	��B	�sB	�B	�qB	�B	��B	�B	�B	�oB	�OB	�cB	��B	�B	��B	�|B	��B	�'B	�3B	�MB	�B	��B	�B	�MB	�B	�AB	�[B	�'B	�B	�B	�iB	�B	�B	�sB	�,B	ߊB	�qB	ևB	յB	��B	�B	�DB	��B	�B	�B	�B	�B	�B	��B	�WB	�6B	�B	��B	�B	��B	�B	��B	��B	��B	��B	�	B	�fB	��B	�B	�;B	��B	�B	�B	��B	��B	�[B	�[B	�GB	�|B	��B	�B	�ZB	�B	��B	�DB	��B	��B	��B	��B	�dB	�dB	�dB	�JB	�0B	�JB	�0B	�0B	��B	��B	��B	�dB	�0B	��B	�xB	�^B	�B	��B	�^B	�^B	�DB	��B	�XB	��B	��B	�rB	�XB	��B	��B	�B	�xB	��B	��B	��B	��B	��B	�^B	��B	�JB	��B	��B	�"B	�qB	��B	��B	��B	��B	��B	�]B	��B	��B	�HB	��B	��B	��B	��B
 B
 �B
 �B
 �B
 B
 B
 �B
;B
�B
�B
-B
GB
aB
{B
�B
�B
MB
�B
�B
SB
�B
9B
SB
�B
B
�B
?B
tB
�B
�B
B
9B
�B
B
SB
�B
B
zB
zB
zB
�B
_B
�B
1B
1B
	RB
	�B
	�B
	�B
	�B

�B
dB
B
�B
"B
B
B
�B
HB
�B
�B
�B
�B
�B
�B
NB
4B
�B
 B
�B
B
B
B
:B
oB
�B
�B
�B
�B
B
B
B
@B
�B
FB
aB
{B
�B
{B
�B
�B
aB
B
,B
,B
�B
�B
gB
�B
B
9B
mB
�B
�B
�B
�B
?B
?B
$B
sB
�B
B
EB
�B
�B
B
�B
�B
=B
�B
B
�B
�B
dB
/B
dB
�B
�B
�B
OB
�B
B
�B
B
!B
pB
�B
 \B
 �B
!bB
!�B
"�B
"�B
#:B
#nB
#nB
#TB
#�B
#�B
#�B
$ZB
$&B
$tB
$tB
#nB
$&B
#�B
#�B
$�B
$�B
%`B
%�B
%�B
&LB
&fB
&fB
&�B
&�B
&�B
&�B
&�B
'B
&�B
'RB
'�B
(
B
(�B
(�B
(�B
)*B
)B
)B
(�B
)*B
)DB
)_B
)DB
)�B
*B
*�B
*�B
+B
+�B
+�B
,�B
-CB
-CB
-wB
-]B
-�B
-�B
-�B
-�B
.}B
.�B
/B
/�B
/�B
/�B
0;B
1�B
2B
1�B
2-B
1�B
2-B
2�B
3B
2�B
3hB
4B
4�B
5B
5tB
6�B
6zB
6`B
7LB
7�B
7�B
8�B
9rB
9�B
9�B
:DB
:�B
:�B
:�B
;�B
<�B
=B
=B
=<B
>�B
>�B
?HB
?cB
?�B
?�B
@iB
@iB
@ B
?�B
?}B
?.B
>�B
>]B
>�B
?HB
?.B
>�B
>�B
>�B
>�B
>�B
?.B
?cB
?}B
?�B
?�B
@ B
@iB
AUB
A�B
BAB
B[B
B�B
B�B
CaB
C�B
DB
D3B
DB
D3B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
E�B
E�B
E�B
FB
F?B
F�B
G+B
G�B
G�B
H1B
H1B
HfB
H�B
IB
IRB
IlB
I�B
I�B
I�B
JXB
J#B
J=B
J#B
J�B
J�B
J�B
KxB
K^B
KxB
K�B
K�B
L�B
MB
M�B
M�B
M�B
M�B
M�B
NB
NpB
N�B
OB
N�B
OBB
O�B
O�B
O�B
P�B
P}B
P�B
Q4B
QB
QNB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
RB
Q�B
Q�B
SB
R�B
R�B
R�B
S[B
S�B
S�B
S�B
TFB
T�B
T�B
T�B
T�B
T�B
UB
U�B
U�B
U�B
VB
V9B
V9B
VSB
VSB
V�B
V�B
WsB
W�B
X+B
X+B
X�B
X�B
X�B
YB
Y�B
Y�B
Y�B
Z7B
Z�B
Z�B
Z�B
[	B
[#B
[	B
[#B
[qB
[�B
[�B
\]B
\xB
\CB
\�B
\�B
\�B
]B
]B
]/B
]/B
]dB
]�B
^OB
^�B
_!B
_!B
_�B
`'B
`BB
`�B
`�B
a-B
aHB
a�B
a|B
a|B
a�B
a�B
a�B
a�B
b�B
b�B
b�B
c:B
c B
cnB
c�B
c�B
d@B
dB
dtB
d�B
d�B
e,B
e�B
e�B
fB
fLB
f�B
f�B
gB
gRB
g�B
hsB
hsB
h�B
h�B
h�B
h�B
h�B
iB
jB
i�B
jB
jB
j0B
jB
jeB
jeB
jeB
jeB
j�B
j�B
j�B
kkB
k�B
k�B
k�B
lB
l�B
l�B
l�B
mCB
mCB
mwB
m�B
m�B
nB
n/B
nIB
n}B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p!B
p!B
p;B
pUB
pUB
qB
q'B
qAB
qvB
q�B
q�B
q�B
rB
r-B
r-B
rB
rB
raB
r|B
raB
r�B
r�B
s3B
sMB
sMB
s�B
s�B
s�B
s�B
s�B
tB
tB
tTB
t9B
tB
tB
tB
tnB
t�B
t�B
t�B
t�B
uB
uB
u?B
uZB
utB
u�B
u�B
u�B
u�B
u�B
u�B
vB
v+B
vB
vB
v`B
v�B
wB
wLB
wfB
w�B
w�B
w�B
w�B
w�B
xB
w�B
w�B
xB
x8B
xRB
x8B
x8B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y$B
y$B
yXB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z*B
zDB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
{�B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}"B
}"B
}<B
}VB
}qB
}�B
}�B
}�B
}�B
~BB
~wB
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
}B
�B
� B
�OB
�iB
��B
��B
��B
�B
�UB
�UB
�oB
�oB
�oB
�oB
��B
��B
�B
��B
�-B
��B
��B
��B
��B
��B
�B
�3B
�3B
�3B
�MB
��B
��B
��B
��B
��B
��B
��B
�%B
�?B
�?B
�YB
��B
�+B
�E111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�BB�qB�B�"B��B��B��B��B��B�B�"B�"B�<B�<B�"B�<B�"B�"B�<B�<B�<B�"B�<B�<B�<B�<B�VB�VB�VB�VB�qB�qB��B�VB�qB��B��B	EB	�B	�B
B
�SB
�B
�oB
�B
��B
��B
�_B
��B
�vB
kQB
RB
N<B
%,B
9B
FB
�B
�B
B	�B	ԯB	�zB	�B	��B	��B	��B	�B	t�B	o�B	WYB	9�B	5B	�B		lB�-B��B�5B�B�B�5B�B�WB�7B�?B�B��BŢB��B�wB��B�B��BӏB�
B�yB��B�B��B�}BϑBѝB�B�bB�B҉B� BǔBªB�#BچB	B	)�B	A�B	V�B	k6B	��B	��B	��B	��B	��B	�AB	��B	��B	�>B	��B	��B	��B	�B	�mB	ۦB	ۦB	�B	�CB	�1B	��B	��B	�WB	ּB	�B	ƨB	��B	��B	�;B	�(B	�}B	��B	�9B	�B	�HB	��B	�0B	��B	��B	��B	ȀB	�7B	��B	�rB	�B	��B	�B	�RB	�B	�B	�VB	�FB	�NB	� B	ѝB	��B	՛B	��B	�@B	�fB	�"B	�IB	�B	�AB	��B	�eB	��B	�B	��B	�sB	�B	�qB	�B	��B	�B	�B	�oB	�OB	�cB	��B	�B	��B	�|B	��B	�'B	�3B	�MB	�B	��B	�B	�MB	�B	�AB	�[B	�'B	�B	�B	�iB	�B	�B	�sB	�,B	ߊB	�qB	ևB	յB	��B	�B	�DB	��B	�B	�B	�B	�B	�B	��B	�WB	�6B	�B	��B	�B	��B	�B	��B	��B	��B	��B	�	B	�fB	��B	�B	�;B	��B	�B	�B	��B	��B	�[B	�[B	�GB	�|B	��B	�B	�ZB	�B	��B	�DB	��B	��B	��B	��B	�dB	�dB	�dB	�JB	�0B	�JB	�0B	�0B	��B	��B	��B	�dB	�0B	��B	�xB	�^B	�B	��B	�^B	�^B	�DB	��B	�XB	��B	��B	�rB	�XB	��B	��B	�B	�xB	��B	��B	��B	��B	��B	�^B	��B	�JB	��B	��B	�"B	�qB	��B	��B	��B	��B	��B	�]B	��B	��B	�HB	��B	��B	��B	��B
 B
 �B
 �B
 �B
 B
 B
 �B
;B
�B
�B
-B
GB
aB
{B
�B
�B
MB
�B
�B
SB
�B
9B
SB
�B
B
�B
?B
tB
�B
�B
B
9B
�B
B
SB
�B
B
zB
zB
zB
�B
_B
�B
1B
1B
	RB
	�B
	�B
	�B
	�B

�B
dB
B
�B
"B
B
B
�B
HB
�B
�B
�B
�B
�B
�B
NB
4B
�B
 B
�B
B
B
B
:B
oB
�B
�B
�B
�B
B
B
B
@B
�B
FB
aB
{B
�B
{B
�B
�B
aB
B
,B
,B
�B
�B
gB
�B
B
9B
mB
�B
�B
�B
�B
?B
?B
$B
sB
�B
B
EB
�B
�B
B
�B
�B
=B
�B
B
�B
�B
dB
/B
dB
�B
�B
�B
OB
�B
B
�B
B
!B
pB
�B
 \B
 �B
!bB
!�B
"�B
"�B
#:B
#nB
#nB
#TB
#�B
#�B
#�B
$ZB
$&B
$tB
$tB
#nB
$&B
#�B
#�B
$�B
$�B
%`B
%�B
%�B
&LB
&fB
&fB
&�B
&�B
&�B
&�B
&�B
'B
&�B
'RB
'�B
(
B
(�B
(�B
(�B
)*B
)B
)B
(�B
)*B
)DB
)_B
)DB
)�B
*B
*�B
*�B
+B
+�B
+�B
,�B
-CB
-CB
-wB
-]B
-�B
-�B
-�B
-�B
.}B
.�B
/B
/�B
/�B
/�B
0;B
1�B
2B
1�B
2-B
1�B
2-B
2�B
3B
2�B
3hB
4B
4�B
5B
5tB
6�B
6zB
6`B
7LB
7�B
7�B
8�B
9rB
9�B
9�B
:DB
:�B
:�B
:�B
;�B
<�B
=B
=B
=<B
>�B
>�B
?HB
?cB
?�B
?�B
@iB
@iB
@ B
?�B
?}B
?.B
>�B
>]B
>�B
?HB
?.B
>�B
>�B
>�B
>�B
>�B
?.B
?cB
?}B
?�B
?�B
@ B
@iB
AUB
A�B
BAB
B[B
B�B
B�B
CaB
C�B
DB
D3B
DB
D3B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
E�B
E�B
E�B
FB
F?B
F�B
G+B
G�B
G�B
H1B
H1B
HfB
H�B
IB
IRB
IlB
I�B
I�B
I�B
JXB
J#B
J=B
J#B
J�B
J�B
J�B
KxB
K^B
KxB
K�B
K�B
L�B
MB
M�B
M�B
M�B
M�B
M�B
NB
NpB
N�B
OB
N�B
OBB
O�B
O�B
O�B
P�B
P}B
P�B
Q4B
QB
QNB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
RB
Q�B
Q�B
SB
R�B
R�B
R�B
S[B
S�B
S�B
S�B
TFB
T�B
T�B
T�B
T�B
T�B
UB
U�B
U�B
U�B
VB
V9B
V9B
VSB
VSB
V�B
V�B
WsB
W�B
X+B
X+B
X�B
X�B
X�B
YB
Y�B
Y�B
Y�B
Z7B
Z�B
Z�B
Z�B
[	B
[#B
[	B
[#B
[qB
[�B
[�B
\]B
\xB
\CB
\�B
\�B
\�B
]B
]B
]/B
]/B
]dB
]�B
^OB
^�B
_!B
_!B
_�B
`'B
`BB
`�B
`�B
a-B
aHB
a�B
a|B
a|B
a�B
a�B
a�B
a�B
b�B
b�B
b�B
c:B
c B
cnB
c�B
c�B
d@B
dB
dtB
d�B
d�B
e,B
e�B
e�B
fB
fLB
f�B
f�B
gB
gRB
g�B
hsB
hsB
h�B
h�B
h�B
h�B
h�B
iB
jB
i�B
jB
jB
j0B
jB
jeB
jeB
jeB
jeB
j�B
j�B
j�B
kkB
k�B
k�B
k�B
lB
l�B
l�B
l�B
mCB
mCB
mwB
m�B
m�B
nB
n/B
nIB
n}B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p!B
p!B
p;B
pUB
pUB
qB
q'B
qAB
qvB
q�B
q�B
q�B
rB
r-B
r-B
rB
rB
raB
r|B
raB
r�B
r�B
s3B
sMB
sMB
s�B
s�B
s�B
s�B
s�B
tB
tB
tTB
t9B
tB
tB
tB
tnB
t�B
t�B
t�B
t�B
uB
uB
u?B
uZB
utB
u�B
u�B
u�B
u�B
u�B
u�B
vB
v+B
vB
vB
v`B
v�B
wB
wLB
wfB
w�B
w�B
w�B
w�B
w�B
xB
w�B
w�B
xB
x8B
xRB
x8B
x8B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y$B
y$B
yXB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z*B
zDB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
{�B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}"B
}"B
}<B
}VB
}qB
}�B
}�B
}�B
}�B
~BB
~wB
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
}B
�B
� B
�OB
�iB
��B
��B
��B
�B
�UB
�UB
�oB
�oB
�oB
�oB
��B
��B
�B
��B
�-B
��B
��B
��B
��B
��B
�B
�3B
�3B
�3B
�MB
��B
��B
��B
��B
��B
��B
��B
�%B
�?B
�?B
�YB
��B
�+B
�E111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230113154255  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230113154316  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230113154317  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230113154317                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230113154318  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230113154318  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230113162614                      G�O�G�O�G�O�                