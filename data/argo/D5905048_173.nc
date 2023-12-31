CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-10-25T00:35:08Z creation;2017-10-25T00:35:11Z conversion to V3.1;2019-12-19T07:54:15Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20171025003508  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_173                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�05�w�1   @�06DDD�@4����D��d�����1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A~ffA�33A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^y�D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@~�R@�(�@�(�A{A>{A^{A|z�A�=qA�
=A�=qA�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	��C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1ǮC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW��CY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDq�D�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^q�D^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D���D�<)D�|)D���D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�?\D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��)D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�\D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��\D�E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�~�Aه+AمAه+AًDAى7Aى7A�z�A�z�A�|�Aه+Aى7Aى7Aى7AًDAى7Aى7AًDAًDAًDAٍPAُ\Aى7A�|�A�r�A�VA��TA���A�`BAѼjA��mA�M�A̗�A�?}A�  A�33Aǝ�A���A�%A��A²-A�
=A��A���A�&�A�jA��FA��wA�ffA�(�A�A���A�  A��!A�dZA��\A�\)A���A�bNA��#A���A�A�A��A�A�A���A��A�ȴA�=qA��!A��mA��A���A�E�A�\)A��jA��hA��A��;A�bA���A��A��jA���A�r�A�$�A���A���A�;dA��A��A��hA�{A���A��\A��/A�S�A��TA�{A�G�A�7LA�\)A�n�A�5?A�ZA�&�A��A���A�?}A���A���A�O�A��HA���A�  A���A��A|��A|��A|Az-Ax��AwdZAu+As�-Ar�/ApȴAo/Al��Ait�Ae�;Ae?}AdI�Ac+AaƨA\��AZbAY��AX��AW%AVr�AT^5AS�AR�`AQ��ANr�AH~�AE��AD�ACoAA�7A@�jA>�A=�7A<��A;�A:^5A8ȴA77LA6�jA6JA5|�A4�A3�
A2ZA0ffA/��A.��A.ZA-;dA,A�A+"�A)�A(A�A&��A$ĜA#�PA#�A#�A"��A"ffA!�7A r�A�A`BA~�A�-A�AS�A�9A|�A�AM�AVA�RA^5A"�A�A��AA�A�HAx�A��A-A��AC�A
n�A	\)A	%A��Al�A�A=qA/A��A��A��A7LA �@���@���@�Q�@���@�G�@�A�@�ȴ@��@�l�@�+@�5?@��@�b@���@�7@���@��@��@�7@��/@�u@�b@��@�u@���@�^@���@�|�@ް!@���@���@�  @ۍP@��@�Z@�n�@�@Ձ@�7L@��@��@�^5@љ�@ЋD@���@�\)@��H@ΰ!@�n�@��@���@Ͳ-@���@�Z@˥�@�"�@��@��@ȃ@ǅ@��@ă@Å@��y@�v�@��7@��9@��@��P@�S�@�+@�@���@��@�V@�$�@���@�~�@�{@�V@�Q�@�1'@��@�{@��@��y@���@�~�@�=q@���@���@��\@��+@�V@�J@���@���@�1@�S�@��+@�M�@�5?@���@��/@��@�S�@�S�@�"�@�S�@��F@���@�t�@�;d@�n�@�E�@��@��@�r�@�1'@�A�@�9X@��@�b@���@��F@�l�@�+@��!@���@��H@�v�@���@�`B@��/@���@�@��@��@��@���@��@�S�@��F@��P@�r�@�b@��@�
=@��R@�X@��D@�bN@��@���@�Ĝ@���@��@�z�@�1@�t�@�
=@���@��R@�~�@���@��@���@�hs@��@���@��D@��
@��@�t�@�C�@��@��!@�^5@�@���@�G�@�/@�V@���@��@��`@���@�z�@�I�@� �@�b@��m@�ƨ@���@�"�@���@��H@��+@��@���@��#@��^@���@��@�p�@�G�@�V@��9@��@�Q�@�I�@�b@��w@���@�t�@�S�@�o@��y@��R@�ff@�-@�@��@��@��#@���@�`B@�O�@�G�@�O�@�G�@��@��/@���@��@�Z@��@���@���@��@���@�l�@�@�V@�J@���@�`B@�&�@���@��@�bN@�9X@�  @��m@��;@��
@��F@���@��P@�l�@���@�-@��@���@��-@��@��/@�A�@��@��@�t�@�;d@�o@���@���@�M�@�=q@�5?@�@��T@���@�x�@�`B@�&�@��@��@��@�Z@�I�@�1'@�  @��w@�S�@��y@���@��R@��R@��R@��!@���@�ff@��T@��7@�?}@�V@��@���@�9X@��@��m@��
@��F@��@��F@��F@��@���@���@��+@�ff@�$�@��T@��T@��@��@��#@��^@���@��h@�V@�Ĝ@�r�@�(�@�1'@�(�@��@��@K�@�@~�@~ff@~V@~V@~5?@}��@}�-@}/@}V@|z�@{�m@{dZ@{@z��@z-@y�#@yhs@x��@x�u@x�@xbN@x �@w�;@w;d@vȴ@vV@vE�@v$�@u@t�@tZ@s�@so@r��@rM�@qx�@q%@p�@pQ�@pA�@o��@ol�@o\)@o�@o�@n�y@n�R@n��@nv�@nE�@n$�@m�h@mV@l�/@lj@k�F@k33@j��@j~�@jn�@j�@i�#@i��@i��@i�7@i7L@h��@hĜ@h��@h�u@h�u@hr�@hbN@hA�@g��@gl�@g;d@f��@f�@fv�@f5?@e�@e�-@ep�@eO�@d�/@dz�@dZ@d�@c�m@c33@b�@b��@b~�@bJ@ax�@`�9@`1'@_|�@^�R@^$�@]�@\��@\��@\�D@\Z@\1@[33@Z�\@Z^5@Z=q@Z=q@ZM�@Z=q@Z�@ZJ@X��@X1'@W��@W�P@V�@V�+@Vv�@VV@V$�@U�-@UO�@UV@T(�@S�F@S��@SdZ@S"�@St�@SdZ@R�H@R�!@RM�@Q�@Q�@PĜ@P��@Pr�@P1'@O�@O�P@N��@Nȴ@N�R@N@M�-@M��@M��@M/@L�/@L��@L�@L�D@LZ@L�@K��@K�m@Kƨ@Kƨ@K�@I�@IG�@I7L@I�@I%@H��@H�`@HĜ@H��@H�@HA�@HA�@H �@H  @G�@G\)@G;d@G;d@G�@F�@Fȴ@F�R@FE�@E�@E�@D��@D�D@C��@C��@CC�@C@Bn�@A�#@A�^@A��@AX@@��@@�9@@�@@r�@@Q�@@A�@@  @?�w@?l�@?+@>�y@>ȴ@>��@>ff@>V@>{@=�h@=/@<�/@<�j@<z�@<9X@<1@;��@;S�@;@:��@:�@:=q@:=q@:-@9�@9��@9G�@8��@8�9@7�@7�;@7�@6�R@6v�@65?@5�T@5��@5�h@5�@4�/@4�@4�j@4�@3S�@2��@2n�@2^5@2^5@2M�@2=q@2-@2�@1�@1��@1��@1x�@17L@1�@0�`@0Ĝ@0�9@0��@0b@/�@/��@/�@/�P@/K�@.�R@.v�@-��@-`B@-`B@-`B@-`B@-?}@-�@,��@,��@,�j@,�D@,j@,�@,�@,1@+��@+�m@+�m@+��@+�@+dZ@+"�@*�H@*~�@)�@)�^@)x�@)X@)X@)7L@(�`@(��@(�9@(�u@(�@(r�@(Q�@( �@'|�@';d@'+@&��@&�y@&�+@&@%�@%`B@$�@$��@$��@$z�@$�@$1@#�
@#�F@#��@#S�@"�H@"n�@"-@!�#@!��@!hs@!7L@ ��@ �9@ ��@ �u@ r�@ Q�@ A�@   @�@�w@��@�P@|�@�@v�@$�@{@�T@�-@�h@?}@��@�@��@��@�@dZ@33@�@��@��@��@�\@-@��@�@�7@&�@��@�9@r�@b@�w@�P@
=@�y@�@�R@V@{@�@�@�@�T@@�-@`B@�@��@��@I�@9X@�@1@�m@�F@��@��@t�@33@"�@o@��@�!@�!@��@n�@M�@�@��@X@7L@��@Ĝ@Ĝ@�9@�9@r�@Q�@ �@b@  @�@�;@�w@�P@�@�y@�@ȴ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�~�Aه+AمAه+AًDAى7Aى7A�z�A�z�A�|�Aه+Aى7Aى7Aى7AًDAى7Aى7AًDAًDAًDAٍPAُ\Aى7A�|�A�r�A�VA��TA���A�`BAѼjA��mA�M�A̗�A�?}A�  A�33Aǝ�A���A�%A��A²-A�
=A��A���A�&�A�jA��FA��wA�ffA�(�A�A���A�  A��!A�dZA��\A�\)A���A�bNA��#A���A�A�A��A�A�A���A��A�ȴA�=qA��!A��mA��A���A�E�A�\)A��jA��hA��A��;A�bA���A��A��jA���A�r�A�$�A���A���A�;dA��A��A��hA�{A���A��\A��/A�S�A��TA�{A�G�A�7LA�\)A�n�A�5?A�ZA�&�A��A���A�?}A���A���A�O�A��HA���A�  A���A��A|��A|��A|Az-Ax��AwdZAu+As�-Ar�/ApȴAo/Al��Ait�Ae�;Ae?}AdI�Ac+AaƨA\��AZbAY��AX��AW%AVr�AT^5AS�AR�`AQ��ANr�AH~�AE��AD�ACoAA�7A@�jA>�A=�7A<��A;�A:^5A8ȴA77LA6�jA6JA5|�A4�A3�
A2ZA0ffA/��A.��A.ZA-;dA,A�A+"�A)�A(A�A&��A$ĜA#�PA#�A#�A"��A"ffA!�7A r�A�A`BA~�A�-A�AS�A�9A|�A�AM�AVA�RA^5A"�A�A��AA�A�HAx�A��A-A��AC�A
n�A	\)A	%A��Al�A�A=qA/A��A��A��A7LA �@���@���@�Q�@���@�G�@�A�@�ȴ@��@�l�@�+@�5?@��@�b@���@�7@���@��@��@�7@��/@�u@�b@��@�u@���@�^@���@�|�@ް!@���@���@�  @ۍP@��@�Z@�n�@�@Ձ@�7L@��@��@�^5@љ�@ЋD@���@�\)@��H@ΰ!@�n�@��@���@Ͳ-@���@�Z@˥�@�"�@��@��@ȃ@ǅ@��@ă@Å@��y@�v�@��7@��9@��@��P@�S�@�+@�@���@��@�V@�$�@���@�~�@�{@�V@�Q�@�1'@��@�{@��@��y@���@�~�@�=q@���@���@��\@��+@�V@�J@���@���@�1@�S�@��+@�M�@�5?@���@��/@��@�S�@�S�@�"�@�S�@��F@���@�t�@�;d@�n�@�E�@��@��@�r�@�1'@�A�@�9X@��@�b@���@��F@�l�@�+@��!@���@��H@�v�@���@�`B@��/@���@�@��@��@��@���@��@�S�@��F@��P@�r�@�b@��@�
=@��R@�X@��D@�bN@��@���@�Ĝ@���@��@�z�@�1@�t�@�
=@���@��R@�~�@���@��@���@�hs@��@���@��D@��
@��@�t�@�C�@��@��!@�^5@�@���@�G�@�/@�V@���@��@��`@���@�z�@�I�@� �@�b@��m@�ƨ@���@�"�@���@��H@��+@��@���@��#@��^@���@��@�p�@�G�@�V@��9@��@�Q�@�I�@�b@��w@���@�t�@�S�@�o@��y@��R@�ff@�-@�@��@��@��#@���@�`B@�O�@�G�@�O�@�G�@��@��/@���@��@�Z@��@���@���@��@���@�l�@�@�V@�J@���@�`B@�&�@���@��@�bN@�9X@�  @��m@��;@��
@��F@���@��P@�l�@���@�-@��@���@��-@��@��/@�A�@��@��@�t�@�;d@�o@���@���@�M�@�=q@�5?@�@��T@���@�x�@�`B@�&�@��@��@��@�Z@�I�@�1'@�  @��w@�S�@��y@���@��R@��R@��R@��!@���@�ff@��T@��7@�?}@�V@��@���@�9X@��@��m@��
@��F@��@��F@��F@��@���@���@��+@�ff@�$�@��T@��T@��@��@��#@��^@���@��h@�V@�Ĝ@�r�@�(�@�1'@�(�@��@��@K�@�@~�@~ff@~V@~V@~5?@}��@}�-@}/@}V@|z�@{�m@{dZ@{@z��@z-@y�#@yhs@x��@x�u@x�@xbN@x �@w�;@w;d@vȴ@vV@vE�@v$�@u@t�@tZ@s�@so@r��@rM�@qx�@q%@p�@pQ�@pA�@o��@ol�@o\)@o�@o�@n�y@n�R@n��@nv�@nE�@n$�@m�h@mV@l�/@lj@k�F@k33@j��@j~�@jn�@j�@i�#@i��@i��@i�7@i7L@h��@hĜ@h��@h�u@h�u@hr�@hbN@hA�@g��@gl�@g;d@f��@f�@fv�@f5?@e�@e�-@ep�@eO�@d�/@dz�@dZ@d�@c�m@c33@b�@b��@b~�@bJ@ax�@`�9@`1'@_|�@^�R@^$�@]�@\��@\��@\�D@\Z@\1@[33@Z�\@Z^5@Z=q@Z=q@ZM�@Z=q@Z�@ZJ@X��@X1'@W��@W�P@V�@V�+@Vv�@VV@V$�@U�-@UO�@UV@T(�@S�F@S��@SdZ@S"�@St�@SdZ@R�H@R�!@RM�@Q�@Q�@PĜ@P��@Pr�@P1'@O�@O�P@N��@Nȴ@N�R@N@M�-@M��@M��@M/@L�/@L��@L�@L�D@LZ@L�@K��@K�m@Kƨ@Kƨ@K�@I�@IG�@I7L@I�@I%@H��@H�`@HĜ@H��@H�@HA�@HA�@H �@H  @G�@G\)@G;d@G;d@G�@F�@Fȴ@F�R@FE�@E�@E�@D��@D�D@C��@C��@CC�@C@Bn�@A�#@A�^@A��@AX@@��@@�9@@�@@r�@@Q�@@A�@@  @?�w@?l�@?+@>�y@>ȴ@>��@>ff@>V@>{@=�h@=/@<�/@<�j@<z�@<9X@<1@;��@;S�@;@:��@:�@:=q@:=q@:-@9�@9��@9G�@8��@8�9@7�@7�;@7�@6�R@6v�@65?@5�T@5��@5�h@5�@4�/@4�@4�j@4�@3S�@2��@2n�@2^5@2^5@2M�@2=q@2-@2�@1�@1��@1��@1x�@17L@1�@0�`@0Ĝ@0�9@0��@0b@/�@/��@/�@/�P@/K�@.�R@.v�@-��@-`B@-`B@-`B@-`B@-?}@-�@,��@,��@,�j@,�D@,j@,�@,�@,1@+��@+�m@+�m@+��@+�@+dZ@+"�@*�H@*~�@)�@)�^@)x�@)X@)X@)7L@(�`@(��@(�9@(�u@(�@(r�@(Q�@( �@'|�@';d@'+@&��@&�y@&�+@&@%�@%`B@$�@$��@$��@$z�@$�@$1@#�
@#�F@#��@#S�@"�H@"n�@"-@!�#@!��@!hs@!7L@ ��@ �9@ ��@ �u@ r�@ Q�@ A�@   @�@�w@��@�P@|�@�@v�@$�@{@�T@�-@�h@?}@��@�@��@��@�@dZ@33@�@��@��@��@�\@-@��@�@�7@&�@��@�9@r�@b@�w@�P@
=@�y@�@�R@V@{@�@�@�@�T@@�-@`B@�@��@��@I�@9X@�@1@�m@�F@��@��@t�@33@"�@o@��@�!@�!@��@n�@M�@�@��@X@7L@��@Ĝ@Ĝ@�9@�9@r�@Q�@ �@b@  @�@�;@�w@�P@�@�y@�@ȴ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B�wB�}B�}B�}B�}B�}B��B��B��B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�wB�wB�wB�jB�XB�RB�RBŢB�B9XB49B.BM�B`BBZBJ�B=qB)�B:^Be`BffBo�By�Br�Bm�Bw�Bk�Bm�Bo�Bk�BhsBp�Bo�BiyBn�B� B�B�%B�1B�B}�Bq�B�B|�B}�Bz�Bv�Bo�BiyB]/BQ�BL�BN�BB�BJ�BA�B=qBA�B6FB&�B�BhB
=BB�ZB�ZB�TBŢB�?B�RB�FB�-B��B��B��B�=B~�Bo�B^5BT�B;dB(�B�B�B+B
�B
ŢB
�}B
�3B
��B
�\B
p�B
ffB
L�B
;dB
F�B
?}B
0!B
-B
 �B
�B
DB
+B	��B	�B	�B	�dB	��B	��B	��B	��B	�7B	iyB	T�B	e`B	\)B	J�B	K�B	=qB	1'B	0!B	�B��B��B�NB�B�ZB�/B�5B��B��B�
B��BȴB��B�}BǮBĜB��B�jB�3B��B��B�B�B��B��B��B��B��B�JB�DB�B�%B�DB�oB�\B�JB�+B�B�+B�7B�B|�Bx�Bn�Bt�Bq�Bq�Br�BhsBn�BjBaHB[#B`BBbNBaHB`BBT�BYBYBM�BL�BN�BO�BT�BK�BVBT�BL�BL�BF�BL�BO�BO�BP�BI�BS�BN�BN�BN�BO�BM�BL�BM�BVBQ�BO�BP�BM�BN�BR�BO�BM�BVBVBXBT�BO�BL�BR�BT�BYBXB[#B]/B\)B_;BaHB^5BXB]/BiyBjBiyBgmBhsBm�Bl�Bk�Bp�Bs�Bt�Bw�Bw�Bt�Bw�Bx�Bv�Bx�Bz�B}�B|�B�B�B�B�B�1B�bB��B��B��B��B��B��B��B��B��B��B��B�'B�RBŢBȴB��B��B��B��B��B��BŢB��B�B�B�B�)B�fB�yB�yB�yB�sB�B�sB�B�B�B��B��B��B	B	B	B	DB	PB	bB	uB	{B	�B	�B	�B	�B	�B	�B	!�B	$�B	(�B	+B	-B	1'B	1'B	1'B	2-B	2-B	2-B	7LB	7LB	5?B	49B	7LB	7LB	@�B	@�B	=qB	<jB	<jB	<jB	D�B	K�B	T�B	S�B	YB	[#B	[#B	]/B	]/B	^5B	^5B	ffB	jB	r�B	s�B	u�B	y�B	{�B	z�B	~�B	�B	�7B	�VB	�PB	�VB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�9B	�LB	�LB	�RB	�XB	�XB	�XB	�jB	�jB	�wB	�}B	��B	��B	��B	��B	B	ÖB	ÖB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�/B	�)B	�)B	�/B	�)B	�)B	�/B	�;B	�;B	�BB	�BB	�;B	�5B	�;B	�NB	�ZB	�`B	�mB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
	7B
	7B
	7B

=B

=B

=B
	7B
1B
B
DB
DB
DB
JB
PB
\B
VB
VB
VB
VB
VB
PB
VB
\B
\B
hB
hB
hB
bB
hB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
"�B
"�B
!�B
"�B
#�B
"�B
"�B
"�B
!�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
$�B
$�B
%�B
&�B
(�B
(�B
(�B
(�B
'�B
'�B
'�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
,B
,B
+B
)�B
'�B
&�B
&�B
%�B
'�B
'�B
(�B
)�B
+B
)�B
)�B
(�B
+B
,B
,B
-B
-B
,B
,B
,B
(�B
)�B
,B
-B
-B
.B
/B
.B
.B
.B
/B
0!B
0!B
1'B
33B
2-B
33B
5?B
5?B
49B
5?B
5?B
49B
33B
5?B
6FB
5?B
5?B
5?B
5?B
5?B
7LB
7LB
6FB
8RB
9XB
9XB
8RB
9XB
;dB
;dB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
9XB
7LB
;dB
?}B
?}B
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
?}B
@�B
@�B
@�B
@�B
A�B
A�B
@�B
?}B
A�B
A�B
B�B
A�B
B�B
C�B
C�B
B�B
C�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
J�B
J�B
J�B
K�B
K�B
N�B
N�B
N�B
N�B
N�B
M�B
M�B
N�B
M�B
N�B
N�B
O�B
P�B
P�B
Q�B
R�B
R�B
Q�B
R�B
S�B
S�B
R�B
Q�B
R�B
T�B
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
W
B
VB
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
XB
W
B
XB
YB
XB
YB
XB
W
B
XB
XB
ZB
[#B
[#B
[#B
ZB
ZB
ZB
[#B
[#B
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
^5B
^5B
^5B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
]/B
_;B
`BB
_;B
_;B
_;B
_;B
_;B
aHB
`BB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
bNB
bNB
bNB
dZB
dZB
dZB
dZB
e`B
dZB
e`B
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
ffB
e`B
dZB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
gmB
iyB
jB
jB
jB
jB
k�B
k�B
l�B
k�B
k�B
l�B
l�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
n�B
m�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
r�B
s�B
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
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
u�B
v�B
v�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�iB�wB�}B�}B�}B��B��B��B��B�iB��B��B�}B��B��B��B�}B�}B��B�}B�}B��B��B��B�<B��B��B�]B̘B��B:�B8B2�BPHBa�B[�BL�B?�B-�B=qBf�Bh>Bp�Bz�BtnBo�BzBncBo�Bp�Bm�Bi�BqvBp�Bk�Bp�B� B�B�EB��B�SB�BtTB��B~]B~�B|PBxlBq�Bk�B_�BU�BOBP�BE9BL~BC�B?.BB�B8RB)�B#B�B�B�G�O�B�2B��G�O�B�B�rB��B�B�yB�B��B�JB� BrB`vBWsB>�B,�BQBB	�B
��B
�^B
�AB
�`B
�B
�TB
u%B
i�B
P�B
>]B
G+B
@�B
2aB
.�B
"�B
�B
B
�B	�xB	��B	�=B	��B	��B	�B	�@B	�yB	�0B	n�B	XEB	e�B	]�B	L�B	L�B	?�B	2�B	1B	 �B��B��B�FB�)B�fB�!BߤB�MBԕB�EB�oB��BB�oB�fBňB��B��B��B�B�B�B�"B��B�NB�;B�kB�KB��B��B�aB��B��B��B�B�B��B��B�KB��B�aB~(Bz*BpoBu�BsMBr�Bs�Bi�Bo5Bk�Bc B]Ba|BcnBbNBabBV�BZ7BZ7BO�BN�BP.BQhBU�BM�BV�BU�BNVBNpBH�BNVBQBQ BQ�BKxBT�BO�BO�BO�BP�BOBN"BN�BV�BR�BP�BQ�BN�BO�BS�BQ BN�BV�BV�BX_BU�BQBNVBTBU�BY�BX�B[�B]�B\�B_�Ba�B_!BY�B^OBi�Bj�Bi�BhsBiDBn/BmCBlqBq'BtTBu?BxBx8Bu?BxBy>BwfByrB{�B~wB}�B��B��B��B�aB�7B�4B�B�B�KB�CB�\B�4B�&B�,B�FB�tB��B�'B��BňB�B�^B͟B�vB�@B�uB�6B�_B�uB�EB�EB�KB�CB�2B�B�B�B��B�B�DB�)B�AB�MB�	B�*B��B	�B	�B	�B	DB	�B	HB	uB	�B	�B	�B	
B	�B	�B	jB	"NB	%B	)B	+6B	-CB	1AB	1vB	1vB	2|B	2aB	2�B	7LB	7�B	5�B	4�B	7�B	7�B	@4B	@�B	>(B	<�B	=<B	<�B	D�B	K�B	T�B	TB	X�B	[�B	[�B	]�B	]�B	_!B	^�B	f�B	jB	r�B	s�B	vB	zB	|B	{JB	}B	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�)B	�5B	�B	�*B	�DB	�KB	�=B	�}B	�iB	��B	��B	��B	�fB	�lB	��B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�(B	� B	�B	�:B	� B	�,B	�2B	�B	�MB	�SB	�?B	�YB	�KB	�KB	�QB	�7B	�QB	�eB	�QB	�CB	�]B	�IB	�IB	�xB	�xB	�dB	�xB	�xB	�~B	�pB	�pB	�\B	�\B	ߊB	ޞB	߾B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	�-B	�B	�B	�B	�B	�	B	�$B	�	B	�B	�B	�B	�B	�B	�6B	�<B	�(B	�BB	�(B	�.B
;B
;B
AB
AB
UB
;B
 OB
uB
MB
MB
B
9B
9B
MB
aB
�B
{B
mB
YB
tB
tB
tB
_B
	RB
	lB
	lB

=B

XB

XB
	RB
�G�O�B
^B
^B
�B
�B
jB
\B
VB
�B
�B
�B
�B
�B
�B
�B
�B
hB
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
#B
#B
"�B
"�B
#B
!�B
# B
$B
# B
# B
#B
"B
$B
$B
#�B
%B
%�B
%�B
&B
$�B
$�B
%�B
'B
)B
)B
)*B
)*B
($B
(>B
($B
*0B
*0B
*0B
*0B
*0B
+6B
+6B
+6B
+6B
+QB
+B
+6B
+6B
+6B
+QB
+6B
,=B
,=B
+6B
*KB
(XB
'8B
'RB
&LB
($B
($B
)*B
*B
+6B
*0B
*0B
)DB
+6B
,=B
,=B
-)B
-B
,"B
,=B
,=G�O�B
*eB
,=B
-CB
-]B
./B
/B
./B
./B
.IB
/OB
0UB
0oB
1vB
33B
2aB
3MB
5ZB
5ZB
4�B
5tB
5�B
4�B
3�B
5tB
6zB
5ZB
5ZB
5ZB
5tB
5�B
7�B
7�B
6�B
8lB
9XB
9rB
8�B
9�B
;B
;�B
:�B
:�B
:�B
;B
;�B
;B
;�G�O�G�O�B
;�B
?}B
?�B
?}B
?}B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
@�B
?�B
A�B
A�B
B�B
A�B
B�B
C�B
C�B
B�B
C�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
J�B
J�B
J�B
K�B
LB
N�B
N�B
OB
N�B
N�B
NB
NB
OB
N"B
OB
O(B
PB
QB
QB
RB
S&B
S&B
R B
S&B
TB
T,B
S[B
RTB
S&B
UB
VB
VB
W
B
W$B
W$B
W?B
W?B
V9B
W
B
W?B
W$B
XEB
X+B
X+B
X+B
XEB
WYB
X+B
Y1B
XEB
YKB
X+B
W?B
X_B
XyB
ZQB
[#B
[=B
[=B
ZQB
Z7B
Z7B
[=B
[=B
Z7B
Z7B
ZQB
[=B
[=B
[#B
[=B
[=B
[=B
[=B
[WB
[WB
[WB
[WB
[qB
\CB
]dB
^5B
^OB
^jB
]IB
^OB
^OB
^OB
^OB
^OB
^jB
^OB
]~B
_VB
`\B
_pB
_pB
_pB
_�B
_�B
a|B
`vB
abB
a|B
a|B
a|B
b�B
cnB
cnB
c�B
bhB
b�B
b�B
d�B
d�B
dtB
dtB
e�B
d�B
e�B
e`B
ezB
e�B
e�B
ezB
e�B
f�B
f�B
f�B
f�B
f�B
e�B
d�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
g�B
i�B
j�B
j�B
j�B
j�B
k�B
k�B
l�B
k�B
k�B
l�B
l�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
n�B
m�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
r�B
s�B
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
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
u�B
v�B
v�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111411411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.12(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710290037002017102900370020171029003700201806221321022018062213210220180622132102201804050723482018040507234820180405072348  JA  ARFMdecpA19c                                                                20171025093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171025003508  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171025003510  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171025003510  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171025003511  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171025003511  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171025003511  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171025003511  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171025003511  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171025003511                      G�O�G�O�G�O�                JA  ARUP                                                                        20171025005540                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171025153313  CV  JULD            G�O�G�O�F���                JM  ARSQJMQC2.0                                                                 20171026000000  CF  PSAL_ADJUSTED_QCC.  D�  G�O�                JM  ARCAJMQC2.0                                                                 20171028153700  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171028153700  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222348  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042102  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                